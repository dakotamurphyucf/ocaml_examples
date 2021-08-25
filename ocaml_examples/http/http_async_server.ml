open Core
open Async
open Httpaf_async
open Router

type error_handler =
  Unix.sockaddr
  -> ?request:Httpaf.Request.t
  -> Httpaf.Server_connection.error
  -> (Httpaf.Headers.t -> Httpaf.Body.Writer.t)
  -> unit

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let read_body reqd =
  let req = reqd |> Httpaf.Reqd.request in
  match req.meth with
  | `POST | `PUT ->
    let finished = Ivar.create () in
    let request_body = Httpaf.Reqd.request_body reqd in
    let body = ref "" in
    let on_eof () = Some !body |> Ivar.fill finished in
    let rec on_read buffer ~off:_ ~len:_ =
      body := !body ^ Bigstringaf.to_string buffer;
      Httpaf.Body.Reader.schedule_read request_body ~on_eof ~on_read
    in
    Httpaf.Body.Reader.schedule_read request_body ~on_eof ~on_read;
    Ivar.read finished
  | _ -> Deferred.return None
;;

let create_respond reqd ~status ~headers content =
  let headers =
    (match headers with
    | None -> []
    | Some hs -> hs)
    @ [ "Content-Length", content |> String.length |> string_of_int ]
    |> Httpaf.Headers.of_list
  in
  let res = Httpaf.Response.create status ~headers in
  Httpaf.Reqd.respond_with_string reqd res content
;;

let start ~port ~on_start ~request_handler ~max_accepts_per_batch ~error_handler =
  let on_handler_error _address exn =
    let message = Exn.to_string exn in
    Log.Global.error "%s\n" message
  in
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  (let open Tcp in
  Server.create_sock
    ~on_handler_error:(`Call on_handler_error)
    ~backlog:10000
    ~max_connections:10000
    ~max_accepts_per_batch
    where_to_listen)
    (Server.create_connection_handler ~request_handler ~error_handler)
  >>= fun server ->
  on_start server port;
  Deferred.never ()
;;

let create_server ~port ~on_start ~max_accepts_per_batch ~router ~error_handler ~reqlogger
  =
  let request_handler _conn (reqd : Httpaf.Reqd.t Gluten.Reqd.t) =
    let { Gluten.Reqd.reqd; _ } = reqd in
    let req = Httpaf.Reqd.request reqd in
    let res = create_respond reqd in
    (match reqlogger with
    | Some fn -> fn req
    | None -> ());
    let run_router body =
      match%bind router req body with
      | Some { status; headers; body } -> Deferred.return (res ~status ~headers body)
      | None ->
        let body =
          match body with
          | Some str -> str
          | None -> ""
        in
        let body_ =
          Printf.sprintf
            "Path: %s\nMethod: %s\nBody: %s"
            req.target
            (req.meth |> Httpaf.Method.to_string)
            body
        in
        Deferred.return (res ~status:`Not_found ~headers:None body_)
    in
    read_body reqd >>= run_router |> Deferred.don't_wait_for
  in
  start ~port ~on_start ~max_accepts_per_batch ~request_handler ~error_handler
;;

module type Websocket_interface = sig
  type server_to_client
  type client_to_server

  val server_to_client_of_string : string -> server_to_client
  val string_of_server_to_client : server_to_client -> string
  val client_to_server_of_string : string -> client_to_server
  val string_of_client_to_server : client_to_server -> string
end

module Make_server_with_ws (Ws : Websocket_interface) = struct
  module Body = Httpaf.Body
  module Headers = Httpaf.Headers
  module Reqd = Httpaf.Reqd
  module Response = Httpaf.Response
  module Status = Httpaf.Status

  let send ~wsd server_to_client =
    if not (Websocketaf.Wsd.is_closed wsd)
    then (
      let str = Ws.string_of_server_to_client server_to_client in
      let bs = Bigstringaf.of_string ~off:0 ~len:(String.length str) str in
      Websocketaf.Wsd.schedule wsd bs ~kind:`Text ~off:0 ~len:(String.length str))
  ;;

  let websocket_handler ~state ~on_ws_connect (req : Httpaf.Request.t) _client_address wsd
    =
    let accum = ref [] in
    let chunk = ref 0 in
    let { Websocket.on_message; on_close } =
      on_ws_connect
        { Websocket.wsd
        ; send = send ~wsd
        ; path = get_path req
        ; query = get_query req
        ; server_state = state
        ; req
        }
    in
    let finalise_content accum_content =
      let compare_fst compare a b = compare (fst a) (fst b) in
      List.sort accum_content ~compare:(compare_fst Int.compare)
      |> List.map ~f:snd
      |> String.concat
    in
    let accum_content payload =
      let finished = Ivar.create () in
      let data = ref "" in
      let on_eof () = Ivar.fill finished !data in
      let rec on_read bs ~off ~len =
        data := !data ^ Bigstringaf.substring ~off ~len bs;
        Websocketaf.Payload.schedule_read payload ~on_read ~on_eof
      in
      Websocketaf.Payload.schedule_read payload ~on_eof ~on_read;
      Ivar.read finished
    in
    let frame ~opcode ~is_fin ~len:_ payload =
      match opcode with
      | `Continuation ->
        if List.is_empty !accum
        then (
          Log.Global.error "Bad frame in the middle of a fragmented message";
          Websocketaf.Wsd.close wsd;
          on_close ())
        else (
          chunk := !chunk + 1;
          let chunk_i = !chunk in
          don't_wait_for
            (accum_content payload
            >>| fun data ->
            accum := (chunk_i, data) :: !accum;
            if is_fin
            then (
              let payload = finalise_content !accum |> Ws.client_to_server_of_string in
              accum := [];
              chunk := 0;
              on_message payload)))
      | `Text | `Binary ->
        if List.is_empty !accum
        then
          don't_wait_for
            (accum_content payload
            >>| fun data ->
            if is_fin
            then on_message (Ws.client_to_server_of_string data)
            else accum := [ 0, data ])
        else (
          Log.Global.error "Bad frame in the middle of a fragmented message";
          Websocketaf.Wsd.close wsd;
          accum := [];
          on_close ())
      | `Connection_close ->
        Websocketaf.Wsd.close wsd;
        accum := [];
        on_close ()
      | `Ping -> Websocketaf.Wsd.send_ping wsd
      | `Pong | `Other _ -> print_endline "other"
    in
    let eof () =
      Log.Global.error "EOF\n%!";
      if not (Websocketaf.Wsd.is_closed wsd) then Websocketaf.Wsd.close wsd;
      on_close ()
    in
    { Websocketaf.Server_connection.frame; eof }
  ;;

  let ws_error_handler wsd (`Exn exn) =
    let message = Exn.to_string exn in
    Log.Global.error "%s\n" message;
    Websocketaf.Wsd.close wsd
  ;;

  let http_error_handler _client_address ?request:_ error handle =
    let message =
      match error with
      | `Exn exn -> Exn.to_string exn
      | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
    in
    let body = handle Headers.empty in
    Body.Writer.write_string body message;
    Body.Writer.close body
  ;;

  let upgrade_handler ~state ~on_ws_connect req addr upgrade () =
    let ws_conn =
      Websocketaf.Server_connection.create_websocket
        ~error_handler:ws_error_handler
        (websocket_handler ~state ~on_ws_connect req addr)
    in
    upgrade (Gluten.make (module Websocketaf.Server_connection) ws_conn)
  ;;

  let http_request_handler ~req_logger ~http_router _conn reqd =
    let req = Httpaf.Reqd.request reqd in
    let res = create_respond reqd in
    (match req_logger with
    | Some fn -> fn req
    | None -> ());
    let run_router body =
      match%bind http_router req body with
      | Some { status; headers; body } -> Deferred.return (res ~status ~headers body)
      | None ->
        let body =
          match body with
          | Some str -> str
          | None -> ""
        in
        let body_ =
          Printf.sprintf
            "Path: %s\nMethod: %s\nBody: %s"
            req.target
            (req.meth |> Httpaf.Method.to_string)
            body
        in
        Deferred.return (res ~status:`Not_found ~headers:None body_)
    in
    read_body reqd >>= run_router |> Deferred.don't_wait_for
  ;;

  let request_handler
      ~req_logger
      ~http_router
      ~on_ws_connect
      ~check_for_websocket_request
      addr
      (reqd : Httpaf.Reqd.t Gluten.Reqd.t)
      ~state
    =
    let { Gluten.Reqd.reqd; upgrade } = reqd in
    let req = Httpaf.Reqd.request reqd in
    let io_handler =
      match%bind check_for_websocket_request req with
      | false -> Deferred.return (http_request_handler ~req_logger ~http_router addr reqd)
      | true ->
        (match
           Websocketaf.Handshake.respond_with_upgrade
             ~sha1
             reqd
             (upgrade_handler ~state ~on_ws_connect req addr upgrade)
         with
        | Ok () -> Deferred.return ()
        | Error err_str ->
          let response =
            Response.create
              ~headers:(Httpaf.Headers.of_list [ "Connection", "close" ])
              `Bad_request
          in
          Reqd.respond_with_string reqd response err_str;
          Deferred.return ())
    in
    Deferred.don't_wait_for io_handler
  ;;

  let start
      ~port
      ~on_start
      ~max_accepts_per_batch
      ~http_router
      ~on_ws_connect
      ~check_for_websocket_request
      ~req_logger
      ~state
    =
    start
      ~port
      ~on_start
      ~max_accepts_per_batch
      ~request_handler:
        (request_handler
           ~req_logger
           ~http_router
           ~on_ws_connect
           ~check_for_websocket_request
           ~state)
      ~error_handler:http_error_handler
  ;;
end
