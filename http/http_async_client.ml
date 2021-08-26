open Core
open Async

module Http = struct
  module M : Request.S with type 'a io = 'a Deferred.t and type config = unit =
  Request.Make (struct
    type 'a io = 'a Deferred.t
    type config = unit

    open Client

    let send ?config:_ req =
      let body = Request.body req in
      let uri = Request.uri req in
      let host = Uri.host_with_default uri in
      let port =
        match Uri.port uri with
        | Some port -> port
        | None -> 80
      in
      let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
      Tcp.connect_sock where_to_connect
      >>= fun socket ->
      let request = Request.as_httpaf req in
      let finished = Ivar.create () in
      let response_handler response response_body =
        Ivar.fill finished (Ok (response, response_body))
      in
      let error_handler error =
        Result.Error (`Connection_error error) |> Ivar.fill finished
      in
      let%bind conn = Httpaf_async.Client.create_connection socket in
      let request_body =
        Httpaf_async.Client.request ~error_handler ~response_handler conn request
      in
      (match body with
      | Some body -> Httpaf.Body.Writer.write_string request_body body
      | None -> ());
      Httpaf.Body.Writer.flush request_body (fun () ->
          Httpaf.Body.Writer.close request_body);
      Ivar.read finished
    ;;
  end)

  include M
end

module Https = struct
  module M : Request.S with type 'a io = 'a Deferred.t and type config = unit =
  Request.Make (struct
    type 'a io = 'a Deferred.t
    type config = unit

    open Client

    let send ?config:_ req =
      let body = Request.body req in
      let uri = Request.uri req in
      let host = Uri.host_with_default uri in
      let port = 443 in
      let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
      Tcp.connect_sock where_to_connect
      >>= fun socket ->
      let%bind conn = Httpaf_async.Client.SSL.create_connection_with_default socket in
      let request = Request.as_httpaf req in
      let finished = Ivar.create () in
      let response_handler response response_body =
        Ivar.fill finished (Ok (response, response_body))
      in
      let error_handler error =
        Result.Error (`Connection_error error) |> Ivar.fill finished
      in
      let request_body =
        Httpaf_async.Client.SSL.request ~error_handler ~response_handler conn request
      in
      (match body with
      | Some body -> Httpaf.Body.Writer.write_string request_body body
      | None -> ());
      Httpaf.Body.Writer.flush request_body (fun () ->
          Httpaf.Body.Writer.close request_body);
      Ivar.read finished
    ;;
  end)

  include M
end

module Response = struct
  module M : Response.S with type 'a io = 'a Deferred.t = Response.Make (struct
    type 'a io = 'a Deferred.t

    let body (_response, response_body) =
      let finished = Ivar.create () in
      let body_str = ref "" in
      let on_eof _ = Ivar.fill finished (Ok !body_str) in
      let rec on_read bs ~off ~len =
        body_str := !body_str ^ Bigstringaf.substring ~off ~len bs;
        Httpaf.Body.Reader.schedule_read response_body ~on_read ~on_eof
      in
      Httpaf.Body.Reader.schedule_read response_body ~on_read ~on_eof;
      Ivar.read finished
    ;;
  end)

  include M
end
