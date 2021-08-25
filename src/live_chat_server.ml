open Core
open Async
open Http.Http_async.Server
open Live_chat_interface

module Websocket_interface = struct
  include Websocket_interface

  let string_of_server_to_client v =
    Yojson.Safe.to_string ([%yojson_of: Server_events.t] v)
  ;;

  let server_to_client_of_string v =
    [%of_yojson: Server_events.t] (Yojson.Safe.from_string v)
  ;;

  let client_to_server_of_string v =
    [%of_yojson: Client_events.t] (Yojson.Safe.from_string v)
  ;;

  let string_of_client_to_server v =
    Yojson.Safe.to_string ([%yojson_of: Client_events.t] v)
  ;;
end

module Topic_exchange = Broker.Topic_router.Make (Server_events)
module Topic_server = Broker.Exchange.MakeExchange (Topic_exchange)
module Server = Make_server_with_ws (Websocket_interface)
module Directory = Topic_exchange.Topic.Directory

let transform_topic_for_server topic =
  let open Directory in
  let rec aux acc = function
    | [] -> List.rev acc
    | "*" :: rest -> aux (Star :: acc) rest
    | "#" :: rest -> aux (Hashtag :: acc) rest
    | word :: rest -> aux (Word word :: acc) rest
  in
  aux [] (Topic.to_list topic)
;;

let on_client_to_server_msg ~resgister ~remove ws msg =
  let open Http.Websocket in
  let convo_conn = ws.server_state in
  match msg with
  | Client_events.Leave_topic topic -> remove topic
  | Join_topic topic ->
    let id =
      Topic_server.Client.subscribe
        ~connection:ws.server_state
        ~topic:(transform_topic_for_server topic)
        ~f:(function
          | Closed _ -> Continue
          | Update msg ->
            ws.send msg;
            Continue)
    in
    resgister id topic
  | Send_message msg ->
    Topic_server.Client.publish
      ~connection:convo_conn
      ~topic:(transform_topic_for_server msg.topic)
      ~message:(New_message msg)
    >>| fun () -> None
;;

let on_ws_connect ws =
  let open Http.Websocket in
  let topic_subs_id = Hashtbl.create (module Topic) in
  let resgister id topic =
    Hashtbl.set topic_subs_id ~key:topic ~data:id;
    return None
  in
  let remove topic =
    match Hashtbl.find_and_remove topic_subs_id topic with
    | None -> return None
    | Some id ->
      let%bind connection = Persistent_connection.Rpc.connected ws.server_state in
      (match%bind id with
      | Error _ -> Deferred.return None
      | Ok id ->
        Rpc.Pipe_rpc.abort Topic_server.subscribe_rpc connection id;
        Deferred.return None)
  in
  let on_close () =
    Hashtbl.iter topic_subs_id ~f:(fun id ->
        (let%bind connection = Persistent_connection.Rpc.connected ws.server_state in
         match%bind id with
         | Error _ -> Deferred.return ()
         | Ok id ->
           Rpc.Pipe_rpc.abort Topic_server.subscribe_rpc connection id;
           Deferred.return ())
        |> don't_wait_for)
  in
  let on_message msg =
    let on_message' =
      match%bind on_client_to_server_msg ~resgister ~remove ws msg with
      | None -> return ()
      | Some res ->
        ws.send res;
        return ()
    in
    don't_wait_for on_message'
  in
  { Http.Websocket.on_message; on_close }
;;
