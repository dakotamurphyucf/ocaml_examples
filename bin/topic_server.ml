open Core
open Async
open Lib
open Live_chat_interface
module Topic_server = Live_chat_server.Topic_server
module Server = Topic_server.Server
module Client = Topic_server.Client

module Start_command = struct
  let command =
    Command.async
      ~summary:"Start a Topic server"
      Command.Let_syntax.(
        let%map_open port =
          flag ~aliases:[ "-port" ] "-p" (optional_with_default 3000 int) ~doc:"port"
        in
        fun () ->
          print_endline (Printf.sprintf "starting topic server on port %i" port);
          Server.start ~port >>= Deferred.never)
  ;;
end

module Publish_command = struct
  let command =
    Command.async
      ~summary:"publish to topic server"
      Command.Let_syntax.(
        let%map_open host =
          flag
            ~aliases:[ "-host" ]
            "-h"
            (optional_with_default "localhost" string)
            ~doc:"host"
        and port =
          flag ~aliases:[ "-port" ] "-p" (optional_with_default 3000 int) ~doc:"port"
        and topic_string =
          flag ~aliases:[ "-topic" ] "-t" (optional_with_default "#" string) ~doc:"topic"
        and sender =
          flag
            ~aliases:[ "-sender" ]
            "-s"
            (optional_with_default "anonymous" string)
            ~doc:"sender"
        and message = anon ("message" %: string) in
        fun () ->
          let topic = Live_chat_server.transform_topic_for_server topic_string in
          let timestamp = Time_ns.(to_int_ns_since_epoch (now ())) in
          Client.publish
            ~connection:(Client.get_connection ~host ~port)
            ~topic
            ~message:
              (New_message { topic = topic_string; timestamp; text = message; sender }))
  ;;
end

module Subscribe_command = struct
  let unsubscribe_on_quit ~id ~connection ~user ~topic ~topic_string =
    Async.Signal.handle [ Async.Signal.quit; Signal.int ] ~f:(fun s ->
        (let%bind () =
           Client.publish ~connection ~topic ~message:(Topic_leave (topic_string, user))
         in
         match%bind id with
         | Error _ -> Deferred.return (Async.shutdown (Async.Signal.to_caml_int s))
         | Ok id ->
           let%bind conn = Persistent_connection.Rpc.connected connection in
           Rpc.Pipe_rpc.abort Topic_server.subscribe_rpc conn id;
           Deferred.return (Async.shutdown (Async.Signal.to_caml_int s)))
        |> don't_wait_for)
  ;;

  let command =
    Command.async
      ~summary:"This is summarry"
      Command.Let_syntax.(
        let%map_open host =
          flag
            ~aliases:[ "-host" ]
            "-h"
            (optional_with_default "localhost" string)
            ~doc:"host"
        and port =
          flag ~aliases:[ "-port" ] "-p" (optional_with_default 3000 int) ~doc:"port"
        and topic_string =
          flag ~aliases:[ "-topic" ] "-t" (optional_with_default "#" string) ~doc:"topic"
        and subscriber =
          flag
            ~aliases:[ "-subscriber" ]
            "-sub"
            (optional_with_default "anonymous" string)
            ~doc:"subscriber"
        and country =
          flag
            ~aliases:[ "-country" ]
            "-c"
            (optional_with_default "anonymous" string)
            ~doc:"topic"
        in
        fun () ->
          let topic = Live_chat_server.transform_topic_for_server topic_string in
          let user = { User.name = subscriber; country } in
          let connection = Client.get_connection ~host ~port in
          let id =
            Topic_server.Client.subscribe ~connection ~topic ~f:(function
                | Closed _ -> Continue
                | Update msg ->
                  print_s [%sexp (msg : Server_events.t)];
                  Continue)
          in
          unsubscribe_on_quit ~id ~connection ~user ~topic ~topic_string;
          Client.publish ~connection ~topic ~message:(Topic_join (topic_string, user))
          >>= Deferred.never)
  ;;
end

let command =
  let commands =
    [ "start", Start_command.command
    ; "publish", Publish_command.command
    ; "subscribe", Subscribe_command.command
    ]
  in
  Command.group ~summary:"Commands" commands
;;

let () = Command_unix.run command
