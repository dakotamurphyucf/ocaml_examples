open Core

module Main_command = struct
  let command =
    Command.basic
      ~summary:"This is summarry"
      Command.Let_syntax.(
        let%map_open name =
          flag
            ~aliases:[ "-name" ]
            "-n"
            (optional_with_default "dakota" string)
            ~doc:"name"
        in
        fun () -> print_endline (Printf.sprintf "name: %s" name))
  ;;
end

let command =
  let commands = [ "main", Main_command.command ] in
  Command.group ~summary:"Commands" commands
;;

let () = Command_unix.run command
