open Core

type 'a ws_connection_handler =
  { on_message : 'a -> unit
  ; on_close : unit -> unit
  }

type ('a, 'state) ws_connection =
  { path : string list
  ; query : (string * string list) list
  ; wsd : Websocketaf.Wsd.t
  ; send : 'a -> unit
  ; server_state : 'state
  ; req : Httpaf.Request.t
  }

let regexComma = Str.regexp_string ","

let upgrade_present headers =
  Httpaf.Headers.get_multi headers "connection"
  |> List.map ~f:(fun hs -> Str.split regexComma hs)
  |> List.join
  |> List.exists ~f:(fun h ->
         String.lowercase h |> String.strip |> String.equal "upgrade")
;;

let default_ws_path = Router.check_req_path ~path:"/ws"
