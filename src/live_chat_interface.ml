open Core

module Server_events = struct
  type t =
    | Topic_join of Topic.t * User.t
    | Topic_leave of Topic.t * User.t
    | New_message of Topic.Message.t
  [@@deriving sexp, bin_io, compare, yojson]
end

module Client_events = struct
  type t =
    | Leave_topic of Topic.t
    | Send_message of Topic.Message.t
    | Join_topic of Topic.t
  [@@deriving sexp, bin_io, compare, yojson]
end

module Websocket_interface = struct
  type client_to_server = Client_events.t
  type server_to_client = Server_events.t
end
