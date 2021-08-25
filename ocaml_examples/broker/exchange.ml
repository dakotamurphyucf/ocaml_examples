open Core
open Async

module type Topic = sig
  type t [@@deriving sexp, bin_io, hash, compare]

  val to_string : t -> string
end

module type Message = sig
  type t [@@deriving sexp, bin_io, compare]
end

module type Exchange = sig
  module Topic : Topic
  module Message : Message

  type t

  val version : int
  val create : unit -> t
  val publish : t -> Topic.t -> Message.t -> unit

  val subscribe
    :  t
    -> Topic.t
    -> Message.t Rpc.Pipe_rpc.Direct_stream_writer.t
    -> (unit, string) Result.t

  val close_topic
    :  root:t
    -> topic:Topic.t
    -> closing_msg:Message.t option
    -> unit Deferred.t

  val add_topic : t -> Topic.t -> unit
end

module MakeExchange (Exchange : Exchange) = struct
  include Exchange

  type publish_payload =
    { topic : Exchange.Topic.t
    ; message : Exchange.Message.t
    }
  [@@deriving sexp, bin_io, compare]

  type close_topic_payload =
    { topic : Exchange.Topic.t
    ; closing_msg : Exchange.Message.t option
    }
  [@@deriving sexp, bin_io, compare]

  let publish_rpc =
    Rpc.Rpc.create
      ~name:"publish"
      ~version:Exchange.version
      ~bin_query:bin_publish_payload
      ~bin_response:Unit.bin_t
  ;;

  let subscribe_rpc =
    Rpc.Pipe_rpc.create
      ()
      ~name:"subscribe"
      ~version:Exchange.version
      ~bin_query:Exchange.Topic.bin_t
      ~bin_response:Exchange.Message.bin_t
      ~bin_error:String.bin_t
  ;;

  let close_rpc =
    Rpc.Rpc.create
      ~name:"close"
      ~version:Exchange.version
      ~bin_query:bin_close_topic_payload
      ~bin_response:Unit.bin_t
  ;;

  let add_rpc =
    Rpc.Rpc.create
      ~name:"add"
      ~version:Exchange.version
      ~bin_query:Exchange.Topic.bin_t
      ~bin_response:Unit.bin_t
  ;;

  module Client = struct
    let get_connection ~host ~port =
      let host_and_port = Host_and_port.create ~host ~port in
      let event =
        (function
         | Obtained_address address ->
           print_s [%sexp (address : Host_and_port.t)];
           return ()
         | event ->
           print_s [%sexp (event : Persistent_connection.Rpc.Event.t)];
           return ()
          : Persistent_connection.Rpc.Event.t -> unit Deferred.t)
      in
      let unversioned_conn =
        Persistent_connection.Rpc.create'
          ~on_event:event
          ~server_name:"Exchange rpc"
          (fun () -> return (Ok host_and_port))
      in
      unversioned_conn
    ;;

    let publish ~connection ~topic ~message =
      let%bind connection = Persistent_connection.Rpc.connected connection in
      Rpc.Rpc.dispatch_exn publish_rpc connection { topic; message }
    ;;

    let subscribe ~connection ~topic ~f =
      let%bind connection = Persistent_connection.Rpc.connected connection in
      match%bind Rpc.Pipe_rpc.dispatch_iter subscribe_rpc connection topic ~f with
      | Error err -> Error.raise err
      | Ok (Error s) ->
        eprintf "subscribe failed: %s\n" s;
        return (Error s)
      | Ok (Ok id) -> return (Ok id)
    ;;

    let close ~connection ~topic ~closing_msg =
      let%bind connection = Persistent_connection.Rpc.connected connection in
      Rpc.Rpc.dispatch_exn close_rpc connection { topic; closing_msg }
    ;;

    let add_topic ~connection ~topic =
      let%bind connection = Persistent_connection.Rpc.connected connection in
      Rpc.Rpc.dispatch_exn add_rpc connection topic
    ;;
  end

  module Server = struct
    let publish_impl exchange payload =
      Log.Global.sexp ~level:`Debug [%sexp (payload.message : Exchange.Message.t)];
      return (Exchange.publish exchange payload.topic payload.message)
    ;;

    let subscribe_impl exchange topic writer =
      return (Exchange.subscribe exchange topic writer)
    ;;

    let close_impl exchange payload =
      Log.Global.info "Clearing topic %s" (Exchange.Topic.to_string payload.topic);
      Exchange.close_topic
        ~root:exchange
        ~topic:payload.topic
        ~closing_msg:payload.closing_msg
    ;;

    let add_impl exchange topic =
      Log.Global.info "Clearing topic %s" (Exchange.Topic.to_string topic);
      Exchange.add_topic exchange topic |> return
    ;;

    let implementations =
      [ Rpc.Rpc.implement publish_rpc publish_impl
      ; Rpc.Pipe_rpc.implement_direct subscribe_rpc subscribe_impl
      ; Rpc.Rpc.implement close_rpc close_impl
      ; Rpc.Rpc.implement add_rpc add_impl
      ]
    ;;

    let server ~port =
      let exchange = Exchange.create () in
      let implementations =
        Rpc.Implementations.create_exn
          ~implementations
          ~on_unknown_rpc:
            (`Call
              (fun _ ~rpc_tag ~version ->
                Log.Global.info "Unexpected RPC, tag %s, version %d" rpc_tag version;
                `Continue))
      in
      let%bind _ =
        Rpc.Connection.serve
          ~implementations
          ~initial_connection_state:(fun _addr _conn -> exchange)
          ~where_to_listen:(Tcp.Where_to_listen.of_port port)
          ()
      in
      Deferred.return ()
    ;;
  end
end
