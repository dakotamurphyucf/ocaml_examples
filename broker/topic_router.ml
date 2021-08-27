open Core
open Async
open Exchange

module Make (Message : Message) = struct
  module Topic = struct
    module Directory = struct
      type t =
        | Wildcard's
        | Wildcard
        | Name of string
      [@@deriving sexp, hash, bin_io, compare]
    end

    type t = Directory.t list [@@deriving sexp, bin_io, hash, compare]

    let string_of_directory = function
      | Directory.Wildcard's -> "#"
      | Wildcard -> "*"
      | Name str -> str
    ;;

    let to_string = function
      | [] -> ""
      | hd :: rest ->
        List.fold rest ~init:(string_of_directory hd) ~f:(fun a b ->
            a ^ "/" ^ string_of_directory b)
    ;;
  end

  module Message = Message
  module Group = Rpc.Pipe_rpc.Direct_stream_writer.Group

  type t =
    { node_type : node_type
    ; children : (Topic.Directory.t, t) Core.Hashtbl.t
    ; group : Message.t Group.t
    ; mutable last_message : Message.t option
    }

  and node_type =
    | Root
    | Leaf of t * Topic.Directory.t

  let version = 1

  let create () =
    { node_type = Root
    ; children = Hashtbl.create (module Topic.Directory) ~growth_allowed:true ~size:500
    ; group = Group.create ()
    ; last_message = None
    }
  ;;

  module Topic_cache = Hash_queue.Make (Topic)

  let max_cache_size = 500
  let topic_cache = lazy (Topic_cache.create ())

  let remove_topic_from_cache topic =
    ignore (Topic_cache.remove (Lazy.force topic_cache) topic : [ `No_such_key | `Ok ])
  ;;

  let rec clean_unused_nodes node =
    match Hashtbl.is_empty node.children && Group.length node.group = 0 with
    | false -> ()
    | true ->
      (match node.node_type with
      | Root -> ()
      | Leaf (parent, dir) ->
        Hashtbl.remove parent.children dir;
        clean_unused_nodes parent)
  ;;

  let rec get_leaf' root topic =
    match topic with
    | [] -> Some root
    | hd :: tl ->
      (match Hashtbl.find root.children hd with
      | None -> None
      | Some node -> get_leaf' node tl)
  ;;

  let get_leaf root topic =
    let cache = Lazy.force topic_cache in
    match Topic_cache.lookup_and_move_to_back cache topic with
    | Some result -> Some result
    | None ->
      let result = get_leaf' root topic in
      (match result with
      | None -> ()
      | Some node ->
        Topic_cache.enqueue_back_exn cache topic node;
        if Topic_cache.length cache > max_cache_size
        then ignore (Topic_cache.dequeue_front_exn cache : t));
      result
  ;;

  let close_subscriber_streams t =
    List.iter (t.group |> Group.to_list) ~f:Rpc.Pipe_rpc.Direct_stream_writer.close
  ;;

  let close_topic ~root ~topic ~closing_msg =
    match get_leaf root topic with
    | None -> return ()
    | Some node ->
      [%bind
        let () =
          match closing_msg with
          | None -> return ()
          | Some msg -> Group.write node.group msg
        in
        remove_topic_from_cache topic;
        close_subscriber_streams node;
        clean_unused_nodes node;
        return ()]
  ;;

  let find_or_add_topic root topic =
    let rec aux topic node =
      match topic with
      | [] -> node
      | hd :: tl ->
        let next_node =
          Hashtbl.find_or_add node.children hd ~default:(fun () ->
              let children =
                Hashtbl.create (module Topic.Directory) ~growth_allowed:true ~size:50
              in
              { node_type = Leaf (node, hd)
              ; children
              ; group = Group.create ()
              ; last_message = None
              })
        in
        aux tl next_node
    in
    match get_leaf root topic with
    | None -> aux topic root
    | Some node -> node
  ;;

  let add_topic root topic = ignore (find_or_add_topic root topic : t)

  let subscribe root topic writer =
    let open Rpc.Pipe_rpc.Direct_stream_writer in
    let node = find_or_add_topic root topic in
    Group.add_exn node.group writer;
    (closed writer
    >>> fun () ->
    if Group.length node.group = 0
    then (
      remove_topic_from_cache topic;
      clean_unused_nodes node));
    match node.last_message with
    | None -> Ok ()
    | Some msg -> Ok (ignore (write_without_pushback writer msg : [ `Closed | `Ok ]))
  ;;

  let publish root topic msg =
    let rec aux t nodes =
      match t with
      | [] ->
        List.iter nodes ~f:(fun node ->
            node.last_message <- Some msg;
            Group.write_without_pushback node.group msg)
      | hd :: tl ->
        let nodes =
          List.fold_left nodes ~init:[] ~f:(fun nodes node ->
              let nodes =
                match hd with
                | Topic.Directory.Wildcard's ->
                  (match Hashtbl.find node.children Wildcard's with
                  | None -> nodes
                  | Some node -> node :: nodes)
                | Wildcard ->
                  let nodes =
                    match Hashtbl.find node.children Wildcard with
                    | None -> nodes
                    | Some node -> node :: nodes
                  in
                  (match Hashtbl.find node.children Wildcard's with
                  | None -> nodes
                  | Some node -> node :: nodes)
                | Name _ as hd ->
                  let nodes =
                    match Hashtbl.find node.children hd with
                    | None -> []
                    | Some node -> [ node ]
                  in
                  let nodes =
                    match Hashtbl.find node.children Wildcard with
                    | None -> nodes
                    | Some node -> node :: nodes
                  in
                  (match Hashtbl.find node.children Wildcard's with
                  | None -> nodes
                  | Some node -> node :: nodes)
              in
              match node.node_type with
              | Root -> nodes
              | Leaf (_, dir) ->
                (match dir with
                | Wildcard's -> node :: nodes
                | Wildcard | Name _ -> nodes))
        in
        aux tl nodes
    in
    aux topic [ root ]
  ;;
end
