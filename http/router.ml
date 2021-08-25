open Async

type routeResponseT =
  { status : Httpaf.Status.t
  ; headers : (Httpaf.Headers.name * Httpaf.Headers.value) list option
  ; body : string
  }

type routeFn = Httpaf.Request.t -> string -> routeResponseT option Deferred.t

let createResponse ~status ?headers body =
  Deferred.return (Some { status; headers; body })
;;

type requestHandler =
  string list -> Httpaf.Request.t -> string -> routeResponseT option Deferred.t

type routerList = requestHandler list

let rec run_router (routers : routerList) path req body =
  match routers with
  | head :: tail ->
    (match%bind head path req body with
    | Some _ as res -> Deferred.return res
    | None -> run_router tail path req body)
  | [] -> Deferred.return None
;;

let check_req_path ~path (req : Httpaf.Request.t) =
  Uri.of_string req.target
  |> Uri.path
  |> String.split_on_char '?'
  |> List.hd
  |> String.equal path
;;

let get_path (req : Httpaf.Request.t) =
  Uri.of_string req.target |> Uri.path |> String.split_on_char '/' |> List.tl
;;

let get_query (req : Httpaf.Request.t) = Uri.of_string req.target |> Uri.query

let create_router routerFn (req : Httpaf.Request.t) body_ =
  let body =
    match body_ with
    | Some str -> str
    | None -> ""
  in
  let path =
    Uri.of_string req.target |> Uri.path |> String.split_on_char '/' |> List.tl
  in
  routerFn path req body
;;
