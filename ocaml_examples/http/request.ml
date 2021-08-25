type t =
  { req : Httpaf.Request.t
  ; uri : Uri.t
  ; body : string option
  ; headers : (string * string) list
  }

let create =
  (fun ?(headers = []) ?(body = "") meth uri ->
     let host = Uri.host_with_default uri in
     let content_length = body |> String.length |> string_of_int in
     let headers = [ "Host", host; "Content-Length", content_length ] @ headers in
     { req =
         Httpaf.Request.create
           ~headers:(Httpaf.Headers.of_list headers)
           meth
           (uri |> Uri.to_string)
     ; uri
     ; headers
     ; body =
         (match body with
         | "" -> None
         | _ -> Some body)
     }
    : ?headers:(string * string) list -> ?body:string -> Httpaf.Method.t -> Uri.t -> t)
;;

let as_httpaf req = req.req
let body req = req.body
let headers req = req.headers
let uri req = req.uri

module type S = sig
  type 'a io
  type config

  val send
    :  ?config:config
    -> t
    -> ( Httpaf.Response.t * Httpaf.Body.Reader.t
       , [> `Connection_error of Httpaf.Client_connection.error ] )
       result
       io
end

module Make (M : S) : S with type 'a io = 'a M.io and type config = M.config = struct
  type 'a io = 'a M.io
  type config = M.config

  let send = M.send
end

module Headers = struct
  let json = "Content-Type", "application/json"
  let form_url_encoded = "Content-Type", "application/x-www-form-urlencoded"
  let text = "Content-Type", "text/plain"
  let html = "Content-Type", "text/html"
  let xml = "Content-Type", "application/xml"
  let add_header header (req : t) = { req with headers = header :: req.headers }
  let user_agent_header agent = "User-Agent", agent
  let basic_auth_header token = "Authorization", "Basic " ^ token
end

let set_body body (req : t) = { req with body = Some body }

let set_content_type content (req : t) =
  match content with
  | `JSON -> req |> Headers.add_header Headers.json
  | `FORM_URL_ENCODE -> req |> Headers.add_header Headers.form_url_encoded
  | `TEXT -> req |> Headers.add_header Headers.text
  | `HTML -> req |> Headers.add_header Headers.html
  | `XML -> req |> Headers.add_header Headers.xml
;;

let set_user_agent agent req = req |> Headers.add_header (Headers.user_agent_header agent)
let set_basic_auth token req = req |> Headers.add_header (Headers.basic_auth_header token)
