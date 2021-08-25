module type S = sig
  type 'a io

  val body
    :  Httpaf.Response.t * Httpaf.Body.Reader.t
    -> (string, [> `Reading_error ]) result io
end

module Make (M : S) : S with type 'a io = 'a M.io = struct
  type 'a io = 'a M.io

  let body = M.body
end
