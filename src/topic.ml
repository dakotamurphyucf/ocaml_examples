open Core

module T = struct
  type t = string list [@@deriving sexp, bin_io, compare, yojson, hash]
end

include T

let to_list t = t

module Message = struct
  type t =
    { topic : T.t
    ; timestamp : int
    ; text : string
    ; sender : User.t
    }
  [@@deriving sexp, bin_io, compare, fields, yojson]
end
