open Core

module T = struct
  type t = string [@@deriving sexp, bin_io, compare, yojson, hash]
end

include T

let to_list t = String.split t ~on:'.'

module Message = struct
  type t =
    { topic : T.t
    ; timestamp : int
    ; text : string
    ; sender : string
    }
  [@@deriving sexp, bin_io, compare, fields, yojson]
end
