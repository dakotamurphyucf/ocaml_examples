open Core

type t =
  { name : string
  ; age : int
  }
[@@deriving sexp, bin_io, compare, fields, yojson]
