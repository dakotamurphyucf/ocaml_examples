open Core

type t =
  { name : string
  ; country : string
  }
[@@deriving sexp, bin_io, compare, fields, yojson]
