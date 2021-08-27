open Core
open Lib

let%expect_test "test" =
  let user = { User.name = "dakota"; country = "united states" } in
  print_s [%sexp (user : User.t)];
  [%expect {| ((name dakota) (country united states)) |}]
;;
