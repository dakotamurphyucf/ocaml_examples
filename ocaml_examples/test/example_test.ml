open Core
open Lib

let%expect_test "test" =
  let user = { User.name = "dakota"; age = 27 } in
  print_s [%sexp (user : User.t)];
  [%expect {| ((name dakota) (age 27)) |}]
;;
