open Core
open Python_lib
open Python_lib.Let_syntax

let hello =
  let%map_open name = positional "name" string ~docstring:"name" in
  fun () ->
    print_endline ("hello " ^ name);
    Py.none
;;

let () =
  if not (Py.is_initialized ()) then Py.initialize ();
  let mod_ = Py_module.create "ocaml_module" in
  Py_module.set mod_ "hello" ~docstring:"prints hello <name>" hello
;;
