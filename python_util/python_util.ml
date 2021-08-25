open Core
open Python_lib

module type Pythonable = sig
  type t

  val t_of_python : pyobject -> t
  val python_of_t : t -> pyobject
end

module Make_python_dict (P : Pythonable) = struct
  type t = Pytypes.pyobject

  let set t key value =
    let key = P.python_of_t key in
    Py.Dict.set_item t key value
  ;;

  let find t key =
    let key = P.python_of_t key in
    Py.Dict.find t key
  ;;

  let create assoc =
    let t = Py.Dict.create () in
    List.iter assoc ~f:(fun (key, value) -> set t key value);
    t
  ;;
end

module String = struct
  include String

  let t_of_python str = string_of_python str
  let python_of_t str = python_of_string str
end

module Int = struct
  include Int

  let t_of_python obj = int_of_python obj
  let python_of_t n = python_of_int n
end

let python_of_tbl
    (type b a)
    (module M : Hashtbl.S with type key = b)
    (module P : Pythonable with type t = a)
    (module K : Pythonable with type t = b)
    tbl
  =
  let assoc =
    M.to_alist tbl |> List.map ~f:(fun (key, value) -> key, P.python_of_t value)
  in
  let module D = Make_python_dict (K) in
  D.create assoc
;;

let tbl_of_python
    (type b a)
    (module M : Hashtbl.S with type key = b)
    (module P : Pythonable with type t = a)
    (module K : Pythonable with type t = b)
    value
  =
  let assoc =
    Py.Dict.to_bindings value
    |> List.map ~f:(fun (key, value) -> K.t_of_python key, P.t_of_python value)
  in
  M.of_alist_exn assoc
;;

let tbl_of_python_int
    (type b)
    (module M : Hashtbl.S with type key = int)
    (module P : Pythonable with type t = b)
    value
  =
  let assoc =
    Py.Dict.to_bindings value
    |> List.map ~f:(fun (key, value) -> int_of_python key, P.t_of_python value)
  in
  M.of_alist_exn assoc
;;

module Make_python_hashble
    (K : Pythonable)
    (V : Pythonable)
    (M : Hashtbl.S with type key = K.t) =
struct
  include M

  let t_of_python obj = tbl_of_python (module M) (module V) (module K) obj
  let python_of_t tbl = python_of_tbl (module M) (module V) (module K) tbl
end

module Int_tbl = Make_python_hashble (Int) (Int) (Hashtbl.Make_binable (Int))
module String_tbl = Make_python_hashble (String) (String) (Hashtbl.Make_binable (String))
