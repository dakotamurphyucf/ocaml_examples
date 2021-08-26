#!/usr/bin/env utop

#require "shexp.process"

#require "core"

open Core
module Process = Shexp_process
module P = Process
open Shexp_process
open Shexp_process.Infix

(* build script for python bindings. Make sure to run from this directory *)
let package = "python_ocaml"
let dll = "pywrap.so"
let build_ocaml = call [ "dune"; "build" ]
let copy_so = call [ "cp"; "_build/default/bin/" ^ dll; package ^ "/" ]
let install = call [ "/bin/bash"; "-c"; "source ./env/bin/activate; pip install ." ]
let main : unit t = build_ocaml >> copy_so >> install
let () = eval main
