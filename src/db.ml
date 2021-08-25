open Core
open Async

module Postgres = struct
  let connection_url = "postgresql://postgres:password@localhost:5432"

  let pool =
    match Caqti_async.connect_pool ~max_size:10 (Uri.of_string connection_url) with
    | Ok pool -> pool
    | Error err -> failwith (Caqti_error.show err)
  ;;

  type error = Database_error of string

  module type db = Caqti_async.CONNECTION

  let or_error m =
    match%bind m with
    | Ok a -> Ok a |> Async.return
    | Error e -> Error (Database_error (Caqti_error.show e)) |> Async.return
  ;;

  let run_query value = Caqti_async.Pool.use value pool |> or_error

  let run_transaction fn =
    let txn (module C : db) =
      match%bind C.start () with
      | Error e -> Error e |> Async.return
      | Ok () ->
        (match%bind fn (module C : db) with
        | Error _ as res ->
          (match%bind C.rollback () with
          | Error err ->
            Logs.err (fun m ->
                m "Error rolling back transaction with err %s" (Caqti_error.show err));
            return res
          | Ok () -> return res)
        | Ok _ as res ->
          (match%bind C.commit () with
          | Error err as err_res ->
            Logs.err (fun m ->
                m "Error commiting transaction with err %s" (Caqti_error.show err));
            return err_res
          | Ok () -> return res))
    in
    txn |> run_query
  ;;
end
