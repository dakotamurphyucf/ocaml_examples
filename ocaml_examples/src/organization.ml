open Core

module Id = struct
  module Id_internal : Identifiable = String
  include Id_internal

  let t =
    let encode id = Ok (to_string id) in
    let decode str = Ok (of_string str) in
    let open Caqti_type in
    custom ~encode ~decode string
  ;;
end

module Domain = struct
  module Domain_internal : Identifiable = String
  include Domain_internal

  let t =
    let encode id = Ok (to_string id) in
    let decode str = Ok (of_string str) in
    let open Caqti_type in
    custom ~encode ~decode string
  ;;
end

type t =
  { id : Id.t
  ; name : string
  ; domain : Domain.t
  }
[@@deriving sexp, bin_io]

module Db_query = struct
  let create_orginazation =
    [%rapper
      get_one
        {sql|
        SELECT add_org(
            %string{name}::string,
            %Domain{domain}::string
        ) as @Id{id}
      |sql}]
  ;;

  let get_org_by_id =
    [%rapper
      get_one
        {sql|
        SELECT get_org_by_id(
          %Id{id}::uuid
        ) as @string{name}
      |sql}]
  ;;

  let get_org_by_domain =
    [%rapper
      get_one
        {sql|
          SELECT get_org_by_domain(
            %Domain{domain}::text
          ) as @Id{id}
      |sql}]
  ;;
end
