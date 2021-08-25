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

type t =
  { id : Id.t
  ; org_id : Organization.Id.t
  ; first_name : string
  ; last_name : string
  }
[@@deriving sexp, bin_io, compare, fields]

module Agent_convo_tags = struct
  module Agent_id = Id

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

  type t =
    { id : Id.t
    ; label : string
    ; color : string
    ; agent_id : Agent_id.t
    }
  [@@deriving sexp, bin_io, compare, fields]

  (*Should this be in the module DB_query at line 64?*)
  module Db_query = struct
    let get_agent_convo_tags =
      [%rapper
        get_many
          {sql|
        SELECT @Id{id}, 
        @string{label}, 
        @string{color}, 
        @Agent_id{agent_id} 
        FROM agent_convo_tags tags WHERE tags.agent_id = %Agent_id{agent_id} AND tags.org_id = %Organization.Id{org_id}
      |sql}
          record_out]
    ;;
  end
end

module Db_query = struct
  module Org_id = Organization.Id

  let create_agent =
    [%rapper
      get_one
        {sql|
        SELECT create_agent(
            %Org_id{org_id}::uuid,
            %string{first_name}::text,
            %string{last_name}::text
            ) as @Id{id}

        |sql}]
  ;;

  let get_agent_by_org_id_and_email =
    [%rapper
      get_one
        {sql|
        SELECT @Id{id} 
        FROM get_agent_by_org_id_and_email(
          %Org_id{org_id}::uuid,
          %string{email}::string
        )
        |sql}]
  ;;

  let get_agent_by_id =
    [%rapper
      get_opt
        {sql|
        SELECT @Id{id},
          @Org_id{org_id},
          @string{first_name},
          @string{last_name} 
        FROM get_agent_by_id(
          %Id{agent_id}::uuid
          %Org_id{org_id}::uuid
        )
      |sql}
        record_out]
  ;;
end
