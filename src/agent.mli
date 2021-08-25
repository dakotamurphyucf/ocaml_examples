module Id : sig
  module Id_internal : Core.Identifiable
  include module type of Id_internal

  val t : t Caqti_type.t
end

type t =
  { id : Id.t
  ; org_id : Organization.Id.t
  ; first_name : string
  ; last_name : string
  }
[@@deriving sexp, bin_io, compare, fields]

module Agent_convo_tags : sig
  module Agent_id = Id

  module Id : sig
    module Id_internal : Core.Identifiable
    include module type of Id_internal

    val t : t Caqti_type.t
  end

  type t =
    { id : Id.t
    ; label : string
    ; color : string
    ; agent_id : Agent_id.t
    }
  [@@deriving sexp, bin_io, compare, fields]

  module Db_query : sig
    val get_agent_convo_tags
      :  agent_id:Agent_id.t
      -> org_id:Organization.Id.t
      -> (module Caqti_async.CONNECTION)
      -> (t list, [> Caqti_error.call_or_retrieve ]) result Async.Deferred.t
  end
end

module Db_query : sig
  module Org_id = Organization.Id

  val create_agent
    :  org_id:Org_id.t
    -> first_name:string
    -> last_name:string
    -> (module Caqti_async.CONNECTION)
    -> (Id.t, [> Caqti_error.call_or_retrieve ]) Core.result Async.Deferred.t

  val get_agent_by_org_id_and_email
    :  org_id:Org_id.t
    -> email:string
    -> (module Caqti_async.CONNECTION)
    -> (Id.t, [> Caqti_error.call_or_retrieve ]) Core.result Async.Deferred.t

  val get_agent_by_id
    :  agent_id:Id.t
    -> org_id:Org_id.t
    -> (module Caqti_async.CONNECTION)
    -> (t option, [> Caqti_error.call_or_retrieve ]) Core.result Async.Deferred.t
end
