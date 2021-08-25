module Id : sig
  module Id_internal : Core.Identifiable
  include module type of Id_internal

  val t : t Caqti_type.t
end

module Domain : sig
  module Domain_internal : Core.Identifiable
  include module type of Domain_internal

  val t : t Caqti_type.t
end

type t =
  { id : Id.t
  ; name : string
  ; domain : Domain.t
  }
[@@deriving sexp, bin_io]

module Db_query : sig
  val create_orginazation
    :  name:string
    -> domain:Domain.t
    -> (module Caqti_async.CONNECTION)
    -> (Id.t, [> Caqti_error.call_or_retrieve ]) result Async.Deferred.t

  val get_org_by_id
    :  id:Id.t
    -> (module Caqti_async.CONNECTION)
    -> (string, [> Caqti_error.call_or_retrieve ]) Core.result Async.Deferred.t

  val get_org_by_domain
    :  domain:Domain.t
    -> (module Caqti_async.CONNECTION)
    -> (Id.t, [> Caqti_error.call_or_retrieve ]) Core.result Async.Deferred.t
end
