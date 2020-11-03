module Year : sig
  type t = [`Year] No_plus.t
  val of_string : string -> t
end

module Group : sig
  type t = [`Group] No_plus.t
  val of_string : string -> t
end

module Subgroup : sig
  type t = [`Subgroup] No_plus.t
  val of_string : string -> t
end

type t = [`Year | `Group | `Subgroup] No_plus.t
val of_string : string -> t
