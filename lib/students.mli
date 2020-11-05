open Class

type t =
  | Year of Year.t * int
  | Group of Year.t * Group.t * int
  | Subgroup of Year.t * Group.t * Subgroup.t * int

val header : string list

val of_list : string list -> t
val to_list : t -> string list

type tree = (
  Year.t * int * (
    Group.t * int * (
      Subgroup.t * int
    ) list
  ) list
) list

val tree_to_list : tree -> string list list
