module Tag : sig
  type t = [`Activity_tag] No_plus.t
  val of_string : string -> t
end

type t = private {
  students: Class.t Plus.t;
  subject: string;
  teachers: Teacher.t Plus.t;
  tags: Tag.t Plus.t;
  duration: (int * int list) option;
  min_days: int option;
  weight: float option;
  consecutive: bool option;
}

val make :
  ?students:'a Class.any Plus.t ->
  ?teachers:Teacher.t Plus.t ->
  ?tags:Tag.t Plus.t ->
  ?duration:(int * int list) ->
  ?min_days:int ->
  ?weight:float ->
  ?consecutive:bool ->
  string ->
  t

val header : string list
val of_list : string list -> t
val to_list : t -> string list
