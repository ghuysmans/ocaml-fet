type t = private {
  students: Plus.t;
  subject: string;
  teachers: Plus.t;
  tags: Plus.t;
  duration: (int * int list) option;
  min_days: int option;
  weight: float option;
  consecutive: bool option;
}

val make :
  ?students:Plus.t ->
  ?teachers:Plus.t ->
  ?tags:Plus.t ->
  ?duration:(int * int list) ->
  ?min_days:int ->
  ?weight:float ->
  ?consecutive:bool ->
  string ->
  t

val headers : string array
val of_array : string array -> t
val to_array : t -> string array
