type 'a t = 'a list constraint 'a = _ No_plus.t

val of_string : string -> 'a t
val to_string : 'a t -> string
