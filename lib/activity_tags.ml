type t = Activities.Tag.t

let header = ["Activity Tag"]

let of_list = function
  | [t] -> Activities.Tag.of_string t
  | _ -> failwith "Activity_tags.of_list"

let to_list t =
  [No_plus.to_string t]
