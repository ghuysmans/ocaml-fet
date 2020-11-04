type t = string

let header = ["Subject"]

let of_list = function
  | [s] -> s
  | _ -> failwith "Subjects.of_list"

let to_list s =
  [s]
