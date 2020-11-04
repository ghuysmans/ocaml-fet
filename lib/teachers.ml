type t = Teacher.t

let headers = ["Teacher"]

let of_list = function
  | [t] -> Teacher.of_string t
  | _ -> failwith "Teachers.of_list"

let to_list t =
  [No_plus.to_string t]
