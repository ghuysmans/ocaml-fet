type t = string

let headers = [| "Subject" |]

let of_array = function
  | [| s |] -> s
  | _ -> failwith "Subjects.of_array"

let to_array s =
  [| s |]
