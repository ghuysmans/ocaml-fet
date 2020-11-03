type t = Teacher.t

let headers = [| "Teacher" |]

let of_array = function
  | [| t |] -> Teacher.of_string t
  | _ -> failwith "Teachers.of_array"

let to_array t =
  [| Teacher.to_string t |]
