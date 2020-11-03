type t = Activities.Tag.t

let headers = [| "Activity Tag" |]

let of_array = function
  | [| t |] -> Activities.Tag.of_string t
  | _ -> failwith "Activity_tags.of_array"

let to_array t =
  [| No_plus.to_string t |]
