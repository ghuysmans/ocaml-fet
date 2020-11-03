type t = Activity_tag.t

let headers = [| "Activity Tag" |]

let of_array = function
  | [| t |] -> Activity_tag.of_string t
  | _ -> failwith "Activity_tags.of_array"

let to_array t =
  [| Activity_tag.to_string t |]
