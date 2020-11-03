type t = string

let of_string s =
  if String.contains s '+' then
    failwith "no_plus"
  else
    s

let to_string s = s
