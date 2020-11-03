type t = No_plus.t list

let of_string s =
  String.split_on_char '+' s |>
  List.map No_plus.of_string

let to_string l =
  List.map No_plus.to_string l |>
  String.concat "+"
