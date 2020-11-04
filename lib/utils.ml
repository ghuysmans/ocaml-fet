let csv_of_option f = function
  | None -> ""
  | Some t -> f t

let option_of_csv f = function
  | "" -> None
  | s -> Some (f s)

let bool_of_string = function
  | "yes" | "y" | "true" | "t" | "1" -> true
  | "no" | "n" | "false" | "f" | "0" -> false
  | _ -> failwith "bool_of_string"
