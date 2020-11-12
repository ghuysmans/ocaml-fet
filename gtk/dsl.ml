(* TODO impove Cmdliner! *)

let prefix name =
  if String.length name = 1 then
    "-" ^ name
  else
    "--" ^ name

let option name to_string x args =
  match x with
  | Some x -> [prefix name; to_string x] :: args
  | None -> args

let named name to_string x args =
  [prefix name; to_string x] :: args

let string x = x

let int = string_of_int

let conv c =
  Format.asprintf "%a" (Cmdliner.Arg.conv_printer c)

let list to_string l args =
  List.map to_string l :: args

let flag name b args =
  if b then
    [prefix name] :: args
  else
    args
