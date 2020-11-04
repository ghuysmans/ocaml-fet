open Class

type t =
  | Year of Year.t * int
  | Group of Year.t * Group.t * int
  | Subgroup of Year.t * Group.t * Subgroup.t * int

let header = [
  "Year";
  "Number of Students per Year";
  "Group";
  "Number of Students per Group";
  "Subgroup";
  "Number of Students per Subgroup";
]

let of_list = function
  | [y; py; ""; ""; ""; ""] ->
    Year (Year.of_string y, int_of_string py)
  | [y; _; g; pg; ""; ""] ->
    Group (Year.of_string y, Group.of_string g, int_of_string pg)
  | [y; _; g; _; sg; psg] ->
    Subgroup (Year.of_string y, Group.of_string g, Subgroup.of_string sg, int_of_string psg)
  | _ ->
    failwith "Students.of_list"

let to_list = function
  | Year (y, py) ->
    [No_plus.to_string y; string_of_int py; ""; ""; ""; ""]
  | Group (y, g, pg) ->
    [No_plus.to_string y; "0"; No_plus.to_string g; string_of_int pg; ""; ""]
  | Subgroup (y, g, sg, psg) ->
    [No_plus.to_string y; "0"; No_plus.to_string g; "0"; No_plus.to_string sg; string_of_int psg]

type tree = (
  Year.t * int * (
    Group.t * int * (
      Subgroup.t * int
    ) list
  ) list
) list

let tree_to_list l =
  let s = No_plus.to_string in
  let i = string_of_int in
  List.map (fun (y, py, l) -> [s y; i py; ""; ""; ""; ""] :: (
    List.map (fun (g, pg, l) -> [s y; i py; s g; i pg; ""; ""] :: (
      List.map (fun (sg, psg) -> [s y; i py; s g; i pg; s sg; i psg]) l
    )) l |>
    List.flatten
  )) l |>
  List.flatten
