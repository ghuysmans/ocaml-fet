type t = {
  year: Year.t * int;
  group: Group.t * int;
  subgroup: Subgroup.t * int;
}

let header = [|
  "Year";
  "Number of Students per Year";
  "Group";
  "Number of Students per Group";
  "Subgroup";
  "Number of Students per Subgroup";
|]

let of_array = function
  | [| y; py; g; pg; sg; psg |] -> {
    year = Year.of_string y, int_of_string py;
    group = Group.of_string g, int_of_string pg;
    subgroup = Subgroup.of_string sg, int_of_string psg;
  }
  | _ ->
    failwith "Students.of_array"

let to_array {year = y, py; group = g, pg; subgroup = sg, psg} = [|
  Year.to_string y;
  string_of_int py;
  Group.to_string g;
  string_of_int pg;
  Subgroup.to_string sg;
  string_of_int psg;
|]
