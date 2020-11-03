type t = {
  year: Class.Year.t * int;
  group: Class.Group.t * int;
  subgroup: Class.Subgroup.t * int;
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
    year = Class.Year.of_string y, int_of_string py;
    group = Class.Group.of_string g, int_of_string pg;
    subgroup = Class.Subgroup.of_string sg, int_of_string psg;
  }
  | _ ->
    failwith "Students.of_array"

let to_array {year = y, py; group = g, pg; subgroup = sg, psg} = [|
  No_plus.to_string y;
  string_of_int py;
  No_plus.to_string g;
  string_of_int pg;
  No_plus.to_string sg;
  string_of_int psg;
|]
