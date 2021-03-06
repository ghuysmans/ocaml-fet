open Utils

module Tag = struct
  type t = [`Activity_tag] No_plus.t
  let of_string = No_plus.of_string
end

type t = {
  students: Class.t Plus.t;
  subject: string;
  teachers: Teacher.t Plus.t;
  tags: Tag.t Plus.t;
  duration: (int * int list) option;
  min_days: int option;
  weight: float option;
  consecutive: bool option;
}

let header = [
  "Students Sets";
  "Subject";
  "Teachers";
  "Activity Tags";
  "Total Duration";
  "Split Duration";
  "Min Days";
  "Weight";
  "Consecutive";
]

let make ?(students=[]) ?(teachers=[]) ?(tags=[]) ?duration ?min_days ?weight ?consecutive subject =
  match duration with
  | Some (t, l) when l <> [] && List.fold_left (+) 0 l <> t ->
    failwith "inconsistent Split Duration"
  | _ -> {
    students = (students :> Class.t Plus.t);
    teachers;
    tags;
    duration;
    min_days;
    weight;
    consecutive;
    subject
  }

let of_list = function
  | [st; subject; ts; at; td; sd; m; w; c] ->
    make ~students:(Plus.of_string st)
         ~teachers:(Plus.of_string ts)
         ~tags:(Plus.of_string at)
         ?duration:(
           match td, sd with
           | "", "" -> None
           | "", _ -> failwith "extraneous Split Duration"
           | _, "" -> Some (int_of_string td, [])
           | _, _ -> Some (
             int_of_string td,
             Plus.of_string sd |> List.map (fun x ->
               No_plus.to_string x |>
               int_of_string
             )
           )
         )
         ?min_days:(option_of_csv int_of_string m)
         ?weight:(option_of_csv float_of_string w)
         ?consecutive:(option_of_csv bool_of_string c)
         subject
  | _ -> failwith "Activities.of_list"

let to_list {students; subject; teachers; tags; duration; min_days; weight; consecutive} = [
  Plus.to_string students;
  subject;
  Plus.to_string teachers;
  Plus.to_string tags;
  (match duration with
   | None -> ""
   | Some (d, _) -> string_of_int d);
  (match duration with
   | None -> ""
   | Some (_, l) ->
     l |> List.map (fun i ->
       string_of_int i |>
       No_plus.of_string
     ) |>
     Plus.to_string);
  csv_of_option string_of_int min_days;
  csv_of_option string_of_float weight;
  csv_of_option string_of_bool consecutive;
]
