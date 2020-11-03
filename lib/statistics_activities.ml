type t = {
  students: Class.t Plus.t;
  subject: string;
  teachers: Teacher.t Plus.t;
  total_duration: int;
}

let headers = [|
  "Students Sets";
  "Subject";
  "Teachers";
  "Total Duration";
|]

let of_array = function
  | [| st; subject; ts; d |] -> {
    students = Plus.of_string st;
    subject;
    teachers = Plus.of_string ts;
    total_duration = int_of_string d;
  }
  | _ ->
    failwith "Statistics_activities.of_array"

let to_array {students; subject; teachers; total_duration} = [|
  Plus.to_string students;
  subject;
  Plus.to_string teachers;
  string_of_int total_duration;
|]
