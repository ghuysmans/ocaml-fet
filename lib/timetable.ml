type t = {
  activity_id: int;
  day: Day.t;
  hour: string;
  students: Plus.t;
  subject: string;
  teachers: Plus.t;
  activity_tags: Plus.t;
  room: string;
  comments: string;
}

let headers = [|
  "Activity Id";
  "Day";
  "Hour";
  "Students Sets";
  "Subject";
  "Teachers";
  "Activity Tags";
  "Room";
  "Comments";
|]

let of_array = function
  | [| id; d; hour; st; subject; ts; at; room; comments |] -> {
    activity_id = int_of_string id;
    day = Day.of_string d;
    hour;
    students = Plus.of_string st;
    subject;
    teachers = Plus.of_string ts;
    activity_tags = Plus.of_string at;
    room;
    comments;
  }
  | _ -> failwith "Timetable.of_array"

let to_array {activity_id; day; hour; students; subject; teachers; activity_tags; room; comments} = [|
  string_of_int activity_id;
  Day.to_string day;
  hour;
  Plus.to_string students;
  subject;
  Plus.to_string teachers;
  Plus.to_string activity_tags;
  room;
  comments;
|]
