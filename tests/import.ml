let test of_list f =
  Csv.load f |>
  List.tl |>
  List.map of_list |>
  List.length |>
  Printf.printf "%d\n"

let () = test Fet.Activities.of_list "activities.csv"
let () = test Fet.Activity_tags.of_list "activity_tags.csv"
let () = test Fet.Rooms_and_buildings.of_list "rooms_and_buildings.csv"
let () = test Fet.Statistics_activities.of_list "statistics_activities.csv"
let () = test Fet.Students.of_list "students.csv"
let () = test Fet.Teachers.of_list "teachers.csv"
let () = test Fet.Timetable.of_list "timetable.csv"
