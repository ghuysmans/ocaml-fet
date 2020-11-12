open Dsl

let date = conv Spec.date

let ical_of_timetable tz only duration
                      first until
                      g_teachers
                      show_classes nog nosg g_students
                      g_rooms
                      input output =
  let prog = "_build/default/bin/ical_of_timetable.exe" in (* FIXME *)
  let argv =
    [[prog]] |>
    named "T" string tz |>
    option "only" string only |>
    named "d" int duration |>
    option "from" date first |>
    option "u" date until |>
    flag "t" g_teachers |>
    flag "c" show_classes |>
    flag "no-groups" nog |>
    flag "no-subgroups" nosg |>
    flag "s" g_students |>
    flag "r" g_rooms |>
    named "output-dir" string output |>
    list string input |>
    List.rev |>
    List.flatten |>
    Array.of_list
  in
  let open Unix in
  create_process prog argv stdin stdout stderr |>
  waitpid [] |>
  snd |> function
    | WEXITED 0 -> ()
    | _ -> failwith "failed" (* FIXME *)

let _ =
  (* ensure that it's at least compatible *)
  Spec.ical_of_timetable ical_of_timetable


let () =
  let tz = "Europe/Brussels" in
  let only = None in
  let duration = 60 in
  let first = None in
  let until = None in
  let g_teachers = true in
  let show_classes = false in
  let nog = false in
  let nosg = false in
  let g_students = false in
  let g_rooms = false in
  let input = [
    "Hopwood/Hopwood_rooms_and_buildings.csv";
    "Hopwood/Hopwood_students.csv";
    "Hopwood/Hopwood_teachers.csv";
    "Hopwood/Hopwood_timetable.csv";
  ] in
  let output = "d" in
  ical_of_timetable tz only duration
                    first until
                    g_teachers
                    show_classes nog nosg g_students
                    g_rooms
                    input output
