type date = Ptime.date

type t = {
  timezone: string;
  slot_duration: int;
  first: date option;
  until: date option;
  only: string option;
  generate_teachers: bool;
  generate_students: bool;
  show_classes: bool;
  no_groups: bool;
  no_subgroups: bool;
  generate_rooms: bool;
  input: string;
  output_dir: string;
}


open Cmdliner

let slot_duration =
  let doc = "slot duration (in minutes) when Hour isn't an hh:mm-hh:mm range" in
  Arg.(value & opt int 60 & info ~doc ["d"; "duration"])

let ptime_of_date d =
  match Ptime.of_date d with
  | Some t -> t
  | None -> failwith "Ptime.of_date"

let date =
  let open Ptime in
  Arg.conv ~docv:"date" ((fun x ->
    match of_rfc3339 (x ^ "T00:00:00Z") |> rfc3339_error_to_msg with
    | Ok (t, _, _) -> Ok (Ptime.to_date t)
    | Error e -> Error e
  ), fun fmt d ->
    String.sub (Ptime.to_rfc3339 (ptime_of_date d)) 0 10 |>
    Format.pp_print_string fmt
  )

let first =
  let doc = "start from a given date" in
  Arg.(value & opt (some date) None & info ~doc ["from"])

let until =
  let doc = "repeat events weekly before a given date" in
  Arg.(value & opt (some date) None & info ~doc ["u"; "until"; "repeat-until"])

let generate_teachers =
  let doc = "generate teacher schedules" in
  Arg.(value & flag & info ~doc ["t"; "teachers"])

let generate_students =
  let doc = "generate student schedules" in
  Arg.(value & flag & info ~doc ["s"; "students"])

let generate_rooms =
  let doc = "generate room schedules" in
  Arg.(value & flag & info ~doc ["r"; "rooms"])

let output_dir =
  let doc = "output directory" in
  Arg.(value & opt dir "." & info ~doc ["output-dir"])

let only =
  let doc = "generate a single schedule" in
  Arg.(value & opt (some string) None & info ~doc ["only"])

let show_classes =
  let doc = "show classes in student schedules" in
  Arg.(value & flag & info ~doc ["c"; "show-classes"])

let no_groups =
  let doc = "don't generate group schedules" in
  Arg.(value & flag & info ~doc ["no-groups"])

let no_subgroups =
  let doc = "don't generate subgroup schedules" in
  Arg.(value & flag & info ~doc ["no-subgroups"])

let timezone =
  let doc = "timezone" in
  Arg.(value & opt string "Europe/Brussels" & info ~doc ["T"; "timezone"])

let input =
  Arg.(required & pos 0 (some file) None & info ~docv:"timetable.csv" [])


let prefix ~timetable =
  let s = Filename.basename timetable in
  let suffix = "timetable.csv" in
  let open String in
  if length s >= length suffix then
    let p = length s - length suffix in
    if sub s p (length suffix) = suffix then
      Some (sub s 0 p)
    else
      None
  else
    None

let () =
  assert (prefix ~timetable:"/a/b/whatever.csv" = None);
  assert (prefix ~timetable:"/a/b/timetable.csv" = Some "");
  assert (prefix ~timetable:"/a/b/x_timetable.csv" = Some "x_")


open Term

let term =
  (* FIXME this is awful *)
  let f timezone only slot_duration
        first until
        generate_teachers
        show_classes no_groups no_subgroups generate_students
        generate_rooms
        input output_dir =
    {timezone; only; slot_duration;
     first; until;
     generate_teachers;
     show_classes; no_groups; no_subgroups; generate_students;
     generate_rooms;
     input; output_dir}
  in
  const f $
    timezone $ only $ slot_duration $
    first $ until $
    generate_teachers $
    show_classes $ no_groups $ no_subgroups $ generate_students $
    generate_rooms $
    input $ output_dir

let info =
  info "ical_of_timetable" ~doc:"generate iCal schedules using CSV files exported from FET"
