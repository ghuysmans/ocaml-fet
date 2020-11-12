open Cmdliner

let duration =
  let doc = "slot duration (in minutes) when Hour isn't an hh:mm-hh:mm range" in
  Arg.(value & opt int 60 & info ~doc ["d"; "duration"])

let date =
  Arg.conv ~docv:"date" ((fun x ->
    match Ptime.(of_rfc3339 (x ^ "T00:00:00Z") |> rfc3339_error_to_msg) with
    | Ok (t, _, _) -> Ok t
    | Error e -> Error e
  ), fun fmt t ->
    String.sub (Ptime.to_rfc3339 t) 0 10 |>
    Format.pp_print_string fmt
  )

let first =
  let doc = "start from a given date" in
  Arg.(value & opt (some date) None & info ~doc ["from"])

let until =
  let doc = "repeat events weekly before a given date" in
  Arg.(value & opt (some date) None & info ~doc ["u"; "until"; "repeat-until"])

let teachers =
  let doc = "generate teacher schedules" in
  Arg.(value & flag & info ~doc ["t"; "teachers"])

let students =
  let doc = "generate student schedules" in
  Arg.(value & flag & info ~doc ["s"; "students"])

let rooms =
  let doc = "generate room schedules" in
  Arg.(value & flag & info ~doc ["r"; "rooms"])

let output =
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

let tz =
  let doc = "timezone" in
  Arg.(value & opt string "Europe/Brussels" & info ~doc ["T"; "timezone"])

let input =
  Arg.(non_empty & pos_all file [] & info ~docv:"CSV" [])

let ical_of_timetable f = Term.(
  const f $
    tz $ only $ duration $
    first $ until $
    teachers $
    show_classes $ no_groups $ no_subgroups $ students $
    rooms $
    input $ output,
  info "ical_of_timetable" ~doc:"generate iCal schedules using CSV files exported from FET"
)
