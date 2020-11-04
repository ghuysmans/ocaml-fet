open Icalendar

let generation_time =
  match Unix.gettimeofday () |> Ptime.of_float_s with
  | None -> failwith "Ptime.of_float_s"
  | Some t -> t

(* FIXME lang? *)
let mk_event ~uid ~location start doe ?freq ?description summary = `Event {
  dtstamp = Params.empty, generation_time;
  uid = Params.empty, uid;
  dtstart = Params.empty, `Datetime (`Local start);
  dtend_or_duration = Some (
    match doe with
    | `Minutes m -> `Duration (Params.empty, Ptime.Span.of_int_s (m * 60))
    | `End e -> `Dtend (Params.empty, `Datetime (`Local e))
  );
  rrule = (
    match freq with
    | None -> None
    | Some f -> Some (Params.empty, (f, None, None, []))
  );
  props = (
    `Location (Params.empty, location) ::
    `Summary (Params.empty, summary) ::
    match description with
    | None -> []
    | Some d -> [`Description (Params.empty, d)]
  );
  alarms = [];
}

module H = Hashtbl.Make (struct
  type t = Fet.Class.Group.t
  let hash = Hashtbl.hash
  let equal = (=)
end)

let ends_with suffix s =
  let open String in
  length s >= length suffix &&
  sub s (length s - length suffix) (length suffix) = suffix

type data = {
  rooms: Fet.Rooms_and_buildings.t list option;
  students: (Fet.Class.Group.t list * Fet.Class.Subgroup.t H.t) option;
  teachers: Fet.Teachers.t list option;
  timetable: Fet.Timetable.t list option;
}

let import input =
  match
    input |> List.fold_left (fun data x ->
      let read f = Csv.load x |> List.tl |> List.map f in
      if x |> ends_with "rooms_and_buildings.csv" then
        {data with rooms = Some (read Fet.Rooms_and_buildings.of_list)}
      else if x |> ends_with "students.csv" then
        let h = H.create 100 in
        let groups = ref [] in
        let index = function
          | Fet.Students.Year _ -> ()
          | Group (_, g, _) -> groups := g :: !groups
          | Subgroup (_, g, sg, _) -> H.add h g sg
        in
        read Fet.Students.of_list |> List.iter index;
        {data with students = Some (!groups, h)}
      else if x |> ends_with "teachers.csv" then
        {data with teachers = Some (read Fet.Teachers.of_list)}
      else if x |> ends_with "timetable.csv" then
        {data with timetable = Some (read Fet.Timetable.of_list)}
      else (
        Printf.eprintf "ignored %S.\n" x;
        data
      )
    ) {
      rooms = None;
      students = None;
      teachers = None;
      timetable = None;
    }
  with
  | {rooms = None; _} -> failwith "missing rooms_and_buildings.csv"
  | {students = None; _} -> failwith "missing students.csv"
  | {teachers = None; _} -> failwith "missing teachers.csv"
  | {timetable = None; _} -> failwith "missing timetable.csv"
  | {rooms = Some r; students = Some s; teachers = Some t; timetable = Some tt} -> r, s, t, tt

let generate l =
  to_ics ([`Prodid (Params.empty, "ical_of_timetable")], l)

let interval_of_timetable default_duration first (tt : Fet.Timetable.t) =
  let d =
    match tt.day with
    | Monday -> 0
    | Tuesday -> 1
    | Wednesday -> 2
    | Thursday -> 3
    | Friday -> 4
    | Saturday -> 5
    | Sunday -> 6
  in
  let of_hour h =
    Scanf.sscanf h "%d:%d" (fun h m ->
      float d *. 24. *. 60. *. 60. +.
      float h *. 60. *. 60. +.
      float m *. 60.
    ) |>
    Ptime.Span.of_float_s |> function
      | None -> failwith "Ptime.Span.of_float_s"
      | Some s ->
        match Ptime.add_span first s with
        | None -> failwith "Ptime.add_span"
        | Some t -> t
  in
  match String.split_on_char '-' tt.hour with
  | [] -> assert false
  | [h] -> of_hour h, `Minutes default_duration
  | [start; stop] -> of_hour start, `End (of_hour stop)
  | _ -> failwith "invalid Hour range format"

let bulk weekly first duration g_teachers g_students g_rooms input output =
  let first =
    match first with
    | Some f -> f
    | None ->
      match Ptime.(to_date generation_time |> of_date) with
      | None -> failwith "Ptime.of_date"
      | Some t -> t
  in
  let freq =
    if weekly then
      Some `Weekly
    else
      None
  in
  let rooms, (groups, subgroups), teachers, timetable = import input in
  let write fn l =
    let fn = Filename.concat output fn in
    let ch = open_out fn in
    Printf.eprintf "writing %S...\n" fn;
    generate l |> output_string ch;
    close_out ch
  in
  if g_rooms then (
    rooms |> List.iter (fun Fet.Rooms_and_buildings.{name; _} ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if tt.room = name then
          let start, doe = interval_of_timetable duration first tt in
          let uid = string_of_int tt.activity_id in (* FIXME how unique? *)
          [mk_event
            ~uid
            ~location:tt.room
            start
            doe
            ?freq
            Fet.Plus.(to_string tt.teachers ^ " (" ^ to_string tt.students ^ ")")
          ]
        else
          []
      ) |>
      List.flatten |>
      write (name ^ ".ics")
    )
  );
  if g_teachers then (
    teachers |> List.iter (fun t ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if List.mem t tt.teachers then
          let start, doe = interval_of_timetable duration first tt in
          let uid = string_of_int tt.activity_id in (* FIXME how unique? *)
          [mk_event
            ~uid
            ~location:tt.room
            start
            doe
            ?freq
            (Fet.Plus.to_string tt.students ^ " (" ^ tt.subject ^ ")")
          ]
        else
          []
      ) |>
      List.flatten |>
      write (Fet.No_plus.to_string t ^ ".ics")
    )
  );
  if g_students then (
    groups |> List.iter (fun g ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if List.mem (g :> Fet.Class.t) tt.students then
          let start, doe = interval_of_timetable duration first tt in
          let uid = string_of_int tt.activity_id in (* FIXME how unique? *)
          [mk_event
            ~uid
            ~location:tt.room
            start
            doe
            ?freq
            (match tt.teachers with
             | [] -> tt.subject
             | l -> tt.subject ^ ", " ^ Fet.Plus.to_string l)
          ]
        else
          []
      ) |>
      List.flatten |>
      write (Fet.No_plus.to_string g ^ ".ics")
    );
    ignore subgroups (* FIXME *)
  );
  prerr_endline "done."


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
  let doc = "first Monday (default: today)" in (* FIXME *)
  Arg.(value & opt (some date) None & info ~doc ["f"; "first-day"])

let teachers =
  let doc = "generate teacher schedules" in
  Arg.(value & flag & info ~doc ["t"; "teachers"])

let students =
  let doc = "generate student schedules" in
  Arg.(value & flag & info ~doc ["s"; "students"])

let rooms =
  let doc = "generate room schedules" in
  Arg.(value & flag & info ~doc ["r"; "rooms"])

let once =
  let doc = "don't repeat courses weekly" in
  Arg.(value & flag & info ~doc ["1"; "once"])

let output =
  let doc = "output directory" in
  Arg.(required & opt (some dir) None & info ~doc ["o"; "output"])

let input =
  Arg.(non_empty & pos_all file [] & info ~docv:"CSV" [])

let () =
  let open Term in
  exit @@ eval (
    const bulk $ once $ first $ duration $ teachers $ students $ rooms $ input $ output,
    info "ical_of_timetable" ~doc:"generate iCal schedules from FET CSV files"
  )
