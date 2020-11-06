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
    | Some (f, None) ->
      Some (Params.empty, (f, None, None, []))
    | Some (f, Some last) ->
      Some (Params.empty, (f, Some (`Until (`Local last)), None, []))
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

module HS = Hashtbl.Make (struct
  type t = Fet.Class.Subgroup.t
  let hash = Hashtbl.hash
  let equal = (=)
end)

let ends_with suffix s =
  let open String in
  length s >= length suffix &&
  sub s (length s - length suffix) (length suffix) = suffix

type data = {
  rooms: Fet.Rooms_and_buildings.t list option;
  students: (Fet.Class.Group.t list * Fet.Class.Group.t HS.t) option;
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
        let h = HS.create 100 in
        let groups = ref [] in
        let index = function
          | Fet.Students.Year _ -> ()
          | Group (_, g, _) -> groups := g :: !groups
          | Subgroup (_, g, sg, _) -> HS.add h sg g
        in
        read Fet.Students.of_list |> List.iter index;
        {data with students = Some (!groups, h)}
      else if x |> ends_with "teachers.csv" then
        {data with teachers = Some (read Fet.Teachers.of_list)}
      else if x |> ends_with "timetable.csv" then
        {data with timetable = Some (read Fet.Timetable.of_list)}
      else (
        Printf.eprintf "ignored: %s\n" x;
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

let generate tz l =
  to_ics ([
    `Prodid (Params.empty, "ical_of_timetable");
    `Version (Params.empty, "2.0");
    `Xprop (("WR", "TIMEZONE"), Params.empty, tz);
  ], l)

let add_span t span =
  match Ptime.add_span t span with
  | None -> failwith "Ptime.add_span"
  | Some t -> t

let interval_of_timetable default_duration fst (tt : Fet.Timetable.t) =
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
  let open Re in
  let conv g =
    let i i = Group.get g i |> int_of_string in
    (  d * 24 * 60 * 60 +
     i 1 * 60 * 60 +
     i 2 * 60) |>
    Ptime.Span.of_int_s |>
    add_span fst
  in
  match
    let n = rep1 (rg '0' '9') in
    all (compile (seq [group n; set ".:h"; group n])) tt.hour
  with
  | [] -> assert false
  | [start] -> conv start, `Minutes default_duration
  | [start; stop] -> conv start, `End (conv stop)
  | _ -> failwith "invalid Hour range format"

let bulk tz only duration
         first until
         g_teachers
         show_classes nog nosg g_students
         g_rooms
         input output =
  let filter s =
    match only with
    | None -> true
    | Some o -> o = s
  in
  let first =
    let t =
      match first with
      | Some f -> f
      | None ->
        match Ptime.(to_date generation_time |> of_date) with
        | None -> failwith "Ptime.of_date"
        | Some t -> t
    in
    let dd =
      match Ptime.weekday t with
      | `Mon -> 0
      | `Tue -> 6
      | `Wed -> 5
      | `Thu -> 4
      | `Fri -> 3
      | `Sat -> 2
      | `Sun -> 1
    in
    add_span t (Ptime.Span.of_int_s (dd * 24 * 60 * 60))
  in
  let freq =
    match until with
    | None -> None
    | Some d -> Some (`Weekly, Some d)
  in
  let rooms, (groups, subgroups), teachers, timetable = import input in
  let write fn l =
    let fn =
      Re.(replace_string (compile (char '/')) ~by:"_") fn |>
      Filename.concat output
    in
    let ch = open_out fn in
    Printf.printf "%s\n" fn;
    generate tz l |> output_string ch;
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
      if filter name then
        write (name ^ ".ics")
      else
        ignore
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
      if filter (Fet.No_plus.to_string t) then
        write (Fet.No_plus.to_string t ^ ".ics")
      else
        ignore
    )
  );
  if g_students then (
    (* FIXME what about years? *)
    let student_view (tt : Fet.Timetable.t) =
      let start, doe = interval_of_timetable duration first tt in
      let uid = string_of_int tt.activity_id in (* FIXME how unique? *)
      mk_event
        ~uid
        ~location:tt.room
        start
        doe
        ?freq
        (if show_classes then
           tt.subject ^ " " ^ Fet.Plus.to_string tt.students
         else
           match tt.teachers with
           | [] -> tt.subject
           | l -> tt.subject ^ ", " ^ Fet.Plus.to_string l)
    in
    let group_cals = H.create 100 in
    groups |> List.iter (fun g ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if List.mem (g :> Fet.Class.t) tt.students then
          [student_view tt]
        else
          []
      ) |>
      List.flatten |> fun l ->
      if filter (Fet.No_plus.to_string g) && not nog then
        write (Fet.No_plus.to_string g ^ ".ics") l;
      H.replace group_cals g l
    );
    HS.fold (fun sg _ l -> sg :: l) subgroups [] |>
    List.sort_uniq compare |>
    List.iter (fun sg ->
      let from_groups =
        HS.find_all subgroups sg |>
        List.map (H.find group_cals) |>
        List.flatten
      in
      (* TODO fold_right elsewhere? *)
      List.fold_right (fun (tt : Fet.Timetable.t) l ->
        if List.mem (sg :> Fet.Class.t) tt.students then
          student_view tt :: l
        else
          l
      ) timetable from_groups |>
      if filter (Fet.No_plus.to_string sg) && not nosg then
        write (Fet.No_plus.to_string sg ^ ".ics")
      else
        ignore
    )
  )


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

let () =
  let open Term in
  exit @@ eval (
    const bulk $
      tz $ only $ duration $
      first $ until $
      teachers $
      show_classes $ no_groups $ no_subgroups $ students $
      rooms $
      input $ output,
    info "ical_of_timetable" ~doc:"generate iCal schedules using CSV files exported from FET"
  )
