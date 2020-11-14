open Icalendar

let generation_time =
  match Unix.gettimeofday () |> Ptime.of_float_s with
  | None -> failwith "Ptime.of_float_s"
  | Some t -> t

(* FIXME lang? *)
let mk_event ~id ~location start doe ?freq ?description summary = `Event {
  dtstamp = Params.empty, generation_time;
  uid = Params.empty, string_of_int id; (* FIXME *)
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

let import timetable =
  let prefix =
    match Spec.prefix ~timetable with
    | Some p -> p
    | None -> failwith "unsupported filename"
  in
  let read f name =
    Filename.(concat (dirname timetable) (prefix ^ name)) |>
    Csv.load |>
    List.tl |> (* skip the header *)
    List.map f
  in
  read Fet.Rooms_and_buildings.of_list "rooms_and_buildings.csv",
  read Fet.Teachers.of_list "teachers.csv",
  read Fet.Timetable.of_list "timetable.csv",
  let h = HS.create 100 in
  let groups = ref [] in
  let index = function
    | Fet.Students.Year _ -> ()
    | Group (_, g, _) -> groups := g :: !groups
    | Subgroup (_, g, sg, _) -> HS.add h sg g
  in
  read Fet.Students.of_list "students.csv" |> List.iter index;
  !groups, h

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

let bulk (t : Spec.t) =
  let filter s =
    match t.only with
    | None -> true
    | Some o -> o = s
  in
  let first =
    let t =
      match t.first with
      | Some f -> Spec.ptime_of_date f
      | None -> Spec.ptime_of_date (Ptime.to_date generation_time)
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
    match t.until with
    | None -> None
    | Some d -> Some (`Weekly, Some (Spec.ptime_of_date d))
  in
  let rooms, teachers, timetable, (groups, subgroups) = import t.input in
  let write fn l =
    let fn =
      Re.(replace_string (compile (char '/')) ~by:"_") fn |>
      Filename.concat t.output_dir
    in
    let ch = open_out fn in
    Printf.printf "%s\n" fn;
    generate t.timezone l |> output_string ch;
    close_out ch
  in
  if t.generate_rooms then (
    rooms |> List.iter (fun Fet.Rooms_and_buildings.{name; _} ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if tt.room = name then
          let start, doe = interval_of_timetable t.slot_duration first tt in
          [mk_event
            ~id:tt.activity_id
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
  if t.generate_teachers then (
    teachers |> List.iter (fun teacher ->
      timetable |> List.map (fun (tt : Fet.Timetable.t) ->
        if List.mem teacher tt.teachers then
          let start, doe = interval_of_timetable t.slot_duration first tt in
          [mk_event
            ~id:tt.activity_id
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
      if filter (Fet.No_plus.to_string teacher) then
        write (Fet.No_plus.to_string teacher ^ ".ics")
      else
        ignore
    )
  );
  if t.generate_students then (
    (* FIXME what about years? *)
    let student_view (tt : Fet.Timetable.t) =
      let start, doe = interval_of_timetable t.slot_duration first tt in
      mk_event
        ~id:tt.activity_id
        ~location:tt.room
        start
        doe
        ?freq
        (if t.show_classes then
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
      if filter (Fet.No_plus.to_string g) && not t.no_groups then
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
      if filter (Fet.No_plus.to_string sg) && not t.no_subgroups then
        write (Fet.No_plus.to_string sg ^ ".ics")
      else
        ignore
    )
  )


let () =
  Cmdliner.Term.(exit @@ eval @@ (const bulk $ Spec.term, Spec.info))
