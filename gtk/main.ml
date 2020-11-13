open GMain

let _locale = GtkMain.Main.init ()

let () =
  let title = "Generate iCalendar files from a FET timetable" in
  let window = GWindow.window ~title () in
  ignore @@ window#connect#destroy ~callback:Main.quit;
  let vbox = GPack.vbox ~packing:window#add () in
  let grid = GPack.grid ~packing:vbox#add () in
  let timezones = ["Europe/Brussels"] in (* TODO get a list of values for iCal *)
  let tz = GEdit.combo_box_text ~strings:timezones () in
  (fst tz)#set_active 0;
  let duration = GData.adjustment ~value:60. ~lower:5. ~upper:360. () in
  let duration_s = GEdit.spin_button ~adjustment:duration ~digits:0 () in
  ignore @@ duration_s#connect#output ~callback:(fun () ->
    (* FIXME +, - *)
    let e = new GEdit.entry (GtkEditProps.Entry.cast duration_s#as_widget) in
    let set x =
      (* FIXME clamp *)
      duration#set_value (float x);
      let h, m = x / 60, x mod 60 in
      e#set_text @@ Printf.sprintf "%d:%02d" h m
    in
    if String.contains e#text ':' then
      try
        Scanf.sscanf e#text "%d:%d" (fun h m -> set (h * 60 + m))
      with _ ->
        prerr_endline @@ "invalid duration: " ^ e#text
    else
      set (int_of_string e#text);
    true
  );
  let start = GMisc.calendar () in
  let repeat = OptionalDate.combo_box
    ~parent:window
    ~empty:"don't repeat"
    ~set:"select date"
    ~title:"Select an end date"
    ~ok:"OK"
    ~cancel:"Cancel"
    ~fmt:"until %d/%d/%d"
  in
  let students = GButton.check_button ~active:true ~label:"generate" () in
  let show_classes = GButton.check_button ~label:"show classes" () in
  let no_groups = GButton.check_button ~label:"skip groups" () in
  let no_subgroups = GButton.check_button ~label:"skip subgroups" () in
  let teachers = GButton.check_button ~active:true ~label:"generate" () in
  let rooms = GButton.check_button ~label:"generate" () in
  let input = GFile.chooser_button ~action:`OPEN () in (* FIXME filter *)
  let output = GFile.chooser_button ~action:`CREATE_FOLDER () in (* FIXME *)
  let only = GEdit.entry () in (* FIXME filter depending on checkboxes *)
  only#set_placeholder_text "Select a specific target...";
  [
    "Time zone: ", (fst tz :> GObj.widget);
    "Start on: ", (start :> GObj.widget);
    "Repeat: ", (fst repeat :> GObj.widget);
    "Slot duration: ", (duration_s :> GObj.widget);
    "FET timetable: ", (input :> GObj.widget);
    "Output: ", (output :> GObj.widget);
    "Filter: ", (only :> GObj.widget);
    "Students: ", (students :> GObj.widget);
    "", (no_groups :> GObj.widget);
    "", (no_subgroups :> GObj.widget);
    "", (show_classes :> GObj.widget);
    "Teachers: ", (teachers :> GObj.widget);
    "Rooms: ", (rooms :> GObj.widget);
  ] |>
  List.iteri (fun top (text, widget) ->
    let label = GMisc.label ~text () in
    label#set_xalign 1.;
    grid#attach ~left:0 ~top (label :> GObj.widget);
    grid#attach ~left:1 ~top widget;
  );
  let button = GButton.button ~label:"Generate" ~packing:vbox#add () in
  ignore @@ button#connect#clicked ~callback:(fun () ->
    if students#active then prerr_endline "students";
    if teachers#active then prerr_endline "teachers";
    if rooms#active then prerr_endline "rooms";
    prerr_endline (match GEdit.text_combo_get_active tz with
      | None -> "no timezone"
      | Some tz -> "timezone=" ^ tz
    );
    prerr_endline @@ Printf.sprintf "duration=%f" duration#value;
    prerr_endline (match output#filename with
      | None -> "no output"
      | Some f -> "output=" ^ f
    );
  );
  window#show ();
  Main.main ()
