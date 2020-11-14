open GMain

let _locale = GtkMain.Main.init ()

let run prog t =
  let open Unix in
  let argv = Spec.argv prog t in
  Array.iter prerr_endline argv;
  create_process prog argv stdin stdout stderr |>
  waitpid [] |>
  snd |> function
    | WEXITED 0 -> ()
    | _ -> failwith "failed" (* FIXME *)

let () =
  let title = s_ "Generate iCalendar files from a FET timetable" in
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
    ~empty:(s_ "don't repeat")
    ~set:(s_ "select date")
    ~title:(s_ "Select an end date")
    ~ok:(s_ "OK")
    ~cancel:(s_ "Cancel")
    ~fmt:(f_ "until %d/%d/%d")
  in
  let students = GButton.check_button ~active:true ~label:(s_ "generate") () in
  let show_classes = GButton.check_button ~label:(s_ "show classes") () in
  let no_groups = GButton.check_button ~label:(s_ "skip groups") () in
  let no_subgroups = GButton.check_button ~label:(s_ "skip subgroups") () in
  let teachers = GButton.check_button ~active:true ~label:(s_ "generate") () in
  let rooms = GButton.check_button ~label:(s_ "generate") () in
  let input = GFile.chooser_button ~action:`OPEN () in (* FIXME filter *)
  let output = GFile.chooser_button ~action:`CREATE_FOLDER () in (* FIXME *)
  let only = GEdit.entry () in (* FIXME filter depending on checkboxes *)
  only#set_placeholder_text (s_ "Select a specific target...");
  [
    s_ "Time zone: ", (fst tz :> GObj.widget);
    s_ "Start on: ", (start :> GObj.widget);
    s_ "Repeat: ", (fst repeat :> GObj.widget);
    s_ "Slot duration: ", (duration_s :> GObj.widget);
    s_ "FET timetable: ", (input :> GObj.widget);
    s_ "Output: ", (output :> GObj.widget);
    s_ "Filter: ", (only :> GObj.widget);
    s_ "Students: ", (students :> GObj.widget);
    "", (no_groups :> GObj.widget);
    "", (no_subgroups :> GObj.widget);
    "", (show_classes :> GObj.widget);
    s_ "Teachers: ", (teachers :> GObj.widget);
    s_ "Rooms: ", (rooms :> GObj.widget);
  ] |>
  List.iteri (fun top (text, widget) ->
    let label = GMisc.label ~text () in
    label#set_xalign 1.;
    grid#attach ~left:0 ~top (label :> GObj.widget);
    grid#attach ~left:1 ~top widget;
  );
  let button = GButton.button ~label:(s_ "Generate") ~packing:vbox#add () in
  ignore @@ button#connect#clicked ~callback:(fun () ->
    match input#filename, GEdit.text_combo_get_active tz, output#filename with
    | None, _, _ -> failwith (s_ "missing input")
    | _, None, _ -> failwith (s_ "missing timezone")
    | _, _, None -> failwith (s_ "missing output")
    | Some input, Some timezone, Some output_dir ->
      let t = {
        Spec.timezone;
        slot_duration = int_of_float duration#value;
        first = Some (OptionalDate.date start);
        until = !(snd repeat);
        only = if only#text = "" then None else Some only#text;
        generate_teachers = teachers#active;
        generate_students = students#active;
        show_classes = show_classes#active;
        no_groups = no_groups#active;
        no_subgroups = no_subgroups#active;
        generate_rooms = rooms#active;
        input;
        output_dir = Filename.dirname output_dir;
      } in
      let prog = "_build/default/bin/ical_of_timetable.exe" in
      run prog t
  );
  window#show ();
  Main.main ()
