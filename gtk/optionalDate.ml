type date = {
  day: int;
  month: int;
  year: int;
}

type t = GEdit.combo_box * date option ref

let combo_box ~parent ~empty ~set ~title ~ok ~cancel ~fmt =
  let v = ref None in
  let model, column = GTree.store_of_list Gobject.Data.string [empty; set] in
  let combo = GEdit.combo_box ~model () in
  let () =
    let renderer = GTree.cell_renderer_pixbuf [`STOCK_SIZE `BUTTON] in
    combo#pack renderer;
    combo#add_attribute renderer "stock_id" column
    (* TODO use GtkStock! *)
  in
  let () =
    let renderer = GTree.cell_renderer_text [`XPAD 5] in
    combo#pack renderer;
    combo#add_attribute renderer "text" column
  in
  combo#set_active 0;
  let dialog =
    GWindow.dialog ~parent ~title ~modal:true
                   ~width:300 ~height:200 ~show:false ()
  in
  let cal = GMisc.calendar ~packing:dialog#vbox#add () in
  dialog#add_button ok `OK;
  dialog#add_button cancel `CANCEL;
  dialog#set_default_response `CANCEL;
  ignore @@ combo#connect#changed ~callback:(fun () ->
    if combo#active = 0 then
      v := None
    else (
      (match dialog#run () with
       | `OK ->
         let t = {day = cal#day; month = cal#month; year = cal#year} in
         v := Some t;
         let row = model#get_iter (GTree.Path.create [1]) in
         model#set ~row ~column (Printf.sprintf fmt t.day t.month t.year);
       | `CANCEL ->
         v := None;
         combo#set_active 0
       | `DELETE_EVENT ->
         failwith "wtf?");
      dialog#set_visible false
    )
  );
  combo, v
