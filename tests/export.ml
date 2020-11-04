let () =
  match Sys.argv with
  | [| _; "students" |] ->
    let ch = Csv.to_channel stdout in
    Fet.Students.tree_to_list Fet.Class.[
      Year.of_string "y", 7, [
        Group.of_string "g", 4, [];
        Group.of_string "g2", 3, [
          Subgroup.of_string "sg1", 2;
          Subgroup.of_string "sg2", 1;
        ];
      ];
    ] |>
    List.iter (Csv.output_record ch)
  | _ -> exit 1
