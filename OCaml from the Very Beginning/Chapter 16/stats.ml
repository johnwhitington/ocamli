let print_histogram stats =
  print_string "Character frequencies:\n";
  for x = 0 to 255 do
    let freq = Textstat.frequency stats (char_of_int x) in
      if freq > 0 then
        begin
          print_string "For character '";
          print_char (char_of_int x);
          print_string "'(character number ";
          print_int x;
          print_string ") the count is ";
          print_int freq;
          print_string ".\n"
        end
  done
in
  try
    begin match Sys.argv with
      [|_; filename|] ->
        let stats = Textstat.stats_from_file filename in
          print_string "Words: ";
          print_int (Textstat.words stats);
          print_newline ();
          print_string "Characters: ";
          print_int (Textstat.characters stats);
          print_newline ();
          print_string "Sentences: ";
          print_int (Textstat.sentences stats);
          print_newline ();
          print_string "Lines: ";
          print_int (Textstat.lines stats);
          print_newline ();
          print_histogram stats
    | _ ->
      print_string "Usage: stats <filename>";
      print_newline ()
    end
  with
    e ->
      print_string "An error occurred: ";
      print_string (Printexc.to_string e);
      print_newline ();
      exit 1
