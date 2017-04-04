(* Search for a string in a file *)

let rec string_in_line term line pos =
    pos + String.length term <= String.length line
  &&
    (String.sub line pos (String.length term) = term
     || string_in_line term line (pos + 1))

let getlines filename =
  let channel = open_in filename in
    let lines = ref [] in
      try
        while true do
          lines := input_line channel :: !lines
        done;
        []
      with
        End_of_file ->
          close_in channel;
          List.rev !lines

let _ =
  match Sys.argv with
    [|_; searchterm; filename|] ->
      begin
        try
          List.iter
            (fun line ->
               if string_in_line searchterm line 0
                 then
                   begin
                     print_string line;
                     print_newline ()
                   end)
            (getlines filename)
        with
          e ->
            print_string "An error occurred:";
            print_newline ();
            print_string (Printexc.to_string e);
            print_newline ()
      end
  | _ ->
      print_string "Usage: search search_term filename";
      print_newline ()

