(* Reverse the lines in a file *)

let putlines lines filename =
  let channel = open_out filename in
    List.iter
      (fun s ->
         output_string channel s;
         output_char channel '\n')
      lines;
    close_out channel

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
    [|_; infile; outfile|] ->
      begin
        try
          let lines = List.rev (getlines infile) in
            putlines lines outfile
        with
          e ->
            print_string "There was an error. Details follow:\n";
            print_string (Printexc.to_string e);
            print_newline ();
            exit 1
      end
  | _ ->
      print_string "Usage: reverse input_filename output_filename\n";
      exit 1
