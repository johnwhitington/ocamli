(* For now, just copy input to output, byte by byte *)
let copyfile infile outfile =
  let fhi = open_in_bin infile in
  let fho = open_out_bin outfile in
    try
      while true do output_char fho (input_char fhi) done
    with
      End_of_file ->
        close_in fhi;
        close_out fho

let _ =
  print_endline "Tppx running...";
  match Sys.argv with
    [|_; infile; outfile|] -> copyfile infile outfile
  | _ -> prerr_endline "Usage ppx_interpret <infile> <outfile>"

