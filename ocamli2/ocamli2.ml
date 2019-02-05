let showsteps = ref false

let rec eval_full v =
  if !showsteps then Printf.printf "%s\n" (Ocamli2print.string_of_t v);
  Printf.printf "%s\n" (Ocamli2print.to_string ~preamble:"" (Ocamli2eval.eval [] true v));
  if Ocamli2type.is_value v then v else eval_full (Ocamli2eval.eval [] false v)

let programtext = ref ""

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let setfile filename =
  programtext := load_file filename

let argspec =
  ["-e", Arg.Set_string programtext, " Set program text";
   "-dno-syntax", Arg.Clear Ocamli2print.syntax, " Do not use syntax highlighting";
   "-dsteps", Arg.Set showsteps, " Show information for each step of evaluation";
   "-dnovals", Arg.Clear Ocamli2print.showvals, "Do not show values in steps";
   "-dshowalllets", Arg.Set Ocamli2print.show_all_lets, "Show even unused lets";
   "-drules", Arg.Set Ocamli2eval.showrules, " Show reduction rule for each step of evaluation"]

let _ =
  Arg.parse argspec setfile "Syntax: ocamli2 <filename | -e program>\n";
  eval_full (Ocamli2read.finaltype_of_typedtree (Ocamli2read.typedtree_of_string !programtext))
