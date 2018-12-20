let showsteps = ref false

let rec eval_full v =
  if !showsteps then Printf.printf "%s\n" (Ocamli2type.string_of_t v);
  Printf.printf "%s\n" (Pptinyocaml.to_string (Ocamli2write.tinyocaml_of_finaltype v));
  if Ocamli2type.is_value v then v else eval_full (Ocamli2eval.eval [] v)

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
   "-dsteps", Arg.Set showsteps, " Show information for each step of evaluation";
   "-dshow-all-lets", Arg.Set Ocamli2write.show_all_lets, " Show all lets, even shadowed or unused"]

let _ =
  Arg.parse argspec setfile "Syntax: ocamli2 <filename | -e program>\n";
  eval_full (Ocamli2read.finaltype_of_typedtree (Ocamli2read.typedtree_of_string !programtext))
