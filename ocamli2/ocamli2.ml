let showsteps = ref false

let peek = ref true

let print = ref false

let programtext = ref ""

let first = ref true

let indent firstlinearrow str =
  firstlinearrow ^ (Ocamli2util.string_replace_all "\n" "\n   " str)

let contains_newline s =
  Ocamli2util.string_replace_all "\n" "xx" s <> s

let rec eval_full v =
  let pre () = let r = if !first then "   " else "=> " in first := false; r in
  if !showsteps then Printf.printf "%s\n" (Ocamli2print.string_of_t v);
  if !print then
    begin
      let str = Ocamli2print.to_string v in
        print_endline (indent (pre ()) str);
        if contains_newline str then print_newline ();
        exit 0
    end
  else
    begin
      flush stdout; if !Ocamli2eval.showrules then print_endline "---Beginning of evaluation";
      let evalled = if !peek then Ocamli2eval.eval [] true v else v in
      flush stdout; if !Ocamli2eval.showrules then print_endline "---End of evaluation, beginning of printing";
      let str = Ocamli2print.to_string evalled in
        print_endline (indent (pre ()) str);
        flush stdout; if !Ocamli2eval.showrules then print_endline "---End of printing";
        if contains_newline str then print_newline ();
        flush stdout; if Ocamli2type.is_value v then v else eval_full (Ocamli2eval.eval [] false v)
    end

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
   "-dprint", Arg.Set print, " Just show the program, do not evaluate it";
   "-dsteps", Arg.Set showsteps, " Show information for each step of evaluation";
   "-dnovals", Arg.Clear Ocamli2print.showvals, "Do not show values in steps";
   "-dshowalllets", Arg.Set Ocamli2print.show_all_lets, "Show even unused lets";
   "-dnopeek", Arg.Clear peek, "Do not peek";
   "-drules", Arg.Set Ocamli2eval.showrules, " Show reduction rule for each step of evaluation"]

let _ =
  Arg.parse argspec setfile "Syntax: ocamli2 <filename | -e program>\n";
  eval_full (Ocamli2read.finaltype_of_typedtree (Ocamli2read.typedtree_of_string !programtext))
