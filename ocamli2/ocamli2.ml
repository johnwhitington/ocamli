let showsteps = ref false

let peek = ref true

let print = ref false

let programtext = ref ""

let first = ref true

let indent firstlinearrow str =
  firstlinearrow ^ (Util.string_replace_all "\n" "\n   " str)

let contains_newline s =
  Util.string_replace_all "\n" "xx" s <> s

let last_s = ref ""

let rec eval_full v =
  let pre () = let r = if !first then "   " else "=> " in first := false; r in
  if !showsteps then Printf.printf "%s\n" (Print.string_of_t v);
  if !print then
    begin
      let str = Print.to_string v in
        print_endline (indent (pre ()) str);
        if contains_newline str then print_newline ();
        exit 0
    end
  else
    begin
      flush stdout; if !Eval.showrules then print_endline "---Beginning of evaluation";
      let evalled = if !peek then Eval.eval Lib.stdlib true v else v in
      flush stdout; if !Eval.showrules then print_endline "---End of evaluation, beginning of printing";
      let str = Print.to_string evalled in
        if str <> !last_s then print_endline (indent (pre ()) str);
        last_s := str;
        flush stdout; if !Eval.showrules then print_endline "---End of printing";
        if contains_newline str then print_newline ();
        flush stdout; if Type.is_value v then v else eval_full (Eval.eval Lib.stdlib false v)
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
   "-dno-syntax", Arg.Clear Print.syntax, " Do not use syntax highlighting";
   "-dprint", Arg.Set print, " Just show the program, do not evaluate it";
   "-dsteps", Arg.Set showsteps, " Show information for each step of evaluation";
   "-dsteps-no-types", Arg.Clear Print.string_of_t_show_types, " But not types...";
   "-dnovals", Arg.Clear Print.showvals, " Do not show values in steps";
   "-dshowalllets", Arg.Set Print.show_all_lets, " Show even unused lets";
   "-dnopeek", Arg.Clear peek, " Do not peek";
   "-dnoprintas", Arg.Clear Print.printas, " Do not use printas";
   "-drules", Arg.Set Eval.showrules, " Show reduction rule for each step of evaluation"]

let _ =
  Arg.parse argspec setfile "Syntax: ocamli2 <filename | -e program>\n";
  eval_full (Read.read !programtext)
