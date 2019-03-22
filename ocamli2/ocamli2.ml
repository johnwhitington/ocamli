let programtext = ref ""

let setfile filename =
  programtext := Util.load_file filename

let argspec =
  ["-e", Arg.Set_string programtext, " Set program text";
   "-dno-syntax", Arg.Clear Print.syntax, " Do not use syntax highlighting";
   "-dprint", Arg.Set Eval.print, " Just show the program, do not evaluate it";
   "-dsteps", Arg.Set Eval.showsteps, " Show information for each step of evaluation";
   "-dsteps-no-types", Arg.Clear Print.string_of_t_show_types, " But not types...";
   "-dnovals", Arg.Clear Print.showvals, " Do not show values in steps";
   "-dshowalllets", Arg.Set Print.show_all_lets, " Show even unused lets";
   "-dnopeek", Arg.Clear Eval.peek, " Do not peek";
   "-dnoprintas", Arg.Clear Print.printas, " Do not use printas";
   "-drules", Arg.Set Eval.showrules, " Show reduction rule for each step of evaluation"]

let _ =
  Arg.parse argspec setfile "Syntax: ocamli2 <filename | -e program>\n";
  let read = Read.read !programtext in
    Eval.eval_full Lib.stdlib read
