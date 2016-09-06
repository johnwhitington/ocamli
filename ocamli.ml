open Runeval

let setdebug () =
  Runeval.debug := true;
  Eval.debug := true;
  Ocamlilib.debug := true;
  Pptinyocaml.debug := true

let searchfor = ref "" (* always matches *)
let searchuntil = ref "$a" (* never matches *)
let searchafter = ref "" (* always matches *)
let untilany = ref false
let afterany = ref false
let numresults = ref max_int
let invertsearch = ref false
let invertuntil = ref false
let invertafter = ref false

let argspec =
  [("-search", Arg.Set_string searchfor, " Show only matching evaluation steps");
   ("-invert-search", Arg.Set invertsearch, " Invert the search, showing non-matching steps");
   ("-n", Arg.Set_int numresults, " Show only <x> results");
   ("-until", Arg.Set_string searchuntil, " show only until this matches");
   ("-after", Arg.Set_string searchafter, " show only after this matches");
   ("-until-any", Arg.Set_string searchuntil, " show only until this matches");
   ("-after-any", Arg.Set_string searchafter, " show only after this matches");
   ("-invert-after", Arg.Set invertafter, " invert the after condition");
   ("-invert-until", Arg.Set invertuntil, " invert the until condition");
   ("-show", Arg.Set show, " Print the final result of the program");
   ("-show-all", Arg.Set showall, " Print steps of evaluation");
   ("-prompt", Arg.Set prompt, " Require enter after each step but last");
   ("-step", Arg.Set_float step, " Wait a number of seconds after each step but last");
   ("-pp", Arg.Set_string printer, " Set the prettyprinter");
   ("-width", Arg.Set_int width, " Set the output width");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-top", Arg.Set top, " Do nothing, exit cleanly (for top level)");
   ("-remove-rec", Arg.String add_remove_rec, " Do not print the given recursive function");
   ("-remove-rec-all", Arg.Set remove_rec_all, " Do not print any recursive functions");
   ("-show-pervasives", Arg.Set showpervasives, " Show Pervasives such as :=");
   ("-fast-curry", Arg.Set fastcurry, " Apply all curried arguments at once. ");
   ("-dtiny", Arg.Set debugtiny, " Show Tinyocaml representation");
   ("-dpp", Arg.Set debugpp, " Show the pretty-printed program");
   ("-debug", Arg.Unit setdebug, " Debug (for OCAMLRUNPARAM=b)");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic");
   ("-no-peek", Arg.Clear Eval.dopeek, " Avoid peeking for debug");
   ("-no-syntax", Arg.Clear Pptinyocaml.syntax, " Don't use syntax highlighting");
   ("-no-typecheck", Arg.Clear Ocamliutil.typecheck, " Don't typecheck");
   ("-no-collect", Arg.Clear Eval.docollectunusedlets, " Don't collect unused lets");
   ("-no-stdlib", Arg.Clear Ocamlilib.load_stdlib, " Don't load the standard library");
   ("-otherlibs", Arg.Set_string Ocamlilib.otherlibs, " Location of OCaml otherlibs")]

let linecount = ref 0

(* True if we are in printing range. i.e before or including 'until' and after or
including 'after' *)
let inrange = ref false

let print_line preamble tiny =
  let invert = if !invertsearch then not else (fun x -> x) in
    let s = string_of_tiny ~preamble:"" ~codes:false (Tinyocamlutil.strip_control tiny) in
      (* Check if we are entering the range *)
      if not !inrange && Str.string_match (Str.regexp !searchafter) s 0 then inrange := true;
      (* If it matches the search, and we are in the range, print the line *)
      if !inrange && invert (Str.string_match (Str.regexp !searchfor) s 0) then
        begin
          print_string (string_of_tiny ~preamble tiny);
          incr linecount;
        end;
      (* Check if we are leaving the range *)
      if !inrange && Str.string_match (Str.regexp !searchuntil) s 0 then inrange := false

(* Must be called following print_line, not before, because of range. Can we combine? *)
let would_print_line tiny =
  let invert = if !invertsearch then not else (fun x -> x) in
  let s = string_of_tiny ~preamble:"" ~codes:false (Tinyocamlutil.strip_control tiny) in
    !inrange && invert (Str.string_match (Str.regexp !searchfor) s 0)

let go () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program>\n";
  Eval.fastcurry := !fastcurry;
  Tinyocamlutil.fastcurry := !fastcurry;
  Pptinyocaml.fastcurry := !fastcurry;
  Ocamlilib.load_library ();
  Ocamlilib.showlib ();
  if !searchfor <> "" || !searchuntil <> "" || !searchafter <> "" then showall := true;
  let rec really_run first state =
    if !prompt then wait_for_enter ();
    Unix.sleepf !step;
    match Eval.next state with
      Next state' ->
        (*Printf.printf "Considering printing stage %s...skipped last is %b\n"
        (string_of_tiny ~preamble:"" (I.tiny state')) !skipped;*)
        begin if
          !showall &&  
          (!show_simple_arithmetic || show_this_stage (Eval.last ()) (Eval.peek
          state') (Eval.tiny state) (Eval.tiny state')) &&
          (!showpervasives || show_this_pervasive_stage (Eval.last ()))
        then
          begin
            let preamble = if !skipped then "=>* " else "=>  " in
            if Eval.newlines state then print_string "\n";
            print_line preamble (Eval.tiny state');
            skipped := false;
            if not !prompt && would_print_line (Eval.tiny state') then print_string "\n";
            if !linecount >= !numresults then exit 0
          end
        else
          skipped := true
        end;
        really_run false state'
    | IsValue ->
        (* Only print if !quiet. On Silent we don't want it, on normal, we have already printed *)
        if !show && not !showall then begin
          print_line "" (Eval.tiny state);
          print_string "\n"
        end
    | Malformed s ->
        print_string "Malformed AST node\n";
        print_string s;
        print_string "\n";
        if !debug then raise Exit
    | Unimplemented s ->
        print_string "Unimplemented AST node\n";
        print_string s;
        print_string "\n";
        if !debug then raise Exit
  in
   let run code =
    if !printer = "simple" then Pptinyocaml.simple := true;
    Pptinyocaml.width := !width;
    let state = Eval.init (Tinyocamlrw.of_real_ocaml !Eval.lib (Ocamliutil.ast code)) in
       if !debugtiny then
         begin
           print_string (Tinyocaml.to_string (Eval.tiny state));
           print_string "\n";
           flush stdout;
           exit 0
         end;
      if !showall then
        begin
          if !linecount >= !numresults then exit 0;
          print_line "    " (Eval.tiny state);
          if not !prompt && would_print_line (Eval.tiny state) then print_string "\n";
          if !linecount >= !numresults then exit 0
        end;
      if !debugpp then exit 0;
      really_run true state
   in
     try
       if not !top then
         match load_code () with
           None -> failwith "No source code provided"
         | Some x -> run x
     with
       Eval.ExceptionRaised(n, payload) ->
         let expstr =
           match payload with None -> "" | Some p -> Pptinyocaml.to_string p
         in
           prerr_string (Printf.sprintf "Exception: %s %s.\n" n expstr)
     | e ->
         if !debug then raise e else Printf.eprintf "Error: [%s]\n" (Printexc.to_string e);
         exit 1

let () =
  if not !Sys.interactive then go ()

