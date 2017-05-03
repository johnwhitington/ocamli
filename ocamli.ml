open Runeval
open Ocamliutil

let setdebug () =
  Runeval.debug := true;
  Eval.debug := true;
  Ocamlilib.debug := true;
  Pptinyocaml.debug := true;
  Ocamliprim.debug := true

let reverse_video = "\x1b[7m"
let code_end = "\x1b[0m"

let searchfor = ref (Str.regexp "") (* always matches *)
let searchuntil = ref (Str.regexp "$a") (* never matches *)
let searchafter = ref (Str.regexp "") (* always matches *)
let untilany = ref false
let afterany = ref false
let numresults = ref max_int
let invertsearch = ref false
let invertuntil = ref false
let invertafter = ref false
let stopaftersearch = ref false
let upto = ref 0
let repeat = ref false
let silenced = ref false
let highlight = ref false
let regexp = ref false
let showregexps = ref false
let noparens = ref false

let set_until_any s =
  untilany := true;
  searchuntil := Str.regexp s

let set_after_any s =
  afterany := true;
  searchafter := Str.regexp s

let make_regexp reference str =
  reference :=
    if !regexp
      then Str.regexp str
      else
        let r = Ocamlipat.regexp_of_lexbuf (Lexing.from_string str) in 
          if !showregexps then Printf.printf "Made search term %S\n" (Ocamlipat.regexp_of_string str);
          Str.regexp r

let times = ref 1

let _ =
  Ocamliprim.exe := "-e"

let argv str =
  Ocamliprim.argv := Array.of_list (Array.to_list !Ocamliprim.argv @ [str])

let ename = ref ""

let setename s =
  ename := s

(* For "Mod" check for Mod.cmi and mod.cmi. If neither exists, create Mod.cmi,Mod.cmo *)
let create_cmi_cmo (ename : string) (text : string) =
  if ename = "" then raise (Invalid_argument "create_cmi");
  let uncap =
    let chars = explode ename in
      implode (Char.lowercase_ascii (List.hd chars)::(List.tl chars))
  in
    if not (Sys.file_exists (ename ^ ".ml")) && not (Sys.file_exists (uncap ^ ".ml")) then
      begin
        let fh = open_out (ename ^ ".ml") in
          output_string fh text;
          close_out fh;
        ignore (Sys.command ("ocamlc " ^ ename ^ ".ml"))
      end

let settext s =
  Runeval.settext ~modname:!ename s;
  if !ename <> "" then create_cmi_cmo !ename s;
  ename := ""

let argspec =
  [("-search", Arg.String (fun x -> make_regexp searchfor x; showall := true), " Show only matching evaluation steps");
   ("-regexp", Arg.Set regexp, " Search terms are regular expressions rather than the built-in system");
   ("-no-parens", Arg.Set noparens, "Ignore parentheses and begin and end when matching with classic search syntax");
   ("-invert-search", Arg.Set invertsearch, " Invert the search, showing non-matching steps");
   ("-highlight", Arg.Set highlight, "Highlight the matching part of each matched step.");
   ("-n", Arg.Set_int numresults, " Show only <x> results");
   ("-until", Arg.String (fun x -> make_regexp searchuntil x; showall := true), " show only until this matches a printed step");
   ("-after", Arg.String (fun x -> make_regexp searchafter x; showall := true), " show only after this matches a printed step");
   ("-until-any", Arg.String set_until_any, " show only until this matches any step");
   ("-after-any", Arg.String set_after_any, " show only after this matches any step");
   ("-invert-after", Arg.Set invertafter, " invert the after condition");
   ("-invert-until", Arg.Set invertuntil, " invert the until condition");
   ("-stop", Arg.Set stopaftersearch, " stop computation after final search results");
   ("-repeat", Arg.Set repeat, " allow the after...until result to be repeated.");
   ("-upto", Arg.Set_int upto, " show n lines up to each result line");
   ("-show", Arg.Set show, " Print the final result of the program");
   ("-show-all", Arg.Set showall, " Print steps of evaluation");
   ("-show-stdlib-init", Arg.Set Ocamlilib.showstdlibinit, " Show initialisation of standard library");
   ("-prompt", Arg.Set prompt, " Require enter after each step but last");
   ("-step", Arg.Set_float step, " Wait a number of seconds after each step but last");
   ("-pp", Arg.Set_string printer, " Set the prettyprinter");
   ("-width", Arg.Set_int width, " Set the output width");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-e-name", Arg.String setename, " Set the module name for the next -e instance");
   ("-top", Arg.Set top, " Do nothing, exit cleanly (for top level)");
   ("-remove-rec", Arg.String add_remove_rec, " Do not print the given recursive function");
   ("-remove-rec-all", Arg.Set remove_rec_all, " Do not print any recursive functions");
   ("-show-pervasives", Arg.Set showpervasives, " Show Pervasives such as :=");
   ("-fast-curry", Arg.Set fastcurry, " Apply all curried arguments at once. ");
   ("-dtiny", Arg.Set debugtiny, " Show Tinyocaml representation");
   ("-dpp", Arg.Set debugpp, " Show the pretty-printed program");
   ("-debug", Arg.Unit setdebug, " Debug (for OCAMLRUNPARAM=b)");
   ("-debug-show-regexps", Arg.Set showregexps, " Debug output of computed regular expressions");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic");
   ("-no-peek", Arg.Clear Eval.dopeek, " Avoid peeking for debug");
   ("-no-syntax", Arg.Clear Pptinyocaml.syntax, " Don't use syntax highlighting");
   ("-no-typecheck", Arg.Clear Ocamliutil.typecheck, " Don't typecheck");
   ("-no-collect", Arg.Clear Eval.docollectunusedlets, " Don't collect unused lets");
   ("-no-stdlib", Arg.Clear Ocamlilib.load_stdlib, " Don't load the standard library");
   ("-otherlibs", Arg.Set_string Ocamlilib.otherlibs, " Location of OCaml otherlibs");
   ("-times", Arg.Set_int times, " Do many times");
   ("--", Arg.Rest argv, "")]

let linecount = ref 0

(* True if we are in printing range. i.e before or including 'until' and after or
including 'after' *)
let inrange = ref false

let take l n =
  if n < 0 then raise (Invalid_argument "take") else
  let rec take_inner r l n =
    if n = 0 then List.rev r else
      match l with
      | [] -> raise (Invalid_argument "take")
      | h::t -> take_inner (h::r) t (n - 1)
  in
    take_inner [] l n

let rec take_up_to l n =
  if n < 0 then raise (Invalid_argument "take_up_to") else
    try take l n with _ -> take_up_to l (n - 1)

let cache = ref []

let clean_cache () =
  cache := try take_up_to !cache !upto with _ -> []

let really_print_line line =
  if !upto > 0 then print_string "\n";
  List.iter
    (fun x -> print_string x; print_string "\n")
    (take_up_to !cache !upto);
  print_string line

(* Make a list of characters from a string, preserving order. *)
let explode s =
  let l = ref [] in
    for p = String.length s downto 1 do
      l := String.unsafe_get s (p - 1)::!l
    done;
    !l

(* Make a string from a list of characters, preserving order. *)
let implode l =
  let s = Bytes.create (List.length l) in
    let rec list_loop x = function
       [] -> ()
     | i::t -> Bytes.unsafe_set s x i; list_loop (x + 1) t
    in
      list_loop 0 l;
      Bytes.to_string s

(* To highlight a string, we proceed through it, counting all non-escaped
 * characters, to determine start and end points.
 * a) At position 's' we add a highlight-start code.
 * b) At position 'e', we add and end-code and then re-emit any start codes
 * occuring since the last end-code
 * First we build three sections: before, during and after. Return also any
 * start-codes in operation (i.e not cancelled) at 'e'. *)
let update count b e l before during after =
  if count >= e then (before, during, List.rev l @ after)
  else if count >= b then (before, List.rev l @ during, after)
  else (List.rev l @ before, during, after)

let rec sections b e codes count before during after = function
  '\x1b'::'['::'0'::'m'::t ->
    let l = ['\x1b'; '['; 'm'] in
    let before, during, after = update count b e l before during after in
      sections b e [] count before during after t
| '\x1b'::'['::x::'m'::t ->
    let l = ['\x1b'; '['; x; 'm'] in
    let before, during, after = update count b e l before during after in
      sections b e (l::codes) count before during after t
| h::t ->
    let before, during, after = update count b e [h] before during after in
      sections b e codes (count + 1) before during after t
| [] ->
    (List.rev before, List.rev during, List.rev after, codes)

let highlight_charlist b e chars =
  let before, during, after, codes = sections b e [] 0 [] [] [] chars in
      before @ explode reverse_video @ during @ explode code_end
    @ (List.flatten codes) @ after

let highlight_string b e s =
  implode (highlight_charlist b e (explode s))

let highlight_search regexp plainstr str =
  ignore (Str.search_forward regexp plainstr 0);
  let beginning = Str.match_beginning () + 4 in
  let theend = Str.match_end () + 4 in
    highlight_string beginning theend str

let print_line newline preamble tiny =
  cache := string_of_tiny ~preamble:"    " tiny :: !cache;
  clean_cache ();
  let invert x = if x then not else (fun x -> x) in
  let s = string_of_tiny ~preamble:"" ~codes:false (Tinyocamlutil.strip_control tiny) in
  let matched =
    (invert !invertsearch)
    (try ignore (Str.search_forward !searchfor s 0); true with Not_found -> false)
  in
  let matched_until =
    (!untilany || matched) &&
    (invert !invertuntil)
    (try ignore (Str.search_forward !searchuntil s 0); true with Not_found -> false)
  in
  let matched_after =
    (!afterany || matched) &&
    (invert !invertafter)
    (try ignore (Str.search_forward !searchafter s 0); true with Not_found -> false)
  in
    (* Check if we are entering the range *)
    if not !inrange && matched_after then inrange := true;
    (* If it matches the search, and we are in the range, print the line *)
    if !inrange && matched then
      begin
        let str = string_of_tiny ~preamble tiny in
        let str = if !highlight then highlight_search !searchfor s str else str in
        if not !silenced then really_print_line str;
        if newline && not !silenced then print_string "\n";
        flush stdout;
        incr linecount;
      end;
    (* Check if we are leaving the range. If so, set numresults = 0 to prevent
    more printing, but allow computation to continue (unless -stop is set). If
    'repeat' is set, we continue as normal, waiting for the next -after
    condition. If repeat, we output an extra newline to demarcate the results. *)
    if !inrange && matched_until then
      begin
        inrange := false;
        if !repeat then print_string "\n" else numresults := 0
      end

external reraise : exn -> 'a = "%reraise"

let mainfile = ref ""

let setfile x =
  mainfile := x;
  setfile x

let go () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program> [-- arg1 arg2 ...]\n";
  Ocamliprim.argv := Array.of_list (!Ocamliprim.exe::Array.to_list !Ocamliprim.argv);
  Eval.fastcurry := !fastcurry;
  Tinyocamlutil.fastcurry := !fastcurry;
  Pptinyocaml.fastcurry := !fastcurry;
  Ocamlilib.load_library ();
  (*Ocamlilib.showlib ();*)
  (*if !searchfor <> "" || !searchuntil <> "" || !searchafter <> "" then showall
   * := true;*)
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
            print_line (not !prompt) preamble (Eval.tiny state');
            skipped := false;
            if !linecount >= !numresults && !stopaftersearch then raise Exit;
            if !linecount >= !numresults && not !stopaftersearch then silenced := true
          end
        else
          skipped := true
        end;
        really_run false state'
    | IsValue ->
        (* Only print if !quiet. On Silent we don't want it, on normal, we have already printed *)
        if !show && not !showall then print_line true "" (Eval.tiny state)
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
    let state = Eval.init (Tinyocamlrw.of_real_ocaml !Eval.lib (Ocamliutil.ast ~filename:!mainfile code)) in
       if !debugtiny then
         begin
           print_string (Tinyocaml.to_string (Eval.tiny state));
           print_string "\n";
           flush stdout;
           raise Exit
         end;
      if !showall then
        begin
          if !linecount >= !numresults && !stopaftersearch then raise Exit;
          print_line (not !prompt) "    " (Eval.tiny state);
          if !linecount >= !numresults && !stopaftersearch then raise Exit
        end;
      if !debugpp then raise Exit;
      for _ = 1 to !times do really_run true state done
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
           prerr_string (Printf.sprintf "Exception: %s%s.\n" n (if expstr = "" then "" else " " ^ expstr))
     | Exit ->
         if !debug then reraise Exit;
         exit 0
     | e ->
         if !debug then raise e else Printf.eprintf "Error: [%s]\n" (Printexc.to_string e);
         exit 1

let () =
  if not !Sys.interactive then go ()

