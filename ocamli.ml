open Runeval
open Ocamliutil
open Tinyocaml

let version = "4.05.0"

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
let sidelets = ref false

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
  Ocamliprim.exe := Bytes.of_string "-e"

let argv str =
  Ocamliprim.argv := Array.of_list (Array.to_list !Ocamliprim.argv @ [Bytes.of_string str])

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

let printversion () =
  print_string version;
  print_string "\n";
  exit 0

let set_notypecheck =
  Ocamliutil.typecheck := false;
  Eval.runtime_typecheck := true

let argspec =
  [("-version", Arg.Unit printversion, " Print the version number of ocamli");
   ("-search", Arg.String (fun x -> make_regexp searchfor x; showall := true), " Show only matching evaluation steps");
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
   ("-show-pervasives", Arg.Set showpervasives, " Show more of pervasives");
   ("-real-ops", Arg.Set Tinyocamlrw.realops, " Don't treat :=, ref etc. as operators");
   ("-fast-curry", Arg.Set fastcurry, " Apply all curried arguments at once. ");
   ("-fast-for", Arg.Set Eval.fastfor, " Elide the evaluation of the inside of a FOR loop");
   ("-dtiny", Arg.Set debugtiny, " Show Tinyocaml representation");
   ("-dpp", Arg.Set debugpp, " Show the pretty-printed program");
   ("-debug", Arg.Unit setdebug, " Debug (for OCAMLRUNPARAM=b)");
   ("-debug-show-regexps", Arg.Set showregexps, " Debug output of computed regular expressions");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic");
   ("-no-if-bool", Arg.Set noifbool, "Don't show if false, if true stage");
   ("-no-var-lookup", Arg.Set novarlookup, "Don't show stage immediately after variable lookup");
   ("-no-peek", Arg.Clear Eval.dopeek, " Avoid peeking for debug");
   ("-no-syntax", Arg.Clear Pptinyocaml.syntax, " Don't use syntax highlighting");
   ("-tex-syntax", Arg.Set Pptinyocaml.syntax_tex, " Output tex instead of ANSI control codes");
   ("-no-typecheck", Arg.Clear Ocamliutil.typecheck, " Don't typecheck");
   ("-no-collect", Arg.Clear Eval.docollectunusedlets, " Don't collect unused lets");
   ("-no-stdlib", Arg.Clear Ocamlilib.load_stdlib, " Don't load the standard library");
   ("-side-lets", Arg.Set sidelets, "Show value-lets at the side");
   ("-otherlibs", Arg.Set_string Ocamlilib.otherlibs, " Location of OCaml otherlibs");
   ("-emulated-stdlib", Arg.Set Ocamliprim.emulated, " Use emulated Standard Library %externals");
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

let sls_width = ref 0

let print_sls_binding = function
  (PatVar v, e) ->
    let str = Pptinyocaml.to_string e in
    Printf.printf "%s = %s " v str;
| _ -> failwith "print_sls_binding"

let calculate_sls_width sls =
  List.fold_left ( + ) 0
    (List.map
      (function (PatVar v, e) ->
         String.length (Pptinyocaml.to_string e) + 4 + String.length v
       | _ -> failwith "set_sls_width")
      sls)

let set_sls_width sls =
  sls_width := max !sls_width (calculate_sls_width sls)

let print_sls ls =
  set_sls_width ls;
  List.iter print_sls_binding ls

(* For now, remove any in remove_rec *)
let filter_sl : binding -> binding option = function
  (PatVar p, e) when List.mem p !remove_recs -> None
| x -> Some x

let filter_sls (sls : binding list) =
  option_map filter_sl sls

(* Given a Tinyocaml.t, find all the bound names of all let bindings. Return a
 * list of them, including all duplicates. We use Eval.bound_in_bindings *)
let bound_names e =
  let names = ref [] in
    Tinyocaml.iter
      (function x ->
        (*Printf.printf "iter:%s\n" (Tinyocaml.to_string x);*)
        match x with
        | Let (_, bindings, _) ->
            names := bound_in_bindings bindings @ !names
        | x -> ())
      e;
    !names

(* Given a Tinyocaml.t, remove any outer value-lets, and return the new
 * Tinyocaml.t and the list of value-lets *)
(* For now, just single binding, just PatVar. *)
let lets = ref []

let rec find_sidelets allowed_names x =
  match x with
  | (Let (recflag, [(PatVar v, e)], t)) when Tinyocamlutil.is_value e && List.mem v allowed_names ->
      if !remove_rec_all && recflag then () else lets := (PatVar v, e)::!lets;
      find_sidelets allowed_names t
  | x -> Tinyocaml.recurse (find_sidelets allowed_names) x

let find_sidelets allowed_names e =
  lets := [];
  let e' = find_sidelets allowed_names e in
    (e', List.rev !lets)

let rec remove_items_with_duplicates = function
  [] -> []
| h::t ->
    if List.mem h t
      then remove_items_with_duplicates (List.filter (fun x -> x <> h) t)
      else h::remove_items_with_duplicates t

(* To qualify for extraction, a name must be singly bound in the whole expression *)
let find_sidelets tiny =
  let bound = bound_names tiny in
    let singly_bound_names = remove_items_with_duplicates bound in
      let allowed = List.filter (fun x -> not (List.mem x !remove_recs)) singly_bound_names in
        find_sidelets allowed tiny

let really_print_line sls line =
  if !upto > 0 then print_string "\n";
  List.iter
    (fun x -> print_string x; print_string "\n")
    (take_up_to !cache !upto);
  for x = 0 to !sls_width - 1 - calculate_sls_width sls do print_string " " done; 
  print_sls sls;
  print_string line

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
      sections b e codes count before during after t
| '\x1b'::'['::x::'m'::t ->
   let l = ['\x1b'; '['; x; 'm'] in
    let before, during, after = update count b e l before during after in
      let codes' = if count > b then codes else l::codes in
      sections b e codes' count before during after t
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
        let tiny, sls = if !sidelets then let t, sls = find_sidelets tiny in t, filter_sls sls else tiny, [] in
        let str = string_of_tiny ~preamble tiny in
        let str = if !highlight then highlight_search !searchfor s str else str in
        if not !silenced then really_print_line sls str;
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
      end;
  (* Update the cache *)
  cache := string_of_tiny ~preamble:"    " tiny :: !cache;
  clean_cache ();

external reraise : exn -> 'a = "%reraise"

let mainfile = ref ""

let setfile x =
  if not (Sys.file_exists x) then
    begin
      Printf.printf "File %s not found\n" x;
      exit 1
    end
  else
    begin
      mainfile := x;
      setfile x
    end

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
          (!show_simple_arithmetic || show_this_stage (Eval.last ()) (Eval.peek state') (Eval.tiny state) (Eval.tiny state')) &&
          (!showpervasives || show_this_pervasive_stage (Eval.last ()))
        then
          begin
            let preamble = (*if !skipped then "=>* " else*) "=>  " in
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
    let state = Eval.init (snd (Tinyocamlrw.of_real_ocaml !Eval.lib (Ocamliutil.ast ~filename:!mainfile code))) in
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
           None -> print_string "No source code provided.\n"; exit 1
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

