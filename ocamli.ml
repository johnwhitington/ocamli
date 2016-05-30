open Eval

let argspec =
  [("-machine", Arg.Set_string machine, " Set the abstract machine");
   ("-quiet", Arg.Set quiet, " Print only the result");
   ("-silent", Arg.Set silent, " Print only what the program prints");
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
   ("-debug", Arg.Set debug, " Debug (for OCAMLRUNPARAM=b)");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic");
   ("-no-peek", Arg.Clear Environment.dopeek, " Avoid peeking for debug");
   ("-no-syntax", Arg.Clear Pptinyocaml.syntax, " Don't use syntax highlighting");
   ("-no-collect", Arg.Clear Environment.docollectunusedlets, " Don't collect unused lets")]

let go () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program>
             [-pp <ocaml | tiny* | simple ]
             [-machine <naive | naiveSimple | naiveSimpleOneStep | environment*]\n";
  let module I =
    (val
       (try List.assoc !machine implementations with
         _ -> failwith "Unknown machine"
       ) : Evaluator)
  in
    I.fastcurry := !fastcurry;
    TinyocamlUtils.fastcurry := !fastcurry;
    Pptinyocaml.fastcurry := !fastcurry;
  let rec really_run first state =
    if !prompt then wait_for_enter ();
    Unix.sleepf !step;
    match I.next state with
      Next state' ->
        (*Printf.printf "Considering printing stage %s...skipped last is %b\n"
        (string_of_tiny ~preamble:"" (I.tiny state')) !skipped;*)
        begin if
          not (!quiet || !silent) &&
          (!show_simple_arithmetic || show_this_stage (I.last ()) (I.peek state') (I.tiny state) (I.tiny state')) &&
          (!showpervasives || show_this_pervasive_stage (I.last ()))
        then
          begin
            if !printer = "tiny" then
              begin
                let preamble = if !skipped then "=>* " else "=>  " in
                if I.newlines state then print_string "\n";
                print_string (string_of_tiny ~preamble (fixup (I.peek state') (I.tiny state')))
              end
            else
              ();
              (*print_string (to_string (getexpr (I.tree state')));*)
            skipped := false;
            if not !prompt then print_string "\n"
          end
        else
          skipped := true
        end;
        really_run false state'
    | IsValue ->
        (* Only print if !quiet. On Silent we don't want it, on normal, we have already printed *)
        if !quiet then begin
          if !printer = "tiny" then
            begin
              print_string (string_of_tiny ~preamble:"=>* " (fixup (I.peek state) (I.tiny state)))
            end
          else
            (); (*print_string (to_string (getexpr (I.tree state)));*)
          print_string "\n"
        end
    | Malformed s ->
        print_string "Malformed AST node\n";
        print_string s;
        print_string "\n"
    | Unimplemented s ->
        print_string "Unimplemented AST node\n";
        print_string s;
        print_string "\n"
  in
   let run code =
    if !printer = "simple" then
      begin
        Pptinyocaml.simple := true;
        printer := "tiny"
      end;
    Pptinyocaml.width := !width;
    let state = I.init (Evalutils.ast code) in
       if !debugtiny then
         begin
           print_string (Tinyocaml.to_string (I.tiny state));
           print_string "\n";
           flush stdout;
           exit 0
         end;
      if not (!quiet || !silent) then
        begin
          if !printer = "tiny" then
            print_string (string_of_tiny ~preamble:"    " (fixup (I.peek state) (I.tiny state)))
          else
            ();
            (*print_string (to_string (getexpr (I.tree state)));*)
          if not !prompt then print_string "\n"
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
       Environment.ExceptionRaised(n, payload) ->
         let expstr =
           match payload with None -> "" | Some p -> Pptinyocaml.to_string p
         in
           prerr_string (Printf.sprintf "Exception: %s %s.\n" n expstr)
     | e ->
         if !debug then raise e else Printf.eprintf "Error: [%s]\n" (Printexc.to_string e);
         exit 1

let () =
  if not !Sys.interactive then go ()

