open Nanocaml

let _ = Pptinyocaml.simple := true

(* string -> ocaml ast -> tinyocaml ast -> newtype ast *)
let tinyocaml_of_program_text s =
  snd (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

(* newtype ast -> tinyocaml ast -> pptinyocaml -> string *)
let to_program_text x =
  Pptinyocaml.to_string (Nanocamlrw.to_tinyocaml x)

let show p =
  print_string (to_program_text p);
  print_string "\n"

(* Run the program p *)
let run p = Eval2.eval [] p

let rec run_step p =
  show p;
  if is_value p then () else
    let p' = Eval2.seval [] p in
      run_step p'

type mode =
  FromFile of string
| FromText of string

let source = ref None

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

let load_code () =
  match !source with
    Some (FromFile s) -> Some (Ocamliutil.load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let step = ref false
let dnano = ref false
let dtiny = ref false

let argspec =
  [("-e", Arg.String settext, " Evaluate the program text given");
   ("-dnano", Arg.Set dnano, " Show nanocaml representation");
   ("-dtiny", Arg.Set dtiny, " Show tinyocaml representation");
   ("-show-all", Arg.Set step, " Evaluate step-by-step")]

let _ =
  Arg.parse argspec setfile "Syntax: newtype <filename | -e program>\n";
  match load_code () with
    Some code ->
      let tiny = tinyocaml_of_program_text code in
        if !dtiny then Printf.printf "%s\n" (Tinyocaml.to_string tiny);
      let p = Nanocamlrw.of_tinyocaml tiny in
        if !dnano then Printf.printf "%s\n" (Nanocaml.string_of_t p);
        if !step then
          run_step p
        else
          begin
            show p;
            print_string "\n";
            show (run p)
          end
  | None ->
      Printf.eprintf "No source code provided.\n";
      exit 2

