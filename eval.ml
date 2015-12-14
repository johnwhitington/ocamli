(* Command line interface *)

(* In debug mode, extra info is printed. *)
let debug = ref false

(* In quiet mode, only the final value is printed *)
let quiet = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref None

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let run _ = ()

let argspec =
  [("-quiet", Arg.Set quiet, " Print only the result");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-debug", Arg.Set debug, " Print extra information")]

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let () =
  Arg.parse argspec setfile
    "Syntax: eval <naive | cc | scc | ck | cek | secd> <filename | -e program>";
  try run (load_code ()) with
  | e -> Printf.eprintf "Uncaught Error: [%s]\n" (Printexc.to_string e); exit 1


