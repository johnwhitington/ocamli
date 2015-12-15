let quiet = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref None

let machine = ref "naive"

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

(* Main loop. Get the right abstract machine module, and keep calling it until
IsValue or Malformed or Unimplemented *)
let ast code =
  code |> Lexing.from_string |> Parse.implementation

let rec really_run first state =
  match Naive.next state with
    Naive.Next state' ->
      if not !quiet then begin
        print_string (Naive.repr state');
        print_string "\n"
      end;
      really_run false state'
  | Naive.IsValue ->
      if !quiet || first then begin
        print_string (Naive.repr state);
        print_string "\n"
      end
  | Naive.Malformed s ->
      print_string "Malformed AST node\n";
      print_string s;
      print_string "\n"
  | Naive.Unimplemented s ->
      print_string "Unimplemented AST node\n";
      print_string s;
      print_string "\n"

let run code =
  let state = Naive.init (ast code) in
    really_run true state

let argspec =
  [("-machine", Arg.Set_string machine, " Set the abstract machine");
   ("-quiet", Arg.Set quiet, " Print only the result");
   ("-e", Arg.String settext, " Evaluate the program text given")]

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program>
             [-quiet]
             [-machine <naive | cc | scc | ck | cek | secd>]\n";
  try
    match load_code () with
      None -> failwith "No source code provided"
    | Some x -> run x
  with
    e -> Printf.eprintf "Error: [%s]\n" (Printexc.to_string e); exit 1


