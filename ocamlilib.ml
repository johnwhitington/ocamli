(* ocamlilib *)
(* Load a module from disk *)
open Ocamliutil
open Tinyocaml

let debug = ref false

let stdlib_dir =
  let tname = Filename.temp_file "ocaml" "ocamli" in
    ignore (Sys.command ("ocamlc -config >" ^ tname));
    let tmp = open_in tname in
      let line = ref "" in
        try
          while true do
            let s = input_line tmp in
              if
                String.length s >= 18 &&
                String.sub s 0 18 = "standard_library: "
              then
                line := s
          done;
          assert false
        with
          End_of_file ->
            close_in tmp;
            Sys.remove tname;
            if !line <> "" then
              (Filename.dir_sep ^
              (String.sub !line 19 (String.length !line - 19)))
            else
              raise (Failure "could not find standard library")

let definitions_of_module = function
  Struct (_, items) ->
    Ocamliutil.option_map
      (fun x ->
        match x with
          LetDef (recflag, bindings) -> Some (recflag, ref bindings)
        | _ -> None) 
      items
| _ -> failwith "definitions_of_module"

let rec add_prefix_to_pattern f = function
  PatVar v -> PatVar (f v)
| PatTuple ps -> PatTuple (List.map (add_prefix_to_pattern f) ps)
| PatArray ps -> PatArray (Array.map (add_prefix_to_pattern f) ps)
| x -> x (* FIXME: Fill in the rest *)

let add_prefix_to_binding name (pattern, e) =
  (add_prefix_to_pattern (fun x -> name ^ "." ^ x) pattern, e)

let add_prefix_to_bindings name (recflag, bindings) =
  (recflag, ref (List.map (add_prefix_to_binding name) !bindings))

let load_module (name : string) (env : env) (file : string) =
  if !debug then Printf.printf "Loading module %s...%!" name;
  let themod = Tinyocamlrw.of_real_ocaml (ast (load_file file)) in
    let themod' = Eval.eval false env themod in
      if !debug then Printf.printf "done\n%!";
      List.rev (List.map (add_prefix_to_bindings name) (definitions_of_module themod'))

(* FIXME This needs to reflect the link order of the OCaml standard library, so
that any module initialisations happen in the correct order. *)
(* FIXME Once we have the 'open' keyword working, we can use an 'open Pervasives'
to do pervasives automatically *)
let stdlib_modules =
  [(*("Unix", "./stdlib", "unix.ml");*)
   (*("Sys", stdlib_dir, "sys.ml"); 
   ("Callback", stdlib_dir, "callback.ml");
   ("Obj", stdlib_dir, "obj.ml");
   ("Array", stdlib_dir, "array.ml");*)
   ("List", "./stdlib", "testlist.ml");
   (*("List", stdlib_dir, "list.ml");*)
   (*("Pervasives", stdlib_dir, "pervasives.ml");
   ("CamlinternalFormatBasics", stdlib_dir, "camlinternalFormatBasics.ml")*)]

let loadlib () =
  List.fold_right
    (fun (n, lib, filename) libs ->
      load_module n libs (Filename.concat lib filename) @ libs)
    stdlib_modules
    []

let _ = Eval.lib := loadlib ()

let print_binding (pat, e) =
  Printf.printf "%s = %s\n" (to_string_pat pat) (Pptinyocaml.to_string e)

let showlib () =
  if !debug then
    List.iter
      (fun (recflag, bindings) ->
        print_string (if recflag then "let rec:\n" else "let:\n");
        List.iter print_binding !bindings)
      !Eval.lib
