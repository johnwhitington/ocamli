open Tinyocaml
open Ocamliutil

let debug = ref false

(* Beginning of what was ocamilib.ml *)
let load_stdlib = ref true

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

(* Read the definitions from a loaded module, so they can be put into the
environment. *)
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
  let themod = Tinyocamlrw.of_real_ocaml env (ast (load_file file)) in
    (*let themod' = Eval.eval_until_value false env themod in (* <-- module
     * initialisation *)*)
      if !debug then Printf.printf "done\n%!";
      List.rev (List.map (add_prefix_to_bindings name) (definitions_of_module themod))

let stdlib_modules =
  [(*("Foo",                    "./stdlib", "foo.ml");*)
   (*("Unix",                     "./stdlib", "unix.ml"); (* Calling Printexc give Not_found *)
   (*("Bigarray",                 "./stdlib", "bigarray.ml"); (* unknown node * fun*) *)
   (* bigarray, thread, num, str, graphics, any others? *)
   (*("stdLabels",                stdlib_dir, "stdLabels.ml");
   ("moreLabels",               stdlib_dir, "moreLabels.ml");
   ("stringLabels",             stdlib_dir, "stringLabels.ml");
   ("bytesLabels",              stdlib_dir, "bytesLabels.ml");
   ("listLabels",               stdlib_dir, "listLabels.ml");
   ("arrayLabels",              stdlib_dir, "arrayLabels.ml");*) (* FIXME labels *)
   ("Complex",                  stdlib_dir, "complex.ml");
   (*("Filename",                 stdlib_dir, "filename.ml"); (* unknown node * *)*)
   (*("Emphemeron",               stdlib_dir, "ephemeron.ml"); (* unknown * structure item *)*)
   (*("Genlex",                   stdlib_dir, "genlex.ml"); (* FIXME: unknown * node *)*)
   (*("CamlinternalMod",          stdlib_dir, "camlinternalMod.ml"); (* FIXME * unknow node *)*)
   (*("Oo",                       stdlib_dir, "oo.ml"); FIXME Depends on * camlinternalOO *)
   (*("CamlinternalOO",           stdlib_dir, "camlinternalOO.ml"); FIXME of_real_ocaml_module_expr *)
   ("Callback",                 stdlib_dir, "callback.ml");
   (*("Scanf",                    stdlib_dir, "scanf.ml"); (* FIXME Unknown * structure item *)*)
   ("Uchar",                    stdlib_dir, "uchar.ml");
   (*("Format",                  stdlib_dir, "format.ml"); (* FIXME Unknown * pattern *)*)
   (*("Weak",                     stdlib_dir, "weak.ml"); FIXME Unknown structure item *)
   (*("Hashtbl",                  stdlib_dir, "hashtbl.ml"); FIXME Unknown node *)
   (*("Random",                   stdlib_dir, "random.ml"); FIXME Unknown record entry type *)
   (*("Digest",                   stdlib_dir, "digest.ml"); (* Unknown pattern * *)*)
   ("Gc",                       stdlib_dir, "gc.ml");
   ("Printexc",                 stdlib_dir, "printexc.ml");
   (*("Arg",                      stdlib_dir, "arg.ml"); (* FIXME unknown node * fun (labelled/optional *)*)
   ("Printf",                   stdlib_dir, "printf.ml"); (* FIXME Printf doesn't work - give Not_found *)
   ("CamlinternalFormat",       "./stdlib", "camlinternalFormat.ml"); (* FIXME Parse labelled and optional arguments *)
   ("Buffer",                   stdlib_dir, "buffer.ml");
   (*("Stream",                   stdlib_dir, "stream.ml");*) (* Unknown pattern *)
   (*("Lazy",                     stdlib_dir, "lazy.ml");*)
   ("CamlinternalLazy",         stdlib_dir, "camlinternalLazy.ml");
   (*("Queue",                    stdlib_dir, "queue.ml"); (* FIXME Unknown * pattern *)*)
   ("Stack",                    stdlib_dir, "stack.ml");
   (*("Map",                      stdlib_dir, "map.ml");*)
   (*("Set",                      stdlib_dir, "set.ml");*)
   ("Parsing",                  stdlib_dir, "parsing.ml");
   ("Lexing",                   stdlib_dir, "lexing.ml");
   ("Nativeint",                stdlib_dir, "nativeint.ml");
   ("Int64",                    stdlib_dir, "int64.ml");
   ("Int32",                    stdlib_dir, "int32.ml");
   ("Array",                    stdlib_dir, "array.ml");
   ("Obj",                      stdlib_dir, "obj.ml");
   ("Marshal",                  stdlib_dir, "marshal.ml");
   ("Sort",                     stdlib_dir, "sort.ml");*)
   ("Sys",                      "./stdlib", "sys.ml");
   (*("String",                   stdlib_dir, "string.ml");*)
   ("Bytes",                    stdlib_dir, "bytes.ml");
   ("Char",                     stdlib_dir, "char.ml");
   ("List",                     stdlib_dir, "list.ml");
   ("Pervasives",               stdlib_dir, "pervasives.ml");
   ("CamlinternalFormatBasics", stdlib_dir, "camlinternalFormatBasics.ml")]

let loadlib () =
  List.fold_right
    (fun (n, lib, filename) libs ->
      load_module n libs (Filename.concat lib filename) @ libs)
    stdlib_modules
    []

let load_library () =
  if !load_stdlib then Eval.lib := loadlib ()

let print_binding (pat, e) =
  Printf.printf "%s = %s\n" (to_string_pat pat) (Pptinyocaml.to_string e)

let showlib () =
  if !debug then
    List.iter
      (fun (recflag, bindings) ->
        print_string (if recflag then "let rec:\n" else "let:\n");
        List.iter print_binding !bindings)
      !Eval.lib
