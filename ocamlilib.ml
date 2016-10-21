open Tinyocaml
open Ocamliutil

let debug = ref false
let otherlibs = ref ""
let showstdlibinit = ref false

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
let rec add_prefix_to_pattern f = function
  PatVar v -> PatVar (f v)
| PatTuple ps -> PatTuple (List.map (add_prefix_to_pattern f) ps)
| PatArray ps -> PatArray (Array.map (add_prefix_to_pattern f) ps)
| x -> x (* FIXME: Fill in the rest *)

let add_prefix_to_binding name (pattern, e) =
  if name = "" then (pattern, e) else (* pervasives *)
  (add_prefix_to_pattern (fun x -> name ^ "." ^ x) pattern, e)

let add_prefix_to_bindings name envitem =
  match envitem with
    EnvBinding (recflag, bindings) ->
      EnvBinding(recflag, ref (List.map (add_prefix_to_binding name) !bindings))
  | EnvFunctor (n, i, a, b, c) ->
      if name = "" then EnvFunctor (n, i, a, b, c) else (* pervasives *)
      EnvFunctor (name ^ "." ^ n, i, a, b, c)

let rec definitions_of_module (env : Tinyocaml.env) = function
  Struct (_, items) ->
    List.flatten 
      (List.map
        (fun x ->
          match x with
            LetDef (recflag, bindings) -> [EnvBinding (recflag, ref bindings)]
          | ModuleBinding (name, (Struct (_, items) as themod)) ->
              load_module_from_struct name env themod
          | ModuleBinding (name, Functor (fname, ftype, fcontents)) ->
              [EnvFunctor (name, fname, ftype, fcontents, [])] (* FIXME env?*)
          | _ -> []) 
        items)
| s ->
    failwith (Printf.sprintf "definitions_of_module: found a %s" (Tinyocaml.to_string s))

and load_module_from_struct name (env : Tinyocaml.env) themod : Tinyocaml.env =
  let themod = Eval.eval_until_value false env themod in (* <-- module initialisation  *)
    List.rev (List.map (add_prefix_to_bindings name) (definitions_of_module env themod))

and load_module (name : string) (env : Tinyocaml.env) (file : string) : Tinyocaml.env =
  if !debug then Printf.printf "Loading module %s...%!" name;
    let themod = Tinyocamlrw.of_real_ocaml env (ast (load_file file)) in
      if !debug then Printf.printf "read...%!";
      let r = load_module_from_struct name env themod in
      if !debug then Printf.printf "done\n%!";
      r

let otherlib_modules () =
  [("Unix",                    !otherlibs, "unix.ml");
   (*("Num",                     !otherlibs, "num.ml");*) (* Not_found *)
   (*("Str",                     !otherlibs, "str.ml");*) (* modinit fails *)
   (*("Threads",                 !otherlibs, "thread.ml");*) (* not clear what files to use *)
   (*("Graphics",                !otherlibs, "graphics.ml");*) (* slow... *)
   ("Bigarray",                 !otherlibs, "bigarray.ml")]

(*let stdlib_modules () =
  [("Example", "./stdlib", "example.ml")]*)

let stdlib_modules () =
  [("StdLabels",                stdlib_dir, "stdLabels.ml");
   ("MoreLabels",               stdlib_dir, "moreLabels.ml");
   ("StringLabels",             stdlib_dir, "stringLabels.ml");
   ("BytesLabels",              stdlib_dir, "bytesLabels.ml");
   ("ListLabels",               stdlib_dir, "listLabels.ml");
   ("ArrayLabels",              stdlib_dir, "arrayLabels.ml");
   ("Complex",                  stdlib_dir, "complex.ml");
   (*("Filename",                 stdlib_dir, "filename.ml"); a bit slow *)
   ("Emphemeron",               stdlib_dir, "ephemeron.ml");
   ("Genlex",                   stdlib_dir, "genlex.ml");
   ("CamlinternalMod",          stdlib_dir, "camlinternalMod.ml");
   ("Oo",                       stdlib_dir, "oo.ml");
   ("CamlinternalOO",           stdlib_dir, "camlinternalOO.ml");
   ("Callback",                 stdlib_dir, "callback.ml");
   (*("Scanf",                    "./stdlib", "scanf.ml"); (* let-refactoring will fix. See programs/scanf_fail.ml *)*)
   ("Uchar",                    stdlib_dir, "uchar.ml");
   (*("Format",                   "./stdlib", "format.ml"); (* stack overflow.
    * * * * something to do with camlinternalformat. Again, probably lets.  *)*)
   ("Weak",                     stdlib_dir, "weak.ml");
   ("Hashtbl",                  stdlib_dir, "hashtbl.ml");
   ("Random",                   stdlib_dir, "random.ml");
   ("Digest",                   stdlib_dir, "digest.ml");
   ("Gc",                       stdlib_dir, "gc.ml");
   ("Printexc",                 stdlib_dir, "printexc.ml");
   ("Arg",                      stdlib_dir, "arg.ml");
   ("Printf",                   stdlib_dir, "printf.ml");
   ("CamlinternalFormat",       stdlib_dir, "camlinternalFormat.ml");
   ("Buffer",                   stdlib_dir, "buffer.ml");
   ("Stream",                   stdlib_dir, "stream.ml");
   ("Lazy",                     stdlib_dir, "lazy.ml");
   ("CamlinternalLazy",         stdlib_dir, "camlinternalLazy.ml");
   ("Queue",                    stdlib_dir, "queue.ml");
   ("Stack",                    stdlib_dir, "stack.ml");
   ("Map",                      stdlib_dir, "map.ml");
   ("Set",                      stdlib_dir, "set.ml");
   ("Parsing",                  stdlib_dir, "parsing.ml");
   ("Lexing",                   stdlib_dir, "lexing.ml");
   ("Nativeint",                stdlib_dir, "nativeint.ml");
   ("Int64",                    stdlib_dir, "int64.ml");
   ("Int32",                    stdlib_dir, "int32.ml");
   ("Array",                    stdlib_dir, "array.ml");
   ("Obj",                      stdlib_dir, "obj.ml");
   ("Marshal",                  stdlib_dir, "marshal.ml");
   ("Sort",                     stdlib_dir, "sort.ml");
   ("Sys",                      stdlib_dir, "sys.ml");
   ("String",                   stdlib_dir, "string.ml");
   ("Bytes",                    stdlib_dir, "bytes.ml");
   ("Char",                     stdlib_dir, "char.ml");
   ("List",                     stdlib_dir, "list.ml");
   ("",                         stdlib_dir, "pervasives.ml"); (* The special empty strng *)
   ("Pervasives",               stdlib_dir, "pervasives.ml");
   ("CamlinternalFormatBasics", stdlib_dir, "camlinternalFormatBasics.ml")]

let loadlib () =
  List.fold_right
    (fun (n, lib, filename) libs ->
      load_module n libs (Filename.concat lib filename) @ libs)
    ((if !otherlibs <> "" then otherlib_modules () else []) @ stdlib_modules ())
    []

let load_library () =
  let t = !Ocamliutil.typecheck in
    Ocamliutil.typecheck := false;
    if !load_stdlib then Eval.lib := loadlib ();
    Ocamliutil.typecheck := t

let print_binding (pat, e) =
  Printf.printf "%s = %s\n" (to_string_pat pat) (Pptinyocaml.to_string e)

let showlib () =
  if !debug then
    List.iter
      (fun envitem ->
        match envitem with
          EnvBinding (recflag, bindings) ->
            print_string (if recflag then "let rec:\n" else "let:\n");
            List.iter print_binding !bindings
        | EnvFunctor (n, i, a, b, c) ->
            print_string "EnvFunctor: "; print_string n; print_string "\n")
      !Eval.lib
