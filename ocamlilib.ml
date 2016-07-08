(* ocamlilib *)
(* Load a module from disk *)
open Ocamliutil
open Tinyocaml

let debug = ref false

let add_prefix x (y, v) =
  (x ^ "." ^ y, v)

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
    List.flatten
      (List.map
        (fun x ->
          match x with
            LetDef (_, bindings) -> Tinyocamlutil.read_bindings bindings
          | _ -> []) 
        items)
| _ -> failwith "definitions_of_module"

let load_module name env file =
  if !debug then Printf.printf "Loading module %s...%!" name;
  let themod = Tinyocamlrw.of_real_ocaml (ast (load_file file)) in
    let themod' = Eval.eval false env themod in
      if !debug then Printf.printf "done\n%!";
      List.rev (List.map (add_prefix name) (definitions_of_module themod'))

let stdlib_modules =
  [(*("Unix", "./stdlib", "unix.ml");*)
   ("Sys", stdlib_dir, "sys.ml"); 
   ("Callback", stdlib_dir, "callback.ml");
   ("Obj", stdlib_dir, "obj.ml");
   ("Array", stdlib_dir, "array.ml");
   ("List", stdlib_dir, "list.ml");
   ("Pervasives", stdlib_dir, "pervasives.ml");
   ("CamlinternalFormatBasics", stdlib_dir, "camlinternalFormatBasics.ml")]

let loadlib () =
  List.fold_right
    (fun (n, lib, filename) libs ->
      load_module n libs (Filename.concat lib filename) @ libs)
    stdlib_modules
    []

  (*let _ =
    List.iter
      (fun (n, v) -> Printf.printf "%s = %s\n" n (Pptinyocaml.to_string v)) !lib
  in*)

let _ = Eval.lib := loadlib ()

