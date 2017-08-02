open Ast_helper
open Ast_mapper
open Parsetree
open Asttypes
open Longident
open Tinyocaml
open Ocamliutil

(* We remove the [%interpret] structure item, then process all the others via Tinyocaml. *)

let _ =
  Ocamliutil.typecheck := false

(* To process via Tinyocaml, separate out "external" declarations, and keep
 * these. All others should survive roundtripping with Tinyocaml. *)

(* For each call to an external resource e.g A.double, build a shim for calling
 * it. FIXME Extend to values, use the env to resolve scoping issues etc. *)

(* This creates an ocaml function a_dot_double_builtin to appear in the output.
 * A CallBuiltIn for it will then be put in the environment for the Tinyocaml
 * program which calls A.double when it is interpreted. *)
let make_external_call_shim_ocaml_part name =
  let modname, leafname =
    Filename.remove_extension name,
    implode (List.tl (explode (Filename.extension name)))
  in
  let code =
    {|module |} ^ modname ^ {| =
        struct
          let |} ^ leafname ^ {| env = function
            | [x] ->
                let heap_x = Tinyexternal.to_ocaml_value x in
                let result = |} ^ name ^ {| heap_x in
                  Tinyexternal.of_ocaml_value env result "int"
            | _ -> failwith "|} ^ name ^ {|: arity"
        end|}
  in let env_binding =
    {|EnvBinding (false, ref [(PatVar "|} ^
    name ^
    {|", mk "|} ^
    name ^
    {|" |} ^
    name ^
    {|)]);|}
  in
    (code, env_binding)

(* FIXME: scoping *)
let find_dotted_names e =
  let names = ref [] in
    Tinyocaml.iter
    (function Var v ->
      if List.mem '.' (explode v) && v <> "Callback.register" then names := v::!names
     | _ -> ())
    e;
    setify !names

let make_external_call_shims (body : Tinyocaml.t) =
  let dotted_names = find_dotted_names body in
    List.map make_external_call_shim_ocaml_part dotted_names

(* Given a Tinyocaml.t representing a structure item let x = ..., build the main shim, and any bits required to call it *)
let make_shim external_envbindings = function
  | LetDef (_, [(PatVar fun_name, Fun (_, PatVar var_name, body, _))]) ->      
      let saved = !Pptinyocaml.syntax in
      let _ = Pptinyocaml.syntax := false in
      let ocaml_part, env_binding_strings = List.split (make_external_call_shims body) in
      let body_str = Pptinyocaml.to_string body in
      let _ = Pptinyocaml.syntax := saved in
      let in_type = "int" in (* FIXME *)
      let out_type = "int" in (* FIXME *)
      let new_body =
        "let rec " ^ fun_name ^ " " ^ var_name ^ " = " ^ body_str ^ " in " ^ fun_name ^ " " ^ var_name
      in
      let ocaml_modules =
        List.fold_left ( ^ ) "" (List.map (fun s -> " let " ^ s ^ " in ") ocaml_part)
      in
      let code_str =
        {|let |} ^ fun_name ^ " " ^ var_name ^ {| = |} ^ ocaml_modules ^ {|
          let open Tinyocaml in
          let tiny_|} ^ var_name ^ {| = Tinyexternal.of_ocaml_value [] |} ^ var_name ^ " " ^ "{|" ^ in_type ^ "|}" ^ {| in
          let _, program = Tinyocamlrw.of_string |} ^ "{|" ^ new_body ^ "|}" ^ {| in
          let env =
            [EnvBinding (false, ref [(PatVar |} ^ "\"" ^ var_name ^ "\"" ^ {|, tiny_|} ^ var_name ^ {|)]);
            ]
           |}
            ^ "@ [" ^ List.fold_left ( ^ ) "" env_binding_strings ^ "]"
            ^ "@ [" ^ List.fold_left ( ^ ) "" external_envbindings ^ "]" ^ 
           {|
          in
          let tiny_result = Eval.eval_until_value true false (env @ !Eval.lib) program in
          (Tinyexternal.to_ocaml_value tiny_result : |} ^ out_type ^ ")"   (* FIXME *)
      in
        [Ocamliutil.ast code_str]
  | x ->
      Printf.eprintf "Failed to make shim for %s\n" (Tinyocaml.to_string x);
      []


let make_shims external_envbindings = function
  Struct (_, structitems) ->
    List.flatten (List.map (make_shim external_envbindings) structitems)
| _ -> failwith "make_shims: not a struct"

(* The preamble, as an OCaml parse tree *)
let preamble =
 Ocamliutil.ast
  {|let _ =
      Pptinyocaml.simple := true;
      Ocamliutil.typecheck := false;
      Ocamlilib.load_stdlib := true;
      Ocamlilib.load_library ()
  
    let mk name f =
      let open Tinyocaml in
        Fun (NoLabel, PatVar "*x", CallBuiltIn (None, name, [Var "*x"], f), [])

    let exception_from_ocaml e = Tinyocaml.Unit|}

(* For each external, we keep it, and also create a %%auto instance for it. *)
let process_external e =
  let name =
    match e with
      {pstr_desc = Pstr_primitive {pval_name = {txt}}} -> txt
   | _ -> assert false
  in
  let auto = "[%%auto {|" ^ Pprintast.string_of_structure [e] ^ "|}]" in
  let fix = "let " ^ name ^ "_builtin  = snd " ^ name in
  let str = auto ^ " " ^ fix in
    (Ocamliutil.ast str, 
     "EnvBinding (false, ref [(PatVar \"" ^ name ^ "\", " ^ name ^ "_builtin)]);")

let process structure =
  let externals, nonexternals =
    List.partition
      (function {pstr_desc = Pstr_primitive _} -> true | _ -> false)
      structure
  in
    let processed_externals, external_envbindings = List.split (List.map process_external externals) in
    let tinyocaml_repr = snd (Tinyocamlrw.of_real_ocaml [] nonexternals) in
        preamble
      @ List.flatten processed_externals
      @ List.flatten (make_shims external_envbindings tinyocaml_repr)

let interpret_mapper argv =
  {default_mapper with
     structure =
       fun mapper structure ->
         (* If first is %interpret, process. Otherwise, use default mapper *)
         match structure with
           {pstr_desc = Pstr_eval ({pexp_desc = Pexp_extension ({txt = "interpret"}, _)}, _)}::t ->
             process t
         | l ->
             List.map (default_mapper.structure_item mapper) l}

let _ = register "interpret" interpret_mapper

