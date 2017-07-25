open Ast_helper
open Ast_mapper
open Parsetree
open Asttypes
open Longident
open Tinyocaml

(* We remove the [%interpret] structure item, then process all the others via Tinyocaml. *)

let _ = Ocamliutil.typecheck := false

(* To process via Tinyocaml, separate out "external" declarations, and keep
 * these. All others should survive roundtripping with Tinyocaml. *)

(* For each call to an external resource e.g A.double, build a shim for calling
 * it. FIXME Extend to values, use the env to resolve scoping issues etc. *)

(* This creates a_dot_double_builtin *)
let make_external_call_shim text =
  (* FIXME: Need to make the tinyocaml of this, so make_external_call_shims below can
   * use it in Tinyocaml.recurse *)
  {|let a_dot_double_builtin =
    let f env = function
      | [x] ->
          let heap_x = Tinyexternal.to_ocaml_value x in
          let result = |} ^ text ^ {| heap_x in
            Tinyexternal.of_ocaml_value env result "int"
      | _ -> failwith "a_dot_double_builtin: arity"
    in
      mk |} ^ text ^ {| f|}

(* Find every use of A.double and replace with a_dot_double_builtin. FIXME generalize *)
let make_external_call_shims (body : Tinyocaml.t) =
  let a_dot_double_builtin = make_external_call_shim "A.double" in
    let rec replace_vars varname f =
      (function 
        Var v when v = varname -> f
       | x -> Tinyocaml.recurse (replace_vars varname f) x) 
    in
      replace_vars "A.double" a_dot_double_builtin

(* Given a Tinyocaml.t representing a structure item let x = ..., build the main shim, and any bits required to call it *)
let make_shim = function
  | LetDef (false, [(PatVar fun_name, Fun (NoLabel, PatVar var_name, body, _))]) ->
      let code = Pptinyocaml.to_string (make_external_call_shims body) in
      let in_type = "int" in (* FIXME *)
      let out_type = "int" in (* FIXME *)
      let code_str =
        {|let |} ^ fun_name ^ " " ^ var_name ^ {| =
          let open Tinyocaml in
          let tiny_|} ^ var_name ^ {| = Tinyexternal.of_ocaml_value [] |} ^ var_name ^ " " ^ "{|" ^ in_type ^ "|}" ^ {| in
          let _, program = Tinyocamlrw.of_string |} ^ "{|" ^ code ^ "|}" ^ {| in
          let env = [EnvBinding (false, ref [(PatVar |} ^ "\"" ^ var_name ^ "\"" ^ {|, tiny_|} ^ var_name ^ {|)])] in
          let tiny_result = Eval.eval_until_value true false env program in
          (Tinyexternal.to_ocaml_value tiny_result : |} ^ out_type ^ ")"   (* FIXME *)
      in
      Printf.eprintf "make_shim: found a normal function like trip...\n";
      Printf.eprintf "Code is: %s\n" code_str;
      Ocamliutil.ast code_str
  | _ -> Printf.eprintf "make_shim: unknown structure item, ignoring...\n"; Ocamliutil.ast "()"

let make_shims = function
  Struct (_, structitems) ->
    List.map make_shim structitems
| _ -> failwith "make_shims: not a struct"

(* The preamble, as an OCaml parse tree *)
let preamble =
 Ocamliutil.ast
  {|let _ =
      Pptinyocaml.simple := true;
      Ocamliutil.typecheck := false
  
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
    Ocamliutil.ast str

let process structure =
  let externals, nonexternals =
    List.partition
      (function {pstr_desc = Pstr_primitive _} -> true | _ -> false)
      structure
  in
    let tinyocaml_repr = snd (Tinyocamlrw.of_real_ocaml [] nonexternals) in
        preamble
      @ List.flatten (List.map process_external externals)
      @ List.flatten (make_shims tinyocaml_repr)

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

