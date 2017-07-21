open Ast_helper
open Ast_mapper
open Parsetree
open Asttypes
open Longident

(* We remove the [%interpret] structure item, then process all the others via Tinyocaml. *)

let _ = Ocamliutil.typecheck := false

(* To process via Tinyocaml, separate out "external" declarations, and keep
 * these. All others should survive roundtripping with Tinyocaml. *)
let make_shims structitems = structitems

(* The preamble, as an OCaml parse tree *)
let preamble =
 Ocamliutil.ast
  {|let _ =
      Pptinyocaml.simple := true;
      Ocamliutil.typecheck := false
  
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
      @ Tinyocamlrw.to_real_ocaml (make_shims tinyocaml_repr)

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

