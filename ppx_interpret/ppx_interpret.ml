open Ast_helper
open Ast_mapper
open Parsetree
open Asttypes
open Longident

(* We remove the [%interpret] structure item, then process all the others via Tinyocaml. *)

(* To process via Tinyocaml, seperate out "external" declarations, and keep
 * these. All others should survive roundtripping with Tinyocaml. *)
let process structure =
  Printf.printf "%i items in the structure\n" (List.length structure);
  let externals, nonexternals =
    List.partition
      (function {pstr_desc = Pstr_primitive _} -> true | _ -> false)
      structure
  in
    Printf.printf "%i externals and %i nonexternals\n" (List.length externals) (List.length nonexternals);
    externals @
    Tinyocamlrw.to_real_ocaml (snd (Tinyocamlrw.of_real_ocaml [] nonexternals))

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

