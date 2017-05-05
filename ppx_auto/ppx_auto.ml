open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* Parse {|external word_size : unit -> int = "%word_size"|} returning:
  
  a) n = "word_size"
  b) t_in = "Unit"
  c) t_out = "Int"
  d) nstr = "%word_size" *)
let parse_external e =
  match
    Parse.implementation (Lexing.from_string e)
  with
    [structure_item] ->
      (structure_item, "word_size", "Unit", "Int", "%word_size")
  | _ -> failwith "parse_external"

(* Convert type name e.g unit to Tinyocaml constructor name e.g Unit *)
let tinyocaml_constructor_of_type = 0

(* Build the auto itself *)
let build_auto t_in t_out n nstr =
  Ast_convenience.int 0

(* Two structure items. First, the external, then the let percent_word_size = *)
let build_all str =
  Ast_convenience.int 0

let auto_mapper argv =
  {default_mapper with
   structure_item =
     fun mapper structitem ->
       match structitem with
         {pstr_desc = Pstr_extension (({txt = "auto"},
                        PStr [{pstr_desc =
                          Pstr_eval ({pexp_desc =
                            Pexp_constant (Pconst_string (s, _))}, _)}]), _)} ->
             let parsed, _, _, _, _ = parse_external s in
               parsed
     | x ->
         default_mapper.structure_item mapper structitem}

let _ = register "auto" auto_mapper

