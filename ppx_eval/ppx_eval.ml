open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let extract_expression [{pstr_desc = Pstr_eval (e, _)}] = e

let compiletime_mapper argv =
  {default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* [%compiletime] *)
      | {pexp_desc =
           Pexp_extension
             ({ txt = "compiletime"; loc},
              PStr [{pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])} ->
          Eval.eval_string_to_ast s
      (* [%runtime] *)
      | {pexp_desc =
           Pexp_extension
             ({ txt = "runtime"; loc},
              PStr [{pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])} ->
                  extract_expression
                    (Evalutils.ast ("Tinyocaml.to_ocaml_value (Eval.eval_string " ^ Printf.sprintf "%S" s ^ ")"))
      | x -> default_mapper.expr mapper x;
  }

let () = register "compiletime" compiletime_mapper

