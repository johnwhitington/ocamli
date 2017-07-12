open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let _ =
 Ocamliprim.emulated := true;
 Ocamlilib.load_library ()

let extract_expression = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e
| _ -> failwith "ppx_eval: extract_expression"

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
          Runeval.eval_string_to_ast s
      (* [%runtime] *)
      | {pexp_desc =
           Pexp_extension
             ({ txt = "runtime"; loc},
              PStr [{pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _)}])} ->
                  extract_expression
                    (Ocamliutil.ast ("Tinyocaml.to_ocaml_value (Eval.eval_string " ^ Printf.sprintf "%S" s ^ ")"))
      | x -> default_mapper.expr mapper x;
  }

let () = register "compiletime" compiletime_mapper

