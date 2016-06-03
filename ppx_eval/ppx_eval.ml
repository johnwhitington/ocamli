open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let compiletime_mapper argv =
  {default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | {pexp_desc = Pexp_extension ({ txt = "compiletime"; loc }, PStr structure)} ->
          Eval.eval_ast structure
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "compiletime" compiletime_mapper

