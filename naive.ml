open Parsetree
open Asttypes

type t = Parsetree.structure

let init ast = ast

type result =
    Next of t
  | IsValue
  | Malformed of string
  | Unimplemented of string

(* Naive evaluation by substitution *)
let rec is_value e =
  match e.pexp_desc with
    Pexp_constant (Const_int _)
  | Pexp_construct ({txt = Longident.Lident ("true" | "false")}, None)
  | Pexp_fun _ -> true
  | Pexp_function _ -> true
  | _ -> false

let appears_in_value_bindings var value_bindings =
  List.exists
    (function
      {pvb_pat = {ppat_desc = Ppat_var {txt}}} when txt = var -> true
      | _ -> false)
    value_bindings

let rec substitute_expression_desc var value = function
  | Pexp_ident {txt} when txt = Longident.Lident var ->
      value.pexp_desc
  | Pexp_apply (expr, args) ->
      Pexp_apply
        (substitute_expression var value expr,
         List.map
           (function (arg_label, expr) ->
             (arg_label, substitute_expression var value expr))
           args)
  | Pexp_let (rec_flag, value_bindings, e) ->
      let e' =
        if appears_in_value_bindings var value_bindings
          then e
          else substitute_expression var value e
      in
        let value_bindings' =
          (* If recursive, don't binding names shadow in binding *)
          List.map
            (fun binding ->
               if rec_flag = Recursive && appears_in_value_bindings var [binding]
                 then binding
                 else substitute_value_binding var value binding)
            value_bindings
        in
          Pexp_let (rec_flag, value_bindings', e')
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      let r = substitute_expression var value in
        Pexp_ifthenelse (r e1, r e2, Some (r e3))
  | x -> x

and substitute_value_binding var value value_binding =
  {value_binding with
     pvb_expr = substitute_expression var value value_binding.pvb_expr}

and substitute_expression var value exp =
  {exp with
    pexp_desc = substitute_expression_desc var value exp.pexp_desc}

let with_desc x =
  {pexp_desc = x;
   pexp_loc = Location.none;
   pexp_attributes = []}

let string_of_expression exp =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter exp;
    Format.pp_print_flush formatter ();
    Buffer.contents b

let string_of_expression_desc pexp_desc =
  string_of_expression (with_desc pexp_desc)

let next ast = IsValue

let repr ast = string_of_expression ast

