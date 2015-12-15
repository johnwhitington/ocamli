open Parsetree
open Asttypes

type t = Parsetree.expression

let init = function
  [{pstr_desc = Pstr_eval (e, _)}]
| [{pstr_desc =
     Pstr_value
       (Nonrecursive, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])}] -> e
| _ -> failwith "Not a single structure item"

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

exception ExnUnimplemented of string
exception ExnMalformed of string

let unimp s = raise (ExnUnimplemented s)
let malformed s = raise (ExnMalformed s)

let bool_of_bool_value e =
  match e.pexp_desc with
  | Pexp_construct
      ({txt = Longident.Lident (("true" | "false") as b)}, None) ->
        bool_of_string b
  | _ -> malformed __LOC__

let mkbool b =
  with_desc
    (Pexp_construct
      ({txt = Longident.Lident (string_of_bool b); loc = Location.none}, None))

let comparison_of_string = function
  | "=" -> ( = )
  | "<>" -> ( <> )
  | "<" -> ( < )
  | ">" -> ( > )
  | "<=" -> ( <= )
  | ">=" -> ( >= )
  | _ -> malformed __LOC__

let calculate a b = function
    "*" -> a * b
  | "+" -> a + b
  | "-" -> a - b
  | "/" -> a / b
  | _ -> unimp __LOC__

let polymorphic_comparison a b op =
  match a.pexp_desc, b.pexp_desc with
    Pexp_constant (Const_int ai), Pexp_constant (Const_int bi) ->
      (comparison_of_string op) ai bi
  | _ -> unimp __LOC__

(* Evaluate an abstract syntax tree by one step *)
let rec eval_expression e = 
  if is_value e then e else
    begin match e.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "&&"}} as op, args) ->
        (* The short-circuit boolean operator && *)
        (* &&: If first is a value, and is true, return second.
               If first is a value, and is false, return false.
               If first not a value, eval one step *)
        begin match args with
        | [(l1, a1); (l2, a2)] ->
            if is_value a1 then 
              if bool_of_bool_value a1 then a2 else mkbool false
            else
              {e with pexp_desc = Pexp_apply (op, evaluate_first_args args)}
        | _ -> malformed __LOC__
        end
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "||"}} as op, args) ->
        (* The short-circuit boolean operator || *)
        (* ||: If first is a value, and is false, return second.
               If first is a value, and is true, return true.
               If first is not a value, eval one step *)
        begin match args with
        | [(l1, a1); (l2, a2)] ->
            if is_value a1 then
              if bool_of_bool_value a1 then mkbool true else a2
            else
              {e with pexp_desc = Pexp_apply (op, evaluate_first_args args)}
        | _ -> malformed __LOC__
        end
    | Pexp_apply (expr, args) ->
        (* If args all values, proceed to apply *)
        if
          List.for_all (fun (_, arg) -> is_value arg) args
        then
          begin match expr.pexp_desc with
            Pexp_ident
              {txt =
                Longident.Lident (("=" | "<>" | "<" | ">" | "<=" | ">=") as op)} ->
                begin match args with
                  [(_, a); (_, b)] when is_value a && is_value b ->
                     mkbool (polymorphic_comparison a b op)
                | _ -> malformed __LOC__
                end
          | Pexp_ident {txt = Longident.Lident (("*" | "/" | "+" | "-") as op)} ->
              begin match args with
                [(_, {pexp_desc = Pexp_constant (Const_int a)});
                 (_, {pexp_desc = Pexp_constant (Const_int b)})] ->
                  let result = calculate a b op in
                    {e with pexp_desc = Pexp_constant (Const_int result)}
              | _ -> malformed __LOC__
              end
          | Pexp_ident {txt = Longident.Lident t} ->
              unimp (__LOC__ ^ ": " ^ t)
          | Pexp_fun (arg_label, opt, {ppat_desc = Ppat_var {txt}}, body) ->
              begin match args with
              | [] -> malformed __LOC__
              | (label, toapply)::more ->
                    if more = [] then
                      substitute_expression txt toapply body
                    else
                      {e with
                         pexp_desc =
                           Pexp_apply
                             (substitute_expression txt toapply body, more)}
              end
          | _ ->
              (* If it's not something we can apply, evaluate it one step. *)
              {e with pexp_desc = Pexp_apply (eval_expression expr, args)}
          end
        else
          (* Work on the arguments *)
          {e with pexp_desc = Pexp_apply (expr, evaluate_first_args args)}
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        (* If e1 not a value already, evaluate it one step and return.
        Otherwise, pick e2 or e3, and replace the ifthenelse with it *)
        if is_value e1 then
          if bool_of_bool_value e1 then e2 else e3
        else
          {e with pexp_desc = Pexp_ifthenelse (eval_expression e1, e2, Some e3)}
    | Pexp_let (rec_flag, value_bindings, let_exp) ->
        eval_let_expr e rec_flag let_exp value_bindings
    | x -> unimp (__LOC__ ^ "\n" (*^ string_of_expression_desc x*))
    end

and eval_single_let value_bindings e rec_flag binding let_exp =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var {txt} ->
      substitute_expression
        txt
        (if rec_flag = Recursive
           then
             {e with pexp_desc =
               Pexp_let (rec_flag, value_bindings, binding.pvb_expr)}
           else
             binding.pvb_expr)
        let_exp
  | Ppat_tuple vars ->
      begin match binding with
      | {pvb_expr = {pexp_desc = Pexp_tuple vals}} 
           when List.length vals = List.length vars ->
              List.fold_left
                (fun let_exp (var, value) ->
                   substitute_expression
                     (match var with {ppat_desc = Ppat_var {txt}} -> txt | _ -> unimp __LOC__)
                     (if rec_flag = Recursive then unimp __LOC__ else value)
                     let_exp)
                let_exp
                (List.combine vars vals)
      | _ -> malformed __LOC__
      end
 | _ -> unimp __LOC__

and eval_let_expr e rec_flag let_exp = function
  | [] -> malformed __LOC__
  | [{pvb_expr} as binding] as value_bindings ->
      if is_value pvb_expr then
        eval_single_let value_bindings e rec_flag binding let_exp
      else
        {e with
          pexp_desc =
            Pexp_let
              (rec_flag,
               [{binding with pvb_expr = eval_expression pvb_expr}],
               let_exp)}
  | value_bindings ->
    (*Printast.expression 0 Format.std_formatter e; Format.print_newline ();*)
    List.iter
      (fun vb ->
        Format.print_string "BINDING:";
        Format.print_newline ();
        Pprintast.expression Format.std_formatter vb.pvb_expr;
        Format.print_newline ())
      value_bindings;
    unimp (Printf.sprintf "type of LET with %i bindings\n"
    (List.length value_bindings) ^ __LOC__)

and evaluate_first = function
  [] -> malformed __LOC__ (* Already a value *)
| l -> really_evaluate_first l

and really_evaluate_first = function
  [] -> []
| h::t ->
  if is_value h
    then h::really_evaluate_first t
    else eval_expression h::t

and evaluate_first_args args =
  let l, a = List.split args in
    List.combine l (evaluate_first a)

let to_string ast =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter ast;
    Format.pp_print_flush formatter ();
    Buffer.contents b

let next ast =
  if is_value ast then IsValue else
    try Next (eval_expression ast) with
      ExnUnimplemented s -> Unimplemented s
    | ExnMalformed s -> Malformed s

let repr ast = to_string ast

