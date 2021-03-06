open Ast_mapper
open Parsetree

let ast ?(filename="") code =
  let lexer = Lexing.from_string code in
  Location.init lexer filename;
  Parse.implementation lexer

let expr_ast code =
  match ast code with
  [{pstr_desc = Pstr_eval (e, _)}] -> e
  | _ -> failwith "expr_ast"

let preamble = ast
{|let env = ref Lib.stdlib
let () = Tppxsupport.init ()
let template_string = ""
let () = Print.showvals := false
let eval_full = Tppxsupport.eval_full_from_typedtree env|}

let printas_text = function None -> "None" | Some x -> "(Some \"" ^ x ^ "!\")" 

let global_addenv printas n =
  ast ("let () = Tppxsupport.addenv env " ^ printas_text printas ^ " \"" ^ n ^ "\" (Obj.magic " ^ n ^ "\ : Obj.t) \"\"")

let local_addenv printas n =
  match ast ("Tppxsupport.addenv env " ^ printas_text printas ^ " \"" ^ n ^ "\" (Obj.magic " ^ n ^ "\ : Obj.t) \"\"") with
   [{pstr_desc = Pstr_eval (e, _)}] -> e
  | _ -> failwith "local_addenv"

let rec add_global_addenvs default_mapper mapper = function
  | {pstr_desc = Pstr_value (_, [{pvb_expr; pvb_pat = {ppat_desc = Ppat_var {txt = n}}}])} as letdef::t ->
      (* Add a printas if it's a function, since we won't be able to print it... *)
      let printas =
        match pvb_expr with
          {pexp_desc = Pexp_function _ | Pexp_fun _} -> Some n
        | _ -> None
      in
         mapper.structure_item mapper letdef
      :: global_addenv printas n
      @  add_global_addenvs default_mapper mapper t
  | h::t ->
         mapper.structure_item mapper h
      :: add_global_addenvs default_mapper mapper t
  | [] -> []

let add_local_addenvs default_mapper mapper lett =
  match lett with
  | {pexp_desc = Pexp_let (recflag, ([{pvb_expr; pvb_pat = {ppat_desc = Ppat_var {txt = n}}}] as bindings), expr)} ->
      let printas =
        match pvb_expr with
          {pexp_desc = Pexp_function _ | Pexp_fun _} -> Some n
        | _ -> None
      in
      let sequence =
        {expr with pexp_desc =
          Pexp_sequence (local_addenv printas n, mapper.expr mapper expr)}
      in
        {lett with pexp_desc = Pexp_let (recflag, bindings, sequence)}
  | e -> default_mapper.expr mapper e

let string_of_pattern p =
  let b = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer b in
  Pprintast.pattern formatter p;
  Format.pp_print_flush formatter ();
  Buffer.contents b

(* Expression mapper for [@showmatch]. 1. recognise the attribute 2. check it's on a function. 3. Modify the cases *)
let add_patmatch_printer default_mapper mapper expr =
  match expr with
  | {pexp_desc = Pexp_function cases;
     pexp_attributes = [{attr_name = {txt ="showmatch"}}]} ->
       Printf.printf "||||| found a showmatch\n";
       let f case = match case with
         {pc_lhs; pc_rhs} ->
           let printit = expr_ast ("print_endline \"{matches " ^ string_of_pattern pc_lhs ^ "}\"") in
             let sequence =
               {pexp_desc = Pexp_sequence (printit, pc_rhs); 
                pexp_loc = expr.pexp_loc;
                pexp_loc_stack = expr.pexp_loc_stack;
                pexp_attributes = []}
             in
               {case with pc_rhs = sequence}
       in
         {expr with pexp_desc = Pexp_function (List.map f cases); pexp_attributes = []}
  | {pexp_desc;
     pexp_attributes = [{attr_name = {txt ="interpret"}; attr_payload}]} as e ->
       print_string "&&&&&&&&&&&&&&&&&&&&&&&&&Plain PPX found the [@interpret] annotation\n";
       e
  | e -> e (* Don't do anything. This needs to fixed properly to allow us to have multiple expr mappers which do not interfere with one another... *)

let interpret_mapper argv =
  {default_mapper with
     structure =
       (fun mapper structure ->
         preamble @ add_global_addenvs default_mapper mapper structure);
     expr =
       (fun mapper expression ->
         add_patmatch_printer default_mapper mapper (add_local_addenvs default_mapper mapper expression))} 

let _ = register "interpret" interpret_mapper

