open Ast_mapper
open Parsetree

let ast ?(filename="") code =
  let lexer = Lexing.from_string code in
  Location.init lexer filename;
  Parse.implementation lexer

let preamble = ast
{|let env = ref Lib.stdlib
let template_string = ""
let () = Print.showvals := false
let eval_full = Tppxsupport.eval_full env|}

let global_addenv n =
  ast ("let () = Tppxsupport.addenv env \"" ^ n ^ "\" (Obj.magic " ^ n ^ "\ : Obj.t) \"\"")

let local_addenv n =
  match ast ("Tppxsupport.addenv env \"" ^ n ^ "\" (Obj.magic " ^ n ^ "\ : Obj.t) \"\"") with
   [{pstr_desc = Pstr_eval (e, _)}] -> e
  | _ -> failwith "local_addenv"

let rec add_global_addenvs default_mapper mapper = function
  | {pstr_desc = Pstr_value (_, [{pvb_pat = {ppat_desc = Ppat_var {txt = n}}}])} as letdef::t ->
         mapper.structure_item mapper letdef
      :: global_addenv n
      @  add_global_addenvs default_mapper mapper t
  | h::t ->
         mapper.structure_item mapper h
      :: add_global_addenvs default_mapper mapper t
  | [] -> []

let add_local_addenvs default_mapper mapper lett =
  match lett with
  | {pexp_desc = Pexp_let (recflag, ([{pvb_pat = {ppat_desc = Ppat_var {txt = n}}}] as bindings), expr)} ->
      let sequence =
        {expr with pexp_desc =
          Pexp_sequence (local_addenv n, mapper.expr mapper expr)}
      in
        {lett with pexp_desc = Pexp_let (recflag, bindings, sequence)}
  | e -> default_mapper.expr mapper e

let interpret_mapper argv =
  {default_mapper with
     structure =
       (fun mapper structure ->
         preamble @ add_global_addenvs default_mapper mapper structure);
     expr =
       (fun mapper expression ->
         add_local_addenvs default_mapper mapper expression)} 

let _ = register "interpret" interpret_mapper

