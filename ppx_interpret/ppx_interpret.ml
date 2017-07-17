open Ast_helper
open Ast_mapper
open Parsetree
open Longident

let interpret_mapper argv =
  {default_mapper with
     structure =
       let rec f mapper structure =
         match structure with
           [] -> []
         | h::t ->
             default_mapper.structure_item mapper h::f mapper t
       in
         f}

let _ = register "interpret" interpret_mapper

