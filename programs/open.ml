(* Works *)

(*open Sys

let x () = argv

let y = x ()*)



(* Does not work *)
open Sys

let x = argv

let y = x
