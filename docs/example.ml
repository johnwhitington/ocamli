let f : string -> int -> int = [%withx]

(* Convert automatically into: *)
let f expr typ =
  function x ->
    Tinyocaml.to_ocaml_value
      (Eval.eval_string
         ("let x = " ^ Pptinyocaml.to_string (Tinyocaml.of_ocaml_value x typ) ^ " in " ^ expr));;

let a : int list = f "List.map (fun n -> n + 1) x" "int list" [1; 2; 3]

let a : int list * int list = f "List.split x" "(int * int) list" [(1, 2); (3, 4)];;

let rec sum = function
  Tinyocaml.UInt x -> x
| Tinyocaml.UBlock (_, vars) -> Array.fold_left ( + ) 0 (Array.map sum vars)

let sum x =
  sum (Tinyocaml.untyped_of_ocaml_value x)

