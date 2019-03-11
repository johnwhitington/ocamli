open Tast_mapper
open Asttypes

(* We pull the eval_full function out when we find it, at the top of the file,
 * so that we have access to it to make up the fragment of typed tree which
 * applies it to the program. *)
let eval_full_function : Typedtree.expression option ref = ref None

let newmapper argv =
  {default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = [{attr_name = {txt = "interpret"}; attr_payload =  payload}];
          exp_type;
          exp_desc} ->
            print_endline "Found a [@interpret] annotation";
            (* We build a new expression to replace this one, with the same
             * type, and remove the attribute. *)
            (* 1. Make tinyocaml expression as a value. *)
            let tinyocaml_expression =
              Read.finaltype_of_expression [] expr 
            in
            (* 2. Make the typed-tree representation of the expression which
             * calls eval_full on the tinyocaml representation, returning the
             * value which is the result of the evaluation. i.e "eval_full
             * <expr" *)
            let exp_desc =
              exp_desc
            in
            (* 3. This new little typed-tree fragment is spliced in to the node
             * which was annotated, hopefully keeping all the invariants needed
             * to allow the OCaml compiler to complete its job correctly and
             * produce an executable. *)
            {expr with exp_desc = exp_desc}
       | other -> 
           default.expr mapper other);
     structure_item = (fun mapper sitem ->
        (* We are trying to find "let eval_full = Eval.eval_full". When we find
         * it, we need to extract the function, making sure it has the correct
         * type with it. *)
        default.structure_item mapper sitem)
  }

let () =
  Tast_mapper.register "tppx_interpret" newmapper

