open Tast_mapper
open Asttypes
open Typedtree

(* We pull the eval_full function out when we find it, at the top of the file,
 * so that we have access to it to make up the fragment of typed tree which
 * applies it to the program. *)
let eval_full_function : expression option ref = ref None
let template_string : expression option ref = ref None
let template_int : expression option ref = ref None

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
            (* 1. Make tinyocaml expression as a typed tree fragment *)
            let tinyocaml_expression =
              match !template_string with
                None -> failwith "no template string"
              | Some x ->
                  {x with exp_desc =
                     Texp_constant
                       (Const_string
                          (Marshal.to_string ((Read.finaltype_of_expression [] expr)) [], None))}
            in
            (* 2. Make the typed-tree representation of the expression which
             * calls eval_full on the tinyocaml representation, returning the
             * value which is the result of the evaluation. i.e "eval_full
             * <expr>" *)
            let exp_desc' =
              let eval_full =
                match !eval_full_function with
                  None -> failwith "no eval_full found"
                | Some x -> x
              in
                Texp_apply (eval_full, [(Nolabel,  Some tinyocaml_expression)])
            in
            (* 3. This new little typed-tree fragment is spliced in to the node
             * which was annotated, hopefully keeping all the invariants needed
             * to allow the OCaml compiler to complete its job correctly and
             * produce an executable. *)
            {expr with exp_desc = exp_desc'}

       (* Finding calls to addenv. Can we rely on Tast_mapper to traverse in
        * the correct order to build our env? May need our own mapper when we have to
        * keep an env... *)
       (* Finding addenv "y" (Obj.magic y : Obj.t) (Types.Tvar (Some "TPPX will fill me in") *)
       | {exp_desc = Texp_apply ({exp_desc = Texp_ident (path, _, _)} as addenv, [a; b; (arg_label, Some typ)])} as other
             when Path.name path = "addenv" ->
           Printf.printf "****Found an addenv instance!";
             let typ' =
               match !template_int with
                 Some x ->
                   begin match !template_string with
                     None -> failwith "no template string 2"
                   | Some s ->
                       {s with exp_desc =
                         Texp_constant
                           (Const_string
                              (Marshal.to_string x.exp_type.desc [], None))}
                   end
               | None -> failwith "template_int not found"
             in
             {other with exp_desc =
               Texp_apply (addenv, [a; b; (arg_label, Some typ')])}
       | other -> 
           default.expr mapper other);
     structure_item = (fun mapper sitem ->
        (* We are trying to find "let eval_full = Eval.eval_full". When we find
         * it, we need to extract the function, making sure it has the correct
         * type with it. *)
        match sitem with
          {str_desc =
            Tstr_value (_, [{vb_pat = {pat_desc = Tpat_var (varname, _)}; vb_expr}])}
              when
            Ident.name varname = "eval_full" ->
              eval_full_function := Some vb_expr;
              default.structure_item mapper sitem
       |  {str_desc =
            Tstr_value (_, [{vb_pat = {pat_desc = Tpat_var (varname, _)}; vb_expr}])}
              when
            Ident.name varname = "template_string" ->
              template_string := Some vb_expr;
              default.structure_item mapper sitem
       |  {str_desc =
            Tstr_value (_, [{vb_pat = {pat_desc = Tpat_var (varname, _)}; vb_expr}])}
              when
            Ident.name varname = "template_int" ->
              template_int := Some vb_expr;
              default.structure_item mapper sitem
       | _ ->
          default.structure_item mapper sitem)
  }

let () =
  Tast_mapper.register "tppx_interpret" newmapper

