open Tast_mapper
open Asttypes
open Typedtree

(* We pull the eval_full function out when we find it, at the top of the file,
 * so that we have access to it to make up the fragment of typed tree which
 * applies it to the program. *)
let eval_full_function : expression option ref = ref None
let template_string : expression option ref = ref None
let last_global_type : Types.type_expr option ref = ref None

let newmapper argv =
  {default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = [{attr_name = {txt = "interpret"}; attr_payload =  payload}];
          exp_type;
          exp_desc} ->
            print_endline "Found a [@interpret] annotation\n";
            (* Make tinyocaml expression as a typed tree fragment *)
            let tinyocaml_expression =
              match !template_string with None -> failwith "no template string" | Some x ->
                {x with exp_desc =
                   Texp_constant
                     (Const_string
                        (Marshal.to_string ((Read.finaltype_of_expression [] expr)) [], None))}
            in
            (* Make the typed-tree representation of the expression which
             * calls eval_full on the tinyocaml representation, returning the
             * value which is the result of the evaluation. i.e "eval_full
             * <expr>" *)
            let exp_desc' =
              let eval_full =
                match !eval_full_function with None -> failwith "no eval_full found" | Some x -> x
              in
                Texp_apply (eval_full, [(Nolabel,  Some tinyocaml_expression)])
            in
            (* Splice into typed tree *)
            {expr with exp_desc = exp_desc'}

       (* Finding let y = ... in addenv "y" (Obj.magic y : Obj.t) ""; <expr'> *)
         | {exp_desc =
             Texp_let (recflag, [binding], ({exp_desc = Texp_sequence (whole_addenv, expr')} as sequence))} as other ->
               begin match whole_addenv with
                 {exp_desc =
                   Texp_apply ({exp_desc = Texp_ident (path, _, _)} as addenv, [a; ((_, Some {exp_desc = Texp_constant (Const_string (name, _))}) as b); c; (arg_label, Some typ)])}
                 when Path.name path = "Tppxsupport.addenv" ->
                   Printf.printf "****Found an addenv instance! for %s\n" name;
                   let typ' =
                     match !template_string with None -> failwith "no template string 2" | Some s ->
                       {s with exp_desc =
                         Texp_constant (Const_string (Marshal.to_string other.exp_type.desc [], None))}
                   in
                     {other with exp_desc =
                       Texp_let (recflag, [binding],
                                 {sequence with exp_desc =
                                   Texp_sequence ({whole_addenv with exp_desc =
                                     Texp_apply (addenv, [a; b; c; (arg_label, Some typ')])}, default.expr mapper expr')})}
               |  _ ->
                    default.expr mapper other
               end
       | other -> 
           default.expr mapper other);
     structure_item = (fun mapper sitem ->
        (* Extract pre-made things provided by the untyped PPX. *)
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
       (* Finding globals: let () = addenv "y" (Obj.magic y : Obj.t) "" *)
       | {str_desc =
            Tstr_value (vlhs,
              [{vb_pat = {pat_desc = Tpat_construct ({txt = Lident "()"}, _, _)};
                vb_expr = {exp_desc = Texp_apply ({exp_desc = Texp_ident (path, _, _)} as addenv, [a; ((_, Some {exp_desc = Texp_constant (Const_string (name, _))}) as b); c; (arg_label, Some typ)])} as vbexpr}
               as vbinding])}
          when Path.name path = "Tppxsupport.addenv"
          ->
              let typ' =
                match !template_string with None -> failwith "no template string 2" | Some s ->
                  match !last_global_type with
                    None -> failwith "no last_global_type"
                  | Some typ ->
                     {s with exp_desc =
                       Texp_constant (Const_string (Marshal.to_string typ [], None))}
              in
              Printf.printf "***found let () = ...\n";
              let vbinding' =
                {vbinding with vb_expr = {vbexpr with exp_desc = Texp_apply (addenv, [a; b; c; (arg_label, Some typ')])}}
              in
                {sitem with
                  str_desc =
                    Tstr_value (vlhs, [vbinding'])}
       (* Finding globals : store the last global type we see, so that when we find its addenv, the type is ready *)
       |  {str_desc =
            Tstr_value (_, [{vb_expr; vb_pat = {pat_desc = Tpat_var (varname, _)}; _}])} ->
              Printf.printf "***storing type for %s\n" (Ident.name varname);
              last_global_type := Some (vb_expr.exp_type);
              default.structure_item mapper sitem
       | _ ->
          default.structure_item mapper sitem)
  }

let () =
  Tast_mapper.register "tppx_interpret" newmapper

