let typedtree_of_file fn =
  let ic = open_in_bin fn in
  let tstr = (input_value ic : Typedtree.structure) in
    close_in ic;
    tstr

let typedtree_to_file fn tstr =
  let oc = open_out_bin fn in
  output_value oc tstr;
  close_out oc

open Tast_mapper
open Asttypes

let process tstr =
  let newmapper =
    {default with
       expr = (fun mapper expr ->
         match expr with
         | {exp_attributes = [({txt = "showtype"}, payload)];
            exp_type} as other ->
              begin
                match payload with
                | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _)) }, _)}] ->
                    Format.print_string (s ^ " : ")
                | _ -> ()
              end;
              Printtyp.type_expr Format.std_formatter exp_type;
              Format.print_newline ();
              default.expr mapper other
         | other -> default.expr mapper other);
      structure_item = (fun mapper stritem ->
        match stritem with
        | {str_desc = Tstr_value (_, [{vb_pat = {pat_desc = Tpat_var (ident, _)}; vb_expr}])} ->
            Format.print_string (Ident.name ident ^ " : ");
            Printtyp.type_expr Format.std_formatter vb_expr.exp_type;
            Format.print_newline ();
            default.structure_item mapper stritem
        | other -> default.structure_item mapper other);
      pat = (fun mapper pat ->
        match pat with
        | {pat_attributes = [({txt = "showtype"}, payload)];
            pat_type;
            pat_desc = Tpat_var (ident, _)} as other ->
              Format.print_string (Ident.name ident ^ " : ");
              Printtyp.type_expr Format.std_formatter pat_type;
              Format.print_newline ();
              default.pat mapper other
         | other -> default.pat mapper other)
    }
  in
    newmapper.structure newmapper tstr

let _ =
  match Sys.argv with
    [|_; infile; outfile|] ->
      typedtree_to_file outfile (process (typedtree_of_file infile))
  | _ -> prerr_endline "Usage tppx_interpret <infile> <outfile>"

