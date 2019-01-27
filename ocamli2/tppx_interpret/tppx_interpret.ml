open Tast_mapper
open Asttypes

let newmapper argv = default
  (*{default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = [{attr_name = {txt = "showtype"}; attr_payload =  payload}];
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
      | {pat_attributes = [{attr_name = {txt = "showtype"}; attr_payload = payload}];
          pat_type;
          pat_desc = Tpat_var (ident, _)} as other ->
            Format.print_string (Ident.name ident ^ " : ");
            Printtyp.type_expr Format.std_formatter pat_type;
            Format.print_newline ();
            default.pat mapper other
       | other -> default.pat mapper other)
  }*)

let () =
  Tast_mapper.register "tppx_interpret" newmapper

