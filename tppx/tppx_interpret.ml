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

(* Example: replace any number 1 annotate with [@interpret] by the number 42, and remove the annotation *)
let process tstr =
  let newmapper =
    {default with
       expr = fun mapper expr ->
         match expr with
         | {exp_attributes = [({txt = "print_type"}, _)];
            exp_type} as other ->
              Printtyp.type_expr Format.std_formatter exp_type;
              Format.print_newline ();
              default.expr mapper other
         | other -> default.expr mapper other}
  in
    newmapper.structure newmapper tstr

let _ =
  print_endline "Tppx running...";
  match Sys.argv with
    [|_; infile; outfile|] ->
      typedtree_to_file outfile (process (typedtree_of_file infile))
  | _ -> prerr_endline "Usage tppx_interpret <infile> <outfile>"

