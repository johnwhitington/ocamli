open Tast_mapper
open Asttypes

let newmapper argv =
  {default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = [{attr_name = {txt = "interpret"}; attr_payload =  payload}];
          exp_type} as other ->
            print_endline "Found a [@interpret] annotation";
            default.expr mapper other
       | other -> 
           print_endline "Not an [@interpret] annotation";
           default.expr mapper other)}

let () =
  Tast_mapper.register "tppx_interpret" newmapper

