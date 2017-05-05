open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let blob_mapper argv =
  (* Our blob_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "blob". *)
          Pexp_extension ({ txt = "auto"; loc }, pstr)} ->
        let error () =
          raise (Location.Error (
                  Location.error ~loc "[%auto] accepts a string, e.g. [%auto \"...\"]"))
        in
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc = Pstr_eval ({ pexp_loc = loc } as expr, _) }] ->
            begin match Ast_convenience.get_str expr with
            | Some sym ->
              (* Replace with the contents of the file. *)
              (* Have to use Ast_helper.with_default_loc to pass the location to Ast_convenience.str until
                 https://github.com/alainfrisch/ppx_tools/pull/38 is released. *)
              with_default_loc
                loc
                (fun () -> Ast_convenience.int 0)
            | None -> error ()
            end
        | _ -> error ()
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "blob" blob_mapper

