open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getblob ~loc s =
  try
    let c = open_in_bin s in
      let s = String.init (in_channel_length c) (fun _ -> input_char c) in
        close_in c;
        s
   with
    _ -> raise (Location.Error (Location.error ~loc ("[%blob] could not find or load file " ^ s)))

let blob_mapper argv =
  (* Our blob_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "blob". *)
          Pexp_extension ({ txt = "blob"; loc }, pstr)} ->
        let error () =
          raise (Location.Error (
                  Location.error ~loc "[%blob] accepts a filename as a string, e.g. [%blob \"file.dat\"]"))
        in
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc = Pstr_eval ({ pexp_loc = loc } as expr, _) }] ->
            begin match Ast_convenience.get_str expr with
            | Some sym ->
              (* Replace with the contents of the file. *)
              (* Have to use Ast_helper.with_default_loc to pass the location to Ast_convenience.str until
                 https://github.com/alainfrisch/ppx_tools/pull/38 is released. *)
              with_default_loc loc (fun () -> Ast_convenience.str (getblob ~loc sym))
            | None -> error ()
            end
        | _ -> error ()
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "blob" blob_mapper

