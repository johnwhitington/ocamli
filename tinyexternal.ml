open Tinyocaml
open Parsetree
open Asttypes

(* The type of OCaml values in memory *)
type untyped_ocaml_value =
  UInt of int
| UBlock of int * untyped_ocaml_value array
| UString of string
| UDouble of float
| UDoubleArray of float array

let rec string_of_untyped = function
  UInt i -> Printf.sprintf "UInt %i" i
| UBlock (tag, arr) -> Printf.sprintf "UBlock %i [|%s|]" tag (string_of_untyped_array arr)
| UString s -> Printf.sprintf "UString %s" s
| UDouble f -> Printf.sprintf "UDouble %f" f
| UDoubleArray arr -> Printf.sprintf "UDoubleArray [|%s|]" (string_of_untyped_float_array arr)

and string_of_untyped_array arr =
  List.fold_left ( ^ ) "" (List.map (fun x -> Printf.sprintf "%s; " (string_of_untyped x)) (Array.to_list arr))

and string_of_untyped_float_array arr =
  List.fold_left ( ^ ) "" (List.map (fun x -> Printf.sprintf "%f; " x) (Array.to_list arr))

external to_ocaml_value : t -> 'a = "to_ocaml_value"

external untyped_of_ocaml_value : 'a -> untyped_ocaml_value = "untyped_of_ocaml_value"

let rec read_untyped debug_typ v typ =
  Printf.printf "read_untyped: considering %s of type %s\n" (string_of_untyped v) debug_typ;
  match v, typ.ptyp_desc with
  | UInt n, Ptyp_constr ({txt = Longident.Lident "int"}, _) ->
      Int n
  | UInt n, Ptyp_constr ({txt = Longident.Lident "bool"}, _) ->
      Bool (n <> 0)
  | UInt n, Ptyp_constr ({txt = Longident.Lident "char"}, _) ->
      Char (char_of_int n)
  | UInt 0, Ptyp_constr ({txt = Longident.Lident "list"}, _) ->
      Nil
  | UInt 0, Ptyp_constr ({txt = Longident.Lident "unit"}, _) ->
      Unit
  | UString s, Ptyp_constr ({txt = Longident.Lident "string"}, _) ->
      String s
  | UDouble d, Ptyp_constr ({txt = Longident.Lident "float"}, _) ->
      Float d
  | UBlock (0, vs), Ptyp_tuple ts when Array.length vs = List.length ts ->
      Tuple (List.map2 (read_untyped debug_typ) (Array.to_list vs) ts) (* FIXME: Check no array duplication here *)
  | UBlock (0, vs), Ptyp_constr ({txt = Longident.Lident "array"}, [elt_typ]) ->
      Array (Array.map (fun x -> read_untyped debug_typ x elt_typ) vs)
  | UBlock (0, [|h; t|]), Ptyp_constr ({txt = Longident.Lident "list"}, [elt_typ]) ->
      Cons (read_untyped debug_typ h elt_typ, read_untyped debug_typ t typ)
  | UBlock (0, vs), Ptyp_constr ({txt = Longident.Lident "ref"}, [elt_typ]) ->
      (* Just an example. We will need to look up the type to reconstruct the real record *)
      Record (List.map (fun x -> ("contents", ref (read_untyped debug_typ x elt_typ))) (Array.to_list vs))
  | UInt 0, Ptyp_constr ({txt = Longident.Lident "option"}, [elt_typ]) ->
      (* Just an example. None *)
      Constr (0, "None", None)
  | UBlock (0, [|v|]), Ptyp_constr ({txt = Longident.Lident "option"}, [elt_typ]) ->
      (* Just an example. Some x *)
      Constr (0, "Some", Some (read_untyped debug_typ v elt_typ))
  | UDoubleArray arr, _ -> Array (Array.map (fun x -> Float x) arr)
  | b, _ -> failwith (Printf.sprintf "read_untyped: unimplemented : %s of type %s" (string_of_untyped b) debug_typ)

let parse_type typ =
  typ |> Lexing.from_string |> Parse.core_type

let of_ocaml_value x typ =
  read_untyped typ (untyped_of_ocaml_value x) (parse_type typ)

