(* The Standard Library *)
open Type

(* We build a Ocamli2type.t which is
 *
 * fun l -> CallBuiltIn (f, l)
 *
 * l is a list of arguments, f is a native function which have them applied. But how to apply?
 *
 * must be different for different arities? Anyway, proceed with just one for now.
 *
 * with a printas of List.rev *)

let list_rev =
  {e = CallBuiltIn (Obj.magic (List.rev : 'a -> 'a));
   lets = [];
   typ = Types.Tvar None;
   printas = Some "List.rev";
   peek = None}

let stdlib =
  [(false, ref [("Stdlib.+", Read.read "fun a b -> a + b")]);
   (false, ref [("Stdlib.List.rev", list_rev)])]

