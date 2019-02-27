(* The Standard Library *)
open Type

let rec make arity arity_left n f =
  {e =
    (if arity_left = 1 then CallBuiltIn (arity - 1, f) else
      (Function
        ([(PatVar (string_of_int (arity_left - 1)), None, (make arity (arity_left - 1) n f))],
        [])));
   lets = [];
   typ = Types.Tvar None;
   printas = Some n;
   peek = None}

let entry name arity func =
  (false, ref [("Stdlib." ^ name, make arity arity name (Obj.magic func : Obj.t))])

let stdlib =
  [entry "+" 2 ( + );
   entry "List.nth" 2 List.nth;
   entry "List.rev" 1 List.rev]

