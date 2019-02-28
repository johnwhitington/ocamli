(* The Standard Library *)
open Type

(* Build nested arity 1 functions so that partial application works. *)
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

let f name arity func =
  (false, ref [("Stdlib." ^ name, make arity arity name (Obj.magic func : Obj.t))])

let stdlib =
  [f "+" 2 ( + );
   f "List.nth" 2 List.nth;
   f "List.rev" 1 List.rev]

