(* The Standard Library *)
open Type

let make_t n e =
  {e; lets = []; typ = Types.Tvar None; printas = Some n; peek = None}

let rec make arity arity_left n f =
  if arity_left = 1 then
    make_t n (CallBuiltIn (arity - 1, f))
  else
    make_t n
      (Function
         ([(PatVar (string_of_int (arity_left - 1)), None, (make arity (arity_left - 1) n f))],
          []))

let entry name arity func =
  (false, ref [("Stdlib." ^ name, make arity arity name (Obj.magic func : Obj.t))])

let stdlib =
  [entry "( + )" 2 ( + );
   entry "List.nth" 2 List.nth;
   entry "List.rev" 1 List.rev]

