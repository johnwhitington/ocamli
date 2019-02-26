(* The Standard Library *)
open Type

let make_t e n =
  {e; lets = []; typ = Types.Tvar None; printas = Some n; peek = None}

let make1 n f =
  make_t (CallBuiltIn (0, f)) n

let make2 n f =
  make_t
    (Function
       ([(PatVar "a", None, (make_t (CallBuiltIn (1, f)) n))],
        [])
    )
    n

let stdlib =
  [(false, ref [("Stdlib.( + )", make2 "( + )" (Obj.magic ( + ) : Obj.t))]);
   (false, ref [("Stdlib.List.nth", make2 "List.nth" (Obj.magic List.nth : Obj.t))]);
   (false, ref [("Stdlib.List.rev", make1 "List.rev" (Obj.magic List.rev : Obj.t))])]

