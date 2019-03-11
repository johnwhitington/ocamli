(* The Standard Library *)
let f name func =
  let e = 
    let open Type in
      {e = Value (Obj.magic func : Obj.t);
       lets = [];
       typ = Types.Tvar (Some "DEBUG-lib.ml");
       printas = Some name;
       peek = None}
  in
    (false, ref [("Stdlib." ^ name, e)])

let stdlib =
  [f "+" ( + );
   f "List.nth" List.nth;
   f "List.rev" List.rev;
   f "List.map2" List.map2;
   f "List.tl" List.tl;
   f "List.map" List.map;
  ]

