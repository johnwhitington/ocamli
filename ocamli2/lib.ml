(* The Standard Library *)
let f name func =
  let e = 
    let open Type in
      {e = Value (Obj.magic func : Obj.t);
       lets = [];
       typ = {level = 0; scope = 0; id = 0; desc = Types.Tvar (Some "DEBUG-lib.ml")};
       printas = Some name;
       printbefore = Some ("{entering " ^ name ^ "}\n");
       printafter = Some ("{leaving " ^ name ^ "}\n");
       peek = None}
  in
    (false, ref [("Stdlib." ^ name, e)])

let stdlib =
  [f "+" ( + );
   f "List.nth" List.nth;
   f "List.rev" List.rev;
   f "List.map2" List.map2;
   f "List.tl" List.tl;
   f "List.hd" List.hd;
   f "List.map" List.map;
   f "Random.int" Random.int;
  ]

