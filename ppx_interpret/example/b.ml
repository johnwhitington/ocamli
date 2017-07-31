(* This the module we wish to interpret *)

(* The marker to say this file should be interpreted *)
[%interpret]

let rec double x =
  if x < 100 then double (x * 2) else A.double x

