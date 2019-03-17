(* This will be added by the PPX, eventually. *)
let env = ref []
let template_string = ""
let () = Print.showvals := false
let eval_full = Tppxsupport.eval_full env

let global = 40 + 2
let () = Tppxsupport.addenv env "global" (Obj.magic global : Obj.t) "" (* Added by PPX eventually *)

let x =
  let y = 1 - 0 in
    Tppxsupport.addenv env "y" (Obj.magic y : Obj.t) ""; (* Added by PPX, eventually *)
    (y + 2 * 3 + global [@interpret]) + 4

let _ =
  Printf.printf "Result is %i\n" x;
  exit 0

