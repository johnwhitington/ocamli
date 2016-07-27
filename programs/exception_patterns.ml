match raise Not_found with
  8 -> "yes"
| exception Not_found -> "no"

(* FIXME: Add guards, both for this and for try....with *)
(*match raise Not_found with
  8 -> "yes"
| exception Not_found when 1 = 1 -> "no"*)
