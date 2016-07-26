match raise Not_found with
  8 -> "yes"
| exception Not_found -> "no"

