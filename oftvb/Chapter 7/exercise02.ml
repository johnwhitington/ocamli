let rec smallest_inner current found l =
  match l with
    [] ->
      if found then current else raise Not_found 
  | h::t ->
      if h > 0 && h < current
        then smallest_inner h true t
        else smallest_inner current found t

let smallest l =
  smallest_inner max_int false l

let smallest_or_zero l =
  try smallest l with Not_found -> 0
