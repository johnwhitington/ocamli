let x = 1 + 1 and y = 2 + 2

let p = 
  let x = 1 + 1 and y = 2 + 2 in x + y

(*let rec x = 1 and y = 2

let rec x = 1 and y = 2 in x + y*)

(*(* FIXME: Example which requires alpha-renaming of the anonymous function to allow the
Lets to be moved under the fun: *)

(let rec f x = g x + 1 and g x = f x + 1 in fun f -> f + g 1) 2

(* Neither part of the recursive binding 'f' or 'g' is unused, so we cannot
 * remove either. And we cannot do this, beause the 'f's clash: *)

(fun f -> let rec f x = g x + 1 and g x = f x + 1 in f + g 1) 2

(* So we are forced to alpha-rename: *)

(fun f' -> let rec f x = g x + 1 and g x = f x + 1 in f' + g 1) 2

(* Is this situation so rare that the alpha-renaming is not a problem for the
user? *)*)
