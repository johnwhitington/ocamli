(* Environments must relate to the point of definition *)
(*let _ =
  let a = 6 in
    let f = function () -> a in
      let a = 7 in
        f ()*)

(*let a = 6

let f () = a

let a = 7

let _ = f ()*)

(* Now, a recursive example *)
(*let f x = x + 1

(*let rec sum x =
  if x = 0 then f 0 else 1 + sum (x - 1)*)

let f x = x - 1*)

let _ = List.sum 2

