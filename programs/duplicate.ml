(* Environments must relate to the point of definition *)
(*let _ =
  let a = 6 in
    let f () = print_int a in
      let a = 7 in
        f ()*)

let _ =
  let a = 6 in
    let f = function () -> a in
      let a = 7 in
        f ()

(*let a = 6

let f () = print_int a

let a = 7

let _ = f ()*)

