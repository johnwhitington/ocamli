external random_seed: unit -> int array = "caml_sys_random_seed"

type t = { st : int array; mutable idx : int }

let new_state () = { st = Array.make 55 0; idx = 0 }
let assign st1 st2 =
  Array.blit st2.st 0 st1.st 0 55;
  st1.idx <- st2.idx

let full_init s seed =
  let combine accu x = Digest.string (accu ^ string_of_int x) in
  let extract d =
    Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
    + (Char.code d.[3] lsl 24)
  in
  let seed = if Array.length seed = 0 then [| 0 |] else seed in
  let l = Array.length seed in
  for i = 0 to 54 do
    s.st.(i) <- i;
  done;
  let accu = ref "x" in
  for i = 0 to 54 + max 55 l do
    let j = i mod 55 in
    let k = i mod l in
    accu := combine !accu seed.(k);
    s.st.(j) <- (s.st.(j) lxor extract !accu) land 0x3FFFFFFF;  (* PR#5575 *)
  done;
  s.idx <- 0

let make seed =
  let result = new_state () in
  full_init result seed;
  result

let make_self_init () = make (random_seed ())

let _ = make_self_init ()

