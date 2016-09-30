(* An example from Real World OCaml *)
module type X_int = sig val x : int end

module Three : X_int = struct let x = 3 end

let y = Three.x

module Increment (M : X_int) : X_int =
  struct
    let x = M.x + 1
  end

module Four = Increment(Three)

let z = Four.x

