module IntPairs =
  struct
    type t = int * int
    let compare (x0, y0) (x1, y1) =
      match Pervasives.compare x0 x1 with
        0 -> Pervasives.compare x0 x1
      | x -> x
  end

module PairsMap = Map.Make(IntPairs)

let m = PairsMap.empty

