let n = ref 0

let p =
  let n = ref 0 in (fun x -> n := 32); !n

let x = n

