exception E of int * string

let x =
  raise (E (1, "foo"))

