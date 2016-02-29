exception E

let _ =
  try raise E with E -> 4

