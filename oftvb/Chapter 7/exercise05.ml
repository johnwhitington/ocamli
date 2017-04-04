exception Complex

let safe_sqrt n =
  try sqrt n with Complex -> 0
