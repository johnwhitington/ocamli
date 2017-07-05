let rec gcd a b =
  match b with
    0 -> a
  | _ -> gcd b (a mod b)
