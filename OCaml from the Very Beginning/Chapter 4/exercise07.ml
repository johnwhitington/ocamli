let rec rev_inner a l =
  match l with
    [] -> a
  | h::t -> rev_inner (h :: a) t

let rev l =
  rev_inner [] l
