let rec chop k l =
  if k = 0 then l else begin
    match l with
    | _::t -> chop (k-1) t
    | _ -> assert false
  end
;;

