(* Tail-recursive lists *)
module List :
  sig
    include (module type of List)
  end
=
  struct
    include List

    let map f l =
      List.rev (List.rev_map f l)

    let append a b =
      List.rev_append (List.rev a) b

    let ( @ ) = append

    let map f l =
      List.rev (List.rev_map f l)

    let rev_map2 f l l2 =
      let rec rev_map_inner acc a b =
        match a, b with
          [], [] -> acc
        | x::xs, y::ys -> rev_map_inner (f x y :: acc) xs ys
        | _ -> raise (Invalid_argument "List.map2")
      in
        rev_map_inner [] l l2

    let map2 f l l2 =
      List.rev (rev_map2 f l l2)

    let concat lists =
      let rec concat out acc =
        match acc with
        | [] -> out
        | l::ls -> concat (append l out) ls
      in
        concat [] (List.rev lists)

    let fold_right f l e =
      List.fold_left (fun x y -> f y x) e (List.rev l)
  end

