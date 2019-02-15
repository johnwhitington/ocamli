let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t

