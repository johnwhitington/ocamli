let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t

let string_replace_all x x' s =
  if x = "" then s else
    let p = ref 0
    and slen = String.length s
    and xlen = String.length x in
      let output = Buffer.create (slen * 2) in
        while !p < slen do
          try
            if String.sub s !p xlen = x
              then (Buffer.add_string output x'; p := !p + xlen)
              else (Buffer.add_char output s.[!p]; incr p)
          with
            _ -> Buffer.add_char output s.[!p]; incr p
        done;
        Buffer.contents output

