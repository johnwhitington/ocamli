let rec read_dict () =
  let i = read_int () in
    if i = 0 then [] else
      let name = read_line () in
        (i, name) :: read_dict ()

