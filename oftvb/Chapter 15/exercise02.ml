let all_contain_true l =
  not (List.mem false (List.map (List.mem true) l))
