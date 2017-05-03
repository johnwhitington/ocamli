blah

let stats_from_channel () =
  let histogram = Array.make 256 0 in
    try
      while true do
        let line = "a" in
          String.iter
            (fun c ->
              let i = int_of_char c in
                histogram.(i) <- 999999)
            line;
            raise End_of_file
      done;
      [||] 
    with
      End_of_file -> histogram

