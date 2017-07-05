let _ =
  match Unix.fork () with
    0 -> print_string "I am in the child process!\n"
  | pid -> 
      if pid = 0 then print_string "Why is pid 0?\n" else
        begin
          print_string "I am in process: "; print_int pid; print_string "\n"
        end
