let pp_make_formatter f g h i =
  {
    pp_scan_stack = 0;
  }

let make_formatter output flush =
  pp_make_formatter output flush ignore ignore

let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> 0)

let std_formatter = formatter_of_out_channel 0
