type formatter = {
  mutable pp_out_string : string -> int -> int -> unit;
  mutable pp_out_newline : unit -> unit;
}

let display_newline state () =
  state.pp_out_string "\n" 0  1

let pp_make_formatter f h =
  {
    pp_out_string = f;
    pp_out_newline = h;
  }

let make_formatter output flush =
  let ppf = pp_make_formatter output ignore in
  ppf.pp_out_newline <- display_newline ppf;
  ppf

let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> flush oc)

let std_formatter = formatter_of_out_channel Pervasives.stdout
