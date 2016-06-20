(* tester *)
let dir_listing directory =
  Array.to_list (Sys.readdir directory)

let command_and_print s = Printf.printf "%s%!" s; Sys.command s

let exec = if Sys.os_type = "Unix" then "cpdf" else "cpdf.exe"

let progdir = ref "../programs"

let args = ref ""

let collect_strings s =
  args := !args ^ " " ^ s

let runtest filename =
  prerr_string filename;
  prerr_newline ();
  let r = command_and_print
    ("../ocamli -pp simple " ^ !args ^ " " ^ filename)
  in
    if r = 0 then
      prerr_string "Passed.\n"
    else
      prerr_string (Printf.sprintf "Failed with code %i.\n" r)

let banned = ["ackermann.ml"; "input.ml"; "factorialpaper.ml"]

let remove = List.filter (fun x -> not (List.mem x banned))

let runtests dir =
  List.iter runtest (List.map (Filename.concat dir) (remove (dir_listing dir)))

let argspec =
  [("-args", Arg.Rest collect_strings, " Arguments for ocamli")]

let usage = ""

let _ =
  Arg.parse argspec (fun _ -> ()) usage;
  runtests !progdir

