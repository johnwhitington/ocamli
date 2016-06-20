(* tester *)
let dir_listing directory =
  Array.to_list (Sys.readdir directory)

let command_and_print s = Printf.printf "%s%!" s; Sys.command s

let exec = if Sys.os_type = "Unix" then "cpdf" else "cpdf.exe"

let runtest filename =
  prerr_string filename;
  prerr_newline ()

let runtests dir =
  List.iter runtest (List.map (Filename.concat dir) (dir_listing dir))

let _ =
  match Sys.argv with
  | [|_|] -> runtests "../programs"
  | _ ->
      prerr_string "Syntax:"
