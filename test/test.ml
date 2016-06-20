(* tester *)
let dir_listing directory =
  Array.to_list (Sys.readdir directory)

let command_and_print s = Printf.printf "%s%!" s; Sys.command s

let exec = if Sys.os_type = "Unix" then "cpdf" else "cpdf.exe"

let runtest filename =
  prerr_string filename;
  prerr_newline ();
  let r = command_and_print
    ("../ocamli -pp simple -no-peek " ^ filename)
  in
    if r = 0 then
      prerr_string "Passed.\n"
    else
      prerr_string (Printf.sprintf "Failed with code %i.\n" r)

let banned = ["ackermann.ml"; "input.ml"; "factorialpaper.ml"]

let remove = List.filter (fun x -> not (List.mem x banned))

let runtests dir =
  List.iter runtest (List.map (Filename.concat dir) (remove (dir_listing dir)))

let _ =
  match Sys.argv with
  | [|_|] -> runtests "../programs"
  | _ ->
      prerr_string "Syntax:"
