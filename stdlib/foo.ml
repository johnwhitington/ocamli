external get_argv: unit -> string * string array = "caml_sys_get_argv"

let (executable_name, argv) = get_argv()

