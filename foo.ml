open Ocamliutil
open Parsetree
let of_real_ocaml =
  ref (fun (_ : Tinyocaml.env) -> fun _ -> ([], (Tinyocaml.Int 0)))
let exception_from_ocaml e =
  snd
    ((!of_real_ocaml) [] (ast ("raise (" ^ ((Printexc.to_string e) ^ ")"))))
external word_size : unit -> int = "%word_size"
let percent_word_size =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Int (word_size ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%word_size")) in
  ("%word_size",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%word_size", [Tinyocaml.Var "*a"], f)),
         [])))
external int_size : unit -> int = "%int_size"
let percent_int_size =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Int (int_size ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%int_size")) in
  ("%int_size",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%int_size", [Tinyocaml.Var "*a"], f)),
         [])))
external max_wosize : unit -> int = "%max_wosize"
let percent_max_wosize =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Int (max_wosize ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%max_wosize")) in
  ("%max_wosize",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%max_wosize", [Tinyocaml.Var "*a"], f)), [])))
external big_endian : unit -> bool = "%big_endian"
let percent_big_endian =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Bool (big_endian ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%big_endian")) in
  ("%big_endian",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%big_endian", [Tinyocaml.Var "*a"], f)), [])))
external unix : unit -> bool = "%ostype_unix"
let percent_ostype_unix =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Bool (unix ()) with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%ostype_unix")) in
  ("%ostype_unix",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%ostype_unix", [Tinyocaml.Var "*a"], f)), [])))
external win32 : unit -> bool = "%ostype_win32"
let percent_ostype_win32 =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Bool (win32 ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%ostype_win32")) in
  ("%ostype_win32",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%ostype_win32", [Tinyocaml.Var "*a"], f)), [])))
external cygwin : unit -> bool = "%ostype_cygwin"
let percent_ostype_cygwin =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Unit)::[] ->
             (try Tinyocaml.Bool (cygwin ())
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%ostype_cygwin")) in
  ("%ostype_cygwin",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%ostype_cygwin", [Tinyocaml.Var "*a"], f)), [])))
external percent_boolnot : bool -> bool = "%boolnot"
let percent_boolnot =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Bool a)::[] ->
             (try Tinyocaml.Bool (percent_boolnot a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%boolnot")) in
  ("%boolnot",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%boolnot", [Tinyocaml.Var "*a"], f)),
         [])))
external percent_negint : int -> int = "%negint"
let percent_negint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.Int (percent_negint a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%negint")) in
  ("%negint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%negint", [Tinyocaml.Var "*a"], f)),
         [])))
external caml_ml_input_scan_line :
  in_channel -> int = "caml_ml_input_scan_line"
let caml_ml_input_scan_line =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.InChannel a)::[] ->
             (try Tinyocaml.Int (caml_ml_input_scan_line a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_input_scan_line")) in
  ("caml_ml_input_scan_line",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ml_input_scan_line", [Tinyocaml.Var "*a"], f)), [])))
external caml_create_string : int -> string = "caml_create_string"
let caml_create_string =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.String (Bytes.of_string (caml_create_string a))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_create_string")) in
  ("caml_create_string",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_create_string", [Tinyocaml.Var "*a"], f)), [])))
external caml_create_bytes : int -> string = "caml_create_bytes"
let caml_create_bytes =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.String (Bytes.of_string (caml_create_bytes a))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_create_bytes")) in
  ("caml_create_bytes",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_create_bytes", [Tinyocaml.Var "*a"], f)), [])))
external caml_int64_float_of_bits :
  int64 -> float = "caml_int64_float_of_bits"
let caml_int64_float_of_bits =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int64 a)::[] ->
             (try Tinyocaml.Float (caml_int64_float_of_bits a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_int64_float_of_bits")) in
  ("caml_int64_float_of_bits",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_int64_float_of_bits", [Tinyocaml.Var "*a"], f)), [])))
external ignore : 'a -> unit = "%ignore"
let percent_ignore =
  let f =
    function
    | env ->
        (function
         | a::[] ->
             (try ignore a; Tinyocaml.Unit with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%ignore")) in
  ("%ignore",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%ignore", [Tinyocaml.Var "*a"], f)),
         [])))
external caml_ml_input_char : in_channel -> char = "caml_ml_input_char"
let caml_ml_input_char =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.InChannel a)::[] ->
             (try Tinyocaml.Char (caml_ml_input_char a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_input_char")) in
  ("caml_ml_input_char",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ml_input_char", [Tinyocaml.Var "*a"], f)), [])))
external open_descriptor_out :
  int -> out_channel = "caml_ml_open_descriptor_out"
let caml_ml_open_descriptor_out =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.OutChannel (open_descriptor_out a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_open_descriptor_out")) in
  ("caml_ml_open_descriptor_out",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ml_open_descriptor_out", [Tinyocaml.Var "*a"], f)),
         [])))
external open_descriptor_in :
  int -> in_channel = "caml_ml_open_descriptor_in"
let caml_ml_open_descriptor_in =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.InChannel (open_descriptor_in a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_open_descriptor_in")) in
  ("caml_ml_open_descriptor_in",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ml_open_descriptor_in", [Tinyocaml.Var "*a"], f)),
         [])))
external caml_ml_flush : out_channel -> unit = "caml_ml_flush"
let caml_ml_flush =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.OutChannel a)::[] ->
             (try caml_ml_flush a; Tinyocaml.Unit
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_flush")) in
  ("caml_ml_flush",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ml_flush", [Tinyocaml.Var "*a"], f)), [])))
external caml_sqrt_float : float -> float = "caml_sqrt_float"
let caml_sqrt_float =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Float (caml_sqrt_float a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_sqrt_float")) in
  ("caml_sqrt_float",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_sqrt_float", [Tinyocaml.Var "*a"], f)), [])))
external caml_ceil_float : float -> float = "caml_ceil_float"
let caml_ceil_float =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Float (caml_ceil_float a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ceil_float")) in
  ("caml_ceil_float",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_ceil_float", [Tinyocaml.Var "*a"], f)), [])))
external caml_floor_float : float -> float = "caml_floor_float"
let caml_floor_float =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Float (caml_floor_float a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_floor_float")) in
  ("caml_floor_float",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_floor_float", [Tinyocaml.Var "*a"], f)), [])))
external percent_negfloat : float -> float = "%negfloat"
let percent_negfloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Float (percent_negfloat a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%negfloat")) in
  ("%negfloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "%negfloat", [Tinyocaml.Var "*a"], f)),
         [])))
external percent_intoffloat : float -> int = "%intoffloat"
let percent_intoffloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Int (percent_intoffloat a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%intoffloat")) in
  ("%intoffloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%intoffloat", [Tinyocaml.Var "*a"], f)), [])))
external caml_sin_float : float -> float = "caml_sin_float"
let caml_sin_float =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::[] ->
             (try Tinyocaml.Float (caml_sin_float a)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_sin_float")) in
  ("caml_sin_float",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_sin_float", [Tinyocaml.Var "*a"], f)), [])))
external percent_divfloat : float -> float -> float = "%divfloat"
let percent_divfloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::(Tinyocaml.Float b)::[] ->
             (try Tinyocaml.Float (percent_divfloat a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%divfloat")) in
  ("%divfloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%divfloat",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external percent_mulfloat : float -> float -> float = "%mulfloat"
let percent_mulfloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::(Tinyocaml.Float b)::[] ->
             (try Tinyocaml.Float (percent_mulfloat a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%mulfloat")) in
  ("%mulfloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%mulfloat",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external percent_subloat : float -> float -> float = "%subfloat"
let percent_subfloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::(Tinyocaml.Float b)::[] ->
             (try Tinyocaml.Float (percent_subloat a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%subfloat")) in
  ("%subfloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%subfloat",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external percent_addfloat : float -> float -> float = "%addfloat"
let percent_addfloat =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Float a)::(Tinyocaml.Float b)::[] ->
             (try Tinyocaml.Float (percent_addfloat a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%addfloat")) in
  ("%addfloat",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%addfloat",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external caml_format_int : string -> int -> string = "caml_format_int"
let caml_format_int =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::[] ->
             (try
                Tinyocaml.String
                  (Bytes.of_string (caml_format_int (Bytes.to_string a) b))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_format_int")) in
  ("caml_format_int",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "caml_format_int",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external nativeint_sub :
  nativeint -> nativeint -> nativeint = "%nativeint_sub"
let percent_nativeint_sub =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.NativeInt a)::(Tinyocaml.NativeInt b)::[] ->
             (try Tinyocaml.NativeInt (nativeint_sub a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%nativeint_sub")) in
  ("%nativeint_sub",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%nativeint_sub",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external nativeint_lsl : nativeint -> int -> nativeint = "%nativeint_lsl"
let percent_nativeint_lsl =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.NativeInt a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.NativeInt (nativeint_lsl a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%nativeint_lsl")) in
  ("%nativeint_lsl",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%nativeint_lsl",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external string_safe_get : string -> int -> char = "%string_safe_get"
let percent_string_safe_get =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Char (string_safe_get (Bytes.to_string a) b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%string_safe_get")) in
  ("%string_safe_get",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%string_safe_get",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external modint : int -> int -> int = "%modint"
let percent_modint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (modint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%modint")) in
  ("%modint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%modint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external caml_ml_output_char :
  out_channel -> char -> unit = "caml_ml_output_char"
let caml_ml_output_char =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.OutChannel a)::(Tinyocaml.Char b)::[] ->
             (try caml_ml_output_char a b; Tinyocaml.Unit
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_output_char")) in
  ("caml_ml_output_char",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "caml_ml_output_char",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external xorint : int -> int -> int = "%xorint"
let percent_xorint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (xorint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%xorint")) in
  ("%xorint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%xorint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external lslint : int -> int -> int = "%lslint"
let percent_lslint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (lslint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%lslint")) in
  ("%lslint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%lslint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external andint : int -> int -> int = "%andint"
let percent_andint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (andint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%andint")) in
  ("%andint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%andint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external caml_sys_exit : int -> 'a = "caml_sys_exit"
let caml_sys_exit =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try caml_sys_exit a with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_sys_exit")) in
  ("caml_sys_exit",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_sys_exit", [Tinyocaml.Var "*a"], f)), [])))
external string_unsafe_get : string -> int -> char = "%string_unsafe_get"
let percent_string_unsafe_get =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Char (string_unsafe_get (Bytes.to_string a) b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%string_unsafe_get")) in
  ("%string_unsafe_get",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%string_unsafe_get",
                   [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"], f)), [])), [])))
external string_safe_set : string -> int -> char -> unit = "%string_safe_set"
let percent_string_safe_set =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::(Tinyocaml.Char c)::[] ->
             (try string_safe_set (Bytes.to_string a) b c; Tinyocaml.Unit
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%string_safe_set")) in
  ("%string_safe_set",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.Fun
                 (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*c"),
                   (Tinyocaml.CallBuiltIn
                      (None, "%string_safe_set",
                        [Tinyocaml.Var "*a";
                        Tinyocaml.Var "*b";
                        Tinyocaml.Var "*c"], f)), [])), [])), [])))
external caml_md5_string : string -> int -> int -> string = "caml_md5_string"
let caml_md5_string =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::(Tinyocaml.Int c)::[] ->
             (try
                Tinyocaml.String
                  (Bytes.of_string (caml_md5_string (Bytes.to_string a) b c))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_md5_string")) in
  ("caml_md5_string",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.Fun
                 (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*c"),
                   (Tinyocaml.CallBuiltIn
                      (None, "caml_md5_string",
                        [Tinyocaml.Var "*a";
                        Tinyocaml.Var "*b";
                        Tinyocaml.Var "*c"], f)), [])), [])), [])))
external unsafe_input :
  in_channel -> string -> int -> int -> int = "caml_ml_input"
let caml_ml_input =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.InChannel a)::(Tinyocaml.String b)::(Tinyocaml.Int
             c)::(Tinyocaml.Int d)::[] ->
             (try Tinyocaml.Int (unsafe_input a (Bytes.to_string b) c d)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_input")) in
  ("caml_ml_input",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.Fun
                 (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*c"),
                   (Tinyocaml.Fun
                      (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*d"),
                        (Tinyocaml.CallBuiltIn
                           (None, "caml_ml_input",
                             [Tinyocaml.Var "*a";
                             Tinyocaml.Var "*b";
                             Tinyocaml.Var "*c";
                             Tinyocaml.Var "*d"], f)), [])), [])), [])), [])))
external unsafe_output_string :
  out_channel -> string -> int -> int -> unit = "caml_ml_output"
let caml_ml_output =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.OutChannel a)::(Tinyocaml.String b)::(Tinyocaml.Int
             c)::(Tinyocaml.Int d)::[] ->
             (try
                unsafe_output_string a (Bytes.to_string b) c d;
                Tinyocaml.Unit
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_ml_output")) in
  ("caml_ml_output",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.Fun
                 (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*c"),
                   (Tinyocaml.Fun
                      (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*d"),
                        (Tinyocaml.CallBuiltIn
                           (None, "caml_ml_output",
                             [Tinyocaml.Var "*a";
                             Tinyocaml.Var "*b";
                             Tinyocaml.Var "*c";
                             Tinyocaml.Var "*d"], f)), [])), [])), [])), [])))
external caml_blit_string :
  string -> int -> string -> int -> int -> unit = "caml_blit_string"
let caml_blit_string =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::(Tinyocaml.Int b)::(Tinyocaml.String
             c)::(Tinyocaml.Int d)::(Tinyocaml.Int e)::[] ->
             (try
                caml_blit_string (Bytes.to_string a) b (Bytes.to_string c) d
                  e;
                Tinyocaml.Unit
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_blit_string")) in
  ("caml_blit_string",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.Fun
                 (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*c"),
                   (Tinyocaml.Fun
                      (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*d"),
                        (Tinyocaml.Fun
                           (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*e"),
                             (Tinyocaml.CallBuiltIn
                                (None, "caml_blit_string",
                                  [Tinyocaml.Var "*a";
                                  Tinyocaml.Var "*b";
                                  Tinyocaml.Var "*c";
                                  Tinyocaml.Var "*d";
                                  Tinyocaml.Var "*e"], f)), [])), [])), [])),
              [])), [])))
external lsrint : int -> int -> int = "%lsrint"
let percent_lsrint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (lsrint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%lsrint")) in
  ("%lsrint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%lsrint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external string_length : string -> int = "%string_length"
let percent_string_length =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::[] ->
             (try Tinyocaml.Int (string_length (Bytes.to_string a))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%string_length")) in
  ("%string_length",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%string_length", [Tinyocaml.Var "*a"], f)), [])))
external asrint : int -> int -> int = "%asrint"
let percent_asrint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (asrint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%asrint")) in
  ("%asrint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%asrint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external addint : int -> int -> int = "%addint"
let percent_addint =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::(Tinyocaml.Int b)::[] ->
             (try Tinyocaml.Int (addint a b)
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%addint")) in
  ("%addint",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.Fun
            (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*b"),
              (Tinyocaml.CallBuiltIn
                 (None, "%addint", [Tinyocaml.Var "*a"; Tinyocaml.Var "*b"],
                   f)), [])), [])))
external caml_int_of_string : string -> int = "caml_int_of_string"
let caml_int_of_string =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::[] ->
             (try Tinyocaml.Int (caml_int_of_string (Bytes.to_string a))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "caml_int_of_string")) in
  ("caml_int_of_string",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "caml_int_of_string", [Tinyocaml.Var "*a"], f)), [])))
external bytes_length : string -> int = "%bytes_length"
let percent_bytes_length =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.String a)::[] ->
             (try Tinyocaml.Int (bytes_length (Bytes.to_string a))
              with | e -> exception_from_ocaml e)
         | _ -> raise (RuntimeTypeError "%bytes_length")) in
  ("%bytes_length",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn
            (None, "%bytes_length", [Tinyocaml.Var "*a"], f)), [])))
open Tinyocaml
let exe = ref (Bytes.of_string "")
let argv = ref [||]
let debug = ref false
let emulated = ref false
let mk name f =
  (name,
    (Fun
       (NoLabel, (PatVar "*x"), (CallBuiltIn (None, name, [Var "*x"], f)),
         [])))
let mk2 name f =
  (name,
    (Fun
       (NoLabel, (PatVar "*x"),
         (Fun
            (NoLabel, (PatVar "*y"),
              (CallBuiltIn (None, name, [Var "*x"; Var "*y"], f)), [])), [])))
let mk3 name f =
  (name,
    (Fun
       (NoLabel, (PatVar "*x"),
         (Fun
            (NoLabel, (PatVar "*y"),
              (Fun
                 (NoLabel, (PatVar "*z"),
                   (CallBuiltIn
                      (None, name, [Var "*x"; Var "*y"; Var "*z"], f)), [])),
              [])), [])))
let mk4 ?(x1= "x")  ?(x2= "y")  ?(x3= "z")  ?(x4= "q")  name f =
  (name,
    (Fun
       (NoLabel, (PatVar (star x1)),
         (Fun
            (NoLabel, (PatVar (star x2)),
              (Fun
                 (NoLabel, (PatVar (star x3)),
                   (Fun
                      (NoLabel, (PatVar (star x4)),
                        (CallBuiltIn
                           (None, name,
                             [Var (star x1);
                             Var (star x2);
                             Var (star x3);
                             Var (star x4)], f)), [])), [])), [])), [])))
let mk5 ?(x1= "x")  ?(x2= "y")  ?(x3= "z")  ?(x4= "q")  ?(x5= "p")  name f =
  (name,
    (Fun
       (NoLabel, (PatVar (star x1)),
         (Fun
            (NoLabel, (PatVar (star x2)),
              (Fun
                 (NoLabel, (PatVar (star x3)),
                   (Fun
                      (NoLabel, (PatVar (star x4)),
                        (Fun
                           (NoLabel, (PatVar (star x5)),
                             (CallBuiltIn
                                (None, name,
                                  [Var (star x1);
                                  Var (star x2);
                                  Var (star x3);
                                  Var (star x4);
                                  Var (star x5)], f)), [])), [])), [])), [])),
         [])))
let eval_until_value = ref (fun _ -> fun _ -> fun _ -> fun x -> x)
let caml_register_named_value =
  mk2 "caml_register_named_value"
    (function
     | env ->
         (function
          | (String name)::func::[] ->
              let c_function x =
                let tinyocaml_x = Tinyexternal.of_ocaml_value [] x "int" in
                let output =
                  (!eval_until_value) true false [] (App (func, tinyocaml_x)) in
                Tinyexternal.to_ocaml_value output in
              (Callback.register (Bytes.to_string name) c_function; Unit)
          | _ -> failwith "builtin_caml_register_value"))
external array_safe_get : 'a array -> int -> 'a = "%array_safe_get"
let percent_array_safe_get =
  mk2 "%array_safe_get"
    (function
     | env ->
         (function
          | (Array x)::(Int i)::[] ->
              (try
                 Tinyexternal.of_ocaml_value env
                   (array_safe_get (Tinyexternal.to_ocaml_value (Array x)) i)
                   "int"
               with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_array_safe_get"))
let emulated_percent_array_safe_get =
  mk2 "%array_safe_get"
    (function
     | env ->
         (function
          | (Array x)::(Int i)::[] ->
              (try x.(i) with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_array_safe_get"))
external percent_boolnot : bool -> bool = "%boolnot"
let percent_boolnot =
  mk "%boolnot"
    (function
     | env ->
         (function
          | (Bool b)::[] ->
              (try
                 Tinyexternal.of_ocaml_value env
                   (percent_boolnot (Tinyexternal.to_ocaml_value (Bool b)))
                   "bool"
               with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_boolnot"))
let emulated_percent_boolnot =
  mk "%boolnot"
    (function
     | env ->
         (function
          | (Bool b)::[] ->
              (try Bool (not b) with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_boolnot"))
external percent_negfloat : float -> float = "%negfloat"
let percent_negfloat =
  mk "%negfloat"
    (function
     | env ->
         (function
          | (Float f)::[] ->
              (try
                 Tinyexternal.of_ocaml_value env
                   (percent_negfloat (Tinyexternal.to_ocaml_value (Float f)))
                   "float"
               with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_negfloat"))
let emulated_percent_negfloat =
  mk "%negfloat"
    (function
     | env ->
         (function
          | (Float f)::[] ->
              (try Float (-. f) with | e -> exception_from_ocaml e)
          | _ -> failwith "percent_negfloat"))
external caml_int_of_string : string -> int = "caml_int_of_string"
let caml_int_of_string =
  mk "caml_int_of_string"
    (function
     | env ->
         (function
          | (String s)::[] ->
              (try
                 Tinyexternal.of_ocaml_value []
                   (caml_int_of_string
                      (Tinyexternal.to_ocaml_value (String s))) "int"
               with | e -> exception_from_ocaml e)
          | _ -> failwith "caml_int_of_string"))
let emulated_caml_int_of_string =
  mk "caml_int_of_string"
    (function
     | env ->
         (function
          | (String s)::[] ->
              (try Int (int_of_string (Bytes.to_string s))
               with | e -> exception_from_ocaml e)
          | _ -> failwith "caml_int_of_string"))
external caml_create_string : int -> string = "caml_create_string"
let caml_create_string =
  mk "caml_create_string"
    (function
     | env ->
         (function
          | (Int i)::[] ->
              (try
                 Tinyexternal.of_ocaml_value []
                   (caml_create_string (Tinyexternal.to_ocaml_value (Int i)))
                   "string"
               with | e -> exception_from_ocaml e)
          | _ -> failwith "caml_create_string"))
let emulated_caml_create_string =
  mk "caml_create_string"
    (function
     | env ->
         (function
          | (Int i)::[] ->
              (try String (Bytes.create i) with | e -> exception_from_ocaml e)
          | _ -> failwith "caml_create_string"))
[@@@ocaml.text " END OF VALUE TESTS "]
external bytes_to_string : bytes -> string = "%bytes_to_string"
let percent_bytes_to_string =
  mk "%bytes_to_string"
    (function
     | env ->
         (function
          | (String x)::[] -> String (Bytes.copy x)
          | _ -> failwith "%bytes_to_string"))
let percent_backend_type =
  mk "%backend_type"
    (function
     | env ->
         (function
          | (Unit)::[] ->
              Constr (0, "Other", (Some (String (Bytes.of_string "ocamli"))))
          | _ -> failwith "percent_backend_type"))
let percent_raise =
  mk "%raise"
    (function
     | env ->
         (function
          | (Constr (_, n, eopt))::[] -> Raise (n, eopt)
          | _ -> failwith "percent_raise"))
let percent_raise_notrace =
  mk "%raise_notrace"
    (function
     | env ->
         (function
          | e::[] -> Raise ("FixPercentRaiseNotrace", None)
          | _ -> failwith "percent_raise_notrace"))
let percent_apply =
  mk2 "%apply"
    (function
     | env ->
         (function | f::a::[] -> App (f, a) | _ -> failwith "percent_apply"))
let percent_revapply =
  mk2 "%revapply"
    (function
     | env ->
         (function
          | a::f::[] -> App (f, a)
          | _ -> failwith "percent_revapply"))
let percent_identity =
  mk "%identity"
    (function
     | env -> (function | x::[] -> x | _ -> failwith "percent_identity"))
let percent_makemutable =
  mk "%makemutable"
    (function
     | env ->
         (function
          | e::[] -> Record [("contents", (ref e))]
          | _ -> failwith "percent_makemutable"))
let percent_field0 =
  mk "%field0"
    (function
     | env ->
         (function
          | (Record ((_, { contents = e })::[]))::[] -> e
          | _ -> failwith "percent_field0"))
let percent_setfield0 =
  mk2 "%setfield0"
    (function
     | env ->
         (function
          | (Record ((_, r)::[]))::e::[] -> (r := e; Unit)
          | _ -> failwith "percent_setfield0"))
let percent_compare =
  mk2 "%compare"
    (function
     | env ->
         (function
          | a::b::[] -> Int (compare a b)
          | _ -> failwith "percent_compare"))
external inet_addr_of_string :
  string -> Unix.inet_addr = "unix_inet_addr_of_string"
let unix_inet_addr_of_string =
  mk "unix_inet_addr_of_string"
    (function
     | env ->
         (function
          | (String s)::[] ->
              String (Obj.magic (inet_addr_of_string (Bytes.to_string s)))
          | _ -> failwith "unix_inet_addr_of_string"))
let caml_sys_get_argv =
  mk "caml_sys_get_argv"
    (function
     | env ->
         (function
          | (Unit)::[] ->
              Tuple
                [String (!exe);
                Array (Array.map (fun x -> String x) (!argv))]
          | _ -> failwith "caml_sys_get_argv"))
external get_config : unit -> (string * int * bool) = "caml_sys_get_config"
let caml_sys_get_config =
  mk "caml_sys_get_config"
    (function
     | env ->
         (function
          | (Unit)::[] ->
              let (s, i, b) = get_config () in
              Tuple [String (Bytes.of_string s); Int i; Bool b]
          | _ -> failwith "caml_sys_get_config"))
let caml_obj_tag =
  mk "caml_obj_tag" (function | env -> (function | _ -> Int 0))
let percent_obj_field =
  mk2 "%obj_field"
    (function
     | env ->
         (function | e::(Int i)::[] -> e | _ -> failwith "percent_obj_field"))
let caml_obj_block =
  mk2 "caml_obj_block"
    (function
     | env ->
         (function
          | (Int a)::(Int b)::[] -> Int 0
          | _ -> failwith "caml_obj_block"))
let caml_make_vect =
  mk2 "caml_make_vect"
    (function
     | env ->
         (function
          | (Int len)::x::[] -> Array (Array.make len x)
          | _ -> failwith "caml_make_vect"))
external unsafe_fill :
  bytes -> int -> int -> char -> unit = "caml_fill_string"[@@noalloc ]
let caml_fill_string =
  mk4 "caml_fill_string"
    (function
     | env ->
         (function
          | (String b)::(Int x)::(Int y)::(Char c)::[] ->
              (unsafe_fill b x y c; Unit)
          | _ -> failwith "caml_fill_string"))
type 'a w
external caml_weak_create : int -> 'a w = "caml_weak_create"
let caml_weak_create =
  mk "caml_weak_create"
    (function
     | env ->
         (function | (Int i)::[] -> Unit | _ -> failwith "caml_weak_create"))
external getenv : string -> string = "caml_sys_getenv"
let caml_sys_getenv =
  mk "caml_sys_getenv"
    (function
     | env ->
         (function
          | (String s)::[] ->
              (try String (Bytes.of_string (getenv (Bytes.to_string s)))
               with | Not_found -> Raise ("Not_found", None))
          | _ -> failwith "caml_sys_getenv"))
external random_seed : unit -> int array = "caml_sys_random_seed"
let caml_sys_random_seed =
  mk "caml_sys_random_seed"
    (function
     | env ->
         (function
          | (Unit)::[] -> Array (Array.map (fun x -> Int x) (random_seed ()))
          | _ -> failwith "caml_sys_random_seed"))
external percent_array_length : 'a array -> int = "%array_length"
let percent_array_length =
  mk "%array_length"
    (function
     | env ->
         (function
          | (Array x)::[] -> Int (Array.length x)
          | _ -> failwith "percent_array_length"))
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
let percent_array_safe_set =
  mk3 "%array_safe_set"
    (function
     | env ->
         (function
          | (Array x)::(Int i)::v::[] -> (set x i v; Unit)
          | _ -> failwith "percent_array_safe_set"))
let thread_initialize =
  mk "thread_initialize" (function | env -> (function | _ -> Unit))
let unix_gettimeofday =
  mk "unix_gettimeofday"
    (function
     | env ->
         (function
          | (Unit)::[] -> Float (Unix.gettimeofday ())
          | _ -> failwith "unix_gettimeofday"))
external percent_string_safe_get : string -> int -> char = "%string_safe_get"
let percent_string_safe_get =
  mk2 "%string_safe_get"
    (function
     | env ->
         (function
          | (String s)::(Int i)::[] ->
              (try Char (Bytes.get s i)
               with
               | _ ->
                   Raise
                     ("Invalid_argument",
                       (Some (String (Bytes.of_string "index out of bounds")))))
          | _ -> failwith "percent_string_safe_get"))
let unix_fork =
  mk "unix_fork"
    (function
     | env ->
         (function
          | (Unit)::[] -> Int (Unix.fork ())
          | _ -> failwith "unix_fork"))
external percent_greaterthan : 'a -> 'a -> bool = "%greaterthan"
external percent_greaterequal : 'a -> 'a -> bool = "%greaterequal"
external percent_lessthan : 'a -> 'a -> bool = "%lessthan"
external percent_lessequal : 'a -> 'a -> bool = "%lessequal"
external percent_notequal : 'a -> 'a -> bool = "%notequal"
external percent_equal : 'a -> 'a -> bool = "%equal"
let percent_greaterthan =
  mk2 "%greaterthan"
    (function
     | env ->
         (function
          | x::y::[] -> Bool (x > y)
          | _ -> failwith "percent_greaterthan"))
let percent_greaterequal =
  mk2 "%greaterequal"
    (function
     | env ->
         (function
          | x::y::[] -> Bool (x >= y)
          | _ -> failwith "percent_greaterequal"))
let percent_lessthan =
  mk2 "%lessthan"
    (function
     | env ->
         (function
          | x::y::[] -> Bool (x < y)
          | _ -> failwith "percent_lessthan"))
let percent_lessequal =
  mk2 "%lessequal"
    (function
     | env ->
         (function
          | x::y::[] -> Bool (x <= y)
          | _ -> failwith "percent_lessequal"))
let percent_notequal =
  mk2 "%notequal"
    (function
     | env ->
         (function
          | x::y::[] -> Bool (x <> y)
          | _ -> failwith "percent_notequal"))
let percent_equal =
  mk2 "%equal"
    (function
     | env ->
         (function | x::y::[] -> Bool (x = y) | _ -> failwith "percent_equal"))
external caml_sys_open :
  string -> open_flag list -> int -> int = "caml_sys_open"
let flag_of_string =
  function
  | "Open_rdonly" -> Open_rdonly
  | "Open_wronly" -> Open_wronly
  | "Open_append" -> Open_append
  | "Open_creat" -> Open_creat
  | "Open_trunc" -> Open_trunc
  | "Open_excl" -> Open_excl
  | "Open_binary" -> Open_binary
  | "Open_text" -> Open_text
  | "Open_nonblock" -> Open_nonblock
  | _ -> failwith "Ocamliprim.flag_of_string"
let rec convert_flags =
  function
  | Nil -> []
  | Cons (Constr (tag, x, None), more) -> (flag_of_string x) ::
      (convert_flags more)
  | _ -> failwith "Ocamliprim.convert_flags"
let caml_sys_open =
  mk3 "caml_sys_open"
    (function
     | env ->
         (function
          | (String filename)::flags::(Int perm)::[] ->
              Int
                (caml_sys_open (Bytes.to_string filename)
                   (convert_flags flags) perm)
          | _ -> failwith "caml_sys_open"))
external caml_ml_set_channel_name :
  out_channel -> string -> int = "caml_ml_set_channel_name"
external caml_ml_set_channel_name' :
  in_channel -> string -> int = "caml_ml_set_channel_name"
let caml_ml_set_channel_name =
  mk2 "caml_ml_set_channel_name"
    (function
     | env ->
         (function
          | (OutChannel x)::(String y)::[] ->
              Int (caml_ml_set_channel_name x (Bytes.to_string y))
          | (InChannel x)::(String y)::[] ->
              Int (caml_ml_set_channel_name' x (Bytes.to_string y))
          | x -> failwith "caml_ml_set_channel_name"))
external caml_ml_close_channel :
  out_channel -> unit = "caml_ml_close_channel"
external caml_ml_close_channel' :
  in_channel -> unit = "caml_ml_close_channel"
let caml_ml_close_channel =
  mk "caml_ml_close_channel"
    (function
     | env ->
         (function
          | (OutChannel i)::[] -> (caml_ml_close_channel i; Unit)
          | (InChannel i)::[] -> (caml_ml_close_channel' i; Unit)
          | x -> failwith "caml_ml_close_channel"))
external percent_bytes_of_string : string -> bytes = "%bytes_of_string"
let percent_bytes_of_string =
  mk "%bytes_of_string"
    (function
     | env ->
         (function
          | (String x)::[] -> String x
          | _ -> failwith "percent_bytes_of_string"))
external caml_blit_bytes :
  bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes"
let caml_blit_bytes =
  mk5 "caml_blit_bytes"
    (function
     | env ->
         (function
          | (String x)::(Int a)::(String y)::(Int b)::(Int c)::[] ->
              (caml_blit_bytes (Obj.magic x : bytes) a (Obj.magic y : 
                 bytes) b c;
               Unit)
          | _ -> failwith "caml_blit_bytes"))
external caml_ml_out_channels_list :
  unit -> out_channel list = "caml_ml_out_channels_list"
let rec make_tinyocaml_list =
  function | [] -> Nil | h::t -> Cons (h, (make_tinyocaml_list t))
let caml_ml_out_channels_list =
  mk "caml_ml_out_channels_list"
    (function
     | env ->
         (function
          | (Unit)::[] ->
              make_tinyocaml_list
                (List.map (fun x -> OutChannel x)
                   (caml_ml_out_channels_list ()))
          | _ -> failwith "caml_ml_out_channels_list"))
let builtin_primitives_common =
  [caml_sys_exit;
  caml_ml_out_channels_list;
  caml_blit_bytes;
  caml_ml_close_channel;
  caml_ml_set_channel_name;
  caml_sys_open;
  caml_ml_flush;
  caml_ml_output_char;
  caml_md5_string;
  caml_blit_string;
  caml_sys_random_seed;
  caml_sys_getenv;
  caml_weak_create;
  caml_fill_string;
  caml_make_vect;
  caml_obj_block;
  caml_obj_tag;
  caml_ml_open_descriptor_in;
  caml_ml_open_descriptor_out;
  caml_sys_get_argv;
  caml_sys_get_config;
  caml_register_named_value;
  caml_ml_output;
  caml_ml_input;
  caml_format_int;
  caml_ml_input_scan_line;
  caml_ml_input_char;
  caml_create_bytes;
  caml_int64_float_of_bits;
  caml_sqrt_float;
  caml_ceil_float;
  caml_floor_float;
  caml_sin_float;
  unix_fork;
  unix_gettimeofday;
  unix_inet_addr_of_string;
  thread_initialize;
  percent_bytes_length;
  percent_bytes_of_string;
  percent_intoffloat;
  percent_negfloat;
  percent_addfloat;
  percent_subfloat;
  percent_divfloat;
  percent_mulfloat;
  percent_greaterthan;
  percent_greaterequal;
  percent_lessthan;
  percent_lessequal;
  percent_notequal;
  percent_equal;
  percent_andint;
  percent_lslint;
  percent_xorint;
  percent_string_unsafe_get;
  percent_string_safe_get;
  percent_bytes_to_string;
  percent_backend_type;
  percent_modint;
  percent_array_safe_set;
  percent_array_length;
  percent_string_safe_get;
  percent_string_safe_set;
  percent_nativeint_sub;
  percent_nativeint_lsl;
  percent_obj_field;
  percent_max_wosize;
  percent_word_size;
  percent_int_size;
  percent_big_endian;
  percent_identity;
  percent_ignore;
  percent_string_length;
  percent_raise;
  percent_raise_notrace;
  percent_apply;
  percent_revapply;
  percent_asrint;
  percent_makemutable;
  percent_field0;
  percent_setfield0;
  percent_compare;
  percent_addint;
  percent_ostype_unix;
  percent_ostype_win32;
  percent_ostype_cygwin;
  percent_lsrint]
let builtin_primitives =
  [percent_array_safe_get;
  percent_boolnot;
  percent_negfloat;
  caml_int_of_string;
  caml_create_string] @ builtin_primitives_common
let builtin_primitives_emulated =
  [emulated_percent_array_safe_get;
  emulated_percent_boolnot;
  emulated_percent_negfloat;
  emulated_caml_int_of_string;
  emulated_caml_create_string] @ builtin_primitives_common
let rec final_type t =
  match t.ptyp_desc with
  | Ptyp_arrow (_, _, next) -> final_type next
  | _ -> t
let our_type =
  function
  | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident "char" }, _) } ->
      Some TypChar
  | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident "int" }, _) } ->
      Some TypInt
  | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident x }, _) } ->
      (if !debug then Printf.printf "missing our_type %s\n" x; None)
  | _ -> (if !debug then Printf.printf "missing our_type2\n"; None)
let rec really_add_type typ =
  function
  | Fun (x, y, z, e) -> Fun (x, y, (really_add_type typ z), e)
  | CallBuiltIn (_, a, b, c) -> CallBuiltIn (typ, a, b, c)
  | _ -> failwith "unexpecetd: really_add_type"
let add_type typ (name, implementation) =
  match typ with
  | None -> implementation
  | Some typ ->
      if name = "%identity"
      then really_add_type (our_type (final_type typ)) implementation
      else implementation
let lookup_primitive ?typ  n =
  let prims =
    if !emulated then builtin_primitives_emulated else builtin_primitives in
  try add_type typ (n, (List.assoc n prims))
  with
  | Not_found ->
      snd
        (mk n
           (function
            | _ ->
                (function
                 | e::[] -> Raise (("UnknownPrimitive: " ^ n), None)
                 | _ -> failwith "unknown unknown primitive")))
