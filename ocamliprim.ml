open Tinyocaml
open Ocamliutil
open Parsetree

let debug = ref false

let mk name f =
  (name, Fun (NoLabel, PatVar "*x", CallBuiltIn (None, name, [Var "*x"], f), []))

let mk2 name f =
  (name,
   Fun (NoLabel, PatVar "*x",
     Fun (NoLabel, PatVar "*y", CallBuiltIn (None, name, [Var "*x"; Var "*y"], f), []), []))

let mk3 name f =
  (name,
   Fun (NoLabel, PatVar "*x",
     Fun (NoLabel, PatVar "*y",
       Fun (NoLabel, PatVar "*z", CallBuiltIn (None, name, [Var "*x"; Var "*y"; Var "*z"], f), []), []), []))

let mk4 ?(x1="x") ?(x2="y") ?(x3="z") ?(x4="q") name f =
   (name,
     Fun (NoLabel, PatVar (star x1),
       Fun (NoLabel, PatVar (star x2),
         Fun (NoLabel, PatVar (star x3),
           Fun (NoLabel, PatVar (star x4),
             CallBuiltIn (None, name, [Var (star x1); Var (star x2); Var (star x3); Var (star x4)], f), []), []), []), []))

let mk5 ?(x1="x") ?(x2="y") ?(x3="z") ?(x4="q") ?(x5="p") name f =
   (name,
     Fun (NoLabel, PatVar (star x1),
       Fun (NoLabel, PatVar (star x2),
         Fun (NoLabel, PatVar (star x3),
           Fun (NoLabel, PatVar (star x4),
             Fun (NoLabel, PatVar (star x5),
               CallBuiltIn (None, name, [Var (star x1); Var (star x2); Var (star x3); Var (star x4); Var (star x5)], f), []), []), []), []), []))

let caml_register_named_value =
  mk2 "caml_register_named_value"
    (function [String name; func] -> Unit | _ -> failwith "builtin_caml_register_value")

external unsafe_output_string : out_channel -> string -> int -> int -> unit
                              = "caml_ml_output"

external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

let caml_ml_input_scan_line =
  mk "caml_ml_input_scan_line"
    (function [InChannel i] -> Int (input_scan_line i)
     | _ -> failwith "caml_ml_input_scan_line")


external string_create : int -> string = "caml_create_string"

let caml_create_string =
  mk "caml_create_string"
    (function [Int i] -> String (string_create i)
     | _ -> failwith "caml_create_string")

external bytes_create : int -> bytes = "caml_create_bytes"

let caml_create_bytes =
  mk "caml_create_bytes"
    (function [Int i] -> String (string_create i)
     | _ -> failwith "caml_create_bytes")

external bytes_to_string : bytes -> string = "%bytes_to_string"

let percent_bytes_to_string =
  mk "%bytes_to_string"
    (function [String x] -> String (String.copy x)
     | _ -> failwith "%bytes_to_string")

external unsafe_input : in_channel -> string -> int -> int -> int
                      = "caml_ml_input"

let caml_ml_input =
  mk4 "caml_ml_input"
    (function [InChannel i; String s; Int o; Int l] -> Int (unsafe_input i s o l)
     | _ -> failwith "caml_ml_input")

let caml_ml_output =
  mk4 ~x1:"o" ~x2:"s" ~x3:"p" ~x4:"l" "caml_ml_output"
    (function
      [OutChannel o; String s; Int p; Int l] -> unsafe_output_string o s p l; Unit
    | _ -> failwith "caml_ml_output")

external format_int : string -> int -> string = "caml_format_int"

let caml_format_int =
  mk2 "caml_format_int"
    (function
     | [String s; Int i] -> String (format_int s i)
     | _ -> failwith "caml_format_int")

let percent_string_length =
  mk "%string_length"
    (function
     | [String e] -> Int (String.length e)
     | _ -> failwith "percent_string_length")

external get_backend_type : unit -> Sys.backend_type = "%backend_type"
    
let percent_backend_type =
  mk "%backend_type"
    (function
     | [Unit] -> Int 0 (* FIXME *)
     | _ -> failwith "percent_backend_type")

let percent_raise =
  mk "%raise"
    (function
     | [Constr (n, eopt)] -> Raise (n, eopt)
     | _ -> failwith "percent_raise")

let percent_raise_notrace =
  mk "%raise_notrace"
    (function [e] -> Raise ("FixPercentRaiseNotrace", None) | _ -> failwith "percent_raise_notrace")

let percent_apply =
  mk2 "%apply"
    (function [f; a] -> App (f, a)
     | _ -> failwith "percent_apply")

let percent_revapply =
  mk2 "%revapply"
    (function [a; f] -> App (f, a)
     | _ -> failwith "percent_revapply")

let percent_asrint =
  mk2 "%asrint"
    (function [Int x; Int y] -> Int (x asr y)
     | _ -> failwith "percent_asrint")

let percent_makemutable =
  mk "%makemutable"
    (function [e] -> Record [("contents", ref e)]
     | _ -> failwith "percent_makemutable") 

let percent_field0 =
  mk "%field0"
    (function [Record [(_, {contents = e})]] -> e
     | _ -> failwith "percent_field0")

let percent_setfield0 =
  mk2 "%setfield0"
    (function
      [Record [(_, r)]; e] -> r := e; Unit
     | _ -> failwith "percent_setfield0")

let percent_compare =
  mk2 "%compare"
    (function [a; b] -> Int (compare a b)
     | _ -> failwith "percent_compare") (* Obviously not *) 

let percent_addint =
  mk2 "%addint"
    (function [Int a; Int b] -> Int (a + b)
     | _ -> failwith "percent_addint")

let percent_array_safe_get =
  mk2 "%array_safe_get"
    (function [Array x; Int i] -> x.(i)
     | _ -> failwith "percent_array_safe_get")

external ignore : 'a -> unit = "%ignore"

let percent_ignore =
  mk "%ignore"
    (function [_] -> Unit
     | _ -> failwith "percent_ignore")

let percent_identity =
  mk "%identity"
    (function [x] -> x
     | _ -> failwith "percent_identity")

external input_char : in_channel -> char = "caml_ml_input_char"

let caml_ml_input_char =
  mk "caml_ml_input_char"
    (function [InChannel i] -> Char (input_char i)
     | _ -> failwith "caml_ml_input_char")

external inet_addr_of_string : string -> Unix.inet_addr
                                    = "unix_inet_addr_of_string"

(* FIXME This Obj.magic stuff - can we avoid it? *)
let unix_inet_addr_of_string =
  mk "unix_inet_addr_of_string"
    (function [String s] -> String (Obj.magic (inet_addr_of_string s))
     | _ -> failwith "unix_inet_addr_of_string")

let caml_int_of_string =
  mk "caml_int_of_string"
    (function [String s] -> Int (int_of_string s)
     | _ -> failwith "caml_int_of_string")

external get_argv: unit -> string * string array = "caml_sys_get_argv"

let caml_sys_get_argv =
  mk "caml_sys_get_argv"
    (function [Unit] ->
       let exe, args = get_argv () in
         Tuple [String exe; Array (Array.map (fun x -> String x) args)]
     | _ -> failwith "caml_sys_get_argv")

external get_config: unit -> string * int * bool = "caml_sys_get_config"

let caml_sys_get_config =
  mk "caml_sys_get_config"
    (function [Unit] ->
       let s, i, b = get_config () in
         Tuple [String s; Int i; Bool b]
     | _ -> failwith "caml_sys_get_config")
 
external big_endian : unit -> bool = "%big_endian"

let percent_big_endian =
  mk "%big_endian"
    (function [Unit] -> Bool (big_endian ())
     | _ -> failwith "percent_big_endian")

external word_size : unit -> int = "%word_size"

let percent_word_size =
  mk "%word_size"
    (function [Unit] -> Int (word_size ())
     | _ -> failwith "percent_word_size")

external int_size : unit -> int = "%int_size"

let percent_int_size =
  mk "%int_size"
    (function [Unit] -> Int (int_size ())
     | _ -> failwith "percent_int_size")

external unix : unit -> bool = "%ostype_unix"
external win32 : unit -> bool = "%ostype_win32"
external cygwin : unit -> bool = "%ostype_cygwin"

let percent_ostype_unix =
  mk "%ostype_unix"
    (function [Unit] -> Bool (unix ())
     | _ -> failwith "percent_ostype_unix")

let percent_ostype_win32 =
  mk "%ostype_win32"
    (function [Unit] -> Bool (win32 ())
     | _ -> failwith "percent_ostype_win32")

let percent_ostype_cygwin =
  mk "%ostype_cygwin"
    (function [Unit] -> Bool (cygwin ())
     | _ -> failwith "percent_ostype_cygwin")

external max_wosize : unit -> int = "%max_wosize"

let percent_max_wosize =
  mk "%max_wosize"
    (function [Unit] -> Int (max_wosize ())
     | _ -> failwith "percent_max_wosize")

let percent_lsrint =
  mk2 "%lsrint"
    (function [Int a; Int b] -> Int (a lsr b)
     | l -> failwith "percent_lsrint")

external float_of_bits : int64 -> float = "caml_int64_float_of_bits"

let caml_int64_float_of_bits =
  mk "caml_int64_float_of_bits"
    (function [Int64 i] -> Float (float_of_bits i)
     | _ -> failwith "caml_int64_float_of_bits")

external open_descriptor_out : int -> out_channel
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"

let caml_ml_open_descriptor_out =
  mk "caml_ml_open_descriptor_out"
    (function [Int i] -> OutChannel (open_descriptor_out i)
     | _ -> failwith "caml_ml_open_descriptor_out")

let caml_ml_open_descriptor_in =
  mk "caml_ml_open_descriptor_in"
    (function [Int i] -> InChannel (open_descriptor_in i)
     | _ -> failwith "caml_ml_open_descriptor_in")

let caml_obj_tag =
  mk "caml_obj_tag"
    (function _ -> Int 0)

let percent_obj_field =
  mk2 "%obj_field"
    (function [e; Int i] -> e
     | _ -> failwith "percent_obj_field") 

let caml_obj_block =
  mk2 "caml_obj_block"
    (function [Int a; Int b] -> Int 0
     | _ -> failwith "caml_obj_block")

external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"

let percent_nativeint_lsl =
  mk2 "%nativeint_lsl"
    (function [NativeInt a; Int b] -> NativeInt (shift_left a b)
     | _ -> failwith "percent_nativeint_lsl")

let percent_nativeint_sub =
  mk2 "%nativeint_sub"
    (function [NativeInt a; NativeInt b] -> NativeInt (sub a b)
     | _ -> failwith "percent_nativeint_sub")

let caml_make_vect =
  mk2 "caml_make_vect"
    (function [Int len; x] -> Array (Array.make len x)
     | _ -> failwith "caml_make_vect")

external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_string" [@@noalloc]

let caml_fill_string =
  mk4 "caml_fill_string"
    (function
       [String b; Int x; Int y; Char c] -> unsafe_fill (Bytes.unsafe_of_string b) x y c; Unit
     | _ -> failwith "caml_fill_string")

external caml_ba_init : unit -> unit = "caml_ba_init"

let caml_ba_init =
  mk "caml_ba_init"
    (function
        [Unit] -> caml_ba_init (); Unit
      | _ -> failwith "caml_ba_init")

type 'a w

external caml_weak_create : int -> 'a w = "caml_weak_create"

let caml_weak_create =
  mk "caml_weak_create"
    (function
        [Int i] -> Unit (* FIXME*)
      | _ -> failwith "caml_weak_create")

external string_safe_get : string -> int -> char = "%string_safe_get"

external string_safe_set : string -> int -> char -> unit = "%string_safe_set"

let percent_string_safe_get =
  mk2 "%string_safe_get"
    (function
       [String s; Int i] -> Char (s.[i])
     | _ -> failwith "percent_string_safe_get")

let percent_string_safe_set =
  mk3 "%string_safe_set"
    (function
       [String s; Int i; Char c] -> ignore (string_safe_set s i c); Unit
     | _ -> failwith "percent_string_safe_set")

external getenv: string -> string = "caml_sys_getenv"

let caml_sys_getenv =
  mk "caml_sys_getenv"
    (function [String s] ->
        begin try String (getenv s) with
          Not_found -> Raise ("Not_found", None) 
        end
     | _ -> failwith "caml_sys_getenv")

external random_seed: unit -> int array = "caml_sys_random_seed"

let caml_sys_random_seed =
  mk "caml_sys_random_seed"
    (function [Unit] ->
        Array (Array.map (fun x -> Int x) (random_seed ()))
     | _ -> failwith "caml_sys_random_seed")

external percent_array_length: 'a array -> int = "%array_length"

let percent_array_length =
  mk "%array_length"
    (function [Array x] -> Int (Array.length x)
     | _ -> failwith "percent_array_length")

external set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let percent_array_safe_set =
  mk3 "%array_safe_set"
    (function [Array x; Int i; v] -> set x i v; Unit
     | _ -> failwith "percent_array_safe_set")

external modint: int -> int -> int = "%modint"

let percent_modint =
  mk2 "%modint"
    (function [Int x; Int y] -> Int (modint x y)
     | _ -> failwith "percent_modint")

external caml_blit_string : string -> int -> string -> int -> int -> unit =
  "caml_blit_string"

let caml_blit_string =
  mk5 "caml_blit_string"
    (function [String s; Int x; String s'; Int y; Int z] ->
       caml_blit_string s x s' y z; Unit
     | _ -> failwith "caml_blit_string")

external caml_md5_string : string -> int -> int -> string = "caml_md5_string"

let caml_md5_string =
  mk3 "caml_md5_string"
    (function [String s; Int i; Int j] ->
       String (caml_md5_string s i j)
     | _ -> failwith "caml_md5_string")

let percent_xorint =
  mk2 "%xorint"
    (function [Int i; Int j] -> Int (i lxor j)
     | _ -> failwith "percent_xorint")

let percent_lslint =
  mk2 "%lslint"
    (function [Int i; Int j] -> Int (i lsl j)
     | _ -> failwith "percent_lslint")

let percent_andint =
  mk2 "%andint"
    (function [Int i; Int j] -> Int (i land j)
     | _ -> failwith "percent_andint")

(* FIXME *)
let thread_initialize =
  mk "thread_initialize"
    (function _ -> Unit)

let unix_gettimeofday =
  mk "unix_gettimeofday"
    (function [Unit] -> Float (Unix.gettimeofday ())
     | _ -> failwith "unix_gettimeofday")

external percent_string_safe_get : string -> int -> char = "%string_safe_get"

let percent_string_safe_get =
  mk2 "%string_safe_get"
    (function [String s; Int i] ->
      begin try Char s.[i] with _ ->
         Raise ("Invalid_argument", Some (String "index out of bounds"))
      end
     | _ -> failwith "percent_string_safe_get")

let percent_string_unsafe_get =
  mk2 "%string_unsafe_get"
    (function [String s; Int i] -> Char s.[i]
     | _ -> failwith "percent_string_unsafe_get")

external percent_boolnot : bool -> bool = "%boolnot"

external percent_negint : int -> int = "%negint"

let percent_boolnot =
  mk "%boolnot"
    (function [Bool b] -> Bool (percent_boolnot b)
     | _ -> failwith "%boolnot")

let percent_negint =
  mk "%negint"
    (function [Int i] -> Int (percent_negint i)
     | _ -> failwith "%negint")

let unix_fork =
  mk "unix_fork"
    (function [Unit] -> Int (Unix.fork ())
     | _ -> failwith "unix_fork")

external percent_greaterthan : 'a -> 'a -> bool = "%greaterthan"
external percent_greaterequal : 'a -> 'a -> bool = "%greaterequal"
external percent_lessthan : 'a -> 'a -> bool = "%lessthan"
external percent_lessequal : 'a -> 'a -> bool = "%lessequal"
external percent_notequal : 'a -> 'a -> bool = "%notequal"
external percent_equal : 'a -> 'a -> bool = "%equal"

let percent_greaterthan =
  mk2 "%greaterthan"
    (function [x; y] -> Bool (x > y)
     | _ -> failwith "percent_greaterthan")

let percent_greaterequal =
  mk2 "%greaterequal"
    (function [x; y] -> Bool (x >= y)
     | _ -> failwith "percent_greaterequal")

let percent_lessthan =
  mk2 "%lessthan"
    (function [x; y] -> Bool (x < y)
     | _ -> failwith "percent_lessthan")

let percent_lessequal =
  mk2 "%lessequal"
    (function [x; y] -> Bool (x <= y)
     | _ -> failwith "percent_lessequal")

let percent_notequal =
  mk2 "%notequal"
    (function [x; y] -> Bool (x <> y)
     | _ -> failwith "percent_notequal")

let percent_equal =
  mk2 "%equal"
    (function [x; y] -> Bool (x = y)
     | _ -> failwith "percent_equal")

let builtin_primitives = [
  caml_md5_string;
  caml_blit_string;
  caml_sys_random_seed;
  caml_sys_getenv;
  caml_weak_create;
  caml_ba_init;
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
  caml_int_of_string;
  caml_create_string;
  caml_create_bytes;
  caml_int64_float_of_bits;

  unix_fork;
  unix_gettimeofday;
  unix_inet_addr_of_string;

  thread_initialize;

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
  percent_array_safe_get;
  percent_ostype_unix;
  percent_ostype_win32;
  percent_ostype_cygwin;
  percent_lsrint;
  percent_negint;
  percent_boolnot;
]

(* For %identity, we need to annotate the output type, so that eval.ml can do
the coercion at runtime. *)

(* e.g for x -> y -> z return z *)
let rec final_type t =
  match t.ptyp_desc with
    Ptyp_arrow (_, _, next) -> final_type next
  | _ -> t

(* e.g convert Ptyp_constr Int to TypInt *)
let our_type = function
  {ptyp_desc = Ptyp_constr ({txt = Longident.Lident "char"}, _)} -> Some TypChar
| {ptyp_desc = Ptyp_constr ({txt = Longident.Lident "int"}, _)} -> Some TypInt
| {ptyp_desc = Ptyp_constr ({txt = Longident.Lident x}, _)} ->
    if !debug then Printf.printf "missing our_type %s\n" x; None
| _ ->
    if !debug then Printf.printf "missing our_type2\n"; None

(* insert the type into the CallBuiltIn *)
let rec really_add_type typ = function
  Fun (x, y, z, e) -> Fun (x, y, really_add_type typ z, e)
| CallBuiltIn (_, a, b, c) -> CallBuiltIn (typ, a, b, c)
| _ -> failwith "unexpecetd: really_add_type"

let add_type typ (name, implementation) =
  if name = "%identity" then
    really_add_type (our_type (final_type typ)) implementation
  else
    implementation

let lookup_primitive typ n =
  try add_type typ (n, (List.assoc n builtin_primitives)) with
    Not_found ->
      snd
        (mk n
          (function
             [e] -> Raise ("UnknownPrimitive: " ^ n, None)
             | _ -> failwith "unknown unknown primitive"))
 
(*"%identity"
  "%ignore"
  "%field0"
  "%field1"
  "%setfield0"
  "%makeblock"
  "%makemutable"
  "%raise"
  "%reraise"
  "%raise_notrace"
  "%sequand"
  "%sequor"
  "%boolnot"
  "%big_endian"
  "%backend_type"
  "%word_size"
  "%int_size"
  "%max_wosize"
  "%ostype_unix"
  "%ostype_win32"
  "%ostype_cygwin"

  "%succint"
  "%predint"
  "%addint"
  "%subint"
  "%mulint"
  "%divint"
  "%modint"
  "%andint"
  "%orint"
  "%xorint"
  "%lslint"
  "%lsrint"
  "%asrint"
  "%eq"
  "%noteq"
  "%ltint"
  "%leint"
  "%gtint"
  "%geint"
  "%incr"
  "%decr"
  "%intoffloat"
  "%floatofint"
  "%negfloat"
  "%absfloat"
  "%addfloat"
  "%subfloat"
  "%mulfloat"
  "%divfloat"
  "%eqfloat"
  "%noteqfloat"
  "%ltfloat"
  "%lefloat"
  "%gtfloat"
  "%gefloat"
  "%string_length"
  "%string_safe_get"
  "%string_safe_set"
  "%string_unsafe_get"
  "%string_unsafe_set"
  "%array_length"
  "%array_safe_get"
  "%array_safe_set"
  "%array_unsafe_get"
  "%array_unsafe_set"
  "%obj_size"
  "%obj_field"
  "%obj_set_field"
  "%obj_is_int"
  "%lazy_force"
  "%nativeint_of_int"
  "%nativeint_to_int"
  "%nativeint_neg"
  "%nativeint_add"
  "%nativeint_sub"
  "%nativeint_mul"
  "%nativeint_div"
  "%nativeint_mod"
  "%nativeint_and"
  "%nativeint_or"
  "%nativeint_xor"
  "%nativeint_lsl"
  "%nativeint_lsr"
  "%nativeint_asr"
  "%int32_of_int"
  "%int32_to_int"
  "%int32_neg"
  "%int32_add"
  "%int32_sub"
  "%int32_mul"
  "%int32_div"
  "%int32_mod"
  "%int32_and"
  "%int32_or"
  "%int32_xor"
  "%int32_lsl"
  "%int32_lsr"
  "%int32_asr"
  "%int64_of_int"
  "%int64_to_int"
  "%int64_neg"
  "%int64_add"
  "%int64_sub"
  "%int64_mul"
  "%int64_div"
  "%int64_mod"
  "%int64_and"
  "%int64_or"
  "%int64_xor"
  "%int64_lsl"
  "%int64_lsr"
  "%int64_asr"
  "%nativeint_of_int32"
  "%nativeint_to_int32"
  "%int64_of_int32"
  "%int64_to_int32"
  "%int64_of_nativeint"
  "%int64_to_nativeint"
  "%caml_ba_ref_1"
  "%caml_ba_ref_2"
  "%caml_ba_ref_3"
  "%caml_ba_set_1"
  "%caml_ba_set_2"
  "%caml_ba_set_3"
  "%caml_ba_unsafe_ref_1"
  "%caml_ba_unsafe_ref_2"
  "%caml_ba_unsafe_ref_3"
  "%caml_ba_unsafe_set_1"
  "%caml_ba_unsafe_set_2"
  "%caml_ba_unsafe_set_3"
  "%caml_ba_dim_1"
  "%caml_ba_dim_2"
  "%caml_ba_dim_3"
  "%caml_string_get16"
  "%caml_string_get16u"
  "%caml_string_get32"
  "%caml_string_get32u"
  "%caml_string_get64"
  "%caml_string_get64u"
  "%caml_string_set16"
  "%caml_string_set16u"
  "%caml_string_set32"
  "%caml_string_set32u"
  "%caml_string_set64"
  "%caml_string_set64u"
  "%caml_bigstring_get16"
  "%caml_bigstring_get16u"
  "%caml_bigstring_get32"
  "%caml_bigstring_get32u"
  "%caml_bigstring_get64"
  "%caml_bigstring_get64u"
  "%caml_bigstring_set16"
  "%caml_bigstring_set16u"
  "%caml_bigstring_set32"
  "%caml_bigstring_set32u"
  "%caml_bigstring_set64"
  "%caml_bigstring_set64u"
  "%bswap16"
  "%bswap_int32"
  "%bswap_int64"
  "%bswap_native"
  "%int_as_pointer"
  "%opaque"
  "%equal"
  "%notequal"
  "%lessthan"
  "%greaterthan"
  "%lessequal"
  "%greaterequal"
  "%revapply"
  "%apply"
  "%loc_LOC"
  "%loc_FILE"
  "%loc_LINE"
  "%loc_POS"
  "%loc_MODULE"
  "caml_abs_float";
  "caml_acos_float";
  "caml_add_debug_info";
  "caml_add_float";
  "caml_alloc_dummy";
  "caml_alloc_dummy_float";
  "caml_alloc_dummy_function";
  "caml_array_append";
  "caml_array_blit";
  "caml_array_concat";
  "caml_array_get";
  "caml_array_get_addr";
  "caml_array_get_float";
  "caml_array_set";
  "caml_array_set_addr";
  "caml_array_set_float";
  "caml_array_sub";
  "caml_array_unsafe_get";
  "caml_array_unsafe_get_float";
  "caml_array_unsafe_set";
  "caml_array_unsafe_set_addr";
  "caml_array_unsafe_set_float";
  "caml_asin_float";
  "caml_atan2_float";
  "caml_atan_float";
  "caml_backtrace_status";
  "caml_bitvect_test";
  "caml_blit_string";
  "caml_bswap16";
  "caml_ceil_float";
  "caml_channel_descriptor";
  "caml_classify_float";
  "caml_compare";
  "caml_convert_raw_backtrace_slot";
  "caml_copysign_float";
  "caml_cos_float";
  "caml_cosh_float";
  "caml_create_string";
  "caml_div_float";
  "caml_dynlink_add_primitive";
  "caml_dynlink_close_lib";
  "caml_dynlink_get_current_libs";
  "caml_dynlink_lookup_symbol";
  "caml_dynlink_open_lib";
  "caml_ensure_stack_capacity";
  "caml_ephe_blit_data";
  "caml_ephe_blit_key";
  "caml_ephe_check_data";
  "caml_ephe_check_key";
  "caml_ephe_create";
  "caml_ephe_get_data";
  "caml_ephe_get_data_copy";
  "caml_ephe_get_key";
  "caml_ephe_get_key_copy";
  "caml_ephe_set_data";
  "caml_ephe_set_key";
  "caml_ephe_unset_data";
  "caml_ephe_unset_key";
  "caml_eq_float";
  "caml_equal";
  "caml_exp_float";
  "caml_expm1_float";
  "caml_fill_string";
  "caml_final_register";
  "caml_final_release";
  "caml_float_compare";
  "caml_float_of_int";
  "caml_float_of_string";
  "caml_floor_float";
  "caml_fmod_float";
  "caml_format_float";
  "caml_format_int";
  "caml_fresh_oo_id";
  "caml_frexp_float";
  "caml_gc_compaction";
  "caml_gc_counters";
  "caml_gc_full_major";
  "caml_gc_get";
  "caml_gc_huge_fallback_count";
  "caml_gc_major";
  "caml_gc_major_slice";
  "caml_gc_minor";
  "caml_gc_quick_stat";
  "caml_gc_set";
  "caml_gc_stat";
  "caml_ge_float";
  "caml_get_current_callstack";
  "caml_get_current_environment";
  "caml_get_exception_backtrace";
  "caml_get_exception_raw_backtrace";
  "caml_get_global_data";
  "caml_get_major_bucket";
  "caml_get_major_credit";
  "caml_get_minor_free";
  "caml_get_public_method";
  "caml_get_section_table";
  "caml_greaterequal";
  "caml_greaterthan";
  "caml_gt_float";
  "caml_hash";
  "caml_hash_univ_param";
  "caml_hexstring_of_float";
  "caml_hypot_float";
  "caml_input_value";
  "caml_input_value_from_string";
  "caml_install_signal_handler";
  "caml_int32_add";
  "caml_int32_and";
  "caml_int32_bits_of_float";
  "caml_int32_bswap";
  "caml_int32_compare";
  "caml_int32_div";
  "caml_int32_float_of_bits";
  "caml_int32_format";
  "caml_int32_mod";
  "caml_int32_mul";
  "caml_int32_neg";
  "caml_int32_of_float";
  "caml_int32_of_int";
  "caml_int32_of_string";
  "caml_int32_or";
  "caml_int32_shift_left";
  "caml_int32_shift_right";
  "caml_int32_shift_right_unsigned";
  "caml_int32_sub";
  "caml_int32_to_float";
  "caml_int32_to_int";
  "caml_int32_xor";
  "caml_int64_add";
  "caml_int64_and";
  "caml_int64_bits_of_float";
  "caml_int64_bswap";
  "caml_int64_compare";
  "caml_int64_div";
  "caml_int64_float_of_bits";
  "caml_int64_format";
  "caml_int64_mod";
  "caml_int64_mul";
  "caml_int64_neg";
  "caml_int64_of_float";
  "caml_int64_of_int";
  "caml_int64_of_int32";
  "caml_int64_of_nativeint";
  "caml_int64_of_string";
  "caml_int64_or";
  "caml_int64_shift_left";
  "caml_int64_shift_right";
  "caml_int64_shift_right_unsigned";
  "caml_int64_sub";
  "caml_int64_to_float";
  "caml_int64_to_int";
  "caml_int64_to_int32";
  "caml_int64_to_nativeint";
  "caml_int64_xor";
  "caml_int_as_pointer";
  "caml_int_compare";
  "caml_int_of_float";
  "caml_int_of_string";
  "caml_invoke_traced_function";
  "caml_lazy_follow_forward";
  "caml_lazy_make_forward";
  "caml_ldexp_float";
  "caml_le_float";
  "caml_lessequal";
  "caml_lessthan";
  "caml_lex_engine";
  "caml_log10_float";
  "caml_log1p_float";
  "caml_log_float";
  "caml_lt_float";
  "caml_make_array";
  "caml_make_float_vect";
  "caml_make_vect";
  "caml_marshal_data_size";
  "caml_md5_chan";
  "caml_md5_string";
  "caml_ml_channel_size";
  "caml_ml_channel_size_64";
  "caml_ml_close_channel";
  "caml_ml_enable_runtime_warnings";
  "caml_ml_flush";
  "caml_ml_flush_partial";
  "caml_ml_input";
  "caml_ml_input_char";
  "caml_ml_input_int";
  "caml_ml_input_scan_line";
  "caml_ml_open_descriptor_in";
  "caml_ml_open_descriptor_out";
  "caml_ml_out_channels_list";
  "caml_ml_output";
  "caml_ml_output_char";
  "caml_ml_output_int";
  "caml_ml_output_partial";
  "caml_ml_pos_in";
  "caml_ml_pos_in_64";
  "caml_ml_pos_out";
  "caml_ml_pos_out_64";
  "caml_ml_runtime_warnings_enabled";
  "caml_ml_seek_in";
  "caml_ml_seek_in_64";
  "caml_ml_seek_out";
  "caml_ml_seek_out_64";
  "caml_ml_set_binary_mode";
  "caml_ml_set_channel_name";
  "caml_ml_string_length";
  "caml_modf_float";
  "caml_mul_float";
  "caml_nativeint_add";
  "caml_nativeint_and";
  "caml_nativeint_bswap";
  "caml_nativeint_compare";
  "caml_nativeint_div";
  "caml_nativeint_format";
  "caml_nativeint_mod";
  "caml_nativeint_mul";
  "caml_nativeint_neg";
  "caml_nativeint_of_float";
  "caml_nativeint_of_int";
  "caml_nativeint_of_int32";
  "caml_nativeint_of_string";
  "caml_nativeint_or";
  "caml_nativeint_shift_left";
  "caml_nativeint_shift_right";
  "caml_nativeint_shift_right_unsigned";
  "caml_nativeint_sub";
  "caml_nativeint_to_float";
  "caml_nativeint_to_int";
  "caml_nativeint_to_int32";
  "caml_nativeint_xor";
  "caml_neg_float";
  "caml_neq_float";
  "caml_new_lex_engine";
  "caml_notequal";
  "caml_obj_add_offset";
  "caml_obj_block";
  "caml_obj_dup";
  "caml_obj_is_block";
  "caml_obj_set_tag";
  "caml_obj_tag";
  "caml_obj_truncate";
  "caml_output_value";
  "caml_output_value_to_buffer";
  "caml_output_value_to_string";
  "caml_parse_engine";
  "caml_power_float";
  "caml_realloc_global";
  "caml_record_backtrace";
  "caml_register_code_fragment";
  "caml_reify_bytecode";
  "caml_remove_debug_info";
  "caml_runtime_parameters";
  "caml_runtime_variant";
  "caml_set_oo_id";
  "caml_set_parser_trace";
  "caml_sin_float";
  "caml_sinh_float";
  "caml_sqrt_float";
  "caml_static_alloc";
  "caml_static_free";
  "caml_static_release_bytecode";
  "caml_static_resize";
  "caml_string_compare";
  "caml_string_equal";
  "caml_string_get";
  "caml_string_get16";
  "caml_string_get32";
  "caml_string_get64";
  "caml_string_greaterequal";
  "caml_string_greaterthan";
  "caml_string_lessequal";
  "caml_string_lessthan";
  "caml_string_notequal";
  "caml_string_set";
  "caml_string_set16";
  "caml_string_set32";
  "caml_string_set64";
  "caml_sub_float";
  "caml_sys_chdir";
  "caml_sys_close";
  "caml_sys_const_big_endian";
  "caml_sys_const_int_size";
  "caml_sys_const_max_wosize";
  "caml_sys_const_ostype_cygwin";
  "caml_sys_const_ostype_unix";
  "caml_sys_const_ostype_win32";
  "caml_sys_const_word_size";
  "caml_sys_exit";
  "caml_sys_file_exists";
  "caml_sys_get_argv";
  "caml_sys_get_config";
  "caml_sys_getcwd";
  "caml_sys_getenv";
  "caml_sys_is_directory";
  "caml_sys_isatty";
  "caml_sys_open";
  "caml_sys_random_seed";
  "caml_sys_read_directory";
  "caml_sys_remove";
  "caml_sys_rename";
  "caml_sys_system_command";
  "caml_sys_time";
  "caml_tan_float";
  "caml_tanh_float";
  "caml_terminfo_backup";
  "caml_terminfo_resume";
  "caml_terminfo_setup";
  "caml_terminfo_standout";
  "caml_update_dummy";
  "caml_weak_blit";
  "caml_weak_check";
  "caml_weak_create";
  "caml_weak_get";
  "caml_weak_get_copy";
  "caml_weak_set";*)
