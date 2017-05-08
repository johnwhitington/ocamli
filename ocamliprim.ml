open Tinyocaml
open Ocamliutil
open Parsetree

(* We write, for example:

[%%auto {|external word_size : unit -> int = "%word_size"|}]

Which generates:

external word_size : unit -> int = "%word_size"

let percent_word_size =
  let f =
    (function [Unit] -> Int (word_size ())
     | _ -> failwith "%word_size")
  in
    ("%word_size", Fun (NoLabel, PatVar "*x", CallBuiltIn (None, "%word_size", [Var "*x"], f), []))
*)

[%%auto {|external word_size : unit -> int = "%word_size"|}]
[%%auto {|external int_size : unit -> int = "%int_size"|}]
[%%auto {|external max_wosize : unit -> int = "%max_wosize"|}]
[%%auto {|external big_endian : unit -> bool = "%big_endian"|}]
[%%auto {|external unix : unit -> bool = "%ostype_unix"|}]
[%%auto {|external win32 : unit -> bool = "%ostype_win32"|}]
[%%auto {|external cygwin : unit -> bool = "%ostype_cygwin"|}]
[%%auto {|external percent_boolnot : bool -> bool = "%boolnot"|}]
[%%auto {|external percent_negint : int -> int = "%negint"|}]
[%%auto {|external caml_ml_input_scan_line : in_channel -> int = "caml_ml_input_scan_line"|}]
[%%auto {|external caml_create_string : int -> string = "caml_create_string"|}]
[%%auto {|external caml_create_bytes : int -> bytes = "caml_create_bytes"|}]
[%%auto {|external caml_int64_float_of_bits : int64 -> float = "caml_int64_float_of_bits"|}]
[%%auto {|external ignore : 'a -> unit = "%ignore"|}]
[%%auto {|external caml_ml_input_char : in_channel -> char = "caml_ml_input_char"|}]
[%%auto {|external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"|}]
[%%auto {|external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"|}]
[%%auto {|external caml_ba_init : unit -> unit = "caml_ba_init"|}]
[%%auto {|external caml_ml_flush : out_channel -> unit = "caml_ml_flush"|}]
[%%auto {|external caml_sqrt_float : float -> float = "caml_sqrt_float"|}]
[%%auto {|external caml_ceil_float : float -> float = "caml_ceil_float"|}]
[%%auto {|external caml_floor_float : float -> float = "caml_floor_float"|}]
[%%auto {|external percent_negfloat : float -> float = "%negfloat"|}]
[%%auto {|external percent_intoffloat : float -> int = "%intoffloat"|}]
[%%auto {|external caml_sin_float : float -> float = "caml_sin_float"|}]

let exe = ref ""
let argv = ref [||]

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

let percent_backend_type =
  mk "%backend_type"
    (function
     | [Unit] -> Constr ("Other", Some (String "ocamli"))
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



let percent_identity =
  mk "%identity"
    (function [x] -> x
     | _ -> failwith "percent_identity")


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

(*external get_argv: unit -> string * string array = "caml_sys_get_argv"*)

let caml_sys_get_argv =
  mk "caml_sys_get_argv"
    (function [Unit] ->
       Tuple [String !exe; Array (Array.map (fun x -> String x) !argv)]
     | _ -> failwith "caml_sys_get_argv")

external get_config: unit -> string * int * bool = "caml_sys_get_config"

let caml_sys_get_config =
  mk "caml_sys_get_config"
    (function [Unit] ->
       let s, i, b = get_config () in
         Tuple [String s; Int i; Bool b]
     | _ -> failwith "caml_sys_get_config")
 

let percent_lsrint =
  mk2 "%lsrint"
    (function [Int a; Int b] -> Int (a lsr b)
     | l -> failwith "percent_lsrint")




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

external percent_divfloat : float -> float -> float = "%divfloat"
external percent_mulfloat : float -> float -> float = "%mulfloat"
external percent_subloat : float -> float -> float = "%subfloat"
external percent_addfloat : float -> float -> float = "%addfloat"

let percent_divfloat =
  mk2 "%divfloat"
    (function [Float x; Float y] -> Float (x /. y)
     | _ -> failwith "divfloat")

let percent_mulfloat =
  mk2 "%mulfloat"
    (function [Float x; Float y] -> Float (x *. y)
     | _ -> failwith "mulfloat")

let percent_subfloat =
  mk2 "%subfloat"
    (function [Float x; Float y] -> Float (x -. y)
     | _ -> failwith "subfloat")

let percent_addfloat =
  mk2 "%addfloat"
    (function [Float x; Float y] -> Float (x +. y)
     | _ -> failwith "addfloat")

external caml_ml_output_char : out_channel -> char -> unit = "caml_ml_output_char"

let caml_ml_output_char =
  mk2 "caml_ml_output_char"
    (function [OutChannel ch; Char c] ->
       caml_ml_output_char ch c; Unit
     | _ -> failwith "caml_ml_output_char")




(* These convert the result of caml_sys_open to an in_channel or out_channel *)
(*external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"*)

external caml_sys_open : string -> open_flag list -> int -> int = "caml_sys_open"

(* Convert from Cons (Constr ("Open_rdonly", Nil)) ---> [Open_rdonly] *)
let flag_of_string = function
  "Open_rdonly" -> Open_rdonly
| "Open_wronly" -> Open_wronly
| "Open_append" -> Open_append
| "Open_creat" -> Open_creat
| "Open_trunc" -> Open_trunc
| "Open_excl" -> Open_excl
| "Open_binary" -> Open_binary
| "Open_text" -> Open_text
| "Open_nonblock" -> Open_nonblock
| _ -> failwith "Ocamliprim.flag_of_string"

let rec convert_flags = function
  | Nil -> []
  | Cons (Constr (x, None), more) -> flag_of_string x::convert_flags more
  | _ -> failwith "Ocamliprim.convert_flags"

let caml_sys_open =
  mk3 "caml_sys_open"
    (function [String filename; flags; Int perm] ->
       Int (caml_sys_open filename (convert_flags flags) perm)
     | _ -> failwith "caml_sys_open")

external caml_ml_set_channel_name : out_channel -> string -> int = "caml_ml_set_channel_name"
external caml_ml_set_channel_name' : in_channel -> string -> int = "caml_ml_set_channel_name"

let debug_args =
  List.iter (fun x -> print_string (Tinyocaml.to_string x); print_string "\n")

let caml_ml_set_channel_name =
  mk2 "caml_ml_set_channel_name"
    (function
       [OutChannel x; String y] -> Int (caml_ml_set_channel_name x y)
     | [InChannel x; String y] -> Int (caml_ml_set_channel_name' x y)
     | x -> debug_args x; failwith "caml_ml_set_channel_name")

external caml_ml_close_channel : out_channel -> unit = "caml_ml_close_channel"
external caml_ml_close_channel' : in_channel -> unit = "caml_ml_close_channel"

let caml_ml_close_channel =
  mk "caml_ml_close_channel"
    (function
       [OutChannel i] -> caml_ml_close_channel i; Unit
     | [InChannel i] -> caml_ml_close_channel' i; Unit
     | x -> debug_args x; failwith "caml_ml_close_channel")



external percent_bytes_of_string : string -> bytes = "%bytes_of_string"

let percent_bytes_of_string =
  mk "%bytes_of_string"
    (function [String x] -> String x
     | _ -> failwith "percent_bytes_of_string")

external bytes_length : bytes -> int = "%bytes_length"

let percent_bytes_length =
  mk "%bytes_length"
    (function [String x] -> Int (String.length x)
     | _ -> failwith "percent_bytes_length")

external caml_blit_bytes : bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes"

let caml_blit_bytes =
  mk5 "caml_blit_bytes"
    (function [String x; Int a; String y; Int b; Int c] ->
       caml_blit_bytes (Obj.magic x : bytes) a (Obj.magic y : bytes) b c;
       Unit
     | _ -> failwith "caml_blit_bytes")

external caml_ml_out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"

let rec make_tinyocaml_list = function
  [] -> Nil
| h::t -> Cons (h, make_tinyocaml_list t)

let caml_ml_out_channels_list =
  mk "caml_ml_out_channels_list"
    (function [Unit] -> make_tinyocaml_list (List.map (fun x -> OutChannel x) (caml_ml_out_channels_list ()))
     | _ -> failwith "caml_ml_out_channels_list")

external caml_sys_exit : int -> 'a = "caml_sys_exit"

let caml_sys_exit =
  mk "caml_sys_exit"
    (function [Int i] -> caml_sys_exit i
     | _ -> failwith "caml_sys_exit")

let builtin_primitives = [
  caml_sys_exit;
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

