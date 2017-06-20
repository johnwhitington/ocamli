open Tinyocaml
open Ocamliutil
open Parsetree

(* We write, for example:

[%%auto {|external word_size : unit -> int = "%word_size"|}]

Which generates:

external word_size : unit -> int = "%word_size"

let percent_word_size =
  let f =
    (function [Unit] ->
      begin try Int (word_size ()) with e -> exception_from_ocaml e end
     | _ -> failwith "%word_size")
  in
    ("%word_size", Fun (NoLabel, PatVar "*x", CallBuiltIn (None, "%word_size", [Var "*x"], f), []))
*)

(* Build a Raise () from a native OCaml exn. *)
let of_real_ocaml = ref (fun (_ : Tinyocaml.env) _ -> ([], Tinyocaml.Int 0))

let exception_from_ocaml e =
  snd (!of_real_ocaml [] (ast ("raise (" ^ Printexc.to_string e ^ ")")))

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
[%%auto {|external percent_divfloat : float -> float -> float = "%divfloat"|}]
[%%auto {|external percent_mulfloat : float -> float -> float = "%mulfloat"|}]
[%%auto {|external percent_subloat : float -> float -> float = "%subfloat"|}]
[%%auto {|external percent_addfloat : float -> float -> float = "%addfloat"|}]
[%%auto {|external caml_format_int : string -> int -> string = "caml_format_int"|}]
[%%auto {|external nativeint_sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"|}]
[%%auto {|external nativeint_lsl: nativeint -> int -> nativeint = "%nativeint_lsl"|}]
[%%auto {|external string_safe_get : string -> int -> char = "%string_safe_get"|}]
[%%auto {|external modint : int -> int -> int = "%modint"|}]
[%%auto {|external caml_ml_output_char : out_channel -> char -> unit = "caml_ml_output_char"|}]
[%%auto {|external xorint : int -> int -> int = "%xorint"|}]
[%%auto {|external lslint : int -> int -> int = "%lslint"|}]
[%%auto {|external andint : int -> int -> int = "%andint"|}]
[%%auto {|external caml_sys_exit : int -> 'a = "caml_sys_exit"|}]
[%%auto {|external string_unsafe_get : string -> int -> char = "%string_unsafe_get"|}]
[%%auto {|external string_safe_set : string -> int -> char -> unit = "%string_safe_set"|}]
[%%auto {|external caml_md5_string : string -> int -> int -> string = "caml_md5_string"|}]
[%%auto {|external unsafe_input : in_channel -> string -> int -> int -> int = "caml_ml_input"|}]
[%%auto {|external unsafe_output_string : out_channel -> string -> int -> int -> unit = "caml_ml_output"|}]
[%%auto {|external caml_blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string"|}]
[%%auto {|external lsrint : int -> int -> int = "%lsrint"|}]
[%%auto {|external string_length : string -> int = "%string_length"|}]
[%%auto {|external asrint : int -> int -> int = "%asrint"|}]
[%%auto {|external addint : int -> int -> int = "%addint"|}]
[%%auto {|external caml_int_of_string : string -> int = "caml_int_of_string"|}]
[%%auto {|external bytes_length : bytes -> int = "%bytes_length"|}]


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

(* not implemented *)
let caml_register_named_value =
  mk2 "caml_register_named_value"
    (function env -> function [String name; func] -> Unit | _ -> failwith "builtin_caml_register_value")


(* BEGINNING OF VALUE TESTS *)
external array_safe_get : 'a array -> int -> 'a = "%array_safe_get"

let percent_array_safe_get =
  mk2 "%array_safe_get"
    (function env -> function [Array x; Int i] ->
      (try
         Tinyexternal.of_ocaml_value env (array_safe_get (Tinyexternal.to_ocaml_value (Array x)) i) "int"
       with
         e -> exception_from_ocaml e)
     | _ -> failwith "percent_array_safe_get")

(* BOOL IN / OUT *)
external percent_boolnot : bool -> bool = "%boolnot"

let percent_boolnot =
  mk "%boolnot"
    (function env -> function [Bool b] ->
       begin try
         Tinyexternal.of_ocaml_value env (percent_boolnot (Tinyexternal.to_ocaml_value (Bool b))) "bool"
       with
         e -> exception_from_ocaml e
       end
     | _ -> failwith "percent_boolnot")

(* FLOAT IN / OUT *)
external percent_negfloat : float -> float = "%negfloat"

let percent_negfloat =
  mk "%negfloat"
    (function env -> function [Float f] ->
       begin try
         Tinyexternal.of_ocaml_value env (percent_negfloat (Tinyexternal.to_ocaml_value (Float f))) "float"
       with
         e -> exception_from_ocaml e
       end
     | _ -> failwith "percent_negfloat")

(* STRING IN / INT OUT *)
external caml_int_of_string : string -> int = "caml_int_of_string"

let caml_int_of_string =
  mk "caml_int_of_string"
    (function env -> function [String s] ->
       begin try
         Tinyexternal.of_ocaml_value []
           (caml_int_of_string (Tinyexternal.to_ocaml_value (String s)))
           "int"
       with 
         e -> exception_from_ocaml e
       end
     | _ -> failwith "caml_int_of_string")

(* INT IN / STRING OUT *)
external caml_create_string : int -> string = "caml_create_string"

let caml_create_string =
  mk "caml_create_string"
    (function env -> function [Int i] ->
       begin try
         Tinyexternal.of_ocaml_value []
           (caml_create_string (Tinyexternal.to_ocaml_value (Int i)))
           "string"
       with
         e -> exception_from_ocaml e
       end
     | _ -> failwith "caml_create_string")

(* UNIT IN / UNIT OUT *)
external caml_ba_init : unit -> unit = "caml_ba_init"

let caml_ba_init =
  mk "caml_ba_init"
    (function env -> function [Unit] ->
      Tinyexternal.of_ocaml_value [] (caml_ba_init (Tinyexternal.to_ocaml_value Unit)) "unit"
     | _ -> failwith "caml_ba_init")

(** END OF VALUE TESTS *)

(* bytes *)
external bytes_to_string : bytes -> string = "%bytes_to_string"

let percent_bytes_to_string =
  mk "%bytes_to_string"
    (function env -> function [String x] -> String (String.copy x)
     | _ -> failwith "%bytes_to_string")


let percent_backend_type =
  mk "%backend_type"
    (function env -> function
     | [Unit] -> Constr (0, "Other", Some (String "ocamli")) (* FIXME tag *)
     | _ -> failwith "percent_backend_type")

let percent_raise =
  mk "%raise"
    (function env -> function
     | [Constr (_, n, eopt)] -> Raise (n, eopt)
     | _ -> failwith "percent_raise")

let percent_raise_notrace =
  mk "%raise_notrace"
    (function env -> function [e] -> Raise ("FixPercentRaiseNotrace", None) | _ -> failwith "percent_raise_notrace")

let percent_apply =
  mk2 "%apply"
    (function env -> function [f; a] -> App (f, a)
     | _ -> failwith "percent_apply")

let percent_revapply =
  mk2 "%revapply"
    (function env -> function [a; f] -> App (f, a)
     | _ -> failwith "percent_revapply")

let percent_identity =
  mk "%identity"
    (function env -> function [x] -> x
     | _ -> failwith "percent_identity")

let percent_makemutable =
  mk "%makemutable"
    (function env -> function [e] -> Record [("contents", ref e)]
     | _ -> failwith "percent_makemutable") 

let percent_field0 =
  mk "%field0"
    (function env -> function [Record [(_, {contents = e})]] -> e
     | _ -> failwith "percent_field0")

let percent_setfield0 =
  mk2 "%setfield0"
    (function env -> function
      [Record [(_, r)]; e] -> r := e; Unit
     | _ -> failwith "percent_setfield0")

let percent_compare =
  mk2 "%compare"
    (function env -> function [a; b] -> Int (compare a b)
     | _ -> failwith "percent_compare") (* Obviously not *) 



external inet_addr_of_string : string -> Unix.inet_addr
                                    = "unix_inet_addr_of_string"

(* FIXME This Obj.magic stuff - can we avoid it? *)
let unix_inet_addr_of_string =
  mk "unix_inet_addr_of_string"
    (function env -> function [String s] -> String (Obj.magic (inet_addr_of_string s))
     | _ -> failwith "unix_inet_addr_of_string")


(*external get_argv: unit -> string * string array = "caml_sys_get_argv"*)

let caml_sys_get_argv =
  mk "caml_sys_get_argv"
    (function env -> function [Unit] ->
       Tuple [String !exe; Array (Array.map (fun x -> String x) !argv)]
     | _ -> failwith "caml_sys_get_argv")

external get_config: unit -> string * int * bool = "caml_sys_get_config"

let caml_sys_get_config =
  mk "caml_sys_get_config"
    (function env -> function [Unit] ->
       let s, i, b = get_config () in
         Tuple [String s; Int i; Bool b]
     | _ -> failwith "caml_sys_get_config")

let caml_obj_tag =
  mk "caml_obj_tag"
    (function env -> function _ -> Int 0)

let percent_obj_field =
  mk2 "%obj_field"
    (function env -> function [e; Int i] -> e
     | _ -> failwith "percent_obj_field") 

let caml_obj_block =
  mk2 "caml_obj_block"
    (function env -> function [Int a; Int b] -> Int 0
     | _ -> failwith "caml_obj_block")

let caml_make_vect =
  mk2 "caml_make_vect"
    (function env -> function [Int len; x] -> Array (Array.make len x)
     | _ -> failwith "caml_make_vect")

external unsafe_fill : bytes -> int -> int -> char -> unit = "caml_fill_string" [@@noalloc]

let caml_fill_string =
  mk4 "caml_fill_string"
    (function env -> function
       [String b; Int x; Int y; Char c] -> unsafe_fill (Bytes.unsafe_of_string b) x y c; Unit
     | _ -> failwith "caml_fill_string")

type 'a w

external caml_weak_create : int -> 'a w = "caml_weak_create"

let caml_weak_create =
  mk "caml_weak_create"
    (function env -> function
        [Int i] -> Unit (* FIXME*)
      | _ -> failwith "caml_weak_create")

external getenv: string -> string = "caml_sys_getenv"

let caml_sys_getenv =
  mk "caml_sys_getenv"
    (function env -> function [String s] ->
        begin try String (getenv s) with
          Not_found -> Raise ("Not_found", None) 
        end
     | _ -> failwith "caml_sys_getenv")

external random_seed: unit -> int array = "caml_sys_random_seed"

let caml_sys_random_seed =
  mk "caml_sys_random_seed"
    (function env -> function [Unit] ->
        Array (Array.map (fun x -> Int x) (random_seed ()))
     | _ -> failwith "caml_sys_random_seed")

external percent_array_length: 'a array -> int = "%array_length"

let percent_array_length =
  mk "%array_length"
    (function env -> function [Array x] -> Int (Array.length x)
     | _ -> failwith "percent_array_length")

external set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let percent_array_safe_set =
  mk3 "%array_safe_set"
    (function env -> function [Array x; Int i; v] -> set x i v; Unit
     | _ -> failwith "percent_array_safe_set")

(* FIXME *)
let thread_initialize =
  mk "thread_initialize"
    (function env -> function _ -> Unit)

let unix_gettimeofday =
  mk "unix_gettimeofday"
    (function env -> function [Unit] -> Float (Unix.gettimeofday ())
     | _ -> failwith "unix_gettimeofday")

external percent_string_safe_get : string -> int -> char = "%string_safe_get"

let percent_string_safe_get =
  mk2 "%string_safe_get"
    (function env -> function [String s; Int i] ->
      begin try Char s.[i] with _ ->
         Raise ("Invalid_argument", Some (String "index out of bounds"))
      end
     | _ -> failwith "percent_string_safe_get")

let unix_fork =
  mk "unix_fork"
    (function env -> function [Unit] -> Int (Unix.fork ())
     | _ -> failwith "unix_fork")

external percent_greaterthan : 'a -> 'a -> bool = "%greaterthan"
external percent_greaterequal : 'a -> 'a -> bool = "%greaterequal"
external percent_lessthan : 'a -> 'a -> bool = "%lessthan"
external percent_lessequal : 'a -> 'a -> bool = "%lessequal"
external percent_notequal : 'a -> 'a -> bool = "%notequal"
external percent_equal : 'a -> 'a -> bool = "%equal"

let percent_greaterthan =
  mk2 "%greaterthan"
    (function env -> function [x; y] -> Bool (x > y)
     | _ -> failwith "percent_greaterthan")

let percent_greaterequal =
  mk2 "%greaterequal"
    (function env -> function [x; y] -> Bool (x >= y)
     | _ -> failwith "percent_greaterequal")

let percent_lessthan =
  mk2 "%lessthan"
    (function env -> function [x; y] -> Bool (x < y)
     | _ -> failwith "percent_lessthan")

let percent_lessequal =
  mk2 "%lessequal"
    (function env -> function [x; y] -> Bool (x <= y)
     | _ -> failwith "percent_lessequal")

let percent_notequal =
  mk2 "%notequal"
    (function env -> function [x; y] -> Bool (x <> y)
     | _ -> failwith "percent_notequal")

let percent_equal =
  mk2 "%equal"
    (function env -> function [x; y] -> Bool (x = y)
     | _ -> failwith "percent_equal")

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
  | Cons (Constr (tag, x, None), more) -> flag_of_string x::convert_flags more
  | _ -> failwith "Ocamliprim.convert_flags"

let caml_sys_open =
  mk3 "caml_sys_open"
    (function env -> function [String filename; flags; Int perm] ->
       Int (caml_sys_open filename (convert_flags flags) perm)
     | _ -> failwith "caml_sys_open")

external caml_ml_set_channel_name : out_channel -> string -> int = "caml_ml_set_channel_name"
external caml_ml_set_channel_name' : in_channel -> string -> int = "caml_ml_set_channel_name"

let caml_ml_set_channel_name =
  mk2 "caml_ml_set_channel_name"
    (function env -> function
       [OutChannel x; String y] -> Int (caml_ml_set_channel_name x y)
     | [InChannel x; String y] -> Int (caml_ml_set_channel_name' x y)
     | x -> failwith "caml_ml_set_channel_name")

external caml_ml_close_channel : out_channel -> unit = "caml_ml_close_channel"
external caml_ml_close_channel' : in_channel -> unit = "caml_ml_close_channel"

let caml_ml_close_channel =
  mk "caml_ml_close_channel"
    (function env -> function
       [OutChannel i] -> caml_ml_close_channel i; Unit
     | [InChannel i] -> caml_ml_close_channel' i; Unit
     | x -> failwith "caml_ml_close_channel")

external percent_bytes_of_string : string -> bytes = "%bytes_of_string"

let percent_bytes_of_string =
  mk "%bytes_of_string"
    (function env -> function [String x] -> String x
     | _ -> failwith "percent_bytes_of_string")


external caml_blit_bytes : bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes"

let caml_blit_bytes =
  mk5 "caml_blit_bytes"
    (function env -> function [String x; Int a; String y; Int b; Int c] ->
       caml_blit_bytes (Obj.magic x : bytes) a (Obj.magic y : bytes) b c;
       Unit
     | _ -> failwith "caml_blit_bytes")

external caml_ml_out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"

let rec make_tinyocaml_list = function
  [] -> Nil
| h::t -> Cons (h, make_tinyocaml_list t)

let caml_ml_out_channels_list =
  mk "caml_ml_out_channels_list"
    (function env -> function [Unit] -> make_tinyocaml_list (List.map (fun x -> OutChannel x) (caml_ml_out_channels_list ()))
     | _ -> failwith "caml_ml_out_channels_list")

let builtin_primitives =
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
  match typ with None -> implementation | Some typ ->
  if name = "%identity" then
    really_add_type (our_type (final_type typ)) implementation
  else
    implementation

let lookup_primitive ?typ n =
  try add_type typ (n, (List.assoc n builtin_primitives)) with
    Not_found ->
      snd
        (mk n
          (function _ -> function
             [e] -> Raise ("UnknownPrimitive: " ^ n, None)
             | _ -> failwith "unknown unknown primitive"))

