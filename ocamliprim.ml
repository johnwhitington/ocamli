open Tinyocaml

let mk name f =
  (name, Fun (PatVar "*x", CallBuiltIn (name, [Var "*x"], f), [])) (* FIXME Add environment *)

let mk2 name f =
  (name,
   Fun (PatVar "*x",
     Fun (PatVar "*y", CallBuiltIn (name, [Var "*x"; Var "*y"], f), []), [])) (* FIXME Add environement *)

let mk4 name f =
   (name,
     Fun (PatVar "*x",
       Fun (PatVar "*y",
         Fun (PatVar "*z",
           Fun (PatVar "*q",
             CallBuiltIn (name, [Var "*x"; Var "*y"; Var "*z"; Var "*q"], f), []), []), []), []))

(* FIXME. Make these actually do something *)
let caml_register_named_value =
  mk2 "caml_register_named_value"
    (function [String name; func] -> Unit | _ -> failwith "builtin_caml_register_value")

external unsafe_output_string : out_channel -> string -> int -> int -> unit
                              = "caml_ml_output"

let caml_ml_output =
  mk4 "caml_ml_output"
    (function [OutChannel o; String s; Int p; Int l] ->
       unsafe_output_string o s p l;
       Unit)

external format_int : string -> int -> string = "caml_format_int"

let caml_format_int =
  mk2 "caml_format_int"
    (function [String s; Int i] -> String (format_int s i))

let percent_string_length =
  mk "%string_length"
    (function [String e] -> Int (String.length e))

let percent_raise =
  mk "%raise"
    (function [e] -> Raise ("FixPercentRaise", None) | _ -> failwith "percent_raise")

let percent_raise_notrace =
  mk "%raise_notrace"
    (function [e] -> Raise ("FixPercentRaiseNotrace", None) | _ -> failwith "percent_raise_notrace")

let percent_apply =
  mk2 "%apply"
    (function [f; a] -> App (f, a))

let percent_revapply =
  mk2 "%revapply"
    (function [a; f] -> App (f, a))

let percent_asrint =
  mk2 "%asrint"
    (function [Int x; Int y] -> Int (x asr y))

let percent_makemutable =
  mk "%makemutable"
    (function [e] -> Record [("contents", ref e)]) 

let percent_field0 =
  mk "%field0"
    (function [Record [(_, {contents = e})]] -> e)

let percent_setfield0 =
  mk2 "%setfield0"
    (function
      [Record [(_, r)]; e] -> r := e; Unit)

let percent_compare =
  mk2 "%compare"
    (function [a; b] -> Int (compare a b)) (* Obviously not *) 

let percent_addint =
  mk2 "%addint"
    (function [Int a; Int b] -> Int (a + b))

let percent_array_safe_get =
  mk2 "%array_safe_get"
    (function [Array x; Int i] -> x.(i))

external inet_addr_of_string : string -> Unix.inet_addr
                                    = "unix_inet_addr_of_string"

(* FIXME This Obj.magic stuff - can we avoid it? *)
let unix_inet_addr_of_string =
  mk "unix_inet_addr_of_string"
    (function [String s] -> String (Obj.magic (inet_addr_of_string s)))

let builtin_primitives = [
  caml_register_named_value;
  caml_ml_output;
  caml_format_int;
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
  unix_inet_addr_of_string;
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
  "%negint"
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
]

let lookup_primitive n =
  try List.assoc n builtin_primitives with
    Not_found ->
      snd
        (mk n
          (function
             [e] -> Raise ("UnknownPrimitive: " ^ n, None)
             | _ -> failwith "unknown unknown primitive"))