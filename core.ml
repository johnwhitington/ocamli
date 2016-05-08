(* Core / Persvasives *)
open Tinyocaml

(* This contains externals from Core / Pervasives *)
let builtin_output_string = function
  [OutChannel c; String s] -> output_string c s; Unit
| _ -> failwith "builtin_output_string"

let builtin_print_int = function
  [Int i] -> output_string stdout (string_of_int i); Unit
| _ -> failwith "builtin_print_int"

let builtin_input_line = function
  [InChannel c] -> String (input_line c)
| _ -> failwith "builtin_input_line"

let builtin_int_of_string = function
  [String s] ->
    begin try Int (int_of_string s) with
      e -> failwith "builtin_int_of_string" (* FIXME: A Proper exception here *)
    end
| _ -> failwith "builtin_int_of_string"

(* The initial asterisk will be used to elide these variables when not showing
pervasives in the output. When showing pervasives, we just remove the asterisk. *)
let mk name f =
  (name, LetDef (name, Fun ("__PER__x", CallBuiltIn (name, [Var "__PER__x"], f))))

let mk2 name f =
  (name, LetDef (name, Fun ("__PER__x", Fun ("__PER__y", CallBuiltIn (name, [Var "__PER__x"; Var "__PER__y"], f)))))

(* String to tinyocaml *)
let make_tiny s =
  let (r, r') = match
    s |> Lexing.from_string |> Parse.implementation |> of_real_ocaml
  with
    Module [LetDef (n, x)]
  | Module [LetRecDef (n, x)] -> (n, x)
  | Module [ExceptionDef (n, _) as h] -> (n, h)
  | exception e -> print_string s; print_newline (); raise e
  | _ -> failwith "make_tiny"
 in
   (*Printf.printf "%s, %s\n" r (Tinyocaml.to_string r');*)
   (r, r')

(* This contains pure ocaml functions for things in Pervasives. FIXME:
Autogenerate these by reading pervasives.ml(i) from the OCaml installation. *)
let pervasives =
  [make_tiny "let ref = fun __PER__x -> {contents = __PER__x}";
   make_tiny "let ( ! ) = fun __PER__x -> __PER__x.contents";
   make_tiny "let ( := ) = fun __PER__a -> fun __PER__b -> __PER__a.contents <- __PER__b";
   mk2 "output_string" builtin_output_string;
   make_tiny "let print_string = fun __PER__x -> output_string stdout __PER__x";
   mk "print_int" builtin_print_int;
   mk "int_of_string" builtin_int_of_string;
   mk "input_line" builtin_input_line;
   make_tiny "let failwith = fun __PER__s -> raise (Failure __PER__s)";
   make_tiny "let invalid_arg = fun __PER__s -> raise (Invalid_argument __PER__s)"]

(* Core exceptions. These cannot be autogenerated without access to an
OCaml source tree, so we list them explicitly. We don't need the core types like
int and format6 etc, because we deal with them explicitly in Tinyocaml. *)
let core =
  [make_tiny "exception Match_failure of string * int * int";
   make_tiny "exception Assert_failure of string * int * int";
   make_tiny "exception Invalid_argument of string";
   make_tiny "exception Failure of string";
   make_tiny "exception Not_found";
   make_tiny "exception Out_of_memory";
   make_tiny "exception Stack_overflow";
   make_tiny "exception Sys_error of string";
   make_tiny "exception End_of_file";
   make_tiny "exception Division_by_zero";
   make_tiny "exception Sys_blocked_io";
   make_tiny "exception Undefined_recursive_module of string * int * int"]

(* OCaml's built-in primitives. Cannot be autogenerated *)
let builtin_primitives = [
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
  "caml_register_named_value";
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
  "caml_weak_set";
]

