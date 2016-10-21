type size

external size_of_int : int -> size = "%identity"

external int_of_size : size -> int = "%identity"

type box_type = CamlinternalFormatBasics.block_type =
  | Pp_hbox | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits

type pp_token =
  | Pp_text of string          (* normal text *)
  | Pp_break of int * int      (* complete break *)
  | Pp_tbreak of int * int     (* go to next tabulation *)
  | Pp_stab                    (* set a tabulation *)
  | Pp_begin of int * box_type (* beginning of a box *)
  | Pp_end                     (* end of a box *)
  | Pp_tbegin of tbox          (* beginning of a tabulation box *)
  | Pp_tend                    (* end of a tabulation box *)
  | Pp_newline                 (* to force a newline inside a box *)
  | Pp_if_newline              (* to do something only if this very
                                  line has been broken *)
  | Pp_open_tag of tag         (* opening a tag name *)
  | Pp_close_tag               (* closing the most recently opened tag *)

and tag = string

and tbox = Pp_tbox of int list ref  (* Tabulation box *)

type 'a queue_elem =
  | Nil
  | Cons of 'a queue_cell

and 'a queue_cell = {
  mutable head : 'a;
  mutable tail : 'a queue_elem;
}

type 'a queue = {
  mutable insert : 'a queue_elem;
  mutable body : 'a queue_elem;
}

type pp_queue_elem = {
  mutable elem_size : size;
  token : pp_token;
  length : int;
}


type pp_queue = pp_queue_elem queue

type pp_scan_elem = Scan_elem of int * pp_queue_elem

type pp_scan_stack = pp_scan_elem list

type pp_format_elem = Format_elem of box_type * int

type pp_format_stack = pp_format_elem list

type pp_tag_stack = tag list

type formatter = {
  (* The various stacks. *)
  mutable pp_scan_stack : pp_scan_stack;
  mutable pp_format_stack : pp_format_stack;
  mutable pp_tbox_stack : tbox list;
  mutable pp_tag_stack : pp_tag_stack;
  mutable pp_mark_stack : pp_tag_stack;
  (* Value of right margin. *)
  mutable pp_margin : int;
  (* Minimal space left before margin, when opening a box. *)
  mutable pp_min_space_left : int;
  (* Maximum value of indentation:
     no box can be opened further. *)
  mutable pp_max_indent : int;
  (* Space remaining on the current line. *)
  mutable pp_space_left : int;
  (* Current value of indentation. *)
  mutable pp_current_indent : int;
  (* True when the line has been broken by the pretty-printer. *)
  mutable pp_is_new_line : bool;
  (* Total width of tokens already printed. *)
  mutable pp_left_total : int;
  (* Total width of tokens ever put in queue. *)
  mutable pp_right_total : int;
  (* Current number of opened boxes. *)
  mutable pp_curr_depth : int;
  (* Maximum number of boxes which can be simultaneously opened. *)
  mutable pp_max_boxes : int;
  (* Ellipsis string. *)
  mutable pp_ellipsis : string;
  (* Output function. *)
  mutable pp_out_string : string -> int -> int -> unit;
  (* Flushing function. *)
  mutable pp_out_flush : unit -> unit;
  (* Output of new lines. *)
  mutable pp_out_newline : unit -> unit;
  (* Output of indentation spaces. *)
  mutable pp_out_spaces : int -> unit;
  (* Are tags printed ? *)
  mutable pp_print_tags : bool;
  (* Are tags marked ? *)
  mutable pp_mark_tags : bool;
  (* Find opening and closing markers of tags. *)
  mutable pp_mark_open_tag : tag -> string;
  mutable pp_mark_close_tag : tag -> string;
  mutable pp_print_open_tag : tag -> unit;
  mutable pp_print_close_tag : tag -> unit;
  (* The pretty-printer queue. *)
  mutable pp_queue : pp_queue;
}


(* The formatter specific tag handling functions. *)
type formatter_tag_functions = {
  mark_open_tag : tag -> string;
  mark_close_tag : tag -> string;
  print_open_tag : tag -> unit;
  print_close_tag : tag -> unit;
}


(* The formatter functions to output material. *)
type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
}


let make_queue () = { insert = Nil; body = Nil; }

let display_newline state () = state.pp_out_string "\n" 0  1

let blank_line = String.make 80 ' '

let rec display_blanks state n =
  if n > 0 then
  if n <= 80 then state.pp_out_string blank_line 0 n else
  begin
    state.pp_out_string blank_line 0 80;
    display_blanks state (n - 80)
  end

let default_pp_mark_open_tag s = "<" ^ s ^ ">"
let default_pp_mark_close_tag s = "</" ^ s ^ ">"

let default_pp_print_open_tag = ignore
let default_pp_print_close_tag = ignore

let pp_make_formatter f g h i =
  let pp_queue = make_queue () in
  let pp_margin = 78
  and pp_min_space_left = 10 in
  {
    pp_scan_stack = [];
    pp_format_stack = [];
    pp_tbox_stack = [];
    pp_tag_stack = [];
    pp_mark_stack = [];
    pp_margin = pp_margin;
    pp_min_space_left = pp_min_space_left;
    pp_max_indent = pp_margin - pp_min_space_left;
    pp_space_left = pp_margin;
    pp_current_indent = 0;
    pp_is_new_line = true;
    pp_left_total = 1;
    pp_right_total = 1;
    pp_curr_depth = 1;
    pp_max_boxes = max_int;
    pp_ellipsis = ".";
    pp_out_string = f;
    pp_out_flush = g;
    pp_out_newline = h;
    pp_out_spaces = i;
    pp_print_tags = false;
    pp_mark_tags = false;
    pp_mark_open_tag = default_pp_mark_open_tag;
    pp_mark_close_tag = default_pp_mark_close_tag;
    pp_print_open_tag = default_pp_print_open_tag;
    pp_print_close_tag = default_pp_print_close_tag;
    pp_queue = pp_queue;
  }

let make_formatter output flush =
  let ppf = pp_make_formatter output flush ignore ignore in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf

let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> flush oc)

let std_formatter = formatter_of_out_channel Pervasives.stdout
