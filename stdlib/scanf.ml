open CamlinternalFormatBasics
open CamlinternalFormat

module Scanning = struct

  type file_name = string

  type in_channel_name =
    | From_channel of Pervasives.in_channel
    | From_file of file_name * Pervasives.in_channel
    | From_function
    | From_string

  type in_channel = {
    mutable ic_eof : bool;
    mutable ic_current_char : char;
    mutable ic_current_char_is_valid : bool;
    mutable ic_char_count : int;
    mutable ic_line_count : int;
    mutable ic_token_count : int;
    mutable ic_get_next_char : unit -> char;
    ic_token_buffer : Buffer.t;
    ic_input_name : in_channel_name;
  }

  type scanbuf = in_channel

  let null_char = '\000'

  let next_char ib =
    try
      let c = ib.ic_get_next_char () in
      ib.ic_current_char <- c;
      ib.ic_current_char_is_valid <- true;
      ib.ic_char_count <- succ ib.ic_char_count;
      if c = '\n' then ib.ic_line_count <- succ ib.ic_line_count;
      c with
    | End_of_file ->
      let c = null_char in
      ib.ic_current_char <- c;
      ib.ic_current_char_is_valid <- false;
      ib.ic_eof <- true;
      c

  let peek_char ib =
    if ib.ic_current_char_is_valid
    then ib.ic_current_char
    else next_char ib

  let checked_peek_char ib =
    let c = peek_char ib in
    if ib.ic_eof then raise End_of_file;
    c

  let end_of_input ib =
    ignore (peek_char ib);
    ib.ic_eof

  let eof ib = ib.ic_eof

  let beginning_of_input ib = ib.ic_char_count = 0

  let name_of_input ib =
    match ib.ic_input_name with
    | From_channel _ic -> "unnamed Pervasives input channel"
    | From_file (fname, _ic) -> fname
    | From_function -> "unnamed function"
    | From_string -> "unnamed character string"

  let char_count ib =
    if ib.ic_current_char_is_valid
    then ib.ic_char_count - 1
    else ib.ic_char_count

  let line_count ib = ib.ic_line_count

  let reset_token ib = Buffer.reset ib.ic_token_buffer

  let invalidate_current_char ib = ib.ic_current_char_is_valid <- false

  let token ib =
    let token_buffer = ib.ic_token_buffer in
    let tok = Buffer.contents token_buffer in
    Buffer.clear token_buffer;
    ib.ic_token_count <- succ ib.ic_token_count;
    tok

  let token_count ib = ib.ic_token_count

  let skip_char width ib =
    invalidate_current_char ib;
    width

  let ignore_char width ib = skip_char (width - 1) ib

  let store_char width ib c =
    Buffer.add_char ib.ic_token_buffer c;
    ignore_char width ib

  let default_token_buffer_size = 1024

  let create iname next = {
    ic_eof = false;
    ic_current_char = null_char;
    ic_current_char_is_valid = false;
    ic_char_count = 0;
    ic_line_count = 0;
    ic_token_count = 0;
    ic_get_next_char = next;
    ic_token_buffer = Buffer.create default_token_buffer_size;
    ic_input_name = iname;
  }

  let from_string s =
    let i = ref 0 in
    let len = String.length s in
    let next () =
      if !i >= len then raise End_of_file else
      let c = s.[!i] in
      incr i;
      c in
    create From_string next

  let from_function = create From_function

  let file_buffer_size = ref 1024

  let scan_close_at_end ic = Pervasives.close_in ic; raise End_of_file

  let scan_raise_at_end _ic = raise End_of_file

  let from_ic scan_close_ic iname ic =
    let len = !file_buffer_size in
    let buf = Bytes.create len in
    let i = ref 0 in
    let lim = ref 0 in
    let eof = ref false in
    let next () =
      if !i < !lim then begin let c = Bytes.get buf !i in incr i; c end else
      if !eof then raise End_of_file else begin
        lim := input ic buf 0 len;
        if !lim = 0 then begin eof := true; scan_close_ic ic end else begin
          i := 1;
          Bytes.get buf 0
        end
      end in
    create iname next

  let from_ic_close_at_end = from_ic scan_close_at_end
  let from_ic_raise_at_end = from_ic scan_raise_at_end

  let stdin =
    from_ic scan_raise_at_end
      (From_file ("-", Pervasives.stdin)) Pervasives.stdin

  let stdib = stdin

  let open_in_file open_in fname =
    match fname with
    | "-" -> stdin
    | fname ->
      let ic = open_in fname in
      from_ic_close_at_end (From_file (fname, ic)) ic

  let open_in = open_in_file Pervasives.open_in
  let open_in_bin = open_in_file Pervasives.open_in_bin

  let from_file = open_in
  let from_file_bin = open_in_bin

  let from_channel ic =
    from_ic_raise_at_end (From_channel ic) ic

  let close_in ib =
    match ib.ic_input_name with
    | From_channel ic ->
      Pervasives.close_in ic
    | From_file (_fname, ic) -> Pervasives.close_in ic
    | From_function | From_string -> ()

  let memo_from_ic =
    let memo = ref [] in
    (fun scan_close_ic ic ->
     try List.assq ic !memo with
     | Not_found ->
       let ib =
         from_ic scan_close_ic (From_channel ic) ic in
       memo := (ic, ib) :: !memo;
       ib)

  let memo_from_channel = memo_from_ic scan_raise_at_end

end

let token_string = Scanning.token

