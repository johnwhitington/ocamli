(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

(* The string buffering machinery *)

let initial_buffer = Bytes.create 32

let buffer = ref initial_buffer
let bufpos = ref 0

let reset_buffer () = buffer := initial_buffer; bufpos := 0

let store c =
  if !bufpos >= Bytes.length !buffer then begin
    let newbuffer = Bytes.create (2 * !bufpos) in
    Bytes.blit !buffer 0 newbuffer 0 !bufpos;
    buffer := newbuffer
  end;
  Bytes.set !buffer !bufpos c;
  incr bufpos

let get_string () =
  let s = Bytes.sub_string !buffer 0 !bufpos in buffer := initial_buffer; s

(* The lexer *)

let make_lexer keywords =
  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
      ()
    (*try Hashtbl.find kwd_table s with
      Not_found -> raise (Stream.Error ("Illegal character "))*)
  in
    ()
