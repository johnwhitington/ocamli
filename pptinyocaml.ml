open Tinyocaml

(* If true, whole program printed on one line *)
let simple = ref false

(* Width to format to *)
let width = ref 80

type assoc = L | R | N

let rec assoc = function
  Control (_, x) -> assoc x
| Op _ | Cmp _ | App _ -> L
| And _ | Or _ | Seq _ | SetField _ -> R
| _ -> N

let prec = function
  Field _ -> 110
| App _ -> 100
| Op ((Mul | Div), _, _) -> 90
| Op (_, _, _) -> 80
| Cmp _ -> 70
| And _ -> 65
| Or _ -> 60
| SetField _ -> 55
| If _ -> 50
| Fun _ | Let _ | LetRec _ -> 10
| Module _ -> 0 (* FIXME *)
| _ -> max_int

let parens node parent isleft =
  match parent with
    None -> ("","")
  | Some p ->
      if   prec node > prec p
        || assoc node = N && prec node = prec p
        || assoc node = L && isleft && assoc p = L && prec node = prec p
        || assoc node = R && not isleft && assoc p = R && prec node = prec p
      then
        ("","")
      else
        ("(", ")")

let rec print_tiny_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let lp, rp = parens node parent isleft in
  match node with
  | Module structure_items ->
      List.iter
        (print_tiny_inner f false (Some node))
        structure_items
  | Control (Underline, x) ->
      Format.pp_open_tag f "underline";
      print_tiny_inner f isleft parent x;
      Format.pp_close_tag f ()
  | Control (Pervasive, _) -> str ""
  | Unit -> str "()"
  | Int i -> str (string_of_int i)
  | Bool b -> str (string_of_bool b)
  | String s -> str ("\"" ^ String.escaped s ^ "\"")
  | OutChannel s -> str "<out_channel>"
  | InChannel s -> str "<in_channel>"
  | CallBuiltIn (args, fn) -> str "<call_built_in>"
  | Var v -> str v
  | Op (op, l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " ";
      str (Tinyocaml.string_of_op op);
      txt " ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Cmp (cmp, l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " ";
      str (Tinyocaml.string_of_cmp cmp);
      txt " ";
      print_tiny_inner f false (Some node) r;
      str rp
  | And (l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " && ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Or (l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " || ";
      print_tiny_inner f false (Some node) r;
      str rp
  | If (e, e1, e2) ->
      str lp;
      txt "if ";
      print_tiny_inner f false (Some node) e;
      txt " then ";
      print_tiny_inner f false (Some node) e1;
      txt " else ";
      print_tiny_inner f false (Some node) e2;
      str rp
  | Let (v, e, e') ->
      str lp;
      txt "let ";
      str v;
      txt " = ";
      print_tiny_inner f false (Some node) e;
      txt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | LetRec (v, e, e') ->
      str lp;
      str "let rec";
      txt " ";
      str v;
      txt " = ";
      if not !simple then txt "\n";
      print_tiny_inner f false (Some node) e;
      txt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Fun (v, e) ->
      str lp;
      txt "fun ";
      str v;
      txt " -> ";
      print_tiny_inner f false (Some node) e;
      str rp
  | App (e, e') ->
      str lp;
      print_tiny_inner f false (Some node) e;
      txt " ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Seq (e, e') ->
      str lp;
      print_tiny_inner f false (Some node) e;
      txt "; ";
      print_tiny_inner f false (Some node) e';
      str rp
  | While (e, e', _, _) ->
      str lp;
      txt "while ";
      print_tiny_inner f false (Some node) e;
      txt " do ";
      print_tiny_inner f false (Some node) e';
      txt " done";
      str rp
  | For (var, e, flag, e', e'', _) ->
      str lp;
      txt "for ";
      str var;
      txt " = ";
      print_tiny_inner f false (Some node) e;
      txt ((function UpTo -> " to " | DownTo -> " down ") flag);
      print_tiny_inner f false (Some node) e';
      txt " do ";
      print_tiny_inner f false (Some node) e'';
      txt " done"
  | Record items ->
      str "{";
      List.iter (print_record_entry f) items;
      str "}"
  | Field (e, n) ->
      str lp;
      print_tiny_inner f false (Some node) e;
      str ".";
      str n;
      str rp
  | SetField (e, n, e') ->
      str lp;
      print_tiny_inner f false (Some node) e;
      str ".";
      str n;
      txt " <- ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Raise e ->
      str lp;
      txt "raise ";
      str e;
      str rp
  | TryWith (e, (s, e')) ->
      str lp;
      txt "try ";
      print_tiny_inner f false (Some node) e;
      txt " with ";
      str s;
      txt " -> ";
      print_tiny_inner f false (Some node) e';
      str rp

and print_record_entry f (n, {contents = e}) =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
    str n;
    txt " = ";
    print_tiny_inner f false None e

let print ?(preamble="") f t =
  let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m") in
  let tagfuns =
    {Format.mark_open_tag = (fun _ -> "");
     Format.mark_close_tag = (fun _ -> "");
     Format.print_open_tag = (fun _ -> Format.pp_print_string f ul);
     Format.print_close_tag = (fun _ -> Format.pp_print_string f code_end)}
  in
    Format.pp_set_formatter_tag_functions f tagfuns;
    Format.pp_set_tags f true;
    Format.pp_set_print_tags f true;
    Format.pp_set_margin f !width;
    if !simple then Format.pp_set_margin f max_int;
    Format.pp_open_box f 4;
    Format.pp_print_string f preamble;
    print_tiny_inner f true None t;
    Format.pp_close_box f ();
    Format.pp_print_flush f ()

let to_string ?(preamble="") t =
  print ~preamble Format.str_formatter t;
  Format.flush_str_formatter ()

