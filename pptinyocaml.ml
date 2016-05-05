open Tinyocaml

(* If true, whole program printed on one line *)
let simple = ref false

(* Width to format to *)
let width = ref 80

let fastcurry = ref false

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

let string_of_tag = function
  Underline -> "underline"
| Bold -> "bold"

let rec find_funs e =
  match e with
    Fun (fname, fexp) ->
      let more, e' = find_funs fexp in
        (fname::more, e')
  | _ -> ([], e)

let rec print_tiny_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  let boldstr s = bold (); str s; unbold () in
  let lp, rp = parens node parent isleft in
  match node with
  | Module structure_items ->
      let l = List.length structure_items in
        List.iteri
          (fun i x ->
             print_tiny_inner f false (Some node) x;
             if i < l - 1 then txt "\n\n")
          structure_items
  | Cons (x, y) ->
      print_tiny_inner f isleft parent x;
      str "::";
      print_tiny_inner f isleft parent y;
  | Append (x, y) ->
      print_tiny_inner f isleft parent x;
      txt " @ ";
      print_tiny_inner f isleft parent y
  | Nil ->
      str "[]"
  | Control (tag, x) ->
      Format.pp_open_tag f (string_of_tag tag);
      print_tiny_inner f isleft parent x;
      Format.pp_close_tag f ()
  | Unit -> str "()"
  | Int i -> str (string_of_int i)
  | Bool b -> str (string_of_bool b)
  | Float f -> str (string_of_float f)
  | String s -> str ("\"" ^ String.escaped s ^ "\"")
  | OutChannel s -> str "<out_channel>"
  | InChannel s -> str "<in_channel>"
  | CallBuiltIn (name, args, fn) -> str "<<"; str name; str ">>"
  | Var v -> str (Evalutils.unstar v)
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
      boldtxt "if ";
      print_tiny_inner f false (Some node) e;
      boldtxt " then ";
      print_tiny_inner f false (Some node) e1;
      boldtxt " else ";
      print_tiny_inner f false (Some node) e2;
      str rp
  | Let (v, e, e') ->
      let morefuns, e = find_funs e in
      str lp;
      boldtxt "let ";
      str (Evalutils.unstar v);
      txt " ";
      List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
      txt "= ";
      print_tiny_inner f false (Some node) e;
      boldtxt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | LetRec (v, e, e') ->
      let morefuns, e = find_funs e in
      str lp;
      boldstr "let rec";
      txt " ";
      str (Evalutils.unstar v);
      txt " ";
      List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
      txt "= ";
      if not !simple then txt "\n";
      print_tiny_inner f false (Some node) e;
      boldtxt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | LetDef (v, e) ->
      let morefuns, e = find_funs e in
      str lp;
      boldtxt "let ";
      str (Evalutils.unstar v);
      txt " ";
      List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
      txt "= ";
      print_tiny_inner f false (Some node) e;
      str rp
  | LetRecDef (v, e) ->
      let morefuns, e = find_funs e in
      str lp;
      boldstr "let rec ";
      str (Evalutils.unstar v);
      txt " ";
      List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
      txt "= ";
      if not !simple then txt "\n";
      print_tiny_inner f false (Some node) e;
      str rp
  | Fun (fname, fexp) ->
      str lp;
      boldtxt "fun ";
      str (Evalutils.unstar fname);
      txt " -> ";
      print_tiny_inner f false (Some node) fexp;
      str rp
  | App (e, e') ->
      str lp;
      print_tiny_inner f true (Some node) e;
      txt " ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Seq (e, e') ->
      str lp;
      print_tiny_inner f true (Some node) e;
      txt "; ";
      print_tiny_inner f false (Some node) e';
      str rp
  | While (e, e', _, _) ->
      str lp;
      boldtxt "while ";
      print_tiny_inner f false (Some node) e;
      boldtxt " do ";
      print_tiny_inner f false (Some node) e';
      boldtxt " done";
      str rp
  | For (var, e, flag, e', e'', _) ->
      str lp;
      boldtxt "for ";
      str var;
      txt " = ";
      print_tiny_inner f false (Some node) e;
      txt ((function UpTo -> " to " | DownTo -> " down ") flag);
      print_tiny_inner f false (Some node) e';
      boldtxt " do ";
      print_tiny_inner f false (Some node) e'';
      boldtxt " done"
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
  | Raise (e, payload) ->
      str lp;
      begin match payload with
      | None ->
          boldtxt "raise ";
          str e;
      | Some p ->
          boldtxt "raise ";
          str "(";
          str e;
          txt " ";
          print_tiny_inner f false (Some node) p;
          str ")"
      end;
      str rp
  | TryWith (e, (s, e')) ->
      str lp;
      boldtxt "try ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      str s;
      txt " -> ";
      print_tiny_inner f false (Some node) e';
      str rp
  | ExceptionDef e ->
      str lp;
      boldtxt "exception ";
      str e;
      str rp

and print_record_entry f (n, {contents = e}) =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
    str n;
    txt " = ";
    print_tiny_inner f false None e

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

(* Current tags opened. Whenever a tag is added, we have to end the codes, and
begin new ones *) 
let tags = ref []

let output_tag f = function
  "underline" -> Format.pp_print_string f ul
| "bold" -> Format.pp_print_string f bold
| _ -> failwith "format: unknown tag"

let output_tags f =
  List.iter (output_tag f) !tags

let print ?(preamble="") f t =
  let tagfuns =
    {Format.mark_open_tag = (fun _ -> "");
     Format.mark_close_tag = (fun _ -> "");
     Format.print_open_tag =
       (fun tag ->
         tags := tag::!tags;
         output_tag f tag);
     Format.print_close_tag =
       (fun _ ->
          if !tags = [] then failwith "ill-matched tags: pop";
          tags := List.tl !tags;
          Format.pp_print_string f code_end;
          if !tags <> [] then output_tags f)}
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

(* FIXME: Need to update this to only run on stdout or something *)
let _ =
  ignore
    (Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> print_string code_end)))
