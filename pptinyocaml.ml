open Tinyocaml
open Asttypes
open Parsetree

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
| Fun _ | Function _ | Let _ -> 10
| Struct _ -> 0 | Tuple _ -> 0 (* FIXME *)
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

let pp_constructor_arg pp = function
  Pcstr_tuple coretypes ->
    Pprintast.core_type pp
      {ptyp_desc = Ptyp_tuple coretypes;
       ptyp_loc = Location.none; 
       ptyp_attributes = []}
| _ -> failwith "unimplemented record type"

let rec print_tiny_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  let lp, rp = parens node parent isleft in
  match node with
  | Match (e, patmatch) ->
      str lp;
      boldtxt "match ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      List.iter (print_case f false (Some node)) patmatch;
      str rp
  | Function patmatch ->
      str lp;
      boldtxt "function ";
      List.iter (print_case f false (Some node)) patmatch;
      str rp
  | Struct structure_items ->
      let l = List.length structure_items in
        List.iteri
          (fun i x ->
             print_tiny_inner f false (Some node) x;
             if i < l - 1 then txt "\n\n")
          structure_items
  | Tuple xs ->
      let l = List.length xs in
      str "(";
      List.iteri
        (fun i x ->
           print_tiny_inner f false (Some node) x;
           if i < l - 1 then txt ", ")
        xs;
      str ")"
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
  | Let (recflag, bindings, e') ->
      str lp;
      let first = ref true in
        List.iter
          (fun (v, e) ->
             if !first then
               if recflag then
                 boldtxt "let rec " else boldtxt "let "
               else
                 boldtxt " and ";
             print_pattern f false (Some node) v;
             txt " ";
             let morefuns, e = find_funs e in
             List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
             txt "= ";
             print_tiny_inner f false (Some node) e)
          bindings;
      boldtxt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | LetDef (recflag, bindings) ->
      str lp;
      let first = ref true in
      List.iter
        (fun (v, e) -> 
           if !first then
             if recflag then boldtxt "let rec " else boldtxt "let "
           else
             boldtxt " and ";
           first := false;
           print_pattern f false (Some node) v;
           txt " ";
           let morefuns, e = find_funs e in
             List.iter (fun l -> str (Evalutils.unstar l); txt " ") morefuns;
           txt "= ";
           print_tiny_inner f false (Some node) e)
        bindings;
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
  | ExceptionDef (e, t) ->
      str lp;
      boldtxt "exception ";
      str e;
      begin match t with
        Pcstr_tuple (_::_) ->
          boldtxt " of ";
          pp_constructor_arg f t
      | _ -> ()
      end;
      str rp

and print_case f isleft parent (pattern, guard, rhs) =
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  txt "| ";
  print_pattern f isleft parent pattern;
  begin match guard with
  | None -> ()
  | Some g ->
      boldtxt " when ";
      print_tiny_inner f false parent g
  end;
  boldtxt " -> ";
  print_tiny_inner f false parent rhs;
  txt " "

and print_pattern f isleft parent pat =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
    match pat with
      PatAny ->
        str "_"
    | PatUnit ->
        str "()"
    | PatVar v ->
        str (Evalutils.unstar v)
    | PatInt i ->
        str (string_of_int i)
    | PatTuple items ->
        str "(";
        let l = List.length items in
          List.iteri
            (fun i x ->
               print_pattern f isleft parent x;
               if i < l - 1 then txt ", ")
            items;
        str ")"
    | PatNil -> str "[]"
    | PatCons (h, t) ->
        print_pattern f isleft parent h;
        str "::";
        print_pattern f isleft parent t
    | PatAlias (a, p) ->
        print_pattern f isleft parent p;
        boldtxt " as ";
        str a

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
