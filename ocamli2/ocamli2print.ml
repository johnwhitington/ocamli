open Asttypes
open Parsetree
open Types
open Ocamli2type

(* Set this to false to debug failures in string_of_value *)
let showvals = ref true

(* Use bold and underlining *)
let syntax = ref true

(* To output \textbf{} and \underline{} for thesis writing. *)
let syntax_tex = ref false

(* Show all implicit lets, for debug *)
let show_all_lets = ref false

type assoc = L | R | N

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

(* Current tags opened. Whenever a tag is added, we have to end the codes, and
begin new ones *) 
let tags = ref []

let output_tag f = function
  "underline" ->
    if !syntax_tex
      then Format.pp_print_string f "\\underline{"
      else Format.pp_print_string f ul
| "bold" ->
    if !syntax_tex
      then Format.pp_print_string f "\\textbf{"
      else Format.pp_print_string f bold
| _ -> failwith "format: unknown tag"

let _ =
  ignore
    (Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> print_string code_end)))

let rec string_of_ocaml_type = function
  Tvar (Some x) -> x
| Tvar None -> "Tvar None"
| Tnil -> "Tnil"
| Tarrow (arg_label, a, b, commutable) ->
    Printf.sprintf
      "Tarrow (%s, %s)"
      (string_of_ocaml_type a.desc)
      (string_of_ocaml_type b.desc)
| Tconstr (path, types, abbrev_memo) ->
    "Tconstr " ^ Path.name path
  ^ "("
  ^ List.fold_left ( ^ ) "" (List.map (fun x -> string_of_ocaml_type x.desc ^ " ") types)
  ^ ")"
| Ttuple _ -> "Ttuple"
| Tobject (_, _) -> "Tobject"
| Tfield (_, _, _, _) -> "Tfield"
| Tlink x -> string_of_ocaml_type (find_type_desc x)
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let printstring_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/" 

let printstring_of_compop = function
    LT -> "<"
  | GT -> ">"
  | EQ -> "="
  | NEQ -> "<>"
  | GTE -> ">="
  | LTE -> "<="

let printstring_of_boolop = function
   AND -> "&&"
 | OR -> "||"

let rec assoc = function
| IntOp _ | Apply _ | Compare _ -> L
| ArraySet _ | BoolOp _ -> R
| _ -> N

let prec = function
  ArrayGet _ -> 110
| Apply _ -> 100
| Append _ -> 95
| Compare _ -> 94
| BoolOp (AND, _, _) -> 65
| BoolOp (OR, _, _) -> 60
| IntOp ((Mul | Div), _, _) -> 90
| IntOp (_, _, _) -> 80
| ArraySet _ -> 55
| Function _ | Let _ | LetDef _ -> 10
| Struct _  -> 3
| Value _ | Var _ | ArrayExpr _ | Cons _ | FOp _ | Match _ -> max_int


let is_op_char = function
  '!' | '$' | '%' | '*' | '+' | '-' | '.' | '/' | ':'
| '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' -> true
| _ -> false

let is_op s =
  String.length s > 0 && is_op_char s.[0]

let parens node parent isleft =
  match parent with
    None -> ("","")
  | Some p ->
      let p = p.e in 
      if   prec node > prec p
        || assoc node = N && prec node = prec p
        || assoc node = L && isleft && assoc p = L && prec node = prec p
        || assoc node = R && not isleft && assoc p = R && prec node = prec p
      then
        ("","")
      else
        ("(", ")")

let rec list_elements value =
  if Obj.is_int value then [] else
    Obj.field value 0 :: list_elements (Obj.field value 1)

let rec string_of_value v = function
    Tconstr (p, _, _) when Path.name p = "int" -> string_of_int (Obj.magic v : int)
  | Tconstr (p, _, _) when Path.name p = "float" -> string_of_float (Obj.magic v : float)
  | Tconstr (p, _, _) when Path.name p = "unit" -> "()"
  | Tconstr (p, _, _) when Path.name p = "bool" -> string_of_bool (Obj.magic v : bool)
  | Tconstr (p, [elt_t], _) when Path.name p = "array" ->
      let strings =
        Array.init
          (Obj.size v)
          (fun i -> string_of_value (Obj.field v i) (find_type_desc elt_t))
      in
        "[|"
      ^ Array.fold_left ( ^ ) "" (Array.map (fun x -> x ^ "; ") strings)
      ^ "|]"
  | Tconstr (p, [elt_t], _) when Path.name p = "list" ->
      let first = ref true in
        "["
      ^ List.fold_left ( ^ ) "" (List.map (fun x -> let r = (if !first then "" else "; ") ^ x in first := false; r) 
          (List.map (fun v -> string_of_value v (find_type_desc elt_t)) (list_elements v)))
      ^ "]"
  | t ->
      if !showvals
        then failwith (Printf.sprintf "string_of_value: unknown type %s" (string_of_ocaml_type t))
        else Printf.sprintf "<%s>" (string_of_ocaml_type t)

(* Find the names of functions which are candidates for abbreviation, and return the expression below *)
let rec find_funs x =
  match x.e with
  | Function ([(PatVar v, _, e)], _) ->
      let more, e' = find_funs e in
        (v::more, e')
  | e -> ([], x)

let rec print_finaltype_inner f isleft parent node =
  let str = Format.pp_print_string f in
  let newline () = Format.fprintf f "@\n" in
  let bold () = Format.pp_open_tag f "bold" in
  let unbold () = Format.pp_close_tag f () in
  let boldstr t = bold (); str t; unbold () in
  let lp, rp = parens node.e parent isleft in
  if node.peek = Some {underline = true} then Format.pp_open_tag f "underline";
  (* 1. Print any implicit lets which are not shadowed (or preprocess?) *)
  if List.length node.lets > 0 then str lp;
  List.iter
    (fun (recflag, {contents = bindings}) ->
       List.iter
         (fun (n, e) ->
            if recflag then boldstr "let rec " else boldstr "let ";
            str n;
            str " = ";
            print_finaltype_inner f true (Some node) e;
            boldstr " in ")
         bindings)
    node.lets;
  (* 2. Match on the expression itself, and print *)
  begin match node.printas with
    Some x -> str x
  | None -> ()
  end;
  if node.printas = None then begin match node.e with
    Value v ->
      str (string_of_value v node.typ)
  | IntOp (op, l, r) ->
      str lp;
      print_finaltype_inner f true (Some node) l;
      str " ";
      str (printstring_of_op op);
      str " ";
      print_finaltype_inner f false (Some node) r;
      str rp
  | Compare (op, l, r) ->
      str lp;
      print_finaltype_inner f true (Some node) l;
      str " ";
      str (printstring_of_compop op);
      str " ";
      print_finaltype_inner f false (Some node) r;
      str rp
  | BoolOp (op, l, r) ->
      str lp;
      print_finaltype_inner f true (Some node) l;
      str " ";
      str (printstring_of_boolop op);
      str " ";
      print_finaltype_inner f false (Some node) r;
      str rp
  | FOp (op, l, r) ->
      str lp;
      print_finaltype_inner f true (Some node) l;
      str " ";
      str (printstring_of_op op);
      str ". ";
      print_finaltype_inner f false (Some node) r;
      str rp
  | Struct structure_items ->
      let l = List.length structure_items in
        List.iteri
          (fun i x ->
             print_finaltype_inner f false (Some node) x;
             if i < l - 1 then (newline (); newline ()))
          structure_items;
  | LetDef (recflag, (n, e)) ->
      str lp;
      if recflag then boldstr "let rec " else boldstr "let ";
      str n;
      let names, e' = find_funs e in
      List.iter (fun x -> str " "; str x) names;
      str " ";
      str "= ";
      print_finaltype_inner f false (Some node) e';
      str rp
  | Let (recflag, (n, e), e') ->
      str lp;
      if recflag then boldstr "let rec " else boldstr "let ";
      str n;
      let names, e' = find_funs e in
      List.iter (fun x -> str " "; str x) names;
      str " = ";
      print_finaltype_inner f false (Some node) e;
      boldstr " in ";
      print_finaltype_inner f false (Some node) e';
      str rp
  | Var x ->
      let s =
        if String.length x > 7 && String.sub x 0 7 = "Stdlib."
          then String.sub x 7 (String.length x - 7)
          else x
      in
        if is_op s then str "( ";
        str s;
        if is_op s then str " )"
  | Apply (fn, args) ->
      str lp;
      print_finaltype_inner f false (Some node) fn;
      List.iter (fun arg -> str " "; print_finaltype_inner f false (Some node) arg) args;
      str rp
  | Function (cases, _) ->
      str lp;
      boldstr "function";
      if lp = "" then newline ();
      let first = ref true in
      let l = List.length cases in
      List.iteri
       (fun i (pat, _, rhs) ->
         if !first then
           (if lp = "(" then str " " else str "  ") else
           (if lp = "(" then str " | " else str "| ");
         first := false;
         print_finaltype_pattern f false (Some node) pat;
         str " -> ";
         print_finaltype_inner f false (Some node) rhs;
         if i < l - 1 && lp = "" then newline ())
       cases;
      str rp
  | Match (e, cases) ->
      str lp;
      boldstr "match ";
      print_finaltype_inner f false (Some node) e;
      boldstr " with";
      let first = ref true in
      List.iter
       (fun (pat, _, rhs) ->
         if !first then str " " else str " | ";
         first := false;
         print_finaltype_pattern f false (Some node) pat;
         str " -> ";
         print_finaltype_inner f false (Some node) rhs)
       cases;
      str rp;
  | Cons (h, ({e = Value v; typ})) when string_of_value v typ = "[]" ->
      str lp;
      str "[";
      print_finaltype_inner f false (Some node) h;
      str "]";
      str rp
  | Cons (h, t) ->
      str lp;
      print_finaltype_list f false (Some node) h t;
      str rp
  | ArrayExpr elts ->
      str lp;
      str "[|";
      Array.iter (fun e -> print_finaltype_inner f false (Some node) e; str "; ") elts;
      str "|]";
      str rp
  | ArrayGet (arr, i) ->
      str lp;
      print_finaltype_inner f false (Some node) arr;
      str ".(";
      print_finaltype_inner f false (Some node) i;
      str ")";
      str rp
  | ArraySet (arr, i, v) ->
      str lp;
      print_finaltype_inner f false (Some node) arr;
      str ".(";
      print_finaltype_inner f false (Some node) i;
      str ") <- ";
      print_finaltype_inner f false (Some node) v;
      str rp
  | Append (h, t) ->
      str lp;
      print_finaltype_inner f false (Some node) h;
      str " @ ";
      print_finaltype_inner f false (Some node) t;
      str rp
  end;
  if List.length node.lets > 0 then str rp;
  if node.peek = Some {underline = true} then Format.pp_close_tag f ()

and print_finaltype_list f isleft parent h t =
  let str = Format.pp_print_string f in
  print_finaltype_inner f false parent h;
  str " :: ";
  print_finaltype_inner f false parent t

and print_finaltype_pattern_list_inner f isleft parent pat =
  let str = Format.pp_print_string f in
  match pat with
  | PatConstr ("::", h::t) ->
      print_finaltype_pattern f isleft parent h;
      if t <> [] then str "::";
      print_finaltype_pattern f isleft parent (PatConstr ("::", t));
  | PatConstr ("::", []) -> ()
  | _ -> failwith "print_finaltype_pattern_list_inner"

and print_finaltype_pattern_list f isleft parent pat =
  let str = Format.pp_print_string f in
  match pat with
  | PatConstr ("::", [h; PatConstr ("[]", _)]) ->
      str "[";
      print_finaltype_pattern f isleft parent h;
      str "]"
  | pat -> print_finaltype_pattern_list_inner f isleft parent pat

and print_finaltype_pattern f isleft parent pat =
  let str = Format.pp_print_string f in
    match pat with
      PatAny -> str "_"
    | PatVar v -> str v
    | PatConstr ("::", _) -> print_finaltype_pattern_list f isleft parent pat
    | PatConstr (name, pats) ->
        str name;
        if pats <> [] then begin
          str " ";
          List.iter
            (fun x -> str " "; print_finaltype_pattern f isleft parent x)
            pats
        end
    | PatConstant (IntConstant i) -> str (string_of_int i)

let print_finaltype f t =
  print_finaltype_inner f true None t

let output_tags f =
  List.iter (output_tag f) !tags

let print f (v : t) =
  let tagfuns =
    {Format.mark_open_tag = (fun _ -> "");
     Format.mark_close_tag = (fun _ -> "");
     Format.print_open_tag =
       (if !syntax then
         (fun tag -> tags := tag::!tags; output_tag f tag)
       else
         (fun _ -> ()));
     Format.print_close_tag =
       (if !syntax then
         (fun _ ->
            if !tags = [] then failwith "ill-matched tags: pop";
            tags := List.tl !tags;
            begin if !syntax_tex
              then Format.pp_print_string f "}"
              else Format.pp_print_string f code_end
            end;
            if !tags <> [] then output_tags f)
       else
         (fun _ -> ()))}
  in
    Format.pp_set_formatter_tag_functions f tagfuns;
    Format.pp_set_tags f true;
    Format.pp_set_print_tags f true;
    Format.pp_open_box f 0;
    print_finaltype f (if !show_all_lets then v else Ocamli2type.remove_unused_lets v);
    Format.pp_close_box f ();
    Format.pp_print_flush f ()

let to_string v =
  print Format.str_formatter v;
  Format.flush_str_formatter ()

let to_string_from_heap typ v =
  to_string
    {e = Value v;
     lets = [];
     typ = typ;
     peek = None;
     printas = None}

let string_of_t_show_types = ref true

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let string_of_compop = function
  | LT -> "LT"
  | GT -> "GT"
  | GTE -> "GTE"
  | LTE -> "LTE"
  | EQ -> "EQ"
  | NEQ -> "NEQ"

let string_of_boolop = function
  | AND -> "AND"
  | OR -> "OR"

let rec string_of_t' typ = function
  Value x -> to_string_from_heap typ x
| Function (cases, env) ->
    Printf.sprintf "Function (%s, env = %s)" (string_of_cases cases) (string_of_env env)
| Apply (e, args) ->
    Printf.sprintf "Apply (%s, [%s])" (string_of_t e) (string_of_items args)
| Var x -> Printf.sprintf "Var %s" x
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| Cons (a, b) ->
    Printf.sprintf
      "Cons (%s, %s)" (string_of_t a) (string_of_t b)
| Append (a, b) ->
    Printf.sprintf
      "Append (%s, %s)" (string_of_t a) (string_of_t b)
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| Compare (op, a, b) ->
    Printf.sprintf 
      "Compare (%s, %s, %s)"
      (string_of_compop op) (string_of_t a) (string_of_t b)
| BoolOp (op, a, b) ->
    Printf.sprintf 
      "BoolOp (%s, %s, %s)"
      (string_of_boolop op) (string_of_t a) (string_of_t b)
| IntOp (op, a, b) ->
    Printf.sprintf
      "IntOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_t arr) (string_of_t i)
| ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_t arr) (string_of_t i) (string_of_t newval)
| Let (recflag, (n, e), e') ->
    Printf.sprintf
      "Let (%b, %s, %s, %s)"
      recflag n (string_of_t e) (string_of_t e')
| Match (e, cases) ->
    Printf.sprintf "Match (%s, %s)" (string_of_t e) "<cases>"
| Struct l ->
    Printf.sprintf "Struct:%s\n"
      (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (List.map string_of_t l))
| LetDef (recflag, (n, e)) ->
    Printf.sprintf "LetDef %b (%s, %s)"
      recflag n (string_of_t e)

and string_of_case (p, _, e) = (* FIXME guard *)
  Printf.sprintf "[%s -> %s]" (string_of_pattern p) (string_of_t e)
     
and string_of_cases cases =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_case x ^ " ") cases)

and string_of_pattern = function
  PatAny -> "_"
| PatVar v -> v
| PatConstr (constr, pats) ->
    "PatConstr " ^ constr ^ "("
  ^ List.fold_left ( ^ ) "" (List.map (fun x -> string_of_pattern x ^ ", ") pats)
  ^ ")"
| PatConstant (IntConstant i) -> string_of_int i

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_t x ^ ";") items)

and string_of_t {typ; e; lets; peek} =
  List.fold_left ( ^ ) ""
    (List.map
    (fun (recflag, r) ->
       Printf.sprintf "{%b, %s}" recflag (string_of_bindings !r))
    lets)
  ^
  (if !string_of_t_show_types then "{typ = " ^ string_of_ocaml_type typ ^ "}" else "")
  ^
  string_of_t' typ e

and string_of_bindings bs =
  List.fold_left ( ^ ) "" (List.map string_of_binding bs)

and string_of_binding (n, e) =
  Printf.sprintf "%s = %s; " n (string_of_t e)

and string_of_envitem (recflag, {contents}) =
  Printf.sprintf "(%b, %s)" recflag (string_of_bindings contents)

and string_of_env es =
  List.fold_left ( ^ ) "" (List.map (fun e -> string_of_envitem e ^ ";\n") es)

