open Tinyocaml
open Asttypes
open Parsetree

let debug = ref false

(* Use bold and underlining *)
let syntax = ref true

(* To output \textbf{} and \underline{} for thesis writing. *)
let syntax_tex = ref false

(* If true, whole program printed on one line *)
let simple = ref false

let is_operator_char = function
    '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
  | '?' | '@' | '^' | '|' | '~' -> true
  | _ -> false

let is_operator = function
  "lsl" | "lsr" | "asr" | "mod" | "land" | "lor" | "lxor" -> true
| "[:=" -> true
| v -> is_operator_char v.[0]

(* Width to format to *)
let width = ref 80

let fastcurry = ref false

type assoc = L | R | N

(* FIXME Add associativities for App (Var v, x) according to table *)
let rec assoc = function
  Control (_, x) -> assoc x
| Op _ | Cmp _ | App _ -> L
| And _ | Or _ | Seq _ | SetField _ -> R
| _ -> N

(* FIXME Need to add Control possibilities to this, or strip controls? *)
let prec = function
  Field _ -> 110
| App (Var v, App (_, _)) when is_operator v ->
    (* Binary operator application *)
    begin match v with
      "**" | "**." | "lsr" | "lsl" | "asr" -> 98
    | "*" | "*." | "/" | "/." | "%" | "mod" | "land" | "lor" | "lxor" -> 97
    | "+." | "-." -> 96
    | "@" | "^" -> 95
    | "=" | "<" | ">" | "<>" | "|" | "&" | "$" | "!=" -> 94
    | "<-" | ":=" -> 93
    | _ -> 100 (*FIXME*)
    end
| App (Var v, _) when is_operator v ->
    99
| Seq _ -> 91
| App _ ->
    (* All other function applications *)
    100
| Op ((Mul | Div), _, _) -> 90
| Op (_, _, _) -> 80
| Cmp _ -> 70
| And _ -> 65
| Or _ -> 60
| SetField _ -> 55
| If _ -> 50
| Fun _ | Function _ | Let _ | LetDef _ -> 10
| Struct _
| Tuple _
| Cons _ | Constr (_, _, _) | Var _ | TypeDef _ | While _ | For _ | Raise _
| Match _ | TryWith _ | ExceptionDef _ | Control _ | CallBuiltIn _ |Sig _
| ModuleBinding _ | ModuleConstraint _ | ModuleIdentifier _ | ModuleApply _
| Functor (_, _, _) | Append _ | Assert _ | Open _ | LocalOpen _ | Include _
| Lazy _ | Annot (_, _, _) -> 0
| Unit | Nil | Int _ | Bool _ | Float _ | String _ | OutChannel _
| InChannel _ | Record _ | Int32 _ | Int64 _ | NativeInt _ | Char _
| Array _ -> max_int

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
    Fun (flabel, fname, fexp, _) ->
      let more, e' = find_funs fexp in
        ((fname, flabel)::more, e')
  | _ -> ([], e)

let pp_constructor_arg pp = function
  Pcstr_tuple coretypes ->
    Pprintast.core_type pp
      {ptyp_desc = Ptyp_tuple coretypes;
       ptyp_loc = Location.none; 
       ptyp_attributes = []}
| _ -> failwith "unimplemented record type"

let str_of_builtin_args n = " x"

let rec print_tiny_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  let lp, rp = parens node parent isleft in
  match node with
  | Include e ->
      str lp;
      boldtxt "include ";
      print_tiny_inner f isleft (Some node) e;
      str rp
  | ModuleApply (m1, m2) ->
      str lp;
      print_tiny_inner f isleft (Some node) m1;
      txt "(";
      print_tiny_inner f isleft (Some node) m2;
      txt ")";
      str rp
  | Functor (n, mt, ModuleConstraint (mtc, me)) ->
      str lp;
      boldtxt "functor (";
      str n;
      begin match mt with
        None -> ()
      | Some mt ->
          str " : ";
          print_modtype f isleft (Some node) mt;
          str ")"
      end;
      txt " : ";
      print_modtype f isleft (Some node) mtc;
      txt " -> ";
      print_tiny_inner f isleft (Some node) me;
      str rp
  | Functor (n, mt, me) ->
      str lp;
      boldtxt "functor (";
      str n;
      begin match mt with
        None -> ()
      | Some mt ->
          str " : ";
          print_modtype f isleft (Some node) mt;
          str ")"
      end;
      txt " -> ";
      print_tiny_inner f isleft (Some node) me;
      str rp
  | Lazy e ->
      str lp;
      boldtxt "lazy ";
      print_tiny_inner f isleft (Some node) e;
      str rp
  | Open x ->
      str lp;
      boldtxt "open ";
      txt x;
      str rp;
      txt "\n";
  | LocalOpen (x, e) ->
      str lp;
      txt x;
      txt ".";
      str "(";
      print_tiny_inner f isleft (Some node) e;
      str ")";
      str rp
  | Assert e ->
      str lp;
      boldtxt "assert ";
      print_tiny_inner f false (Some node) e;
      str rp;
  | Match (e, patmatch) ->
      str lp;
      boldtxt "match ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      print_cases f false (Some node) patmatch;
      str rp
  | Function (patmatch, _) ->
      str lp;
      boldtxt "function ";
      print_cases f false (Some node) patmatch;
      str rp
  | Struct (b, structure_items) ->
      if b then boldtxt "struct \n";
      let l = List.length structure_items in
        List.iteri
          (fun i x ->
             print_tiny_inner f false (Some node) x;
             if i < l - 1 then txt "\n\n")
          structure_items;
      if b then begin
        txt "\n";
        boldtxt "end"
      end
  | Sig (sig_items) ->
      boldtxt "sig \n";
      let l = List.length sig_items in
        List.iteri
          (fun i x ->
             print_tiny_inner f false (Some node) x;
             if i < l - 1 then txt "\n\n")
          sig_items;
      txt "\n";
      boldtxt "end"
  | ModuleBinding (n, ModuleConstraint (t, e)) ->
      boldtxt "module ";
      txt n;
      txt " : ";
      print_modtype f false (Some node) t;
      txt " = \n";
      print_tiny_inner f false (Some node) e
  | ModuleBinding (n, e) ->
      boldtxt "module ";
      txt n;
      txt " = \n";
      print_tiny_inner f false (Some node) e
  | ModuleConstraint (t, e) ->
      ()
  | ModuleIdentifier x ->
      txt x
  | Tuple xs ->
      let l = List.length xs in
      str "(";
      List.iteri
        (fun i x ->
           print_tiny_inner f false (Some node) x;
           if i < l - 1 then txt ", ")
        xs;
      str ")"
  | Array xs -> 
      let l = Array.length xs in
      str "[|";
      Array.iteri
        (fun i x ->
           print_tiny_inner f false (Some node) x;
           if i < l - 1 then txt "; ")
        xs;
      str "|]"
  | Cons (x, y) ->
      if not (try_printing_literal_list f isleft parent node) then begin
        print_tiny_inner f isleft parent x;
        str "::";
        print_tiny_inner f isleft parent y
      end
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
  | Annot (n, p, x) ->
      if n = "show" then
        print_tiny_inner f false (Some node) x
      else
        begin
          str "[@";
          str n;
          str " ";
          print_tiny_inner f false (Some node) p;
          str "]";
          txt " ";
          print_tiny_inner f false (Some node) x
        end
  | Unit -> str "()"
  | Int i -> str (string_of_int i)
  | Int32 i -> str (Int32.to_string i ^ "l")
  | Int64 i -> str (Int64.to_string i ^ "L")
  | NativeInt i -> str (Nativeint.to_string i ^ "n")
  | Bool b -> str (string_of_bool b)
  | Float f -> str (string_of_float f)
  | String s -> str ("\"" ^ String.escaped (Bytes.to_string s) ^ "\"")
  | Char c -> str (Printf.sprintf "%C" c)
  | OutChannel s -> str "<out_channel>"
  | InChannel s -> str "<in_channel>"
  | CallBuiltIn (typ, name, args, fn) -> str "<<"; str name; str (str_of_builtin_args (List.length args)); str ">>"
  | Var v -> str (Ocamliutil.unstar v)
  | Constr (_, s, None) -> str s
  | Constr (_, s, Some x) ->
      str s;
      str " ";
      str lp;
      print_tiny_inner f false (Some node) x;
      str rp
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
      begin match e2 with
        None -> ()
      | Some e2 ->
          boldtxt " else ";
          print_tiny_inner f false (Some node) e2;
      end;
      str rp
  | Let (recflag, bindings, e') ->
      str lp;
      let first = ref true in
        List.iter
          (fun (v, e) ->
             if !first then
               if recflag then
                 boldtxt "let rec "
               else boldtxt "let "
             else
               boldtxt " and ";
             first := false;
             print_pattern f false (Some node) v NoLabel;
             txt " ";
             let morefuns, e = find_funs e in
             List.iter (fun (p, l) -> print_pattern f false (Some node) p l; txt " ") morefuns;
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
           print_pattern f false (Some node) v NoLabel;
           txt " ";
           let morefuns, e = find_funs e in
             List.iter (fun (p, l) -> print_pattern f false (Some node) p l; txt " ") morefuns;
           txt "= ";
           print_tiny_inner f false (Some node) e)
        bindings;
      str rp
  | Fun ((_, _, _, fenv) as fn) ->
      (*if !debug then begin txt "|E|"; txt (to_string_env fenv); txt "|E|" end;*)
      print_series_of_funs lp rp f true (Some node) (Fun fn)
  | (App (App (Var v, a), b) | App (Control (_, App (Var v, a)), b)) when is_operator v ->
      let v = if String.length v > 0 && v.[0] = '[' then String.sub v 1 (String.length v - 1) else v in
      str lp;
      print_tiny_inner f true (Some node) a;
      txt (" " ^ v ^ " ");
      print_tiny_inner f false (Some node) b;
      str rp
  | App (Var v, e') when is_operator v ->
      str lp;
      print_tiny_inner f true (Some node) (Var v);
      print_tiny_inner f false (Some node) e';
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
      let first = ref true in
      List.iter (fun x -> if not !first then txt "; "; first := false; print_record_entry f x) items;
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
  | TryWith (e, patmatch) ->
      str lp;
      boldtxt "try ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      print_cases f false (Some node) patmatch;
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
  | TypeDef (recflag, type_declaration) ->
      str lp;
      print_type_declaration f isleft parent type_declaration;
      str rp

and print_modtype f isleft parent modtype =
  let str = Format.fprintf f "%s" in
  match modtype with
    ModTypeSignature e -> str "ModTypeSignature"
  | ModTypeIdent s -> str s
  | ModTypeWith _ -> str "ModTypeWith"

(* Print the list of type declarations type t = ... [and t' = ...] *) 
and print_type_declaration f isleft parent tds =
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  let first = ref true in
    List.iter
      (function td ->
         if !first then boldtxt "type " else boldtxt "and ";
         first := false;
         txt td.ptype_name.txt;
         txt " =";
         begin match td.ptype_kind with
           Ptype_abstract -> ()
         | Ptype_variant cons_decls ->
             let first = ref true in
               List.iter
                (fun cd ->
                   if not !first then txt " | " else txt " ";
                   first := false;
                   print_constuctor_declaration f isleft parent cd)
                cons_decls
         | _ -> txt "print_type_declaration not known\n"
         end)
      tds

and print_label_declaration f isleft parent labeldec =
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  if labeldec.pld_mutable = Mutable then boldtxt "mutable ";
  txt labeldec.pld_name.txt;
  txt " : ";
  print_core_type f isleft parent labeldec.pld_type

and print_constuctor_declaration f isleft parent cd =
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  txt cd.pcd_name.txt;
  begin match cd.pcd_args with
    Pcstr_tuple ts ->
      if ts <> [] then boldtxt " of ";
      let first = ref true in
      List.iter
        (fun t ->
          if not !first then txt " * ";
          first := false;
          print_core_type f isleft parent t)
        ts
  | Pcstr_record labels ->
      if labels <> [] then (boldtxt " of "; txt "{");
      let first = ref true in
        List.iter
          (fun labeldec ->
            if not !first then txt "; ";
            first := false;
            print_label_declaration f isleft parent labeldec)
          labels;
      if labels <> [] then txt "}"
  end

and print_core_type f isleft parent t =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
    match t.ptyp_desc with
      Ptyp_any -> str "_"
    | Ptyp_var s -> str "'"; str s
    | Ptyp_arrow (_, c, c') ->
        print_core_type f isleft parent c;
        txt " -> ";
        print_core_type f isleft parent c'
    | Ptyp_constr (name, ts) ->
        txt (match name.txt with Longident.Lident x -> x | _ -> "print_core_type unknwn")
    | _ -> txt "print_core_type2 unknwn"

(* We can print a list as a literal iff it has a Nil at the end of a series of
one or more conses. *)
and try_printing_literal_list f isleft parent e =
  let str = Format.fprintf f "%s" in
  let rec get_list_elements = function
    Cons (h, t) -> h::get_list_elements t
  | Nil -> []
  | _ -> raise Exit
  in
    match get_list_elements e with
      ls ->
        let l = List.length ls in
        str "[";
        List.iteri
         (fun i e ->
           print_tiny_inner f isleft parent e;
           if i < l - 1 then str "; ")
         ls;
        str "]";
        true
  | exception Exit -> false

(* Print fun x -> fun y -> fun z -> ... as fun x y z -> ... *)
and print_series_of_funs lp rp f isleft parent e =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  (* Return a list of fnames and an fexp. There will be at least one. *)
  let rec gather_funs fnames = function
     Fun (flabel, fname, fexp, _) -> gather_funs ((flabel, fname)::fnames) fexp
  |  x -> (List.rev fnames, x)
  in
    str lp;
    boldtxt "fun ";
    let names, exp = gather_funs [] e in
    List.iter
      (fun (label, x) -> print_pattern f false (Some e) x label; txt " ")
      names;
    txt "-> ";
    print_tiny_inner f false (Some e) exp;
    str rp

and print_cases f isleft parent cases =
  match cases with
    [] -> ()
  | h::t ->
      print_case ~bar:false f isleft parent h;
      List.iter (print_case f isleft parent) t

and print_case ?(bar=true) f isleft parent (pattern, guard, rhs) =
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  if bar then txt "| ";
  print_pattern f isleft parent pattern NoLabel;
  begin match guard with
  | None -> ()
  | Some g ->
      boldtxt " when ";
      print_tiny_inner f false parent g
  end;
  boldtxt " -> ";
  print_tiny_inner f false parent rhs;
  txt " "

and print_pattern f isleft parent pat label =
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
        (* Print 'v' or '~v' or '?v' or '?(v = 4)' *)
        let pvar () = str (Ocamliutil.unstar v) in
        begin match label with
          NoLabel -> pvar ();
        | Labelled s -> str "~"; pvar ()
        | Optional (s, None) -> str "?"; pvar ()
        | Optional (s, Some e) ->
            str "?("; str s; str " = ";
            print_tiny_inner f false parent e;
            str ")"
        end
    | PatBool b ->
        str (string_of_bool b)
    | PatInt i ->
        str (string_of_int i)
    | PatInt32 i ->
        str (Int32.to_string i)
    | PatInt64 i ->
        str (Int64.to_string i)
    | PatNativeInt i ->
        str (Nativeint.to_string i)
    | PatChar c ->
        str (Printf.sprintf "%C" c)
    | PatCharRange (c, c') ->
        str (Printf.sprintf "%C .. %C" c c')
    | PatString s ->
        str (Printf.sprintf "\"%s\"" (String.escaped s))
    | PatTuple items ->
        str "(";
        let l = List.length items in
          List.iteri
            (fun i x ->
               print_pattern f isleft parent x NoLabel;
               if i < l - 1 then txt ", ")
            items;
        str ")"
    | PatArray items ->
        str "[|";
        let l = Array.length items in
          Array.iteri
            (fun i x ->
               print_pattern f isleft parent x NoLabel;
               if i < l - 1 then txt "; ")
            items;
        str "|]"
    | PatNil -> str "[]"
    | PatCons (h, t) ->
        print_pattern f isleft parent h NoLabel;
        str "::";
        print_pattern f isleft parent t NoLabel
    | PatAlias (a, p) ->
        print_pattern f isleft parent p NoLabel;
        boldtxt " as ";
        str a
    | PatOr (a, b) ->
        print_pattern f isleft parent a NoLabel;
        txt " | ";
        print_pattern f isleft parent b NoLabel
    | PatConstr (name, None) ->
        txt name
    | PatConstr (name, Some p) ->
        txt name;
        txt " ";
        print_pattern f isleft parent p NoLabel
    | PatConstraint (p, typ) ->
        print_pattern f isleft parent p NoLabel;
        txt " : <<typ>>";
    | PatRecord (openflag, items) ->
        txt "{";
        List.iter
          (fun (n, p) ->
            txt n;
            txt " = ";
            print_pattern f isleft parent p NoLabel;
            txt "; ")
          items;
        if openflag then txt " _";
        txt "}"
    | PatException p ->
        boldtxt "exception ";
        print_pattern f isleft parent p NoLabel

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
  "underline" ->
    if !syntax_tex
      then Format.pp_print_string f "\\underline{"
      else Format.pp_print_string f ul
| "bold" ->
    if !syntax_tex
      then Format.pp_print_string f "\\textbf{"
      else Format.pp_print_string f bold
| _ -> failwith "format: unknown tag"

let output_tags f =
  List.iter (output_tag f) !tags

let print ?(preamble="") f t =
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
