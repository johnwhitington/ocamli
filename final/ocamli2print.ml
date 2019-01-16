open Asttypes
open Parsetree
open Types

let debug = ref false

(* Set this to false to debug failures in tinyocaml_of_ocaml_heap_value *)
let showvals = ref true

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
  Tinyocaml.Control (_, x) -> assoc x
| Tinyocaml.Op _ | Tinyocaml.Cmp _ | Tinyocaml.App _ -> L
| Tinyocaml.And _ | Tinyocaml.Or _ | Tinyocaml.Seq _ | Tinyocaml.SetField _ -> R
| _ -> N

(* FIXME Need to add Control possibilities to this, or strip controls? *)
let prec = function
  Tinyocaml.Field _ -> 110
| Tinyocaml.App (Tinyocaml.Var v, Tinyocaml.App (_, _)) when is_operator v ->
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
| Tinyocaml.App (Tinyocaml.Var v, _) when is_operator v ->
    99
| Tinyocaml.Seq _ -> 91
| Tinyocaml.App _ ->
    (* All other function applications *)
    100
| Tinyocaml.Op ((Tinyocaml.Mul | Tinyocaml.Div), _, _) -> 90
| Tinyocaml.Op (_, _, _) -> 80
| Tinyocaml.Cmp _ -> 70
| Tinyocaml.And _ -> 65
| Tinyocaml.Or _ -> 60
| Tinyocaml.SetField _ -> 55
| Tinyocaml.If _ -> 50
| Tinyocaml.Fun _ | Tinyocaml.Function _ | Tinyocaml.Let _ | Tinyocaml.LetDef _ -> 10
| Tinyocaml.Struct _
| Tinyocaml.Tuple _
| Tinyocaml.Cons _ | Tinyocaml.Constr (_, _, _) | Tinyocaml.Var _ | Tinyocaml.TypeDef _ | Tinyocaml.While _ | Tinyocaml.For _ | Tinyocaml.Raise _
| Tinyocaml.Match _ | Tinyocaml.TryWith _ | Tinyocaml.ExceptionDef _ | Tinyocaml.Control _ | Tinyocaml.CallBuiltIn _ | Tinyocaml.Sig _
| Tinyocaml.ModuleBinding _ | Tinyocaml.ModuleConstraint _ | Tinyocaml.ModuleIdentifier _ | Tinyocaml.ModuleApply _
| Tinyocaml.Functor (_, _, _) | Tinyocaml.Append _ | Tinyocaml.Assert _ | Tinyocaml.Open _ | Tinyocaml.LocalOpen _ | Tinyocaml.Include _
| Tinyocaml.Lazy _ | Tinyocaml.Annot (_, _, _) -> 0
| Tinyocaml.Unit | Tinyocaml.Nil | Tinyocaml.Int _ | Tinyocaml.Bool _ | Tinyocaml.Float _ | Tinyocaml.String _ | Tinyocaml.OutChannel _
| Tinyocaml.InChannel _ | Tinyocaml.Record _ | Tinyocaml.Int32 _ | Tinyocaml.Int64 _ | Tinyocaml.NativeInt _ | Tinyocaml.Char _
| Tinyocaml.Array _ -> max_int

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
  Tinyocaml.Underline -> "underline"
| Tinyocaml.Bold -> "bold"

let rec find_funs e =
  match e with
    Tinyocaml.Fun (flabel, fname, fexp, _) ->
      let more, e' = find_funs fexp in
        ((fname, flabel)::more, e')
  | _ -> ([], e)

let pp_constructor_arg pp = function
  Pcstr_tuple coretypes ->
    Pprintast.core_type pp
      {ptyp_desc = Ptyp_tuple coretypes;
       ptyp_loc = Location.none; 
       ptyp_attributes = [];
       ptyp_loc_stack = []}
| _ -> failwith "unimplemented record type"

let string_of_op = function
  Tinyocaml.Add -> "+" | Tinyocaml.Sub -> "-" | Tinyocaml.Mul -> "*" | Tinyocaml.Div -> "/"

let string_of_cmp = function
  Tinyocaml.LT -> "<" | Tinyocaml.EQ -> "=" | Tinyocaml.GT -> ">" | Tinyocaml.EQLT -> "<=" | Tinyocaml.EQGT -> ">=" | Tinyocaml.NEQ -> "<>"

let rec print_tiny_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  let lp, rp = parens node parent isleft in
  match node with
  | Tinyocaml.Include e ->
      str lp;
      boldtxt "include ";
      print_tiny_inner f isleft (Some node) e;
      str rp
  | Tinyocaml.ModuleApply (m1, m2) ->
      str lp;
      print_tiny_inner f isleft (Some node) m1;
      txt "(";
      print_tiny_inner f isleft (Some node) m2;
      txt ")";
      str rp
  | Tinyocaml.Functor (n, mt, Tinyocaml.ModuleConstraint (mtc, me)) ->
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
  | Tinyocaml.Functor (n, mt, me) ->
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
  | Tinyocaml.Lazy e ->
      str lp;
      boldtxt "lazy ";
      print_tiny_inner f isleft (Some node) e;
      str rp
  | Tinyocaml.Open x ->
      str lp;
      boldtxt "open ";
      txt x;
      str rp;
      txt "\n";
  | Tinyocaml.LocalOpen (x, e) ->
      str lp;
      txt x;
      txt ".";
      str "(";
      print_tiny_inner f isleft (Some node) e;
      str ")";
      str rp
  | Tinyocaml.Assert e ->
      str lp;
      boldtxt "assert ";
      print_tiny_inner f false (Some node) e;
      str rp;
  | Tinyocaml.Match (e, patmatch) ->
      str lp;
      boldtxt "match ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      print_cases f false (Some node) patmatch;
      str rp
  | Tinyocaml.Function (patmatch, _) ->
      str lp;
      boldtxt "function ";
      print_cases f false (Some node) patmatch;
      str rp
  | Tinyocaml.Struct (b, structure_items) ->
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
  | Tinyocaml.Sig (sig_items) ->
      boldtxt "sig \n";
      let l = List.length sig_items in
        List.iteri
          (fun i x ->
             print_tiny_inner f false (Some node) x;
             if i < l - 1 then txt "\n\n")
          sig_items;
      txt "\n";
      boldtxt "end"
  | Tinyocaml.ModuleBinding (n, Tinyocaml.ModuleConstraint (t, e)) ->
      boldtxt "module ";
      txt n;
      txt " : ";
      print_modtype f false (Some node) t;
      txt " = \n";
      print_tiny_inner f false (Some node) e
  | Tinyocaml.ModuleBinding (n, e) ->
      boldtxt "module ";
      txt n;
      txt " = \n";
      print_tiny_inner f false (Some node) e
  | Tinyocaml.ModuleConstraint (t, e) ->
      ()
  | Tinyocaml.ModuleIdentifier x ->
      txt x
  | Tinyocaml.Tuple xs ->
      let l = List.length xs in
      str "(";
      List.iteri
        (fun i x ->
           print_tiny_inner f false (Some node) x;
           if i < l - 1 then txt ", ")
        xs;
      str ")"
  | Tinyocaml.Array xs -> 
      let l = Array.length xs in
      str "[|";
      Array.iteri
        (fun i x ->
           print_tiny_inner f false (Some node) x;
           if i < l - 1 then txt "; ")
        xs;
      str "|]"
  | Tinyocaml.Cons (x, y) ->
      if not (try_printing_literal_list f isleft parent node) then begin
        print_tiny_inner f isleft parent x;
        str "::";
        print_tiny_inner f isleft parent y
      end
  | Tinyocaml.Append (x, y) ->
      print_tiny_inner f isleft parent x;
      txt " @ ";
      print_tiny_inner f isleft parent y
  | Tinyocaml.Nil ->
      str "[]"
  | Tinyocaml.Control (tag, x) ->
      Format.pp_open_tag f (string_of_tag tag);
      print_tiny_inner f isleft parent x;
      Format.pp_close_tag f ()
  | Tinyocaml.Annot (n, p, x) ->
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
  | Tinyocaml.Unit -> str "()"
  | Tinyocaml.Int i -> str (string_of_int i)
  | Tinyocaml.Int32 i -> str (Int32.to_string i ^ "l")
  | Tinyocaml.Int64 i -> str (Int64.to_string i ^ "L")
  | Tinyocaml.NativeInt i -> str (Nativeint.to_string i ^ "n")
  | Tinyocaml.Bool b -> str (string_of_bool b)
  | Tinyocaml.Float f -> str (string_of_float f)
  | Tinyocaml.String s -> str ("\"" ^ String.escaped (Bytes.to_string s) ^ "\"")
  | Tinyocaml.Char c -> str (Printf.sprintf "%C" c)
  | Tinyocaml.OutChannel s -> str "<out_channel>"
  | Tinyocaml.InChannel s -> str "<in_channel>"
  | Tinyocaml.CallBuiltIn (typ, name, args, fn) -> str "<<"; str name; str ">>"
  | Tinyocaml.Var v -> str (Ocamli2util.unstar v)
  | Tinyocaml.Constr (_, s, None) -> str s
  | Tinyocaml.Constr (_, s, Some x) ->
      str s;
      str " ";
      str lp;
      print_tiny_inner f false (Some node) x;
      str rp
  | Tinyocaml.Op (op, l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " ";
      str (string_of_op op);
      txt " ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Tinyocaml.Cmp (cmp, l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " ";
      str (string_of_cmp cmp);
      txt " ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Tinyocaml.And (l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " && ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Tinyocaml.Or (l, r) ->
      str lp;
      print_tiny_inner f true (Some node) l;
      txt " || ";
      print_tiny_inner f false (Some node) r;
      str rp
  | Tinyocaml.If (e, e1, e2) ->
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
  | Tinyocaml.Let (recflag, bindings, e') ->
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
             print_pattern f false (Some node) v Tinyocaml.NoLabel;
             txt " ";
             let morefuns, e = find_funs e in
             List.iter (fun (p, l) -> print_pattern f false (Some node) p l; txt " ") morefuns;
             txt "= ";
             print_tiny_inner f false (Some node) e)
          bindings;
      boldtxt " in ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Tinyocaml.LetDef (recflag, bindings) ->
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
  | Tinyocaml.Fun ((_, _, _, fenv) as fn) ->
      (*if !debug then begin txt "|E|"; txt (to_string_env fenv); txt "|E|" end;*)
      print_series_of_funs lp rp f true (Some node) (Tinyocaml.Fun fn)
  | (Tinyocaml.App (Tinyocaml.App (Tinyocaml.Var v, a), b) | Tinyocaml.App (Tinyocaml.Control (_, Tinyocaml.App (Tinyocaml.Var v, a)), b)) when is_operator v ->
      let v = if String.length v > 0 && v.[0] = '[' then String.sub v 1 (String.length v - 1) else v in
      str lp;
      print_tiny_inner f true (Some node) a;
      txt (" " ^ v ^ " ");
      print_tiny_inner f false (Some node) b;
      str rp
  | Tinyocaml.App (Tinyocaml.Var v, e') when is_operator v ->
      str lp;
      print_tiny_inner f true (Some node) (Var v);
      print_tiny_inner f false (Some node) e';
      str rp
  | Tinyocaml.App (e, e') ->
      str lp;
      print_tiny_inner f true (Some node) e;
      txt " ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Tinyocaml.Seq (e, e') ->
      str lp;
      print_tiny_inner f true (Some node) e;
      txt "; ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Tinyocaml.While (e, e', _, _) ->
      str lp;
      boldtxt "while ";
      print_tiny_inner f false (Some node) e;
      boldtxt " do ";
      print_tiny_inner f false (Some node) e';
      boldtxt " done";
      str rp
  | Tinyocaml.For (var, e, flag, e', e'', _) ->
      str lp;
      boldtxt "for ";
      str var;
      txt " = ";
      print_tiny_inner f false (Some node) e;
      txt ((function Tinyocaml.UpTo -> " to " | Tinyocaml.DownTo -> " down ") flag);
      print_tiny_inner f false (Some node) e';
      boldtxt " do ";
      print_tiny_inner f false (Some node) e'';
      boldtxt " done"
  | Tinyocaml.Record items ->
      str "{";
      let first = ref true in
      List.iter (fun x -> if not !first then txt "; "; first := false; print_record_entry f x) items;
      str "}"
  | Tinyocaml.Field (e, n) ->
      str lp;
      print_tiny_inner f false (Some node) e;
      str ".";
      str n;
      str rp
  | Tinyocaml.SetField (e, n, e') ->
      str lp;
      print_tiny_inner f false (Some node) e;
      str ".";
      str n;
      txt " <- ";
      print_tiny_inner f false (Some node) e';
      str rp
  | Tinyocaml.Raise (e, payload) ->
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
  | Tinyocaml.TryWith (e, patmatch) ->
      str lp;
      boldtxt "try ";
      print_tiny_inner f false (Some node) e;
      boldtxt " with ";
      print_cases f false (Some node) patmatch;
      str rp
  | Tinyocaml.ExceptionDef (e, t) ->
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
  | Tinyocaml.TypeDef (recflag, type_declaration) ->
      str lp;
      print_type_declaration f isleft parent type_declaration;
      str rp

and print_modtype f isleft parent modtype =
  let str = Format.fprintf f "%s" in
  match modtype with
    Tinyocaml.ModTypeSignature e -> str "ModTypeSignature"
  | Tinyocaml.ModTypeIdent s -> str s
  | Tinyocaml.ModTypeWith _ -> str "ModTypeWith"

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
    Tinyocaml.Cons (h, t) -> h::get_list_elements t
  | Tinyocaml.Nil -> []
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
     Tinyocaml.Fun (flabel, fname, fexp, _) -> gather_funs ((flabel, fname)::fnames) fexp
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
      Tinyocaml.PatAny ->
        str "_"
    | Tinyocaml.PatUnit ->
        str "()"
    | Tinyocaml.PatVar v ->
        (* Print 'v' or '~v' or '?v' or '?(v = 4)' *)
        let pvar () = str (Ocamli2util.unstar v) in
        begin match label with
          NoLabel -> pvar ();
        | Labelled s -> str "~"; pvar ()
        | Optional (s, None) -> str "?"; pvar ()
        | Optional (s, Some e) ->
            str "?("; str s; str " = ";
            print_tiny_inner f false parent e;
            str ")"
        end
    | Tinyocaml.PatBool b ->
        str (string_of_bool b)
    | Tinyocaml.PatInt i ->
        str (string_of_int i)
    | Tinyocaml.PatInt32 i ->
        str (Int32.to_string i)
    | Tinyocaml.PatInt64 i ->
        str (Int64.to_string i)
    | Tinyocaml.PatNativeInt i ->
        str (Nativeint.to_string i)
    | Tinyocaml.PatChar c ->
        str (Printf.sprintf "%C" c)
    | Tinyocaml.PatCharRange (c, c') ->
        str (Printf.sprintf "%C .. %C" c c')
    | Tinyocaml.PatString s ->
        str (Printf.sprintf "\"%s\"" (String.escaped s))
    | Tinyocaml.PatTuple items ->
        str "(";
        let l = List.length items in
          List.iteri
            (fun i x ->
               print_pattern f isleft parent x NoLabel;
               if i < l - 1 then txt ", ")
            items;
        str ")"
    | Tinyocaml.PatArray items ->
        str "[|";
        let l = Array.length items in
          Array.iteri
            (fun i x ->
               print_pattern f isleft parent x NoLabel;
               if i < l - 1 then txt "; ")
            items;
        str "|]"
    | Tinyocaml.PatNil -> str "[]"
    | Tinyocaml.PatCons (h, t) ->
        print_pattern f isleft parent h NoLabel;
        str "::";
        print_pattern f isleft parent t NoLabel
    | Tinyocaml.PatAlias (a, p) ->
        print_pattern f isleft parent p NoLabel;
        boldtxt " as ";
        str a
    | Tinyocaml.PatOr (a, b) ->
        print_pattern f isleft parent a NoLabel;
        txt " | ";
        print_pattern f isleft parent b NoLabel
    | Tinyocaml.PatConstr (name, None) ->
        txt name
    | Tinyocaml.PatConstr (name, Some p) ->
        txt name;
        txt " ";
        print_pattern f isleft parent p NoLabel
    | Tinyocaml.PatConstraint (p, typ) ->
        print_pattern f isleft parent p NoLabel;
        txt " : <<typ>>";
    | Tinyocaml.PatRecord (openflag, items) ->
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
    | Tinyocaml.PatException p ->
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



(* FIXME: Need to update this to only run on stdout or something *)
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
| Tlink x -> string_of_ocaml_type (Ocamli2type.find_type_desc x)
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let rec tinyocaml_of_ocaml_heap_value (typ : type_desc) (value : Obj.t) =
  (*Printf.printf "tinyocaml_of_ocaml_heap_value: %s\n" (string_of_ocaml_type typ);*)
  match typ with
    Tconstr (p, _, _) when Path.name p = "int" -> Tinyocaml.Int (Obj.magic value : int)
  | Tconstr (p, _, _) when Path.name p = "float" -> Tinyocaml.Float (Obj.magic value : float)
  | Tconstr (p, _, _) when Path.name p = "unit" -> Tinyocaml.Unit
  | Tconstr (p, [elt_t], _) when Path.name p = "array" ->
     Tinyocaml.Array
        (Array.init
          (Obj.size value)
          (fun i -> tinyocaml_of_ocaml_heap_value (Ocamli2type.find_type_desc elt_t) (Obj.field value i)))
  | Tconstr (p, [elt_t], _) when Path.name p = "list" ->
      if Obj.is_int value then Tinyocaml.Nil else
        Tinyocaml.Cons
          (tinyocaml_of_ocaml_heap_value (Ocamli2type.find_type_desc elt_t) (Obj.field value 0),
           tinyocaml_of_ocaml_heap_value typ (Obj.field value 1))
  | _ -> if !showvals
           then failwith "tinyocaml_of_ocaml_heap_value: unknown type"
           else String (Bytes.of_string "<unknown val>")


(* From the former ocamli2write.ml *)
let show_all_lets = ref false

(* For now, convert to tinyocaml thence to pptinyocaml. Soon, we will need our own prettyprinter, of course *)
let tinyocaml_op_of_finaltype_op = function
  Ocamli2type.Add -> Tinyocaml.Add
| Ocamli2type.Sub -> Tinyocaml.Sub
| Ocamli2type.Mul -> Tinyocaml.Mul
| Ocamli2type.Div -> Tinyocaml.Div

let rec tinyocaml_of_finaltype_t' typ = function
  Ocamli2type.Value x -> tinyocaml_of_ocaml_heap_value typ x
| Ocamli2type.Function (cases, env) -> Function (List.map tinyocaml_of_finaltype_case cases, [])
| Ocamli2type.Apply (e, args) -> tinyocaml_of_finaltype_apply e args
| Ocamli2type.Var x -> Var x
| Ocamli2type.ArrayExpr arr -> Array (Array.map tinyocaml_of_finaltype arr)
| Ocamli2type.Cons (h, t) -> Cons (tinyocaml_of_finaltype h, tinyocaml_of_finaltype t)
| Ocamli2type.Append (a, b) -> Append (tinyocaml_of_finaltype a, tinyocaml_of_finaltype b)
| Ocamli2type.IntOp (op, x, y) ->
    Op
      (tinyocaml_op_of_finaltype_op op,
       tinyocaml_of_finaltype x,
       tinyocaml_of_finaltype y)
| Ocamli2type.FOp (op, x, y) ->
    App
      ((App (Var "Stdlib.+.", tinyocaml_of_finaltype x)), tinyocaml_of_finaltype y)
| Ocamli2type.ArrayGet (x, y) ->
    App
      ((App
        (Var "Stdlib.Array.get", tinyocaml_of_finaltype x)),
      (tinyocaml_of_finaltype y))
| Ocamli2type.ArraySet (arr, index, newval) ->
    App
      (App
        ((App
          (Var "Stdlib.Array.set", tinyocaml_of_finaltype arr)),
        (tinyocaml_of_finaltype index)),
        (tinyocaml_of_finaltype newval))
| Ocamli2type.Let (recflag, (n, a), b) ->
    Let
      (recflag,
       [(PatVar n, tinyocaml_of_finaltype a)],
       tinyocaml_of_finaltype b)
| Ocamli2type.Match (e, cases) ->
    Match
      (tinyocaml_of_finaltype e,
       List.map tinyocaml_of_finaltype_case cases)
| Ocamli2type.Struct ls ->
    Struct (false, List.map tinyocaml_of_finaltype ls)
| Ocamli2type.LetDef (recflag, (n, e)) ->
    LetDef (recflag, [(PatVar n, tinyocaml_of_finaltype e)]) 

(* Here, e is the function, and the next argument is all the args in order. *)
and tinyocaml_of_finaltype_apply_inner (e : Tinyocaml.t) (args : Tinyocaml.t list) =
  match args with
    h::t ->
     tinyocaml_of_finaltype_apply_inner (Tinyocaml.App (e, h)) t
  | [] -> e

and tinyocaml_of_finaltype_apply e args =
  tinyocaml_of_finaltype_apply_inner
    (tinyocaml_of_finaltype e)
    (List.map tinyocaml_of_finaltype args)

and tinyocaml_of_finaltype_case (pat, guard, rhs) =
  (tinyocaml_of_finaltype_pattern pat,
   tinyocaml_of_finaltype_guard guard,
   tinyocaml_of_finaltype rhs)

and tinyocaml_of_finaltype_guard = function
  None -> None
| Some g -> Some (tinyocaml_of_finaltype g)

and tinyocaml_of_finaltype_pattern = function
  Ocamli2type.PatAny -> PatAny
| Ocamli2type.PatConstr ("[]", []) -> PatNil
| Ocamli2type.PatVar v -> PatVar v
| Ocamli2type.PatConstr ("::", [h; t]) ->
    PatCons (tinyocaml_of_finaltype_pattern h, tinyocaml_of_finaltype_pattern t)
| Ocamli2type.PatConstant (IntConstant i) ->
    PatInt i
| _ -> failwith "tinyocaml_of_finaltype_pattern: unknown"

(* FIXME Need to remove anything shadowed by a name binding because of a pattern in a pattern match too *)
  (* If any implicit lets, fabricate them -- but only if they are used in the
   * expression underneath, and not shadowed. *)
  (*Printf.printf "We have %i lets\n" (List.length lets);*)

and basiclets_of_envitem (recflag, r) =
  List.map
    (fun (n, e) -> (recflag, n, e))
    !r

and basiclets_of_env env =
  List.flatten (List.map basiclets_of_envitem env)

and tinyocaml_of_finaltype {e; typ; lets} =
  let remove_names_from_lets names =
    List.filter (fun (_, v, _) -> List.mem v names)
  in
  let rec remove_shadowed_implicits = function
    [] -> []
  | (recflag, n, e)::r ->
      if List.mem n (List.map (fun (_, x, _) -> x) r)
        then remove_shadowed_implicits r
        else (recflag, n, e)::remove_shadowed_implicits r
  in
  let rec fabricate_lets e = function
    [] -> e
  | (recflag, n, rhs)::r ->
      fabricate_lets (Tinyocaml.Let (recflag, [(Tinyocaml.PatVar n, tinyocaml_of_finaltype rhs)], e)) r
  in
  let inner = tinyocaml_of_finaltype_t' typ e in
    if lets = [] then inner else
      let names = Ocamli2type.names_in_t' e in
      (*Printf.printf "%i names in t'\n" (List.length names);*)
      (* FIXME. For now, we convert lets to just (recflag, n, e) "basiclets" to make it easier to deal with.
      Eventually, once we have let...and and mutual recursion, we must do it properly. *)
      let lets_to_print =
        if !show_all_lets then basiclets_of_env lets else
          remove_names_from_lets names (remove_shadowed_implicits (basiclets_of_env lets))
      in
        (*Printf.printf "lets to print: %i\n" (List.length lets_to_print);*)
        fabricate_lets inner (List.rev lets_to_print)



let print_finaltype_inner f isleft parent node =
  let str = Format.fprintf f "%s" in
  let txt = Format.pp_print_text f in
  let bold () = Format.pp_open_tag f (string_of_tag Bold) in
  let unbold () = Format.pp_close_tag f () in
  let boldtxt t = bold (); txt t; unbold () in
  (*let lp, rp = parens node parent isleft in*)
  boldtxt "\nNEW: "

let output_tags f =
  List.iter (output_tag f) !tags

let print ?(preamble="") f (t : Tinyocaml.t) (v : Ocamli2type.t) =
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
    print_finaltype_inner f true None v;
    Format.pp_close_box f ();
    Format.pp_print_flush f ()

let to_string ?(preamble="") t v =
  print ~preamble Format.str_formatter t v;
  Format.flush_str_formatter ()

let to_string_from_finaltype ?(preamble="") v =
  to_string (tinyocaml_of_finaltype v) v

let to_string_from_heap ?(preamble="") typ v =
  to_string
    (tinyocaml_of_ocaml_heap_value typ v)
    {Ocamli2type.e = Ocamli2type.Value v;
     Ocamli2type.lets = [];
     Ocamli2type.typ = typ}

let string_of_op = function
    Ocamli2type.Add -> "Add"
  | Ocamli2type.Sub -> "Sub"
  | Ocamli2type.Mul -> "Mul"
  | Ocamli2type.Div -> "Div" 


let rec string_of_t' typ = function
  Ocamli2type.Value x -> to_string_from_heap typ x
| Ocamli2type.Function (cases, env) ->
    Printf.sprintf "Function (%s, env = %s)" (string_of_cases cases) (string_of_env env)
| Ocamli2type.Apply (e, args) ->
    Printf.sprintf "Apply (%s, [%s])" (string_of_t e) (string_of_items args)
| Ocamli2type.Var x -> Printf.sprintf "Var %s" x
| Ocamli2type.ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| Ocamli2type.Cons (a, b) ->
    Printf.sprintf
      "Cons (%s, %s)" (string_of_t a) (string_of_t b)
| Ocamli2type.Append (a, b) ->
    Printf.sprintf
      "Append (%s, %s)" (string_of_t a) (string_of_t b)
| Ocamli2type.FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| Ocamli2type.IntOp (op, a, b) ->
    Printf.sprintf
      "IntOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| Ocamli2type.ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_t arr) (string_of_t i)
| Ocamli2type.ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_t arr) (string_of_t i) (string_of_t newval)
| Ocamli2type.Let (recflag, (n, e), e') ->
    Printf.sprintf
      "Let (%b, %s, %s, %s)"
      recflag n (string_of_t e) (string_of_t e')
| Ocamli2type.Match (e, cases) ->
    Printf.sprintf "Match (%s, %s)" (string_of_t e) "<cases>"
| Ocamli2type.Struct l ->
    Printf.sprintf "Struct:%s\n"
      (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (List.map string_of_t l))
| Ocamli2type.LetDef (recflag, (n, e)) ->
    Printf.sprintf "LetDef %b (%s, %s)"
      recflag n (string_of_t e)

and string_of_case (p, _, e) = (* FIXME guard *)
  Printf.sprintf "[%s -> %s]" (string_of_pattern p) (string_of_t e)
     
and string_of_cases cases =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_case x ^ " ") cases)

and string_of_pattern = function
  Ocamli2type.PatAny -> "_"
| Ocamli2type.PatVar v -> v
| Ocamli2type.PatConstr (constr, pats) ->
    "PatConstr " ^ constr ^ "("
  ^ List.fold_left ( ^ ) "" (List.map (fun x -> string_of_pattern x ^ ", ") pats)
  ^ ")"
| Ocamli2type.PatConstant (Ocamli2type.IntConstant i) -> string_of_int i

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_t x ^ ";") items)

and string_of_t {typ; e; lets} =
  List.fold_left ( ^ ) ""
    (List.map
    (fun (recflag, r) ->
       Printf.sprintf "{%b, %s}" recflag (string_of_bindings !r))
    lets)
  ^
  "{typ = " ^ string_of_ocaml_type typ ^ "}" 
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



