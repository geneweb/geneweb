(* camlp4r ./pa_html.cmo *)
(* $Id: templ.ml,v 4.3 2001-06-13 14:35:27 ddr Exp $ *)

open Config;
open Util;
open Def;

(* Parsing *)

type ast =
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and char
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of string and list ast_expr ]
and ast_expr =
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eop of string and ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Eint of string
  | Evar of string and list string
  | Etransl of bool and string and char ]
;

type token =
  [ BANGEQUAL
  | COMMA
  | DOT
  | EQUAL
  | LPAREN
  | RPAREN
  | IDENT of string
  | STRING of string
  | INT of string
  | LEXICON of bool and string and char ]
;

value rec get_ident len =
  parser
  [ [: `('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c); s :] ->
      get_ident (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value rec get_string len =
  parser
  [ [: `'"' :] -> Buff.get len
  | [: `c; s :] -> get_string (Buff.store len c) s ]
;

value rec get_int len =
  parser
  [ [: `('0'..'9' as c); s :] -> get_int (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value get_variable =
  let rec var_kont =
    parser
    [ [: `'.'; s = get_ident 0; sl = var_kont :] -> [s :: sl]
    | [: `';' :] -> []
    | [: :] -> [] ]
  in
  parser
  [ [: `'%' :] -> ("%", [])
  | [: v = get_ident 0; vl = var_kont :] -> (v, vl) ]
;

value lexicon_word =
  let upper = parser [ [: `'*' :] -> True | [: :] -> False ] in
  let rec text len =
    parser
    [ [: `']' :] -> Buff.get len
    | [: `c; s :] -> text (Buff.store len c) s ]
  in
  parser [: upp = upper; s = text 0; `n :] -> (upp, s, n)
;

value rec get_token =
  parser
  [ [: `' ' | '\t' | '\n' | '\r'; s :] -> get_token s
  | [: `'(' :] -> LPAREN
  | [: `')' :] -> RPAREN
  | [: `',' :] -> COMMA
  | [: `'.' :] -> DOT
  | [: `'=' :] -> EQUAL
  | [: `'!'; `'=' :] -> BANGEQUAL
  | [: `'"'; s = get_string 0 :] -> STRING s
  | [: `('0'..'9' as c); s = get_int (Buff.store 0 c) :] -> INT s
  | [: `'['; (upp, s, n) = lexicon_word :] -> LEXICON upp s n
  | [: s = get_ident 0 :] -> IDENT s ]
;

value buff = ref (String.create 80);

value buff_store len x =
  do {
    if len >= String.length buff.val then
      buff.val := buff.val ^ String.create (String.length buff.val)
    else ();
    buff.val.[len] := x;
    succ len
  }
;

value buff_mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len
    else add_rec (buff_store len s.[i]) (succ i)
;

value buff_get len = String.sub buff.val 0 len;

value rec parse_var = parser [: `IDENT id; idl = ident_list :] -> (id, idl)
and ident_list =
  parser
  [ [: `DOT;
       id = parser [ [: `IDENT id :] -> id | [: `_ :] -> "parse_error" ];
       idl = ident_list :] ->
      [id :: idl]
  | [: :] -> [] ]
;

value rec parse_expr strm =
  let rec parse_1 =
    parser
      [: e = parse_2;
         a =
           parser
           [ [: `IDENT "or"; s :] -> Eor e (parse_1 s)
           | [: :] -> e ] :] ->
        a
  and parse_2 =
    parser
      [: e = parse_3;
         a =
           parser
           [ [: `IDENT "and"; s :] -> Eand e (parse_2 s)
           | [: :] -> e ] :] ->
        a
  and parse_3 =
    parser
      [: e = parse_simple;
         a =
           parser
           [ [: `EQUAL; e2 = parse_simple :] -> Eop "=" e e2
           | [: `BANGEQUAL; e2 = parse_simple :] -> Eop "!=" e e2
           | [: :] -> e ] :] ->
        a
  and parse_simple =
    parser
    [ [: `LPAREN; e = parse_1;
         a =
           parser
           [ [: `RPAREN :] -> e
           | [: `_ :] -> Evar "parse_error" [] ] :] ->
        a
    | [: `IDENT "not"; e = parse_simple :] -> Enot e
    | [: `STRING s :] -> Estr s
    | [: `INT s :] -> Eint s
    | [: (id, idl) = parse_var :] -> Evar id idl
    | [: `_ :] -> Evar "parse_error" [] ]
  in
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  let r = parse_simple (Stream.from f) in
  do { match strm with parser [ [: `';' :] -> () | [: :] -> () ]; r }
;

value parse_real_params strm =
  let expr =
    parser
    [ [: `STRING s :] -> Estr s
    | [: `LEXICON upp s n :] -> Etransl upp s n
    | [: (id, idl) = parse_var :] -> Evar id idl ]
  in
  let rec parse_expr_list =
    parser
      [: x = expr;
         xl =
           parser
           [ [: `COMMA; a = parse_expr_list :] -> a
           | [: :] -> [] ] :] ->
        [x :: xl]
  in
  let parse_tuple =
    parser
      [: `LPAREN;
         xl = parser [ [: a = parse_expr_list :] -> a | [: :] -> [] ];
         a =
           parser
           [ [: `RPAREN :] -> xl
           | [: :] -> [Estr "parse_error"] ] :] ->
        a
  in
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  parse_tuple (Stream.from f)
;

value parse_formal_params strm =
  let rec parse_ident_list =
    parser
      [: `IDENT x;
         xl =
           parser
           [ [: `COMMA; a = parse_ident_list :] -> a
           | [: :] -> [] ] :] ->
        [x :: xl]
  in
  let parse_tuple =
    parser
      [: `LPAREN;
         xl = parser [ [: a = parse_ident_list :] -> a | [: :] -> [] ];
         a = parser [ [: `RPAREN :] -> xl | [: :] -> ["parse_error"] ] :] ->
        a
  in
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  parse_tuple (Stream.from f)
;

value parse_templ conf base strm =
  let rec parse_astl astl bol len end_list strm =
    match strm with parser
    [ [: `'%' :] ->
        let astl = if len = 0 then astl else [Atext (buff_get len) :: astl] in
        match get_variable strm with
        [ ("%", []) -> parse_astl [Atext "%" :: astl] False 0 end_list strm
        | (v, []) when List.mem v end_list -> (List.rev astl, v)
        | ("define", []) -> parse_define astl end_list strm
        | x ->
            let ast =
              match x with
              [ ("if", []) -> parse_if strm
              | ("foreach", []) -> parse_foreach strm
              | ("apply", []) -> parse_apply strm
              | ("wid_hei", []) -> Awid_hei (get_ident 0 strm)
              | (v, vl) -> Avar v vl ]
            in
            parse_astl [ast :: astl] False 0 end_list strm ]
    | [: `'[' :] ->
        let astl = if len = 0 then astl else [Atext (buff_get len) :: astl] in
        let a = let (x, y, z) = lexicon_word strm in Atransl x y z in
        parse_astl [a :: astl] False 0 end_list strm
    | [: `c :] ->
        let empty_c = c = ' ' || c = '\t' in
        let len = if empty_c && bol then len else buff_store len c in
        let bol = empty_c && bol || c = '\n' in
        parse_astl astl bol len end_list strm
    | [: :] ->
        let astl = if len = 0 then astl else [Atext (buff_get len) :: astl] in
        (List.rev astl, "") ]
  and parse_define astl end_list strm =
    let fxlal =
      try
        let f = get_ident 0 strm in
        let xl = parse_formal_params strm in
        let (al, _) = parse_astl [] False 0 ["end"] strm in Some (f, xl, al)
      with
      [ Stream.Failure | Stream.Error _ -> None ]
    in
    let (alk, v) = parse_astl [] False 0 end_list strm in
    let astl =
      match fxlal with
      [ Some (f, xl, al) -> [Adefine f xl al alk :: astl]
      | None -> [Atext "define error" :: alk @ astl] ]
    in
    (List.rev astl, v)
  and parse_apply strm =
    try
      let f = get_ident 0 strm in
      let el = parse_real_params strm in Aapply f el
    with
    [ Stream.Failure | Stream.Error _ -> Atext "apply error" ]
  and parse_if strm =
    let e = parse_expr strm in
    let (al1, al2) =
      loop () where rec loop () =
        let (al1, tok) =
          parse_astl [] False 0 ["elseif"; "else"; "end"] strm
        in
        match tok with
        [ "elseif" ->
            let e2 = parse_expr strm in
            let (al2, al3) = loop () in (al1, [Aif e2 al2 al3])
        | "else" ->
            let (al2, _) = parse_astl [] False 0 ["end"] strm in (al1, al2)
        | _ -> (al1, []) ]
    in
    Aif e al1 al2
  and parse_foreach strm =
    let (v, vl) = get_variable strm in
    let (astl, _) = parse_astl [] False 0 ["end"] strm in Aforeach v vl astl
  in
  fst (parse_astl [] True 0 [] strm)
;

value open_templ conf dir name =
  let std_fname =
    List.fold_right Filename.concat [lang_dir.val; "etc"] (name ^ ".txt")
  in
  if dir = "" then try Some (open_in std_fname) with [ Sys_error _ -> None ]
  else
    let dir = Filename.basename dir in
    let fname =
      Filename.concat (Util.base_path ["etc"] dir) (name ^ ".txt")
    in
    try Some (open_in fname) with
    [ Sys_error _ ->
        if dir = conf.bname then
          try Some (open_in std_fname) with [ Sys_error _ -> None ]
        else None ]
;

value strip_newlines_after_variables =
  loop where rec loop =
    fun
    [ [Atext s :: astl] ->
        let s =
          if s.[0] = '\n' then String.sub s 1 (String.length s - 1) else s
        in
        [Atext s :: loop astl]
    | [Aif s alt ale :: astl] -> [Aif s (loop alt) (loop ale) :: loop astl]
    | [Aforeach s sl al :: astl] -> [Aforeach s sl (loop al) :: loop astl]
    | [Adefine f x al alk :: astl] ->
        [Adefine f x (loop al) (loop alk) :: loop astl]
    | [(Avar _ _ | Aapply _ _ as ast) :: astl] -> [ast :: loop astl]
    | [(Atransl _ _ _ | Awid_hei _ as ast1); ast2 :: astl] ->
        [ast1; ast2 :: loop astl]
    | [ast] -> [ast]
    | [] -> [] ]
;

value input conf base fname =
  let config_templ =
    try
      let s = List.assoc "template" conf.base_env in
      let rec loop list i len =
        if i == String.length s then List.rev [Buff.get len :: list]
        else if s.[i] = ',' then loop [Buff.get len :: list] (i + 1) 0
        else loop list (i + 1) (Buff.store len s.[i])
      in
      loop [] 0 0
    with
    [ Not_found -> [conf.bname; "*"] ]
  in
  let dir =
    match p_getenv conf.env "templ" with
    [ Some x when List.mem "*" config_templ -> x
    | Some x when List.mem x config_templ -> x
    | Some _ | None ->
        match config_templ with
        [ [] | ["*"] -> ""
        | [x :: _] -> x ] ]
  in
  let dir = Filename.basename dir in
  match open_templ conf dir fname with
  [ Some ic ->
      let astl = parse_templ conf base (Stream.of_channel ic) in
      let astl = strip_newlines_after_variables astl in
      do { close_in ic; astl }
  | None ->
      let title _ = Wserver.wprint "Error" in
      do {
        Util.header conf title;
        tag "ul" begin
          html_li conf;
          Wserver.wprint "Cannot access file \"%s/%s.txt\".\n" dir fname;
        end;
        Util.trailer conf;
        raise Exit
      } ]
;

(* Common evaluation functions *)

value subst_text x v s =
  if String.length x = 0 then s
  else
    let rec loop len i i_ok =
      if i = String.length s then
        if i_ok > 0 then loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
        else Buff.get len
      else if s.[i] = x.[i_ok] then
        if i_ok = String.length x - 1 then loop (Buff.mstore len v) (i + 1) 0
        else loop len (i + 1) (i_ok + 1)
      else if i_ok > 0 then
        loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
      else loop (Buff.store len s.[i]) (i + 1) 0
    in
    loop 0 0 0
;

value rec subst sf =
  fun
  [ Atext s -> Atext (sf s)
  | Avar s sl -> Avar (sf s) (List.map sf sl)
  | Atransl b s c -> Atransl b (sf s) c
  | Awid_hei s -> Awid_hei (sf s)
  | Aif e alt ale -> Aif (subste sf e) (substl sf alt) (substl sf ale)
  | Aforeach s sl al -> Aforeach (sf s) (List.map sf sl) (substl sf al)
  | Adefine f xl al alk ->
      Adefine (sf f) (List.map sf xl) (substl sf al) (substl sf alk)
  | Aapply f el -> Aapply (sf f) (substel sf el) ]
and substl sf al = List.map (subst sf) al
and subste sf =
  fun
  [ Eor e1 e2 -> Eor (subste sf e1) (subste sf e2)
  | Eand e1 e2 -> Eand (subste sf e1) (subste sf e2)
  | Eop op e1 e2 -> Eop (sf op) (subste sf e1) (subste sf e2)
  | Enot e -> Enot (subste sf e)
  | Estr s -> Estr (sf s)
  | Eint s -> Eint s
  | Evar s sl -> Evar (sf s) (List.map sf sl)
  | Etransl upp s c -> Etransl upp s c ]
and substel sf el = List.map (subste sf) el;

value split_at_coloncolon s =
  loop 0 where rec loop i =
    if i >= String.length s - 1 then None
    else
      match (s.[i], s.[i + 1]) with
      [ (':', ':') ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 2) (String.length s - i - 2) in
          Some (s1, s2)
      | _ -> loop (i + 1) ]
;

value eval_date_field =
  fun
  [ Some d ->
      match d with
      [ Dgreg d Dgregorian -> Some d
      | Dgreg d Djulian -> Some (Calendar.julian_of_gregorian d)
      | Dgreg d Dfrench -> Some (Calendar.french_of_gregorian d)
      | Dgreg d Dhebrew -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None ]
  | None -> None ]
;

value eval_date_text =
  fun
  [ Some (Dtext s) -> s
  | _ -> "" ]
;

value eval_date_variable od =
  fun
  [ "day" ->
      match eval_date_field od with
      [ Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "" ]
  | "month" ->
      match eval_date_field od with
      [ Some d -> if d.month = 0 then "" else string_of_int d.month
      | None -> "" ]
  | "text" -> eval_date_text od
  | "year" ->
      match eval_date_field od with
      [ Some d -> string_of_int d.year
      | None -> "" ]
  | "oryear" ->
      match od with
      [ Some (Dgreg {prec = OrYear y} _) -> string_of_int y
      | Some (Dgreg {prec = YearInt y} _) -> string_of_int y
      | _ -> "" ]
  | "calendar" ->
      match od with
      [ Some (Dgreg _ Dgregorian) -> "gregorian"
      | Some (Dgreg _ Djulian) -> "julian"
      | Some (Dgreg _ Dfrench) -> "french"
      | Some (Dgreg _ Dhebrew) -> "french"
      | _ -> "" ]
  | "prec" ->
      match od with
      [ Some (Dgreg {prec = Sure} _) -> "sure"
      | Some (Dgreg {prec = About} _) -> "about"
      | Some (Dgreg {prec = Maybe} _) -> "maybe"
      | Some (Dgreg {prec = Before} _) -> "before"
      | Some (Dgreg {prec = After} _) -> "after"
      | Some (Dgreg {prec = OrYear _} _) -> "oryear"
      | Some (Dgreg {prec = YearInt _} _) -> "yearint"
      | _ -> "" ]
  | v -> ">%" ^ v ^ "???" ]
;

value eval_transl conf base env upp s c =
  let r =
    match c with
    [ '0'..'9' ->
        let n = Char.code c - Char.code '0' in
        match split_at_coloncolon s with
        [ None -> Gutil.nominative (Util.transl_nth conf s n)
        | Some (s1, s2) ->
            Util.transl_decline conf s1 (Util.transl_nth conf s2 n) ]
    | _ -> Gutil.nominative (Util.transl conf s) ^ String.make 1 c ]
  in
  if upp then capitale r else r
;

value print_body_prop conf base =
  let s =
    try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with [ Not_found -> "" ]
  in
  let s = s ^ body_prop conf in Wserver.wprint "%s" s
;

value print_copyright conf base =
  let env =
    [('s', fun _ -> commd conf);
     ('d',
      fun _ ->
        if conf.cancel_links then ""
        else " - <a href=\"" ^ conf.indep_command ^ "m=DOC\">DOC</a>")]
  in
  match open_etc_file "copyr" with
  [ Some ic -> copy_from_etc env conf.indep_command ic
  | None ->
      do {
        html_p conf;
        Wserver.wprint "
<hr><font size=-1><em>(c) Copyright 2001 INRIA -
GeneWeb %s</em></font>" Version.txt;
        html_br conf;
      } ]
;

value print_variable conf base =
  fun
  [ "action" -> Wserver.wprint "%s" conf.command
  | "base_header" -> include_hed_trl conf (Some base) ".hed"
  | "base_trailer" -> include_hed_trl conf (Some base) ".trl"
  | "body_prop" -> print_body_prop conf base
  | "copyright" -> print_copyright conf base
  | "hidden" -> Util.hidden_env conf
  | "highlight" -> Wserver.wprint "%s" conf.highlight
  | "image_prefix" -> Wserver.wprint "%s" (image_prefix conf)
  | "nl" -> Wserver.wprint "\n"
  | "nn" -> ()
  | "prefix" -> Wserver.wprint "%s" (commd conf)
  | "sp" -> Wserver.wprint " "
  | s -> Wserver.wprint ">%%%s???" s ]
;
