(* camlp5r ./pa_html.cmo *)
(* $Id: templ.ml,v 5.36 2007-09-12 09:58:44 ddr Exp $ *)

open Config;
open Printf;
open TemplAst;

(* Parsing *)

type token =
  [ BANGEQUAL
  | COMMA
  | DOT
  | DIV
  | EQUAL
  | GREATER
  | GREATEREQUAL
  | LESS
  | LESSEQUAL
  | LPAREN
  | MINUS
  | PERCENT
  | PLUS
  | RPAREN
  | STAR
  | IDENT of string
  | STRING of string
  | INT of string
  | LEXICON of bool and string and string ]
;

type loc_token = [ Tok of loc and token ];

exception Exc_located of loc and exn;

value raise_with_loc loc exc =
  match exc with
  [ Exc_located _ _ -> raise exc
  | _ -> raise (Exc_located loc exc) ]
;

value rec get_ident len =
  parser
  [ [: `('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c); s :] ->
      get_ident (Buff.store len c) s
  | [: :] -> Buff.get len ]
;

value rec get_value len =
  parser
  [ [: `(' ' | '>' | ';' | '\n' | '\r' | '\t') :] -> Buff.get len
  | [: `c; s :] -> get_value (Buff.store len c) s ]
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

value get_compound_var =
  let rec var_kont =
    parser
    [ [: `'.'; s = get_ident 0; sl = var_kont :] -> [s :: sl]
    | [: :] -> [] ]
  in
  parser bp
  [ [: v = get_ident 0; vl = var_kont :] ep -> ((bp, ep), v, vl) ]
;

value get_variable =
  let rec var_kont =
    parser
    [ [: `'.'; s = get_ident 0; sl = var_kont :] -> [s :: sl]
    | [: `';' :] -> []
    | [: :] -> [] ]
  in
  parser bp
  [ [: `'%' :] ep -> ((bp, ep), "%", [])
  | [: `'/' :] ep -> ((bp, ep), "/", [])
  | [: `'[' :] ep -> ((bp, ep), "[", [])
  | [: `']' :] ep -> ((bp, ep), "]", [])
  | [: `'(' :] ep -> ((bp, ep), "(", [])
  | [: `')' :] ep -> ((bp, ep), ")", [])
  | [: v = get_ident 0; vl = var_kont :] ep -> ((bp, ep), v, vl) ]
;

value rec transl_num_index =
  parser
  [ [: `('0'..'9' as c); s :] -> String.make 1 c ^ transl_num_index s
  | [: :] -> "" ]
;

value transl_index =
  parser
  [ [: `('0'..'9' as c); s :] -> String.make 1 c ^ transl_num_index s
  | [: `('a'..'z' as c) :] -> String.make 1 c
  | [: :] -> "" ]
;

value lexicon_word =
  let upper = parser [ [: `'*' :] -> True | [: :] -> False ] in
  let rec text lev len =
    parser
    [ [: `'['; s :] -> text (lev + 1) (Buff.store len '[') s
    | [: `']'; s :] ->
        if lev = 0 then Buff.get len
        else text (lev - 1) (Buff.store len ']') s
    | [: `c; s :] -> text lev (Buff.store len c) s ]
  in
  parser [: upp = upper; s = text 0 0; n = transl_index :] -> (upp, s, n)
;

value rec parse_comment =
  parser
  [ [: `'%'; s :] -> parse_comment_after_percent s
  | [: `_; s :] -> parse_comment s ]
and parse_comment_after_percent =
  parser
  [ [: `')'; s :] -> parse_spaces_after_comment s
  | [: `'('; s :] -> do { parse_comment s; parse_comment s }
  | [: `_; s :] -> parse_comment s ]
and parse_spaces_after_comment =
  parser
  [ [: `(' ' | '\n' | '\t' | '\r'); s :] -> parse_spaces_after_comment s
  | [: :] -> () ]
;

value rec get_token =
  parser bp
  [ [: `' ' | '\t' | '\n' | '\r'; s :] -> get_token s
  | [: `'(' :] ep -> Tok (bp, ep) LPAREN
  | [: `')' :] ep -> Tok (bp, ep) RPAREN
  | [: `',' :] ep -> Tok (bp, ep) COMMA
  | [: `'.' :] ep -> Tok (bp, ep) DOT
  | [: `'=' :] ep -> Tok (bp, ep) EQUAL
  | [: `'+' :] ep -> Tok (bp, ep) PLUS
  | [: `'-' :] ep -> Tok (bp, ep) MINUS
  | [: `'*' :] ep -> Tok (bp, ep) STAR
  | [: `'/' :] ep -> Tok (bp, ep) DIV
  | [: `'%';
       a =
         parser
         [ [: `'('; _ = parse_comment; s :] -> get_token s
         | [: :]  ep -> Tok (bp, ep) PERCENT ] :] ->
      a
  | [: `'!'; `'='?"'=' expected" :] ep -> Tok (bp, ep) BANGEQUAL
  | [: `'>';
       tok =
         parser
         [ [: `'=' :] -> GREATEREQUAL
         | [: :] -> GREATER ] :] ep -> Tok (bp, ep) tok
  | [: `'<';
       tok =
         parser
         [ [: `'=' :] -> LESSEQUAL
         | [: :] -> LESS ] :] ep -> Tok (bp, ep) tok
  | [: `'"'; s = get_string 0 :] ep -> Tok (bp, ep) (STRING s)
  | [: `('0'..'9' as c); s = get_int (Buff.store 0 c) :] ep ->
      Tok (bp, ep) (INT s)
  | [: `'['; (upp, s, n) = lexicon_word :] ep -> Tok (bp, ep) (LEXICON upp s n)
  | [: s = get_ident 0 :] ep -> Tok (bp, ep) (IDENT s) ]
;

module Buff2 = Buff.Make (struct value buff = ref (String.create 80); end);

value rec parse_var =
  parser
    [: `Tok loc (IDENT id); (loc, idl) = ident_list loc :] -> (loc, id, idl)
and ident_list ((bp, _) as loc) =
  parser
  [ [: `Tok _ DOT;
       (loc, id) =
         parser
         [ [: `Tok (_, ep) (IDENT id) :] -> ((bp, ep), id)
         | [: `Tok (_, ep) (INT id) :] -> ((bp, ep), id)
         | [: `Tok loc _ :] -> (loc, "parse_error1") ];
       (loc, idl) = ident_list loc :] ->
      (loc, [id :: idl])
  | [: :] -> (loc, []) ]
;

value rec parse_expr strm = parse_expr_if strm
and parse_expr_if =
  parser
  [ [: `Tok _ (IDENT "if"); e1 = parse_expr_or; `Tok _ (IDENT "then");
       e2 = parse_expr_or; `Tok _ (IDENT "else"); e3 = parse_expr_or :] ->
      Aif e1 [e2] [e3]
  | [: e = parse_expr_or :] -> e ]
and parse_expr_or =
  parser
    [: e = parse_expr_and;
       a =
         parser
         [ [: `Tok loc (IDENT "or"); s :] ->
             Aop2 loc "or" e (parse_expr_or s)
         | [: :] -> e ] :] ->
      a
and parse_expr_and =
  parser
    [: e = parse_expr_3;
       a =
         parser
         [ [: `Tok loc (IDENT "and"); s :] ->
             Aop2 loc "and" e (parse_expr_and s)
         | [: :] -> e ] :] ->
      a
and parse_expr_3 =
  parser
    [: e = parse_expr_4;
       a =
         parser
         [ [: `Tok loc EQUAL; e2 = parse_expr_4 :] -> Aop2 loc "=" e e2
         | [: `Tok loc BANGEQUAL; e2 = parse_expr_4 :] -> Aop2 loc "!=" e e2
         | [: `Tok loc GREATER; e2 = parse_expr_4 :] -> Aop2 loc ">" e e2
         | [: `Tok loc GREATEREQUAL; e2 = parse_expr_4 :] -> Aop2 loc ">=" e e2
         | [: `Tok loc LESS; e2 = parse_expr_4 :] -> Aop2 loc "<" e e2
         | [: `Tok loc LESSEQUAL; e2 = parse_expr_4 :] -> Aop2 loc "<=" e e2
         | [: :] -> e ] :] ->
      a
and parse_expr_4 =
  parser
    [: e = parse_expr_5; a = parse_expr_4_kont e :] -> a
and parse_expr_4_kont e =
  parser
  [ [: `Tok loc PLUS; e2 = parse_expr_5;
       a = parse_expr_4_kont (Aop2 loc "+" e e2) :] -> a
  | [: `Tok loc MINUS; e2 = parse_expr_5;
       a = parse_expr_4_kont (Aop2 loc "-" e e2) :] -> a
  | [: :] -> e ]
and parse_expr_5 =
  parser
    [: e = parse_simple_expr; a = parse_expr_5_kont e :] -> a
and parse_expr_5_kont e =
  parser
  [ [: `Tok loc STAR; e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 loc "*" e e2) :] -> a
  | [: `Tok loc DIV; e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 loc "/" e e2) :] -> a
  | [: `Tok loc PERCENT; e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 loc "%" e e2) :] -> a
  | [: :] -> e ]
and parse_simple_expr =
  parser
  [ [: `Tok _ LPAREN; e = parse_expr;
       a =
         parser
         [ [: `Tok _ RPAREN :] -> e
         | [: `Tok loc _ :] -> Avar loc "parse_error2" [] ] :] ->
      a
  | [: `Tok loc (IDENT "not"); e = parse_simple_expr :] -> Aop1 loc "not" e
  | [: `Tok loc (STRING s) :] -> Atext loc s
  | [: `Tok loc (INT s) :] -> Aint loc s
  | [: `Tok loc (LEXICON upp s n) :] -> Atransl loc upp s n
  | [: (loc, id, idl) = parse_var;
       a =
         parser
         [ [: t = parse_tuple :] -> Aapply loc id t
         | [: :] -> Avar loc id idl ] :] ->
     a ]
and parse_tuple strm =
  let rec parse_expr_list =
    parser
      [: x = parse_expr;
         xl =
           parser
           [ [: `Tok _ COMMA; a = parse_expr_list :] -> a
           | [: :] -> [] ] :] ->
        [[x] :: xl]
  in
  let parse_tuple =
    parser
      [: `Tok _ LPAREN;
         xl = parser [ [: a = parse_expr_list :] -> a | [: :] -> [] ];
         a =
           parser
           [ [: `Tok _ RPAREN :] -> xl
           | [: `Tok loc _ :] -> [[Atext loc "parse_error4"]] ] :] ->
        a
  in
  parse_tuple strm
;

value parse_char_stream p strm =
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  p (Stream.from f)
;

value parse_char_stream_semi p strm =
  let opt_semi = Stream.peek strm <> Some '(' in
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  let r = p (Stream.from f) in
  do {
    if opt_semi then
      match strm with parser [ [: `';' :] -> () | [: :] -> () ]
    else ();
    r
  }
;

value parse_formal_params strm =
  let rec parse_ident_list =
    parser
      [: `Tok _ (IDENT x);
         xl =
           parser
           [ [: `Tok _ COMMA; a = parse_ident_list :] -> a
           | [: :] -> [] ] :] ->
        [x :: xl]
  in
  let parse_tuple =
    parser
      [: `Tok _ LPAREN;
         xl = parser [ [: a = parse_ident_list :] -> a | [: :] -> [] ];
         a =
           parser
           [ [: `Tok _ RPAREN :] -> xl
           | [: :] -> ["parse_error5"] ] :] ->
        a
  in
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  parse_tuple (Stream.from f)
;

value strip_newlines_after_variables =
  loop where rec loop =
    fun
    [ [Atext loc s :: astl] ->
        let s =
          loop 0 where rec loop i =
            if i = String.length s then s
            else if s.[i] = ' ' || s.[i] = '\t' then loop (i + 1)
            else if s.[i] = '\n' then
              String.sub s (i + 1) (String.length s - i - 1)
            else s
        in
        [Atext loc s :: loop astl]
    | [Aif s alt ale :: astl] -> [Aif s (loop alt) (loop ale) :: loop astl]
    | [Aforeach v pl al :: astl] -> [Aforeach v pl (loop al) :: loop astl]
    | [Adefine f x al alk :: astl] ->
        [Adefine f x (loop al) (loop alk) :: loop astl]
    | [Aapply loc f all :: astl] ->
        [Aapply loc f (List.map loop all) :: loop astl]
    | [Alet k v al :: astl] ->
        [Alet k (loop v) (loop al) :: loop astl]
    | [Afor i min max al :: astl] ->
        [Afor i min max (loop al) :: loop astl]
    | [(Atransl _ _ _ _ | Awid_hei _ as ast1); (Atext _ _ as ast2) :: astl] ->
        [ast1; ast2 :: loop astl]
    | [ast :: astl] -> [ast :: loop astl]
    | [] -> [] ]
;

value parse_templ conf strm =
  let rec parse_astl astl bol len end_list strm =
    match strm with parser bp
    [ [: `'%' :] ->
        let astl =
          if len = 0 then astl
          else [Atext (bp - len, bp) (Buff2.get len) :: astl]
        in
        match get_variable strm with
        [ (_, ("%" | "[" | "]" as c), []) ->
            parse_astl [Atext (bp - 1, bp) c :: astl] False 0 end_list strm
        | (_, "(", []) ->
            do {
              parse_comment strm;
              parse_astl astl False 0 end_list strm
            }
        | (_, v, []) when List.mem v end_list -> (List.rev astl, v)
        | (_, "define", []) -> parse_define astl end_list strm
        | (_, "let", []) -> parse_let astl end_list strm
        | x ->
            let ast =
              match x with
              [ (_, "if", []) -> parse_if strm
              | (_, "foreach", []) -> parse_foreach strm
              | (_, "apply", []) -> parse_apply bp strm
              | (_, "expr", []) -> parse_expr_stmt strm
              | (_, "for", []) -> parse_for strm
              | (_, "wid_hei", []) -> Awid_hei (get_value 0 strm)
              | ((_, ep), v, vl) -> Avar (bp, ep) v vl ]
            in
            parse_astl [ast :: astl] False 0 end_list strm ]
    | [: `'[' :] ->
        let astl =
          if len = 0 then astl
          else [Atext (bp - len, bp) (Buff2.get len) :: astl]
        in
        let a =
          let (upp, s, n) = lexicon_word strm in
          if String.length s > 1 && (s.[0] = '[' || s.[0] = '@') then
            let (astl, _) = parse_astl [] False 0 [] (Stream.of_string s) in
            Aconcat (bp, Stream.count strm) astl
          else Atransl (bp, Stream.count strm) upp s n
        in
        parse_astl [a :: astl] False 0 end_list strm
    | [: `c :] ->
        let empty_c = c = ' ' || c = '\t' in
        let len = if empty_c && bol then len else Buff2.store len c in
        let bol = empty_c && bol || c = '\n' in
        parse_astl astl bol len end_list strm
    | [: :] ->
        let astl =
          if len = 0 then astl
          else [Atext (bp - len, bp) (Buff2.get len) :: astl]
        in
        (List.rev astl, "") ]
  and parse_define astl end_list strm =
    let fxlal =
      try
        let f = get_ident 0 strm in
        let xl = parse_formal_params strm in
        let (al, _) = parse_astl [] False 0 ["end"] strm in
        do {
          loop () where rec loop () =
            match strm with parser
            [ [: `('\n' | '\r') :] -> loop ()
            | [: :] -> () ];
          Some (f, xl, al)
        }
      with
      [ Stream.Failure | Stream.Error _ -> None ]
    in
    let (alk, v) = parse_astl [] False 0 end_list strm in
    let astl =
      match fxlal with
      [ Some (f, xl, al) -> [Adefine f xl al alk :: astl]
      | None ->
          let bp = Stream.count strm - 1 in
          [Atext (bp, bp + 1) "define error" :: alk @ astl] ]
    in
    (List.rev astl, v)
  and parse_let astl end_list strm =
    let (ast, tok) =
      try
        let k = get_ident 0 strm in
        match strm with parser
        [ [: `';' :] ->
            let (v, _) = parse_astl [] False 0 ["in"] strm in
            let (al, tok) = parse_astl [] False 0 end_list strm in
            (Alet k v al, tok) ]
      with
      [ Stream.Failure | Stream.Error _ ->
          let bp = Stream.count strm - 1 in
          (Atext (bp, bp + 1) "let syntax error", "") ]
    in        
    (List.rev [ast :: astl], tok)
  and parse_apply bp strm =
    try
      let f = get_ident 0 strm in
      match strm with parser
      [ [: `'%' :] ->
          match get_variable strm with
          [ (_, "with", []) ->
              let all =
                loop () where rec loop () =
                  let (al, tok) = parse_astl [] False 0 ["and"; "end"] strm in
                  match tok with
                  [ "and" -> [al :: loop ()]
                  | _ -> [al] ]
              in
              Aapply (bp, Stream.count strm) f all
          | _ -> raise (Stream.Error "'with' expected") ]
      | [: :] ->
          let el = parse_char_stream parse_tuple strm in
          Aapply (bp, Stream.count strm) f el ]
    with
    [ Stream.Failure | Stream.Error _ ->
        let bp = Stream.count strm - 1 in
        Atext (bp, bp + 1) "apply syntax error" ]
  and parse_expr_stmt strm =
    parse_char_stream_semi parse_simple_expr strm
  and parse_if strm =
    let e = parse_char_stream_semi parse_simple_expr strm in
    let (al1, al2) =
      loop () where rec loop () =
        let (al1, tok) =
          parse_astl [] False 0 ["elseif"; "else"; "end"] strm
        in
        match tok with
        [ "elseif" ->
            let e2 = parse_char_stream_semi parse_simple_expr strm in
            let (al2, al3) = loop () in
            (al1, [Aif e2 al2 al3])
        | "else" ->
            let (al2, _) = parse_astl [] False 0 ["end"] strm in
            (al1, al2)
        | _ -> (al1, []) ]
    in
    Aif e al1 al2
  and parse_for strm =
    try 
      let iterator = get_ident 0 strm in
        match strm with parser
        [ [: `';' :] ->
          let min = parse_char_stream_semi parse_simple_expr strm in
          let max = parse_char_stream_semi parse_simple_expr strm in
          let (al, _) = parse_astl [] False 0 ["end"] strm in
          Afor iterator min max al ]
    with
    [ Stream.Failure | Stream.Error _ ->
        let bp = Stream.count strm - 1 in
        Atext (bp, bp + 1) "for syntax error" ]
  and parse_foreach strm =
    let (loc, v, vl) = get_compound_var strm in
    let params =
      if Stream.peek strm = Some '(' then parse_char_stream parse_tuple strm
      else do {
        match strm with parser
        [ [: `';' :] -> ()
        | [: :] -> () ];
        []
      }
    in
    let (astl, _) = parse_astl [] False 0 ["end"] strm in
    Aforeach (loc, v, vl) params astl
  in
  let astl = fst (parse_astl [] True 0 [] strm) in
  strip_newlines_after_variables astl
;

value input_templ conf fname =
  match Util.open_templ conf fname with
  [ Some ic ->
      let astl = parse_templ conf (Stream.of_channel ic) in
      do { close_in ic; Some astl }
  | None -> None ]
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
  [ Atext loc s -> Atext loc (sf s)
  | Avar loc s sl ->
      let s1 = sf s in
      let sl1 = List.map sf sl in
      if sl = [] &&
        try let _ = int_of_string s1 in True with [ Failure _ -> False ]
      then
        Aint loc s1
      else
        let strm = Stream.of_string s1 in
        let (_, s2, sl2) = get_compound_var strm in
        if Stream.peek strm <> None then Avar loc s1 sl1
        else Avar loc s2 (sl2 @ sl1)
  | Atransl loc b s c -> Atransl loc b (sf s) c
  | Aconcat loc al -> Aconcat loc (List.map (subst sf) al)
  | Awid_hei s -> Awid_hei (sf s)
  | Aif e alt ale -> Aif (subst sf e) (substl sf alt) (substl sf ale)
  | Aforeach (loc, s, sl) pl al ->
      Aforeach (loc, sf s, List.map sf sl) (List.map (substl sf) pl)
        (substl sf al)
  | Afor i min max al -> 
      Afor (sf i) (subst sf min) (subst sf max) (substl sf al)
  | Adefine f xl al alk ->
      Adefine (sf f) (List.map sf xl) (substl sf al) (substl sf alk)
  | Aapply loc f all -> Aapply loc (sf f) (List.map (substl sf) all)
  | Alet k v al -> Alet (sf k) (substl sf v) (substl sf al)
  | Aint loc s -> Aint loc s
  | Aop1 loc op e -> Aop1 loc op (subst sf e)
  | Aop2 loc op e1 e2 -> Aop2 loc (sf op) (subst sf e1) (subst sf e2) ]
and substl sf al = List.map (subst sf) al
;

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

value rec skip_spaces_and_newlines s i =
  if i = String.length s then i
  else
    match s.[i] with
    [ ' ' | '\n' | '\r' -> skip_spaces_and_newlines s (i + 1)
    | _ -> i ]
;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  "Templ." ^ func ^ ": not impl " ^ desc
;

value setup_link conf =
  let s = Wserver.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    let s = "http://" ^ String.sub s 0 i ^ ":2316/" in
    "<a href=\"" ^ s ^ "gwsetup?v=main.htm\">gwsetup</a>"
  with
  [ Not_found -> "" ]
;

value rec eval_variable conf =
  fun
  [ ["bvar"; v] ->
      try List.assoc v conf.base_env with [ Not_found -> "" ]
  | ["evar"; v; "ns"] ->
      try
        let vv = List.assoc v (conf.env @ conf.henv) in
        Util.quote_escaped (Wserver.gen_decode False vv)
      with
      [ Not_found -> "" ]
  | ["evar"; v] ->
      match Util.p_getenv (conf.env @ conf.henv) v with
      [ Some vv -> Util.quote_escaped vv
      | None -> "" ]
  | ["time" :: sl] -> eval_time_var conf sl
  | ["user"; "ident"] -> conf.user
  | ["user"; "name"] -> conf.username
  | [s] -> eval_simple_variable conf s
  | _ -> raise Not_found ]
and eval_time_var conf =
  fun
  [ ["hours"] ->
      let (hh, mm, ss) = conf.time in
      sprintf "%02d" hh
  | ["minutes"] ->
      let (hh, mm, ss) = conf.time in
      sprintf "%02d" mm
  | ["seconds"] ->
      let (hh, mm, ss) = conf.time in
      sprintf "%02d" ss
  | [] ->
      let (hh, mm, ss) = conf.time in
      sprintf "%02d:%02d:%02d" hh mm ss
| _ -> raise Not_found ]
and eval_simple_variable conf =
  fun 
  [ "action" -> conf.command
  | "border" -> string_of_int conf.border
  | "charset" -> conf.charset
  | "compilation_time" -> Util.compilation_time conf
  | "connections" ->
      match conf.n_connect with
      [ Some (c, cw, cf, _) ->
          if c > 0 then
            " - " ^ sprintf "%s %d" (Util.transl conf "connections") c ^
            (if cw > 0 then
               sprintf ", %s %s"
                 (Util.transl_nth conf
                    "wizard/wizards/friend/friends/exterior" 1)
                 (if conf.wizard then
                    sprintf "<a href=\"%sm=CONN_WIZ\">%d</a>"
                       (Util.commd conf) cw
                  else
                    string_of_int cw)
             else "") ^
            (if cf > 0 then
               sprintf ", %s %d"
                 (Util.transl_nth conf
                    "wizard/wizards/friend/friends/exterior" 3)
                 cf
             else "")
          else ""
      | None -> "" ]
  | "doctype" -> Util.doctype conf ^ "\n"
  | "doctype_transitional" ->
      let doctype =
        match Util.p_getenv conf.base_env "doctype" with
        [ Some ("html-4.01" | "html-4.01-trans") -> "html-4.01-trans"
        | _ -> "xhtml-1.0-trans" ]
      in
      let conf =
        {(conf) with base_env = [("doctype", doctype) :: conf.base_env]}
      in
      Util.doctype conf ^ "\n"
  | "highlight" -> conf.highlight
  | "image_prefix" -> Util.image_prefix conf
  | "left" -> conf.left
  | "nl" -> "\n"
  | "nn" -> ""
  | "prefix" -> Util.commd conf
  | "prefix_base" ->
      conf.command ^ "?" ^ (if conf.cgi then "b=" ^ conf.bname ^ ";" else "")
  | "referer" -> Util.get_referer conf
  | "right" -> conf.right
  | "setup_link" -> if conf.setup_link then " - " ^ setup_link conf else ""
  | "sp" -> " "
  | "version" -> Version.txt
  | "/" -> conf.xhs
  | s -> raise Not_found ]
;

value rec string_of_expr_val =
  fun
  [ VVstring s -> s
  | VVbool True -> "1"
  | VVbool False -> "0"
  | VVother f -> string_of_expr_val (f []) ]
;

value eval_string_var conf eval_var sl =
  try eval_var sl with
  [ Not_found ->
      try VVstring (eval_variable conf sl) with
      [ Not_found -> VVstring (" %" ^ String.concat "." sl ^ "?") ] ]
;

value eval_var_handled conf sl =
  try eval_variable conf sl with
  [ Not_found -> sprintf " %%%s?" (String.concat "." sl) ]
;

value apply_format conf nth s1 s2 =
  let transl_nth_format s =
    match nth with
    [ Some n -> Util.ftransl_nth conf s n
    | None -> Util.ftransl conf s ]
  in
  match Util.check_format "%t" s1 with
  [ Some s3 -> sprintf (transl_nth_format s3) (fun _ -> s2)
  | None ->
      match Util.check_format "%s" s1 with
      [ Some s3 -> sprintf (transl_nth_format s3) s2
      | None ->
          match Util.check_format "%d" s1 with
          [ Some s3 ->
              sprintf (transl_nth_format s3) (int_of_string s2)
          | None ->
              match Util.check_format "%s%s" s1 with
              [ Some s3 ->
                  let (s21, s22) =
                    let i = String.index s2 ':' in
                    (String.sub s2 0 i,
                     String.sub s2 (i + 1) (String.length s2 - i - 1))
                  in
                  sprintf (transl_nth_format s3) s21 s22
              | None -> raise Not_found ] ] ] ]
;

value rec eval_ast conf =
  fun
  [ Atext _ s -> s
  | Avar _ s sl -> eval_var_handled conf [s :: sl]
  | Atransl _ upp s c -> eval_transl conf upp s c
  | ast -> not_impl "eval_ast" ast ]
and eval_transl conf upp s c =
  if c = "" && String.length s > 0 && s.[0] = '\n' then
    eval_transl_inline conf s
  else
    eval_transl_lexicon conf upp s c
and eval_transl_inline conf s =
  let (s, alt) =
    Translate.inline conf.lang '%' (fun c -> "%" ^ String.make 1 c) s
  in
  s
and eval_transl_lexicon conf upp s c =
  let r =
    let nth = try Some (int_of_string c) with [ Failure _ -> None ] in
    match split_at_coloncolon s with
    [ None ->
        let s2 =
          match nth with
          [ Some n -> Util.transl_nth conf s n
          | None -> Util.transl conf s ]
        in
        if c = "n" then s2 else Mutil.nominative s2
    | Some (s1, s2) ->
        try
          if String.length s2 > 0 && s2.[0] = '|' then
            let i = 1 in
            let j = String.rindex s2 '|' in
            let k = skip_spaces_and_newlines s2 (j + 1) in
            let s3 =
              let s = String.sub s2 i (j - i) in
              let astl = parse_templ conf (Stream.of_string s) in
              List.fold_left (fun s a -> s ^ eval_ast conf a) "" astl
            in
            let s4 = String.sub s2 k (String.length s2 - k) in
            let s5 =
              match nth with
              [ Some n -> Util.transl_nth conf s4 n
              | None -> Util.transl conf s4 ]
            in
            let s2 = s3 ^ s5 in
            Util.transl_decline conf s1 s2
          else if String.length s2 > 0 && s2.[0] = ':' then
            let s2 = String.sub s2 1 (String.length s2 - 1) in
            try apply_format conf nth s1 s2 with
            [ Failure _ -> raise Not_found ]
          else raise Not_found
        with
        [ Not_found ->
            let s3 =
              match nth with
              [ Some n -> Util.transl_nth conf s2 n
              | None -> if s2 = "" then "" else Util.transl conf s2 ]
            in
            Util.transl_decline conf s1 s3 ] ]
  in
  let r = Util.translate_eval r in
  if upp then Util.capitale r else r
;

value nb_errors = ref 0;

value loc_of_expr =
  fun
  [ Atext loc _ -> loc
  | Avar loc _ _ -> loc
  | Atransl loc _ _ _ -> loc
  | Aapply loc _ _ -> loc
  | Aop1 loc _ _ -> loc
  | Aop2 loc _ _ _ -> loc
  | Aint loc _ -> loc
  | _ -> (-1, -1) ]
;

value templ_eval_var conf =
  fun
  [ ["cancel_links"] -> VVbool conf.cancel_links
  | ["cgi"] -> VVbool conf.cgi
  | ["false"] -> VVbool False
  | ["has_referer"] -> (* deprecated since version 5.00 *) 
      VVbool (Wserver.extract_param "referer: " '\n' conf.request <> "")
  | ["just_friend_wizard"] -> VVbool conf.just_friend_wizard
  | ["friend"] -> VVbool conf.friend
  | ["manitou"] -> VVbool conf.manitou
  | ["supervisor"] -> VVbool conf.supervisor
  | ["true"] -> VVbool True
  | ["wizard"] -> VVbool conf.wizard
  | _ -> raise Not_found ]
;

value bool_of e =
  fun
  [ VVbool b -> b
  | VVstring _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "bool value expected") ]
;

value string_of e =
  fun
  [ VVstring s -> s
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "string value expected") ]
;

value int_of e =
  fun
  [ VVstring s ->
      try int_of_string s with
      [ Failure _ ->
          raise_with_loc (loc_of_expr e)
            (Failure ("int value expected\nFound = " ^ s)) ]
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "int value expected") ]
;

value num_of e =
  fun
  [ VVstring s ->
      try Num.of_string s with
      [ Failure _ ->
          raise_with_loc (loc_of_expr e)
            (Failure ("num value expected\nFound = " ^ s)) ]
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "num value expected") ]
;

value rec eval_expr ((conf, eval_var, eval_apply) as ceva) =
  fun
  [ Atext _ s -> VVstring s
  | Avar loc s sl ->
      try eval_var loc [s :: sl] with
      [ Not_found ->
          try templ_eval_var conf [s :: sl] with
          [ Not_found ->
              raise_with_loc loc
                (Failure
                   ("unbound var \"" ^ String.concat "." [s :: sl] ^ "\"")) ] ]
  | Atransl _ upp s c -> VVstring (eval_transl conf upp s c)
  | Aconcat _ al ->
      let vl = List.map (eval_expr ceva) al in
      let sl = List.map string_of_expr_val vl in
      VVstring (String.concat "" sl)
  | Aapply loc s ell ->
      let vl =
        List.map
          (fun el ->
             match List.map (eval_expr ceva) el with
             [ [e] -> e
             | el ->
                 let sl = List.map string_of_expr_val el in
                 VVstring (String.concat "" sl) ])
          ell
      in
      VVstring (eval_apply loc s vl)
  | Aop1 loc op e ->
      let v = eval_expr ceva e in
      match op with
      [ "not" -> VVbool (not (bool_of e v))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")) ]
  | Aop2 loc op e1 e2 ->
      let int e = int_of e (eval_expr ceva e) in
      let num e = num_of e (eval_expr ceva e) in
      let bool e = bool_of e (eval_expr ceva e) in
      match op with
      [ "and" -> VVbool (if bool e1 then bool e2 else False)
      | "or" -> VVbool (if bool e1 then True else bool e2)
      | "=" -> VVbool (eval_expr ceva e1 = eval_expr ceva e2)
      | "<" -> VVbool (int e1 < int e2)
      | ">" -> VVbool (int e1 > int e2)
      | "!=" -> VVbool (eval_expr ceva e1 <> eval_expr ceva e2)
      | "<=" -> VVbool (int e1 <= int e2)
      | ">=" -> VVbool (int e1 >= int e2)
      | "+" -> VVstring (Num.to_string (Num.add (num e1) (num e2)))
      | "-" -> VVstring (Num.to_string (Num.sub (num e1) (num e2)))
      | "*" -> VVstring (Num.to_string (Num.mul (num e1) (int e2)))
      | "/" -> VVstring (Num.to_string (Num.div (num e1) (int e2)))
      | "%" -> VVstring (string_of_int (Num.modl (num e1) (int e2)))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")) ]
  | Aint loc s -> VVstring s
  | e -> raise_with_loc (loc_of_expr e) (Failure (not_impl "eval_expr" e)) ]
;

value line_of_loc conf fname (bp, ep) =
  match Util.open_templ conf fname with
  [ Some ic ->
      let strm = Stream.of_channel ic in
      let rec loop lin =
        let rec scan_line col =
          parser cnt
            [: `c; s :] ->
              if cnt < bp then
                if c = '\n' then loop (lin + 1)
                else scan_line (col + 1) s
              else
                let col = col - (cnt - bp) in
                (lin, col, col + ep - bp)
        in
        scan_line 0 strm
      in
      let r = try Some (loop 1) with [ Stream.Failure -> None ] in
      do { close_in ic; r }
  | None -> None ]
;

value template_file = ref "";
value print_error conf (bp, ep) exc =
  do {
    incr nb_errors;
    IFDEF UNIX THEN do {
      if nb_errors.val <= 10 then do {
        if template_file.val = "" then eprintf "*** <W> template file"
        else eprintf "File \"%s.txt\"" template_file.val;
        let line =
          if template_file.val = "" then None
          else line_of_loc conf template_file.val (bp, ep)
        in
        eprintf ", ";
        match line with
        [ Some (lin, col1, col2) ->
            eprintf "line %d, characters %d-%d:\n" lin col1 col2
        | None ->
            eprintf "characters %d-%d:\n" bp ep ];
        match exc with
        [ Failure s -> eprintf "Failed - %s" s
        | _ -> eprintf "%s" (Printexc.to_string exc) ];
        eprintf "\n\n";
        flush stderr;
      }
      else ();
    }
    ELSE () END;
  }
;

value eval_bool_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    [ VVbool b -> b
    | VVstring _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "bool value expected") ]
  with
  [ Exc_located loc exc -> do { print_error conf loc exc; False } ]
;

value eval_string_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    [ VVstring s -> Util.translate_eval s
    | VVbool _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "string value expected") ]
  with
  [ Exc_located loc exc -> do { print_error conf loc exc; "" } ]
;

value print_body_prop conf =
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
    [ Not_found -> "" ]
  in
  Wserver.wprint "%s" (s ^ Util.body_prop conf)
;

type vother 'a =
  [ Vdef of list string and list ast
  | Vval of expr_val 'a
  | Vbind of string and string ]
;
type env 'a = list (string * 'a);

type interp_fun 'a 'b =
  { eval_var : env 'a -> 'b -> loc -> list string -> expr_val 'b;
    eval_transl : env 'a -> bool -> string -> string -> string;
    eval_predefined_apply : env 'a -> string -> list (expr_val 'b) -> string;
    get_vother : 'a -> option (vother 'b);
    set_vother : vother 'b -> 'a;
    print_foreach : 
      (env 'a -> 'b -> ast -> unit) ->
         (env 'a -> 'b -> ast -> string) ->
         env 'a -> 'b -> loc -> string -> list string ->
         list (list ast) -> list ast -> unit }
;

value get_def get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
    [ Some (Vdef al el) -> Some (al, el)
    | _ -> None ]
  with
  [ Not_found -> None ]
;
value get_val get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
    [ Some (Vval x) -> Some x
    | _ -> None ]
  with
  [ Not_found -> None ]
;
value set_def set_vother k al el env =
  let k = "#" ^ k in
  [(k, set_vother (Vdef al el)) :: env]
;
value set_val set_vother k v env =
  let k = "#" ^ k in
  [(k, set_vother (Vval v)) :: env]
;

value eval_subst loc f set_vother env xl vl a =
  loop env a xl vl where rec loop env a xl vl =
    match (xl, vl) with
    [ ([x :: xl], [VVstring v :: vl]) ->
        loop env (subst (subst_text x v) a) xl vl
    | ([x :: xl], [v :: vl]) ->
        let env = set_val set_vother x v env in
        loop env a xl vl
    | ([], []) -> (env, a)
    | _ -> (env, Atext loc (f ^ ": bad # of params")) ]
;

value squeeze_spaces s =
  loop 0 where rec loop i =
    if i = String.length s then ""
    else
      match s.[i] with
      [ ' ' | '\n' | '\r' | '\t' -> loop (i + 1)
      | _ -> String.sub s i (String.length s - i) ]
;

value print_apply loc f set_vother print_ast env ep gxl al gvl =
  let local_print_ast a =
    let (env, a) =
      loop env a gxl gvl where rec loop env a xl vl =
        match (xl, vl) with
        [ ([x :: xl], [VVstring v :: vl]) ->
            loop env (subst (subst_text x v) a) xl vl
        | ([x :: xl], [v :: vl]) ->
            loop (set_val set_vother x v env) a xl vl
        | ([], []) -> (env, a)
        | _ ->
            (env,
             Atext loc
               (sprintf "%s: bad # of params (%d instead of %d)" f
                  (List.length gvl) (List.length gxl))) ]
    in
    print_ast env ep a
  in
  loop al where rec loop =
    fun
    [ [] -> ()
    | [Avar _ "sq" []; Atext loc s :: al] ->
        let s = squeeze_spaces s in
        loop [Atext loc s :: al]
    | [a :: al] -> do { local_print_ast a; loop al } ]
;

value templ_eval_expr = eval_string_expr;
value templ_print_apply = print_apply;

value rec templ_print_foreach conf print_ast set_vother env ep loc s sl el al =
  match [s :: sl] with
  [ ["env_binding"] ->
      print_foreach_env_binding conf print_ast set_vother env ep al
  | _ -> raise Not_found ]
and print_foreach_env_binding conf print_ast set_vother env ep al =
  List.iter
    (fun (k, v) ->
       let print_ast =
         print_ast [("binding", set_vother (Vbind k v)) :: env] ep
       in
       List.iter print_ast al)
    conf.env
;

value float_rgb_of_hsv h s v =
  let h = if h > 1. then 1. else if h < 0. then 0. else h in
  let s = if s > 1. then 1. else if s < 0. then 0. else s in
  let v = if v > 1. then 1. else if v < 0. then 0. else v in
  let h = if h = 1.0 then 0. else h in
  let h = h *. 6. in
  let i = truncate h in
  let f = h -. float i in
  let p = v *. (1. -. s) in
  let q = v *. (1. -. s *. f) in
  let t = v *. (1. -. s *. (1. -. f)) in
  match i with
  [ 0 -> (v, t, p)
  | 1 -> (q, v, p)
  | 2 -> (p, v, t)
  | 3 -> (p, q, v)
  | 4 -> (t, p, v)
  | 5 -> (v, p, q)
  | _ -> assert False ]
;

value rgb_of_hsv h s v =
  let (r, g, b) =
    float_rgb_of_hsv (float h /. 256.) (float s /. 256.) (float v /. 256.)
  in
  (truncate (r *. 256.), truncate (g *. 256.), truncate (b *. 256.))
;

value rgb_of_str_hsv h s v =
  let ios s = int_of_string s in
  rgb_of_hsv (ios h) (ios s) (ios v)
;

value eval_var conf ifun env ep loc sl =
  try
    match sl with
    [ ["env"; "key"] ->
        match ifun.get_vother (List.assoc "binding" env) with
        [ Some (Vbind k v) -> VVstring k
        | _ -> raise Not_found ]
    | ["env"; "val"] ->
        match ifun.get_vother (List.assoc "binding" env) with
        [ Some (Vbind k v) -> VVstring v
        | _ -> raise Not_found ]
    | ["env"; "val"; "decoded"] ->
        match ifun.get_vother (List.assoc "binding" env) with
        [ Some (Vbind k v) -> VVstring (Util.decode_varenv v)
        | _ -> raise Not_found ]
    | ["today" :: sl] ->
        TemplDate.eval_date_var conf (Calendar.sdn_of_gregorian conf.today)
          sl
    | [s :: sl] ->
        match (get_val ifun.get_vother s env, sl) with
        [ (Some (VVother f), sl) -> f sl
        | (Some v, []) -> v
        | (_, sl) -> ifun.eval_var env ep loc [s :: sl] ]
    | _ -> ifun.eval_var env ep loc sl ]
  with
  [ Not_found -> VVstring (eval_variable conf sl) ]
;

value print_foreach conf ifun print_ast eval_expr env ep loc s sl el al =
  try ifun.print_foreach print_ast eval_expr env ep loc s sl el al with
  [ Not_found ->
      templ_print_foreach conf print_ast ifun.set_vother env ep loc s sl
        el al ]
;

value print_wid_hei env fname =
  match Util.image_size (Util.image_file_name fname) with
  [ Some (wid, hei) -> Wserver.wprint " width=\"%d\" height=\"%d\"" wid hei
  | None -> () ]
;

value copy_from_templ_fwd = ref (fun []);
value copy_from_templ conf env ic = copy_from_templ_fwd.val conf env ic;

value print_copyright conf =
  match Util.open_etc_file "copyr" with
  [ Some ic -> copy_from_templ conf [] ic
  | None -> do {
      xtag "hr" "style=\"margin:0\"";
      tag "div" "style=\"font-size: 80%%\"" begin
        stag "em" begin
          Wserver.wprint
            "Copyright (c) 1998-2007 INRIA - GeneWeb %s" Version.txt;
        end;
      end;
      xtag "br";
    } ]
;

value print_copyright_with_logo conf =
  let conf =
     {(conf) with env = [("with_logo", "yes") :: conf.env]}
  in
  print_copyright conf
;

value old_include_hed_trl conf base_opt suff =
  let hed_fname =
    let fname = Util.base_path ["lang"; conf.lang] (conf.bname ^ suff) in
    if Sys.file_exists fname then fname
    else Util.base_path ["lang"] (conf.bname ^ suff)
  in
  match try Some (Secure.open_in hed_fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let url () =
        match base_opt with
        [ Some base -> Util.url_no_index conf base
        | None -> Util.get_server_string conf ^ Util.get_request_string conf ]
      in
      let pref () =
        let s = url () in
        match Mutil.rindex s '?' with
        [ Some i -> String.sub s 0 (i + 1)
        | None -> s ]
      in
      let suff () =
        let s = url () in
        match Mutil.rindex s '?' with
        [ Some i -> String.sub s (i + 1) (String.length s - i - 1)
        | None -> "" ]
      in
      Util.copy_from_etc
        [('p', pref); ('s', suff); ('t', fun _ -> Util.commd conf);
          ('/', fun _ -> conf.xhs)]
        conf.lang conf.indep_command ic
  | None -> () ]
;

value include_hed_trl conf base_opt name =
  let fname =
    Filename.concat (Util.base_path ["etc"] conf.bname) (name ^ ".txt")
  in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic -> copy_from_templ conf [] ic
  | None -> old_include_hed_trl conf base_opt ("." ^ name) ]
;

value rec interp_ast conf base ifun =
  let rec eval_ast env ep a =
    string_of_expr_val (eval_ast_expr env ep a)
  and eval_ast_list env ep =
    fun
    [ [] -> []
    | [Avar _ "sq" []; Atext loc s :: al] ->
        let s = squeeze_spaces s in
        eval_ast_list env ep [Atext loc s :: al]
    | [a :: al] -> [eval_ast env ep a :: eval_ast_list env ep al] ]
  and eval_ast_expr env ep =
    fun
    [ Atext _ s -> VVstring s
    | Avar loc s sl ->
        eval_string_var conf (eval_var conf ifun env ep loc) [s :: sl]
    | Atransl _ upp s n -> VVstring (ifun.eval_transl env upp s n)
    | Aif e alt ale -> VVstring (eval_if env ep e alt ale)
    | Aapply loc f all ->
        let vl = List.map (eval_ast_expr_list env ep) all in
        VVstring (eval_apply env ep loc f vl)
    | Afor i min max al ->
        VVstring (eval_for env ep i min max al)
    | x -> VVstring (eval_expr env ep x) ]
  and eval_ast_expr_list env ep v =
    let rec loop =
      fun
      [ [] -> []
     | [Avar _ "sq" []; Atext loc s :: al] ->
         let s = squeeze_spaces s in
         loop [Atext loc s :: al]
      | [a :: al] -> [eval_ast_expr env ep a :: loop al] ]
    in
    match loop v with
    [ [e] -> e
    | el ->
        let sl = List.map string_of_expr_val el in
        VVstring (String.concat "" sl) ]
  and eval_expr env ep e =
    let eval_apply = eval_apply env ep in
    let eval_var = eval_var conf ifun env ep in
    templ_eval_expr conf (eval_var, eval_apply) e
  and eval_apply env ep loc f vl =
    match get_def ifun.get_vother f env with
    [ Some (xl, al) ->
        let (env, al) =
          List.fold_right
            (fun a (env, al) ->
               let (env, a) = eval_subst loc f ifun.set_vother env xl vl a in
               (env, [a :: al]))
          al (env, [])
        in
        let sl = List.map (eval_ast env ep) al in
        String.concat "" sl
    | None ->
        match (f, vl) with
        [ ("capitalize", [VVstring s]) -> Util.capitale s
        | ("interp", [VVstring s]) ->
            let astl = parse_templ conf (Stream.of_string s) in
            String.concat "" (eval_ast_list env ep astl)
        | ("language_name", [VVstring s]) ->
            Translate.language_name s (Util.transl conf " !languages")
        | ("nth", [VVstring s1; VVstring s2]) ->
            let n = try int_of_string s2 with [ Failure _ -> 0 ] in
            Util.translate_eval (Util.nth_field s1 n)
        | ("red_of_hsv", [VVstring h; VVstring s; VVstring v]) ->
            try
              let (r, g, b) = rgb_of_str_hsv h s v in
              string_of_int r
            with
            [ Failure _ -> "red_of_hsv bad params" ]
        | ("green_of_hsv", [VVstring h; VVstring s; VVstring v]) ->
            try
              let (r, g, b) = rgb_of_str_hsv h s v in
              string_of_int g
            with
            [ Failure _ -> "green_of_hsv bad params" ]
        | ("blue_of_hsv", [VVstring h; VVstring s; VVstring v]) ->
            try
              let (r, g, b) = rgb_of_str_hsv h s v in
              string_of_int b
            with
            [ Failure _ -> "blue_of_hsv bad params" ]
        | _ ->
            try ifun.eval_predefined_apply env f vl with
            [ Not_found -> sprintf "%%apply;%s?" f ] ] ]
  and eval_if env ep e alt ale =
    let eval_var = eval_var conf ifun env ep in
    let eval_ast = eval_ast env ep in
    let eval_apply = eval_apply env ep in
    let al =
      if eval_bool_expr conf (eval_var, eval_apply) e then alt else ale
    in
    String.concat "" (List.map eval_ast al)
  and eval_for env ep iterator min max al = 
    let rec loop env min max accu =
      let new_env = env in
      let v = eval_ast_expr_list new_env ep [min] in
      let new_env = set_val ifun.set_vother iterator v new_env in 
      let eval_var = eval_var conf ifun new_env ep in
      let eval_apply = eval_apply new_env ep in
      let eval_ast = eval_ast new_env ep in
      let int_min = 
        int_of_string (eval_string_expr conf (eval_var, eval_apply) min) 
      in
      let int_max = 
        int_of_string (eval_string_expr conf (eval_var, eval_apply) max) 
      in
      if int_min < int_max then 
        let instr = (String.concat "" (List.map eval_ast al)) in
        let accu = accu ^ instr in
        loop new_env (Aop2 (0, 0) "+" min (Aint (0, 0) "1")) max accu
      else accu
    in loop env min max ""
  in
  let rec print_ast env ep =
    fun
    [ Avar loc s sl ->
        print_var print_ast_list conf base ifun env ep loc [s :: sl]
    | Awid_hei s -> print_wid_hei env s
    | Aif e alt ale -> print_if env ep e alt ale
    | Aforeach (loc, s, sl) el al ->
        try
          print_foreach conf ifun print_ast eval_expr env ep loc s sl el al
        with
        [ Not_found ->
            Wserver.wprint " %%foreach;%s?" (String.concat "." [s :: sl]) ]
    | Adefine f xl al alk -> print_define env ep f xl al alk
    | Aapply loc f ell -> print_apply env ep loc f ell
    | Alet k v al -> print_let env ep k v al
    | Afor i min max al -> print_for env ep i min max al
    | x -> Wserver.wprint "%s" (eval_ast env ep x) ]
  and print_ast_list env ep =
    fun
    [ [] -> ()
    | [Avar _ "sq" []; Atext loc s :: al] ->
        let s = squeeze_spaces s in
        print_ast_list env ep [Atext loc s :: al]
    | [a :: al] -> do {
        print_ast env ep a;
        print_ast_list env ep al
      } ]
  and print_define env ep f xl al alk =
    let env = set_def ifun.set_vother f xl al env in
    print_ast_list env ep alk
  and print_apply env ep loc f ell =
    let vl = List.map (eval_ast_expr_list env ep) ell in
    match get_def ifun.get_vother f env with
    [ Some (xl, al) ->
        templ_print_apply loc f ifun.set_vother print_ast env ep xl al vl
    | None ->
        Wserver.wprint "%s" (eval_apply env ep loc f vl) ]
  and print_let env ep k v al =
    let v = eval_ast_expr_list env ep v in
    let env = set_val ifun.set_vother k v env in
    print_ast_list env ep al
  and print_if env ep e alt ale =
    let eval_var = eval_var conf ifun env ep in
    let eval_apply = eval_apply env ep in
    let al =
      if eval_bool_expr conf (eval_var, eval_apply) e then alt else ale
    in
    print_ast_list env ep al
  and print_for env ep i min max al =
    let rec loop env min max =
      let new_env = env in
      let v = eval_ast_expr_list new_env ep [min] in
      let new_env = set_val ifun.set_vother i v new_env in 
      let eval_var = eval_var conf ifun new_env ep in
      let eval_apply = eval_apply new_env ep in
      let int_min = 
        int_of_string (eval_string_expr conf (eval_var, eval_apply) min) 
      in
      let int_max = 
        int_of_string (eval_string_expr conf (eval_var, eval_apply) max) 
      in
      if int_min < int_max then 
        let _ = print_ast_list new_env ep al in
        loop new_env (Aop2 (0, 0) "+" min (Aint (0, 0) "1")) max
      else ()
    in loop env min max
  in
  print_ast_list
and print_var print_ast_list conf base ifun env ep loc sl =
  let rec print_var1 eval_var sl =
    try
      match eval_var sl with
      [ VVstring s -> Wserver.wprint "%s" s
      | VVbool True -> Wserver.wprint "1"
      | VVbool False -> Wserver.wprint "0"
      | VVother f -> print_var1 f [] ]
    with
    [ Not_found ->
        match sl with
        [ ["include"; templ] ->
            match input_templ conf templ with
            [ Some astl -> print_ast_list env ep astl
            | None ->  Wserver.wprint " %%%s?" (String.concat "." sl) ]
        | sl ->
            print_variable conf base sl ] ]
  in
  let eval_var = eval_var conf ifun env ep loc in
  print_var1 eval_var sl
and print_simple_variable conf base_opt =
  fun
  [ "base_header" -> include_hed_trl conf base_opt "hed"
  | "base_trailer" -> include_hed_trl conf base_opt "trl"
  | "body_prop" -> print_body_prop conf
  | "copyright" -> print_copyright_with_logo conf
  | "copyright_nologo" -> print_copyright conf
  | "hidden" -> Util.hidden_env conf
  | "message_to_wizard" -> Util.message_to_wizard conf
  | _ -> raise Not_found ]
and print_variable conf base_opt sl =
  try Wserver.wprint "%s" (eval_variable conf sl) with
  [ Not_found ->
      try
        match sl with
        [ [s] -> print_simple_variable conf base_opt s
        | _ -> raise Not_found ]
      with
      [ Not_found -> Wserver.wprint " %%%s?" (String.concat "." sl) ] ]
;

value copy_from_templ conf env ic = do {
  let astl = parse_templ conf (Stream.of_channel ic) in
  close_in ic;
  let ifun =
    {eval_var env accu loc =
       fun
       [ [s] -> VVstring (List.assoc s env)
       | _ -> raise Not_found ];
     eval_transl env = eval_transl conf;
     eval_predefined_apply _ = raise Not_found;
     get_vother _ = None;
     set_vother _ = "";
     print_foreach _ = raise Not_found}
  in
  let v = template_file.val in
  template_file.val := "";
  try interp_ast conf None ifun env () astl with e ->
    do { template_file.val := v; raise e };
  template_file.val := v;
};

copy_from_templ_fwd.val := copy_from_templ;
