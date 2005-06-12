(* camlp4r ./pa_html.cmo *)
(* $Id: templ.ml,v 4.63 2005-06-12 06:33:41 ddr Exp $ *)

open Config;
open TemplAst;

(* Parsing *)

type token =
  [ BANGEQUAL
  | COMMA
  | DOT
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
  let rec text len =
    parser
    [ [: `']' :] -> Buff.get len
    | [: `c; s :] -> text (Buff.store len c) s ]
  in
  parser [: upp = upper; s = text 0; n = transl_index :] -> (upp, s, n)
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

value buff_get len = String.sub buff.val 0 len;

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

value rec parse_expr_1 =
  parser
    [: e = parse_expr_2;
       a =
         parser
         [ [: `Tok _ (IDENT "or"); s :] -> Aop2 "or" e (parse_expr_1 s)
         | [: :] -> e ] :] ->
      a
and parse_expr_2 =
  parser
    [: e = parse_expr_3;
       a =
         parser
         [ [: `Tok _ (IDENT "and"); s :] -> Aop2 "and" e (parse_expr_2 s)
         | [: :] -> e ] :] ->
      a
and parse_expr_3 =
  parser
    [: e = parse_expr_4;
       a =
         parser
         [ [: `Tok _ EQUAL; e2 = parse_expr_4 :] -> Aop2 "=" e e2
         | [: `Tok _ BANGEQUAL; e2 = parse_expr_4 :] -> Aop2 "!=" e e2
         | [: `Tok _ GREATER; e2 = parse_expr_4 :] -> Aop2 ">" e e2
         | [: `Tok _ GREATEREQUAL; e2 = parse_expr_4 :] -> Aop2 ">=" e e2
         | [: `Tok _ LESS; e2 = parse_expr_4 :] -> Aop2 "<" e e2
         | [: `Tok _ LESSEQUAL; e2 = parse_expr_4 :] -> Aop2 "<=" e e2
         | [: :] -> e ] :] ->
      a
and parse_expr_4 =
  parser
    [: e = parse_expr_5;
       a =
         parser
         [ [: `Tok _ PLUS; e2 = parse_expr_5 :] -> Aop2 "+" e e2
         | [: `Tok _ MINUS; e2 = parse_expr_5 :] -> Aop2 "-" e e2
         | [: :] -> e ] :] ->
      a
and parse_expr_5 =
  parser
    [: e = parse_simple_expr;
       a =
         parser
         [ [: `Tok _ STAR; e2 = parse_simple_expr :] -> Aop2 "*" e e2
         | [: `Tok _ PERCENT; e2 = parse_simple_expr :] -> Aop2 "%" e e2
         | [: :] -> e ] :] ->
      a
and parse_simple_expr =
  parser
  [ [: `Tok _ LPAREN; e = parse_expr_1;
       a =
         parser
         [ [: `Tok _ RPAREN :] -> e
         | [: `Tok loc _ :] -> Avar loc "parse_error2" [] ] :] ->
      a
  | [: `Tok _ (IDENT "not"); e = parse_simple_expr :] -> Aop1 "not" e
  | [: `Tok _ (STRING s) :] -> Atext s
  | [: `Tok loc (INT s) :] -> Aint loc s
  | [: `Tok _ (LEXICON upp s n) :] -> Atransl upp s n
  | [: (loc, id, idl) = parse_var;
       a =
         parser
         [ [: t = parse_tuple :] -> Aapply loc id t
         | [: :] -> Avar loc id idl ] :] ->
     a ]
and parse_tuple strm =
  let rec parse_expr_list =
    parser
      [: x = parse_expr_1;
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
           | [: :] -> [[Atext "parse_error4"]] ] :] ->
        a
  in
  parse_tuple strm
;

value parse_char_stream p strm =
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  p (Stream.from f)
;

value parse_char_stream_semi p strm =
  let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
  let r = p (Stream.from f) in
  do { match strm with parser [ [: `';' :] -> () | [: :] -> () ]; r }
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

value parse_templ conf strm =
  let rec parse_astl astl bol len end_list strm =
    match strm with parser bp
    [ [: `'%' :] ->
        let astl = if len = 0 then astl else [Atext (buff_get len) :: astl] in
        match get_variable strm with
        [ (_, ("%" | "[" | "]" as c), []) ->
            parse_astl [Atext c :: astl] False 0 end_list strm
        | (_, "(", []) ->
            do {
              parse_comment strm;
              parse_astl astl bol len end_list strm
            }
        | (_, v, []) when List.mem v end_list -> (List.rev astl, v)
        | (_, "define", []) -> parse_define astl end_list strm
        | x ->
            let ast =
              match x with
              [ (_, "if", []) -> parse_if strm
              | (_, "foreach", []) -> parse_foreach strm
              | (_, "apply", []) -> parse_apply bp strm
              | (_, "expr", []) -> parse_expr_stmt strm
              | (_, "wid_hei", []) -> Awid_hei (get_value 0 strm)
              | ((_, ep), v, vl) -> Avar (bp, ep) v vl ]
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
      | None -> [Atext "define error" :: alk @ astl] ]
    in
    (List.rev astl, v)
  and parse_apply bp strm =
    try
      let f = get_ident 0 strm in
      match strm with parser
      [ [: `'%' :] ->
          match get_variable strm with
          [ (_, "with", []) ->
              let all =
                loop () where rec loop () =
                  let (al, tok) =
                    parse_astl [] False 0 ["and"; "end"] strm
                  in
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
    [ Stream.Failure | Stream.Error _ -> Atext "apply syntax error" ]
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
  fst (parse_astl [] True 0 [] strm)
;

value open_templ conf dir name =
  let std_fname =
    Util.search_in_lang_path (Filename.concat "etc" (name ^ ".txt"))
  in
  if dir = "" || dir = Filename.current_dir_name then
    try Some (Secure.open_in std_fname) with [ Sys_error _ -> None ]
  else
    let dir = Filename.basename dir in
    let fname =
      Filename.concat (Util.base_path ["etc"] dir) (name ^ ".txt")
    in
    try Some (Secure.open_in fname) with
    [ Sys_error _ ->
        if (*dir = conf.bname*)True(**) then
          try Some (Secure.open_in std_fname) with [ Sys_error _ -> None ]
        else None ]
;

value strip_newlines_after_variables =
  loop where rec loop =
    fun
    [ [Atext s :: astl] ->
        let s =
          loop 0 where rec loop i =
            if i = String.length s then s
            else if s.[i] = ' ' || s.[i] = '\t' then loop (i + 1)
            else if s.[i] = '\n' then
              String.sub s (i + 1) (String.length s - i - 1)
            else s
        in
        [Atext s :: loop astl]
    | [Aif s alt ale :: astl] -> [Aif s (loop alt) (loop ale) :: loop astl]
    | [Aforeach v pl al :: astl] -> [Aforeach v pl (loop al) :: loop astl]
    | [Adefine f x al alk :: astl] ->
        [Adefine f x (loop al) (loop alk) :: loop astl]
    | [Aapply loc f all :: astl] ->
        [Aapply loc f (List.map loop all) :: loop astl]
    | [(Atransl _ _ _ | Awid_hei _ as ast1); (Atext _ as ast2) :: astl] ->
        [ast1; ast2 :: loop astl]
    | [ast :: astl] -> [ast :: loop astl]
    | [] -> [] ]
;

value input conf fname =
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
    match Util.p_getenv conf.env "templ" with
    [ Some x when List.mem "*" config_templ -> x
    | Some x when List.mem x config_templ -> x
    | Some _ | None ->
        match config_templ with
        [ [] | ["*"] -> ""
        | [x :: _] -> x ] ]
  in
  let dir =
    if dir = "" then Filename.current_dir_name
    else Filename.basename dir
  in
  match open_templ conf dir fname with
  [ Some ic ->
      let astl = parse_templ conf (Stream.of_channel ic) in
      let astl = strip_newlines_after_variables astl in
      do { close_in ic; astl }
  | None ->
      let title _ = Wserver.wprint "Error" in
      do {
        Util.header conf title;
        tag "ul" begin
          tag "li" begin
            Wserver.wprint "Cannot access file \"%s/%s.txt\".\n" dir fname;
          end;
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
  | Atransl b s c -> Atransl b (sf s) c
  | Awid_hei s -> Awid_hei (sf s)
  | Aif e alt ale -> Aif (subst sf e) (substl sf alt) (substl sf ale)
  | Aforeach (loc, s, sl) pl al ->
      Aforeach (loc, sf s, List.map sf sl) (List.map (substl sf) pl)
        (substl sf al)
  | Adefine f xl al alk ->
      Adefine (sf f) (List.map sf xl) (substl sf al) (substl sf alk)
  | Aapply loc f all -> Aapply loc (sf f) (List.map (substl sf) all)
  | Aint loc s -> Aint loc s
  | Aop1 op e -> Aop1 op (subst sf e)
  | Aop2 op e1 e2 -> Aop2 (sf op) (subst sf e1) (subst sf e2) ]
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

value eval_variable conf =
  fun 
  [ "action" -> conf.command
  | "border" -> string_of_int conf.border
  | "charset" -> conf.charset
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
  | "referer" -> Wserver.extract_param "referer: " '\n' conf.request
  | "right" -> conf.right
  | "sp" -> " "
  | "/" -> conf.xhs
  | _ -> raise Not_found ]
;

value eval_ast conf =
  fun
  [ Atext s -> s
  | Avar _ s [] ->
      try eval_variable conf s with
      [ Not_found -> " %%" ^ s ^ "?" ]
  | x -> not_impl "eval_ast" x ]
;

value eval_string_var conf eval_var s sl =
  try
    match eval_var [s :: sl] with
    [ VVstring s -> s
    | VVbool True -> "1"
    | VVbool False -> "0" ]
  with
  [ Not_found ->
      try
        match sl with
        [ [] -> eval_variable conf s
        | _ -> raise Not_found ]
      with
      [ Not_found -> " %" ^ String.concat "." [s :: sl] ^ "?" ] ]
;

value eval_subst f xl vl a =
  loop a xl vl where rec loop a xl vl =
    match (xl, vl) with
    [ ([x :: xl], [v :: vl]) -> loop (subst (subst_text x v) a) xl vl
    | ([], []) -> a
    | _ -> Atext (f ^ ": bad # of params") ]
;

value eval_transl_lexicon conf upp s c =
  let r =
    let nth = try Some (int_of_string c) with [ Failure _ -> None ] in
    match split_at_coloncolon s with
    [ None ->
        let s2 =
          match nth with
          [ Some n -> Util.transl_nth conf s n
          | None -> Util.transl conf s ]
        in
        Gutil.nominative s2
    | Some (s1, s2) ->
        try
          if String.length s2 > 0 && s2.[0] = '|' then
            let i = 1 in
            let j = String.rindex s2 '|' in
            let k = skip_spaces_and_newlines s2 (j + 1) in
            let s3 =
              let s = String.sub s2 i (j - i) in
              let astl = parse_templ conf (Stream.of_string s) in
              let astl = strip_newlines_after_variables astl in
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
            let transl_nth_format s =
              match nth with
              [ Some n -> Util.ftransl_nth conf s n
              | None -> Util.ftransl conf s ]
            in
            match Util.check_format "%t" s1 with
            [ Some s3 -> Printf.sprintf (transl_nth_format s3) (fun _ -> s2)
            | None ->
                match Util.check_format "%s" s1 with
                [ Some s3 -> Printf.sprintf (transl_nth_format s3) s2
                | None -> raise Not_found ] ]
          else raise Not_found
        with
        [ Not_found ->
            let s3 =
              match nth with
              [ Some n -> Util.transl_nth conf s2 n
              | None -> Util.transl conf s2 ]
            in
            Util.transl_decline conf s1 s3 ] ]
  in
  if upp then Util.capitale r else r
;

value eval_transl_inline conf s =
  let (s, alt) =
    Translate.inline conf.lang '%' (fun c -> "%" ^ String.make 1 c) s
  in
  if alt then "[" ^ s ^ "]" else s
;

value eval_transl conf upp s c =
  if c = "" && String.length s > 0 && s.[0] = '\n' then
    eval_transl_inline conf s
  else
    eval_transl_lexicon conf upp s c
;

value eval_bool_var conf eval_var =
  fun
  [ ["cancel_links"] -> conf.cancel_links
  | ["friend"] -> conf.friend
  | ["has_referer"] -> (* deprecated since version 5.00 *) 
      Wserver.extract_param "referer: " '\n' conf.request <> ""
  | ["wizard"] -> conf.wizard
  | sl ->
      match eval_var sl with
      [ VVbool x -> x
      | VVstring "" -> False
      | VVstring _ -> True ] ]
;

value nb_errors = ref 0;

value loc_of_expr =
  fun
  [ Aint loc _ -> loc
  | Avar loc _ _ -> loc
  | _ -> (-1, -1) ]
;

value rec bool_eval ((conf, eval_var, _) as ceva) =
  fun
  [ Aop2 "or" e1 e2 -> bool_eval ceva e1 || bool_eval ceva e2
  | Aop2 "and" e1 e2 -> bool_eval ceva e1 && bool_eval ceva e2
  | Aop2 op e1 e2 ->
      match op with
      [ "=" -> string_eval ceva e1 = string_eval ceva e2
      | "!=" -> string_eval ceva e1 <> string_eval ceva e2
      | ">" -> int_eval ceva e1 > int_eval ceva e2
      | ">=" -> int_eval ceva e1 >= int_eval ceva e2
      | "<" -> int_eval ceva e1 < int_eval ceva e2
      | "<=" -> int_eval ceva e1 <= int_eval ceva e2
      | _ -> do { Wserver.wprint "op %s???" op; False } ]
  | Aop1 "not" e -> not (bool_eval ceva e)
  | Aapply loc s el -> do { Wserver.wprint "not impl %s" s; False }
  | Avar loc s sl ->
      try eval_bool_var conf (eval_var loc) [s :: sl] with
      [ Not_found ->
          do {
            Wserver.wprint " %%%s" s;
            List.iter (fun s -> Wserver.wprint ".%s" s) sl;
            Wserver.wprint "?";
            False
          } ]
  | Atext s -> do { Wserver.wprint "\"%s\"???" s; False }
  | Aint loc s -> raise_with_loc loc (Failure "bool value expected")
  | Atransl _ s _ -> do { Wserver.wprint "[%s]???" s; False }
  | Aop1 _ _ | Adefine _ _ _ _ | Aforeach _ _ _ | Aif _ _ _ | Awid_hei _ ->
      do { Wserver.wprint "error14"; False } ]
and int_eval ceva =
  fun
  [ Aop2 "+" e1 e2 -> int_eval ceva e1 + int_eval ceva e2
  | Aop2 "-" e1 e2 -> int_eval ceva e1 - int_eval ceva e2
  | Aop2 "*" e1 e2 -> int_eval ceva e1 * int_eval ceva e2
  | Aop2 "%" e1 e2 -> int_eval ceva e1 mod int_eval ceva e2
  | Aint _ x -> int_of_string x
  | e ->
      let s = string_eval ceva e in
      try int_of_string s with
      [ Failure _ ->
          raise_with_loc (loc_of_expr e)
            (Failure (s ^ " int value expected")) ] ]
and string_eval ((conf, eval_var, eval_apply) as ceva) =
  fun
  [ Atext s -> s
  | Aapply loc s el ->
      eval_apply s
        (List.map
           (fun el -> String.concat "" (List.map (string_eval ceva) el)) el)
  | Avar loc s sl ->
      try eval_string_var conf (eval_var loc) s sl with
      [ Not_found ->
          do {
            Wserver.wprint " %%%s" s;
            List.iter (fun s -> Wserver.wprint ".%s" s) sl;
            Wserver.wprint "?";
            ""
          } ]
  | Atransl upp s c -> eval_transl conf upp s c
  | e ->
      try Num.to_string (num_eval ceva e) with
      [ Failure x -> x ] ]
and num_eval ((_, eval_var, _) as ceva) =
  fun
  [ Aint _ x -> Num.of_string x
  | Aop2 "+" x y -> Num.add (num_eval ceva x) (num_eval ceva y)
  | Aop2 "-" x y -> Num.sub (num_eval ceva x) (num_eval ceva y)
  | Aop2 "*" x (Aint _ y) -> Num.mul (num_eval ceva x) (int_of_string y)
  | Aop2 "%" x (Aint _ y) ->
      Num.of_int (Num.modl (num_eval ceva x) (int_of_string y))
  | Avar loc s sl ->
      try
        match eval_var loc [s :: sl] with
        [ VVstring s -> Num.of_string s
        | VVbool True -> Num.one
        | VVbool False -> Num.zero ]
      with
      [ Not_found ->
          failwith ("parse error/(" ^ String.concat "." [s :: sl] ^ ")") ]
  | _ -> failwith "parse_error6" ]
;

value handle_eval thing_eval nothing conf (eval_var, eval_apply) e =
  try thing_eval (conf, eval_var, eval_apply) e with
  [ Exc_located (bp, ep) exc ->
      do {
        incr nb_errors;
        IFDEF UNIX THEN do {
          if nb_errors.val <= 10 then do {
            Printf.eprintf "*** <W> template file";
            Printf.eprintf ", chars %d-%d" bp ep;
            Printf.eprintf ": %s\n" (Printexc.to_string exc);
            flush stderr;
          }
          else ();
        }
        ELSE () END;
        nothing
      } ]
;

value eval_bool_expr = handle_eval bool_eval False;
value eval_int_expr = handle_eval int_eval 0;
value eval_expr = handle_eval string_eval "eval_error";

value print_body_prop conf base =
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
    [ Not_found -> "" ]
  in
  Wserver.wprint "%s" (s ^ Util.body_prop conf)
;

value print_variable conf base =
  fun
  [ "base_header" -> Util.include_hed_trl conf (Some base) ".hed"
  | "base_trailer" -> Util.include_hed_trl conf (Some base) ".trl"
  | "body_prop" -> print_body_prop conf base
  | "copyright" -> Util.print_copyright conf
  | "hidden" -> Util.hidden_env conf
  | "message_to_wizard" -> Util.message_to_wizard conf
  | s ->
      try Wserver.wprint "%s" (eval_variable conf s) with
      [ Not_found -> Wserver.wprint " %%%s?" s ] ]
;

value print_var conf base eval_var s sl =
  try
    match eval_var [s :: sl] with
    [ VVstring s -> Wserver.wprint "%s" s
    | VVbool True -> Wserver.wprint "1"
    | VVbool False -> Wserver.wprint "0" ]
  with
  [ Not_found ->
      match sl with
      [ [] -> print_variable conf base s
      | _ ->
          do {
            Wserver.wprint " %%%s" s;
            List.iter (fun s -> Wserver.wprint ".%s" s) sl;
            Wserver.wprint "?"
          } ] ]
;

value print_apply f print_ast xl al vl =
  List.iter
    (fun a ->
       let a =
         loop a xl vl where rec loop a xl vl =
           match (xl, vl) with
           [ ([x :: xl], [v :: vl]) -> loop (subst (subst_text x v) a) xl vl
           | ([], []) -> a
           | _ ->
               Atext
                 (Printf.sprintf "%s: bad # of params (%d instead of %d)" f
                    (List.length vl) (List.length xl)) ]
       in
       print_ast a)
    al
;
