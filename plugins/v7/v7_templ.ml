open Geneweb.Config
open Geneweb.TemplAst

module Output = Geneweb.Output
module Translate = Geneweb.Translate
module Util = Geneweb.Util
module Version = Geneweb.Version

(* Parsing *)

type token =
    BANGEQUAL
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
  | EXP
  | STAR
  | IDENT of string
  | STRING of string
  | INT of string
  | LEXICON of bool * string * string

type loc_token =
    Tok of loc * token

exception Exc_located of loc * exn

let raise_with_loc loc exc =
  match exc with
    Exc_located (_, _) -> raise exc
  | _ -> raise (Exc_located (loc, exc))

let rec get_ident len =
  parser
    [< ''a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c;
       a = get_ident (Buff.store len c) ?! >] ->
      a
  | [< >] -> Buff.get len

let rec get_value len =
  parser
    [< '' ' | '>' | ';' | '\n' | '\r' | '\t' >] -> Buff.get len
  | [< 'c; a = get_value (Buff.store len c) ?! >] -> a

let rec get_string len =
  parser
    [< ''"' >] -> Buff.get len
  | [< 'c; a = get_string (Buff.store len c) ?! >] -> a

let rec get_int len =
  parser
    [< ''0'..'9' as c; a = get_int (Buff.store len c) ?! >] -> a
  | [< >] -> Buff.get len

let get_compound_var =
  let rec var_kont =
    parser
      [< ''.'; s = get_ident 0; sl = var_kont >] -> s :: sl
    | [< >] -> []
  in
  parser bp [< v = get_ident 0; vl = var_kont >] ep -> (bp, ep), v, vl

let get_variable =
  let rec var_kont =
    parser
      [< ''.'; s = get_ident 0; sl = var_kont >] -> s :: sl
    | [< '';' >] -> []
    | [< >] -> []
  in
  parser bp
    [< ''%' >] ep -> (bp, ep), "%", []
  | [< ''/' >] ep -> (bp, ep), "/", []
  | [< ''[' >] ep -> (bp, ep), "[", []
  | [< '']' >] ep -> (bp, ep), "]", []
  | [< ''(' >] ep -> (bp, ep), "(", []
  | [< '')' >] ep -> (bp, ep), ")", []
  | [< v = get_ident 0; vl = var_kont >] ep -> (bp, ep), v, vl

let rec transl_num_index =
  parser
    [< ''0'..'9' as c >] -> String.make 1 c ^ transl_num_index strm__
  | [< >] -> ""

let transl_index =
  parser
    [< ''0'..'9' as c >] -> String.make 1 c ^ transl_num_index strm__
  | [< ''a'..'z' as c >] -> String.make 1 c
  | [< >] -> ""

let lexicon_word =
  let upper =
    parser
      [< ''*' >] -> true
    | [< >] -> false
  in
  let rec text lev len =
    parser
      [< ''['; a = text (lev + 1) (Buff.store len '[') ?! >] -> a
    | [< '']'; s >] ->
        if lev = 0 then Buff.get len
        else text (lev - 1) (Buff.store len ']') s
    | [< 'c; a = text lev (Buff.store len c) ?! >] -> a
  in
  parser [< upp = upper; s = text 0 0; n = transl_index >] -> upp, s, n

let rec parse_comment =
  parser
    [< ''%'; a = parse_comment_after_percent ?! >] -> a
  | [< '_; a = parse_comment ?! >] -> a
and parse_comment_after_percent =
  parser
    [< '')'; a = parse_spaces_after_comment ?! >] -> a
  | [< ''('; s >] -> parse_comment s; parse_comment s
  | [< '_; a = parse_comment ?! >] -> a
and parse_spaces_after_comment =
  parser
    [< '' ' | '\n' | '\t' | '\r'; a = parse_spaces_after_comment ?! >] -> a
  | [< >] -> ()

let rec get_token =
  parser bp
    [< '' ' | '\t' | '\n' | '\r'; a = get_token ?! >] -> a
  | [< ''(' >] ep -> Tok ((bp, ep), LPAREN)
  | [< '')' >] ep -> Tok ((bp, ep), RPAREN)
  | [< '',' >] ep -> Tok ((bp, ep), COMMA)
  | [< ''.' >] ep -> Tok ((bp, ep), DOT)
  | [< ''=' >] ep -> Tok ((bp, ep), EQUAL)
  | [< ''+' >] ep -> Tok ((bp, ep), PLUS)
  | [< ''-' >] ep -> Tok ((bp, ep), MINUS)
  | [< ''*' >] ep -> Tok ((bp, ep), STAR)
  | [< ''^' >] ep -> Tok ((bp, ep), EXP)
  | [< ''/' >] ep -> Tok ((bp, ep), DIV)
  | [< ''%';
       a =
         parser
           [< ''('; _ = parse_comment; a = get_token ?! >] -> a
         | [< >] ep -> Tok ((bp, ep), PERCENT) >] ->
      a
  | [< ''!'; ''=' ?? "'=' expected" >] ep -> Tok ((bp, ep), BANGEQUAL)
  | [< ''>';
       tok =
         parser
           [< ''=' >] -> GREATEREQUAL
         | [< >] -> GREATER ?! >] ep ->
      Tok ((bp, ep), tok)
  | [< ''<';
       tok =
         parser
           [< ''=' >] -> LESSEQUAL
         | [< >] -> LESS ?! >] ep ->
      Tok ((bp, ep), tok)
  | [< ''"'; s = get_string 0 >] ep -> Tok ((bp, ep), STRING s)
  | [< ''0'..'9' as c; s = get_int (Buff.store 0 c) >] ep ->
      Tok ((bp, ep), INT s)
  | [< ''['; upp, s, n = lexicon_word >] ep ->
      Tok ((bp, ep), LEXICON (upp, s, n))
  | [< s = get_ident 0 >] ep -> Tok ((bp, ep), IDENT s)

module Buff2 = Buff.Make (struct  end)

let rec parse_var =
  parser [< 'Tok (loc, IDENT id); loc, idl = ident_list loc >] -> loc, id, idl
and ident_list (bp, _ as loc) =
  parser
    [< 'Tok (_, DOT);
       loc, id =
         (parser
            [< 'Tok ((_, ep), IDENT id) >] -> (bp, ep), id
          | [< 'Tok ((_, ep), INT id) >] -> (bp, ep), id
          | [< 'Tok (loc, _) >] -> loc, "parse_error1");
       loc, idl = ident_list loc >] ->
      loc, id :: idl
  | [< >] -> loc, []

let rec parse_expr strm = parse_expr_if strm
and parse_expr_if =
  parser
    [< 'Tok (_, IDENT "if"); e1 = parse_expr_or; 'Tok (_, IDENT "then");
       e2 = parse_expr_or; 'Tok (_, IDENT "else"); e3 = parse_expr_or >] ->
      Aif (e1, [e2], [e3])
  | [< a = parse_expr_or >] -> a
and parse_expr_or =
  parser
    [< e = parse_expr_and;
       a =
         parser
           [< 'Tok (loc, IDENT "or") >] ->
             Aop2 (loc, "or", e, parse_expr_or strm__)
         | [< >] -> e >] ->
      a
and parse_expr_and =
  parser
    [< e = parse_expr_is_substr;
       a =
         parser
           [< 'Tok (loc, IDENT "and") >] ->
             Aop2 (loc, "and", e, parse_expr_and strm__)
         | [< >] -> e >] ->
      a
and parse_expr_is_substr =
  parser
    [< e = parse_expr_in;
       a =
         parser
           [< 'Tok (loc, IDENT "is_substr"); s >] ->
             Aop2 (loc, "is_substr", e, (parse_expr_is_substr s))
         | [< >] -> e >] ->
      a
and parse_expr_in =
  parser
    [< e = parse_expr_3;
       a =
         parser
           [< 'Tok (loc, IDENT "in"); s >] ->
             Aop2 (loc, "in", e, (parse_expr_in s))
         | [< >] -> e >] ->
      a
and parse_expr_3 =
  parser
    [< e = parse_expr_4;
       a =
         parser
           [< 'Tok (loc, EQUAL); e2 = parse_expr_4 >] ->
             Aop2 (loc, "=", e, e2)
         | [< 'Tok (loc, BANGEQUAL); e2 = parse_expr_4 >] ->
             Aop2 (loc, "!=", e, e2)
         | [< 'Tok (loc, GREATER); e2 = parse_expr_4 >] ->
             Aop2 (loc, ">", e, e2)
         | [< 'Tok (loc, GREATEREQUAL); e2 = parse_expr_4 >] ->
             Aop2 (loc, ">=", e, e2)
         | [< 'Tok (loc, LESS); e2 = parse_expr_4 >] -> Aop2 (loc, "<", e, e2)
         | [< 'Tok (loc, LESSEQUAL); e2 = parse_expr_4 >] ->
             Aop2 (loc, "<=", e, e2)
         | [< >] -> e ?! >] ->
      a
and parse_expr_4 = parser [< e = parse_expr_5; a = parse_expr_4_kont e >] -> a
and parse_expr_4_kont e =
  parser
    [< 'Tok (loc, PLUS); e2 = parse_expr_5;
       a = parse_expr_4_kont (Aop2 (loc, "+", e, e2)) >] ->
      a
  | [< 'Tok (loc, MINUS); e2 = parse_expr_5;
       a = parse_expr_4_kont (Aop2 (loc, "-", e, e2)) >] ->
      a
  | [< >] -> e
and parse_expr_5 =
  parser [< e = parse_simple_expr; a = parse_expr_5_kont e >] -> a
and parse_expr_5_kont e =
  parser
    [< 'Tok (loc, STAR); e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 (loc, "*", e, e2)) >] ->
      a
  | [< 'Tok (loc, EXP); e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 (loc, "^", e, e2)) >] ->
      a
  | [< 'Tok (loc, DIV); e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 (loc, "/", e, e2)) >] ->
      a
  | [< 'Tok (loc, PERCENT); e2 = parse_simple_expr;
       a = parse_expr_5_kont (Aop2 (loc, "%", e, e2)) >] ->
      a
  | [< >] -> e
and parse_simple_expr =
  parser
    [< 'Tok (_, LPAREN); e = parse_expr;
       a =
         parser
           [< 'Tok (_, RPAREN) >] -> e
         | [< 'Tok (loc, _) >] -> Avar (loc, "parse_error2", []) >] ->
      a
  | [< 'Tok (loc, IDENT "not"); e = parse_simple_expr >] ->
      Aop1 (loc, "not", e)
  | [< 'Tok (loc, STRING s) >] -> Atext (loc, s)
  | [< 'Tok (loc, INT s) >] -> Aint (loc, s)
  | [< 'Tok (loc, LEXICON (upp, s, n)) >] -> Atransl (loc, upp, s, n)
  | [< loc, id, idl = parse_var;
       a =
         parser
           [< t = parse_tuple >] -> Aapply (loc, id, t)
         | [< >] -> Avar (loc, id, idl) >] ->
      a
and parse_tuple strm =
  let rec parse_expr_list =
    parser
      [< x = parse_expr;
         xl =
           parser
             [< 'Tok (_, COMMA); a = parse_expr_list >] -> a
           | [< >] -> [] ?! >] ->
        [x] :: xl
  in
  let parse_tuple =
    parser
      [< 'Tok (_, LPAREN);
         xl =
           (parser
              [< a = parse_expr_list >] -> a
            | [< >] -> []);
         a =
           parser
             [< 'Tok (_, RPAREN) >] -> xl
           | [< 'Tok (loc, _) >] -> [[Atext (loc, "parse_error4")]] >] ->
        a
  in
  parse_tuple strm

let parse_char_stream p strm =
  let f _ = try Some (get_token strm) with Stream.Failure -> None in
  p (Stream.from f)

let parse_char_stream_semi p strm =
  let opt_semi = Stream.peek strm <> Some '(' in
  let f _ = try Some (get_token strm) with Stream.Failure -> None in
  let r = p (Stream.from f) in
  if opt_semi then
    begin let (strm__ : _ Stream.t) = strm in
      match Stream.peek strm__ with
        Some ';' -> Stream.junk strm__; ()
      | _ -> ()
    end;
  r

let parse_formal_params strm =
  let rec parse_ident_list =
    parser
      [< 'Tok (_, IDENT x);
         xl =
           parser
             [< 'Tok (_, COMMA); a = parse_ident_list >] -> a
           | [< >] -> [] ?! >] ->
        x :: xl
  in
  let parse_tuple =
    parser
      [< 'Tok (_, LPAREN);
         xl =
           (parser
              [< a = parse_ident_list >] -> a
            | [< >] -> []);
         a =
           parser
             [< 'Tok (_, RPAREN) >] -> xl
           | [< >] -> ["parse_error5"] ?! >] ->
        a
  in
  let f _ = try Some (get_token strm) with Stream.Failure -> None in
  parse_tuple (Stream.from f)

let strip_newlines_after_variables =
  let rec loop =
    function
      Atext (loc, s) :: astl ->
        let s =
          let rec loop i =
            if i = String.length s then s
            else if s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\r' then
              loop (i + 1)
            else if s.[i] = '\n' then
              String.sub s (i + 1) (String.length s - i - 1)
            else s
          in
          loop 0
        in
        Atext (loc, s) :: loop astl
    | Aif (s, alt, ale) :: astl -> Aif (s, loop alt, loop ale) :: loop astl
    | Aforeach (v, pl, al) :: astl -> Aforeach (v, pl, loop al) :: loop astl
    | Adefine (f, x, al, alk) :: astl ->
        Adefine (f, x, loop al, loop alk) :: loop astl
    | Aapply (loc, f, all) :: astl ->
        Aapply (loc, f, List.map loop all) :: loop astl
    | Alet (k, v, al) :: astl -> Alet (k, loop v, loop al) :: loop astl
    | Afor (i, min, max, al) :: astl ->
        Afor (i, min, max, loop al) :: loop astl
    | (Atransl (_, _, _, _) | Awid_hei _ as ast1) :: (Atext (_, _) as ast2) ::
      astl ->
        ast1 :: ast2 :: loop astl
    | Ainclude (file, al) :: astl -> Ainclude (file, loop al) :: loop astl
    | ast :: astl -> ast :: loop astl
    | [] -> []
  in
  loop

let included_files = ref []

let begin_end_include conf fname al =
  if conf.trace_templ then
    Atext ((0,0), "\n\n<!-- begin include " ^ fname ^ " -->\n")
    :: al
    @ [ Atext ((0,0), "<!-- end include " ^ fname ^ " -->\n") ]
  else al

let parse_templ conf strm =
  let rec parse_astl astl bol len end_list strm =
    match strm with parser bp
      [< ''%' >] ->
        let astl =
          if len = 0 then astl
          else Atext ((bp - len, bp), Buff2.get len) :: astl
        in
        begin match get_variable strm with
          _, ("%" | "[" | "]" as c), [] ->
            parse_astl (Atext ((bp - 1, bp), c) :: astl) false 0 end_list strm
        | _, "(", [] ->
            parse_comment strm; parse_astl astl false 0 end_list strm
        | _, v, [] when List.mem v end_list -> List.rev astl, v
        | _, "define", [] -> parse_define astl end_list strm
        | _, "let", [] -> parse_let astl end_list strm
        | _, "include", [] -> parse_include astl end_list strm
        | x ->
            let ast =
              match x with
                _, "if", [] -> parse_if strm
              | _, "foreach", [] -> parse_foreach strm
              | _, "apply", [] -> parse_apply bp strm
              | _, "expr", [] -> parse_expr_stmt strm
              | _, "for", [] -> parse_for strm
              | _, "wid_hei", [] -> Awid_hei (get_value 0 strm)
              | (_, ep), v, vl -> Avar ((bp, ep), v, vl)
            in
            parse_astl (ast :: astl) false 0 end_list strm
        end
    | [< ''[' >] ->
        let astl =
          if len = 0 then astl
          else Atext ((bp - len, bp), Buff2.get len) :: astl
        in
        let a =
          let (upp, s, n) = lexicon_word strm in
          if String.length s > 1 && (s.[0] = '[' || s.[0] = '@') then
            let (astl, _) = parse_astl [] false 0 [] (Stream.of_string s) in
            Aconcat ((bp, Stream.count strm), astl)
          else Atransl ((bp, Stream.count strm), upp, s, n)
        in
        parse_astl (a :: astl) false 0 end_list strm
    | [< 'c >] ->
        let empty_c = c = ' ' || c = '\t' in
        let len = if empty_c && bol then len else Buff2.store len c in
        let bol = empty_c && bol || c = '\n' in
        parse_astl astl bol len end_list strm
    | [< >] ->
        let astl =
          if len = 0 then astl
          else Atext ((bp - len, bp), Buff2.get len) :: astl
        in
        List.rev astl, ""
  and parse_define astl end_list strm =
    let fxlal =
      try
        let f = get_ident 0 strm in
        let xl = parse_formal_params strm in
        let (al, _) = parse_astl [] false 0 ["end"] strm in
        begin let rec loop () =
          match strm with parser
            [< ''\n' | '\r' >] -> loop ()
          | [< >] -> ()
        in
          loop ()
        end;
        Some (f, xl, al)
      with Stream.Failure | Stream.Error _ -> None
    in
    let (alk, v) = parse_astl [] false 0 end_list strm in
    let astl =
      match fxlal with
        Some (f, xl, al) -> Adefine (f, xl, al, alk) :: astl
      | None ->
          let bp = Stream.count strm - 1 in
          Atext ((bp, bp + 1), "define error") :: (alk @ astl)
    in
    List.rev astl, v
  and parse_let astl end_list strm =
    let (ast, tok) =
      try
        let k = get_ident 0 strm in
        match strm with parser
          [< '';' >] ->
            let (v, _) = parse_astl [] false 0 ["in"] strm in
            let (al, tok) = parse_astl [] false 0 end_list strm in
            Alet (k, v, al), tok
      with Stream.Failure | Stream.Error _ ->
        let bp = Stream.count strm - 1 in
        Atext ((bp, bp + 1), "let syntax error"), ""
    in
    List.rev (ast :: astl), tok
  and parse_include astl end_list strm =
    let ast =
      try
        let file = get_value 0 strm in
        match List.assoc_opt file !included_files with
        | Some ast -> Some (Ainclude (file, ast))
        | None ->
          match Util.open_templ_fname conf file with
          | Some (ic, fname) ->
            let strm2 = Stream.of_channel ic in
            let (al, _) = parse_astl [] false 0 [] strm2 in
            close_in ic;
            let al = begin_end_include conf fname al in
            let () = included_files := (file, al) :: !included_files in
            Some (Ainclude (file, al))
          | None -> None
      with Stream.Failure | Stream.Error _ -> None
    in
    let (alk, tok) = parse_astl [] false 0 end_list strm in
    match ast with
      Some ast -> List.rev (ast :: astl) @ alk, tok
    | None -> astl @ alk, tok
  and parse_apply bp strm =
    try
      let f = get_ident 0 strm in
      match strm with parser
        [< ''%' >] ->
          begin match get_variable strm with
            _, "with", [] ->
              let all =
                let rec loop () =
                  let (al, tok) = parse_astl [] false 0 ["and"; "end"] strm in
                  match tok with
                    "and" -> al :: loop ()
                  | _ -> [al]
                in
                loop ()
              in
              Aapply ((bp, Stream.count strm), f, all)
          | _ -> raise (Stream.Error "'with' expected")
          end
      | [< >] ->
          let el = parse_char_stream parse_tuple strm in
          Aapply ((bp, Stream.count strm), f, el)
    with Stream.Failure | Stream.Error _ ->
      let bp = Stream.count strm - 1 in
      Atext ((bp, bp + 1), "apply syntax error")
  and parse_expr_stmt strm = parse_char_stream_semi parse_simple_expr strm
  and parse_if strm =
    let e = parse_char_stream_semi parse_simple_expr strm in
    let (al1, al2) =
      let rec loop () =
        let (al1, tok) =
          parse_astl [] false 0 ["elseif"; "else"; "end"] strm
        in
        match tok with
          "elseif" ->
            let e2 = parse_char_stream_semi parse_simple_expr strm in
            let (al2, al3) = loop () in al1, [Aif (e2, al2, al3)]
        | "else" ->
            let (al2, _) = parse_astl [] false 0 ["end"] strm in al1, al2
        | _ -> al1, []
      in
      loop ()
    in
    Aif (e, al1, al2)
  and parse_for strm =
    try
      let iterator = get_ident 0 strm in
      match strm with parser
        [< '';' >] ->
          let min = parse_char_stream_semi parse_simple_expr strm in
          let max = parse_char_stream_semi parse_simple_expr strm in
          let (al, _) = parse_astl [] false 0 ["end"] strm in
          Afor (iterator, min, max, al)
    with Stream.Failure | Stream.Error _ ->
      let bp = Stream.count strm - 1 in
      Atext ((bp, bp + 1), "for syntax error")
  and parse_foreach strm =
    let (loc, v, vl) = get_compound_var strm in
    let params =
      if Stream.peek strm = Some '(' then parse_char_stream parse_tuple strm
      else
        begin
          begin let (strm__ : _ Stream.t) = strm in
            match Stream.peek strm__ with
              Some ';' -> Stream.junk strm__; ()
            | _ -> ()
          end;
          []
        end
    in
    let (astl, _) = parse_astl [] false 0 ["end"] strm in
    Aforeach ((loc, v, vl), params, astl)
  in
  let astl = fst (parse_astl [] true 0 [] strm) in
  strip_newlines_after_variables astl

let input_templ conf fname =
  match Util.open_templ conf fname with
    Some ic ->
      let astl = parse_templ conf (Stream.of_channel ic) in
      close_in ic; Some astl
  | None -> None

(* Common evaluation functions *)

let subst_text x v s =
  if String.length x = 0 then s
  else
    let rec loop len i i_ok =
      if i = String.length s then
        if i_ok > 0 then loop (Buff.store len s.[i-i_ok]) (i - i_ok + 1) 0
        else Buff.get len
      else if s.[i] = x.[i_ok] then
        if i_ok = String.length x - 1 then loop (Buff.mstore len v) (i + 1) 0
        else loop len (i + 1) (i_ok + 1)
      else if i_ok > 0 then loop (Buff.store len s.[i-i_ok]) (i - i_ok + 1) 0
      else loop (Buff.store len s.[i]) (i + 1) 0
    in
    loop 0 0 0

let rec subst sf =
  function
    Atext (loc, s) -> Atext (loc, sf s)
  | Avar (loc, s, sl) ->
      let s1 = sf s in
      let sl1 = List.map sf sl in
      if sl = [] &&
         (try let _ = int_of_string s1 in true with Failure _ -> false)
      then
        Aint (loc, s1)
      else
        let strm = Stream.of_string s1 in
        let (_, s2, sl2) = get_compound_var strm in
        if Stream.peek strm <> None then Avar (loc, s1, sl1)
        else Avar (loc, s2, sl2 @ sl1)
  | Atransl (loc, b, s, c) -> Atransl (loc, b, sf s, c)
  | Aconcat (loc, al) -> Aconcat (loc, List.map (subst sf) al)
  | Awid_hei s -> Awid_hei (sf s)
  | Aif (e, alt, ale) -> Aif (subst sf e, substl sf alt, substl sf ale)
  | Aforeach ((loc, s, sl), pl, al) ->
      (* Dans le cas d'une "compound variable", il faut la décomposer. *)
      (* Ex: "ancestor.father".family  =>  ancestor.father.family      *)
      let s1 = sf s in
      let strm = Stream.of_string s1 in
      let (_, s2, sl2) = get_compound_var strm in
      let (s, sl) =
        if Stream.peek strm <> None then s, sl else s2, sl2 @ sl
      in
      Aforeach
        ((loc, sf s, List.map sf sl), List.map (substl sf) pl, substl sf al)
  | Afor (i, min, max, al) ->
      Afor (sf i, subst sf min, subst sf max, substl sf al)
  | Adefine (f, xl, al, alk) ->
      Adefine (sf f, List.map sf xl, substl sf al, substl sf alk)
  | Aapply (loc, f, all) -> Aapply (loc, sf f, List.map (substl sf) all)
  | Alet (k, v, al) -> Alet (sf k, substl sf v, substl sf al)
  | Ainclude (file, al) -> Ainclude (sf file, substl sf al)
  | Aint (loc, s) -> Aint (loc, s)
  | Aop1 (loc, op, e) -> Aop1 (loc, op, subst sf e)
  | Aop2 (loc, op, e1, e2) -> Aop2 (loc, sf op, subst sf e1, subst sf e2)
and substl sf al = List.map (subst sf) al

let split_at_coloncolon s =
  let rec loop i =
    if i >= String.length s - 1 then None
    else
      match s.[i], s.[i+1] with
        ':', ':' ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 2) (String.length s - i - 2) in
          Some (s1, s2)
      | _ -> loop (i + 1)
  in
  loop 0

let rec skip_spaces_and_newlines s i =
  if i = String.length s then i
  else
    match s.[i] with
      ' ' | '\n' | '\r' -> skip_spaces_and_newlines s (i + 1)
    | _ -> i

let not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  "Templ." ^ func ^ ": not impl " ^ desc

let setup_link conf =
  let s = Mutil.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    let s = "http://" ^ String.sub s 0 i ^ ":2316/" in
    "<a href=\"" ^ s ^ "gwsetup?v=main.htm\">gwsetup</a>"
  with Not_found -> ""

let rec eval_variable conf =
  function
    ["bvar"; v] -> (try List.assoc v conf.base_env with Not_found -> "")
  | ["evar"; v; "ns"] ->
      begin try
        let vv = List.assoc v (conf.env @ conf.henv) in
        Util.escape_html (Mutil.gen_decode false vv)
      with Not_found -> ""
      end
  | ["evar"; v] ->
      begin match Util.p_getenv (conf.env @ conf.henv) v with
        Some vv -> Util.escape_html vv
      | None -> ""
      end
  | "time" :: sl -> eval_time_var conf sl
  | ["user"; "ident"] -> conf.user
  | ["user"; "name"] -> conf.username
  | [s] -> eval_simple_variable conf s
  | _ -> raise Not_found
and eval_time_var conf =
  function
    ["hours"] -> let (hh, _, _) = conf.time in Printf.sprintf "%02d" hh
  | ["minutes"] -> let (_, mm, _) = conf.time in Printf.sprintf "%02d" mm
  | ["seconds"] -> let (_, _, ss) = conf.time in Printf.sprintf "%02d" ss
  | [] -> let (hh, mm, ss) = conf.time in Printf.sprintf "%02d:%02d:%02d" hh mm ss
  | _ -> raise Not_found
and eval_simple_variable conf =
  function
    "action" -> conf.command
  | "border" -> string_of_int conf.border
  | "charset" -> conf.charset
  | "connections" ->
      begin match conf.n_connect with
        Some (c, cw, cf, _) ->
          if c > 0 then
            " - " ^ Printf.sprintf "%s %d" (Util.transl conf "connections") c ^
            (if cw > 0 then
               Printf.sprintf ", %s %s"
                 (Util.transl_nth conf
                    "wizard/wizards/friend/friends/exterior" 1)
                 (if conf.wizard then
                    Printf.sprintf "<a href=\"%sm=CONN_WIZ\">%d</a>"
                      (Util.commd conf) cw
                  else string_of_int cw)
             else "") ^
            (if cf > 0 then
               Printf.sprintf ", %s %d"
                 (Util.transl_nth conf
                    "wizard/wizards/friend/friends/exterior" 3)
                 cf
             else "")
          else ""
      | None -> ""
      end
  | "doctype" -> Util.doctype conf ^ "\n"
  | "doctype_transitional" ->
      let doctype =
        match Util.p_getenv conf.base_env "doctype" with
          Some ("html-4.01" | "html-4.01-trans") -> "html-4.01-trans"
        | _ -> "xhtml-1.0-trans"
      in
      let conf =
        {conf with base_env = ("doctype", doctype) :: conf.base_env}
      in
      Util.doctype conf ^ "\n"
  | "highlight" -> conf.highlight
  | "image_prefix" -> Util.image_prefix conf
  | "lang" -> conf.lang
  | "left" -> conf.left
  | "nl" -> "\n"
  | "nn" -> ""
  | "prefix" -> Util.commd conf
  | "prefix_2" -> Util.commd_2 conf
  | "prefix_base" -> Util.prefix_base conf
  | "prefix_base_2" -> Util.prefix_base_2 conf
  | "prefix_base_password" -> Util.prefix_base_password conf
  | "prefix_base_password_2" -> Util.prefix_base_password_2 conf
  | "prefix_no_iz" ->
      let henv =
        List.fold_left (fun accu k -> List.remove_assoc k accu) conf.henv
          ["iz"; "nz"; "pz"; "ocz"]
      in
      Util.commd {conf with henv = henv}
  | "prefix_no_templ" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "templ" then henv else (k, v) :: henv)
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) ->
        if ( (k = "oc" || k = "ocz") && v = "0" ) || v = "" then
          c else c ^ k ^ "=" ^ v ^ "&") c (henv @ conf.senv)
  | "prefix_no_pmod" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "p_mod" then henv else (k, v) :: henv)
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ "&") c
        (henv @ conf.senv)
  | "prefix_no_wide" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "wide" then henv else (k, v) :: henv)
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) ->
        if ( (k = "oc" || k = "ocz") && v = "0" ) || v = "" then
          c else c ^ k ^ "=" ^ v ^ "&") c (henv @ conf.senv)
  | "prefix_no_lang" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "lang" then henv else (k, v) :: henv)
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) ->
          if ( (k = "oc" || k = "ocz") && v = "0") || v = "" then c
          else c ^ k ^ "=" ^ v ^ "&") c (henv @ conf.senv)
  | "prefix_no_all" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "templ" || k = "p_mod" || k = "wide" then henv else (k, v) :: henv)
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) ->
        if ( ( k = "oc" || k = "ocz") && v = "0") || v = "" then c
        else c ^ k ^ "=" ^ v ^ "&") c (henv @ conf.senv)
  | "referer" -> Util.get_referer conf
  | "right" -> conf.right
  | "setup_link" -> if conf.setup_link then " - " ^ setup_link conf else ""
  | "sp" -> " "
  | "suffix" ->
      (* On supprime de env toutes les paires qui sont dans (henv @ senv) *)
      let l =
        List.fold_left (fun accu (k, _) -> List.remove_assoc k accu) conf.env
          (List.rev_append conf.henv conf.senv)
      in
      List.fold_left
        (fun c (k, v) ->
          if ( ( k = "oc" || k = "ocz" ) && v = "0" ) || k = "" then c
          else c ^ k ^ "=" ^ v ^ "&") "" l
  | "url" ->
      let c = Util.commd conf in
      (* On supprime de env toutes les paires qui sont dans (henv @ senv) *)
      let l =
        List.fold_left (fun accu (k, _) -> List.remove_assoc k accu) conf.env
          (List.rev_append conf.henv conf.senv)
      in
      List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ "&") c l
  | "version" -> Version.txt
  | "/" -> ""
  | _ -> raise Not_found

let rec string_of_expr_val =
  function
    VVstring s -> s
  | VVbool true -> "1"
  | VVbool false -> "0"
  | VVother f -> string_of_expr_val (f [])

let eval_string_var conf eval_var sl =
  try eval_var sl with
    Not_found ->
      try VVstring (eval_variable conf sl) with
        Not_found -> VVstring (" %" ^ String.concat "." sl ^ "?")

let eval_var_handled conf sl =
  try eval_variable conf sl with
    Not_found -> Printf.sprintf " %%%s?" (String.concat "." sl)

let apply_format conf nth s1 s2 =
  let transl_nth_format s =
    match nth with
      Some n -> Util.ftransl_nth conf s n
    | None -> Util.ftransl conf s
  in
  match Util.check_format "%t" s1 with
    Some s3 -> Printf.sprintf (transl_nth_format s3) (fun _ -> s2)
  | None ->
      match Util.check_format "%s" s1 with
        Some s3 -> Printf.sprintf (transl_nth_format s3) s2
      | None ->
          match Util.check_format "%d" s1 with
            Some s3 -> Printf.sprintf (transl_nth_format s3) (int_of_string s2)
          | None ->
              match Util.check_format "%s%s" s1 with
                Some s3 ->
                  let (s21, s22) =
                    let i = String.index s2 ':' in
                    String.sub s2 0 i,
                    String.sub s2 (i + 1) (String.length s2 - i - 1)
                  in
                  Printf.sprintf (transl_nth_format s3) s21 s22
              | None -> raise Not_found

let rec eval_ast conf =
  function
    Atext (_, s) -> s
  | Avar (_, s, sl) -> eval_var_handled conf (s :: sl)
  | Atransl (_, upp, s, c) -> eval_transl conf upp s c
  | ast -> not_impl "eval_ast" ast
and eval_transl conf upp s c =
  if c = "" && String.length s > 0 && s.[0] = '\n' then
    eval_transl_inline conf s
  else eval_transl_lexicon conf upp s c
and eval_transl_inline conf s =
    fst @@ Translate.inline conf.lang '%' (fun c -> "%" ^ String.make 1 c) s
and eval_transl_lexicon conf upp s c =
  let r =
    let nth = try Some (int_of_string c) with Failure _ -> None in
    match split_at_coloncolon s with
      None ->
        let s2 =
          match nth with
            Some n -> Util.transl_nth conf s n
          | None -> Util.transl conf s
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
                Some n -> Util.transl_nth conf s4 n
              | None -> Util.transl conf s4
            in
            let s2 = s3 ^ s5 in Util.transl_decline conf s1 s2
          else if String.length s2 > 0 && s2.[0] = ':' then
            let s2 = String.sub s2 1 (String.length s2 - 1) in
            try apply_format conf nth s1 s2 with Failure _ -> raise Not_found
          else raise Not_found
        with Not_found ->
          let s3 =
            match nth with
              Some n -> Util.transl_nth conf s2 n
            | None -> if s2 = "" then "" else Util.transl conf s2
          in
          Util.transl_decline conf s1 s3
  in
  let r = Util.translate_eval r in if upp then Utf8.capitalize_fst r else r

let nb_errors = ref 0

let loc_of_expr =
  function
    Atext (loc, _) -> loc
  | Avar (loc, _, _) -> loc
  | Atransl (loc, _, _, _) -> loc
  | Aapply (loc, _, _) -> loc
  | Aop1 (loc, _, _) -> loc
  | Aop2 (loc, _, _, _) -> loc
  | Aint (loc, _) -> loc
  | _ -> -1, -1

let templ_eval_var conf =
  function
  | ["browsing_with_sosa_ref"] ->
      begin match Util.p_getenv conf.env "sosa_ref" with
        Some _ -> VVbool true
      | _ -> VVbool false
      end
  | ["cancel_links"] -> VVbool (Util.p_getenv conf.env "cgl" = Some "on" )
  | ["cgi"] -> VVbool conf.cgi
  | ["false"] -> VVbool false
  | ["has_referer"] ->
      (* deprecated since version 5.00 *)
      VVbool (Mutil.extract_param "referer: " '\n' conf.request <> "")
  | ["just_friend_wizard"] -> VVbool conf.just_friend_wizard
  | ["friend"] -> VVbool conf.friend
  | ["manitou"] -> VVbool conf.manitou
  | ["supervisor"] -> VVbool conf.supervisor
  | ["true"] -> VVbool true
  | ["wizard"] -> VVbool conf.wizard
  | ["is_printed_by_template"] -> VVbool conf.is_printed_by_template
  | _ -> raise Not_found

let bool_of e =
  function
    VVbool b -> b
  | VVstring _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "bool value expected")

let string_of e =
  function
    VVstring s -> s
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "string value expected")

let int_of e =
  function
    VVstring s ->
      begin try int_of_string s with
        Failure _ ->
          raise_with_loc (loc_of_expr e)
            (Failure ("int value expected\nFound = " ^ s))
      end
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "int value expected")

let num_of e =
  function
    VVstring s ->
      begin try Sosa.of_string s with
        Failure _ ->
          raise_with_loc (loc_of_expr e)
            (Failure ("num value expected\nFound = " ^ s))
      end
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "num value expected")

let rec eval_expr (conf, eval_var, eval_apply as ceva) =
  function
    Atext (_, s) -> VVstring s
  | Avar (loc, s, sl) ->
      begin try eval_var loc (s :: sl) with
        Not_found ->
          begin try templ_eval_var conf (s :: sl) with
            Not_found ->
              raise_with_loc loc
                (Failure
                   ("unbound var \"" ^ String.concat "." (s :: sl) ^ "\""))
          end
      end
  | Atransl (_, upp, s, c) -> VVstring (eval_transl conf upp s c)
  | Aconcat (_, al) ->
      let vl = List.map (eval_expr ceva) al in
      let sl = List.map string_of_expr_val vl in
      VVstring (String.concat "" sl)
  | Aapply (loc, s, ell) ->
      let vl =
        List.map
          (fun el ->
             match List.map (eval_expr ceva) el with
               [e] -> e
             | el ->
                 let sl = List.map string_of_expr_val el in
                 VVstring (String.concat "" sl))
          ell
      in
      VVstring (eval_apply loc s vl)
  | Aop1 (loc, op, e) ->
      let v = eval_expr ceva e in
      begin match op with
        "not" -> VVbool (not (bool_of e v))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\""))
      end
  | Aop2 (loc, op, e1, e2) ->
      let int e = int_of e (eval_expr ceva e) in
      let num e = num_of e (eval_expr ceva e) in
      let bool e = bool_of e (eval_expr ceva e) in
      let string e = string_of e (eval_expr ceva e) in
      begin match op with
        "and" -> VVbool (if bool e1 then bool e2 else false)
      | "or" -> VVbool (if bool e1 then true else bool e2)
      | "is_substr" | "in" -> VVbool (Mutil.contains (string e2) (string e1))
      | "=" -> VVbool (eval_expr ceva e1 = eval_expr ceva e2)
      | "<" -> VVbool (int e1 < int e2)
      | ">" -> VVbool (int e1 > int e2)
      | "!=" -> VVbool (eval_expr ceva e1 <> eval_expr ceva e2)
      | "<=" -> VVbool (int e1 <= int e2)
      | ">=" -> VVbool (int e1 >= int e2)
      | "+" -> VVstring (Sosa.to_string (Sosa.add (num e1) (num e2)))
      | "-" -> VVstring (Sosa.to_string (Sosa.sub (num e1) (num e2)))
      | "*" -> VVstring (Sosa.to_string (Sosa.mul (num e1) (int e2)))
      | "^" -> VVstring (Sosa.to_string (Sosa.exp (num e1) (int e2)))
      | "/" -> VVstring (Sosa.to_string (Sosa.div (num e1) (int e2)))
      | "%" -> VVstring (Sosa.to_string (Sosa.modl (num e1) (int e2)))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\""))
      end
  | Aint (_, s) -> VVstring s
  | e -> raise_with_loc (loc_of_expr e) (Failure (not_impl "eval_expr" e))

let line_of_loc conf fname (bp, ep) =
  match Util.open_templ conf fname with
    Some ic ->
      let strm = Stream.of_channel ic in
      let rec loop lin =
        let rec scan_line col =
          parser cnt
            [< 'c; s >] ->
              if cnt < bp then
                if c = '\n' then loop (lin + 1) else scan_line (col + 1) s
              else let col = col - (cnt - bp) in lin, col, col + ep - bp
        in
        scan_line 0 strm
      in
      let r = try Some (loop 1) with Stream.Failure -> None in close_in ic; r
  | None -> None

let template_file = ref ""
let print_error conf (bp, ep) exc =
  incr nb_errors;
  if !nb_errors <= 10 then
    begin
      if !template_file = "" then Printf.eprintf "*** <W> template file"
      else Printf.eprintf "File \"%s.txt\"" !template_file;
      let line =
        if !template_file = "" then None
        else line_of_loc conf !template_file (bp, ep)
      in
      Printf.eprintf ", ";
      begin match line with
        Some (lin, col1, col2) ->
          Printf.eprintf "line %d, characters %d-%d:\n" lin col1 col2
      | None -> Printf.eprintf "characters %d-%d:\n" bp ep
      end;
      begin match exc with
        Failure s -> Printf.eprintf "Failed - %s" s
      | _ -> Printf.eprintf "%s" (Printexc.to_string exc)
      end;
      Printf.eprintf "\n\n";
      flush stderr
    end

let eval_bool_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
      VVbool b -> b
    | VVstring _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "bool value expected")
  with Exc_located (loc, exc) -> print_error conf loc exc; false

let eval_string_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
      VVstring s -> Util.translate_eval s
    | VVbool _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "string value expected")
  with Exc_located (loc, exc) -> print_error conf loc exc; ""

let print_body_prop conf =
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\"" with
      Not_found -> ""
  in
  Output.print_string conf (s ^ Util.body_prop conf)

type 'a vother =
    Vdef of string list * ast list
  | Vval of 'a expr_val
  | Vbind of string * string
type 'a env = (string * 'a) list

type ('a, 'b) interp_fun =
  { eval_var : 'a env -> 'b -> loc -> string list -> 'b expr_val;
    eval_transl : 'a env -> bool -> string -> string -> string;
    eval_predefined_apply : 'a env -> string -> 'b expr_val list -> string;
    get_vother : 'a -> 'b vother option;
    set_vother : 'b vother -> 'a;
    print_foreach :
      ('a env -> 'b -> ast -> unit) -> ('a env -> 'b -> ast -> string) ->
        'a env -> 'b -> loc -> string -> string list -> ast list list ->
        ast list -> unit }

let get_def get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
      Some (Vdef (al, el)) -> Some (al, el)
    | _ -> None
  with Not_found -> None
let get_val get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
      Some (Vval x) -> Some x
    | _ -> None
  with Not_found -> None
let set_def set_vother k al el env =
  let k = "#" ^ k in (k, set_vother (Vdef (al, el))) :: env
let set_val set_vother k v env =
  let k = "#" ^ k in (k, set_vother (Vval v)) :: env

let eval_subst loc f set_vother env xl vl a =
  let rec loop env a xl vl =
    match xl, vl with
      x :: xl, VVstring v :: vl -> loop env (subst (subst_text x v) a) xl vl
    | x :: xl, v :: vl ->
        let env = set_val set_vother x v env in loop env a xl vl
    | [], [] -> env, a
    | _ -> env, Atext (loc, f ^ ": bad # of params")
  in
  loop env a xl vl

let squeeze_spaces s =
  let rec loop i =
    if i = String.length s then ""
    else
      match s.[i] with
        ' ' | '\n' | '\r' | '\t' -> loop (i + 1)
      | _ -> String.sub s i (String.length s - i)
  in
  loop 0

let print_apply loc f set_vother print_ast env ep gxl al gvl =
  let local_print_ast a =
    let (env, a) =
      let rec loop env a xl vl =
        match xl, vl with
          x :: xl, VVstring v :: vl ->
            loop env (subst (subst_text x v) a) xl vl
        | x :: xl, v :: vl -> loop (set_val set_vother x v env) a xl vl
        | [], [] -> env, a
        | _ ->
            env,
            Atext
              (loc,
               Printf.sprintf "%s: bad # of params (%d instead of %d)" f
                 (List.length gvl) (List.length gxl))
      in
      loop env a gxl gvl
    in
    print_ast env ep a
  in
  let rec loop =
    function
      [] -> ()
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in loop (Atext (loc, s) :: al)
    | a :: al -> local_print_ast a; loop al
  in
  loop al

let templ_eval_expr = eval_string_expr
let templ_print_apply = print_apply

let rec templ_print_foreach conf print_ast set_vother env ep _loc s sl _el al =
  match s :: sl with
    ["env_binding"] ->
      print_foreach_env_binding conf print_ast set_vother env ep al
  | _ -> raise Not_found
and print_foreach_env_binding conf print_ast set_vother env ep al =
  List.iter
    (fun (k, v) ->
       let print_ast =
         print_ast (("binding", set_vother (Vbind (k, v))) :: env) ep
       in
       List.iter print_ast al)
    conf.env

let float_rgb_of_hsv h s v =
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
    0 -> v, t, p
  | 1 -> q, v, p
  | 2 -> p, v, t
  | 3 -> p, q, v
  | 4 -> t, p, v
  | 5 -> v, p, q
  | _ -> assert false

let rgb_of_hsv h s v =
  let (r, g, b) =
    float_rgb_of_hsv (float h /. 256.) (float s /. 256.) (float v /. 256.)
  in
  truncate (r *. 256.), truncate (g *. 256.), truncate (b *. 256.)

let rgb_of_str_hsv h s v =
  let ios s = int_of_string s in rgb_of_hsv (ios h) (ios s) (ios v)

let eval_var conf ifun env ep loc sl =
  try
    match sl with
      ["env"; "key"] ->
        begin match ifun.get_vother (List.assoc "binding" env) with
          Some (Vbind (k, _)) -> VVstring k
        | _ -> raise Not_found
        end
    | ["env"; "val"] ->
        begin match ifun.get_vother (List.assoc "binding" env) with
          Some (Vbind (_, v)) -> VVstring v
        | _ -> raise Not_found
        end
    | ["env"; "val"; "decoded"] ->
        begin match ifun.get_vother (List.assoc "binding" env) with
          Some (Vbind (_, v)) -> VVstring (Mutil.decode v)
        | _ -> raise Not_found
        end
    | "today" :: sl ->
        Geneweb.TemplDate.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
    | s :: sl ->
        begin match get_val ifun.get_vother s env, sl with
          Some (VVother f), sl -> f sl
        | Some v, [] -> v
        | _, sl -> ifun.eval_var env ep loc (s :: sl)
        end
    | _ -> ifun.eval_var env ep loc sl
  with Not_found -> VVstring (eval_variable conf sl)

let print_foreach conf ifun print_ast eval_expr env ep loc s sl el al =
  try ifun.print_foreach print_ast eval_expr env ep loc s sl el al with
    Not_found ->
      templ_print_foreach conf print_ast ifun.set_vother env ep loc s sl el al

let print_wid_hei conf fname =
  match Util.image_size (Util.image_file_name fname) with
    Some (wid, hei) -> Output.printf conf " width=\"%d\" height=\"%d\"" wid hei
  | None -> ()

let print_copyright conf =
  Util.include_template conf [] "copyr"
    (fun () ->
      Output.print_string conf "<hr style=\"margin:0\">\n";
      Output.print_string conf "<div style=\"font-size: 80%\">\n";
      Output.print_string conf "<em>";
      Output.print_string conf "Copyright (c) 1998-2007 INRIA - GeneWeb " ;
      Output.print_string conf Version.txt;
      Output.print_string conf "</em>";
      Output.print_string conf "</div>\n";
      Output.print_string conf "<br>\n")

let print_copyright_with_logo conf =
  let conf =
    match List.assoc_opt "with_logo" conf.env with
    | Some "yes" -> conf
    | Some v -> { conf with env = Mutil.list_replace ("with_logo", v) ("with_logo", "yes") conf.env }
    | None -> { conf with env = ("with_logo", "yes") :: conf.env }
  in
  print_copyright conf

let include_hed_trl conf name =
  Util.include_template conf [] name  (fun () -> ())

let rec interp_ast conf ifun env =
  let m_env = ref env in
  let rec eval_ast env ep a = string_of_expr_val (eval_ast_expr env ep a)
  and eval_ast_list env ep =
    function
      [] -> []
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in
        eval_ast_list env ep (Atext (loc, s) :: al)
    | a :: al -> eval_ast env ep a :: eval_ast_list env ep al
  and eval_ast_expr env ep =
    function
      Atext (_, s) -> VVstring s
    | Avar (loc, s, sl) ->
        eval_string_var conf (eval_var conf ifun env ep loc) (s :: sl)
    | Atransl (_, upp, s, n) -> VVstring (ifun.eval_transl env upp s n)
    | Aif (e, alt, ale) -> VVstring (eval_if env ep e alt ale)
    | Aapply (loc, f, all) ->
        let vl = List.map (eval_ast_expr_list env ep) all in
        VVstring (eval_apply env ep loc f vl)
    | Afor (i, min, max, al) -> VVstring (eval_for env ep i min max al)
    | x -> VVstring (eval_expr env ep x)
  and eval_ast_expr_list env ep v =
    let rec loop =
      function
        [] -> []
      | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
          let s = squeeze_spaces s in loop (Atext (loc, s) :: al)
      | a :: al -> eval_ast_expr env ep a :: loop al
    in
    match loop v with
      [e] -> e
    | el ->
        let sl = List.map string_of_expr_val el in
        VVstring (String.concat "" sl)
  and eval_expr env ep e =
    let eval_apply = eval_apply env ep in
    let eval_var = eval_var conf ifun env ep in
    templ_eval_expr conf (eval_var, eval_apply) e
  and eval_apply env ep loc f vl =
    match get_def ifun.get_vother f env with
      Some (xl, al) ->
        let (env, al) =
          List.fold_right
            (fun a (env, al) ->
               let (env, a) = eval_subst loc f ifun.set_vother env xl vl a in
               env, a :: al)
            al (env, [])
        in
        let sl = List.map (eval_ast env ep) al in String.concat "" sl
    | None ->
        match f, vl with
          "capitalize", [VVstring s] -> Utf8.capitalize_fst s
        | "interp", [VVstring s] ->
            let astl = parse_templ conf (Stream.of_string s) in
            String.concat "" (eval_ast_list env ep astl)
        | "language_name", [VVstring s] ->
            Translate.language_name s (Util.transl conf "!languages")
        | "nth", [VVstring s1; VVstring s2] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            Util.translate_eval (Util.nth_field s1 n)
        | "nth_c", [VVstring s1; VVstring s2] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            (try Char.escaped (String.get s1 n) with Invalid_argument _ -> "")
        | "red_of_hsv", [VVstring h; VVstring s; VVstring v] ->
            begin try
              let (r, _, _) = rgb_of_str_hsv h s v in string_of_int r
            with Failure _ -> "red_of_hsv bad params"
            end
        | "green_of_hsv", [VVstring h; VVstring s; VVstring v] ->
            begin try
              let (_, g, _) = rgb_of_str_hsv h s v in string_of_int g
            with Failure _ -> "green_of_hsv bad params"
            end
        | "blue_of_hsv", [VVstring h; VVstring s; VVstring v] ->
            begin try
              let (_, _, b) = rgb_of_str_hsv h s v in string_of_int b
            with Failure _ -> "blue_of_hsv bad params"
            end
        | _ ->
            try ifun.eval_predefined_apply env f vl with
              Not_found -> Printf.sprintf "%%apply;%s?" f
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
        let instr = String.concat "" (List.map eval_ast al) in
        let accu = accu ^ instr in
        loop new_env (Aop2 ((0, 0), "+", min, Aint ((0, 0), "1"))) max accu
      else accu
    in
    loop env min max ""
  in
  let rec print_ast env ep =
    function
      Avar (loc, s, sl) ->
        print_var print_ast_list conf ifun env ep loc (s :: sl)
    | Awid_hei s -> print_wid_hei conf s
    | Aif (e, alt, ale) -> print_if env ep e alt ale
    | Aforeach ((loc, s, sl), el, al) ->
        begin try
          print_foreach conf ifun print_ast eval_expr env ep loc s sl el al
        with Not_found ->
          Output.printf conf " %%foreach;%s?" (String.concat "." (s :: sl))
        end
    | Adefine (f, xl, al, alk) -> print_define env ep f xl al alk
    | Aapply (loc, f, ell) -> print_apply env ep loc f ell
    | Alet (k, v, al) -> print_let env ep k v al
    | Afor (i, min, max, al) -> print_for env ep i min max al
    | x -> Output.print_string conf (eval_ast env ep x)
  and print_ast_list env ep =
    function
      [] -> m_env := env
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in
        print_ast_list env ep (Atext (loc, s) :: al)
    | Ainclude (_, astl) :: al ->
        print_ast_list env ep astl; print_ast_list !m_env ep al
    | [a] -> print_ast env ep a
    | a :: al -> print_ast env ep a; print_ast_list env ep al
  and print_define env ep f xl al alk =
    let env = set_def ifun.set_vother f xl al env in print_ast_list env ep alk
  and print_apply env ep loc f ell =
    let vl = List.map (eval_ast_expr_list env ep) ell in
    match get_def ifun.get_vother f env with
      Some (xl, al) ->
        templ_print_apply loc f ifun.set_vother print_ast env ep xl al vl
    | None -> Output.print_string conf (eval_apply env ep loc f vl)
  and print_let env ep k v al =
    let v = eval_ast_expr_list env ep v in
    let env = set_val ifun.set_vother k v env in print_ast_list env ep al
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
        loop new_env (Aop2 ((0, 0), "+", min, Aint ((0, 0), "1"))) max
    in
    loop env min max
  in
  print_ast_list env
and print_var print_ast_list conf ifun env ep loc sl =
  let rec print_var1 eval_var sl =
    try
      match eval_var sl with
        VVstring s -> Output.print_string conf s
      | VVbool true -> Output.print_string conf "1"
      | VVbool false -> Output.print_string conf "0"
      | VVother f -> print_var1 f []
    with Not_found ->
      match sl with
        ["include"; templ] ->
          begin match Util.open_templ_fname conf templ with
            Some (_, fname) ->
              begin match input_templ conf templ with
                Some astl ->
                    let () = included_files := (templ, astl) :: !included_files in
                    print_ast_list env ep (begin_end_include conf fname astl)
              | None -> Output.printf conf " %%%s?" (String.concat "." sl)
              end
            | None -> Output.printf conf " %%%s?" (String.concat "." sl)
          end
      | sl -> print_variable conf sl
  in
  let eval_var = eval_var conf ifun env ep loc in print_var1 eval_var sl
and print_simple_variable conf =
  function
    "base_header" -> include_hed_trl conf "hed"
  | "base_trailer" -> include_hed_trl conf "trl"
  | "body_prop" -> print_body_prop conf
  | "copyright" -> print_copyright_with_logo conf
  | "copyright_nologo" -> print_copyright conf
  | "hidden" -> Util.hidden_env conf
  | "message_to_wizard" -> Util.message_to_wizard conf
  | _ -> raise Not_found
and print_variable conf sl =
  try Output.print_string conf (eval_variable conf sl) with
    Not_found ->
      try
        match sl with
          [s] -> print_simple_variable conf s
        | _ -> raise Not_found
      with Not_found -> Output.printf conf " %%%s?" (String.concat "." sl)

let copy_from_templ conf env ic =
  let astl = parse_templ conf (Stream.of_channel ic) in
  close_in ic;
  let ifun =
    {eval_var =
      (fun env _ _ -> function
        | [s] -> VVstring (List.assoc s env)
        | _ -> raise Not_found);
     eval_transl = (fun _ -> eval_transl conf);
     eval_predefined_apply = (fun _ -> raise Not_found);
     get_vother = (fun _ -> None); set_vother = (fun _ -> "");
     print_foreach = fun _ -> raise Not_found}
  in
  let v = !template_file in
  template_file := "";
  begin try interp_ast conf ifun env () astl with
    e -> template_file := v; raise e
  end;
  template_file := v

let _ = Util.copy_from_templ_ref := copy_from_templ
