{

open Config
open TemplAst

let dump_list pp a = String.concat ";" (List.map pp a)
let rec dump_ast trk depth =
  let dump_ast a = if depth > 0 then dump_ast trk (depth - 1) a else "..." in
  let dump_ast_list a = if depth > 0 then dump_list dump_ast a else "..." in
  let dump_ast_list_list a = if depth > 0 then dump_list dump_ast_list a else "..." in
  let trk s = if String.length s > trk then String.sub s 0 trk ^ "..." else s in
  function
  | Atext (_, a) ->
    "(Atext " ^ trk a ^ ")"
  | Avar (_, a, b) ->
    "(Avar " ^ trk (String.concat "." (a :: b)) ^ ")"
  | Atransl (_, a, b, c) ->
    "(Atransl " ^ (if a then "T," else "F,") ^ trk b ^ "," ^ trk c ^ ")"
  | Aconcat (_, a) ->
    "(Aconcat " ^ dump_ast_list a ^ ")"
  | Awid_hei a ->
    "(Awid_hei " ^ trk a ^ ")"
  | Aif (a, b, c) ->
    "(Aif " ^ dump_ast a ^ "," ^ dump_ast_list b ^ "," ^ dump_ast_list c ^ ")"
  | Aforeach ((_, a, b), c, d) ->
    "(Aif " ^ trk a ^ "," ^ trk (String.concat "." b) ^ "," ^ dump_ast_list_list c ^ "," ^ dump_ast_list d ^ ")"
  | Afor (a, b, c, d) ->
    "(Afor " ^ trk a ^ "," ^ dump_ast b ^ "," ^ dump_ast c ^ "," ^ dump_ast_list d ^ ")"
  | Adefine (a, b, c, d) ->
    "(Adefine " ^ trk a ^ "," ^ String.concat "." b ^ "," ^ dump_ast_list c ^ "," ^ dump_ast_list d ^ ")"
  | Aapply (_, a, b) ->
    "(Aapply " ^ trk a ^ "," ^ dump_ast_list_list b ^ ")"
  | Alet (a, b, c) ->
    "(Alet " ^ trk a ^ "," ^ dump_ast_list b ^ "," ^ dump_ast_list c ^ ")"
  | Aop1 (_, a, b) ->
    "(Aop1 " ^ trk a ^ "," ^ dump_ast b ^ ")"
  | Aop2 (_, a, b, c) ->
    "(Aop2 " ^ trk a ^ "," ^ dump_ast b ^ "," ^ dump_ast c ^ ")"
  | Aint (_, a) ->
    "(Aint " ^ a ^ ")"
  | Ainclude (a, b) ->
    "(Ainclude " ^ trk a ^ "," ^ dump_ast_list b ^ ")"

let dump_ast ?(truncate=max_int) ?(depth=max_int) a = dump_ast truncate depth a

let dummy_pos = (-1, -1)

let pos lex = Lexing.lexeme_start lex, Lexing.lexeme_end lex

let included_files = ref []

let flush ast b lexbuf =
  let s = Buffer.contents b in
  let ast =
    if s = "" then ast
    else Atext ((lexbuf.Lexing.lex_curr_pos - String.length s, lexbuf.Lexing.lex_curr_pos), s) :: ast
  in
  let () = Buffer.reset b in
  ast

let strip_newlines_after_variables =
  let rec loop =
    function
      TemplAst.Atext (loc, s) :: astl ->
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
      TemplAst.Atext (loc, s) :: loop astl
    | TemplAst.Aif (s, alt, ale) :: astl -> TemplAst.Aif (s, loop alt, loop ale) :: loop astl
    | TemplAst.Aforeach (v, pl, al) :: astl -> TemplAst.Aforeach (v, pl, loop al) :: loop astl
    | TemplAst.Adefine (f, x, al, alk) :: astl ->
      TemplAst.Adefine (f, x, loop al, loop alk) :: loop astl
    | TemplAst.Aapply (loc, f, all) :: astl ->
      TemplAst.Aapply (loc, f, List.map loop all) :: loop astl
    | TemplAst.Alet (k, v, al) :: astl -> TemplAst.Alet (k, loop v, loop al) :: loop astl
    | TemplAst.Afor (i, min, max, al) :: astl ->
      TemplAst.Afor (i, min, max, loop al) :: loop astl
    | (TemplAst.Atransl (_, _, _, _) | TemplAst.Awid_hei _ as ast1) :: (TemplAst.Atext (_, _) as ast2) ::
      astl ->
      ast1 :: ast2 :: loop astl
    | TemplAst.Ainclude (file, al) :: astl -> TemplAst.Ainclude (file, loop al) :: loop astl
    | ast :: astl -> ast :: loop astl
    | [] -> []
  in
  loop

}

let ws = ([ ' ' '\n' '\t' '\r' ])

let r_ident = ([ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]+)

let num = (['0'-'9']+)

let var = (r_ident ('.' (r_ident|num))*)

let value = ([^ ' ' '>' ';' '\n' '\r'  '\t' ]+)

rule parse_ast conf b closing ast = parse

  | '%' {
      match match variable lexbuf with `variable [] -> `escaped '%' | x -> x with
      | `escaped c -> Buffer.add_char b c ; parse_ast conf b closing ast lexbuf
      | `comment -> parse_ast conf b closing ast lexbuf
      | x ->
        let pos = pos lexbuf in
        let ast = flush ast b lexbuf in
        match x with
        | `variable [v] when List.mem v closing -> List.rev ast, v
        | `variable ["nn"] -> let () = skip_ws lexbuf in parse_ast conf b closing ast lexbuf
        | `variable ["define"] -> parse_define conf b closing ast lexbuf
        | `variable ["let"] -> parse_let conf b closing ast lexbuf
        | `variable ["include"] -> parse_include conf b closing ast lexbuf
        | x ->
          let a = match x with
            | `variable ["if"] -> parse_if conf b lexbuf
            | `variable ["foreach"] -> parse_foreach conf b lexbuf
            | `variable ["apply"] -> parse_apply conf b lexbuf
            | `variable ["expr"] -> parse_expr_stmt lexbuf
            | `variable ["for"] -> parse_for conf b lexbuf
            | `variable ["wid_hei"] -> Awid_hei (value lexbuf)
            | `variable (hd :: tl) -> Avar (pos, hd, tl)
            | _ -> assert false
          in
          parse_ast conf b closing (a :: ast) lexbuf
    }

  | '[' {
      let ast = flush ast b lexbuf in
      let a =
        let pos = pos lexbuf in
        let (upp, s, n) = lexicon_word lexbuf in
        if String.length s > 1 && (s.[0] = '[' || s.[0] = '@')
        then
          let (ast, _) = parse_ast conf b [] [] (Lexing.from_string s) in
          Aconcat (pos, ast)
        else
          Atransl (pos, upp, s, n)
      in
      parse_ast conf b closing (a :: ast) lexbuf
    }

  | '\n' [ ' ' '\n' '\t' '\r' ]* {
      let () = Buffer.add_char b '\n' in
      parse_ast conf b closing ast lexbuf
    }

  | _ as c {
      let () = Buffer.add_char b c in
      parse_ast conf b closing ast lexbuf
    }

  | eof {
      List.rev (flush ast b lexbuf), ""
    }

and parse_ident_list = parse
  | '.' ((r_ident|num) as id) {
      id :: parse_ident_list lexbuf
    }
  | "" {
      []
    }

and parse_expr = parse
  | "" {
      parse_expr_if lexbuf
    }

and parse_expr_if = parse
  | ws* "if" {
      let e1 = parse_expr_or lexbuf in
      parse_expr_if_1 e1 lexbuf
    }
  | "" {
      parse_expr_or lexbuf
    }
and parse_expr_if_1 e1 = parse
  | ws* "then" {
      let e2 = parse_expr_or lexbuf in
      parse_expr_if_2 e1 e2 lexbuf
    }
and parse_expr_if_2 e1 e2 = parse
  | ws* "else" {
      let e3 = parse_expr_or lexbuf in
      Aif (e1, [e2], [e3])
    }

and parse_expr_or = parse
  | ws* {
      let e1 = parse_expr_and lexbuf in
      parse_expr_or_1 e1 lexbuf
    }
and parse_expr_or_1 e1 = parse
  | ws* "or" {
      let pos = pos lexbuf in
      let e2 = parse_expr_or lexbuf in
      Aop2 (pos, "or", e1, e2)
    }
  | ws* {
      e1
    }

and parse_expr_and = parse
  | ws* {
      let e1 = parse_expr_is_substr lexbuf in
      parse_expr_and_1 e1 lexbuf
    }
and parse_expr_and_1 e1 = parse
  | ws* "and" {
      let pos = pos lexbuf in
      let e2 = parse_expr_and lexbuf in
      Aop2 (pos, "and", e1, e2)
    }
  | ws* {
      e1
    }

and parse_expr_is_substr = parse
  | ws* {
      let e1 = parse_expr_in lexbuf in
      parse_expr_is_substr_1 e1 lexbuf
    }
and parse_expr_is_substr_1 e1 = parse
  | ws* "is_substr" {
      let pos = pos lexbuf in
      let e2 = parse_expr_is_substr lexbuf in
      Aop2 (pos, "is_substr", e1, e2)
    }
  | ws* {
      e1
    }

and parse_expr_in = parse
  | ws* {
      let e1 = parse_expr_3 lexbuf in
      parse_expr_in_1 e1 lexbuf
    }
and parse_expr_in_1 e1 = parse
  | ws* "in" {
      let pos = pos lexbuf in
      let e2 = parse_expr_in lexbuf in
      Aop2 (pos, "in", e1, e2)
    }
  | ws* {
      e1
    }

and parse_expr_3 = parse
  | ws* {
      let e1 = parse_expr_4 lexbuf in
      parse_expr_3_1 e1 lexbuf
    }
and parse_expr_3_1 e1 = parse
  | ws* (("="|"!="|">"|">="|"<"|"<=") as op) {
      let pos = pos lexbuf in
      let e2 = parse_expr_4 lexbuf in
      Aop2 (pos, op, e1, e2)
    }
  | ws* {
      e1
    }

and parse_expr_4 = parse
  | ws* {
      let e1 = parse_expr_5 lexbuf in
      parse_expr_4_1 e1 lexbuf
    }
and parse_expr_4_1 e1 = parse
  | ws* (("+"|"-") as op) ws* {
      let pos = pos lexbuf in
      let e2 = parse_expr_5 lexbuf in
      parse_expr_4_1 (Aop2 (pos, String.make 1 op, e1, e2)) lexbuf
    }
  | ws*  { e1 }

and parse_expr_5 = parse
  | ws* {
      let e1 = parse_simple_expr lexbuf in
      parse_expr_5_1 e1 lexbuf
    }

and parse_expr_5_1 e1 = parse
  | ws* (("*"|"^"|"/"|"%") as op) {
      let pos = pos lexbuf in
      let e2 = parse_simple_expr lexbuf in
      parse_expr_5_1 (Aop2 (pos, String.make 1 op, e1, e2)) lexbuf
    }
  | ws* {
      e1
    }

and parse_simple_expr = parse
  | ws* '(' {
      let e = parse_expr lexbuf in
      let () = discard_RPAREN lexbuf in
      e
    }
  | ws* "not" {
      let pos = pos lexbuf in
      let e = parse_simple_expr lexbuf in
      Aop1 (pos, "not", e)
    }
  | ws* '"' ([^'"']* as s) '"' {
      Atext (pos lexbuf, s)
    }
  | ws* (num as s) {
      Aint (pos lexbuf, s)
    }
  | ws* '[' {
      let pos = pos lexbuf in
      let u, w, n = lexicon_word lexbuf in
      Atransl (pos, u, w, n)
    }
  | ws* (var as id) {
      let pos = pos lexbuf in
      let [@warning "-8"] hd :: tl = String.split_on_char '.' id in
      try
        let t = parse_tuple lexbuf in
        Aapply (pos, hd, t)
      with _ -> Avar (pos, hd, tl)
    }

and parse_simple_expr_1 = parse
  | ws* {
      let e = parse_expr lexbuf in
      let () = discard_RPAREN lexbuf in
      e
    }

and discard_RPAREN = parse
  | ws* ')' {
      ()
    }

and parse_tuple = parse
  | ws* '(' {
      parse_tuple_1 lexbuf
    }
and parse_tuple_1 = parse
  | ws* ')' {
      []
    }
  | ws* {
      let r = parse_expr_list lexbuf in
      let () = discard_RPAREN lexbuf in
      r
    }
and parse_expr_list = parse
  | ws* {
      let x = parse_expr lexbuf in
      parse_expr_list_1 x lexbuf
    }
and parse_expr_list_1 x = parse
  | ws* ',' {
      let tl = parse_expr_list lexbuf in
      [x] :: tl
    }
  | ws* {
      [[x]]
    }

and parse_char_stream_semi fn fn2 = parse
  | '(' {
      fn2 lexbuf
    }
  | ws* {
      let r = fn lexbuf in
      let () = discard_opt_semi lexbuf in
      r
    }

and discard_opt_semi = parse
  | ws* ';'? {
      ()
    }

and lexicon_word = parse
  | ('*'? as upper) {
      let b = Buffer.create 255 in
      let w = lexicon_word_text b 0 lexbuf in
      let n = lexicon_index lexbuf in
      (upper <> "", w, n)
    }

and lexicon_index = parse
  | num as s {
      s
    }
  | ['a'-'z'] as c {
      String.make 1 c
    }
  | "" {
      ""
    }

and value = parse
  | value as v {
      v
    }

and compound_var = parse
  | (r_ident as i) '.' {
      i :: compound_var lexbuf
    }
  | (r_ident as i) {
      [ i ]
    }

(* '%' already consummed *)
and variable = parse
  | [ '%' '/' '[' ']' ] as c {
      `escaped c
    }
  | '(' {
      let () = comment lexbuf in
      `comment
    }
  | (r_ident as i) {
      `variable (i :: variable_ident lexbuf)
    }
  | "" {
      `variable []
    }

and variable_ident = parse
  | '.' (r_ident as i) {
      i :: variable_ident lexbuf
    }
  | ';' {
      []
    }
  | "" {
      []
    }

and transl_index = parse
  | num as n {
      n
    }
  | ['a'-'z'] as c {
      String.make 1 c
    }
  | "" {
      ""
    }

and lexicon_word_text b n = parse
  | '[' {
      let () = Buffer.add_char b '[' in
      lexicon_word_text b (n + 1) lexbuf
    }
  | ']' {
      if n = 0 then
        let r = Buffer.contents b in
        let () = Buffer.reset b in
        r
      else
        let () = Buffer.add_char b ']' in
        lexicon_word_text b (n - 1) lexbuf
    }
  | _ as c {
      let () = Buffer.add_char b c in
      lexicon_word_text b n lexbuf
    }

(* "%(" already consummed *)
and comment = parse
  | "%(" {
      let () = comment lexbuf in
      comment lexbuf
    }
  | "%)" ws* {
      ()
    }
  | _ {
      comment lexbuf
    }

and skip_ws = parse
  | ws* {
      ()
    }

and parse_define conf b closing ast = parse
  | ws* (r_ident as f) ws* '(' {
      let args = parse_params lexbuf in
      let (al, _) = parse_ast conf b ["end"] [] lexbuf in
      let (alk, tok) = parse_ast conf b closing [] lexbuf in
      ( List.rev (Adefine (f, args, al, alk) :: ast)
      , tok
      )
    }
and parse_params = parse
  | ws* (r_ident as a) {
      a :: parse_params_1 lexbuf
    }
  | ws* ')' ws* {
      []
    }
and parse_params_1 = parse
  | ws* ',' ws* (r_ident as a) {
      a :: parse_params_1 lexbuf
    }
  | ws* ')' ws* {
      []
    }

and parse_let conf b closing ast = parse
  |  ws* (r_ident as k) ';' ws* {
      let (v, _) = parse_ast conf b ["in"] [] lexbuf in
      let (al, tok) = parse_ast conf b closing [] lexbuf in
      ( List.rev (Alet (k, v, al) :: ast)
      , tok
      )
    }

and parse_include conf b closing ast = parse
  | value as file {
      let a =
        match List.assoc_opt file !included_files with
        | Some ast -> Ainclude (file, ast)
        | None ->
          match Util.open_templ_fname conf file with
          | Some (ic, fname) ->
            let lex2 = Lexing.from_channel ic in
            let (ast, _) = parse_ast conf (Buffer.create 1024) [] [] lex2 in
            let () = close_in ic in
            let () = included_files := (file, ast) :: !included_files in
            Ainclude (file, ast)
          | None -> assert false
      in
      parse_ast conf b closing (a :: ast) lexbuf
    }

and parse_apply conf b = parse
  | (r_ident as f) '%' {
      let pos = pos lexbuf in
      assert (`variable ["with"] = variable lexbuf) ;
      let app =
        let rec loop () =
          match parse_ast conf b ["and"; "end"] [] lexbuf with
          | a, "and" -> a :: loop ()
          | a, _ -> [ a ]
        in loop ()
      in
      Aapply (pos, f, app)
    }
  | (r_ident as f) {
      let pos = pos lexbuf in
      let app = parse_tuple lexbuf in
      Aapply (pos, f, app)
    }

and parse_expr_stmt = parse
  | "" {
      parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf
    }

and parse_if conf b = parse
  | "" {
      let e = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let (a1, a2) =
        let rec loop () =
          let (a1, t) = parse_ast conf b ["elseif"; "else"; "end"] [] lexbuf in
          match t with
          | "elseif" ->
            let e2 = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
            let (a2, a3) = loop () in
            a1, [ Aif (e2, a2, a3) ]
          | "else" ->
            let (a2, _) = parse_ast conf b ["end"] [] lexbuf in
            a1, a2
          | _ -> a1, []
        in loop ()
      in
      Aif (e, a1, a2)
    }

and parse_for conf b = parse
  | (r_ident as iterator) ';' {
      let min = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let max = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let (a, _) = parse_ast conf b ["end"] [] lexbuf in
      Afor (iterator, min, max, a)
    }

and parse_foreach conf b = parse
  | "" {
      let pos = pos lexbuf in
      let [@warning "-8"] hd :: tl = compound_var lexbuf in
      let params = parse_foreach_params lexbuf in
      let (a, _) = parse_ast conf b ["end"] [] lexbuf in
      Aforeach ((pos, hd, tl), params, a)
    }

and parse_foreach_params = parse
  | '(' {
      parse_tuple_1 lexbuf
    }
  | ';'? {
      []
    }

{

  let parse_templ conf lexbuf =
    parse_ast conf (Buffer.create 1024) [] [] lexbuf
    |> fst
    |> strip_newlines_after_variables

}
