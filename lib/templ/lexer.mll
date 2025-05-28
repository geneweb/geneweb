{

(* let pos lex = !current_file, Lexing.lexeme_start lex, Lexing.lexeme_end lex *)

(* Leading ([' ' '\t' '\r']* '\n') will be removed, except if
   the previous node is a Atransl. *)
let flush ast b _lexbuf =
  let s = Buffer.contents b in
  let trim s =
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
  let s = match ast with
    | Ast.{ desc = Atransl _; _ } :: _ | { desc = Awid_hei _; _ } :: _ -> s
    | _ -> trim s
  in
  let ast =
    if s = "" then ast
    else Ast.mk_text s :: ast
  in
  let () = Buffer.reset b in
  ast

}

let ws = ([ ' ' '\n' '\t' '\r' ])

let r_ident = ([ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]+)

let num = (['0'-'9']+)

let var = (r_ident ('.' (r_ident|num))*)

let value = ([^ ' ' '>' ';' '\n' '\r'  '\t' ]+)

rule parse_ast b closing ast = parse

  (* Special variable: strip whitespaces coming after this. *)
  | "%sq;" ws* {
      parse_ast b closing ast lexbuf
    }

  (* Special variable: strip on newline and its surrounding whitespaces. *)
  | "%nn;" [ ' ' '\t' '\r' ]* '\n'? [ ' ' '\t' '\r' ]* {
      parse_ast b closing ast lexbuf
    }

  | '%' {
      match match variable lexbuf with `variable [] -> `escaped '%' | x -> x with
      | `escaped c -> Buffer.add_char b c ; parse_ast b closing ast lexbuf
      | `comment -> parse_ast b closing ast lexbuf
      | x ->
        let loc = Loc.of_lexbuf lexbuf in
        let ast = flush ast b lexbuf in
        match x with
        | `variable [v] when List.mem v closing -> List.rev ast, v
        | `variable ["define"] -> parse_define b closing ast lexbuf
        | `variable ["let"] -> parse_let b closing ast lexbuf
        | `variable ["include"] -> parse_include b closing ast lexbuf
        | x ->
          let a = match x with
            | `variable ["if"] -> parse_if b lexbuf
            | `variable ["foreach"] -> parse_foreach b lexbuf
            | `variable ["apply"] -> parse_apply b lexbuf
            | `variable ["expr"] -> parse_expr_stmt lexbuf
            | `variable ["for"] -> parse_for b lexbuf
            | `variable ["wid_hei"] -> Ast.mk_wid_hei (value lexbuf)
            | `variable (hd :: tl) -> Ast.mk_var ~loc hd tl
            | `variable [] | `escaped _ | `comment -> assert false
          in
          parse_ast b closing (a :: ast) lexbuf
    }

  | '[' {
      let ast = flush ast b lexbuf in
      let a =
        let loc = Loc.of_lexbuf lexbuf in
        let (upp, s, n) = lexicon_word lexbuf in
        if String.length s > 1 && (s.[0] = '[' || s.[0] = '@') then
            Ast.mk_include ~loc (`Raw s)
        else
          Ast.mk_transl ~loc upp s n
      in
      parse_ast b closing (a :: ast) lexbuf
    }

  | '\n' [ ' ' '\n' '\t' '\r' ]* {
      let () = Buffer.add_char b '\n' in
      parse_ast b closing ast lexbuf
    }

  | _ as c {
      let () = Buffer.add_char b c in
      parse_ast b closing ast lexbuf
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
      Ast.mk_if e1 [e2] [e3]
    }

and parse_expr_or = parse
  | ws* {
      let e1 = parse_expr_and lexbuf in
      parse_expr_or_1 e1 lexbuf
    }
and parse_expr_or_1 e1 = parse
  | ws* "or" {
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_or lexbuf in
      Ast.mk_op2 ~loc "or" e1 e2
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
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_and lexbuf in
      Ast.mk_op2 ~loc "and" e1 e2
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
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_is_substr lexbuf in
      Ast.mk_op2 ~loc "is_substr" e1 e2
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
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_in lexbuf in
      Ast.mk_op2 ~loc "in" e1 e2
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
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_4 lexbuf in
      Ast.mk_op2 ~loc op e1 e2
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
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_expr_5 lexbuf in
      let a = Ast.mk_op2 ~loc (String.make 1 op) e1 e2 in
      parse_expr_4_1 a lexbuf
    }
  | ws*  { e1 }

and parse_expr_5 = parse
  | ws* {
      let e1 = parse_simple_expr lexbuf in
      parse_expr_5_1 e1 lexbuf
    }

and parse_expr_5_1 e1 = parse
  | ws* (("*" | "^" | "/" | "|" | "%" |"/.") as op) {
      let loc = Loc.of_lexbuf lexbuf in
      let e2 = parse_simple_expr lexbuf in
      let a = Ast.mk_op2 ~loc op e1 e2 in
      parse_expr_5_1 a lexbuf
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
      let loc = Loc.of_lexbuf lexbuf in
      let e = parse_simple_expr lexbuf in
      Ast.mk_op1 ~loc "not" e
    }
  | ws* '"' ([^'"']* as s) '"' {
      let loc = Loc.of_lexbuf lexbuf in
      Ast.mk_text ~loc s
    }
  | ws* (num as s) {
      let loc = Loc.of_lexbuf lexbuf in
      Ast.mk_int ~loc s
    }
  | ws* '[' {
      let loc = Loc.of_lexbuf lexbuf in
      let u, w, n = lexicon_word lexbuf in
      Ast.mk_transl ~loc u w n
    }
  | ws* (var as id) {
      let loc = Loc.of_lexbuf lexbuf in
      match String.split_on_char '.' id with
      | hd :: tl -> (
        (* FIXME: This hack introduces backtracking in the parser. We should
           parse our syntax without backtracking at all. *)
        try
          let l = List.map (fun v -> None, v) (parse_tuple lexbuf) in
          Ast.mk_apply ~loc hd l
        with _ -> Ast.mk_var ~loc hd tl)
      | [] -> assert false
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

and parse_apply_tuple = parse
  | ws* '(' ws* ')' {
      []
    }
  | ws* '(' {
     parse_apply_tuple_1 lexbuf
  }
and parse_apply_tuple_1 = parse
  | ws* (r_ident as id) ws* ':' {
      let e = parse_expr_3 lexbuf in
      (Some id, [e]) :: parse_apply_tuple_2 lexbuf
    }
  | ws* {
     let e = parse_expr_3 lexbuf in
     (None, [e]) :: parse_apply_tuple_2 lexbuf
    }
and parse_apply_tuple_2 = parse
  | ws* ',' {
    parse_apply_tuple_1 lexbuf
    }
  | ws* ')' {
      []
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
      let x = parse_expr_3 lexbuf in
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

and parse_define b closing ast = parse
  | ws* (r_ident as f) ws* '(' {
      let args = parse_params lexbuf in
      let (a, _) = parse_ast b ["end"] [] lexbuf in
      let (k, t) = parse_ast b closing [] lexbuf in
      let u = Ast.mk_define f args a k in
      (List.rev (u :: ast), t)
    }
and parse_params = parse
  | ws* (r_ident as a) ws* '=' ws* {
      let e = parse_simple_expr lexbuf in
      (a, Some e) :: parse_params_1 lexbuf
    }
  | ws* (r_ident as a) {
      (a, None) :: parse_params_1 lexbuf
    }
  | ws* ')' ws* {
      []
    }
and parse_params_1 = parse
  | ws* ',' ws* (r_ident as a) ws* '=' {
      let e = parse_simple_expr lexbuf in
      (a, Some e) :: parse_params_1 lexbuf
    }
  | ws* ',' ws* (r_ident as a) {
      (a, None) :: parse_params_1 lexbuf
    }
  | ws* ')' ws* {
      []
    }

and parse_let b closing ast = parse
  |  ws* (r_ident as k) ';' ws* {
      let (v, _) = parse_ast b ["in"] [] lexbuf in
      let (a, t) = parse_ast b closing [] lexbuf in
      let u = Ast.mk_let k v a in
      (List.rev (u :: ast), t)
    }

and parse_include b closing ast = parse
  | value as file {
    let loc = Loc.of_lexbuf lexbuf in
    let a = Ast.mk_include ~loc (`File file) in
    parse_ast b closing (a :: ast) lexbuf
  }

and parse_apply b = parse
  | (r_ident as f) '%' {
      let loc = Loc.of_lexbuf lexbuf in
      assert (`variable ["with"] = variable lexbuf) ;
      let app =
        let rec loop () =
          match parse_ast b ["and"; "end"] [] lexbuf with
          | a, "and" -> a :: loop ()
          | a, _ -> [ a ]
        in loop ()
      in
      Ast.mk_apply ~loc f (List.map (fun v -> None, v) app)
    }
  | (r_ident as f) {
      let loc = Loc.of_lexbuf lexbuf in
      let app = parse_apply_tuple lexbuf in
      Ast.mk_apply ~loc f app
    }

and parse_expr_stmt = parse
  | "" {
      parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf
    }

and parse_if b = parse
  | "" {
      let e = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let (a1, a2) =
        let rec loop () =
          let (a1, t) = parse_ast b ["elseif"; "else"; "end"] [] lexbuf in
          match t with
          | "elseif" ->
            let e2 = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
            let (a2, a3) = loop () in
            a1, [ Ast.mk_if e2 a2 a3 ]
          | "else" ->
            let (a2, _) = parse_ast b ["end"] [] lexbuf in
            a1, a2
          | _ -> a1, []
        in loop ()
      in
      Ast.mk_if e a1 a2
    }

and parse_for b = parse
  | (r_ident as iterator) ';' {
      let min = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let max = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 lexbuf in
      let (a, _) = parse_ast b ["end"] [] lexbuf in
      Ast.mk_for iterator min max a
    }

and parse_foreach b = parse
  | "" {
      let loc = Loc.of_lexbuf lexbuf in
      let [@warning "-8"] hd :: tl = compound_var lexbuf in
      let params = parse_foreach_params lexbuf in
      let (a, _) = parse_ast b ["end"] [] lexbuf in
      Ast.mk_foreach ~loc (hd, tl) params a
    }

and parse_foreach_params = parse
  | '(' {
      parse_tuple_1 lexbuf
    }
  | ';'? {
      []
    }
