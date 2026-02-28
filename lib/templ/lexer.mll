{

module State : sig
  type t = private {
    src : Loc.source;
    lexbuf : Lexing.lexbuf;
    start : int;
    tokens : Ast.t list;
  }

  val create : src:Loc.source -> Lexing.lexbuf -> t
  val update_loc : t -> t
  val current_loc : t -> Loc.t
  val push_token : Ast.t -> t -> t
  val tokens : t -> Ast.t list
end = struct
  type t = {
    src : Loc.source;
    lexbuf : Lexing.lexbuf;
    start : int;
    tokens : Ast.t list;
  }

  let[@inline] offset lexbuf =
    lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_start_pos

  let create ~src lexbuf =
    { src; lexbuf; start = offset lexbuf; tokens = [] }

  let update_loc st =
    { st with start = offset st.lexbuf }

  let current_loc (st : t) =
    let stop = st.lexbuf.Lexing.lex_abs_pos + st.lexbuf.Lexing.lex_curr_pos in
    Loc.of_offsets st.src st.start stop

  let push_token tk st =
    { st with tokens = tk :: st.tokens }

  let tokens st = List.rev st.tokens
end

(* Leading ([' ' '\t' '\r']* '\n') will be removed, except if
   the previous node is a Atransl. *)
let flush b st =
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
  let s = match st.State.tokens with
    | Ast.{ desc = Atransl _; _ } :: _ | { desc = Awid_hei _; _ } :: _ -> s
    | _ -> trim s
  in
  let st =
    if s = "" then st
    else State.push_token (Ast.mk_text s) st
  in
  Buffer.reset b;
  st
}

let ws = ([ ' ' '\n' '\t' '\r' ])

let r_ident = ([ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]+)

let num = (['0'-'9']+)

let var = (r_ident ('.' (r_ident|num))*)

let value = ([^ ' ' '>' ';' '\n' '\r'  '\t' ]+)

rule parse_ast b closing st = parse

  (* Special variable: strip whitespaces coming after this. *)
  | "%sq;" ws* {
      parse_ast b closing st lexbuf
    }

  (* Special variable: strip on newline and its surrounding whitespaces. *)
  | "%nn;" [ ' ' '\t' '\r' ]* '\n'? [ ' ' '\t' '\r' ]* {
      parse_ast b closing st lexbuf
    }

  | '%' {
      let st = State.update_loc st in
      match match variable lexbuf with `variable [] -> `escaped '%' | x -> x with
      | `escaped c ->
          Buffer.add_char b c;
          parse_ast b closing st lexbuf
      | `comment ->
          parse_ast b closing st lexbuf
      | x ->
        let st = flush b st in
        match x with
        | `variable [v] when List.mem v closing ->
            State.tokens st, v
        | `variable ["define"] ->
            parse_define b closing st lexbuf
        | `variable ["let"] ->
            parse_let b closing st lexbuf
        | `variable ["include"] ->
            parse_include b closing st lexbuf
        | x ->
          let a = match x with
            | `variable ["if"] ->
                parse_if b st lexbuf
            | `variable ["foreach"] ->
                parse_foreach b st lexbuf
            | `variable ["apply"] ->
                parse_apply b st lexbuf
            | `variable ["expr"] ->
                parse_expr_stmt st lexbuf
            | `variable ["for"] ->
                parse_for b st lexbuf
            | `variable ["wid_hei"] ->
                Ast.mk_wid_hei ~loc:(State.current_loc st) (value lexbuf)
            | `variable (hd :: tl) ->
                Ast.mk_var ~loc:(State.current_loc st) hd tl
            | `variable [] | `escaped _ | `comment ->
                assert false
          in
          let st = State.push_token a st in
          parse_ast b closing st lexbuf
    }

  | '[' {
      let st = flush b st in
      let a =
        let (upp, s, n) = lexicon_word lexbuf in
        if String.length s > 1 && (s.[0] = '[' || s.[0] = '@') then
            Ast.mk_include (`Raw s)
        else
          Ast.mk_transl upp s n
      in
      let st = State.push_token a st in
      parse_ast b closing st lexbuf
    }

  | '\n' [ ' ' '\n' '\t' '\r' ]* {
      Buffer.add_char b '\n';
      parse_ast b closing st lexbuf
    }

  | _ as c {
      Buffer.add_char b c;
      parse_ast b closing st lexbuf
    }

  | eof {
      let st = flush b st in
      State.tokens st, ""
    }

and parse_ident_list = parse
  | '.' ((r_ident|num) as id) {
      id :: parse_ident_list lexbuf
    }
  | "" {
      []
    }

and parse_expr st = parse
  | "" {
      parse_expr_if st lexbuf
    }

and parse_expr_if st = parse
  | ws* "if" {
      let e1 = parse_expr_or st lexbuf in
      parse_expr_if_1 e1 st lexbuf
    }
  | "" {
      parse_expr_or st lexbuf
    }
and parse_expr_if_1 e1 st = parse
  | ws* "then" {
      let e2 = parse_expr_or st lexbuf in
      parse_expr_if_2 e1 e2 st lexbuf
    }
and parse_expr_if_2 e1 e2 st = parse
  | ws* "else" {
      let e3 = parse_expr_or st lexbuf in
      Ast.mk_if e1 [e2] [e3]
    }

and parse_expr_or st = parse
  | ws* {
      let e1 = parse_expr_and st lexbuf in
      parse_expr_or_1 e1 st lexbuf
    }
and parse_expr_or_1 e1 st = parse
  | ws* "or" {
      let e2 = parse_expr_or st lexbuf in
      Ast.mk_op2 "or" e1 e2
    }
  | ws* {
      e1
    }

and parse_expr_and st = parse
  | ws* {
      let e1 = parse_expr_is_substr st lexbuf in
      parse_expr_and_1 e1 st lexbuf
    }
and parse_expr_and_1 e1 st = parse
  | ws* "and" {
      let e2 = parse_expr_and st lexbuf in
      Ast.mk_op2 "and" e1 e2
    }
  | ws* {
      e1
    }

and parse_expr_is_substr st = parse
  | ws* {
      let e1 = parse_expr_in st lexbuf in
      parse_expr_is_substr_1 e1 st lexbuf
    }
and parse_expr_is_substr_1 e1 st = parse
  | ws* "is_substr" {
      let e2 = parse_expr_is_substr st lexbuf in
      Ast.mk_op2 "is_substr" e1 e2
    }
  | ws* {
      e1
    }

and parse_expr_in st = parse
  | ws* {
      let e1 = parse_expr_3 st lexbuf in
      parse_expr_in_1 e1 st lexbuf
    }
and parse_expr_in_1 e1 st = parse
  | ws* "in" {
      let e2 = parse_expr_in st lexbuf in
      Ast.mk_op2 "in" e1 e2
    }
  | ws* {
      e1
    }

and parse_expr_3 st = parse
  | ws* {
      let e1 = parse_expr_4 st lexbuf in
      parse_expr_3_1 e1 st lexbuf
    }
and parse_expr_3_1 e1 st = parse
  | ws* (("="|"!="|">"|">="|"<"|"<=") as op) {
      let e2 = parse_expr_4 st lexbuf in
      Ast.mk_op2 op e1 e2
    }
  | ws* {
      e1
    }

and parse_expr_4 st = parse
  | ws* {
      let e1 = parse_expr_5 st lexbuf in
      parse_expr_4_1 e1 st lexbuf
    }
and parse_expr_4_1 e1 st = parse
  | ws* (("+"|"-") as op) ws* {
      let e2 = parse_expr_5 st lexbuf in
      let a = Ast.mk_op2 (String.make 1 op) e1 e2 in
      parse_expr_4_1 a st lexbuf
    }
  | ws*  { e1 }

and parse_expr_5 st = parse
  | ws* {
      let e1 = parse_simple_expr st lexbuf in
      parse_expr_5_1 e1 st lexbuf
    }

and parse_expr_5_1 e1 st = parse
  | ws* (("*" | "^" | "/" | "|" | "%" |"/.") as op) {
      let e2 = parse_simple_expr st lexbuf in
      let a = Ast.mk_op2 op e1 e2 in
      parse_expr_5_1 a st lexbuf
    }
  | ws* {
      e1
    }

and parse_simple_expr st = parse
  | ws* '(' {
      let e = parse_expr st lexbuf in
      discard_RPAREN lexbuf;
      e
    }
  | ws* "not" {
      let e = parse_simple_expr st lexbuf in
      Ast.mk_op1 "not" e
    }
  | ws* '"' ([^'"']* as s) '"' {
      Ast.mk_text s
    }
  | ws* (num as s) {
      Ast.mk_int s
    }
  | ws* '[' {
      let st = State.update_loc st in
      let u, w, n = lexicon_word lexbuf in
      Ast.mk_transl ~loc:(State.current_loc st) u w n
    }
  | ws* (var as id) {
      let st = State.update_loc st in
      match String.split_on_char '.' id with
      | hd :: tl -> (
        (* FIXME: This hack introduces backtracking in the parser. We should
           parse our syntax without backtracking at all. *)
        try
          let l = List.map (fun v -> None, v) (parse_tuple st lexbuf) in
          Ast.mk_apply ~loc:(State.current_loc st) hd l
        with _ -> Ast.mk_var ~loc:(State.current_loc st) hd tl)
      | [] -> assert false
    }

and parse_simple_expr_1 st = parse
  | ws* {
      let e = parse_expr st lexbuf in
      discard_RPAREN lexbuf;
      e
    }

and discard_RPAREN = parse
  | ws* ')' {
      ()
    }

and parse_apply_tuple st = parse
  | ws* '(' ws* ')' {
      []
    }
  | ws* '(' {
     parse_apply_tuple_1 st lexbuf
  }
and parse_apply_tuple_1 st = parse
  | ws* (r_ident as id) ws* ':' {
      let e = parse_expr_3 st lexbuf in
      (Some id, [e]) :: parse_apply_tuple_2 st lexbuf
    }
  | ws* {
     let e = parse_expr_3 st lexbuf in
     (None, [e]) :: parse_apply_tuple_2 st lexbuf
    }
and parse_apply_tuple_2 st = parse
  | ws* ',' {
    parse_apply_tuple_1 st lexbuf
    }
  | ws* ')' {
      []
    }

and parse_tuple st = parse
  | ws* '(' {
      parse_tuple_1 st lexbuf
    }
and parse_tuple_1 st = parse
  | ws* ')' {
      []
    }
  | ws* {
      let r = parse_expr_list st lexbuf in
      discard_RPAREN lexbuf;
      r
    }
and parse_expr_list st = parse
  | ws* {
      let x = parse_expr_3 st lexbuf in
      parse_expr_list_1 x st lexbuf
    }
and parse_expr_list_1 x st = parse
  | ws* ',' {
      let tl = parse_expr_list st lexbuf in
      [x] :: tl
    }
  | ws* {
      [[x]]
    }

and parse_char_stream_semi fn fn2 st = parse
  | '(' {
      fn2 st lexbuf
    }
  | ws* {
      let r = fn st lexbuf in
      discard_opt_semi lexbuf;
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

and parse_define b closing st = parse
  | ws* (r_ident as f) ws* '(' {
      let args = parse_params st lexbuf in
      let nst = State.create ~src:st.src lexbuf in
      let (a, _) = parse_ast b ["end"] nst lexbuf in
      (* We do not include the body of the underlying let-expression in the
         computation of the location. *)
      let loc = State.current_loc st in
      let nst = State.create ~src:st.src lexbuf in
      let (k, t) = parse_ast b closing nst lexbuf in
      let u = Ast.mk_define ~loc f args a k in
      let st = State.push_token u st in
      State.tokens st, t
    }
and parse_params st = parse
  | ws* (r_ident as a) ws* '=' ws* {
      let e = parse_simple_expr st lexbuf in
      (a, Some e) :: parse_params_1 st lexbuf
    }
  | ws* (r_ident as a) {
      (a, None) :: parse_params_1 st lexbuf
    }
  | ws* ')' ws* {
      []
    }
and parse_params_1 st = parse
  | ws* ',' ws* (r_ident as a) ws* '=' {
      let e = parse_simple_expr st lexbuf in
      (a, Some e) :: parse_params_1 st lexbuf
    }
  | ws* ',' ws* (r_ident as a) {
      (a, None) :: parse_params_1 st lexbuf
    }
  | ws* ')' ws* {
      []
    }

and parse_let b closing st = parse
  |  ws* (r_ident as k) ';' ws* {
      let nst = State.create ~src:st.src lexbuf in
      let (v, _) = parse_ast b ["in"] nst lexbuf in
      (* We do not include the body of the let-expression in the
         computation of the location. *)
      let loc = State.current_loc st in
      let nst = State.create ~src:st.src lexbuf in
      let (a, t) = parse_ast b closing nst lexbuf in
      let u = Ast.mk_let ~loc k v a in
      let st = State.push_token u st in
      State.tokens st, t
    }

and parse_include b closing st = parse
  | value as file {
    let u = Ast.mk_include ~loc:(State.current_loc st) (`File file) in
    let st = State.push_token u st in
    parse_ast b closing st lexbuf
  }

and parse_apply b st = parse
  | (r_ident as f) '%' {
      assert (`variable ["with"] = variable lexbuf) ;
      let app =
        let rec loop () =
          let nst = State.create ~src:st.src lexbuf in
          match parse_ast b ["and"; "end"] nst lexbuf with
          | a, "and" -> a :: loop ()
          | a, _ -> [ a ]
        in loop ()
      in
      Ast.mk_apply ~loc:(State.current_loc st) f (List.map (fun v -> None, v) app)
    }
  | (r_ident as f) {
      let app = parse_apply_tuple st lexbuf in
      Ast.mk_apply ~loc:(State.current_loc st) f app
    }

and parse_expr_stmt st = parse
  | "" {
      parse_char_stream_semi parse_simple_expr parse_simple_expr_1 st lexbuf
    }

and parse_if b st = parse
  | "" {
      let e = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 st lexbuf in
      let (a1, a2) =
        let rec loop () =
          let nst = State.create ~src:st.src lexbuf in
          let (a1, t) = parse_ast b ["elseif"; "else"; "end"] nst lexbuf in
          match t with
          | "elseif" ->
            let e2 = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 st lexbuf in
            let (a2, a3) = loop () in
            a1, [ Ast.mk_if e2 a2 a3 ]
          | "else" ->
            let nst = State.create ~src:st.src lexbuf in
            let (a2, _) = parse_ast b ["end"] nst lexbuf in
            a1, a2
          | _ -> a1, []
        in loop ()
      in
      Ast.mk_if ~loc:(State.current_loc st) e a1 a2
    }

and parse_for b st = parse
  | (r_ident as iterator) ';' {
      let min = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 st lexbuf in
      let max = parse_char_stream_semi parse_simple_expr parse_simple_expr_1 st lexbuf in
      let nst = State.create ~src:st.src lexbuf in
      let (a, _) = parse_ast b ["end"] nst lexbuf in
      Ast.mk_for ~loc:(State.current_loc st) iterator min max a
    }

and parse_foreach b st = parse
  | "" {
      let [@warning "-8"] hd :: tl = compound_var lexbuf in
      let params = parse_foreach_params st lexbuf in
      let nst = State.create ~src:st.src lexbuf in
      let (a, _) = parse_ast b ["end"] nst lexbuf in
      Ast.mk_foreach ~loc:(State.current_loc st) (hd, tl) params a
    }

and parse_foreach_params st = parse
  | '(' {
      parse_tuple_1 st lexbuf
    }
  | ';'? {
      []
    }
