module Compat = Geneweb_compat
module Fpath = Geneweb_fs.Fpath

let with_cache (type a b) (f : a -> b) : a -> b =
  let cache : (a, b) Hashtbl.t = Hashtbl.create 17 in
  fun x ->
    match Hashtbl.find cache x with
    | exception Not_found ->
        let r = f x in
        Hashtbl.add cache x r;
        r
    | r -> r

let parse_ast ~src lexbuf =
  let st = Lexer.State.create ~src lexbuf in
  Lexer.parse_ast (Buffer.create 1024) [] st lexbuf |> fst

let parse_file ~src fl =
  Compat.In_channel.with_open_text (Fpath.to_string fl) @@ fun ic ->
  (* We do not use position feature of the Lexing module as our lexer does not
     produce a single token per call. Instead, we rely on
     `lex_abs_pos`, `lex_start_pos` and `lex_curr_pos` of [lexbuf] to compute
     locations. *)
  let lexbuf = Lexing.from_channel ~with_positions:false ic in
  parse_ast ~src lexbuf

let parse_source ~cached src =
  match src with
  | `File fl ->
      if cached then with_cache (parse_file ~src) fl else parse_file ~src fl
  | `Raw s -> parse_ast ~src (Lexing.from_string ~with_positions:false s)

let comment fl =
  let s =
    match Fpath.extension fl with
    | ".css" | ".js" -> Fmt.str "/* %a */\n" Fpath.pp fl
    | _ -> Fmt.str "<!-- %a -->\n" Fpath.pp fl
  in
  Ast.mk_text s

let parse ?(cached = true) ~on_exn ~resolve_include src =
  let parse_include ~loc src =
    try Ast.mk_pack ~loc @@ parse_source ~cached src
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      on_exn e bt;
      Ast.mk_pack ~loc []
  in
  let rec expand (Ast.{ desc; loc } as t) =
    match desc with
    | Atext _ | Avar _ | Atransl _ | Awid_hei _ | Aint _ | Aapply _ | Aforeach _
    | Afor _ | Aop1 _ | Aop2 _ ->
        t
    | Adefine (s, l1, l2, l3) ->
        Ast.mk_define ~loc s l1 (expand_list l2) (expand_list l3)
    | Aif (c, l1, l2) -> Ast.mk_if ~loc c (expand_list l1) (expand_list l2)
    | Alet (s, l1, l2) -> Ast.mk_let ~loc s (expand_list l1) (expand_list l2)
    | Apack l -> Ast.mk_pack ~loc (expand_list l)
    | Ainclude src -> (
        match src with
        | `File fl ->
            let fl = resolve_include loc fl in
            let t = parse_include ~loc (`File fl) in
            Ast.mk_pack @@ [ comment fl; expand t; comment fl ]
        | `Raw s -> expand @@ parse_include ~loc (`Raw s))
  and expand_list l = List.map expand l in
  expand_list @@ parse_source ~cached src
