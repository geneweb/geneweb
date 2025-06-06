module Compat = Geneweb_compat

let with_cache (type a b) (f : a -> b) : a -> b =
  let cache : (a, b) Hashtbl.t = Hashtbl.create 17 in
  fun x ->
    match Hashtbl.find cache x with
    | exception Not_found ->
        let r = f x in
        Hashtbl.add cache x r;
        r
    | r -> r

let parse_ast lexbuf = Lexer.parse_ast (Buffer.create 1024) [] [] lexbuf |> fst

let parse_file s =
  Compat.In_channel.with_open_text s @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  (* TODO: This function is not available in OCaml 4.08. We should
     find a workaround after fixing locations in the parser. *)
  (* Lexing.set_filename lexbuf s; *)
  parse_ast lexbuf

let parse_source ~cached src =
  match src with
  | `File s -> if cached then with_cache parse_file s else parse_file s
  | `In_channel ic -> parse_ast @@ Lexing.from_channel ic
  | `Raw s -> parse_ast @@ Lexing.from_string s

let comment fl =
  let s =
    match Filename.extension fl with
    | ".css" | ".js" -> Fmt.str "/* %s */\n" fl
    | _ -> Fmt.str "<!-- %s --->\n" fl
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
