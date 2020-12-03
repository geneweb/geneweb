open Geneweb
open Config
open Jingoo
open Jg_types
open Gwxjg

module Optimize =  struct

  type stats =
    { mutable inline_const : int
    ; mutable preapply : string list
    ; mutable literal_to_text : int
    ; mutable concat : int
    ; mutable flatten : int
    ; mutable inline_trans : int
    }

  let stats =
    { inline_const = 0
    ; preapply = []
    ; literal_to_text = 0
    ; concat = 0
    ; flatten = 0
    ; inline_trans = 0
    }

  let kwargs_args =
    let rec loop kwargs args = function
      | [] -> (kwargs, args)
      | (Some kw, LiteralExpr e) :: tl -> loop (kwargs @ [ (kw, e) ]) args tl
      | (None, LiteralExpr e) :: tl -> loop kwargs (args @ [e]) tl
      | _ -> raise Not_found
    in
    loop [] []

  let is_std_filter n =
    Array.exists (fun (n', _) -> n' = n) Jg_runtime.std_filters

  let inline_const stmts =
    let flag = ref true in
    let open Jg_ast_mapper in
    let ht = Hashtbl.create 128 in
    let statement self = function
      | SetStatement ( SetExpr [ IdentExpr i ]
                     , ApplyExpr (IdentExpr "CONST", [ None, LiteralExpr e ]) ) ->
        Hashtbl.add ht i e ;
        flag := true ;
        Statements []
      | s -> default_mapper.statement self s
    in
    let expression self = function
      | IdentExpr i when Hashtbl.mem ht i ->
        stats.inline_const <- stats.inline_const + 1 ;
        LiteralExpr (Hashtbl.find ht i)
      | e -> default_mapper.expression self e
    in
    let mapper = { default_mapper with statement ; expression } in
    let rec loop ast = if !flag then (flag := false ; loop (mapper.ast mapper ast)) else ast in
    loop stmts

  let preapply stmts =
    let open Jg_ast_mapper in
    let local_variables : (string * string list) list ref = ref [("", [])] in
    let push_block name = local_variables := (name, []) :: !local_variables in
    let pop_block () = local_variables := List.tl !local_variables in
    let set_local x =
      let fst, snd = List.hd !local_variables in
      local_variables := (fst, x :: snd ) :: (List.tl !local_variables) in
    let is_local (x : string) = List.exists (fun (_, l) -> List.mem x l) !local_variables in
    let rec maybe_set = function
      | SetExpr set -> List.iter maybe_set set
      | IdentExpr id -> set_local id
      | _ -> () in
    let statement self = function
      | SetBlockStatement (id, _) as s ->
        set_local id ;
        push_block id ;
        let s = default_mapper.statement self s in
        pop_block () ;
        s
      | SetStatement (id, _) as s ->
        maybe_set id ;
        default_mapper.statement self s
      | ForStatement (id, _, _) as s ->
        push_block "" ;
        List.iter set_local id ;
        let s = default_mapper.statement self s in
        pop_block () ;
        s
      | FunctionStatement (id, args, _)
      | MacroStatement (id, args, _) as s ->
        push_block id ;
        set_local id ;
        List.iter (fun (i, _) -> set_local i) args ;
        let s = default_mapper.statement self s in
        pop_block () ;
        s
      | CallStatement(_, args, _, _) as s ->
        push_block "" ;
        List.iter (fun (i, _) -> set_local i) args ;
        let s = default_mapper.statement self s in
        pop_block () ;
        s
      | TextStatement (_)
      | ExpandStatement (_)
      | IfStatement (_)
      | SwitchStatement (_, _)
      | IncludeStatement (_, _)
      | RawIncludeStatement _
      | ExtendsStatement _
      | ImportStatement (_, _)
      | FromImportStatement (_, _)
      | BlockStatement (_, _)
      | FilterStatement (_, _)
      | WithStatement (_, _)
      | AutoEscapeStatement (_, _)
      | NamespaceStatement (_, _)
      | Statements (_)
        as s -> default_mapper.statement self s
    in
    let expression self = function
      | ApplyExpr (IdentExpr n , args) as e
        when List.for_all (function (_, LiteralExpr _) -> true | _ -> false) args
          && (not @@ is_local n) && is_std_filter n ->
        let kwargs, args = kwargs_args args in
        let rec loop i =
          let (n', fn) = Array.get Jg_runtime.std_filters i in
          if n = n'
          then match Jg_runtime.jg_apply ~kwargs fn args with
            | Tfun _ -> Jg_ast_mapper.default_mapper.expression self e
            | x ->
              stats.preapply <- n :: stats.preapply ;
              LiteralExpr x
          else loop (i + 1)
        in loop 0
      | e -> Jg_ast_mapper.default_mapper.expression self e
    in
    let mapper = { Jg_ast_mapper.default_mapper with statement ; expression } in
    mapper.ast mapper stmts

  let literal_to_text stmts =
    let open Jg_ast_mapper in
    let functions = ref [] in
    let statement self = function
      (* a function returns a tvalue, it do not print text *)
      | FunctionStatement _ as s ->
        functions := () :: !functions ;
        let s = default_mapper.statement self s in
        functions := List.tl @@ !functions ;
        s
      | ExpandStatement (LiteralExpr e) when !functions = [] ->
        stats.literal_to_text <- stats.literal_to_text + 1 ;
        TextStatement (Jg_runtime.string_of_tvalue e)
      | s -> default_mapper.statement self s
    in
    let mapper = { Jg_ast_mapper.default_mapper with statement } in
    mapper.ast mapper stmts

  let concat stmts =
    let flush str acc =
      if str = [] then acc
      else begin
        stats.concat <- stats.concat + 1 ;
        let s = String.concat "" (List.rev str) in
        let s = Str.global_replace (Str.regexp "[\n ]+") " " s in
        TextStatement s :: acc
      end
    in
    let rec loop str acc = function
      | [] -> List.rev (flush str acc)
      | ExpandStatement (LiteralExpr (Tstr s)) :: tl
      | TextStatement s :: tl -> loop (s :: str) acc tl
      | Statements stmts :: tl ->
        cont str acc tl @@
        Statements (loop [] [] stmts)
      | IfStatement br :: tl ->
        cont str acc tl @@
        IfStatement (List.map (fun (e, stmts) -> e, loop [] [] stmts) br)
      | ForStatement (binds, e, stmts) :: tl ->
        cont str acc tl @@
        ForStatement (binds, e, loop [] [] stmts)
      | MacroStatement (exp, args, stmts) :: tl ->
        cont str acc tl @@
        MacroStatement (exp, args, loop [] [] stmts)
      | BlockStatement (e, stmts) :: tl ->
        cont str acc tl @@
        BlockStatement (e, loop [] [] stmts)
      | SwitchStatement (e, br) :: tl ->
        cont str acc tl @@
        SwitchStatement (e, List.map (fun (e, stmts) -> e, loop [] [] stmts) br)
      | s :: tl -> loop [] (s :: flush str acc) tl
    and cont str acc tl s =
      loop [] (s :: flush str acc) tl
    in
    loop [] [] stmts

  let flatten stmts =
    let rec loop acc = function
      | [] -> List.rev acc
      | Statements stmts :: tl ->
        stats.flatten <- stats.flatten + 1 ;
        loop (List.rev_append (loop [] stmts) acc) tl
      | hd :: tl ->
        let hd = match hd with
          | TextStatement _
          | ExpandStatement _
          | IncludeStatement _
          | RawIncludeStatement _
          | ExtendsStatement _
          | ImportStatement _
          | FromImportStatement _
          | SetStatement _
          | SetBlockStatement _
          | NamespaceStatement _
            as s -> s
          | IfStatement (branches) ->
            IfStatement (List.map (fun (e, ast) -> (e, loop [] ast)) branches)
          | SwitchStatement (e, cases) ->
            SwitchStatement (e, List.map (fun (e, ast) -> (e, loop [] ast)) cases)
          | ForStatement (ids, e2, ast) ->
            ForStatement (ids, e2, loop [] ast)
          | BlockStatement (n, ast) ->
            BlockStatement (n, loop [] ast)
          | MacroStatement (n, args, ast) ->
            MacroStatement (n, args, loop [] ast)
          | FunctionStatement (n, args, ast) ->
            FunctionStatement (n, args, loop [] ast)
          | FilterStatement (n, ast) ->
            FilterStatement (n, loop [] ast)
          | CallStatement (n, a1, a2, ast) ->
            CallStatement (n, a1, a2, loop [] ast)
          | WithStatement (el, ast) ->
            WithStatement (el, loop [] ast)
          | AutoEscapeStatement (e, ast) ->
            AutoEscapeStatement (e, loop [] ast)
          | Statements _ -> assert false
        in
        loop (hd :: acc) tl
    in
    loop [] stmts

  let inline_trans lexicon stmts =
    let open Jg_ast_mapper in
    let trans ~kwargs s i =
      stats.inline_trans <- stats.inline_trans + 1 ;
      LiteralExpr (Data.trans_aux lexicon ~kwargs s i)
    in
    let expression self = function
      | ApplyExpr (IdentExpr "trans", args) as e ->
        begin try
            let kwargs, args = kwargs_args args in
            match args with
            | [ Tint i ; Tstr s ] -> trans ~kwargs s i
            | [ Tstr s ] -> trans ~kwargs s 0
            | _ -> default_mapper.expression self e
          with Not_found -> default_mapper.expression self e
        end
      | e -> default_mapper.expression self e
    in
    let mapper = { default_mapper with expression } in
    mapper.ast mapper stmts

  (* let unroll stmts =
   *   let open Jg_ast_mapper in
   *   let statement self = function
   *     | ForStatement (binds, LiteralExpr e, _stmts) as s->
   *       begin match Jg_runtime.jg_list e with
   *         | Tlist es ->
   *           let stmts = [] in
   *           Statements stmts
   *         | _ -> s
   *       end ;
   *     | s -> default_mapper.statement self s
   *   in
   *   let mapper = { default_mapper with statement } in
   *   mapper.ast mapper stmts *)

  let compiled_fname file lang = file ^ "." ^ lang ^ ".bin~"

  let marshal env src ch_out lexicon =
    let ch_in = open_in src in
    let lexbuf = Lexing.from_channel ch_in in
    Jg_lexer.reset_context () ;
    Jg_lexer.init_lexer_pos (Some src) lexbuf ;
    let ast =
      try Jg_parser.input Jg_lexer.main lexbuf
      with e -> raise @@ SyntaxError (Jg_utils.get_parser_error e lexbuf)
    in
    close_in ch_in ;
    let rec loop stmts fn =
      let stmts' = fn stmts in
      if stmts' = stmts then stmts else loop stmts' fn
    in
    let ast =
      loop ast @@ fun ast ->
      Jg_interp.unfold_extends env ast
      |> Jg_ast_optimize.inline_include env
      |> Jg_interp.replace_blocks
      |> inline_const
      |> inline_trans lexicon
      |> preapply
      |> literal_to_text
      |> flatten
      |> concat
      |> Jg_ast_optimize.dead_code_elimination
    in
    let (_, _, _, _) as r =
      let env = { Jg_types.autoescape = false
                ; template_dirs = []
                ; filters = []
                ; extensions = []
                ; strict_mode = true
                }
      in
      let ast, macros = Jg_interp.extract_macros env ast in
      let top_frame = Hashtbl.create (Array.length Jg_runtime.std_filters) in
      Array.iter (fun (n, v) -> Hashtbl.add top_frame n v) Jg_runtime.std_filters;
      let ctx =
        { frame_stack = [ Hashtbl.find top_frame ]
        ; macro_table = Hashtbl.create 0
        ; namespace_table = Hashtbl.create 0
        ; active_filters = []
        ; serialize = false
        ; output = fun _ -> assert false
        }
      in
      env, ctx, ast, macros
    in
    Marshal.to_channel ch_out r [ Marshal.Closures ] ;
    Printf.printf
      "\
      %s: '%s'\n\
      \tinline_const: %d\n\
      \tpreapply: %d\n\
      \tliteral_to_text: %d\n\
      \tconcat: %d\n\
      \tflatten: %d\n\
      \tinline_trans: %d\n\
      "
      __LOC__ src
      stats.inline_const
      (List.length stats.preapply)
      stats.literal_to_text
      stats.concat
      stats.flatten
      stats.inline_trans ;
    List.iter
      (fun s ->
         print_endline @@
         "\t" ^ s ^ " (" ^ string_of_int (List.length (List.filter ((=) s) stats.preapply)) ^ ")" )
      (List.sort_uniq compare stats.preapply) ;
    r

end

module Interp = struct

  let w_updated_conf fn =
    fun assets conf base ->
      let conf =
        if List.mem_assoc "theme" conf.env
        then { conf with henv = ("theme", List.assoc "theme" conf.env) :: conf.henv }
        else conf
      in
      let env = List.filter (fun (k, _) -> not (List.mem_assoc k conf.henv || List.mem_assoc k conf.senv)) conf.env in
      let conf = { conf with env } in
      fn assets conf base

  let w_default_env assets conf base models =
    let asset =
      func_arg1_no_kw begin function
        | Tstr s -> Tstr (Filename.concat assets s)
        | x -> failwith_type_error_1 "asset" x
      end
    in
    let enabled_plugin =
      func_arg1_no_kw begin function
        | Tstr s -> Tbool (List.mem s !Gwd_lib.GwdPlugin.registered
                           && match List.assoc_opt "plugins" conf.Config.base_env with
                           | Some list -> List.mem s @@ String.split_on_char ',' list
                           | None -> false)
        | x -> failwith_type_error_1 "asset" x
      end
    in
    let default = Data.default_env conf base in
    function
    | "M" -> models
    | "asset" -> asset
    | "enabled_plugin" -> enabled_plugin
    | x -> List.assoc x default

  (* let interp_templ ~models (env, ctx, ast, macros) =
   *   let buffer = Buffer.create 1024 in
   *   let ctx =
   *     { ctx with frame_stack = models :: ctx.frame_stack
   *              ; output = fun x -> Buffer.add_string buffer (Jg_runtime.string_of_tvalue x)
   *     }
   *   in
   *   List.iter (fun f -> let (k, v) = f ctx in Hashtbl.add ctx.macro_table k v) macros ;
   *   ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast ;
   *   Wserver.print_string @@ Buffer.contents buffer *)

  let interp_templ ~models (env, ctx, ast, macros) =
    let ctx =
      { ctx with frame_stack = models :: ctx.frame_stack
               ; output = fun x -> Wserver.print_string (Jg_runtime.string_of_tvalue x)
      }
    in
    List.iter (fun f -> let (k, v) = f ctx in Hashtbl.add ctx.macro_table k v) macros ;
    ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast

  let interp file assets conf base models =
    let lang = conf.lang in
    let template_dir = Filename.concat assets "templates" in
    let file = Filename.concat template_dir file in
    let ast =
      let fname = Optimize.compiled_fname file lang in
      Mutil.read_or_create ~wait:true ~magic:Mutil.random_magic fname
        begin fun ch ->
          print_endline __LOC__ ;
          (Marshal.from_channel ch : environment * context * ast * (context -> string * macro) list)
        end
        begin fun ch ->
          print_endline __LOC__ ;
          let env =
            { Jg_types.autoescape = false
            ; template_dirs = [ template_dir ]
            ; filters = []
            ; extensions = []
            ; strict_mode = true
            }
          in
          Optimize.marshal env file ch conf.lexicon
        end
    in
    let models = w_default_env assets conf base models in
    interp_templ ~models ast ;
    true

end

module Handler = struct

  let getenv_list fn prefix conf =
    let rec loop acc i =
      match Util.p_getenv conf.env (prefix ^ string_of_int i) with
      | Some k -> loop (fn k :: acc) (i + 1)
      | None -> acc
    in
    loop [] 0

  let asearch assets conf base =
    Interp.interp "SEARCH_ADVANCED.html.jingoo" assets conf base Tnull

  let adtree_aux assets conf base root =
    let root = Gwxjg.Data.unsafe_mk_person conf base root in
    let models = Tpat begin function "root" -> root | _ -> raise Not_found end in
    Interp.interp "AD_TREE.html.jingoo" assets conf base models

  let adtree assets conf base =
    match Util.find_person_in_env conf base "" with
    | Some p ->
      adtree_aux assets conf base p
    | None -> assert false

  let ssearch assets conf base =
    let v = Mutil.decode @@ List.assoc "v" conf.env in
    let l = Name.split_sname v in
    let rec loop acc = function
      | [] -> []
      | hd :: tl -> let acc = hd :: acc in (acc, tl) :: loop acc tl
    in
    let l = loop [] l in
    let l = List.rev_append l @@ List.rev_map (fun (a, b) -> (b, a)) l in
    let l = List.sort_uniq compare l in
    let l =
      List.map begin fun (fn, sn) ->
        let conf =
          { conf with env =
                        ("first_name", String.concat " " fn)
                        :: ("surname", String.concat " " sn)
                        :: conf.env }
        in
        fst @@ AdvSearchOk.advanced_search conf base max_int
      end l
    in
    let l = List.flatten l in
    match List.sort_uniq compare l with
    | [ p ] ->
      let models =
        Tpat begin function
          | "redirect" -> Tstr ("i=" ^ Gwdb.string_of_iper (Gwdb.get_iper p))
          | _ -> raise Not_found
        end
      in
      Interp.interp "REDIRECT.html.jingoo" assets conf base models
    | l ->
      let l = Tlist (List.map (Data.unsafe_mk_person conf base) l) in
      let models = Tpat (function "result" -> l | _ -> raise Not_found) in
      Interp.interp "SEARCH_RESULT.html.jingoo" assets conf base models

  let shortest_path assets conf base p =
    let fexcl = getenv_list Gwdb.ifam_of_string "ef" conf in
    let root =
      match List.assoc_opt "ei" conf.env with
      | Some i -> Gwdb.iper_of_string i
      | None -> match Util.find_person_in_env conf base "1" with
        | Some root -> (Gwdb.get_iper root)
        | None -> assert false
    in
    match Relation.get_shortest_path_relation conf base (Gwdb.get_iper p) root fexcl with
    | None ->
      let models =
        let target = Data.unsafe_mk_person conf base p in
        let root = Data.get_n_mk_person conf base root in
        let ifams = Tlist (List.map (fun i -> Data.get_n_mk_family conf base i @@ Gwdb.foi base i) fexcl) in
        Tpat begin function
          | "excluded" -> ifams
          | "root" -> root
          | "target" -> target
          | _ -> raise Not_found
        end
      in
      Interp.interp "PATH_ERROR.html.jingoo" assets conf base models

    | Some (path, ifam) ->
      let path =
        Tlist begin List.rev_map begin fun (i, r) ->
            let p = Data.get_n_mk_person conf base i in
            let r = match r with
              | Relation.Self -> Tstr "Self"
              | Relation.Parent -> Tstr "Parent"
              | Relation.Sibling -> Tstr "Sibling"
              | Relation.HalfSibling -> Tstr "HalfSibling"
              | Relation.Mate -> Tstr "Mate"
              | Relation.Child -> Tstr "Child"
            in
            Tpat begin function "person" -> p | "relation" -> r | _ -> raise Not_found end
          end path end
      in
      let models =
        let ifam = Tstr (Gwdb.string_of_ifam ifam) in
        Tpat begin function
          | "path" -> path
          | "ifam" -> ifam
          | _ -> raise Not_found
        end
      in
      Interp.interp "PATH.html.jingoo" assets conf base models

  let h_tree assets conf base =
    match Util.find_person_in_env conf base "" with
    | None -> assert false
    | Some p ->
      let root = Gwxjg.Data.unsafe_mk_person conf base p in
      let models = Tpat (function "root" -> root | _ -> raise Not_found) in
      Interp.interp "H_TREE.html.jingoo" assets conf base models

  let home assets conf base =
    match Util.find_person_in_env conf base "" with
    | Some p ->
      if List.assoc_opt "et" conf.env = Some "S"
      && List.assoc_opt "em" conf.env = Some "R"
      then shortest_path assets conf base p
      else
        let root = Gwxjg.Data.unsafe_mk_person conf base p in
        let models = Tpat (function "root" -> root | _ -> raise Not_found) in
        Interp.interp "IND.html.jingoo" assets conf base models
    | None ->
      Interp.interp "HOME.html.jingoo" assets conf base Tnull

  let mod_ind assets conf base =
    match Util.p_getenv conf.env "i" with
    | Some i ->
      let p = Gwdb.poi base (Gwdb.iper_of_string i) in
      let sp = UpdateInd.string_person_of base p in
      let digest = Tstr (Update.digest_person sp) in
      let root = Gwxjg.Data.unsafe_mk_person conf base p in
      let models =
        Tpat begin function
          | "root" -> root
          | "digest" -> digest
          | _ -> raise Not_found
        end
      in
      Interp.interp "MOD_IND.html.jingoo" assets conf base models
    | _ -> false

  let mod_ind_ok assets conf base =
    let prok conf base wl pgl p ofn osn oocc dr rs =
      if wl = [] && pgl = [] && dr = [] && rs = []
      then
        let models =
          Tpat begin function
            | "redirect" -> Tstr ("i=" ^ Gwdb.string_of_iper p.Def.key_index)
            | _ -> raise Not_found
          end
        in
        ignore @@ Interp.interp "REDIRECT.html.jingoo" assets conf base models
      else UpdateIndOk.print_mod_ok conf base wl pgl p ofn osn oocc dr rs
    in
    UpdateIndOk.print_mod ~prok conf base ;
    true

  let mod_fam assets conf base =
    if not conf.wizard then false
    else match Util.p_getenv conf.env "i" with
      | Some i ->
        let root =
          match Util.find_person_in_env conf base "p" with
          | Some p -> Gwxjg.Data.unsafe_mk_person conf base p
          | None -> Tnull
        in
        let ifam = Gwdb.ifam_of_string i in
        let sfam = UpdateFam.string_family_of conf base ifam in
        let digest = Tstr (Update.digest_family sfam) in
        let models =
          Tpat begin function
            | "digest" -> digest
            | "family" -> Gwxjg.Data.get_n_mk_family conf base ifam @@ Gwdb.foi base ifam
            | "root" -> root
            | _ -> raise Not_found
          end
        in
        Interp.interp "MOD_FAM.html.jingoo" assets conf base models
      | _ -> false

  let warning assets conf base =
    let ht = Hashtbl.create 1024 in
    Check.check_base base ignore (fun x -> Hashtbl.replace ht x ()) ignore ;
    let warnings =
      Hashtbl.fold begin fun w () acc ->
        Gwxjg.Data.mk_warning conf base w :: acc
      end ht []
    in
    let models =
      let warnings = Tlist warnings in
      Tpat begin function
        | "warnings" -> warnings
        | _ -> raise Not_found
      end
    in
    Interp.interp "WARNINGS.html.jingoo" assets conf base models

end

let ns = "v8"

let () =
  let open Handler in
  let aux fn assets conf base =
    match base with
    | None -> assert false
    | Some base -> Interp.w_updated_conf fn assets conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "AD_TREE", aux adtree
    ; "H_TREE", aux h_tree
    ; "MOD_FAM", aux mod_fam
    ; "MOD_IND", aux mod_ind
    ; "MOD_IND_OK", aux mod_ind_ok
    ; "SEARCH_ADVANCED", aux asearch
    ; "SEARCH_SIMPLE", aux ssearch
    ; "WARNINGS", aux warning
    ]
