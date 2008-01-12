(* camlp5r *)
(* $Id: pr_dep.ml,v 5.9 2008-01-12 08:41:18 ddr Exp $ *)

#load "q_MLast.cmo";

open MLast;

value not_impl name x = do {
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  Printf.eprintf "pr_depend: not impl: %s; %s\n" name desc;
  flush stderr
};

module StrSet =
  Set.Make (struct type t = string; value compare = compare; end)
;

value fset = ref StrSet.empty;
value addmodule s = fset.val := StrSet.add s fset.val;

value list = List.iter;

value option f =
  fun
  [ Some x -> f x
  | None -> () ]
;

value vala f v = Pcaml.vala_mapa f (fun _ -> ()) v;

value longident =
  fun
  [ [s; _ :: _] -> addmodule s
  | _ -> () ]
;

value rec ctyp =
  fun
  [ TyAcc _ t _ -> ctyp_module t
  | TyAli _ t1 t2 -> do { ctyp t1; ctyp t2 }
  | TyApp _ t1 t2 -> do { ctyp t1; ctyp t2 }
  | TyAny _ -> ()
  | TyArr _ t1 t2 -> do { ctyp t1; ctyp t2 }  
  | TyLab _ _ t -> ctyp t
  | TyLid _ _ -> ()
  | TyMan _ t1 t2 -> do { ctyp t1; ctyp t2 }
  | TyOlb _ _ t -> ctyp t
  | <:ctyp< ' $_$ >> -> ()
  | <:ctyp< ($list:tl$) >> -> list ctyp tl
  | x ->
      IFDEF OCAML_309 THEN
        match x with
        [ TyRec _ ldl -> list label_decl ldl
        | <:ctyp< [ $list:cdl$ ] >> -> list constr_decl cdl
        | x -> not_impl "ctyp" x ]
      ELSE
        match x with
        [ <:ctyp< { $list:ldl$ } >> -> list label_decl ldl
        | <:ctyp< [ $list:cdl$ ] >> -> list constr_decl cdl
        | x -> not_impl "ctyp" x ]
      END ]
and constr_decl (_, _, tl) =
  IFDEF CAMLP5_4_09 AND STRICT THEN
    match tl with
    [ <:vala< tl >> -> list ctyp tl
    | _ -> failwith "constr_decl" ]
  ELSE
    list ctyp tl
  END
and label_decl (_, _, _, t) = ctyp t
and ctyp_module =
  fun
  [ TyAcc _ t _ -> ctyp_module t
  | TyApp _ t1 t2 -> do { ctyp t1; ctyp t2; }
  | <:ctyp< $uid:m$ >> -> addmodule m
  | x -> not_impl "ctyp_module" x ]
;

value rec patt =
  fun
  [ PaAcc _ p _ -> patt_module p
  | PaAli _ p1 p2 -> do { patt p1; patt p2; }
  | PaAny _ -> ()
  | PaApp _ p1 p2 -> do { patt p1; patt p2; }
  | <:patt< [| $list:pl$ |] >> -> list patt pl
  | PaChr _ _ -> ()
  | <:patt< $int:_$ >> -> ()
  | PaLab _ _ po -> option patt po
  | PaLid _ _ -> ()
  | PaOrp _ p1 p2 -> do { patt p1; patt p2; }
  | <:patt< { $list:lpl$ } >> -> list label_patt lpl
  | PaRng _ p1 p2 -> do { patt p1; patt p2; }
  | PaStr _ _ -> ()
  | <:patt< ($list:pl$) >> -> list patt pl
  | PaTyc _ p t -> do { patt p; ctyp t; }
  | PaUid _ _ -> ()
  | PaVrn _ _ -> ()
  | x -> not_impl "patt" x ]
and patt_module =
  fun
  [ <:patt< $uid:m$ >> -> addmodule m
  | PaAcc _ p _ -> patt_module p
  | x -> not_impl "patt_module" x ]
and label_patt (p1, p2) = do { patt p1; patt p2; }
and expr =
  fun
  [ ExAcc _ e1 e2 -> do { expr_module e1; expr e2; }
  | ExApp _ e1 e2 -> do { expr e1; expr e2; }
  | ExAre _ e1 e2 -> do { expr e1; expr e2; }
  | <:expr< [| $list:el$ |] >> -> list expr el
  | <:expr< assert False >> -> ()
  | <:expr< assert $e$ >> -> expr e
  | ExAss _ e1 e2 -> do { expr e1; expr e2; }
  | ExChr _ _ -> ()
  | ExCoe _ e t1 t2 -> do { expr e; option ctyp t1; ctyp t2 }
  | <:expr< for $_$ = $e1$ $to:_$ $e2$ do { $list:el$ } >> -> do {
      expr e1;
      expr e2;
      list expr el;
    }
  | <:expr< fun [ $list:pwel$ ] >> -> list match_case pwel
  | ExIfe _ e1 e2 e3 -> do { expr e1; expr e2; expr e3; }
  | <:expr< $int:_$ >> -> ()
  | ExFlo _ _ -> ()
  | ExLab _ _ eo -> option expr eo
  | ExLaz _ e -> expr e
  | <:expr< let $flag:_$ $list:pel$ in $e$ >> -> do {
      list let_binding pel;
      expr e;
    }
  | ExLid _ _ -> ()
  | ExLmd _ _ me e -> do { module_expr me; expr e; }
  | <:expr< match $e$ with [ $list:pwel$ ] >> -> do {
      expr e;
      list match_case pwel
    }
  | ExOlb _ _ eo -> option expr eo
  | <:expr< { $list:lel$ } >> -> list label_expr lel
  | <:expr< { ($w$) with $list:lel$ } >> -> do {
      list label_expr lel;
      expr w;
    }
  | <:expr< do { $list:el$ } >> -> list expr el
  | ExSnd _ e _ -> expr e
  | ExSte _ e1 e2 -> do { expr e1; expr e2; }
  | ExStr _ _ -> ()
  | <:expr< try $e$ with [ $list:pwel$ ] >> -> do {
      expr e;
      list match_case pwel
    }
  | <:expr< ($list:el$) >> -> list expr el
  | ExTyc _ e t -> do { expr e; ctyp t; }
  | ExUid _ _ -> ()
  | ExVrn _ _ -> ()
  | <:expr< while $e$ do { $list:el$ } >> -> do { expr e; list expr el; }
  | x -> not_impl "expr" x ]
and expr_module =
  fun
  [ <:expr< $uid:m$ >> -> addmodule m
  | e -> expr e ]
and let_binding (p, e) = do { patt p; expr e }
and label_expr (p, e) = do { patt p; expr e }
and match_case (p, w, e) = do { patt p; vala (option expr) w; expr e; }
and module_type =
  fun 
  [ <:module_type< $uid:m$ . $_$ >> -> addmodule m
  | <:module_type< functor ($uid:_$ : $mt1$) -> $mt2$ >> -> do {
      module_type mt1;
      module_type mt2
    }
  | <:module_type< sig $list:sil$ end >> -> list sig_item sil
  | <:module_type< $uid:_$ >> -> ()
  | <:module_type< $mt$ with $list:wc$ >> -> do {
      module_type mt;
      list with_constr wc
    }
  | x -> not_impl "module_type" x ]
and with_constr =
  fun
  [ WcTyp _ _ _ _ t -> ctyp t
  | x -> not_impl "with_constr" x ]
and sig_item =
  fun
  [ <:sig_item< declare $list:sil$ end >> -> list sig_item sil
  | <:sig_item< exception $_$ of $list:tl$ >> -> list ctyp tl
  | SgExt _ _ t _ -> ctyp t
  | <:sig_item< module $flag:_$ $list:ntl$ >> ->
      list (fun (_, mt) -> module_type mt) ntl
  | SgMty _ _ mt -> module_type mt
  | <:sig_item< open $[s :: _]$ >> -> addmodule s
  | <:sig_item< type $list:tdl$ >> -> list type_decl tdl
  | SgVal _ _ t -> ctyp t
  | x -> not_impl "sig_item" x ]
and module_expr =
  fun
  [ <:module_expr< $uid:m$ . $_$ >> -> addmodule m
  | MeApp _ me1 me2 -> do { module_expr me1; module_expr me2; }
  | MeFun _ _ mt me -> do { module_type mt; module_expr me; }
  | <:module_expr< struct $list:sil$ end >> -> list str_item sil
  | MeTyc _ me mt -> do { module_expr me; module_type mt; }
  | MeUid _ _ -> ()
  | x -> not_impl "module_expr" x ]
and str_item =
  fun
  [ <:str_item< declare $list:sil$ end >> -> list str_item sil
  | StDir _ _ _ -> ()
  | <:str_item< exception $uid:_$ of $list:tl$ = $list:_$ >> -> list ctyp tl
  | <:str_item< $exp:e$ >> -> expr e
  | <:str_item< external $lid:_$ : $t$ = $list:_$ >> -> ctyp t
  | <:str_item< module $flag:_$ $list:nel$ >> ->
      list (fun (_, me) -> module_expr me) nel
  | <:str_item< module type $uid:_$ = $mt$ >> -> module_type mt
  | <:str_item< open $[s :: _]$ >> -> addmodule s
  | <:str_item< type $list:tdl$ >> -> list type_decl tdl
  | <:str_item< value $flag:_$ $list:pel$ >> -> list let_binding pel
  | x -> not_impl "str_item" x ]
and type_decl td = ctyp td.tdDef;

(* Print dependencies *)

value load_path = ref [""];

value find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let rec try_dir =
      fun
      [ [] -> raise Not_found
      | [dir :: rem] ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname then fullname else try_dir rem ]
    in
    try_dir path
;

value find_depend modname (byt_deps, opt_deps) =
  let name = String.uncapitalize modname in
  try
    let filename = find_in_path load_path.val (name ^ ".mli") in
    let basename = Filename.chop_suffix filename ".mli" in
    let byt_dep = basename ^ ".cmi" in
    let opt_dep =
      if Sys.file_exists (basename ^ ".ml") then basename ^ ".cmx"
      else basename ^ ".cmi"
    in
    ([byt_dep :: byt_deps], [opt_dep :: opt_deps])
  with
  [ Not_found ->
      try
        let filename = find_in_path load_path.val (name ^ ".ml") in
        let basename = Filename.chop_suffix filename ".ml" in
        ([basename ^ ".cmo" :: byt_deps], [basename ^ ".cmx" :: opt_deps])
      with
      [ Not_found -> (byt_deps, opt_deps) ] ]
;

value (depends_on, escaped_eol) =
  match Sys.os_type with
  [ "Unix" | "Win32" | "Cygwin" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | _ -> assert False ]
;

value print_depend target_file deps =
  match deps with
  [ [] -> ()
  | _ ->
      do {
        print_string target_file;
        print_string depends_on;
        let rec print_items pos =
          fun
          [ [] -> print_string "\n"
          | [dep :: rem] ->
              if pos + String.length dep <= 76 then do {
                print_string dep;
                print_string " ";
                print_items (pos + String.length dep + 1) rem
              }
              else do {
                print_string escaped_eol;
                print_string dep;
                print_string " ";
                print_items (String.length dep + 5) rem
              } ]
        in
        print_items (String.length target_file + 2) deps
      } ]
;

(* Main *)

value depend_sig ast =
  do {
    fset.val := StrSet.empty;
    List.iter (fun (si, _) -> sig_item si) ast;
    let basename = Filename.chop_suffix Pcaml.input_file.val ".mli" in
    let fset = StrSet.elements fset.val in
    let (byt_deps, opt_deps) = List.fold_right find_depend fset ([], []) in
    print_depend (basename ^ ".cmi") byt_deps;
  }
;

value depend_str ast =
  do {
    fset.val := StrSet.empty;
    List.iter (fun (si, _) -> str_item si) ast;
    let basename =
      if Filename.check_suffix Pcaml.input_file.val ".ml" then
        Filename.chop_suffix Pcaml.input_file.val ".ml"
      else
        try
          let len = String.rindex Pcaml.input_file.val '.' in
          String.sub Pcaml.input_file.val 0 len
        with
        [ Failure _ | Not_found -> Pcaml.input_file.val ]
    in
    let init_deps =
      if Sys.file_exists (basename ^ ".mli") then
        let cmi_name = basename ^ ".cmi" in ([cmi_name], [cmi_name])
      else ([], [])
    in
    let fset = StrSet.elements fset.val in
    let (byt_deps, opt_deps) = List.fold_right find_depend fset init_deps in
    print_depend (basename ^ ".cmo") byt_deps;
    print_depend (basename ^ ".cmx") opt_deps;
  }
;

Pcaml.print_interf.val := depend_sig;
Pcaml.print_implem.val := depend_str;

Pcaml.add_option "-I"
  (Arg.String (fun dir -> load_path.val := load_path.val @ [dir]))
  "<dir> Add <dir> to the list of include directories.";
