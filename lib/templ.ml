open Config
open TemplAst

exception Exc_located of loc * exn

let raise_with_loc loc = function
  | Exc_located (_, _) as e ->
      incr GWPARAM.nb_errors;
      raise e
  | e ->
      incr GWPARAM.nb_errors;
      raise (Exc_located (loc, e))

let input_templ conf fname =
  match Util.open_etc_file conf fname with
  | None -> None
  | Some (ic, fname) ->
      Templ_parser.wrap fname (fun () ->
          let lex = Lexing.from_channel ic in
          let r = Templ_parser.parse_templ conf lex in
          close_in ic;
          Some r)

(* Common evaluation functions *)

let subst_text x v s =
  if String.length x = 0 then s
  else
    let rec loop len i i_ok =
      if i = String.length s then
        if i_ok > 0 then loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
        else Buff.get len
      else if s.[i] = x.[i_ok] then
        if i_ok = String.length x - 1 then loop (Buff.mstore len v) (i + 1) 0
        else loop len (i + 1) (i_ok + 1)
      else if i_ok > 0 then loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
      else loop (Buff.store len s.[i]) (i + 1) 0
    in
    loop 0 0 0

let rec subst sf = function
  | Atext (loc, s) -> Atext (loc, sf s)
  | Avar (loc, s, sl) -> (
      let s1 = sf s in
      if
        sl = []
        &&
        try
          let _ = int_of_string s1 in
          true
        with Failure _ -> false
      then Aint (loc, s1)
      else
        let sl1 = List.map sf sl in
        match String.split_on_char '.' s1 with
        | [ _ ] -> Avar (loc, s1, sl1)
        | s2 :: sl2 -> Avar (loc, s2, sl2 @ sl1)
        | _ -> assert false)
  | Atransl (loc, b, s, c) -> Atransl (loc, b, sf s, c)
  | Aconcat (loc, al) -> Aconcat (loc, List.map (subst sf) al)
  | Awid_hei s -> Awid_hei (sf s)
  | Aif (e, alt, ale) -> Aif (subst sf e, substl sf alt, substl sf ale)
  | Aforeach ((loc, s, sl), pl, al) ->
      (* Dans le cas d'une "compound variable", il faut la décomposer. *)
      (* Ex: "ancestor.father".family  =>  ancestor.father.family      *)
      let s1 = sf s in
      let sl1 = List.map sf sl in
      let s, sl =
        match
          String.split_on_char '.' s1 (* Templ_parser.compound_var lex *)
        with
        | [ _ ] -> (s1, sl1)
        | s2 :: sl2 -> (s2, List.rev_append (List.rev_map sf sl2) sl1)
        | _ -> assert false
      in
      Aforeach ((loc, s, sl), List.map (substl sf) pl, substl sf al)
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
      match (s.[i], s.[i + 1]) with
      | ':', ':' ->
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
    | ' ' | '\n' | '\r' -> skip_spaces_and_newlines s (i + 1)
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

let esc s = (Util.escape_html s :> string)

let url_aux ?(pwd = true) conf =
  let l =
    List.filter_map
      (fun (k, v) ->
        let v = Adef.as_string v in
        if ((k = "oc" || k = "ocz") && v = "0") || k = "" then None
        else Some (Format.sprintf "%s=%s" k v))
      (conf.henv @ conf.senv @ conf.env)
  in
  let prefix =
    if pwd then (Util.prefix_base_password conf :> string)
    else (Util.prefix_base conf :> string)
  in
  if conf.cgi then prefix ^ "?" ^ conf.bname ^ String.concat "&" l
  else prefix ^ String.concat "&" l

let order =
  [
    "b";
    "lang";
    "templ";
    "iz";
    "pz";
    "nz";
    "ocz";
    "m";
    "em";
    "t";
    "et";
    "i";
    "p";
    "n";
    "oc";
    "wide";
    "im";
    "sp";
    "ma";
    "v";
  ]

let reorder conf url_env =
  let new_lang =
    match List.assoc_opt "lang" url_env with Some l1 -> l1 | None -> ""
  in
  let keep_lang =
    (* same condition in Util.commd and copyr.txt *)
    conf.default_lang <> new_lang
  in
  (* process evars from order *)
  let env1, ok =
    let rec loop (acc1, acc2) order =
      match order with
      | [] -> (acc1, acc2)
      | k :: order ->
          let v =
            match List.assoc_opt k url_env with Some v -> v | None -> ""
          in
          if
            List.mem_assoc k url_env
            &&
            match (k, v) with
            | "lang", _ -> keep_lang
            | "oc", v when v = "" || v = "0" -> false
            | "ocz", v when v = "" || v = "0" -> false
            | _, v when v <> "" -> true
            | _, _ -> false
          then loop (Format.sprintf "%s=%s" k v :: acc1, k :: acc2) order
          else loop (acc1, acc2) order
    in
    loop ([], []) order
  in
  (* process other evars from env *)
  let env2 =
    List.fold_left
      (fun acc (k, v) ->
        if
          List.mem k ok
          || (k = "lang" && not keep_lang)
          || ((k = "oc" || k = "ocz") && (v = "" || v = "0"))
          || v = ""
        then acc
        else Format.sprintf "%s=%s" k v :: acc)
      [] url_env
  in
  String.concat "&" (List.rev env1 @ List.rev env2)

let find_sosa_ref conf =
  let env = conf.henv @ conf.senv @ conf.env in
  let get_env evar env =
    match List.assoc_opt evar env with Some s -> Adef.as_string s | None -> ""
  in
  let pz = get_env "pz" env in
  let nz = get_env "nz" env in
  let ocz = get_env "ocz" env in
  let iz = get_env "iz" env in
  match (iz, pz, nz, ocz) with
  | iz, "", "", "" when iz <> "" -> iz
  | "", pz, nz, ocz when pz <> "" || nz <> "" ->
      Format.sprintf "%s.%s %s" pz ocz nz
  | _ -> (
      match List.assoc_opt "sosa_ref" conf.base_env with
      | Some s when s <> "" -> s
      | _ -> "No sosa ref")

(* url_set_aux can reset several evar from evar_l in one call *)
let url_set_aux conf evar_l str_l =
  let str_l =
    List.mapi
      (fun i _evar -> if i < List.length str_l then List.nth str_l i else "")
      evar_l
  in
  let href =
    match String.split_on_char '?' (Util.commd conf :> string) with
    | [] ->
        !GWPARAM.syslog `LOG_WARNING "Empty Url\n";
        ""
    | s :: _l -> s
  in
  let conf_l = conf.henv @ conf.senv @ conf.env in
  let k_l = List.map (fun (k, _v) -> k) conf_l in
  let k_l = List.sort_uniq compare k_l in
  let conf_l = List.map (fun k -> (k, List.assoc k conf_l)) k_l |> List.rev in
  (* process evar_l *)
  let url_env =
    let rec loop i acc evar_l =
      match evar_l with
      | [] -> acc
      | evar :: evar_l ->
          let str = List.nth str_l i in
          if str <> "" then loop (i + 1) ((evar, str) :: acc) evar_l
          else loop (i + 1) acc evar_l
    in
    loop 0 [] evar_l
  in
  (* process the remainder of conf_l *)
  let url_env =
    let rec loop acc conf_l =
      match conf_l with
      | [] -> acc
      | (k, _v) :: conf_l when List.mem k evar_l -> loop acc conf_l
      | (k, v) :: conf_l -> loop ((k, Adef.as_string v) :: acc) conf_l
    in
    loop url_env conf_l
  in
  (* reorder *)
  Format.sprintf "%s?%s" href (reorder conf url_env)

let substr_start_aux n s =
  let len = String.length s in
  let rec loop i n str =
    if n = 0 || i >= len then str
    else
      (* Attention aux caractères utf-8 !! *)
      let nbc = Utf8.nbc s.[i] in
      let car = String.sub s i nbc in
      loop (i + nbc) (n - 1) (str ^ car)
  in
  loop 0 n ""

let rec eval_variable conf = function
  | [ "lang"; "full" ] ->
      let rec func x lst c =
        match lst with
        | [] -> "bad language code"
        | hd :: tl ->
            if hd = x then Util.transl_nth conf "!languages" c
            else func x tl (c + 1)
      in
      func conf.lang Version.available_languages 0
  | [ "bvar"; "list" ] ->
      let is_duplicate key assoc_list =
        let rec aux count = function
          | [] -> count > 1
          | (k, _) :: tl -> if k = key then aux (count + 1) tl else aux count tl
        in
        aux 0 assoc_list
      in
      let l =
        List.sort (fun (k1, _v1) (k2, _v2) -> compare k1 k2) conf.base_env
      in
      List.fold_left
        (fun acc (k, v) ->
          let duplicate =
            if is_duplicate k l then {| style="color:red"|} else ""
          in
          acc
          ^ Format.sprintf "<b%s>%s</b>=%s<br>\n" duplicate k
              (Util.escape_html v :> string))
        "" l
  | [ "gwd"; "arglist" ] -> !GWPARAM.gwd_cmd
  | [ "bvar"; v ] | [ "b"; v ] -> (
      try List.assoc v conf.base_env with Not_found -> "")
  | [ "connections"; "wizards" ] -> (
      match conf.n_connect with
      | Some (_c, cw, _cf, _) -> if cw > 0 then Printf.sprintf "%d" cw else ""
      | None -> "")
  | [ "connections"; "friends" ] -> (
      match conf.n_connect with
      | Some (_c, _cw, cf, _) -> if cf > 0 then Printf.sprintf "%d" cf else ""
      | None -> "")
  | [ "connections"; "total" ] -> (
      match conf.n_connect with
      | Some (c, _cw, _cf, _) -> if c > 0 then Printf.sprintf "%d" c else ""
      | None -> "")
  | [ "evar"; v; "ns" ] | [ "e"; v; "ns" ] -> (
      try
        let vv = List.assoc v (conf.env @ conf.henv) in
        Mutil.gen_decode false vv |> esc
      with Not_found -> "")
  | [ "evar"; v ] | [ "e"; v ] -> (
      (* TODO verify if senv must be treated as well *)
      match Util.p_getenv (conf.env @ conf.henv) v with
      | Some vv -> esc vv
      | None -> "")
  (* look for evar.vi scanning i down to 0 *)
  | [ "evar_cur"; v; i ] ->
      (* TODO same as evar *)
      let n = int_of_string i in
      let rec loop n =
        match Util.p_getenv (conf.env @ conf.henv) (v ^ string_of_int n) with
        | Some vv -> vv
        | None -> if n > 0 then loop (n - 1) else ""
      in
      loop n
  | [ "link_next" ] -> ""
  | [ "person_index"; _x ] -> ""
  | [ "prefix_set"; pl ] ->
      let pl_l =
        match pl with
        | "iz" -> [ "iz"; "nz"; "pz"; "ocz" ]
        | "p" -> [ "i"; "p"; "n"; "oc" ]
        | "p1" -> [ "i1"; "p1"; "n1"; "oc1" ]
        | "p2" -> [ "i2"; "p2"; "n2"; "oc2" ]
        | "pn" -> [ "i1"; "i2"; "p1"; "p2"; "n1"; "n2"; "oc1"; "oc2" ]
        | "all" -> [ "templ"; "p_mod"; "wide" ]
        | _ -> [ pl ]
      in
      (Util.commd ~excl:pl_l conf :> string)
  | [ "prefix_set"; evar; str ] ->
      let prefix = (Util.commd ~excl:[ evar ] conf :> string) in
      let amp = if prefix.[String.length prefix - 1] = '?' then "" else "&" in
      if str = "" then prefix
      else prefix ^ Printf.sprintf "%s%s=%s" amp evar str
  | [ "random"; "init" ] ->
      Random.self_init ();
      ""
  | [ "random"; "bits" ] -> (
      try string_of_int (Random.bits ())
      with Failure _ | Invalid_argument _ -> raise Not_found)
  | [ "random"; s ] -> (
      try string_of_int (Random.int (int_of_string s))
      with Failure _ | Invalid_argument _ -> raise Not_found)
  | "nb_persons" :: sl -> eval_int conf conf.nb_of_persons sl
  | "nb_families" :: sl -> eval_int conf conf.nb_of_families sl
  | [ "substr_start"; n; v ] -> (
      (* extract the n first characters of string v *)
      match int_of_string_opt n with
      | Some n -> substr_start_aux n v
      | None -> raise Not_found)
  | [ "substr_start_e"; n; v ] -> (
      (* extract the n first characters of string v *)
      match int_of_string_opt n with
      | Some n ->
          let v =
            Option.value ~default:""
              (Util.p_getenv (conf.env @ conf.henv @ conf.senv) v)
          in
          substr_start_aux n v
      | None -> raise Not_found)
  | "time" :: sl -> eval_time_var conf sl
  (* clear some variables in url *)
  (* set the first variable to a new value if <> "" *)
  | [ "url_set"; evar_l; str_l ] ->
      let evar_l = String.split_on_char '_' evar_l in
      let str_l = String.split_on_char '_' str_l in
      url_set_aux conf evar_l str_l
  | [ "url_set"; evar_l ] ->
      let evar_l = String.split_on_char '_' evar_l in
      url_set_aux conf evar_l []
  | [ "user"; "ident" ] -> conf.user
  | [ "user"; "name" ] -> conf.username
  | [ "user"; "key" ] -> conf.userkey
  | [ s ] -> eval_simple_variable conf s
  | _ -> raise Not_found

and eval_int conf n = function
  | [ "hexa" ] -> Printf.sprintf "0x%X" n
  | [ "octal" ] -> Printf.sprintf "0x%o" n
  | [ "v" ] -> string_of_int n
  | [] -> Mutil.string_of_int_sep (Util.transl conf "(thousand separator)") n
  | _ -> raise Not_found

and eval_time_var conf = function
  | [ "hours" ] ->
      let hh, _, _ = conf.time in
      Printf.sprintf "%02d" hh
  | [ "minutes" ] ->
      let _, mm, _ = conf.time in
      Printf.sprintf "%02d" mm
  | [ "seconds" ] ->
      let _, _, ss = conf.time in
      Printf.sprintf "%02d" ss
  | [] ->
      let hh, mm, ss = conf.time in
      Printf.sprintf "%02d:%02d:%02d" hh mm ss
  | _ -> raise Not_found

and eval_simple_variable conf = function
  | "action" -> conf.command
  | "border" -> string_of_int conf.border
  | "charset" -> conf.charset
  | "connections" -> (
      match conf.n_connect with
      | Some (c, cw, cf, _) ->
          if c > 0 then
            " - "
            ^ Printf.sprintf "%s %d" (Util.transl conf "connections") c
            ^ (if cw > 0 then
               Printf.sprintf ", %s %s"
                 (Util.transl_nth conf "wizard/wizards/friend/friends/exterior"
                    1)
                 (if conf.wizard then
                  Printf.sprintf "<a href=\"%sm=CONN_WIZ\">%d</a>"
                    (Util.commd conf :> string)
                    cw
                 else string_of_int cw)
              else "")
            ^
            if cf > 0 then
              Printf.sprintf ", %s %d"
                (Util.transl_nth conf "wizard/wizards/friend/friends/exterior" 3)
                cf
            else ""
          else ""
      | None -> "")
  | "doctype" -> (Util.doctype :> string)
  | "highlight" -> conf.highlight
  | "gw_prefix" ->
      let s =
        if conf.cgi then Adef.escaped conf.gw_prefix else Adef.escaped ""
      in
      let s = (s :> string) in
      if s = "" then s else s ^ Filename.dir_sep
  | "images_prefix" | "image_prefix" ->
      Util.images_prefix conf ^ Filename.dir_sep
  | "lang" -> conf.lang
  | "lang_fallback" -> (
      match List.assoc_opt conf.lang !Mutil.fallback with
      | Some l -> l
      | None -> "")
  | "default_lang" -> conf.default_lang
  | "browser_lang" -> conf.browser_lang
  | "left" -> conf.left
  | "nl" -> "\n"
  | "nn" -> ""
  | "plugins" ->
      let l = List.map Filename.basename conf.plugins in
      String.concat "," l
  | "bname" -> conf.bname
  | "token" -> conf.cgi_passwd
  | "bname_token" -> String.concat "_" [ conf.bname; conf.cgi_passwd ]
  | "prefix" -> (Util.commd conf :> string)
  | "prefix_base" -> (Util.commd ~pwd:false conf :> string)
  | "prefix_base_password" -> (Util.commd conf :> string)
  | "prefix_no_iz" ->
      (Util.commd ~excl:[ "iz"; "nz"; "pz"; "ocz" ] conf :> string)
  | "prefix_no_templ" -> (Util.commd ~excl:[ "templ" ] conf :> string)
  | "prefix_no_pmod" -> (Util.commd ~excl:[ "p_mod" ] conf :> string)
  | "prefix_no_wide" -> (Util.commd ~excl:[ "wide" ] conf :> string)
  | "prefix_no_lang" -> (Util.commd ~excl:[ "lang" ] conf :> string)
  | "prefix_no_all" ->
      (Util.commd ~excl:[ "templ"; "p_mod"; "wide" ] conf :> string)
  | "referer" -> (Util.get_referer conf :> string)
  | "right" -> conf.right
  | "sosa_ref" -> find_sosa_ref conf
  | "setup_link" -> if conf.setup_link then " - " ^ setup_link conf else ""
  | "sp" -> " "
  | "static_path" | "etc_prefix" ->
      let s =
        if conf.cgi then Adef.escaped conf.etc_prefix else Adef.escaped ""
      in
      let s = (s :> string) in
      if s = "" then s else s ^ Filename.dir_sep
  | "suffix" ->
      (* On supprime de env toutes les paires qui sont dans (henv @ senv) *)
      let l =
        List.fold_left
          (fun accu (k, _) -> List.remove_assoc k accu)
          conf.env (conf.henv @ conf.senv)
      in
      let l =
        List.filter_map
          (fun (k, v) ->
            let v = Adef.as_string v in
            if ((k = "oc" || k = "ocz") && v = "0") || k = "" then None
            else Some (Format.sprintf "%s=%s" k v))
          l
      in
      String.concat "&" l
  | "url" -> url_aux ~pwd:true conf
  | "url_no_pwd" -> url_aux ~pwd:false conf
  | "version" -> Version.ver
  | "commit_id" -> Version.commit_id
  | "commit_date" -> Version.commit_date
  | "compil_date" -> Version.compil_date
  | "branch" -> Version.branch
  | "source" -> Version.src
  | "/" -> ""
  | _ -> raise Not_found

let rec string_of_expr_val = function
  | VVstring s -> s
  | VVbool true -> "1"
  | VVbool false -> "0"
  | VVother f -> string_of_expr_val (f [])

let eval_string_var conf eval_var sl =
  try eval_var sl
  with Not_found -> (
    try VVstring (eval_variable conf sl)
    with Not_found ->
      GWPARAM.errors_undef :=
        Printf.sprintf "%%%s?" (String.concat "." sl) :: !GWPARAM.errors_undef;
      VVstring (Printf.sprintf " %%%s?" (String.concat "." sl)))

let eval_var_handled conf sl =
  try eval_variable conf sl
  with Not_found ->
    GWPARAM.errors_undef :=
      Printf.sprintf "%%%s?" (String.concat "." sl) :: !GWPARAM.errors_undef;
    Printf.sprintf " %%%s?" (String.concat "." sl)

let apply_format conf nth s1 s2 =
  let s1 =
    (* perform nth selection before format check *)
    match nth with
    | Some n -> Util.transl_nth conf s1 n
    | None -> Util.transl conf s1
  in
  if not (String.contains s1 '%') then s1
  else
    try
      match Util.check_format "%t" s1 with
      | Some s3 -> Printf.sprintf s3 (fun _ -> s2)
      | None -> (
          match Util.check_format "%s" s1 with
          | Some s3 -> Printf.sprintf s3 s2
          | None -> (
              match Util.check_format "%d" s1 with
              | Some s3 -> Printf.sprintf s3 (int_of_string s2)
              | None -> (
                  let s21, s22 =
                    try
                      let i = String.index s2 ':' in
                      ( String.sub s2 0 i,
                        String.sub s2 (i + 1) (String.length s2 - i - 1) )
                    with _ -> ("", "")
                  in
                  match Util.check_format "%s%s" s1 with
                  | Some s3 -> Printf.sprintf s3 s21 s22
                  | None -> (
                      match Util.check_format "%t%s" s1 with
                      | Some s3 -> Printf.sprintf s3 (fun _ -> s21) s22
                      | None -> (
                          match Util.check_format "%d%s" s1 with
                          | Some s3 -> Printf.sprintf s3 (int_of_string s21) s22
                          | None -> (
                              match Util.check_format "%s%t" s1 with
                              | Some s3 -> Printf.sprintf s3 s21 (fun _ -> s22)
                              | None -> (
                                  match Util.check_format "%t%t" s1 with
                                  | Some s3 ->
                                      Printf.sprintf s3
                                        (fun _ -> s21)
                                        (fun _ -> s22)
                                  | None -> (
                                      match Util.check_format "%d%t" s1 with
                                      | Some s3 ->
                                          Printf.sprintf s3 (int_of_string s21)
                                            (fun _ -> s22)
                                      | None -> (
                                          match Util.check_format "%s%d" s1 with
                                          | Some s3 ->
                                              Printf.sprintf s3 s21
                                                (int_of_string s22)
                                          | None -> (
                                              match
                                                Util.check_format "%t%d" s1
                                              with
                                              | Some s3 ->
                                                  Printf.sprintf s3
                                                    (fun _ -> s21)
                                                    (int_of_string s22)
                                              | None -> (
                                                  match
                                                    Util.check_format "%d%d" s1
                                                  with
                                                  | Some s3 ->
                                                      Printf.sprintf s3
                                                        (int_of_string s21)
                                                        (int_of_string s22)
                                                  | None -> "[" ^ s1 ^ "?]")))))
                              ))))))
    with _ ->
      Printf.sprintf "Format error in %s\n" s1 |> !GWPARAM.syslog `LOG_WARNING;
      s1

let rec eval_ast conf = function
  | Atext (_, s) -> s
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
  let c_opt = [ '0'; '1'; '2'; '3'; 'n'; 's'; 'w'; 'f'; 'c'; 'e'; 't' ] in
  let scan_for_transl s c i =
    (* scans starting at i for bracketed translation [to be translated] *)
    (* the space after translation can be used to force a choice *)
    (* if no choice, then c is used *)
    let j =
      match String.index_from_opt s i '[' with Some j -> j | None -> -1
    in
    let k =
      match String.index_from_opt s i ']' with Some k -> k | None -> -1
    in
    let existing_choice =
      if k <> -1 && k < String.length s - 2 then List.mem s.[k + 1] c_opt
      else false
    in
    let c =
      if String.length s = k then c
      else if String.length s > k + 1 && List.mem s.[k + 1] c_opt then
        String.make 1 s.[k + 1]
      else c
    in
    if j = -1 || k = -1 then (-1, s)
    else
      ( k + 1,
        String.sub s 0 (k + 1)
        ^ c
        ^
        if String.length s = k + 1 || String.length s = k + 2 then ""
        else if existing_choice then
          String.sub s (k + 2) (String.length s - k - 2)
        else String.sub s (k + 1) (String.length s - k - 1) )
  in
  let r =
    let nth = try Some (int_of_string c) with Failure _ -> None in
    match split_at_coloncolon s with
    | None -> (
        try apply_format conf nth s ""
        with Failure _ ->
          raise Not_found
          (* TODO check the use of if c = "n" then s else Mutil.nominative s
             nominative expects a : in the string ! *))
    | Some (s1, s2) -> (
        try
          if String.length s2 > 0 && s2.[0] = '|' then
            let i = 1 in
            let j = String.rindex s2 '|' in
            if j = 0 then
              (* missing second | *)
              let s2 = String.sub s2 i (String.length s2 - j - 1) in
              try apply_format conf nth s1 s2
              with Failure _ -> raise Not_found
            else
              let s3 =
                let s = String.sub s2 i (j - i) in
                (* scan s for potential translates *)
                let _k, s =
                  let rec loop (k, s) =
                    let k, s = scan_for_transl s c k in
                    if k = -1 then (k, s) else loop (k, s)
                  in
                  loop (0, s)
                in
                let astl =
                  Templ_parser.parse_templ conf (Lexing.from_string s)
                in
                (* parse_templ handles only text, evars and translations *)
                (* more complex parsing (%surname;, %if; ...) not available *)
                List.fold_left (fun s a -> s ^ eval_ast conf a) "" astl
              in
              let s4 = String.sub s2 (j + 1) (String.length s2 - j - 1) in
              let s2 = s3 ^ s4 in
              try apply_format conf nth s1 s2
              with Failure _ -> raise Not_found
          else if String.length s2 > 0 && s2.[0] = ':' then
            (* this is a third colon *)
            let s2 = String.sub s2 1 (String.length s2 - 1) in
            try apply_format conf nth s1 s2 with Failure _ -> raise Not_found
          else raise Not_found
        with Not_found ->
          let s3 =
            match nth with
            | Some n -> Util.transl_nth conf s2 n
            | None -> if s2 = "" then "" else Util.transl conf s2
          in
          Util.transl_decline conf s1 s3)
  in
  let r = Util.simple_decline conf r in
  let r = Util.translate_eval r in
  if upp then Utf8.capitalize_fst r else r

let loc_of_expr = function
  | Atext (loc, _) -> loc
  | Avar (loc, _, _) -> loc
  | Atransl (loc, _, _, _) -> loc
  | Aapply (loc, _, _) -> loc
  | Aop1 (loc, _, _) -> loc
  | Aop2 (loc, _, _, _) -> loc
  | Aint (loc, _) -> loc
  | _ -> ("", -1, -1)

let templ_eval_var conf = function
  | [ "browsing_with_sosa_ref" ] -> (
      match Util.p_getenv conf.env "sosa_ref" with
      | Some _ -> VVbool true
      | _ -> VVbool false)
  | [ "cancel_links" ] -> VVbool (Util.p_getenv conf.env "cgl" = Some "on")
  | [ "cgi" ] -> VVbool conf.cgi
  | [ "debug" ] -> VVbool conf.debug
  | [ "false" ] -> VVbool false
  | [ "has_referer" ] ->
      (* deprecated since version 5.00 *)
      VVbool (Mutil.extract_param "referer: " '\n' conf.request <> "")
  | [ "just_friend_wizard" ] -> VVbool conf.just_friend_wizard
  | [ "friend" ] -> VVbool conf.friend
  | [ "manitou" ] -> VVbool conf.manitou
  | [ "plugin"; plugin ] ->
      VVbool (List.mem plugin (List.map Filename.basename conf.plugins))
  | [ "supervisor" ] -> VVbool conf.supervisor
  | [ "true" ] -> VVbool true
  | [ "wizard" ] -> VVbool conf.wizard
  | [ "is_printed_by_template" ] -> VVbool conf.is_printed_by_template
  | _ -> raise Not_found

let bool_of e = function
  | VVbool b -> b
  | VVstring _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "bool value expected")

let string_of e = function
  | VVstring s -> s
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "string value expected")

let int_of e = function
  | VVstring s -> (
      try int_of_string s
      with Failure _ ->
        raise_with_loc (loc_of_expr e)
          (Failure ("int value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "int value expected")

let float_of e = function
  | VVstring s -> (
      try Float.of_string s
      with Failure _ ->
        raise_with_loc (loc_of_expr e)
          (Failure ("float value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "float value expected")

let num_of e = function
  | VVstring s -> (
      try Sosa.of_string s
      with Failure _ ->
        raise_with_loc (loc_of_expr e)
          (Failure ("num value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ ->
      raise_with_loc (loc_of_expr e) (Failure "num value expected")

let rec eval_expr ((conf, eval_var, eval_apply) as ceva) = function
  | Atext (_, s) -> VVstring s
  | Avar (loc, s, sl) as e -> (
      try eval_var loc (s :: sl)
      with Not_found -> (
        try templ_eval_var conf (s :: sl)
        with Not_found ->
          raise_with_loc (loc_of_expr e)
            (Failure ("unbound var \"" ^ String.concat "." (s :: sl) ^ "\""))))
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
            | [ e ] -> e
            | el ->
                let sl = List.map string_of_expr_val el in
                VVstring (String.concat "" sl))
          ell
      in
      VVstring (eval_apply loc s vl)
  | Aop1 (loc, op, e) -> (
      let v = eval_expr ceva e in
      match op with
      | "not" -> VVbool (not (bool_of e v))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")))
  | Aop2 (loc, op, e1, e2) -> (
      let int e = int_of e (eval_expr ceva e) in
      let float e = float_of e (eval_expr ceva e) in
      let num e = num_of e (eval_expr ceva e) in
      let bool e = bool_of e (eval_expr ceva e) in
      let string e = string_of e (eval_expr ceva e) in
      match op with
      | "and" -> VVbool (if bool e1 then bool e2 else false)
      | "or" -> VVbool (if bool e1 then true else bool e2)
      | "is_substr" | "in" -> VVbool (Mutil.contains (string e2) (string e1))
      | "=" -> VVbool (eval_expr ceva e1 = eval_expr ceva e2)
      | "<" -> VVbool (int e1 < int e2)
      | ">" -> VVbool (int e1 > int e2)
      | "!=" -> VVbool (eval_expr ceva e1 <> eval_expr ceva e2)
      | "<=" -> VVbool (int e1 <= int e2)
      | ">=" -> VVbool (int e1 >= int e2)
      | "+" -> VVstring (string_of_int (int e1 + int e2))
      | "-" -> VVstring (string_of_int (int e1 - int e2))
      | "*" -> VVstring (string_of_int (int e1 * int e2))
      | "|" -> VVstring (string_of_int (int e1 / int e2))
      | "^" -> VVstring (Sosa.to_string (Sosa.exp (num e1) (int e2)))
      | "/" -> VVstring (Sosa.to_string (Sosa.div (num e1) (int e2)))
      | "/." ->
          VVstring
            (let fl = Float.div (float e1) (float e2) in
             if Float.is_integer fl then string_of_int (Float.to_int fl)
             else Float.to_string fl)
      | "%" -> VVstring (Sosa.to_string (Sosa.modl (num e1) (int e2)))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")))
  | Aint (_, s) -> VVstring s
  | e -> raise_with_loc (loc_of_expr e) (Failure (not_impl "eval_expr" e))

let print_error ((fname, bp, ep) as pos) exc =
  incr GWPARAM.nb_errors;
  if !GWPARAM.nb_errors <= 10 then (
    if fname = "" then Printf.eprintf "*** <W> template file"
    else Printf.eprintf "File %s" fname;
    let line = if fname = "" then None else Templ_parser.line_of_loc pos in
    Printf.eprintf ", ";
    (match line with
    | Some (lin, col1, col2) ->
        Printf.eprintf "line %d, characters %d-%d:\n" lin col1 col2
    | None -> Printf.eprintf "characters %d-%d:\n" bp ep);
    (match exc with
    | Failure s -> Printf.eprintf "Failed - %s" s
    | _ -> Printf.eprintf "%s" (Printexc.to_string exc));
    Printf.eprintf "\n\n";
    flush stderr)

let eval_bool_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    | VVbool b -> b
    | VVstring _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "bool value expected")
  with Exc_located (loc, exc) ->
    print_error loc exc;
    false

let eval_string_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    | VVstring s -> Util.translate_eval s
    | VVbool _ | VVother _ ->
        raise_with_loc (loc_of_expr e) (Failure "string value expected")
  with Exc_located (loc, exc) ->
    print_error loc exc;
    ""

let print_body_prop conf =
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\""
    with Not_found -> ""
  in
  Output.print_sstring conf (s ^ Util.body_prop conf)

type 'a vother =
  | Vdef of string list * ast list
  | Vval of 'a expr_val
  | Vbind of string * Adef.encoded_string

type 'a env = (string * 'a) list

type ('a, 'b) interp_fun = {
  eval_var : 'a env -> 'b -> loc -> string list -> 'b expr_val;
  eval_transl : 'a env -> bool -> string -> string -> string;
  eval_predefined_apply : 'a env -> string -> 'b expr_val list -> string;
  get_vother : 'a -> 'b vother option;
  set_vother : 'b vother -> 'a;
  print_foreach :
    ('a env -> 'b -> ast -> unit) ->
    ('a env -> 'b -> ast -> string) ->
    'a env ->
    'b ->
    loc ->
    string ->
    string list ->
    ast list list ->
    ast list ->
    unit;
}

let get_def get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
    | Some (Vdef (al, el)) -> Some (al, el)
    | _ -> None
  with Not_found -> None

let get_val get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (List.assoc k env) with
    | Some (Vval x) -> Some x
    | _ -> None
  with Not_found -> None

let set_def set_vother k al el env =
  let k = "#" ^ k in
  (k, set_vother (Vdef (al, el))) :: env

let set_val set_vother k v env =
  let k = "#" ^ k in
  (k, set_vother (Vval v)) :: env

let eval_subst loc f set_vother env xl vl a =
  let rec loop env a xl vl =
    match (xl, vl) with
    | x :: xl, VVstring v :: vl -> loop env (subst (subst_text x v) a) xl vl
    | x :: xl, v :: vl ->
        let env = set_val set_vother x v env in
        loop env a xl vl
    | [], [] -> (env, a)
    | _ -> (env, Atext (loc, f ^ ": bad # of params"))
  in
  loop env a xl vl

let squeeze_spaces s =
  let rec loop i =
    if i = String.length s then ""
    else
      match s.[i] with
      | ' ' | '\n' | '\r' | '\t' -> loop (i + 1)
      | _ -> String.sub s i (String.length s - i)
  in
  loop 0

let print_apply loc f set_vother print_ast env ep gxl al gvl =
  let local_print_ast a =
    let env, a =
      let rec loop env a xl vl =
        match (xl, vl) with
        | x :: xl, VVstring v :: vl -> loop env (subst (subst_text x v) a) xl vl
        | x :: xl, v :: vl -> loop (set_val set_vother x v env) a xl vl
        | [], [] -> (env, a)
        | _ ->
            ( env,
              Atext
                ( loc,
                  Printf.sprintf "%s: bad # of params (%d instead of %d)" f
                    (List.length gvl) (List.length gxl) ) )
      in
      loop env a gxl gvl
    in
    print_ast env ep a
  in
  let rec loop = function
    | [] -> ()
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in
        loop (Atext (loc, s) :: al)
    | a :: al ->
        local_print_ast a;
        loop al
  in
  loop al

let templ_eval_expr = eval_string_expr
let templ_print_apply = print_apply

let rec templ_print_foreach conf print_ast set_vother env ep _loc s sl _el al =
  match s :: sl with
  | [ "env_binding" ] ->
      print_foreach_env_binding conf print_ast set_vother env ep al
  | _ -> raise Not_found

and print_foreach_env_binding conf print_ast set_vother env ep al =
  let env_vars =
    let rec loop acc env_vars =
      match env_vars with
      | [] -> acc
      | (k, v) :: env_vars ->
          if List.mem_assoc k acc then loop acc env_vars
          else loop ((k, v) :: acc) env_vars
    in
    loop [] (conf.env @ conf.henv @ conf.senv)
  in
  List.iter
    (fun (k, v) ->
      let print_ast =
        print_ast (("binding", set_vother (Vbind (k, v))) :: env) ep
      in
      List.iter print_ast al)
    env_vars

let float_rgb_of_hsv h s v =
  let h = if h > 1. then 1. else if h < 0. then 0. else h in
  let s = if s > 1. then 1. else if s < 0. then 0. else s in
  let v = if v > 1. then 1. else if v < 0. then 0. else v in
  let h = if h = 1.0 then 0. else h in
  let h = h *. 6. in
  let i = truncate h in
  let f = h -. float i in
  let p = v *. (1. -. s) in
  let q = v *. (1. -. (s *. f)) in
  let t = v *. (1. -. (s *. (1. -. f))) in
  match i with
  | 0 -> (v, t, p)
  | 1 -> (q, v, p)
  | 2 -> (p, v, t)
  | 3 -> (p, q, v)
  | 4 -> (t, p, v)
  | 5 -> (v, p, q)
  | _ -> assert false

let rgb_of_hsv h s v =
  let r, g, b =
    float_rgb_of_hsv (float h /. 256.) (float s /. 256.) (float v /. 256.)
  in
  (truncate (r *. 256.), truncate (g *. 256.), truncate (b *. 256.))

let rgb_of_str_hsv h s v =
  let ios s = int_of_string s in
  rgb_of_hsv (ios h) (ios s) (ios v)

let eval_var conf ifun env ep loc sl =
  try
    match sl with
    | [ "env"; "key" ] -> (
        match ifun.get_vother (List.assoc "binding" env) with
        | Some (Vbind (k, _)) -> VVstring k
        | _ -> raise Not_found)
    | [ "env"; "val" ] -> (
        match ifun.get_vother (List.assoc "binding" env) with
        | Some (Vbind (_, v)) -> VVstring (v :> string)
        | _ -> raise Not_found)
    | [ "env"; "val"; "decoded" ] -> (
        match ifun.get_vother (List.assoc "binding" env) with
        | Some (Vbind (_, v)) -> VVstring (Mutil.decode v)
        | _ -> raise Not_found)
    | "today" :: sl ->
        TemplDate.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
    | [ "trace"; s ] ->
        Printf.eprintf "%s; " s;
        VVstring ""
    | [ "tracenl"; s ] ->
        Printf.eprintf "%s\n" s;
        VVstring ""
    | s :: sl -> (
        match (get_val ifun.get_vother s env, sl) with
        | Some (VVother f), sl -> f sl
        | Some v, [] -> v
        | _, sl -> ifun.eval_var env ep loc (s :: sl))
    | _ -> ifun.eval_var env ep loc sl
  with Not_found -> VVstring (eval_variable conf sl)

let print_foreach conf ifun print_ast eval_expr env ep loc s sl el al =
  try ifun.print_foreach print_ast eval_expr env ep loc s sl el al
  with Not_found ->
    templ_print_foreach conf print_ast ifun.set_vother env ep loc s sl el al

let print_wid_hei conf fname =
  match Image.size_from_path (Image.path_of_filename fname) with
  | Ok (wid, hei) -> Output.printf conf " width=\"%d\" height=\"%d\"" wid hei
  | Error () -> ()

(** Evaluates and prints content of {i cpr} template.
    If template wasn't found prints basic copyrigth HTML structure. *)
let print_copyright conf =
  Util.include_template conf [] "copyr" (fun () ->
      Output.print_sstring conf "<hr style=\"margin:0\">\n";
      Output.print_sstring conf "<div style=\"font-size: 80%\">\n";
      Output.print_sstring conf "<em>";
      Output.print_sstring conf "Copyright (c) 1998-2007 INRIA - GeneWeb ";
      Output.print_sstring conf Version.ver;
      Output.print_sstring conf "</em>";
      Output.print_sstring conf "</div>\n";
      Output.print_sstring conf "<br>\n")

let include_hed_trl conf name =
  if name = "trl" then Util.include_template conf [] name (fun () -> ());
  let query_time = Unix.gettimeofday () -. conf.query_start in
  Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
    !GWPARAM.errors_other !GWPARAM.set_vars

let rec interp_ast :
    config -> ('a, 'b) interp_fun -> 'a env -> 'b -> ast list -> unit =
 fun conf ifun env ->
  let m_env = ref env in
  let rec eval_ast env ep a = string_of_expr_val (eval_ast_expr env ep a)
  and eval_ast_list env ep = function
    | [] -> []
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in
        eval_ast_list env ep (Atext (loc, s) :: al)
    | a :: al -> eval_ast env ep a :: eval_ast_list env ep al
  and eval_ast_expr env ep = function
    | Atext (_, s) -> VVstring s
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
    let rec loop = function
      | [] -> []
      | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
          let s = squeeze_spaces s in
          loop (Atext (loc, s) :: al)
      | a :: al -> eval_ast_expr env ep a :: loop al
    in
    match loop v with
    | [ e ] -> e
    | el ->
        let sl = List.map string_of_expr_val el in
        VVstring (String.concat "" sl)
  and eval_expr env ep e =
    let eval_apply = eval_apply env ep in
    let eval_var = eval_var conf ifun env ep in
    templ_eval_expr conf (eval_var, eval_apply) e
  and eval_apply env ep loc f vl =
    match get_def ifun.get_vother f env with
    | Some (xl, al) ->
        let env, al =
          List.fold_right
            (fun a (env, al) ->
              let env, a = eval_subst loc f ifun.set_vother env xl vl a in
              (env, a :: al))
            al (env, [])
        in
        let sl = List.map (eval_ast env ep) al in
        String.concat "" sl
    | None -> (
        match (f, vl) with
        | "capitalize", [ VVstring s ] -> Utf8.capitalize_fst s
        | "interp", [ VVstring s ] ->
            let astl = Templ_parser.parse_templ conf (Lexing.from_string s) in
            String.concat "" (eval_ast_list env ep astl)
        | "language_name", [ VVstring s ] ->
            Translate.language_name s (Util.transl conf "!languages")
        | "url_set", [ VVstring s1 ] ->
            let s1 = String.split_on_char '/' s1 in
            url_set_aux conf s1 []
        | "url_set", [ VVstring s1; VVstring s2 ] ->
            let s1 = String.split_on_char '/' s1 in
            let s2 = String.split_on_char '/' s2 in
            url_set_aux conf s1 s2
        | "nth", [ VVstring s1; VVstring s2 ] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            Util.translate_eval (Util.nth_field s1 n)
        | "nth_0", [ VVstring s1; VVstring s2 ] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            if Util.nth_field s1 n = "" then "0" else Util.nth_field s1 n
        | "nth_c", [ VVstring s1; VVstring s2 ] -> (
            let n = try int_of_string s2 with Failure _ -> 0 in
            try Char.escaped (String.get s1 n) with Invalid_argument _ -> "")
        | "red_of_hsv", [ VVstring h; VVstring s; VVstring v ] -> (
            try
              let r, _, _ = rgb_of_str_hsv h s v in
              string_of_int r
            with Failure _ -> "red_of_hsv bad params")
        | "green_of_hsv", [ VVstring h; VVstring s; VVstring v ] -> (
            try
              let _, g, _ = rgb_of_str_hsv h s v in
              string_of_int g
            with Failure _ -> "green_of_hsv bad params")
        | "blue_of_hsv", [ VVstring h; VVstring s; VVstring v ] -> (
            try
              let _, _, b = rgb_of_str_hsv h s v in
              string_of_int b
            with Failure _ -> "blue_of_hsv bad params")
        | _ -> (
            try ifun.eval_predefined_apply env f vl
            with Not_found -> Printf.sprintf "%%apply;%s?" f))
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
      let v = eval_ast_expr_list new_env ep [ min ] in
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
        loop new_env
          (Aop2 (("", 0, 0), "+", min, Aint (("", 0, 0), "1")))
          max accu
      else accu
    in
    loop env min max ""
  in
  let rec print_ast env ep = function
    | Avar (loc, s, sl) ->
        print_var print_ast_list conf ifun env ep loc (s :: sl)
    | Awid_hei s -> print_wid_hei conf s
    | Aif (e, alt, ale) -> print_if env ep e alt ale
    | Aforeach ((loc, s, sl), el, al) -> (
        try print_foreach conf ifun print_ast eval_expr env ep loc s sl el al
        with Not_found ->
          Output.printf conf " %%foreach;%s?" (String.concat "." (s :: sl)))
    | Adefine (f, xl, al, alk) -> print_define env ep f xl al alk
    | Aapply (loc, f, ell) -> print_apply env ep loc f ell
    | Alet (k, v, al) -> print_let env ep k v al
    | Afor (i, min, max, al) -> print_for env ep i min max al
    | x -> Output.print_sstring conf (eval_ast env ep x)
  and print_ast_list env ep = function
    | [] -> m_env := env
    | Avar (_, "sq", []) :: Atext (loc, s) :: al ->
        let s = squeeze_spaces s in
        print_ast_list env ep (Atext (loc, s) :: al)
    | Ainclude (fname, astl) :: al ->
        Util.include_begin conf (Adef.safe fname);
        print_ast_list env ep astl;
        Util.include_end conf (Adef.safe fname);
        print_ast_list !m_env ep al
    | [ a ] -> print_ast env ep a
    | a :: al ->
        print_ast env ep a;
        print_ast_list env ep al
  and print_define env ep f xl al alk =
    let env = set_def ifun.set_vother f xl al env in
    print_ast_list env ep alk
  and print_apply env ep loc f ell =
    let vl = List.map (eval_ast_expr_list env ep) ell in
    match get_def ifun.get_vother f env with
    | Some (xl, al) ->
        templ_print_apply loc f ifun.set_vother print_ast env ep xl al vl
    | None -> Output.print_sstring conf (eval_apply env ep loc f vl)
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
      let v = eval_ast_expr_list new_env ep [ min ] in
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
        loop new_env (Aop2 (("", 0, 0), "+", min, Aint (("", 0, 0), "1"))) max
    in
    loop env min max
  in
  print_ast_list env

and print_var print_ast_list conf ifun env ep loc sl =
  let rec print_var1 eval_var sl =
    try
      match eval_var sl with
      | VVstring s -> Output.print_sstring conf s
      | VVbool true -> Output.print_sstring conf "1"
      | VVbool false -> Output.print_sstring conf "0"
      | VVother f -> print_var1 f []
    with Not_found -> (
      match sl with
      (* covers case of %include.file; *)
      | [ "include"; templ ] -> (
          match Util.open_etc_file conf templ with
          | Some (_, fname) -> (
              match input_templ conf templ with
              | Some astl ->
                  let () =
                    Templ_parser.(
                      included_files := (templ, astl) :: !included_files)
                  in
                  Util.include_begin conf (Adef.safe fname);
                  print_ast_list env ep astl;
                  Util.include_end conf (Adef.safe fname)
              | None ->
                  GWPARAM.errors_other :=
                    Format.sprintf "%%%s?" (String.concat "." sl)
                    :: !GWPARAM.errors_other;
                  Output.printf conf " %%%s?" (String.concat "." sl))
          | None ->
              GWPARAM.errors_other :=
                Format.sprintf "%%%s?" (String.concat "." sl)
                :: !GWPARAM.errors_other;
              Output.printf conf " %%%s?" (String.concat "." sl))
      | sl -> print_variable conf sl)
  in
  let eval_var = eval_var conf ifun env ep loc in
  print_var1 eval_var sl

and print_simple_variable conf = function
  | "base_header" -> include_hed_trl conf "hed"
  | "base_trailer" -> include_hed_trl conf "trl"
  | "body_prop" -> print_body_prop conf
  | "copyright" -> print_copyright conf
  | "number_of_bases" ->
      Output.print_sstring conf
        (string_of_int (List.length (Util.get_bases_list ())))
  | "bases_list" ->
      Output.print_sstring conf (String.concat ", " (Util.get_bases_list ()))
  | "bases_list_links" ->
      let format_link bname =
        Format.sprintf {|<a href="%s%s">%s</a>|}
          ((if conf.cgi then "?b=" else "") ^ bname)
          (if conf.lang = conf.default_lang then ""
          else (if conf.cgi then "&" else "?") ^ "lang=" ^ conf.lang)
          bname
      in
      Output.print_sstring conf
        (String.concat ", " (Util.get_bases_list ~format_fun:format_link ()))
  | "hidden" -> Util.hidden_env conf
  | "message_to_wizard" -> Util.message_to_wizard conf
  | _ -> raise Not_found

and print_variable conf sl =
  try Output.print_sstring conf (eval_variable conf sl)
  with Not_found -> (
    try
      match sl with
      | [ s ] -> print_simple_variable conf s
      | _ -> raise Not_found
    with Not_found ->
      GWPARAM.errors_undef :=
        Format.sprintf "%%%s?" (String.concat "." sl) :: !GWPARAM.errors_undef;
      Output.printf conf " %%%s?" (String.concat "." sl))

let copy_from_templ : config -> Adef.encoded_string env -> in_channel -> unit =
 fun conf env ic ->
  let astl = Templ_parser.parse_templ conf (Lexing.from_channel ic) in
  close_in ic;
  let ifun =
    {
      eval_var =
        (fun env _ _ -> function
          | [ s ] -> VVstring (List.assoc s env : Adef.encoded_string :> string)
          | _ -> raise Not_found);
      eval_transl = (fun _ -> eval_transl conf);
      eval_predefined_apply = (fun _ -> raise Not_found);
      get_vother = (fun _ -> None);
      set_vother = (fun _ -> Adef.encoded "");
      print_foreach = (fun _ -> raise Not_found);
    }
  in
  Templ_parser.wrap "" (fun () -> interp_ast conf ifun env () astl)

let _ = Util.copy_from_templ_ref := copy_from_templ
