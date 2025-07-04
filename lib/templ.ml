module Logs = Geneweb_logs.Logs
module Sosa = Geneweb_sosa
module Parser = Geneweb_templ.Parser
module Ast = Geneweb_templ.Ast
module Loc = Geneweb_templ.Loc
module Driver = Geneweb_db.Driver

exception BadApplyArity
exception NamedArgumentNotMatched of string

type 'a expr_val =
  | VVbool of bool
  | VVstring of string
  | VVother of (string list -> 'a expr_val)

exception Exc_located of Loc.t * exn

let raise_with_loc loc = function
  | Exc_located (_, _) as e -> raise e
  | e ->
      incr GWPARAM.nb_errors;
      raise (Exc_located (loc, e))

let pp_exception ppf (e, bt) =
  let pp_header ppf () = Fmt.pf ppf "Uncaught exception in templates:" in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let lines =
    String.split_on_char '\n' @@ Printexc.raw_backtrace_to_string bt
  in
  match e with
  | Exc_located (loc, e) ->
      Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header () (Printexc.to_string e) Loc.pp loc
  | _ ->
      Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header () (Printexc.to_string e)
        Fmt.(list ~sep:cut string)
        lines

let on_exn e bt =
  incr GWPARAM.nb_errors;
  Logs.debug (fun k -> k "%a" pp_exception (e, bt))

let resolve_include conf loc fl =
  let r = Util.etc_file_name conf fl in
  Logs.debug (fun k -> k "%a: resolved %s into: %s" Loc.pp loc fl r);
  r

let parse conf fl =
  let fl = Util.etc_file_name conf fl in
  Parser.parse ~cached:!Logs.debug_flag ~on_exn
    ~resolve_include:(resolve_include conf) (`File fl)

let sort_apply_parameters loc f_expr xl vl =
  let named_vl, unnamed_vl =
    List.partition (fun (id, _v) -> Option.is_some id) vl
  in
  let named_vl = List.map (fun (id, ast) -> (Option.get id, ast)) named_vl in
  let unnamed_vl = List.map snd unnamed_vl in
  let vl, named_vl, unnamed_vl =
    List.fold_left
      (fun (vl, named_vl, unnamed_vl) (id, ast) ->
        match (List.assoc_opt id named_vl, ast) with
        | Some v, _ -> (v :: vl, List.remove_assoc id named_vl, unnamed_vl)
        | None, None when unnamed_vl <> [] ->
            let v = List.hd unnamed_vl in
            let unnamed_vl = List.tl unnamed_vl in
            (v :: vl, named_vl, unnamed_vl)
        | None, None -> raise_with_loc loc BadApplyArity
        | None, Some ast -> (f_expr ast :: vl, named_vl, unnamed_vl))
      ([], named_vl, unnamed_vl) xl
  in
  (match (named_vl, unnamed_vl) with
  | [], [] -> ()
  | (id, _) :: _, _ -> raise_with_loc loc (NamedArgumentNotMatched id)
  | _, _ :: _ -> raise_with_loc loc BadApplyArity);
  let xl = List.map fst xl in
  let vl = List.rev vl in
  (xl, vl)

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

let not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  "Templ." ^ func ^ ": not impl " ^ desc

let setup_link (conf : Config.config) =
  let s = Mutil.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    let s = "http://" ^ String.sub s 0 i ^ ":2316/" in
    "<a href=\"" ^ s ^ "gwsetup?v=main.htm\">gwsetup</a>"
  with Not_found -> ""

let esc s = (Util.escape_html s :> string)

let url_aux ?(pwd = true) (conf : Config.config) =
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

let reorder (conf : Config.config) url_env =
  let new_lang =
    match List.assoc "lang" url_env with exception Not_found -> "" | l -> l
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
            match List.assoc k url_env with exception Not_found -> "" | v -> v
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

let find_sosa_ref (conf : Config.config) =
  let env = conf.henv @ conf.senv @ conf.env in
  let get_env evar env =
    match List.assoc_opt evar env with Some s -> Adef.as_string s | None -> ""
  in
  let pz = get_env "pz" env in
  let nz = get_env "nz" env in
  let ocz = get_env "ocz" env in
  let iz = get_env "iz" env in
  (iz, pz, nz, ocz)

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
        Logs.syslog `LOG_WARNING "Empty Url\n";
        ""
    | s :: _l -> s
  in
  let conf_l = conf.henv @ conf.senv @ conf.env in
  let k_l = List.map (fun (k, _v) -> k) conf_l in

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
  let buffer = Buffer.create (min len (n * 4)) in
  let rec loop i count =
    if count = 0 || i >= len then Buffer.contents buffer
    else
      try
        let nbc = Utf8.nbc s.[i] in
        if i + nbc <= len then (
          let car = String.sub s i nbc in
          Buffer.add_string buffer car;
          loop (i + nbc) (count - 1))
        else Buffer.contents buffer
      with _ -> Buffer.contents buffer
  in
  loop 0 n

let rec eval_variable (conf : Config.config) = function
  | [ "base"; "name" ] -> conf.bname
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
        "" conf.base_env
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
  | "sosa_ref" :: sl -> eval_sosa_ref conf sl
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
  | [ "user"; "index" ] -> (
      match conf.user_iper with
      | Some ip -> Driver.Iper.to_string ip
      | None -> "")
  | [ "user"; "name" ] -> conf.username
  | [ "user"; "key" ] -> conf.userkey
  | [ s ] -> eval_simple_variable conf s
  | _ -> raise Not_found

and eval_sosa_ref conf sl =
  let i, fn, sn, occ = find_sosa_ref conf in
  match sl with
  | [ "first_name" ] -> fn
  | [ "surname" ] -> sn
  | [ "occ" ] -> occ
  | [ "index" ] -> i
  | [ "access" ] ->
      if i <> "" then "i=" ^ i
      else if fn <> "" || sn <> "" then
        Format.sprintf "p=%s&n=%s&oc=%s" fn sn occ
      else ""
  | [] ->
      let occ = if occ = "" then "" else "." ^ occ in
      Format.sprintf "%s%s %s" fn occ sn
  | _ -> "???1"

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
                   (Util.transl_nth conf
                      "wizard/wizards/friend/friends/exterior" 1)
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
  | "images_prefix" | "image_prefix" -> Util.images_prefix conf ^ "/"
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
      String.concat ", " l
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
  | "sosa_ref" -> (
      match find_sosa_ref conf with
      | iz, _, _, _ when iz <> "" -> iz
      | _, fn, sn, occ when fn <> "" || sn <> "" ->
          let occ = if occ = "" then "" else "." ^ occ in
          Format.sprintf "%s%s %s" fn occ sn
      | _ -> "???2")
  | "setup_link" -> if conf.setup_link then " - " ^ setup_link conf else ""
  | "sp" -> " "
  | "static_path" | "etc_prefix" ->
      let s =
        if conf.cgi then Adef.escaped conf.etc_prefix else Adef.escaped ""
      in
      let s = (s :> string) in
      if s = "" then s else s ^ "/"
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
      Printf.sprintf "Format error in %s\n" s1 |> Logs.syslog `LOG_WARNING;
      s1

let rec eval_ast conf Ast.{ desc; _ } =
  match desc with
  | Ast.Atext s -> s
  | Avar (s, sl) -> eval_var_handled conf (s :: sl)
  | Atransl (upp, s, c) -> eval_transl conf upp s c
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
             nominative expects a : in the string ! *)
        )
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
                  Parser.parse ~on_exn ~resolve_include:(resolve_include conf)
                    (`Raw s)
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

let templ_eval_var (conf : Config.config) = function
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
  | [ "is_welcome" ] -> VVbool !Util.is_welcome
  | [ "just_friend_wizard" ] -> VVbool conf.just_friend_wizard
  | [ "friend" ] -> VVbool conf.friend
  | [ "manitou" ] -> VVbool conf.manitou
  | [ "plugin"; plugin ] ->
      let plugins =
        try
          List.assoc "plugins" conf.base_env
          |> String.split_on_char ',' |> List.map String.trim
        with Not_found -> []
      in
      VVbool (List.mem plugin plugins)
  | [ "predictable_mode" ] -> VVbool conf.predictable_mode
  | [ "supervisor" ] -> VVbool conf.supervisor
  | [ "roglo" ] ->
      VVbool
        (try List.assoc "roglo" conf.base_env = "yes" with Not_found -> false)
  | [ "true" ] -> VVbool true
  | [ "wizard" ] -> VVbool conf.wizard
  | [ "is_printed_by_template" ] -> VVbool conf.is_printed_by_template
  | _ -> raise Not_found

let bool_of Ast.{ loc; _ } = function
  | VVbool b -> b
  | VVstring _ | VVother _ -> raise_with_loc loc (Failure "bool value expected")

let string_of Ast.{ loc; _ } = function
  | VVstring s -> s
  | VVbool _ | VVother _ -> raise_with_loc loc (Failure "string value expected")

let int_of Ast.{ loc; _ } = function
  | VVstring s -> (
      try int_of_string s
      with Failure _ ->
        raise_with_loc loc (Failure ("int value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ -> raise_with_loc loc (Failure "int value expected")

let float_of Ast.{ loc; _ } = function
  | VVstring s -> (
      try Float.of_string s
      with Failure _ ->
        raise_with_loc loc (Failure ("float value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ -> raise_with_loc loc (Failure "float value expected")

let num_of Ast.{ loc; _ } = function
  | VVstring s -> (
      try Sosa.of_string s
      with Failure _ ->
        raise_with_loc loc (Failure ("num value expected\nFound = " ^ s)))
  | VVbool _ | VVother _ -> raise_with_loc loc (Failure "num value expected")

let rec eval_expr ((conf, eval_var, eval_apply) as ceva) Ast.{ desc; loc } =
  match desc with
  | Atext s -> VVstring s
  | Avar (s, sl) -> (
      try eval_var loc (s :: sl)
      with Not_found -> (
        try templ_eval_var conf (s :: sl)
        with Not_found ->
          raise_with_loc loc
            (Failure ("unbound var: " ^ String.concat "." (s :: sl)))))
  | Atransl (upp, s, c) -> VVstring (eval_transl conf upp s c)
  | Aapply (s, ell) ->
      let vl =
        List.map
          (fun (id, el) ->
            ( id,
              match List.map (eval_expr ceva) el with
              | [ e ] -> e
              | el ->
                  let sl = List.map string_of_expr_val el in
                  VVstring (String.concat "" sl) ))
          ell
      in
      VVstring (eval_apply loc s vl)
  | Aop1 (op, e) -> (
      let v = eval_expr ceva e in
      match op with
      | "not" -> VVbool (not (bool_of e v))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")))
  | Aop2 (op, e1, e2) -> (
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
  | Aint s -> VVstring s
  | Apack al ->
      let vl = List.map (eval_expr ceva) al in
      let sl = List.map string_of_expr_val vl in
      VVstring (String.concat "" sl)
  | e -> raise_with_loc loc (Failure (not_impl "eval_expr" e))

let eval_bool_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    | VVbool b -> b
    | VVstring _ | VVother _ ->
        raise_with_loc e.Ast.loc (Failure "bool value expected")
  with Exc_located _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.debug (fun k -> k "%a" pp_exception (exn, bt));
    false

let eval_string_expr conf (eval_var, eval_apply) e =
  try
    match eval_expr (conf, eval_var, eval_apply) e with
    | VVstring s -> Util.translate_eval s
    | VVbool _ | VVother _ ->
        raise_with_loc e.Ast.loc (Failure "string value expected")
  with Exc_located _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.debug (fun k -> k "%a" pp_exception (exn, bt));
    ""

let print_body_prop (conf : Config.config) =
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\""
    with Not_found -> ""
  in
  Output.print_sstring conf (s ^ Util.body_prop conf)

type 'a vother =
  | Vdef of (string * Ast.t option) list * Ast.t list
  | Vval of 'a expr_val
  | Vbind of string * Adef.encoded_string

module Env = Map.Make (String)

type ('a, 'b) interp_fun = {
  eval_var : 'a Env.t -> 'b -> Loc.t -> string list -> 'b expr_val;
  eval_transl : 'a Env.t -> bool -> string -> string -> string;
  eval_predefined_apply : 'a Env.t -> string -> 'b expr_val list -> string;
  get_vother : 'a -> 'b vother option;
  set_vother : 'b vother -> 'a;
  print_foreach :
    ('a Env.t -> 'b -> Ast.t -> unit) ->
    ('a Env.t -> 'b -> Ast.t -> string) ->
    'a Env.t ->
    'b ->
    Loc.t ->
    string ->
    string list ->
    Ast.t list list ->
    Ast.t list ->
    unit;
}

let get_def get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (Env.find k env) with
    | Some (Vdef (al, el)) -> Some (al, el)
    | _ -> None
  with Not_found -> None

let get_val get_vother k env =
  let k = "#" ^ k in
  try
    match get_vother (Env.find k env) with Some (Vval x) -> Some x | _ -> None
  with Not_found -> None

let set_def set_vother k al el env =
  let k = "#" ^ k in
  Env.add k (set_vother (Vdef (al, el))) env

let set_val set_vother k v env =
  let k = "#" ^ k in
  Env.add k (set_vother (Vval v)) env

let eval_subst loc f set_vother env xl vl a =
  let rec loop env a xl vl =
    match (xl, vl) with
    | x :: xl, VVstring v :: vl -> loop env (Ast.subst (subst_text x v) a) xl vl
    | x :: xl, v :: vl ->
        let env = set_val set_vother x v env in
        loop env a xl vl
    | [], [] -> (env, a)
    | _ -> (env, Ast.mk_text ~loc (f ^ ": bad # of params"))
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
        | x :: xl, VVstring v :: vl ->
            loop env (Ast.subst (subst_text x v) a) xl vl
        | x :: xl, v :: vl -> loop (set_val set_vother x v env) a xl vl
        | [], [] -> (env, a)
        | _ ->
            ( env,
              Ast.mk_text ~loc
                (Printf.sprintf "%s: bad # of params (%d instead of %d)" f
                   (List.length gvl) (List.length gxl)) )
      in
      loop env a gxl gvl
    in
    print_ast env ep a
  in
  let rec loop = function
    | [] -> ()
    | Ast.{ desc = Avar ("sq", []); _ } :: { desc = Atext s; loc } :: al ->
        let s = squeeze_spaces s in
        loop (Ast.mk_text ~loc s :: al)
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

and print_foreach_env_binding (conf : Config.config) print_ast set_vother env ep
    al =
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
        let env = Env.add "binding" (set_vother (Vbind (k, v))) env in
        print_ast env ep
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

let rec eval_date_var conf jd = function
  | "french" :: sl -> eval_dmy_var (Calendar.french_of_sdn Sure jd) sl
  | "gregorian" :: sl -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl
  | "hebrew" :: sl -> eval_dmy_var (Calendar.hebrew_of_sdn Sure jd) sl
  | "julian" :: sl -> eval_dmy_var (Calendar.julian_of_sdn Sure jd) sl
  | [ "julian_day" ] -> VVstring (string_of_int jd)
  | [ "julian_day"; "sep1000" ] ->
      VVstring
        (Mutil.string_of_int_sep (Util.transl conf "(thousand separator)") jd)
  | [ "moon_age" ] -> (
      try
        let _, md = Calendar.moon_phase_of_sdn jd in
        VVstring (string_of_int md)
      with Failure _ -> VVstring "")
  | "moon_phase" :: sl -> (
      try
        let mp, _ = Calendar.moon_phase_of_sdn jd in
        eval_moon_phase_var mp sl
      with Failure _ -> VVstring "")
  | [ "week_day" ] ->
      let wday =
        let jd_today = Calendar.sdn_of_gregorian conf.today in
        let x = conf.today_wd - jd_today + jd in
        if x < 0 then 6 + ((x + 1) mod 7) else x mod 7
      in
      VVstring (string_of_int wday)
  | sl -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl

and eval_moon_phase_var mp = function
  | [ "hour" ] ->
      let s =
        match mp with None -> "" | Some (_, hh, _) -> Printf.sprintf "%02d" hh
      in
      VVstring s
  | [ "index" ] ->
      let i =
        match mp with
        | None -> 0
        | Some (Calendar.NewMoon, _, _) -> 1
        | Some (Calendar.FirstQuarter, _, _) -> 2
        | Some (Calendar.FullMoon, _, _) -> 3
        | Some (Calendar.LastQuarter, _, _) -> 4
      in
      VVstring (string_of_int i)
  | [ "minute" ] ->
      let s =
        match mp with None -> "" | Some (_, _, mm) -> Printf.sprintf "%02d" mm
      in
      VVstring s
  | _ -> raise Not_found

and eval_dmy_var dmy = function
  | [ "day" ] -> VVstring (string_of_int dmy.day)
  | [ "month" ] -> VVstring (string_of_int dmy.month)
  | "year" :: sl -> eval_integer dmy.year sl
  | [] -> VVstring (Printf.sprintf "%d-%02d-%02d" dmy.year dmy.month dmy.day)
  | _ -> raise Not_found

and eval_integer i = function
  | [ "roman" ] -> VVstring (Mutil.roman_of_arabian i)
  | [] -> VVstring (string_of_int i)
  | _ -> raise Not_found

let eval_var conf ifun env ep loc sl =
  try
    match sl with
    | [ "reorg" ] -> VVbool !GWPARAM.reorg
    | [ "env"; "key" ] -> (
        match ifun.get_vother (Env.find "binding" env) with
        | Some (Vbind (k, _)) -> VVstring k
        | _ -> raise Not_found)
    | [ "env"; "val" ] -> (
        match ifun.get_vother (Env.find "binding" env) with
        | Some (Vbind (_, v)) -> VVstring (v :> string)
        | _ -> raise Not_found)
    | [ "env"; "val"; "decoded" ] -> (
        match ifun.get_vother (Env.find "binding" env) with
        | Some (Vbind (_, v)) -> VVstring (Mutil.decode v)
        | _ -> raise Not_found)
    | "today" :: sl ->
        eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
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
  match Image.size_from_path (Image.path_of_filename conf fname) with
  | Ok (wid, hei) -> Output.printf conf " width=\"%d\" height=\"%d\"" wid hei
  | Error () -> ()

let rec eval conf ifun env =
  let m_env = ref env in
  let rec eval_ast env ep a = string_of_expr_val (eval_ast_expr env ep a)
  and eval_ast_list env ep = function
    | [] -> []
    | Ast.{ desc = Avar ("sq", []); _ } :: { desc = Atext s; loc } :: al ->
        let s = squeeze_spaces s in
        eval_ast_list env ep (Ast.mk_text ~loc s :: al)
    | a :: al -> eval_ast env ep a :: eval_ast_list env ep al
  and eval_ast_expr env ep (Ast.{ desc; loc } as a) =
    match desc with
    | Atext s -> VVstring s
    | Avar (s, sl) ->
        eval_string_var conf (eval_var conf ifun env ep loc) (s :: sl)
    | Atransl (upp, s, n) -> VVstring (ifun.eval_transl env upp s n)
    | Aif (e, alt, ale) -> VVstring (eval_if env ep e alt ale)
    | Aapply (f, all) ->
        let vl =
          List.map (fun (id, ast) -> (id, eval_ast_expr_list env ep ast)) all
        in
        VVstring (eval_apply env ep loc f vl)
    | Afor (i, min, max, al) -> VVstring (eval_for env ep i min max al)
    | _ -> VVstring (eval_expr env ep a)
  and eval_ast_expr_list env ep v =
    let rec loop = function
      | [] -> []
      | Ast.{ desc = Avar ("sq", []); _ } :: Ast.{ desc = Atext s; loc } :: al
        ->
          let s = squeeze_spaces s in
          loop (Ast.mk_text ~loc s :: al)
      | a :: al -> eval_ast_expr env ep a :: loop al
    in
    match loop v with
    | [ e ] -> e
    | el ->
        let sl = List.map string_of_expr_val el in
        VVstring (String.concat "" sl)
  and eval_expr env ep (e : Ast.t) =
    let eval_apply = eval_apply env ep in
    let eval_var = eval_var conf ifun env ep in
    templ_eval_expr conf (eval_var, eval_apply) e
  and eval_apply env ep loc f vl =
    match get_def ifun.get_vother f env with
    | Some (xl, al) ->
        let xl, vl =
          sort_apply_parameters loc
            (fun ast -> VVstring (eval_expr env ep ast))
            xl vl
        in

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
        | "capitalize", [ (None, VVstring s) ] -> Utf8.capitalize_fst s
        | "capitalize_words", [ (None, VVstring s) ] ->
            let wl = String.split_on_char ' ' s in
            (* TODO handle particles !
               let w = Util.surname_particle base w not available *)
            let wl = List.map (fun w -> Utf8.capitalize_fst w) wl in
            String.concat " " wl
        | "hash", [ (None, VVstring file) ] -> (
            let fpath =
              if conf.cgi then file else Util.resolve_asset_file conf file
            in
            match Util.hash_file_cached fpath with
            | Some hash -> hash
            | None -> "")
        | "interp", [ (None, VVstring s) ] ->
            let astl =
              Parser.parse ~on_exn ~resolve_include:(resolve_include conf)
                (`Raw s)
            in
            String.concat "" (eval_ast_list env ep astl)
        | "language_name", [ (None, VVstring s) ] ->
            Translate.language_name s (Util.transl conf "!languages")
        | "url_encode", [ (None, VVstring s) ]
        | "uri_encode", [ (None, VVstring s) ] ->
            Util.uri_encode s
        | "url_set", [ (None, VVstring s1) ] ->
            let s1 = String.split_on_char '/' s1 in
            url_set_aux conf s1 []
        | "url_set", [ (None, VVstring s1); (None, VVstring s2) ] ->
            let s1 = String.split_on_char '/' s1 in
            let s2 = String.split_on_char '/' s2 in
            url_set_aux conf s1 s2
        | "nth", [ (None, VVstring s1); (None, VVstring s2) ] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            Util.translate_eval (Util.nth_field s1 n)
        | "nth_0", [ (None, VVstring s1); (None, VVstring s2) ] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            if Util.nth_field s1 n = "" then "0" else Util.nth_field s1 n
        | "nth_c", [ (None, VVstring s1); (None, VVstring s2) ] -> (
            let n = try int_of_string s2 with Failure _ -> 0 in
            try Char.escaped (String.get s1 n) with Invalid_argument _ -> "")
        | "1000sep", [ (None, VVstring s) ] ->
            let n = try int_of_string s with Failure _ -> 0 in
            let sep = Util.transl conf "(thousand separator)" in
            string_of_expr_val (VVstring (Mutil.string_of_int_sep sep n))
        | ( "red_of_hsv",
            [ (None, VVstring h); (None, VVstring s); (None, VVstring v) ] )
          -> (
            try
              let r, _, _ = rgb_of_str_hsv h s v in
              string_of_int r
            with Failure _ -> "red_of_hsv bad params")
        | ( "green_of_hsv",
            [ (None, VVstring h); (None, VVstring s); (None, VVstring v) ] )
          -> (
            try
              let _, g, _ = rgb_of_str_hsv h s v in
              string_of_int g
            with Failure _ -> "green_of_hsv bad params")
        | ( "blue_of_hsv",
            [ (None, VVstring h); (None, VVstring s); (None, VVstring v) ] )
          -> (
            try
              let _, _, b = rgb_of_str_hsv h s v in
              string_of_int b
            with Failure _ -> "blue_of_hsv bad params")
        | _ -> (
            try ifun.eval_predefined_apply env f (List.map snd vl)
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
        let a = Ast.mk_op2 "+" min (Ast.mk_int "1") in
        loop new_env a max accu
      else accu
    in
    loop env min max ""
  in
  let rec print_ast env ep (Ast.{ desc; loc } as a) =
    match desc with
    | Avar (s, sl) -> print_var print_ast_list conf ifun env ep loc (s :: sl)
    | Awid_hei s -> print_wid_hei conf s
    | Aif (e, alt, ale) -> print_if env ep e alt ale
    | Aforeach ((s, sl), el, al) -> (
        try print_foreach conf ifun print_ast eval_expr env ep loc s sl el al
        with Not_found ->
          Output.printf conf " %%foreach;%s?" (String.concat "." (s :: sl)))
    | Adefine (f, xl, al, alk) -> print_define env ep f xl al alk
    | Aapply (f, ell) -> print_apply env ep loc f ell
    | Alet (k, v, al) -> print_let env ep k v al
    | Afor (i, min, max, al) -> print_for env ep i min max al
    | _ -> Output.print_sstring conf (eval_ast env ep a)
  and print_ast_list env ep = function
    | [] -> m_env := env
    | Ast.{ desc = Avar ("sq", []); _ } :: Ast.{ desc = Atext s; loc } :: al ->
        let s = squeeze_spaces s in
        print_ast_list env ep (Ast.mk_text ~loc s :: al)
    | Ast.{ desc = Ainclude _; _ } :: _ ->
        (* Excluded by [Geneweb_templ.Parser.parse]. *)
        assert false
    | Ast.{ desc = Apack l; _ } :: al ->
        print_ast_list env ep l;
        print_ast_list !m_env ep al
    | [ a ] -> print_ast env ep a
    | a :: al ->
        print_ast env ep a;
        print_ast_list env ep al
  and print_define env ep f xl al alk =
    let env = set_def ifun.set_vother f xl al env in
    print_ast_list env ep alk
  and print_apply env ep loc f ell =
    let vl =
      List.map (fun (id, asts) -> (id, eval_ast_expr_list env ep asts)) ell
    in
    match get_def ifun.get_vother f env with
    | Some (xl, al) ->
        let xl, vl =
          sort_apply_parameters loc
            (fun e -> VVstring (eval_expr env ep e))
            xl vl
        in
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
        let a = Ast.mk_op2 "+" min (Ast.mk_int "1") in
        loop new_env a max
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
          try
            let fl = Util.etc_file_name conf templ in
            let astl =
              Parser.parse ~cached:!Logs.debug_flag ~on_exn
                ~resolve_include:(resolve_include conf) (`File fl)
            in
            print_ast_list env ep astl
          with _ ->
            GWPARAM.errors_other :=
              Format.sprintf "%%%s?" (String.concat "." sl)
              :: !GWPARAM.errors_other;
            Output.printf conf " %%%s?" (String.concat "." sl))
      | sl -> print_variable conf sl)
  in
  let eval_var = eval_var conf ifun env ep loc in
  print_var1 eval_var sl

and print_simple_variable conf = function
  | "body_prop" -> print_body_prop conf
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
  | "query_time" ->
      (* FIXME: This variable have been introduced in order to display the
         query time on pages generated with the template engine. This is a
         workaround and we should refactor our approach to output trailers
         in order to remove it.

         See issue https://github.com/geneweb/geneweb/issues/2231 *)
      let query_time = Unix.gettimeofday () -. conf.query_start in
      Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
        !GWPARAM.errors_other !GWPARAM.set_vars
  | "src_images_list" ->
      let dir = !GWPARAM.images_d conf.bname in
      let f_list = Sys.readdir dir |> Array.to_list |> List.sort compare in
      let res =
        List.fold_left
          (fun acc f ->
            let full_path = Filename.concat dir f in
            if
              (Unix.stat full_path).st_kind = Unix.S_REG
              && f.[0] <> '.'
              && f.[0] <> '~'
            then acc ^ Format.sprintf "<option>%s\n" f
            else acc)
          "" f_list
      in
      Output.print_sstring conf res
  | _ -> raise Not_found

and print_variable conf sl =
  try Output.print_sstring conf (eval_variable conf sl)
  with Not_found -> (
    try
      match sl with
      | [ s ] -> print_simple_variable conf s
      | _ -> raise Not_found
    with Not_found -> Output.printf conf " %%%s?" (String.concat "." sl))

let output conf ifun env ep fl =
  (if Util.is_full_html_template conf fl then
     try Util.html conf with Failure _ -> ());
  try eval conf ifun env ep @@ parse conf fl
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.debug (fun k -> k "%a" pp_exception (e, bt))

type simple_env = Vstring of Adef.encoded_string | Vother of unit vother

let output_simple conf env fl =
  let get_vother = function Vother x -> Some x | _ -> None in
  let set_vother x = Vother x in
  let ifun =
    {
      eval_var =
        (fun env _ _ -> function
          | [ s ] -> (
              match Env.find s env with
              | Vstring s -> VVstring (s :> string)
              | _ -> raise Not_found)
          | _ -> raise Not_found);
      eval_transl = (fun _ -> eval_transl conf);
      eval_predefined_apply = (fun _ -> raise Not_found);
      get_vother;
      set_vother;
      print_foreach = (fun _ -> raise Not_found);
    }
  in
  output conf ifun env () fl
