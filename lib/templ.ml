open Config
open TemplAst

let commd ?excl ?trim conf = (Util.commd ?excl ?trim conf :> string)

exception Exc_located of loc * exn
exception BadApplyArity
exception NamedArgumentNotMatched of string

let raise_with_loc loc = function
  | Exc_located (_, _) as e -> raise e
  | e -> raise (Exc_located (loc, e))

let input_templ conf fname =
  match Util.open_etc_file fname with
  | None -> None
  | Some (ic, fname) ->
      Templ_parser.wrap fname (fun () ->
          let lex = Lexing.from_channel ic in
          let r = Templ_parser.parse_templ conf lex in
          close_in ic;
          Some r)

let sort_apply_parameters loc f_expr xl vl =
  let named_vl, unnamed_vl =
    List.partition (fun (id, v) -> Option.is_some id) vl
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
      Adefine
        ( sf f,
          List.map (fun (x, ast) -> (sf x, Option.map (subst sf) ast)) xl,
          substl sf al,
          substl sf alk )
  | Aapply (loc, f, all) ->
      Aapply
        ( loc,
          sf f,
          List.map (fun (x, asts) -> (Option.map sf x, substl sf asts)) all )
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

let rec eval_variable conf = function
  | [ "bvar"; v ] -> ( try List.assoc v conf.base_env with Not_found -> "")
  | [ "evar"; v; "ns" ] -> (
      try
        let vv = List.assoc v (conf.env @ conf.henv) in
        Mutil.gen_decode false vv |> esc
      with Not_found -> "")
  | [ "evar"; v ] -> (
      match Util.p_getenv (conf.env @ conf.henv) v with
      | Some vv -> esc vv
      | None -> "")
  | "time" :: sl -> eval_time_var conf sl
  | [ "user"; "ident" ] -> conf.user
  | [ "user"; "name" ] -> conf.username
  | [ s ] -> eval_simple_variable conf s
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
                  Printf.sprintf "<a href=\"%sm=CONN_WIZ\">%d</a>" (commd conf)
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
  | "dates_format" ->
      let string_of_dates_format = function
        | Config.DMY -> "day_month_year"
        | Config.MDY -> "month_day_year"
      in
      string_of_dates_format conf.dates_format
  | "doctype" -> (Util.doctype :> string)
  | "highlight" -> conf.highlight
  | "image_prefix" ->
      (let s =
         if conf.cgi then
           match List.assoc_opt "image_prefix" conf.base_env with
           | Some x -> Adef.escaped x
           | None -> Image.prefix conf
         else Image.prefix conf
       in
       s
        :> string)
  | "lang" -> conf.lang
  | "left" -> conf.left
  | "nl" -> "\n"
  | "nn" -> ""
  | "prefix" -> commd conf
  | "prefix_base" -> (Util.prefix_base conf :> string)
  | "prefix_base_password" -> (Util.prefix_base_password conf :> string)
  | "prefix_base_password_2" -> (Util.prefix_base_password_2 conf :> string)
  | "prefix_no_iz" -> commd ~excl:[ "iz"; "nz"; "pz"; "ocz" ] conf
  | "prefix_no_templ" -> commd ~excl:[ "templ" ] conf
  | "prefix_no_pmod" -> commd ~excl:[ "p_mod" ] conf
  | "prefix_no_wide" -> commd ~excl:[ "wide" ] conf
  | "prefix_no_lang" -> commd ~excl:[ "lang" ] conf
  | "prefix_no_all" -> commd ~excl:[ "templ"; "p_mod"; "wide" ] conf
  | "referer" -> (Util.get_referer conf :> string)
  | "right" -> conf.right
  | "setup_link" -> if conf.setup_link then " - " ^ setup_link conf else ""
  | "sp" -> " "
  | "static_path" ->
      (let s =
         if conf.cgi then
           match List.assoc_opt "static_path" conf.base_env with
           | Some x -> Adef.escaped x
           | None -> Adef.escaped conf.static_path
         else Adef.escaped ""
       in
       s
        :> string)
  | "suffix" ->
      (* On supprime de env toutes les paires qui sont dans (henv @ senv) *)
      let l =
        List.fold_left
          (fun accu (k, _) -> List.remove_assoc k accu)
          conf.env
          (List.rev_append conf.henv conf.senv)
      in
      List.fold_left
        (fun c (k, v) ->
          let v = Adef.as_string v in
          if ((k = "oc" || k = "ocz") && v = "0") || k = "" then c
          else c ^ k ^ "=" ^ v ^ "&")
        "" l
  | "url" ->
      let c :> string = Util.commd conf in
      (* On supprime de env toutes les paires qui sont dans (henv @ senv) *)
      let l :> (string * string) list =
        List.fold_left
          (fun accu (k, _) -> List.remove_assoc k accu)
          conf.env
          (List.rev_append conf.henv conf.senv)
      in
      List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ "&") c l
  | "version" -> Version.txt
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
    with Not_found -> VVstring (" %" ^ String.concat "." sl ^ "?"))

let eval_var_handled conf sl =
  try eval_variable conf sl
  with Not_found -> Printf.sprintf " %%%s?" (String.concat "." sl)

let apply_format conf nth s1 s2 =
  let transl_nth_format s =
    match nth with
    | Some n -> Util.ftransl_nth conf s n
    | None -> Util.ftransl conf s
  in
  match Util.check_format "%t" s1 with
  | Some s3 -> Printf.sprintf (transl_nth_format s3) (fun _ -> s2)
  | None -> (
      match Util.check_format "%s" s1 with
      | Some s3 -> Printf.sprintf (transl_nth_format s3) s2
      | None -> (
          match Util.check_format "%d" s1 with
          | Some s3 -> Printf.sprintf (transl_nth_format s3) (int_of_string s2)
          | None -> (
              match Util.check_format "%s%s" s1 with
              | Some s3 ->
                  let s21, s22 =
                    let i = String.index s2 ':' in
                    ( String.sub s2 0 i,
                      String.sub s2 (i + 1) (String.length s2 - i - 1) )
                  in
                  Printf.sprintf (transl_nth_format s3) s21 s22
              | None -> raise Not_found)))

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
  let r =
    let nth = try Some (int_of_string c) with Failure _ -> None in
    match split_at_coloncolon s with
    | None ->
        let s2 =
          match nth with
          | Some n -> Util.transl_nth conf s n
          | None -> Util.transl conf s
        in
        if c = "n" then s2 else Mutil.nominative s2
    | Some (s1, s2) -> (
        try
          if String.length s2 > 0 && s2.[0] = '|' then
            let i = 1 in
            let j = String.rindex s2 '|' in
            let k = skip_spaces_and_newlines s2 (j + 1) in
            let s3 =
              let s = String.sub s2 i (j - i) in
              let astl = Templ_parser.parse_templ conf (Lexing.from_string s) in
              List.fold_left (fun s a -> s ^ eval_ast conf a) "" astl
            in
            let s4 = String.sub s2 k (String.length s2 - k) in
            let s5 =
              match nth with
              | Some n -> Util.transl_nth conf s4 n
              | None -> Util.transl conf s4
            in
            let s2 = s3 ^ s5 in
            Util.transl_decline conf s1 s2
          else if String.length s2 > 0 && s2.[0] = ':' then
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
  let r = Util.translate_eval r in
  if upp then Utf8.capitalize_fst r else r

let nb_errors = ref 0

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
  | [ "false" ] -> VVbool false
  | [ "has_referer" ] ->
      (* deprecated since version 5.00 *)
      VVbool (Mutil.extract_param "referer: " '\n' conf.request <> "")
  | [ "just_friend_wizard" ] -> VVbool conf.just_friend_wizard
  | [ "friend" ] -> VVbool conf.friend
  | [ "manitou" ] -> VVbool conf.manitou
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
  | Aop1 (loc, op, e) -> (
      let v = eval_expr ceva e in
      match op with
      | "not" -> VVbool (not (bool_of e v))
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")))
  | Aop2 (loc, op, e1, e2) -> (
      let int e = int_of e (eval_expr ceva e) in
      let num e = num_of e (eval_expr ceva e) in
      let bool e = bool_of e (eval_expr ceva e) in
      let string e = string_of e (eval_expr ceva e) in
      match op with
      | "and" -> VVbool (if bool e1 then bool e2 else false)
      | "or" -> VVbool (if bool e1 then true else bool e2)
      | "is_substr" | "in" ->
          VVbool (Ext_string.contains (string e2) (string e1))
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
      | _ -> raise_with_loc loc (Failure ("op \"" ^ op ^ "\"")))
  | Aint (_, s) -> VVstring s
  | e -> raise_with_loc (loc_of_expr e) (Failure (not_impl "eval_expr" e))

let print_error ((fname, bp, ep) as pos) exc =
  incr nb_errors;
  if !nb_errors <= 10 then (
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
  | Vdef of (string * ast option) list * ast list
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
        let sdn = Date.to_sdn ~from:Dgregorian conf.today in
        TemplDate.eval_date_var conf sdn sl
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
      Output.print_sstring conf Version.txt;
      Output.print_sstring conf "</em>";
      Output.print_sstring conf "</div>\n";
      Output.print_sstring conf "<br>\n")

let include_hed_trl conf name =
  Util.include_template conf [] name (fun () -> ())

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
        let vl =
          List.map (fun (id, ast) -> (id, eval_ast_expr_list env ep ast)) all
        in
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
        | "interp", [ (None, VVstring s) ] ->
            let astl = Templ_parser.parse_templ conf (Lexing.from_string s) in
            String.concat "" (eval_ast_list env ep astl)
        | "language_name", [ (None, VVstring s) ] ->
            Translate.language_name s (Util.transl conf "!languages")
        | "nth", [ (None, VVstring s1); (None, VVstring s2) ] ->
            let n = try int_of_string s2 with Failure _ -> 0 in
            Util.translate_eval (Util.nth_field s1 n)
        | "nth_c", [ (None, VVstring s1); (None, VVstring s2) ] -> (
            let n = try int_of_string s2 with Failure _ -> 0 in
            try Char.escaped (String.get s1 n) with Invalid_argument _ -> "")
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
    let vl =
      List.map (fun (id, asts) -> (id, eval_ast_expr_list env ep asts)) ell
    in
    match get_def ifun.get_vother f env with
    | Some (xl, al) ->
      begin try
        let xl, vl =
          sort_apply_parameters loc
            (fun e -> VVstring (eval_expr env ep e))
            xl vl
        in
        templ_print_apply loc f ifun.set_vother print_ast env ep xl al vl
        with e -> print_error loc e
      end
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
      | [ "include"; templ ] -> (
          match Util.open_etc_file templ with
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
              | None -> Output.printf conf " %%%s?" (String.concat "." sl))
          | None -> Output.printf conf " %%%s?" (String.concat "." sl))
      | sl -> print_variable conf sl)
  in
  let eval_var = eval_var conf ifun env ep loc in
  print_var1 eval_var sl

and print_simple_variable conf = function
  | "base_header" -> include_hed_trl conf "hed"
  | "base_trailer" -> include_hed_trl conf "trl"
  | "body_prop" -> print_body_prop conf
  | "copyright" -> print_copyright conf
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
    with Not_found -> Output.printf conf " %%%s?" (String.concat "." sl))

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
