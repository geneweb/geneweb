(* camlp4r ./pa_html.cmo *)
(* $Id: history.ml,v 4.28 2005-08-04 16:12:43 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Util;
open Gutil;
open TemplAst;

value file_name conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (Util.base_path [] bname) "history"
;

(* Record history when committing updates *)

value ext_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;

value gen_record conf base item action =
  let do_it =
    match p_getenv conf.base_env "history" with
    [ Some "yes" -> True
    | _ -> False ]
  in
  if do_it then
    let fname = file_name conf in
    match
      try Some (Secure.open_out_gen ext_flags 0o644 fname) with
      [ Sys_error _ -> None ]
    with
    [ Some oc ->
        let (hh, mm, ss) = conf.time in
        do {
          Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d [%s] %s %s\n"
            conf.today.year conf.today.month conf.today.day hh mm ss conf.user
            action item;
          close_out oc;
        }
    | None -> () ]
  else ()
;

value record conf base (fn, sn, occ) action =
  gen_record conf base (fn ^ "." ^ string_of_int occ ^ " " ^ sn) action
;

value record_notes conf base (num, file) action =
  let s =
    match num with
    [ Some num ->
        let s = string_of_int num in
        if file = "" then s else file ^ "/" ^ s
    | None -> file ]
  in
  gen_record conf base s action
;

(* Request for history printing *)

exception Begin_of_file;

value buff_get_rev len =
  let s = String.create len in
  do { for i = 0 to len - 1 do { s.[i] := Buff.buff.val.[len - 1 - i] }; s }
;

value rev_input_char ic (rbuff, rpos) pos =
  do {
    if rpos.val == 0 then do {
      if String.length rbuff.val < 65536 then
        let len =
          if rbuff.val = "" then 1024 else 2 * String.length rbuff.val
        in
        rbuff.val := String.create len
      else ();
      let ppos = max (pos - String.length rbuff.val) 0 in
      seek_in ic ppos;
      really_input ic rbuff.val 0 (pos - ppos);
      rpos.val := pos - ppos;
    }
    else ();
    decr rpos;
    rbuff.val.[rpos.val]
  }
;

value rev_input_line ic pos (rbuff, rpos) =
  if pos <= 0 then raise Begin_of_file
  else
    let rec loop len pos =
      if pos <= 0 then (buff_get_rev len, pos)
      else
        match rev_input_char ic (rbuff, rpos) pos with
        [ '\n' -> (buff_get_rev len, pos)
        | c -> loop (Buff.store len c) (pos - 1) ]
    in
    loop 0 (pos - 1)
;

value action_text conf =
  fun
  [ "ap" -> transl_decline conf "add" (transl_nth conf "person/persons" 0)
  | "mp" -> transl_decline conf "modify" (transl_nth conf "person/persons" 0)
  | "dp" -> transl_decline conf "delete" (transl_nth conf "person/persons" 0)
  | "fp" -> transl_decline conf "merge" (transl_nth conf "person/persons" 1)
  | "si" -> transl_decline conf "send" (transl_nth conf "image/images" 0)
  | "di" -> transl_decline conf "delete" (transl_nth conf "image/images" 0)
  | "af" -> transl_decline conf "add" (transl_nth conf "family/families" 0)
  | "mf" -> transl_decline conf "modify" (transl_nth conf "family/families" 0)
  | "df" -> transl_decline conf "delete" (transl_nth conf "family/families" 0)
  | "if" -> transl_decline conf "invert" (transl_nth conf "family/families" 1)
  | "ff" -> transl_decline conf "merge" (transl_nth conf "family/families" 1)
  | "cn" -> transl conf "change children's names"
  | "aa" -> transl_decline conf "add" (transl conf "parents")
  | "mn" -> transl_decline conf "modify" (transl_nth conf "note/notes" 1)
  | x -> x ]
;

value line_tpl = "0000-00-00 00:00:00 xx .";

value line_fields line =
  if String.length line > String.length line_tpl then
    let time = String.sub line 0 19 in
    let (user, i) =
      match (line.[20], Gutil.lindex line ']') with
      [ ('[', Some i) ->
          let user = String.sub line 21 (i - 21) in (user, i + 2)
      | _ -> ("", 20) ]
    in
    let action = String.sub line i 2 in
    let key =
      let i = i + 3 in
      if i >= String.length line then None
      else Some (String.sub line i (String.length line - i))
    in
    Some (time, user, action, key)
  else None
;

type hist_item =
  [ HI_notes of string and option int
  | HI_ind of person
  | HI_none ]
;

value print_history_line conf base line wiz k i =
  match line_fields line with
  [ Some (time, user, action, keyo) ->
      if wiz = "" || user = wiz then do {
        let hist_item =
          match keyo with
          [ Some key ->
              match action with
              [ "mn" ->
                  let (i, j) =
                    try let i = String.rindex key '/' in (i, i + 1) with
                    [ Not_found -> (0, 0) ]
                  in
                  let pg = String.sub key 0 i in
                  let s = String.sub key j (String.length key - j) in
                  try HI_notes pg (Some (int_of_string s)) with
                  [ Failure _ -> HI_notes key None ]
              | _ ->
                  match person_ht_find_all base key with
                  [ [ip] -> HI_ind (pget conf base ip)
                  | _ -> HI_none ] ]
          | None -> HI_none ]
        in
        let not_displayed =
          match hist_item with
          [ HI_ind p ->
              is_hidden p || (conf.hide_names && not (fast_auth_age conf p))
          | _ -> False ]
        in
        if not_displayed then i
        else do {
          tag "tr" begin
            tag "td" begin
              Wserver.wprint" <tt><b>*</b> %s</tt>\n" time;
              Wserver.wprint "(%s" (action_text conf action);
              if user <> "" then do {
                Wserver.wprint "\n<em>";
                if wiz = "" then
                  Wserver.wprint "- <a href=\"%sm=HIST;k=%d;wiz=%s\">"
                    (commd conf) k (Util.code_varenv user)
                else ();
                Wserver.wprint "%s" user;
                if wiz = "" then Wserver.wprint "</a>" else ();
                Wserver.wprint "</em>";
              }
              else ();
              Wserver.wprint ")";
              Wserver.wprint " ...\n";
              match keyo with
              [ Some key ->
                  match hist_item with
                  [ HI_ind p ->
                      do {
                        Wserver.wprint "<!--%s/%s/%d-->" (p_first_name base p)
                          (p_surname base p) p.occ;
                        Wserver.wprint "%s"
                          (referenced_person_title_text conf base p);
                        Wserver.wprint "%s" (Date.short_dates_text conf base p);
                      }
                  | HI_notes pg x ->
                      do {
                        Wserver.wprint "- ";
                        stag "a" "href=\"%sm=NOTES%s%s\"" (commd conf)
                          (if pg = "" then "" else ";f=" ^ pg)
                          (match x with
                           [ Some x -> ";v=" ^ string_of_int x
                           | None -> "" ])
                        begin
                          stag "i" begin
                            Wserver.wprint "%s"
                              (if pg = "" then transl_nth conf "note/notes" 1
                               else "[" ^ pg ^ "]");
                          end;
                        end;
                        match x with
                        [ Some x ->
                            do {
                              Wserver.wprint " - ";
                              stag "span" "style=\"font-size:50%%\"" begin
                                Wserver.wprint "#%d" x;
                              end;
                            }
                        | None -> () ];
                      }
                  | HI_none -> Wserver.wprint "%s" key ]
              | None -> Wserver.wprint "..." ];
            end;
          end;
          i + 1
        }
      }
      else i
  | None -> i ]
;

value print_history conf base ic =
  let k =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ -> 3 ]
  in
  let pos =
    match p_getint conf.env "pos" with
    [ Some x -> x
    | _ -> in_channel_length ic ]
  in
  let wiz =
    match p_getenv conf.env "wiz" with
    [ Some x ->
        match p_getenv conf.env "n" with
        [ Some "" | None -> x
        | _ -> "" ]
    | _ -> "" ]
  in
  let (pos, n) =
    let vv = (ref "", ref 0) in
    let rec loop pos i =
      if i >= k then (pos, i)
      else
        match
          try Some (rev_input_line ic pos vv) with [ Begin_of_file -> None ]
        with
        [ Some (line, pos) ->
            let i = print_history_line conf base line wiz k i in
            loop pos i
        | _ -> (pos, i) ]
    in
    do {
      Wserver.wprint "<table border=\"%d\" style=\"white-space:nowrap\">\n"
        conf.border;
      let r = loop pos 0 in
      Wserver.wprint "</table>\n";
      r;
    }
  in
  do {
    if pos > 0 then
      tag "form" "method=\"get\" action=\"%s\"" conf.command begin
        tag "p" begin
          Util.hidden_env conf;
          xtag "input" "type=\"hidden\" name=\"m\" value=\"HIST\"";
          xtag "input" "name=\"k\" size=\"3\" value=\"%d\"" k;
          xtag "input" "type=\"hidden\" name=\"pos\" value=\"%d\"" pos;
          if wiz <> "" then do {
            xtag "input" "type=\"hidden\" name=\"wiz\" value=\"%s\"" wiz;
            Wserver.wprint "(%s)\n" wiz;
          }
          else ();
          xtag "input" "type=\"submit\" value=\"&gt;&gt;\"";
          if wiz <> "" then
            xtag "input" "type=\"submit\" name=\"n\" value=\"&gt;&gt;\""
          else ();
        end;
      end
    else ();
  }
;

value print_old conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "history of updates"))
  in
  do {
    header_link_welcome conf title;
    let fname = file_name conf in
    match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
    [ Some ic -> do { print_history conf base ic; close_in ic; }
    | _ -> () ];
    trailer conf;
  }
;

(* *)

type env =
  [ Vinfo of string and string and string and hist_item and string
  | Vpos of ref int
  | Vfun of list string and list TemplAst.ast
  | Vval of string
  | Vnone ]
;

value get_env v env =
  try List.assoc v env with
  [ Not_found -> Vnone ]
;

value rec eval_var conf base env loc =
  fun
  [ ["evar"; v] ->
      match p_getenv (conf.env @ conf.henv) v with
      [ Some vv -> VVstring (quote_escaped vv)
      | None -> VVstring "" ]
  | ["first_name"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_ind p) _ -> VVstring (p_first_name base p)
      | _ -> VVstring "" ]
  | ["is_note"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_notes _ _) _ -> VVbool True
      | _ -> VVbool False ]
  | ["key"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ _ k -> VVstring k
      | _ -> raise Not_found ]
  | ["note"; "page"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_notes pg _) _ -> VVstring pg
      | _ -> raise Not_found ]
  | ["note"; "part"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_notes _ (Some x)) _ -> VVstring (string_of_int x)
      | Vinfo _ _ _ (HI_notes _ None) _ -> VVstring ""
      | _ -> raise Not_found ]
  | ["occ"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_ind p) _ -> VVstring (string_of_int p.occ)
      | _ -> VVstring "" ]
  | ["person" :: sl] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_ind p) _ -> eval_person_field_var conf base env p sl
      | _ -> raise Not_found ]
  | ["pos"] ->
      match get_env "pos" env with
      [ Vpos r -> VVstring (string_of_int r.val)
      | _ -> raise Not_found ]
  | ["surname"] ->
      match get_env "info" env with
      [ Vinfo _ _ _ (HI_ind p) _ -> VVstring (p_surname base p)
      | _ -> VVstring "" ]
  | ["time"] ->
      match get_env "info" env with
      [ Vinfo s _ _ _ _ -> VVstring s
      | _ -> raise Not_found ]
  | ["update"] ->
      match get_env "info" env with
      [ Vinfo _ u _ _ _ -> VVstring u
      | _ -> raise Not_found ]
  | ["user"] ->
      match get_env "info" env with
      [ Vinfo _ _ u _ _ -> VVstring u
      | _ -> raise Not_found ]
  | [v] ->
      match get_env v env with
      [ Vval s -> VVstring s
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
and eval_person_field_var conf base env p =
  fun
  [ ["access"] -> VVstring (Util.acces conf base p)
  | ["dates"] -> VVstring (Date.short_dates_text conf base p)
  | ["title"] -> VVstring (person_title conf base p)
  | [] -> VVstring (simple_person_text conf base p)
  | _ -> VVstring "person..." ]
and simple_person_text conf base p =
  match main_title base p with
  [ Some t -> titled_person_text conf base p t
  | None -> person_text conf base p ]
;

value rec eval_ast conf base env =
  fun
  [ x -> eval_expr conf base env x ]
and eval_expr conf base env e =
  let eval_ast = eval_ast conf base env in
  let eval_apply = eval_apply conf env eval_ast in
  let eval_var = eval_var conf base env in
  Templ.eval_expr conf (eval_var, eval_apply) e
and eval_int_expr conf base env e =
  let s = eval_expr conf base env e in
  try int_of_string s with [ Failure _ -> raise Not_found ]
and eval_apply conf env eval_ast f vl =
  match get_env f env with
  [ Vfun xl al ->
      let al = List.map (Templ.eval_subst f xl vl) al in
      let sl = List.map eval_ast al in
      String.concat "" sl
  | _ ->
      eval_predefined_apply conf env f vl ]
and eval_predefined_apply conf env f vl =
  match (f, vl) with
  [ _ -> Printf.sprintf " %%apply;%s?" f ]
;

value rec print_ast conf base env =
  fun
  [ Avar loc s sl ->
      Templ.print_var conf base (eval_var conf base env loc) s sl
  | Aif e alt ale -> print_if conf base env e alt ale
  | Aforeach (loc, s, sl) el al -> print_foreach conf base env loc s sl el al
  | Adefine f xl al alk -> print_define conf base env f xl al alk
  | Aapply loc f ell -> print_apply conf base env loc f ell
  | Alet k v al -> print_let conf base env k v al
  | x -> Wserver.wprint "%s" (eval_ast conf base env x) ]
and print_define conf base env f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env]) alk
and print_apply conf base env loc f ell =
  let eval_ast = eval_ast conf base env in
  let sll = List.map (List.map eval_ast) ell in
  let vl = List.map (String.concat "") sll in
  match get_env f env with
  [ Vfun xl al ->
      let print_ast = print_ast conf base env in
      Templ.print_apply f print_ast xl al vl
  | _ ->
      Wserver.wprint "%s" (eval_apply conf env eval_ast f vl) ]
and print_let conf base env k v al =
  let v = String.concat "" (List.map (eval_ast conf base env) v) in
  let env = [(k, Vval v) :: env] in
  List.iter (print_ast conf base env) al
and print_if conf base env e alt ale =
  let eval_var = eval_var conf base env in
  let eval_ast = eval_ast conf base env in
  let eval_apply = eval_apply conf env eval_ast in
  let al =
    if Templ.eval_bool_expr conf (eval_var, eval_apply) e then alt else ale
  in
  List.iter (print_ast conf base env) al
and print_foreach conf base env loc s sl el al =
  try
    match (s, sl) with
    [ ("history_line", []) -> print_foreach_history_line conf base env el al
    | (s, _) -> raise Not_found ]
  with
  [ Not_found -> Wserver.wprint " %%foreach;%s?" s ]
and print_foreach_history_line conf base env el al =
  match
    try Some (Secure.open_in_bin (file_name conf)) with [ Sys_error _ -> None ]
  with
  [ Some ic ->
      let (k, pos, wiz) =
        match el with
        [ [[e1]; [e2]; [e3]] ->
            let k = eval_int_expr conf base env e1 in
            let pos =
              try eval_int_expr conf base env e2 with
              [ Not_found -> in_channel_length ic ]
            in
            let wiz = eval_expr conf base env e3 in
            (k, pos, wiz)
        | [] -> (3, in_channel_length ic, "")
        | _ -> raise Not_found ]
      in
      let (pos, n) =
        let vv = (ref "", ref 0) in
        let rec loop pos i =
          if i >= k then (pos, i)
          else
            match
              try Some (rev_input_line ic pos vv) with [ Begin_of_file -> None ]
            with
            [ Some (line, pos) ->
                let i = print_history_line2 conf base env line wiz i al in
                loop pos i
            | None -> (pos, i) ]
        in
        loop pos 0
      in
      match get_env "pos" env with
      [ Vpos r -> r.val := pos
      | _ -> () ]
  | None -> () ]
and print_history_line2 conf base env line wiz i al =
  match line_fields line with
  [ Some (time, user, action, keyo) ->
      if wiz = "" || user = wiz then do {
        let hist_item =
          match keyo with
          [ Some key ->
              match action with
              [ "mn" ->
                  let (i, j) =
                    try let i = String.rindex key '/' in (i, i + 1) with
                    [ Not_found -> (0, 0) ]
                  in
                  let pg = String.sub key 0 i in
                  let s = String.sub key j (String.length key - j) in
                  try HI_notes pg (Some (int_of_string s)) with
                  [ Failure _ -> HI_notes key None ]
              | _ ->
                  match person_ht_find_all base key with
                  [ [ip] -> HI_ind (pget conf base ip)
                  | _ -> HI_none ] ]
          | None -> HI_none ]
        in
        let not_displayed =
          match hist_item with
          [ HI_ind p ->
              is_hidden p || (conf.hide_names && not (fast_auth_age conf p))
          | _ -> False ]
        in
        if not_displayed then i
        else do {
          let key = match keyo with [ Some s -> s | None -> "" ] in
          let env = [("info", Vinfo time action user hist_item key) :: env] in
          List.iter (print_ast conf base env) al;
          i + 1
        }
      }
      else i
  | None -> i ]
;

value interp_templ templ_fname conf base =
  let astl = Templ.input conf templ_fname in
  do {
    Util.html conf;
    Util.nl ();
    let env = [("pos", Vpos (ref 0))] in
    List.iter (print_ast conf base env) astl;
  }
;

value print conf base =
  if p_getenv conf.env "old" = Some "on" then print_old conf base else
  interp_templ "updhist" conf base
;
