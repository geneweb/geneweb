(* camlp5r pa_extend.cmo ./pa_html.cmo ./pa_lock.cmo *)
(* $Id: gwd.ml,v 5.61 2009-03-11 10:56:09 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Mutil;
open Printf;
open Util;

value green_color = "#2f6400";
value selected_addr = ref None;
value selected_port = ref 2317;
value redirected_addr = ref None;
value wizard_passwd = ref "";
value friend_passwd = ref "";
value wizard_just_friend = ref False;
value only_addresses = ref [];
value cgi = ref False;
value default_lang = ref "fr";
value setup_link = ref False;
value choose_browser_lang = ref False;
value images_dir = ref "";
value log_file = ref "";
value log_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;
IFDEF UNIX THEN
value max_clients = ref None
END;
value robot_xcl = ref None;
value auth_file = ref "";
value daemon = ref False;
value login_timeout = ref 1800;
value conn_timeout = ref 120;
value trace_failed_passwd = ref False;
value use_auth_digest_scheme = ref False;
value no_host_address = ref False;
value lexicon_list = ref [];

value log_oc () =
  if log_file.val <> "" then
    match
      try Some (open_out_gen log_flags 0o644 log_file.val) with
      [ Sys_error _ -> None ]
    with
    [ Some oc -> do {
        Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stderr;
        oc
      }
    | None -> do { log_file.val := ""; stderr } ]
  else stderr
;

value flush_log oc = if log_file.val <> "" then close_out oc else flush oc;

value is_multipart_form =
  let s = "multipart/form-data" in
  fun content_type ->
    let rec loop i =
      if i >= String.length content_type then False
      else if i >= String.length s then True
      else if content_type.[i] = Char.lowercase s.[i] then loop (i + 1)
      else False
    in
    loop 0
;

value extract_boundary content_type =
  let e = Util.create_env content_type in List.assoc "boundary" e
;

value fprintf_date oc tm =
  fprintf oc "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;

value print_and_cut_if_too_big oc str =
  loop 0 where rec loop i =
    if i < String.length str then do {
      output_char oc str.[i];
      let i =
        if i > 700 && String.length str - i > 750 then do {
          fprintf oc " ... "; String.length str - 700
        }
        else i + 1
      in
      loop i
    }
    else ()
;

value log oc tm conf from gauth request script_name contents =
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  do {
    let tm = Unix.localtime tm in
    fprintf_date oc tm;
    fprintf oc " (%d)" (Unix.getpid ());
    fprintf oc " %s?" script_name;
    print_and_cut_if_too_big oc contents;
    output_char oc '\n';
    fprintf oc "  From: %s\n" from;
    if gauth <> "" then fprintf oc "  User: %s\n" gauth else ();
    if conf.wizard && not conf.friend then
      fprintf oc "  User: %s%s(wizard)\n" conf.user
        (if conf.user = "" then "" else " ")
    else if conf.friend && not conf.wizard then
      fprintf oc "  User: %s%s(friend)\n" conf.user
        (if conf.user = "" then "" else " ")
    else ();
    if user_agent <> "" then fprintf oc "  Agent: %s\n" user_agent
    else ();
    if referer <> "" then do {
      fprintf oc "  Referer: ";
      print_and_cut_if_too_big oc referer;
      fprintf oc "\n"
    }
    else ();
  }
;

type auth_report =
  { ar_ok : bool; ar_command : string; ar_passwd : string;
    ar_scheme : auth_scheme_kind; ar_user : string; ar_name : string;
    ar_wizard : bool; ar_friend : bool; ar_uauth : string;
    ar_can_stale : bool }
;

value log_passwd_failed ar oc tm from request base_file = do {
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime tm in fprintf_date oc tm;
  fprintf oc " (%d)" (Unix.getpid ());
  fprintf oc " %s_%s" base_file ar.ar_passwd;
  fprintf oc " => failed (%s)" ar.ar_user;
  if trace_failed_passwd.val then
    fprintf oc " (%s)" (String.escaped ar.ar_uauth)
  else ();
  fprintf oc "\n";
  fprintf oc "  From: %s\n" from;
  fprintf oc "  Agent: %s\n" user_agent;
  if referer <> "" then fprintf oc "  Referer: %s\n" referer else ();
};

value copy_file fname =
  match Util.open_etc_file fname with
  [ Some ic ->
      do {
        try
          while True do { let c = input_char ic in Wserver.wprint "%c" c }
        with _ ->
          ();
        close_in ic;
        True
      }
  | None -> False ]
;

value http answer =
  do {
    Wserver.http answer;
    Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
  }
;

value robots_txt () =
  let oc = log_oc () in
  do {
    Printf.fprintf oc "Robot request\n";
    flush_log oc;
    Wserver.http "";
    Wserver.wprint "Content-type: text/plain"; Util.nl (); Util.nl ();
    if copy_file "robots" then ()
    else do {
      Wserver.wprint "User-Agent: *"; nl ();
      Wserver.wprint "Disallow: /"; nl ();
    }
  }
;

value refuse_log from cgi =
  let oc = Secure.open_out_gen log_flags 0o644 "refuse_log" in
  do {
    let tm = Unix.localtime (Unix.time ()) in
    fprintf_date oc tm;
    fprintf oc " excluded: %s\n" from;
    close_out oc;
    if not cgi then http "403 Forbidden" else ();
    Wserver.wprint "Content-type: text/html";
    Util.nl ();
    Util.nl ();
    Wserver.wprint "Your access has been disconnected by administrator.\n";
    let _ : bool = copy_file "refuse" in ();
  }
;

value only_log from cgi =
  let oc = log_oc () in
  do {
    let tm = Unix.localtime (Unix.time ()) in
    fprintf_date oc tm;
    fprintf oc " Connection refused from %s " from;
    fprintf oc "(only ";
    list_iter_first
      (fun first s -> fprintf oc "%s%s" (if not first then "," else "") s)
      only_addresses.val;
    fprintf oc ")\n";
    flush_log oc;
    if not cgi then http "" else ();
    Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
    Util.nl ();
    Util.nl ();
    Wserver.wprint "<head><title>Invalid access</title></head>\n";
    Wserver.wprint "<body><h1>Invalid access</h1></body>\n";
  }
;

value refuse_auth conf from auth auth_type =
  let oc = log_oc () in
  do {
    let tm = Unix.localtime (Unix.time ()) in
    fprintf_date oc tm;
    fprintf oc " Access failed\n";
    fprintf oc "  From: %s\n" from;
    fprintf oc "  Basic realm: %s\n" auth_type;
    fprintf oc "  Response: %s\n" auth;
    flush_log oc;
    Util.unauthorized conf auth_type;
  }
;

value index_from c s =
  loop where rec loop i =
    if i = String.length s then i else if s.[i] = c then i else loop (i + 1)
;

value index c s = index_from c s 0;

value rec extract_assoc key =
  fun
  [ [] -> ("", [])
  | [((k, v) as kv) :: kvl] ->
      if k = key then (v, kvl)
      else let (v, kvl) = extract_assoc key kvl in (v, [kv :: kvl]) ]
;

value input_lexicon lang =
  let ht = Hashtbl.create 501 in
  let fname = Filename.concat "lang" "lex_utf8.txt" in
  do {
    Mutil.input_lexicon lang ht
      (fun () -> Secure.open_in (Util.search_in_lang_path fname));
    ht
  }
;

value add_lexicon fname lang ht =
  let fname = Filename.concat "lang" fname in
  Mutil.input_lexicon lang ht
    (fun () -> Secure.open_in (Util.search_in_lang_path fname))
;

value alias_lang lang =
  if String.length lang < 2 then lang
  else
    let fname =
      Util.search_in_lang_path (Filename.concat "lang" "alias_lg.txt")
    in
    match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
    [ Some ic ->
        let lang =
          try
            let rec loop line =
              match Mutil.lindex line '=' with
              [ Some i ->
                  if lang = String.sub line 0 i then
                    String.sub line (i + 1) (String.length line - i - 1)
                  else loop (input_line ic)
              | None -> loop (input_line ic) ]
            in
            loop (input_line ic)
          with
          [ End_of_file -> lang ]
        in
        do { close_in ic; lang }
    | None -> lang ]
;

value rec cut_at_equal i s =
  if i = String.length s then (s, "")
  else if s.[i] = '=' then
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  else cut_at_equal (succ i) s
;

value strip_trailing_spaces s =
  let len =
    loop (String.length s) where rec loop len =
      if len = 0 then 0
      else
        match s.[len - 1] with
        [ ' ' | '\n' | '\r' | '\t' -> loop (len - 1)
        | _ -> len ]
  in
  String.sub s 0 len
;

value read_base_env cgi bname =
  let fname = Util.base_path [] (bname ^ ".gwf") in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let env =
        loop [] where rec loop env =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some s ->
              let s = strip_trailing_spaces s in
              if s = "" || s.[0] = '#' then loop env
              else loop [cut_at_equal 0 s :: env]
          | None -> env ]
      in
      do { close_in ic; env }
  | None -> [] ]
;

value print_renamed conf new_n =
  let link =
    let req = Util.get_request_string conf in
    let new_req =
      let len = String.length conf.bname in
      let rec loop i =
        if i > String.length req then ""
        else if i >= len && String.sub req (i - len) len = conf.bname then
          String.sub req 0 (i - len) ^ new_n ^
            String.sub req i (String.length req - i)
        else loop (i + 1)
      in
      loop 0
    in
    "http://" ^ Util.get_server_string conf ^ new_req
  in
  let env =
    [("old", conf.bname) ; ("new", new_n) ; ("link", link)]
  in
  match Util.open_etc_file "renamed" with
  [ Some ic ->
      do {
        Util.html conf;
        Util.nl ();
        Templ.copy_from_templ conf env ic;
      }
  | None ->
      let title _ = Wserver.wprint "%s -&gt; %s" conf.bname new_n in
      do {
        Hutil.header conf title;
        tag "ul" begin
          Util.html_li conf;
          tag "a" "href=\"%s\"" link begin Wserver.wprint "%s" link; end;
        end;
        Hutil.trailer conf;
      } ]
;

value log_redirect conf from request req =
  let referer = Wserver.extract_param "referer: " '\n' request in
  lock_wait Srcfile.adm_file "gwd.lck" with
  [ Accept ->
      let oc = log_oc () in
      do {
        let tm = Unix.localtime (Unix.time ()) in
        fprintf_date oc tm;
        fprintf oc " %s\n" req;
        fprintf oc "  From: %s\n" from;
        fprintf oc "  Referer: %s\n" referer;
        flush_log oc;
      }
  | Refuse -> () ]
;

value print_redirected conf from request new_addr =
  let req = Util.get_request_string conf in
  let link = "http://" ^ new_addr ^ req in
  let env = [("link", link)] in
  do {
    log_redirect conf from request req;
    match Util.open_etc_file "redirect" with
    [ Some ic ->
        do {
          Util.html conf;
          Util.nl ();
          Templ.copy_from_templ conf env ic;
        }
    | None ->
        let title _ = Wserver.wprint "Address changed" in
        do {
          Hutil.header conf title;
          Wserver.wprint "Use the following address:\n<p>\n";
          tag "ul" begin
            Util.html_li conf;
            stag "a" "href=\"%s\"" link begin Wserver.wprint "%s" link; end;
            Wserver.wprint "\n";
          end;
          Hutil.trailer conf;
        } ]
  }
;

value propose_base conf =
  let title _ = Wserver.wprint "Base" in
  do {
    Hutil.header conf title;
    tag "ul" begin
      Util.html_li conf;
      Wserver.wprint "<form method=\"get\" action=\"%s\">\n"
        conf.indep_command;
      Wserver.wprint "<input name=\"b\" size=\"40\"> =&gt;\n";
      Wserver.wprint "<input type=\"submit\" value=\"Ok\">\n";
    end;
    Hutil.trailer conf;
  }
;

value general_welcome conf =
  match Util.open_etc_file "index" with
  [ Some ic ->
      do {
        Util.html conf;
        Util.nl ();
        Templ.copy_from_templ conf [] ic;
      }
  | None -> propose_base conf ]
;

value nonce_private_key =
  Lazy.from_fun
    (fun () ->
       let cnt_dir = Filename.concat Util.cnt_dir.val "cnt" in
       let fname = Filename.concat cnt_dir "gwd_private.txt" in
       let k =
         match try Some (open_in fname) with [ Sys_error _ -> None ] with
         [ Some ic -> do {
             let s =
               try
                 loop (input_line ic) where rec loop s =
                   if s = "" || s.[0] = '#' then loop (input_line ic)
                   else s
               with
               [ End_of_file -> "" ]
             in
             close_in ic;
             s
           }
         | None -> "" ]
       in
       if k = "" then do {
         Random.self_init ();
         let k = Random.bits () in
         let oc = open_out fname in
         fprintf oc "\
# Gwd key for better password protection in communication.\n\
# Changing it makes all users receive their login window again.\n\
# Generated by program but can be modified by hand to any value.\n";
         fprintf oc "\n%d\n" k;
         close_out oc;
         string_of_int k
       }
       else k)
;
value digest_nonce tm = Lazy.force nonce_private_key;

value trace_auth base_env f = do {
  if List.mem_assoc "trace_auth" base_env then do {
    let oc =
      open_out_gen [Open_wronly; Open_append; Open_creat] 0o777
        "trace_auth.txt"
    in
    f oc;
    close_out oc
  }
  else ();
};

value unauth_server conf ar =  do {
  let typ = if ar.ar_passwd = "w" then "Wizard" else "Friend" in
  Wserver.wprint "HTTP/1.0 401 Unauthorized"; Util.nl ();
  if use_auth_digest_scheme.val then
    let nonce = digest_nonce conf.ctime in
let _ = let tm = Unix.localtime (Unix.time ()) in trace_auth conf.base_env (fun oc -> fprintf oc "\n401 unauthorized\n- date: %a\n- request:\n%t- passwd: %s\n- nonce: \"%s\"\n- can_stale: %b\n" fprintf_date tm (fun oc -> List.iter (fun s -> fprintf oc "  * %s\n" s) conf.request) ar.ar_passwd nonce ar.ar_can_stale) in
    Wserver.wprint
      "WWW-Authenticate: Digest realm=\"%s %s\"%s%s,qop=\"auth\"" typ
      conf.bname (if nonce = "" then "" else sprintf ",nonce=\"%s\"" nonce)
      (if ar.ar_can_stale then ",stale=true" else "")
  else
    Wserver.wprint "WWW-Authenticate: Basic realm=\"%s %s\"" typ conf.bname;
  Util.nl ();
  Util.nl ();
  let url =
    conf.bname ^ "?" ^
      List.fold_left
        (fun s (k, v) ->
           if s = "" then k ^ "=" ^ v else s ^ ";" ^ k ^ "=" ^ v)
        "" (conf.henv @ conf.senv @ conf.env)
  in
  let txt i = transl_nth conf "wizard/wizards/friend/friends/exterior" i in
  let typ = txt (if ar.ar_passwd = "w" then 0 else 2) in
  let title h =
    Wserver.wprint
      (fcapitale (ftransl conf "%s access cancelled for that page"))
      (if not h then "<em>" ^ typ ^ "</em>" else typ)
  in
  Hutil.header_without_http conf title;
  tag "h1" begin title False; end;
  tag "dl" begin
    let (alt_bind, alt_access) =
      if ar.ar_passwd = "w" then (";w=f", txt 2) else (";w=w", txt 0)
    in
    tag "dd" begin
      tag "ul" begin
        tag "li" begin
          Wserver.wprint "%s : <a href=\"%s%s\">%s</a>"
            (transl conf "access") url alt_bind alt_access;
        end;
        tag "li" begin
          Wserver.wprint "%s : <a href=\"%s\">%s</a>"
            (transl conf "access") url (txt 4);
        end;
      end;
    end;
  end;
  Hutil.trailer conf;
};

value gen_match_auth_file test_user_and_password auth_file =
  if auth_file = "" then None
  else
    let aul = read_gen_auth_file auth_file in
    loop aul where rec loop =
      fun
      [ [au :: aul] ->
          if test_user_and_password au then
            let s =
              try
                let i = String.index au.au_info ':' in
                String.sub au.au_info 0 i
              with
              [ Not_found -> "" ]
            in
            let username =
              try
                let i = String.index s '/' in
                let len = String.length s in
                String.sub s 0 i ^ String.sub s (i + 1) (len - i - 1)
              with
              [ Not_found -> s ]
            in
            Some username
          else loop aul
      | [] -> None ]
;

value basic_match_auth_file uauth =
  gen_match_auth_file (fun au -> au.au_user ^ ":" ^ au.au_passwd = uauth)
;

value digest_match_auth_file asch =
  gen_match_auth_file
    (fun au -> is_that_user_and_password asch au.au_user au.au_passwd)
;

value match_simple_passwd sauth uauth =
  match lindex sauth ':' with
  [ Some _ -> sauth = uauth
  | None ->
      match lindex uauth ':' with
      [ Some i ->
          sauth = String.sub uauth (i + 1) (String.length uauth - i - 1)
      | None -> sauth = uauth ] ]
;

value basic_match_auth passwd auth_file uauth =
  if passwd <> "" && match_simple_passwd passwd uauth then Some ""
  else basic_match_auth_file uauth auth_file
;

type access_type =
  [ ATwizard of string | ATfriend of string | ATnormal | ATnone | ATset ]
;

value compatible_tokens check_from (addr1, base1_pw1) (addr2, base2_pw2) =
  (not check_from || addr1 = addr2) && base1_pw1 = base2_pw2
;

value get_actlog check_from utm from_addr base_password =
  let fname = Srcfile.adm_file "actlog" in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let tmout = float_of_int login_timeout.val in
      let rec loop changed r list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let i = index ' ' line in
            let tm = float_of_string (String.sub line 0 i) in
            let islash = index_from '/' line (i + 1) in
            let ispace = index_from ' ' line (islash + 1) in
            let addr = String.sub line (i + 1) (islash - i - 1) in
            let db_pwd = String.sub line (islash + 1) (ispace - islash - 1) in
            let c = line.[ispace + 1] in
            let user =
              let k = ispace + 3 in
              if k >= String.length line then ""
              else String.sub line k (String.length line - k)
            in
            let (list, r, changed) =
              if utm -. tm >= tmout then (list, r, True)
              else if
                compatible_tokens check_from (addr, db_pwd)
                  (from_addr, base_password)
              then
                let r = if c = 'w' then ATwizard user else ATfriend user in
                ([((from_addr, db_pwd), (utm, c, user)) :: list], r, True)
              else ([((addr, db_pwd), (tm, c, user)) :: list], r, changed)
            in
            loop changed r list
        | None ->
            do {
              close_in ic;
              let list =
                List.sort
                  (fun (_, (t1, _, _)) (_, (t2, _, _)) -> compare t2 t1)
                  list
              in
              (list, r, changed)
            } ]
      in
      loop False ATnormal []
  | None -> ([], ATnormal, False) ]
;

value set_actlog list =
  let fname = Srcfile.adm_file "actlog" in
  match try Some (Secure.open_out fname) with [ Sys_error _ -> None ] with
  [ Some oc ->
      do {
        List.iter
          (fun ((from, base_pw), (a, c, d)) ->
             fprintf oc "%.0f %s/%s %c%s\n" a from base_pw c
               (if d = "" then "" else " " ^ d))
          list;
        close_out oc;
      }
  | None -> () ]
;

value get_token check_from utm from_addr base_password =
  lock_wait Srcfile.adm_file "gwd.lck" with
  [ Accept ->
      let (list, r, changed) =
        get_actlog check_from utm from_addr base_password
      in
      do { if changed then set_actlog list else (); r }
  | Refuse -> ATnormal ]
;

value mkpasswd () =
  loop 0 where rec loop len =
    if len = 9 then Buff.get len
    else
      let v = Char.code 'a' + Random.int 26 in
      loop (Buff.store len (Char.chr v))
;

value random_self_init () =
  let seed = int_of_float (mod_float (Unix.time ()) (float max_int)) in
  Random.init seed
;

value set_token utm from_addr base_file acc user =
  lock_wait Srcfile.adm_file "gwd.lck" with
  [ Accept ->
      do {
        random_self_init ();
        let (list, _, _) = get_actlog False utm "" "" in
        let (x, xx) =
          let base = base_file ^ "_" in
          let rec loop ntimes =
            if ntimes = 0 then failwith "set_token"
            else
              let x = mkpasswd () in
              let xx = base ^ x in
              if List.exists
                   (fun (tok, _) ->
                      compatible_tokens False tok (from_addr, xx))
                   list
              then
                loop (ntimes - 1)
              else (x, xx)
          in
          loop 50
        in
        let list = [((from_addr, xx), (utm, acc, user)) :: list] in
        set_actlog list;
        x
      }
  | Refuse -> "" ]
;

value index_not_name s =
  loop 0 where rec loop i =
    if i = String.length s then i
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> loop (i + 1)
      | _ -> i ]
;

value print_request_failure cgi msg =
  do {
    if not cgi then http "" else ();
    Wserver.wprint "Content-type: text/html";
    Util.nl (); Util.nl ();
    Wserver.wprint "<head><title>Request failure</title></head>\n";
    Wserver.wprint "\
<body bgcolor=\"white\">
<h1 style=\"text-align: center; color: red;\">Request failure</h1>
<p>The request could not be completed.</p>\n";
    Wserver.wprint "<p><em style=\"font-size: smaller;\">Internal message: %s</em></p>\n"
      msg;
    Wserver.wprint "</body>\n";
  }
;

value refresh_url cgi request s i =
  let url =
    let serv = "http://" ^ Util.get_server_string_aux cgi request in
    let req =
      let bname = String.sub s 0 i in
      let str = Util.get_request_string_aux cgi request in
      if cgi then
        let cginame = String.sub str 0 (String.index str '?') in
        cginame ^ "?b=" ^ bname
      else "/" ^ bname ^ "?"
    in
    serv ^ req
  in
  do {
    if not cgi then http "" else ();
    Wserver.wprint "Content-type: text/html";
    Util.nl ();
    Util.nl ();
    Wserver.wprint "\
<head>
<meta http-equiv=\"REFRESH\"
 content=\"1;URL=%s\">
</head>
<body>
<a href=\"%s\">%s</a>
</body>
" url url url;
    raise Exit
  }
;

value http_preferred_language request =
  let v = Wserver.extract_param "accept-language: " '\n' request in
  if v = "" then ""
  else
    let s = String.lowercase v in
    let list =
      loop [] 0 0 where rec loop list i len =
        if i = String.length s then List.rev [Buff.get len :: list]
        else if s.[i] = ',' then loop [Buff.get len :: list] (i + 1) 0
        else loop list (i + 1) (Buff.store len s.[i])
    in
    let list = List.map strip_spaces list in
    let rec loop =
      fun
      [ [lang :: list] ->
          if List.mem lang Version.available_languages then lang
          else if String.length lang = 5 then
            let blang = String.sub lang 0 2 in
            if List.mem blang Version.available_languages then blang
            else loop list
          else loop list
      | [] -> "" ]
    in
    loop list
;

value allowed_denied_titles key extra_line env base_env () =
  if p_getenv env "all_titles" = Some "on" then []
  else
    try
      let fname = List.assoc key base_env in
      if fname = "" then []
      else
        let ic = Secure.open_in (Filename.concat (Secure.base_dir ()) fname) in
        loop StrSet.empty where rec loop set =
          let (line, eof) =
            try (input_line ic, False) with [ End_of_file -> ("", True) ]
          in
          let set =
            let line = if eof then extra_line else line in
            if line = "" || line.[0] = ' ' || line.[0] = '#' then set
            else
              let line =
                match
                  try Some (String.index line '/') with
                  [ Not_found -> None ]
                with
                [ Some i ->
                    let len = String.length line in
                    let tit = String.sub line 0 i in
                    let pla = String.sub line (i + 1) (len - i - 1) in
                    (if tit = "*" then tit else Name.lower tit) ^ "/" ^
                    (if pla = "*" then pla else Name.lower pla)
                | None -> Name.lower line ]
              in
              StrSet.add line set
          in
          if eof then do {
            close_in ic;
            StrSet.elements set
          }
          else loop set
    with
    [ Not_found | Sys_error _ -> [] ]
;

value allowed_titles env =
  let extra_line =
    try List.assoc "extra_title" env with
    [ Not_found -> "" ]
  in
  allowed_denied_titles "allowed_titles_file" extra_line env
;

value denied_titles = allowed_denied_titles "denied_titles_file" "";

value start_with s i p =
  i + String.length p <= String.length s &&
  String.sub s i (String.length p) = p
;

value parse_digest s =
  let rec parse_main =
    parser
    [ [: s = ident; _ = spaces; kvl = key_eq_val_list :] ->
        if s = "Digest" then kvl else []
    | [: :] -> [] ]
  and ident =
    parser
    [ [: `('A'..'Z' | 'a'..'z' as c); len = ident_kont (Buff.store 0 c) :] ->
        Buff.get len ]
  and ident_kont len =
    parser
    [ [: `('A'..'Z' | 'a'..'z' as c); s :] -> ident_kont (Buff.store len c) s
    | [: :] -> len ]
  and spaces =
    parser
    [ [: `' '; a = spaces :] -> a
    | [: :] -> () ]
  and key_eq_val_list =
    parser
    [ [: kv = key_eq_val; kvl = key_eq_val_list_kont :] -> [kv :: kvl]
    | [: :] -> [] ]
  and key_eq_val_list_kont =
    parser
    [ [: `','; _ = spaces; kv = key_eq_val; kvl = key_eq_val_list_kont :] ->
        [kv :: kvl]
    | [: :] -> [] ]
  and key_eq_val =
    parser
    [ [: k = ident; `'='; v = val_or_str :] -> (k, v) ]
  and val_or_str =
    parser
    [ [: `'"'; v = string 0; _ = spaces :] -> v
    | [: v = any_val 0; _ = spaces :] -> v ]
  and string len =
    parser
    [ [: `'"' :] -> Buff.get len
    | [: `c; s :] -> string (Buff.store len c) s ]
  and any_val len =
    parser
    [ [: `('a'..'z' | 'A'..'Z' | '0'..'9' | '-' as c); s :] ->
        any_val (Buff.store len c) s
    | [: :] -> Buff.get len ]
  in
  parse_main (Stream.of_string s)
;

value basic_authorization cgi from_addr request base_env passwd access_type
    utm base_file command =
  let wizard_passwd =
    try List.assoc "wizard_passwd" base_env with
    [ Not_found -> wizard_passwd.val ]
  in
  let wizard_passwd_file =
    try List.assoc "wizard_passwd_file" base_env with [ Not_found -> "" ]
  in
  let friend_passwd =
    try List.assoc "friend_passwd" base_env with
    [ Not_found -> friend_passwd.val ]
  in
  let friend_passwd_file =
    try List.assoc "friend_passwd_file" base_env with [ Not_found -> "" ]
  in
  let passwd1 =
    let auth = Wserver.extract_param "authorization: " '\r' request in
    if auth = "" then ""
    else
      let s = "Basic " in
      if start_with auth 0 s then
        let i = String.length s in
        Base64.decode (String.sub auth i (String.length auth - i))
      else ""
  in
  let uauth = if passwd = "w" || passwd = "f" then passwd1 else passwd in
  let (ok, wizard, friend, username) =
    if not cgi && (passwd = "w" || passwd = "f") then
      if passwd = "w" then
        if wizard_passwd = "" && wizard_passwd_file = "" then
          (True, True, friend_passwd = "", "")
        else
          match basic_match_auth wizard_passwd wizard_passwd_file uauth with
          [ Some username -> (True, True, False, username)
          | None -> (False, False, False, "") ]
      else if passwd = "f" then
        if friend_passwd = "" && friend_passwd_file = "" then
          (True, False, True, "")
        else
          match basic_match_auth friend_passwd friend_passwd_file uauth with
          [ Some username -> (True, False, True, username)
          | None -> (False, False, False, "") ]
      else assert False
    else if wizard_passwd = "" && wizard_passwd_file = "" then
      (True, True, friend_passwd = "", "")
    else
       match basic_match_auth wizard_passwd wizard_passwd_file uauth with
       [ Some username -> (True, True, False, username)
       | _ ->
            if friend_passwd = "" && friend_passwd_file = "" then
              (True, False, True, "")
            else
              match
                basic_match_auth friend_passwd friend_passwd_file uauth
              with
              [ Some username -> (True, False, True, username)
              | None -> (True, False, False, "") ] ]
  in
  let user =
    match lindex uauth ':' with
    [ Some i ->
        let s = String.sub uauth 0 i in
        if s = wizard_passwd || s = friend_passwd then "" else s
    | None -> "" ]
  in
  let (command, passwd) =
    if access_type = ATset then
      if wizard then
        let pwd_id = set_token utm from_addr base_file 'w' user in
        if cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if friend then
        let pwd_id = set_token utm from_addr base_file 'f' user in
        if cgi then (command, pwd_id)
        else (base_file ^ "_" ^ pwd_id, "")
      else if cgi then (command, "")
      else (base_file, "")
    else
      if cgi then (command, passwd)
      else if passwd = "" then (base_file, "")
      else (base_file ^ "_" ^ passwd, passwd)
  in
  let auth_scheme =
    if not wizard && not friend then NoAuth
    else
      let realm =
        if wizard then "Wizard " ^ base_file else "Friend " ^ base_file
      in
        let (u, p) =
          match lindex passwd1 ':' with
          [ Some i ->
              let u = String.sub passwd1 0 i in
              let p =
                String.sub passwd1 (i + 1) (String.length passwd1 - i - 1)
              in
              (u, p)
          | None -> ("", passwd) ]
        in
        HttpAuth (Basic {bs_realm = realm; bs_user = u; bs_pass = p})
  in
  {ar_ok = ok; ar_command = command; ar_passwd = passwd;
   ar_scheme = auth_scheme; ar_user = user; ar_name = username;
   ar_wizard = wizard; ar_friend = friend; ar_uauth = uauth;
   ar_can_stale = False}
;

value bad_nonce_report command passwd_char =
  {ar_ok = False; ar_command = command; ar_passwd = passwd_char;
   ar_scheme = NoAuth; ar_user = ""; ar_name = "";
   ar_wizard = False; ar_friend = False; ar_uauth = "";
   ar_can_stale = True}
;

value test_passwd ds nonce command wf_passwd wf_passwd_file passwd_char wiz =
  let asch = HttpAuth (Digest ds) in
  if wf_passwd <> "" &&
     is_that_user_and_password asch ds.ds_username wf_passwd
  then
    if ds.ds_nonce <> nonce then bad_nonce_report command passwd_char
    else
      {ar_ok = True; ar_command = command ^ "_" ^ passwd_char;
       ar_passwd = passwd_char; ar_scheme = asch; ar_user = ds.ds_username;
       ar_name = ""; ar_wizard = wiz; ar_friend = not wiz; ar_uauth = "";
       ar_can_stale = False}
  else
    match digest_match_auth_file asch wf_passwd_file with
    [ Some username ->
        if ds.ds_nonce <> nonce then bad_nonce_report command passwd_char
        else
          {ar_ok = True; ar_command = command ^ "_" ^ passwd_char;
           ar_passwd = passwd_char; ar_scheme = asch;
           ar_user = ds.ds_username; ar_name = username;
           ar_wizard = wiz; ar_friend = not wiz; ar_uauth = "";
           ar_can_stale = False}
    | None ->
        {ar_ok = False; ar_command = command; ar_passwd = passwd_char;
         ar_scheme = asch; ar_user = ds.ds_username; ar_name = "";
         ar_wizard = False; ar_friend = False; ar_uauth = "";
         ar_can_stale = False} ]
;

value digest_authorization cgi request base_env passwd utm base_file command =
  let wizard_passwd =
    try List.assoc "wizard_passwd" base_env with
    [ Not_found -> wizard_passwd.val ]
  in
  let wizard_passwd_file =
    try List.assoc "wizard_passwd_file" base_env with [ Not_found -> "" ]
  in
  let friend_passwd =
    try List.assoc "friend_passwd" base_env with
    [ Not_found -> friend_passwd.val ]
  in
  let friend_passwd_file =
    try List.assoc "friend_passwd_file" base_env with [ Not_found -> "" ]
  in
  let command = if cgi then command else base_file in
  if wizard_passwd = "" && wizard_passwd_file = "" then
    {ar_ok = True; ar_command = command; ar_passwd = "";
     ar_scheme = NoAuth; ar_user = ""; ar_name = "";
     ar_wizard = True; ar_friend = friend_passwd = ""; ar_uauth = "";
     ar_can_stale = False}
  else if passwd = "w" || passwd = "f" then
    let auth = Wserver.extract_param "authorization: " '\r' request in
    if start_with auth 0 "Digest " then
      (* W3C - RFC 2617 - Jun 1999 *)
      let meth =
        match Wserver.extract_param "GET " ' ' request with
        [ "" -> "POST"
        | s -> "GET" ]
      in
let _ = trace_auth base_env (fun oc -> fprintf oc "\nauth = \"%s\"\n" auth) in
      let digenv = parse_digest auth in
      let get_digenv s = try List.assoc s digenv with [ Not_found -> "" ] in
      let ds =
        {ds_username = get_digenv "username"; ds_realm = get_digenv "realm";
         ds_nonce = get_digenv "nonce"; ds_meth = meth;
         ds_uri = get_digenv "uri"; ds_qop = get_digenv "qop";
         ds_nc = get_digenv "nc"; ds_cnonce = get_digenv "cnonce";
         ds_response = get_digenv "response"}
      in
      let nonce = digest_nonce utm in
let _ = trace_auth base_env (fun oc -> fprintf oc "\nanswer\n- date: %a\n- request:\n%t- passwd: %s\n- nonce: \"%s\"\n- meth: \"%s\"\n- uri: \"%s\"\n" fprintf_date (Unix.localtime utm) (fun oc -> List.iter (fun s -> fprintf oc "  * %s\n" s) request) passwd nonce ds.ds_meth ds.ds_uri) in
      if passwd = "w" then
        test_passwd ds nonce command wizard_passwd wizard_passwd_file "w"
          True
      else if passwd = "f" then
        test_passwd ds nonce command friend_passwd friend_passwd_file "f"
          False
      else
        failwith (sprintf "not impl (2) %s %s" auth meth)
    else
      {ar_ok = False; ar_command = command; ar_passwd = passwd;
       ar_scheme = NoAuth; ar_user = ""; ar_name = ""; ar_wizard = False;
       ar_friend = False; ar_uauth = ""; ar_can_stale = False}
  else
    let friend = friend_passwd = "" && friend_passwd_file = "" in
    {ar_ok = True; ar_command = command; ar_passwd = "";
     ar_scheme = NoAuth; ar_user = ""; ar_name = ""; ar_wizard = False;
     ar_friend = friend; ar_uauth = ""; ar_can_stale = False}
;

value authorization cgi from_addr request base_env passwd access_type utm
    base_file command =
  match access_type with
  [ ATwizard user ->
      let (command, passwd) =
        if cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme = TokenAuth {ts_user = user; ts_pass = passwd} in
      {ar_ok = True; ar_command = command; ar_passwd = passwd;
       ar_scheme = auth_scheme; ar_user = user; ar_name = "";
       ar_wizard = True; ar_friend = False; ar_uauth = "";
       ar_can_stale = False}
  | ATfriend user ->
      let (command, passwd) =
        if cgi then (command, passwd)
        else if passwd = "" then (base_file, "")
        else (base_file ^ "_" ^ passwd, passwd)
      in
      let auth_scheme = TokenAuth {ts_user = user; ts_pass = passwd} in
      {ar_ok = True; ar_command = command; ar_passwd = passwd;
       ar_scheme = auth_scheme; ar_user = user; ar_name = "";
       ar_wizard = False; ar_friend = True; ar_uauth = "";
       ar_can_stale = False}
  | ATnormal ->
      let (command, passwd) =
        if cgi then (command, "") else (base_file, "")
      in
      {ar_ok = True; ar_command = command; ar_passwd = passwd;
       ar_scheme = NoAuth; ar_user = ""; ar_name = ""; ar_wizard = False;
       ar_friend = False; ar_uauth = ""; ar_can_stale = False}
  | ATnone | ATset ->
      if use_auth_digest_scheme.val then
        digest_authorization cgi request base_env passwd utm base_file command
      else
        basic_authorization cgi from_addr request base_env passwd access_type
          utm base_file command ]
;

value make_conf cgi from_addr (addr, request) script_name contents env = do {
  let utm = Unix.time () in
  let tm = Unix.localtime utm in
  let (command, base_file, passwd, env, access_type) =
    let (base_passwd, env) =
      let (x, env) = extract_assoc "b" env in
      if x <> "" || cgi then (x, env) else (script_name, env)
    in
    let ip = index '_' base_passwd in
    let base_file =
      let s = String.sub base_passwd 0 ip in
      let s =
        if Filename.check_suffix s ".gwb" then Filename.chop_suffix s ".gwb"
        else s
      in
      let i = index_not_name s in
      if i = String.length s then s
      else refresh_url cgi request s i
    in
    let (passwd, env, access_type) =
      let has_passwd = List.mem_assoc "w" env in
      let (x, env) = extract_assoc "w" env in
      if has_passwd then
        (x, env, if x = "w" || x = "f" || x = "" then ATnone else ATset)
      else
        let passwd =
          if ip = String.length base_passwd then ""
          else
            String.sub base_passwd (ip + 1)
              (String.length base_passwd - ip - 1)
        in
        let access_type =
          match passwd with
          [ "" | "w" | "f" -> ATnone
          | _ -> get_token True utm from_addr base_passwd ]
        in
        (passwd, env, access_type)
    in
    let passwd = Util.decode_varenv passwd in
    let command = script_name in
    (command, base_file, passwd, env, access_type)
  in
  let (lang, env) = extract_assoc "lang" env in
  let lang =
    if lang = "" && choose_browser_lang.val then
      http_preferred_language request
    else lang
  in
  let lang = alias_lang lang in
  let (from, env) =
    match extract_assoc "opt" env with
    [ ("from", env) -> ("from", env)
    | ("", env) -> ("", env)
    | (x, env) -> ("", [("opt", x) :: env]) ]
  in
  let (threshold_test, env) = extract_assoc "threshold" env in
  if threshold_test <> "" then
    RelationLink.threshold.val := int_of_string threshold_test
  else ();
  let (sleep, env) =
    let (x, env) = extract_assoc "sleep" env in
    (if x = "" then 0 else int_of_string x, env)
  in
  let base_env = read_base_env cgi base_file in
  let default_lang =
    try
      let x = List.assoc "default_lang" base_env in
      if x = "" then default_lang.val else x
    with
    [ Not_found -> default_lang.val ]
  in
  let lexicon = input_lexicon (if lang = "" then default_lang else lang) in
  List.iter
    (fun fname ->
      add_lexicon fname (if lang = "" then default_lang else lang) lexicon)
    lexicon_list.val;
  (* A l'initialisation de la config, il n'y a pas de sosa_ref. *)
  (* Il sera mis à jour par effet de bord dans request.ml       *)
  let default_sosa_ref = (Adef.iper_of_int (-1), None) in
  let ar =
    authorization cgi from_addr request base_env passwd access_type utm
      base_file command
  in
  let wizard_just_friend =
    if wizard_just_friend.val then True
    else
      try List.assoc "wizard_just_friend" base_env = "yes" with
      [ Not_found -> False ]
  in
  let is_rtl =
    try Hashtbl.find lexicon " !dir" = "rtl" with [ Not_found -> False ]
  in
  let manitou =
    try
      ar.ar_wizard && ar.ar_user <> "" &&
      p_getenv env "manitou" <> Some "off" &&
      List.assoc "manitou" base_env = ar.ar_user
    with
    [ Not_found -> False ]
  in
  let supervisor =
    try
      ar.ar_wizard && ar.ar_user <> "" &&
      List.assoc "supervisor" base_env = ar.ar_user
    with
    [ Not_found -> False ]
  in
  let wizard_just_friend = if manitou then False else wizard_just_friend in
  let conf =
    {from = from_addr;
     manitou = manitou;
     supervisor = supervisor;
     wizard = ar.ar_wizard && not wizard_just_friend;
     friend = ar.ar_friend || wizard_just_friend && ar.ar_wizard;
     just_friend_wizard = ar.ar_wizard && wizard_just_friend;
     user = ar.ar_user; username = ar.ar_name;
     auth_scheme = ar.ar_scheme; cgi = cgi; command = ar.ar_command;
     indep_command = (if cgi then ar.ar_command else "geneweb") ^ "?";
     pure_xhtml =
       try List.assoc "pure_xhtml" env = "on" with
       [ Not_found -> False ];
     highlight =
       try List.assoc "highlight_color" base_env with
       [ Not_found -> green_color ];
     lang = if lang = "" then default_lang else lang;
     default_lang = default_lang;
     default_sosa_ref = default_sosa_ref;
     multi_parents =
       try List.assoc "multi_parents" base_env = "yes" with
       [ Not_found -> False ];
     can_send_image =
       try List.assoc "can_send_image" base_env <> "no" with
       [ Not_found -> True ];
     authorized_wizards_notes =
       try List.assoc "authorized_wizards_notes" base_env = "yes" with
       [ Not_found -> False ];
     public_if_titles =
       try List.assoc "public_if_titles" base_env = "yes" with
       [ Not_found -> False ];
     public_if_no_date =
       try List.assoc "public_if_no_date" base_env = "yes" with
       [ Not_found -> False ];
     cancel_links =
       match Util.p_getenv env "cgl" with
       [ Some "on" -> True
       | _ -> False ];
     setup_link = setup_link.val;
     access_by_key =
       try List.assoc "access_by_key" base_env = "yes" with
       [ Not_found -> ar.ar_wizard && ar.ar_friend ];
     private_years =
       try int_of_string (List.assoc "private_years" base_env) with
       [ Not_found | Failure _ -> 150 ];
     hide_names =
       if ar.ar_wizard || ar.ar_friend then False
       else
         try List.assoc "hide_private_names" base_env = "yes" with
         [ Not_found -> False ];
     use_restrict =
       if ar.ar_wizard || ar.ar_friend then False
       else
         try List.assoc "use_restrict" base_env = "yes" with
         [ Not_found -> False ];
     no_image =
       if ar.ar_wizard || ar.ar_friend then False
       else
         try List.assoc "no_image_for_visitor" base_env = "yes" with
         [ Not_found -> False ];
     no_note =
       if ar.ar_wizard || ar.ar_friend then False
       else
         try List.assoc "no_note_for_visitor" base_env = "yes" with
         [ Not_found -> False ];
     bname = base_file; env = env; senv = [];
     henv =
       (if not cgi then []
        else if ar.ar_passwd = "" then [("b", base_file)]
        else [("b", base_file ^ "_" ^ ar.ar_passwd)]) @
         (if lang = "" then [] else [("lang", lang)]) @
         (if from = "" then [] else [("opt", from)]);
     base_env = base_env;
     allowed_titles = Lazy.from_fun (allowed_titles env base_env);
     denied_titles = Lazy.from_fun (denied_titles env base_env);
     request = request; lexicon = lexicon;
     xhs =
       match p_getenv base_env "doctype" with
       [ Some "html-4.01" -> ""
       | _ -> " /" ];
     charset = "utf-8";
     is_rtl = is_rtl;
     left = if is_rtl then "right" else "left";
     right = if is_rtl then "left" else "right";
     auth_file =
       try
         let x = List.assoc "auth_file" base_env in
         if x = "" then auth_file.val else Util.base_path [] x
       with
       [ Not_found -> auth_file.val ];
     border =
       match Util.p_getint env "border" with
       [ Some i -> i
       | None -> 0 ];
     n_connect = None;
     today =
       {day = tm.Unix.tm_mday; month = succ tm.Unix.tm_mon;
        year = tm.Unix.tm_year + 1900; prec = Sure; delta = 0};
     today_wd = tm.Unix.tm_wday;
     time = (tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec);
     ctime = utm}
  in
  (conf, sleep, ar)
};

value log_and_robot_check conf auth from request script_name contents =
  if conf.cgi && log_file.val = "" && robot_xcl.val = None then ()
  else
    let tm = Unix.time () in
    lock_wait Srcfile.adm_file "gwd.lck" with
    [ Accept ->
        let oc = log_oc () in
        do {
          try
            do {
              match robot_xcl.val with
              [ Some (cnt, sec) ->
                  let s = "suicide" in
                  let suicide = Util.p_getenv conf.env s <> None in
                  conf.n_connect :=
                    Some (Robot.check oc tm from cnt sec conf suicide)
              | _ -> () ];
              if conf.cgi && log_file.val = "" then ()
              else log oc tm conf from auth request script_name contents;
            }
          with e ->
            do { flush_log oc; raise e };
          flush_log oc;
        }
    | Refuse -> () ]
;

value is_robot from =
  lock_wait Srcfile.adm_file "gwd.lck" with
  [ Accept ->
      let (robxcl, _) = Robot.robot_excl () in
      try let _ = List.assoc from robxcl.Robot.excl in True with
      [ Not_found -> False ]
  | Refuse -> False ]
;

value auth_err request auth_file =
  if auth_file = "" then (False, "")
  else
    let auth = Wserver.extract_param "authorization: " '\r' request in
    if auth <> "" then
      match
        try Some (Secure.open_in auth_file) with [ Sys_error _ -> None ]
      with
      [ Some ic ->
          let auth =
            let i = String.length "Basic " in
            Base64.decode (String.sub auth i (String.length auth - i))
          in
          try
            let rec loop () =
              if auth = input_line ic then do {
                close_in ic;
                let s =
                  try
                    let i = String.rindex auth ':' in String.sub auth 0 i
                  with
                  [ Not_found -> "..." ]
                in
                (False, s)
              }
              else loop ()
            in
            loop ()
          with
          [ End_of_file -> do { close_in ic; (True, auth) } ]
      | _ -> (True, "(auth file '" ^ auth_file ^ "' not found)") ]
    else (True, "(authorization not provided)")
;

value no_access conf =
  let title _ = Wserver.wprint "Error" in
  do {
    Hutil.rheader conf title;
    Wserver.wprint "No access to this database in CGI mode\n";
    Hutil.trailer conf;
  }
;

value conf_and_connection cgi from (addr, request) script_name contents env =
  let (conf, sleep, passwd_err) =
    make_conf cgi from (addr, request) script_name contents env
  in
  match redirected_addr.val with
  [ Some addr -> print_redirected conf from request addr
  | None ->
      let (auth_err, auth) =
        if conf.auth_file = "" then (False, "")
        else if cgi then (True, "")
        else auth_err request conf.auth_file
      in
      let mode = Util.p_getenv conf.env "m" in
      do {
        if mode <> Some "IM" then
          let contents =
            if List.mem_assoc "log_pwd" env then "..." else contents
          in
          log_and_robot_check conf auth from request script_name contents
        else ();
        match (cgi, auth_err, passwd_err) with
        [ (True, True, _) ->
            if is_robot from then Robot.robot_error conf from 0 0
            else no_access conf
        | (_, True, _) ->
            if is_robot from then Robot.robot_error conf from 0 0
            else
              let auth_type =
                let x =
                  try List.assoc "auth_file" conf.base_env with
                  [ Not_found -> "" ]
                in
                if x = "" then "GeneWeb service" else "database " ^ conf.bname
              in
              refuse_auth conf from auth auth_type
        | (_, _, ({ar_ok = False} as ar)) ->
            if is_robot from then Robot.robot_error conf from 0 0
            else do {
              let tm = Unix.time () in
              lock_wait Srcfile.adm_file "gwd.lck" with
              [ Accept -> do {
                  let oc = log_oc () in
                  log_passwd_failed ar oc tm from request conf.bname;
                  flush_log oc;
                }
              | Refuse -> () ];
              unauth_server conf ar;
            }
        | _ ->
            match mode with
            [ Some "DOC" -> Doc.print conf
            | _ ->
               if conf.bname = "" then general_welcome conf
               else
                 match
                   try Some (List.assoc "renamed" conf.base_env) with
                   [ Not_found -> None ]
                 with
                 [ Some n when n <> "" -> print_renamed conf n
                 | _ ->
                     do {
                       Request.treat_request_on_base conf
                         (log_file.val, log_oc, flush_log);
                       if conf.manitou && sleep > 0 then Unix.sleep sleep
                       else ();
                     } ] ] ]
      } ]
;

value chop_extension name =
  loop (String.length name - 1) where rec loop i =
    if i < 0 then name
    else if name.[i] = '.' then String.sub name 0 i
    else if name.[i] = '/' then name
    else if name.[i] = '\\' then name
    else loop (i - 1)
;

value match_strings regexp s =
  loop 0 0 where rec loop i j =
    if i = String.length regexp && j = String.length s then True
    else if i = String.length regexp then False
    else if j = String.length s then False
    else if regexp.[i] = s.[j] then loop (i + 1) (j + 1)
    else if regexp.[i] = '*' then
      if i + 1 = String.length regexp then True
      else if regexp.[i + 1] = s.[j] then loop (i + 2) (j + 1)
      else loop i (j + 1)
    else False
;

value excluded from =
  let efname = chop_extension Sys.argv.(0) ^ ".xcl" in
  match try Some (open_in efname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let rec loop () =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            if match_strings line from then do { close_in ic; True }
            else loop ()
        | None -> do { close_in ic; False } ]
      in
      loop ()
  | None -> False ]
;

value image_request cgi script_name env =
  match (Util.p_getenv env "m", Util.p_getenv env "v") with
  [ (Some "IM", Some fname) ->
      let fname =
        if fname.[0] = '/' then String.sub fname 1 (String.length fname - 1)
        else fname
      in
      let fname = Filename.basename fname in
      let fname = Util.image_file_name fname in
      let _ = Image.print_image_file cgi fname in True
  | _ ->
      let s = script_name in
      if Util.start_with s 0 "images/" then
        let i = String.length "images/" in
        let fname = String.sub s i (String.length s - i) in
        (* Je ne sais pas pourquoi on fait un basename, mais ça empeche *)
        (* empeche d'avoir des images qui se trouvent dans le dossier   *)
        (* image. Si on ne fait pas de basename, alors ça marche.       *)
        (* let fname = Filename.basename fname in *)
        let fname = Util.image_file_name fname in
        let _ = Image.print_image_file cgi fname in
        True
      else False ]
;


(* Une version un peu à cheval entre avant et maintenant afin de   *)
(* pouvoir inclure une css, un fichier javascript (etc) facilement *)
(* et que le cache du navigateur puisse prendre le relais.         *)
type misc_fname =
  [ Css of string
  | Js of string
  | Other of string ]
;

value content_misc cgi len misc_fname = do {
  if not cgi then Wserver.http "" else ();
  let (fname, t) =
    match misc_fname with
    [ Css fname -> (fname, "text/css")
    | Js fname -> (fname, "text/javascript")
    | Other fname -> (fname, "text/plain") ]
  in
  Wserver.wprint "Content-type: %s" t;
  Util.nl ();
  Wserver.wprint "Content-length: %d" len;
  Util.nl ();
  Wserver.wprint "Content-disposition: inline; filename=%s"
    (Filename.basename fname);
  Util.nl ();
  Util.nl ();
  Wserver.wflush ();
};

value print_misc_file cgi misc_fname =
  match misc_fname with
  [ Css fname | Js fname ->
      match
        try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ]
      with
      [ Some ic ->
          let buf = Bytes.create 1024 in
          let len = in_channel_length ic in
          do {
            content_misc cgi len misc_fname;
            let rec loop len =
              if len = 0 then ()
              else do {
                let olen = min (String.length buf) len in
                really_input ic buf 0 olen;
                Wserver.wprint "%s" (String.sub buf 0 olen);
                loop (len - olen)
              }
            in
            loop len;
            close_in ic;
            True
          }
      | None -> False ]
  | Other _ -> False ]
;

value misc_request cgi fname =
  let fname = Util.find_misc_file fname in
  if fname <> "" then
    let misc_fname =
      if Filename.check_suffix fname ".css" then Css fname
      else if Filename.check_suffix fname ".js" then Js fname
      else Other fname
    in
    print_misc_file cgi misc_fname
  else False
;

value strip_quotes s =
  let i0 = if String.length s > 0 && s.[0] = '"' then 1 else 0 in
  let i1 =
    if String.length s > 0 && s.[String.length s - 1] = '"' then
      String.length s - 1
    else String.length s
  in
  String.sub s i0 (i1 - i0)
;

value extract_multipart boundary str =
  let rec skip_nl i =
    if i < String.length str && str.[i] = '\r' then skip_nl (i + 1)
    else if i < String.length str && str.[i] = '\n' then i + 1
    else i
  in
  let next_line i =
    let i = skip_nl i in
    let rec loop s i =
      if i = String.length str || str.[i] = '\n' || str.[i] = '\r' then
        (s, i)
      else loop (s ^ String.make 1 str.[i]) (i + 1)
    in
    loop "" i
  in
  let boundary = "--" ^ boundary in
  let rec loop i =
    if i = String.length str then []
    else
      let (s, i) = next_line i in
      if s = boundary then
        let (s, i) = next_line i in
        let s = String.lowercase s in
        let env = Util.create_env s in
        match (Util.p_getenv env "name", Util.p_getenv env "filename") with
        [ (Some var, Some filename) ->
            let var = strip_quotes var in
            let filename = strip_quotes filename in
            let i = skip_nl i in
            let i1 =
              loop i where rec loop i =
                if i < String.length str then
                  if i > String.length boundary &&
                     String.sub str (i - String.length boundary)
                       (String.length boundary) =
                       boundary then
                    i - String.length boundary
                  else loop (i + 1)
                else i
            in
            let v = String.sub str i (i1 - i) in
            [(var ^ "_name", filename); (var, v) :: loop i1]
        | (Some var, None) ->
            let var = strip_quotes var in
            let (s, i) = next_line i in
            if s = "" then let (s, i) = next_line i in [(var, s) :: loop i]
            else loop i
        | _ -> loop i ]
      else if s = boundary ^ "--" then []
      else loop i
  in
  let env = loop 0 in
  let (str, _) =
    List.fold_left
      (fun (str, sep) (v, x) ->
         if v = "file" then (str, sep) else (str ^ sep ^ v ^ "=" ^ x, ";"))
      ("", "") env
  in
  (str, env)
;

value build_env request contents =
  let content_type = Wserver.extract_param "content-type: " '\n' request in
  if is_multipart_form content_type then
    let boundary = extract_boundary content_type in
    let (str, env) = extract_multipart boundary contents in (str, env)
  else (contents, Util.create_env contents)
;

value connection cgi (addr, request) script_name contents =
  let from =
    match addr with
    [ Unix.ADDR_UNIX x -> x
    | Unix.ADDR_INET iaddr port ->
        if no_host_address.val then
          Unix.string_of_inet_addr iaddr
        else
          try (Unix.gethostbyaddr iaddr).Unix.h_name with _ ->
            Unix.string_of_inet_addr iaddr ]
  in
  do {
    if not cgi && script_name = "robots.txt" then robots_txt ()
    else if excluded from then refuse_log from cgi
    else
      let accept =
        if only_addresses.val = [] then True
        else List.mem from only_addresses.val
      in
      if not accept then only_log from cgi
      else
        try
          let (contents, env) = build_env request contents in
          if image_request cgi script_name env then ()
          else if misc_request cgi script_name then ()
          else
            conf_and_connection cgi from (addr, request) script_name contents
              env
        with
        [ Adef.Request_failure msg -> print_request_failure cgi msg
        | Exit -> () ];
    Wserver.wflush ();
  }
;

value null_reopen flags fd =
  IFDEF UNIX THEN do {
    let fd2 = Unix.openfile "/dev/null" flags 0 in
    Unix.dup2 fd2 fd;
    Unix.close fd2;
  }
  ELSE () END
;

IFDEF SYS_COMMAND THEN
value wserver_auto_call = ref False
END;

value geneweb_server () =
  let auto_call =
    IFDEF SYS_COMMAND THEN wserver_auto_call.val
    ELSE try let _ = Sys.getenv "WSERVER" in True with [ Not_found -> False ]
    END
  in
  do {
    if not auto_call then do {
      let hostn =
        match selected_addr.val with
        [ Some addr -> addr
        | None -> try Unix.gethostname () with _ -> "computer" ]
      in
      eprintf "GeneWeb %s - " Version.txt;
      if not daemon.val then do {
        eprintf "Possible addresses:";
        eprintf "
   http://localhost:%d/base
   http://127.0.0.1:%d/base
   http://%s:%d/base" selected_port.val selected_port.val hostn
          selected_port.val;
        eprintf "
where \"base\" is the name of the database
Type %s to stop the service
" "control C";
      }
      else ();
      flush stderr;
      if daemon.val then
        if Unix.fork () = 0 then do {
          Unix.close Unix.stdin;
          null_reopen [Unix.O_WRONLY] Unix.stdout;
          null_reopen [Unix.O_WRONLY] Unix.stderr;
        }
        else exit 0
      else ();
      try Unix.mkdir (Filename.concat Util.cnt_dir.val "cnt") 0o777 with
      [ Unix.Unix_error _ _ _ -> () ];
    }
    else ();
    Wserver.f selected_addr.val selected_port.val conn_timeout.val
      (IFDEF UNIX THEN max_clients.val ELSE None END) (connection False)
  }
;

IFDEF UNIX THEN
value cgi_timeout tmout _ =
  let nl () = Wserver.wprint "\013\010" in
  do {
    Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
    nl (); nl ();
    Wserver.wprint "<head><title>Time out</title></head>\n";
    Wserver.wprint "<body><h1>Time out</h1>\n";
    Wserver.wprint "Computation time > %d second(s)\n" tmout;
    Wserver.wprint "</body>\n";
    Wserver.wflush ();
    exit 0;
  }
END;

IFDEF UNIX THEN
value manage_cgi_timeout tmout =
  if tmout > 0 then
    let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (cgi_timeout tmout)) in
    let _ = Unix.alarm tmout in
    ()
  else ()
END;

value geneweb_cgi addr script_name contents =
  do {
    IFDEF UNIX THEN manage_cgi_timeout conn_timeout.val ELSE () END;
    try Unix.mkdir (Filename.concat Util.cnt_dir.val "cnt") 0o755 with
    [ Unix.Unix_error _ _ _ -> () ];
    let add k x request =
      try
        let v = Sys.getenv x in
        if v = "" then raise Not_found
        else [k ^ ": " ^ v :: request]
      with
      [ Not_found -> request ]
    in
    let request = [] in
    let request = add "cookie" "HTTP_COOKIE" request in
    let request = add "content-type" "CONTENT_TYPE" request in
    let request = add "accept-language" "HTTP_ACCEPT_LANGUAGE" request in
    let request = add "referer" "HTTP_REFERER" request in
    let request = add "user-agent" "HTTP_USER_AGENT" request in
    connection True (Unix.ADDR_UNIX addr, request) script_name contents
  }
;

value read_input len =
  if len >= 0 then do {
    let buff = Bytes.create len in really_input stdin buff 0 len; buff
  }
  else do {
    let buff = ref "" in
    try
      while True do { let l = input_line stdin in buff.val := buff.val ^ l }
    with
    [ End_of_file -> () ];
    buff.val
  }
;

value arg_parse_in_file fname speclist anonfun errmsg =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let list =
        let list = ref [] in
        do {
          try
            while True do {
              let line = input_line ic in
              if line <> "" then list.val := [line :: list.val] else ()
            }
          with
          [ End_of_file -> () ];
          close_in ic;
          List.rev list.val
        }
      in
      let list =
        match list with
        [ [x] -> arg_list_of_string x
        | _ -> list ]
      in
      Argl.parse_list speclist anonfun errmsg list
  | _ -> () ]
;

module G =
  Grammar.GMake
    (struct type te = (string * string); value lexer = Plexer.gmake (); end)
;
value robot_xcl_arg = G.Entry.create "robot_xcl arg";
GEXTEND G
  robot_xcl_arg:
    [ [ cnt = INT; ","; sec = INT; EOI ->
          (int_of_string cnt, int_of_string sec) ] ]
  ;
END;

value robot_exclude_arg s =
  try
    robot_xcl.val :=
      Some (G.Entry.parse robot_xcl_arg (G.parsable (Stream.of_string s)))
  with
  [ Ploc.Exc _ (Stream.Error _ | Token.Error _) ->
      do {
        eprintf "Bad use of option -robot_xcl\n";
        eprintf "Use option -help for usage.\n";
        flush Pervasives.stderr;
        exit 2
      } ]
;

value slashify s =
  let s1 = Bytes.copy s in
  do {
    for i = 0 to String.length s - 1 do {
      Bytes.set s1 i
        (match s.[i] with
         [ '\\' -> '/'
         | x -> x ])
    };
    s1
  }
;

value make_cnt_dir x =
  do {
    mkdir_p x;
    IFDEF WIN95 THEN do {
      Wserver.sock_in.val := Filename.concat x "gwd.sin";
      Wserver.sock_out.val := Filename.concat x "gwd.sou";
    }
    ELSE () END;
    Util.cnt_dir.val := x;
  }
;

value main () =
  do {
    IFDEF WIN95 THEN do {
      Wserver.sock_in.val := "gwd.sin";
      Wserver.sock_out.val := "gwd.sou";
    }
    ELSE () END;
    let usage =
      "Usage: " ^ Filename.basename Sys.argv.(0) ^
      " [options] where options are:"
    in
    let speclist =
      [("-hd", Arg.String Util.add_lang_path,
        "<dir>\n       Directory where the directory lang is installed.");
       ("-dd", Arg.String Util.add_doc_path,
        "<dir>\n       Directory where the documentation is installed.");
       ("-bd", Arg.String Util.set_base_dir,
        "<dir>\n       Directory where the databases are installed.");
       ("-wd", Arg.String make_cnt_dir, "\
<dir>
       Directory for socket communication (Windows) and access count.");
       ("-cgi", Arg.Set cgi, "\n       Force cgi mode.");
       ("-images_url", Arg.String (fun x -> Util.images_url.val := x),
        "<url>\n       URL for GeneWeb images (default: gwd send them)");
       ("-images_dir", Arg.String (fun x -> images_dir.val := x), "\
<dir>
       Same than previous but directory name relative to current");
       ("-a", Arg.String (fun x -> selected_addr.val := Some x), "\
<address>
       Select a specific address (default = any address of this computer)");
       ("-p", Arg.Int (fun x -> selected_port.val := x),
        "<number>\n       Select a port number (default = " ^
          string_of_int selected_port.val ^ "); > 1024 for normal users.");
       ("-setup_link", Arg.Set setup_link,
        "\n       Display a link to local gwsetup in bottom of pages.");
       ("-allowed_tags",
        Arg.String (fun x -> Util.allowed_tags_file.val := x), "\
<file>
       HTML tags which are allowed to be displayed. One tag per line in file.");
       ("-wizard", Arg.String (fun x -> wizard_passwd.val := x), "\
<passwd>
       Set a wizard password: access to all dates and updating.");
       ("-friend", Arg.String (fun x -> friend_passwd.val := x),
        "<passwd>\n       Set a friend password: access to all dates.");
       ("-wjf", Arg.Set wizard_just_friend,
        "\n       Wizard just friend (permanently)");
       ("-lang", Arg.String (fun x -> default_lang.val := x),
        "<lang>\n       Set a default language (default: fr).");
       ("-blang", Arg.Set choose_browser_lang,
        "\n       Select the user browser language if any.");
       ("-only",
        Arg.String (fun x -> only_addresses.val := [x :: only_addresses.val]),
        "<address>\n       Only inet address accepted.");
       ("-auth", Arg.String (fun x -> auth_file.val := x), "\
<file>
       Authorization file to restrict access. The file must hold lines
       of the form \"user:password\".");
       ("-no_host_address", Arg.Set no_host_address,"\n       \
        Force no reverse host by address");
       ("-digest", Arg.Set use_auth_digest_scheme, "\n       \
        Use Digest authorization scheme (more secure on passwords)");
       ("-add_lexicon",
        Arg.String (fun x -> lexicon_list.val := [x :: lexicon_list.val]),
        "<lexicon>\n       Add file as lexicon.");
       ("-log", Arg.String (fun x -> log_file.val := x),
        "<file>\n       Redirect log trace to this file.");
       ("-robot_xcl", Arg.String robot_exclude_arg, "\
<cnt>,<sec>
       Exclude connections when more than <cnt> requests in <sec> seconds.");
       ("-min_disp_req", Arg.Int (fun x -> Robot.min_disp_req.val := x),
        "\n       Minimum number of requests in robot trace (default: " ^
        string_of_int Robot.min_disp_req.val ^ ")");
       ("-login_tmout", Arg.Int (fun x -> login_timeout.val := x), "\
<sec>
       Login timeout for entries with passwords in CGI mode (default " ^ string_of_int login_timeout.val ^ "\
s)"); ("-redirect", Arg.String (fun x -> redirected_addr.val := Some x), "\
<addr>
       Send a message to say that this service has been redirected to <addr>");
       ("-trace_failed_passwd", Arg.Set trace_failed_passwd, "\n       \
Print the failed passwords in log (except if option -digest is set) ");
       ("-nolock", Arg.Set Lock.no_lock_flag,
        "\n       Do not lock files before writing.") ::
       IFDEF UNIX THEN
         [("-max_clients", Arg.Int (fun x -> max_clients.val := Some x), "\
<num>
       Max number of clients treated at the same time (default: no limit)
       (not cgi).");
          ("-conn_tmout", Arg.Int (fun x -> conn_timeout.val := x),
           "<sec>\n       Connection timeout (default " ^
             string_of_int conn_timeout.val ^ "s; 0 means no limit)");
          ("-daemon", Arg.Set daemon, "\n       Unix daemon mode.");
          ("-chwd", Arg.String (fun s -> Doc.notify_change_wdoc.val := s),
           "<comm>\n       Call command when wdoc changed")]
       ELSE
         [("-noproc", Arg.Set Wserver.noproc,
           "\n       Do not launch a process at each request.") ::
          IFDEF SYS_COMMAND THEN
            [("-wserver", Arg.String (fun _ -> wserver_auto_call.val := True),
              "\n       (internal feature)")]
          ELSE [] END]
       END]
    in
    let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
    IFDEF UNIX THEN
      default_lang.val :=
        let s = try Sys.getenv "LANG" with [ Not_found -> "" ] in
        if List.mem s Version.available_languages then s
        else
          let s = try Sys.getenv "LC_CTYPE" with [ Not_found -> "" ] in
          if String.length s >= 2 then
            let s = String.sub s 0 2 in
            if List.mem s Version.available_languages then s else "en"
          else "en"
    ELSE () END;
    arg_parse_in_file (chop_extension Sys.argv.(0) ^ ".arg") speclist anonfun
      usage;
    Argl.parse speclist anonfun usage;
    if images_dir.val <> "" then
      let abs_dir =
        let f =
          Util.search_in_lang_path
            (Filename.concat images_dir.val "gwback.jpg")
        in
        let d = Filename.dirname f in
        if Filename.is_relative d then Filename.concat (Sys.getcwd ()) d
        else d
      in
      Util.images_url.val := "file://" ^ slashify abs_dir
    else ();
    if Secure.doc_path () = [] then
      List.iter (fun d -> Util.add_doc_path (Filename.concat d "doc"))
        (List.rev (Secure.lang_path ()))
    else ();
    if Util.cnt_dir.val = Filename.current_dir_name then
      Util.cnt_dir.val := Secure.base_dir ()
    else ();
    Wserver.stop_server.val :=
      List.fold_left Filename.concat Util.cnt_dir.val ["cnt"; "STOP_SERVER"]
    ;
    let (query, cgi) =
      try (Sys.getenv "QUERY_STRING", True) with
      [ Not_found -> ("", cgi.val) ]
    in
    if cgi then
      let is_post =
        try Sys.getenv "REQUEST_METHOD" = "POST" with [ Not_found -> False ]
      in
      let query =
        if is_post then do {
          let len =
            try int_of_string (Sys.getenv "CONTENT_LENGTH") with
            [ Not_found -> -1 ]
          in
          set_binary_mode_in stdin True;
          read_input len
        }
        else query
      in
      let addr =
        try Sys.getenv "REMOTE_HOST" with
        [ Not_found -> try Sys.getenv "REMOTE_ADDR" with [ Not_found -> "" ] ]
      in
      let script =
        try Sys.getenv "SCRIPT_NAME" with [ Not_found -> Sys.argv.(0) ]
      in
      geneweb_cgi addr (Filename.basename script) query
    else geneweb_server ()
  }
;

value test_eacces_bind err fun_name =
  IFDEF UNIX THEN
    if err = Unix.EACCES && fun_name = "bind" then
      try
        do {
          eprintf "
Error: invalid access to the port %d: users port number less than 1024
are reserved to the system. Solution: do it as root or choose another port
number greater than 1024.
" selected_port.val;
          flush stderr;
          True
        }
      with
      [ Not_found -> False ]
    else False
  ELSE False END
;

value print_exc exc =
  match exc with
  [ Unix.Unix_error Unix.EADDRINUSE "bind" _ ->
      do {
        eprintf "\nError: ";
        eprintf "the port %d" selected_port.val;
        eprintf " \
is already used by another GeneWeb daemon
or by another program. Solution: kill the other program or launch
GeneWeb with another port number (option -p)
";
        flush stderr;
      }
  | Unix.Unix_error err fun_name arg ->
      if test_eacces_bind err fun_name then ()
      else do {
        prerr_string "\"";
        prerr_string fun_name;
        prerr_string "\" failed";
        if String.length arg > 0 then do {
          prerr_string " on \""; prerr_string arg; prerr_string "\""; ()
        }
        else ();
        prerr_string ": ";
        prerr_endline (Unix.error_message err);
        flush stderr;
      }
  | _ ->
      do {
        eprintf "%s\n" (Printexc.to_string exc);
        flush stderr
      } ]
;

try main () with exc -> print_exc exc;
