(* camlp4r ./pa_lock.cmo *)
(* $Id: util.ml,v 4.162 2005-10-16 03:03:01 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;
open Gutil;
open Printf;

value sharelib =
  List.fold_right Filename.concat [Gwlib.prefix; "share"] "geneweb"
;

value add_lang_path = Secure.add_lang_path;
value add_doc_path = Secure.add_doc_path;
value set_base_dir = Secure.set_base_dir;

add_lang_path sharelib;
add_lang_path Filename.current_dir_name;

value cnt_dir = ref Filename.current_dir_name;
value images_url = ref "";

value search_in_path p s =
  loop (p ()) where rec loop =
    fun
    [ [d :: dl] ->
        let f = Filename.concat d s in
        if Sys.file_exists f then f else loop dl
    | [] -> s ]
;

value search_in_lang_path = search_in_path Secure.lang_path;
value search_in_doc_path = search_in_path Secure.doc_path;

(* Internationalization *)

value start_with_vowel s =
  if String.length s > 0 then
    match Char.lowercase s.[0] with
    [ 'a' | 'e' | 'i' | 'o' | 'u' -> True
    | _ -> False ]
  else False
;

value match_begin s t =
  loop 0 0 where rec loop i j =
    if i >= String.length s || j >= String.length t then True
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else False
;

value amp_capitalize capitale s =
  if String.length s <= 1 then s
  else if match_begin s "&iexcl;" then
    "&iexcl;" ^ capitale (String.sub s 7 (String.length s - 7))
  else if match_begin s "&aelig;" then
    "&AElig;" ^ String.sub s 7 (String.length s - 7)
  else
    match s.[1] with
    [ 'a'..'z' ->
        "&" ^
          String.make 1
            (Char.chr
               (Char.code s.[1] - Char.code 'a' + Char.code 'A')) ^
          String.sub s 2 (String.length s - 2)
    | _ -> s ]
;

value rec capitale_iso_8859_1 s =
  if String.length s == 0 then ""
  else
    match s.[0] with
    [ 'a'..'z' | 'à'..'ö' | 'ø'..'ý' ->
        String.make 1
          (Char.chr (Char.code s.[0] - Char.code 'a' + Char.code 'A')) ^
          String.sub s 1 (String.length s - 1)
    | '&' -> amp_capitalize capitale_iso_8859_1 s
    | _ -> s ]
;

value rec capitale_utf_8 s =
  if String.length s == 0 then ""
  else
    let c = s.[0] in
    if c = '&' then amp_capitalize capitale_utf_8 s
    else if c = '<' then
      loop 1 where rec loop i =
        if i = String.length s then s
        else if s.[i] = '>' then
          let s1 = String.sub s (i + 1) (String.length s - i - 1) in
          String.sub s 0 (i + 1) ^ capitale_utf_8 s1
        else loop (i + 1)
    else if Char.code c < 0b10000000 then String.capitalize s
    else if String.length s == 1 then s
    else if Char.code c == 0xC3 then
      let c1 = Char.uppercase (Char.chr (Char.code s.[1] + 0x40)) in
      String.make 1 c ^ String.make 1 (Char.chr (Char.code c1 - 0x40)) ^
        String.sub s 2 (String.length s - 2)
    else s
;

value index_of_next_char s i =
  if Gutil.utf_8_db.val then
    min (String.length s) (i + max 1 (Gutil.nbc s.[i]))
  else i + 1
;

value capitale s =
  if Gutil.utf_8_db.val then capitale_utf_8 s
  else capitale_iso_8859_1 s
;

value fcapitale (a : format 'a 'b 'c) : format 'a 'b 'c =
  Obj.magic capitale a
;

value nth_field_abs w n =
  let rec start i n =
    if n == 0 then i
    else if i < String.length w then
      match w.[i] with
      [ '<' -> start (i + 2) n
      | '/' -> start (i + 1) (n - 1)
      | _ -> start (i + 1) n ]
    else i
  in
  let rec stop i =
    if i < String.length w then
      match w.[i] with
      [ '<' -> stop (i + 2)
      | '/' -> i
      | _ -> stop (i + 1) ]
    else i
  in
  let i1 = start 0 n in let i2 = stop i1 in (i1, i2)
;

value nth_field w n =
  let (i1, i2) = nth_field_abs w n in
  let (i1, i2) = if i2 == i1 then nth_field_abs w 0 else (i1, i2) in
  String.sub w i1 (i2 - i1)
;

value tnf s = "[" ^ s ^ "]";

value transl conf w =
  try Hashtbl.find conf.lexicon w with
  [ Not_found -> tnf w ]
;

value transl_nth conf w n =
  try nth_field (Hashtbl.find conf.lexicon w) n with
  [ Not_found -> tnf (nth_field w n) ]
;

value transl_nth_def conf w n def_n =
  try
    let w = Hashtbl.find conf.lexicon w in
    let (i1, i2) = nth_field_abs w n in
    if i2 == i1 then nth_field w def_n else String.sub w i1 (i2 - i1)
  with
  [ Not_found -> tnf (nth_field w def_n) ]
;

value plus_decl s =
  match rindex s '+' with
  [ Some i ->
      if i > 0 && s.[i - 1] == ' ' then
        let start = String.sub s 0 (i - 1) in
        let decl = String.sub s (i - 1) (String.length s - (i - 1)) in
        Some (start, decl)
      else None
  | None -> None ]
;

value gen_decline wt s =
  let s1 = if s = "" then "" else if wt = "" then s else " " ^ s in
  let len = String.length wt in
  if rindex wt '/' <> None then
    match rindex wt '/' with
    [ Some i ->
        if String.length s > 0 && start_with_vowel s then
          nth_field wt 1 ^ Gutil.decline 'n' s
        else nth_field wt 0 ^ Gutil.decline 'n' s1
    | None -> wt ^ Gutil.decline 'n' s1 ]
  else if len >= 3 && wt.[len - 3] == ':' && wt.[len - 1] == ':' then
    let start = String.sub wt 0 (len - 3) in
    start ^ Gutil.decline wt.[len - 2] s
  else
    match plus_decl wt with
    [ Some (start, " +before") -> if s = "" then start else s ^ " " ^ start
    | _ -> wt ^ Gutil.decline 'n' s1 ]
;

value transl_decline conf w s = gen_decline (transl conf w) s;

value gen_decline2 wt s1 s2 =
  let string_of =
    fun
    [ '1' -> Some s1
    | '2' -> Some s2
    | _ -> None ]
  in
  let len = String.length wt in
  let rec loop i =
    if i = len then ""
    else
      let (s, i) =
        match wt.[i] with
        [ '%' when i + 1 < len ->
            match string_of wt.[i + 1] with
            [ Some s -> (nominative s, i + 1)
            | None -> ("%", i) ]
        | ':' when i + 4 < len && wt.[i + 2] = ':' && wt.[i + 3] = '%' ->
            let c = wt.[i + 1] in
            match string_of wt.[i + 4] with
            [ Some s -> (decline c s, i + 4)
            | None -> (":", i) ]
        | '[' ->
            try
              let j = String.index_from wt i ']' in
              if j + 2 < len && wt.[j + 1] = '%' then
                match string_of wt.[j + 2] with
                [ Some s ->
                    let s = nominative s in
                    let s =
                      if start_with_vowel s then String.make 1 wt.[j - 1] ^ s
                      else String.sub wt (i + 1) (j - i - 2) ^ " " ^ s
                    in
                    (s, j + 2)
                | None -> raise Not_found ]
              else raise Not_found
            with
            [ Not_found -> ("[", i) ]
        | c -> (String.make 1 c, i) ]
      in
      s ^ loop (i + 1)
  in
  loop 0
;

value transl_a_of_b conf = gen_decline2 (transl_nth conf "%1 of %2" 0);
value transl_a_of_gr_eq_gen_lev conf =
  gen_decline2 (transl_nth conf "%1 of %2" 1)
;

value check_format ini_fmt (r : string) =
  let s : string = Obj.magic (ini_fmt : format 'a 'b 'c) in
  let rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match (s.[i], s.[i + 1], r.[j], r.[j + 1]) with
      [ ('%', x, '%', y) ->
          if x = y then loop (i + 2) (j + 2) else None
      | ('%', _, _, _) -> loop i (j + 1)
      | (_, _, '%', _) -> loop (i + 1) j
      | _ -> loop (i + 1) (j + 1) ]
    else if i < String.length s - 1 then
      if s.[i] == '%' then None else loop (i + 1) j
    else if j < String.length r - 1 then
      if r.[j] == '%' then None else loop i (j + 1)
    else
      Some (Obj.magic r : format 'a 'b 'c)
  in
  loop 0 0
;

value valid_format ini_fmt r =
  match check_format ini_fmt r with
  [ Some fmt -> fmt
  | None -> Obj.magic (tnf (Obj.magic ini_fmt)) ]
;

value cftransl conf fmt =
  let fmt = transl conf fmt in
  let rec loop i =
    fun
    [ [] -> String.sub fmt i (String.length fmt - i)
    | [a :: al] as gal ->
        if i + 4 < String.length fmt && fmt.[i] == ':' &&
           fmt.[i + 2] == ':' && fmt.[i + 3] == '%' && fmt.[i + 4] == 's' then
          decline fmt.[i + 1] a ^ loop (i + 5) al
        else if
          i + 1 < String.length fmt && fmt.[i] == '%' &&
          fmt.[i + 1] == 's' then
          nominative a ^ loop (i + 2) al
        else if i < String.length fmt then
          String.make 1 fmt.[i] ^ loop (i + 1) gal
        else "" ]
  in
  loop 0
;

value ftransl conf s = valid_format s (transl conf (Obj.magic s : string));

value ftransl_nth conf s p =
  valid_format s (transl_nth conf (Obj.magic s : string) p)
;

value fdecline conf w s =
  valid_format w (gen_decline (Obj.magic w : string) s)
;

(* *)

value begin_centered conf =
  Wserver.wprint
    "<table border=\"%d\" width=\"100%%\"><tr><td align=\"center\">\n"
    conf.border;
value end_centered _ = Wserver.wprint "</td></tr></table>\n";

value html_br conf = Wserver.wprint "<br%s>\n" conf.xhs;

value html_p conf = do { Wserver.wprint "<p>"; Wserver.wprint "\n"; };

value html_li conf = do { Wserver.wprint "<li>"; Wserver.wprint "\n"; };

value nl () = Wserver.wprint "\013\010";

value week_day_txt =
  let txt = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  fun i ->
    let i = if i < 0 || i >= Array.length txt then 0 else i in
    txt.(i)
;
value month_txt =
  let txt =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
       "Nov"; "Dec" |]
  in
  fun i ->
    let i = if i < 0 || i >= Array.length txt then 0 else i in
    txt.(i)
;

value string_of_ctime conf =
  let lt = Unix.gmtime conf.ctime in
  sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    (week_day_txt lt.Unix.tm_wday) lt.Unix.tm_mday (month_txt lt.Unix.tm_mon)
    (1900 + lt.Unix.tm_year) lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec
;

value html conf =
  let charset = if conf.charset = "" then "utf-8" else conf.charset in
  do {
    if not conf.cgi then do {
      Wserver.http "";
      Wserver.wprint "Server: GeneWeb/%s" Version.txt;
      nl ();
    }
    else ();
    Wserver.wprint "Date: %s" (string_of_ctime conf); nl ();
    Wserver.wprint "Connection: close"; nl ();
    Wserver.wprint "Content-type: text/html; charset=%s" charset; nl ();
  }
;

value unauthorized conf auth_type =
  do {
    if not conf.cgi then do {
      Wserver.http "401 Unauthorized";
      Wserver.wprint "WWW-Authenticate: Basic realm=\"%s\"" auth_type;
      nl ()
    }
    else ();
    Wserver.wprint "Content-type: text/html; charset=%s" conf.charset;
    nl ();
    nl ();
    Wserver.wprint "<head><title>Access failed</title></head>\n";
    Wserver.wprint "<body><h1>Access failed</h1>\n";
    Wserver.wprint "<ul><li>%s</ul>\n" auth_type;
    Wserver.wprint "</body>\n";
  }
;

value commd conf =
  let c = conf.command ^ "?" in
  List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ ";") c
    (conf.henv @ conf.senv)
;

value commd_no_params conf =
  conf.command ^ "?" ^
    List.fold_left
      (fun c (k, v) ->
         c ^ (if c = "" then "" else ";") ^ k ^
           (if v = "" then "" else "=" ^ v))
      "" conf.henv
;

value code_varenv = Wserver.encode;
value decode_varenv = Wserver.decode;

value quote_escaped s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '"' | '&' | '<' | '>' -> True
      | x -> need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> i1 + 6
        | '&' -> i1 + 5
        | '<' | '>' -> i1 + 4
        | _ -> succ i1 ]
      in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> do { String.blit "&#034;" 0 s1 i1 6; i1 + 6 }
        | '&' -> do { String.blit "&amp;" 0 s1 i1 5; i1 + 5 }
        | '<' -> do { String.blit "&lt;" 0 s1 i1 4; i1 + 4 }
        | '>' -> do { String.blit "&gt;" 0 s1 i1 4; i1 + 4 }
        | c -> do { s1.[i1] := c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value no_html_tags s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '<' | '>' -> True
      | _ -> need_code (i + 1) ]
    else False
  in
  if need_code 0 then
    let rec loop i len =
      if i = String.length s then Buff.get len
      else
        let (len, next_i) =
          match s.[i] with
          [ '<' -> (Buff.mstore len "&lt;", i + 1)
          | '>' -> (Buff.mstore len "&gt;", i + 1)
          | c -> (Buff.store len c, i + 1) ]
        in
        loop next_i len
    in
    loop 0 0
  else s
;

value hidden_env conf =
  List.iter
    (fun (k, v) ->
       Wserver.wprint "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n" k
         (quote_escaped (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.senv)
;

value p_getenv env label =
  try Some (decode_varenv (List.assoc (decode_varenv label) env)) with
  [ Not_found -> None ]
;

value p_getint env label =
  match p_getenv env label with
  [ Some s ->
      try Some (int_of_string (strip_spaces s)) with
      [ Failure _ -> None ]
  | None -> None ]
;

value nobtit conf base p =
  match Lazy.force conf.allowed_titles with
  [ [] -> p.titles
  | allowed_titles ->
      List.fold_right
        (fun t l ->
           let id = sou base t.t_ident in
           let pl = sou base t.t_place in
           if List.mem (id ^ "/" ^ pl) allowed_titles then [t :: l] else l)
        p.titles [] ]
;

value parent_has_title conf base p =
  let a = aoi base p.cle_index in
  match parents a with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (father cpl) in
      let moth = poi base (mother cpl) in
      fath.access <> Private && nobtit conf base fath <> [] ||
      moth.access <> Private && nobtit conf base moth <> []
  | _ -> False ]
;

value authorized_age conf base p =
  if p.access = Public || conf.friend || conf.wizard then True
  else if
    conf.public_if_titles && p.access = IfTitles &&
    (nobtit conf base p <> [] || parent_has_title conf base p) then
    True
  else
    match
      (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism, p.death,
       date_of_death p.death)
    with
    [ (_, _, NotDead, _) when conf.private_years > 0 -> False
    | (Some (Dgreg d _), _, _, _) ->
        let a = time_gone_by d conf.today in a.year > conf.private_years
    | (_, Some (Dgreg d _), _, _) ->
        let a = time_gone_by d conf.today in a.year > conf.private_years
    | (_, _, _, Some (Dgreg d _)) ->
        let a = time_gone_by d conf.today in a.year > conf.private_years
    | (None, None, DontKnowIfDead, None) ->
        p.access <> Private && conf.public_if_no_date
    | _ ->
        let u = uoi base p.cle_index in
        let rec loop i =
          if i >= Array.length u.family then False
          else
            let fam = foi base u.family.(i) in
            match Adef.od_of_codate fam.marriage with
            [ Some (Dgreg d _) ->
                let a = time_gone_by d conf.today in
                a.year > conf.private_years
            | _ -> loop (i + 1) ]
        in
        loop 0 ]
;

value is_old_person conf p =
  match
    (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism, p.death,
     date_of_death p.death)
  with
  [ (_, _, NotDead, _) when conf.private_years > 0 -> False
  | (Some (Dgreg d _), _, _, _) ->
      let a = time_gone_by d conf.today in a.year > conf.private_years
  | (_, Some (Dgreg d _), _, _) ->
      let a = time_gone_by d conf.today in a.year > conf.private_years
  | (_, _, _, Some (Dgreg d _)) ->
      let a = time_gone_by d conf.today in a.year > conf.private_years
  | (None, None, DontKnowIfDead, None) ->
      p.access <> Private && conf.public_if_no_date
  | _ -> False ]
;

value fast_auth_age conf p =
  if conf.friend || conf.wizard || p.access = Public then True
  else if
    conf.public_if_titles && p.access = IfTitles && p.titles <> []
  then
    True
  else is_old_person conf p
;

value is_restricted (conf : config) base ip =
  let quest_string = Adef.istr_of_int 1 in
  let fct p =
    p.surname <> quest_string && p.first_name <> quest_string &&
    not (fast_auth_age conf p)
  in  
  if conf.use_restrict then
    base.data.visible.v_get fct (Adef.int_of_iper ip)
  else False 
;

value empty_string = Adef.istr_of_int 0;

value is_hidden p =
  p.surname = empty_string
;

value pget (conf : config) base ip =
  if is_restricted conf base ip then
    { first_name = empty_string;
      surname = empty_string;
      occ = 0;
      image = empty_string;
      first_names_aliases = [];
      surnames_aliases = [];
      public_name = empty_string;
      qualifiers = [];
      titles = [];
      rparents = [];
      related = [];
      aliases = [];
      occupation = empty_string;
      sex = Neuter;
      access = Private;
      birth = Adef.codate_None;
      birth_place = empty_string;
      birth_src = empty_string;
      baptism = Adef.codate_None;
      baptism_place = empty_string;
      baptism_src = empty_string;
      death = DontKnowIfDead;
      death_place = empty_string;
      death_src = empty_string;
      burial = UnknownBurial;
      burial_place = empty_string;
      burial_src = empty_string;
      notes = empty_string;
      psources = empty_string;
      cle_index = ip }
  else base.data.persons.get (Adef.int_of_iper ip)
;

value aget (conf : config) base ip =
  if is_restricted conf base ip then no_ascend ()
  else base.data.ascends.get (Adef.int_of_iper ip)
;

value uget (conf : config) base ip =
  if is_restricted conf base ip then
    { family = [| |] }
  else base.data.unions.get (Adef.int_of_iper ip)
;

value know base p =
  sou base p.first_name <> "?" || sou base p.surname <> "?"
;

value is_public conf base p =
  p.access = Public ||
  conf.public_if_titles && p.access = IfTitles && nobtit conf base p <> [] ||
  is_old_person conf p
;

value accessible_by_key conf base p fn sn =
  conf.access_by_key
  && not (fn = "?" || sn = "?")
  && (not conf.hide_names || is_public conf base p)
;

value acces_n conf base n x =
  let first_name = p_first_name base x in
  let surname = p_surname base x in
  if surname = "" then ""
  else if accessible_by_key conf base x first_name surname then
    "p" ^ n ^ "=" ^ code_varenv (Name.lower first_name) ^ ";n" ^ n ^ "=" ^
      code_varenv (Name.lower surname) ^
      (if x.occ > 0 then ";oc" ^ n ^ "=" ^ string_of_int x.occ else "")
  else
    "i" ^ n ^ "=" ^ string_of_int (Adef.int_of_iper x.cle_index) ^
    (if conf.wizard && x.occ > 0 then ";oc" ^ n ^ "=" ^ string_of_int x.occ
     else "")
;

value acces conf base x = acces_n conf base "" x;

type p_access = (base -> person -> string * base -> person -> string);
value std_access = (p_first_name, p_surname);
value raw_access =
  (fun base p -> sou base p.first_name, fun base p -> sou base p.surname)
;

value restricted_txt conf = ".....";

value gen_person_text (p_first_name, p_surname) conf base p =
  if is_hidden p then restricted_txt conf
  else if conf.hide_names && not (fast_auth_age conf p) then "x x"
  else
    let beg =
      match (sou base p.public_name, p.qualifiers) with
      [ ("", [nn :: _]) ->
          p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
      | ("", []) -> p_first_name base p
      | (n, [nn :: _]) -> n ^ " <em>" ^ sou base nn ^ "</em>"
      | (n, []) -> n ]
    in
(*
    let ali =
      match p.aliases with
      [ [alias :: _] -> " <em>(" ^ sou base alias ^ ")</em>"
      | _ -> "" ]
    in
*)
    beg ^ " " ^ p_surname base p (*^ ali*)
;

value gen_person_text_no_html (p_first_name, p_surname) conf base p =
  if is_hidden p then restricted_txt conf
  else if conf.hide_names && not (fast_auth_age conf p) then "x x"
  else
    let beg =
      match (sou base p.public_name, p.qualifiers) with
      [ ("", [nn :: _]) -> p_first_name base p ^ " " ^ sou base nn
      | ("", []) -> p_first_name base p
      | (n, [nn :: _]) -> n ^ " " ^ sou base nn
      | (n, []) -> n ]
    in
    beg ^ " " ^ p_surname base p
;

value gen_person_text_without_surname check_acc (p_first_name, p_surname) conf
    base p
=
  if is_hidden p then restricted_txt conf
  else if check_acc && conf.hide_names && not (fast_auth_age conf p) then
    "x x"
  else
    let s =
      match (sou base p.public_name, p.qualifiers) with
      [ (n, [nn :: _]) when n <> "" -> n ^ " <em>" ^ sou base nn ^ "</em>"
      | (n, []) when n <> "" -> n
      | (_, [nn :: _]) -> p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
      | (_, []) -> p_first_name base p ]
    in
(*
    let ali =
      match p.aliases with
      [ [alias :: _] -> " <em>(" ^ sou base alias ^ ")</em>"
      | _ -> "" ]
    in
*)
    s (*^ ali*)
;

value person_text = gen_person_text std_access;
value person_text_no_html = gen_person_text_no_html std_access;
value person_text_without_surname =
  gen_person_text_without_surname True std_access
;
value person_text_no_surn_no_acc_chk =
  gen_person_text_without_surname False std_access
;

value main_title conf base p =
  let rec find_main =
    fun
    [ [] -> None
    | [x :: l] -> if x.t_name == Tmain then Some x else find_main l ]
  in
  match find_main (nobtit conf base p) with
  [ None ->
      match nobtit conf base p with
      [ [x :: _] -> Some x
      | _ -> None ]
  | x -> x ]
;

value titled_person_text conf base p t =
  let estate = sou base t.t_place in
  let surname = p_surname base p in
  let elen = String.length estate in
  let slen = String.length surname in
  if Name.strip_lower estate = Name.strip_lower surname then
    match (t.t_name, p.qualifiers) with
    [ (Tname n, []) -> sou base n
    | (Tname n, [nn :: _]) -> sou base n ^ " <em>" ^ sou base nn ^ "</em>"
    | _ -> person_text_without_surname conf base p ]
  else if elen < slen && String.sub surname (slen - elen) elen = estate then
    match (t.t_name, p.qualifiers) with
    [ (Tname n, []) -> sou base n
    | (Tname n, [nn :: _]) -> sou base n ^ " <em>" ^ sou base nn ^ "</em>"
    | _ ->
        let trunc_surname _ _ =
          strip_spaces (String.sub surname 0 (slen - elen))
        in
        let trunc_access = (p_first_name, trunc_surname) in
        gen_person_text trunc_access conf base p ]
  else
    match t.t_name with
    [ Tname s ->
        let s = sou base s in
        match p.qualifiers with
        [ [] -> s
        | [nn :: _] -> s ^ " <em>" ^ sou base nn ^ "</em>" ]
    | _ -> person_text conf base p ]
;

value one_title_text conf base p t =
  let place = sou base t.t_place in
  let s = sou base t.t_ident in
  let s = if place = "" then s else s ^ " " ^ place in ", <em>" ^ s ^ "</em>"
;

value geneweb_link conf href s =
  if conf.cancel_links then s
  else "<a href=\"" ^ commd conf ^ href ^ "\">" ^ s ^ "</a>"
;

value wprint_geneweb_link conf href s =
  Wserver.wprint "%s" (geneweb_link conf href s)
;

value reference conf base p s =
  if conf.cancel_links || is_hidden p then s
  else "<a href=\"" ^ commd conf ^ acces conf base p ^ "\">" ^ s ^ "</a>"
;

value no_reference conf base p s = s;

value gen_person_title_text reference p_access conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
    [ Some t ->
        reference conf base p (titled_person_text conf base p t) ^
          one_title_text conf base p t
    | None -> reference conf base p (gen_person_text p_access conf base p) ]
  else reference conf base p (gen_person_text p_access conf base p)
;

value referenced_person_title_text =
  gen_person_title_text reference std_access
;

value person_title_text = gen_person_title_text no_reference std_access;

value referenced_person_text conf base p =
  reference conf base p (person_text conf base p)
;

value referenced_person_text_without_surname conf base p =
  reference conf base p (person_text_without_surname conf base p)
;

value gen_person_text_without_title p_access conf base p =
  match main_title conf base p with
  [ Some t ->
      if t.t_place == p.surname then
        gen_person_text_without_surname True p_access conf base p
      else
        match (t.t_name, p.qualifiers) with
        [ (Tname s, [nn :: _]) -> sou base s ^ " <em>" ^ sou base nn ^ "</em>"
        | (Tname s, _) -> sou base s
        | _ -> gen_person_text p_access conf base p ]
  | None -> gen_person_text p_access conf base p ]
;

value person_text_without_title = gen_person_text_without_title std_access;

value person_title conf base p =
  if authorized_age conf base p then
    match main_title conf base p with
    [ Some t -> one_title_text conf base p t
    | None -> "" ]
  else ""
;

value old_surname_begin n =
  let i = initial n in
  if i == 0 then ""
  else
    let i =
      strip_spaces i where rec strip_spaces i =
        if i >= 1 && n.[i - 1] == ' ' then strip_spaces (pred i) else i
    in
    " (" ^ String.sub n 0 i ^ ")"
;

value old_surname_end n =
  let i = initial n in
  if i == 0 then n else String.sub n i (String.length n - i)
;

value start_with s i p =
  i + String.length p <= String.length s &&
  String.lowercase (String.sub s i (String.length p)) = p
;

value get_particle base s =
  loop base.data.particles where rec loop =
    fun
    [ [part :: parts] -> if start_with s 0 part then part else loop parts
    | [] -> "" ]
;

value surname_begin base s =
  let part = get_particle base s in
  let len = String.length part in
  if len = 0 then ""
  else if part.[len-1] = ' ' then " (" ^ String.sub part 0 (len - 1) ^ ")"
  else " (" ^ part ^ ")"
;

value surname_end base s =
  let part_len = String.length (get_particle base s) in
  String.sub s part_len (String.length s - part_len)
;

value rec skip_spaces s i =
  if i < String.length s && s.[i] == ' ' then skip_spaces s (i + 1) else i
;

value create_env s =
  let rec get_assoc beg i =
    if i == String.length s then
      if i == beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] == ';' || s.[i] == '&' then
      let next_i = skip_spaces s (succ i) in
      [String.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] == '=' then
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)
;

value red_color = "red";
value std_color conf s =
  "<span style=\"color:" ^ conf.highlight ^ "\">" ^ s ^ "</span>"
;

value index_of_sex =
  fun
  [ Male -> 0
  | Female -> 1
  | Neuter -> 2 ]
;

value input_to_semi ic =
  loop 0 where rec loop len =
    let c = input_char ic in
    if c = ';' then Buff.get len else loop (Buff.store len c)
;

value base_path pref bname =
  let pref = [Secure.base_dir () :: pref] in
  let bfile = List.fold_right Filename.concat pref bname in
  IFDEF WIN95 THEN bfile
  ELSE if Sys.file_exists bfile then bfile
  else if String.length bname >= 6 then
    let dirs = pref @ [String.make 1 bname.[0]; String.make 1 bname.[1]] in
    List.fold_right Filename.concat dirs bname
  else bfile
  END
;

value base_len n =
  let n = base_path [] (n ^ ".gwb") in
  match try Some (Iobase.input n) with [ Sys_error _ -> None ] with
  [ Some base ->
      let len = base.data.persons.len in
      do { base.func.cleanup (); string_of_int len }
  | _ -> "?" ]
;

value open_etc_file fname =
  let fname1 = base_path ["etc"] (Filename.basename fname ^ ".txt") in
  let fname2 =
    search_in_lang_path
      (Filename.concat "etc" (Filename.basename fname ^ ".txt"))
  in
  try Some (Secure.open_in fname1) with
  [ Sys_error _ ->
      try Some (Secure.open_in fname2) with [ Sys_error _ -> None ] ]
;

value macro_etc env imcom c =
  try List.assoc c env () with
  [ Not_found ->
      match c with
      [ '%' -> "%"
      | 'k' -> imcom
      | 'o' ->
          if images_url.val <> "" then images_url.val else imcom ^ "m=IM;v="
      | 'v' -> Version.txt
      | '/' -> ""
      | c -> "%" ^ String.make 1 c ] ]
;

(* "copy_from_etc" is old method; rather use "Templ.copy_from_templ" *)
value rec copy_from_etc env lang imcom ic =
  let cnt = ref 0 in
  try
    while True do {
      match input_char ic with
      [ '%' ->
          let c = input_char ic in
          match c with 
          [ '+' -> incr cnt
          | '#' -> Wserver.wprint "%d" cnt.val
          | 'n' -> Wserver.wprint "%s" (base_len (input_to_semi ic))
          | 'r' ->
              let name = input_line ic in
              match open_etc_file name with
              [ Some ic -> copy_from_etc env lang imcom ic
              | None ->
                  Wserver.wprint
                    "<em>... file not found: \"%s.txt\"</em><br>" name ]
          | c -> Wserver.wprint "%s" (macro_etc env imcom c) ]
      | '[' ->
          let c = input_char ic in
          if c = '\n' then
            let s =
              loop 0 (input_char ic) where rec loop len c =
                if c = ']' then Buff.get len
                else loop (Buff.store len c) (input_char ic)
            in
            let (s, alt) = Translate.inline lang '%' (macro_etc env imcom) s in
            let s = if alt then tnf s else s in
            Wserver.wprint "%s" s
          else
            Wserver.wprint "[%c" c
      | c -> Wserver.wprint "%c" c ]
    }
  with exc ->
    do {
      close_in ic;
      match exc with
      [ End_of_file -> ()
      | exc -> raise exc ]
    }
;

value image_prefix conf =
  if images_url.val <> "" then images_url.val
  else if conf.cgi then conf.command ^ "?m=IM;v="
  else "images"
;

value default_body_prop conf =
  " style=\"background: url('" ^ image_prefix conf ^ "/gwback.jpg')\""
;

value body_prop conf =
  try
    match List.assoc "body_prop" conf.base_env with
    [ "" -> default_body_prop conf
    | s -> " " ^ s ]
  with
  [ Not_found -> default_body_prop conf ]
;

value get_server_string_aux cgi request =
  if not cgi then Wserver.extract_param "host: " '\r' request
  else
    let server_name = try Sys.getenv "SERVER_NAME" with [ Not_found -> "" ] in
    let server_port =
      try Sys.getenv "SERVER_PORT" with [ Not_found | Failure _ -> "80" ]
    in
    if server_port = "80" then server_name
    else server_name ^ ":" ^ server_port
;

value get_request_string_aux cgi request =
  if not cgi then Wserver.extract_param "GET " ' ' request
  else
    let script_name = try Sys.getenv "SCRIPT_NAME" with [ Not_found -> "" ] in
    let query_string =
      try Sys.getenv "QUERY_STRING" with [ Not_found -> "" ]
    in
    script_name ^ "?" ^ query_string
;

value get_server_string conf = get_server_string_aux conf.cgi conf.request;
value get_request_string conf = get_request_string_aux conf.cgi conf.request;

value url_no_index conf base =
  let scratch s = code_varenv (Name.lower (sou base s)) in
  let get_person v =
    match try Some (int_of_string v) with [ Failure _ -> None ] with
    [ Some i ->
        if i >= 0 && i < base.data.persons.len then
          let p = pget conf base (Adef.iper_of_int i) in
          if (conf.hide_names && not (fast_auth_age conf p)) || is_hidden p
          then None
          else
            let f = scratch p.first_name in
            let s = scratch p.surname in
            let oc = string_of_int p.occ in
            Some (f, s, oc)
        else None
    | None -> None ]
  in
  let get_family v =
    match try Some (int_of_string v) with [ Failure _ -> None ] with
    [ Some i ->
        if i >= 0 && i < base.data.families.len then
          if is_deleted_family (base.data.families.get i) then None
          else
            let cpl = base.data.couples.get i in
            let p = pget conf base (father cpl) in
            let f = scratch p.first_name in
            let s = scratch p.surname in
            if f = "" || s = "" then None
            else
              let oc = string_of_int p.occ in
              let u = uget conf base (father cpl) in
              let n =
                loop 0 where rec loop k =
                  if u.family.(k) == Adef.ifam_of_int i then string_of_int k
                  else loop (k + 1)
              in
              Some (f, s, oc, n)
        else None
    | None -> None ]
  in
  let env =
    let rec loop =
      fun
      [ [] -> []
      | [("opt", "no_index") :: l] -> loop l
      | [("dsrc" | "escache" | "oc" | "templ", _) :: l] -> loop l
      | [("i", v) :: l] -> new_env "i" v (fun x -> x) l
      | [("ei", v) :: l] -> new_env "ei" v (fun x -> "e" ^ x) l
      | [(k, v) :: l] when String.length k == 2 && k.[0] == 'i' ->
          let c = String.make 1 k.[1] in new_env k v (fun x -> x ^ c) l
      | [(k, v) :: l]
        when String.length k > 2 && k.[0] == 'e' && k.[1] == 'f' ->
          new_fam_env k v (fun x -> x ^ k) l
      | [kv :: l] -> [kv :: loop l] ]
    and new_env k v c l =
      match get_person v with
      [ Some (f, s, oc) ->
          if oc = "0" then [(c "p", f); (c "n", s) :: loop l]
          else [(c "p", f); (c "n", s); (c "oc", oc) :: loop l]
      | None -> [(k, v) :: loop l] ]
    and new_fam_env k v c l =
      match get_family v with
      [ Some (f, s, oc, n) ->
          let l = loop l in
          let l = if n = "0" then l else [(c "f", n) :: l] in
          if oc = "0" then [(c "p", f); (c "n", s) :: l]
          else [(c "p", f); (c "n", s); (c "oc", oc) :: l]
      | None -> [(k, v) :: loop l] ]
    in
    loop conf.env
  in
  let addr =
    let pref =
      let s = get_request_string conf in
      match rindex s '?' with
      [ Some i -> String.sub s 0 i
      | None -> s ]
    in
    get_server_string conf ^ pref
  in
  let suff =
    List.fold_right
      (fun (x, v) s ->
         let sep = if s = "" then "" else ";" in x ^ "=" ^ v ^ sep ^ s)
      [("lang", conf.lang) :: env] ""
  in
  let suff = if conf.cgi then "b=" ^ conf.bname ^ ";" ^ suff else suff in
  addr ^ "?" ^ suff
;

value include_hed_trl conf base_opt suff =
  let hed_fname =
    let fname = base_path ["lang"; conf.lang] (conf.bname ^ suff) in
    if Sys.file_exists fname then fname
    else base_path ["lang"] (conf.bname ^ suff)
  in
  match try Some (Secure.open_in hed_fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let url () =
        match base_opt with
        [ Some base -> url_no_index conf base
        | None -> get_server_string conf ^ get_request_string conf ]
      in
      let pref () =
        let s = url () in
        match rindex s '?' with
        [ Some i -> String.sub s 0 (i + 1)
        | None -> s ]
      in
      let suff () =
        let s = url () in
        match rindex s '?' with
        [ Some i -> String.sub s (i + 1) (String.length s - i - 1)
        | None -> "" ]
      in
      copy_from_etc
        [('p', pref); ('s', suff); ('t', fun _ -> commd conf);
          ('/', fun _ -> conf.xhs)]
        conf.lang conf.indep_command ic
  | None -> () ]
;

value message_to_wizard conf =
  if conf.wizard || conf.just_friend_wizard then
    let print_file fname =
      let fname = base_path ["etc"; conf.bname] (fname ^ ".txt") in
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic ->
          try while True do { Wserver.wprint "%c" (input_char ic); } with
          [ End_of_file -> close_in ic ]
      | None -> () ]
    in
    do {
      print_file "mess_wizard";
      if conf.user <> "" then
        print_file ("mess_wizard_" ^ conf.user)
      else ();
    }
  else ()
;

value doctype conf =
  match p_getenv conf.base_env "doctype" with
  [ Some "html-4.01-trans" -> "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
 \"http://www.w3.org/TR/html4/loose.dtd\">"
  | Some "html-4.01" -> "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
 \"http://www.w3.org/TR/html4/strict.dtd\">"
  | Some "xhtml-1.0-trans" -> "\
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
 \"http://www.w3.org/TR/xhtml10/DTD/loose.dtd\">"
  | _ -> "\
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" 
 \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" ]
;

value header_without_page_title conf title =
  do {
    html conf;
    nl ();
    Wserver.wprint "%s\n" (doctype conf);
    Wserver.wprint "<html>\n<head>\n";
    Wserver.wprint "  <title>";
    title True;
    Wserver.wprint "</title>\n";
    Wserver.wprint "  <meta name=\"ROBOTS\" content=\"NONE\"%s>\n" conf.xhs;
    Wserver.wprint "  <meta http-equiv=\"Content-Type\" \
                      content=\"text/html; charset=%s\"%s>\n"
      conf.charset conf.xhs;
    Wserver.wprint
      "  <meta http-equiv=\"Content-Style-Type\" content=\"text/css\"%s>\n"
      conf.xhs;
    Wserver.wprint "  \
  <style type=\"text/css\"><!--
    .highlight { color: %s; font-weight: bold }
    hr { border: 0; margin: 0; border-bottom: 1px solid }
    a.date { text-decoration: none; color: black }
  --></style>\n" conf.highlight;
    include_hed_trl conf None ".hed";
    Wserver.wprint "</head>\n";
    let s =
      try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
      [ Not_found -> "" ]
    in
    let s = s ^ body_prop conf in Wserver.wprint "<body%s>" s;
    Wserver.wprint "\n";
    message_to_wizard conf;
  }
;

value header conf title =
  do {
    header_without_page_title conf title;
    Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
    title False;
    Wserver.wprint "</h1>\n";
  }
;

value rheader conf title =
  do {
    header_without_page_title conf title;
    Wserver.wprint "<center><h1><font color=%s>" red_color;
    title False;
    Wserver.wprint "</font></h1></center>\n";
  }
;

value header_no_page_title conf title =
  do {
    header_without_page_title conf title;
    match p_getenv conf.env "title" with
    [ None | Some "" -> ()
    | Some x ->
        do {
          Wserver.wprint "<h1 align=\"center\"><font color=%s>" conf.highlight;
          Wserver.wprint "%s" x;
          Wserver.wprint "</font></h1>\n"
        } ];
  }
;

value http_string s i =
  if start_with s i "http://" then
    let j =
      loop (i + String.length "http://") where rec loop j =
        if j < String.length s then
          match s.[j] with
          [ 'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' | '0'..'9' | '/' | ':' |
            '?' | '%' | ';' | '=' | '_' | '-' | '&' | '.' | '~' | '#' | '+' ->
              loop (j + 1)
          | _ -> j ]
        else j
    in
    match s.[j - 1] with
    [ ':' | ';' | '.' -> Some (j - 1)
    | _ -> Some j ]
  else None
;

value rec followed_by_ident_semi s i =
  if i = String.length s then False
  else
    match s.[i] with
    [ 'a'..'z' | 'A'..'Z' -> followed_by_ident_semi s (i + 1)
    | '#' | '0'..'9' -> followed_by_ident_semi s (i + 1)
    | ';' -> True
    | _ -> False ]
;

value expand_ampersand buff s =
  loop 0 where rec loop i =
    if i = String.length s then ()
    else do {
      if s.[i] = '&' then Buffer.add_string buff "&amp;"
      else Buffer.add_char buff s.[i];
      loop (i + 1)
    }
;

value email_addr s i =
  let rec before_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' ->
          before_at False (i + 1)
      | '@' -> if empty then None else after_at True (i + 1)
      | _ -> None ]
  and after_at empty i =
    if i = String.length s then None
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> after_at False (i + 1)
      | '.' -> if empty then None else after_dot 0 (i + 1)
      | _ -> None ]
  and after_dot len i =
    if i = String.length s then Some (len, i)
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' ->
          after_dot (len + 1) (i + 1)
      | _ -> Some (len, i) ]
  in
  match before_at True i with
  [ Some (len, i) ->
      let (len, i) =
        if len > 0 && s.[i - 1] = '.' then (len - 1, i - 1) else (len, i)
      in
      if len = 0 then None else Some i
  | None -> None ]
;

value tag_id s i =
  loop i 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '!' | '-' ->
          loop (i + 1) (Buff.store len (Char.lowercase s.[i]))
      | _ -> if len = 0 then loop (i + 1) 0 else Buff.get len ]
;

value default_good_tag_list =
  ["a"; "b"; "blockquote"; "br"; "center"; "cite"; "dd"; "dir"; "div"; "dl";
   "dt"; "em"; "font"; "hr"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "i"; "img";
   "li"; "ol"; "p"; "pre"; "span"; "strong"; "sup"; "table"; "tbody"; "td";
   "tr"; "tt"; "u"; "ul"; "!--"]
;

value allowed_tags_file = ref "";

value good_tag_list_fun () =
  if allowed_tags_file.val <> "" then
    match
      try Some (open_in allowed_tags_file.val) with [ Sys_error _ -> None ]
    with
    [ Some ic ->
        loop [] where rec loop tags =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some tg -> loop [String.lowercase tg :: tags]
          | None -> do { close_in ic; tags } ]
    | None -> default_good_tag_list ]
  else default_good_tag_list
;

value good_tags_list = Lazy.lazy_from_fun good_tag_list_fun;
value good_tag s i = List.mem (tag_id s i) (Lazy.force good_tags_list);

module Lbuff = Buff.Make (struct value buff = ref (String.create 80); end);

value filter_html_tags s =
  loop 0 0 where rec loop len i =
    if i < String.length s then
      if s.[i] = '<' && not (good_tag s (i + 1)) then
        loop (Lbuff.mstore len "&lt;") (i + 1)
      else loop (Lbuff.store len s.[i]) (i + 1)
    else Lbuff.get len
;

value get_variable s i =
  loop 0 i where rec loop len i =
    if i == String.length s then (Buff.get len, i)
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c ->
          loop (Buff.store len c) (i + 1)
      | ';' -> (Buff.get len, i + 1)
      | _ -> (Buff.get len, i) ]
;

type tag_type = [ In_a_href | In_norm | Out ];

value expand_env =
  let buff = Buffer.create 30 in
  fun conf s ->
    match p_getenv conf.base_env "expand_env" with
    [ Some "yes" ->
        let _ : unit = Buffer.clear buff in
        loop 0 where rec loop i =
          if i = String.length s then Buffer.contents buff
          else if i + 1 < String.length s && s.[i] = '$' && s.[i+1] = '{' then
            try
              let j = String.index_from s (i+1) '}' in
              let v = Sys.getenv (String.sub s (i + 2) (j - i - 2)) in
              do { Buffer.add_string buff v; loop (j + 1) }
            with
            [ Not_found -> do { Buffer.add_char buff s.[i]; loop (i + 1) } ]
          else
            do { Buffer.add_char buff s.[i]; loop (i + 1) }
   | _ -> s ]
;

value string_with_macros conf env s =
  let buff = Buffer.create 1000 in
  loop Out 0 where rec loop tt i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then
        let i =
          try
            do { Buffer.add_string buff (List.assoc s.[i + 1] env ()); i + 2 }
          with
          [ Not_found ->
              match s.[i + 1] with
              [ 's' -> do { Buffer.add_string buff (commd conf); i + 2 }
              | 'v' ->
                  let (k, j) = get_variable s (i + 2) in
                  let (v, i) =
                    let v =
                      try
                        let v = List.assoc ("var_" ^ k) conf.base_env in
                        Some (expand_env conf v)
                      with
                      [ Not_found -> None ]
                    in
                    match v with
                    [ Some v -> (v, j)
                    | None -> ("%", i + 1) ]
                  in
                  do { Buffer.add_string buff v; i }
              | '%' -> do { Buffer.add_string buff "%"; i + 2 }
              | _ -> do { Buffer.add_string buff "%"; i + 1 } ] ]
        in
        loop tt i
      else
        match tt with
        [ In_a_href ->
            let tt = if start_with s i "</a>" then Out else In_a_href in
            do { Buffer.add_char buff s.[i]; loop tt (i + 1) }
        | In_norm ->
            let tt = if s.[i] = '>' then Out else In_norm in
            do { Buffer.add_char buff s.[i]; loop tt (i + 1) }
        | Out ->
            match http_string s i with
            [ Some j ->
                let x = String.sub s i (j - i) in
                do {
                  bprintf buff "<a href=\"%s\">" x;
                  expand_ampersand buff x;
                  bprintf buff "</a>";
                  loop Out j
                }
            | None ->
                match email_addr s i with
                [ Some j ->
                    let x = String.sub s i (j - i) in
                    do {
                      bprintf buff "<a href=\"mailto:%s\">%s</a>" x x;
                      loop Out j
                    }
                | None ->
                    let tt =
                      if start_with s i "<a href=" ||
                         start_with s i "<a\nhref=" then
                        In_a_href
                      else if s.[i] = '<' then In_norm
                      else Out
                    in
                    do {
                      if s.[i] = '&' &&
                      not (followed_by_ident_semi s (i + 1)) then
                        Buffer.add_string buff "&amp;"
                      else
                        Buffer.add_char buff s.[i];
                      loop tt (i + 1)
                    } ] ] ]
    else filter_html_tags (Buffer.contents buff)
;

value setup_link conf =
  let s = Wserver.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    let s = "http://" ^ String.sub s 0 i ^ ":2316/" in
    "<a href=\"" ^ s ^ "gwsetup?v=main.htm\">gwsetup</a>"
  with
  [ Not_found -> "" ]
;

value compilation_time_hook = ref (fun _ -> "");
value compilation_time conf =
  match p_getenv conf.base_env "display_compilation_time" with
  [ Some "on" -> compilation_time_hook.val conf
  | _ -> "" ]
;

value print_copyright conf =
  let env =
    [('s', fun _ -> commd conf);
     ('c', fun _ -> compilation_time conf);
     ('C', fun _ -> if Gutil.utf_8_db.val then "&copy;" else "(c)");
     ('d',
      fun _ ->
        let s =
          if conf.cancel_links then ""
          else
            sprintf " - <a href=\"%sm=DOC\">DOC</a>" (commd conf)
        in
        if not conf.setup_link then s
        else s ^ " - " ^ setup_link conf);
     ('/', fun _ -> conf.xhs)]
  in
  match open_etc_file "copyr" with
  [ Some ic -> copy_from_etc env conf.lang conf.indep_command ic
  | None ->
      do {
        html_p conf;
        Wserver.wprint "
<hr><font size=\"-1\"><em>Copyright (c) 1998-2005 INRIA -
GeneWeb %s</em></font>" Version.txt;
        html_br conf;
      } ]
;

value gen_trailer with_logo conf =
  do {
    if not with_logo then ()
    else
      Wserver.wprint "\
<div>
<a href=\"%s\"><img src=\"%s/gwlogo.png\"
 alt=\"...\" width=\"64\" height=\"72\" style=\"border:0;float:%s\"%s></a>
<br%s>
</div>
" (commd conf) (image_prefix conf) conf.right
      conf.xhs conf.xhs;
    print_copyright conf;
    include_hed_trl conf None ".trl";
    Wserver.wprint "</body>\n</html>\n";
  }
;

value trailer = gen_trailer True;

value menu_threshold = 20;

value is_number t =
  match t.[0] with
  [ '1'..'9' -> True
  | _ -> False ]
;

value hexa_string s =
  let s' = String.create (2 * String.length s) in
  do {
    for i = 0 to String.length s - 1 do {
      s'.[2*i] := "0123456789ABCDEF".[Char.code s.[i] / 16];
      s'.[2*i+1] := "0123456789ABCDEF".[Char.code s.[i] mod 16];
    };
    s'
  }
;

value print_alphab_list conf crit print_elem liste =
  let len = List.length liste in
  do {
    if len > menu_threshold then do {
      Wserver.wprint "<p>\n";
      let _ =
        List.fold_left
          (fun last e ->
             let t = crit e in
             let same_than_last =
               match last with
               [ Some t1 -> t = t1
               | _ -> False ]
             in
             do {
               if not same_than_last then
                 Wserver.wprint "<a href=\"#i%s\">%s</a>\n" (hexa_string t) t
               else ();
               Some t
             })
          None liste
      in
      ();
      Wserver.wprint "</p>\n";
    }
    else ();
    Wserver.wprint "<ul>\n";
    let _ =
      List.fold_left
        (fun last e ->
           let t = crit e in
           let same_than_last =
             match last with
             [ Some t1 -> t = t1
             | _ -> False ]
           in
           do {
             if len > menu_threshold || is_number t then do {
               match last with
               [ Some _ ->
                   if not same_than_last then Wserver.wprint "</ul>\n</li>\n"
                   else ()
               | _ -> () ];
               if not same_than_last then do {
                 Wserver.wprint "<li>\n";
                 Wserver.wprint "<a id=\"i%s\">%s</a>\n" (hexa_string t) t;
                 Wserver.wprint "<ul>\n";
               }
               else ();
             }
             else ();
             Wserver.wprint "<li>\n  ";
             print_elem e;
             Wserver.wprint "</li>\n";
             Some t
           })
        None liste
    in
    ();
    if len > menu_threshold then Wserver.wprint "</ul>\n</li>\n" else ();
    Wserver.wprint "</ul>\n";
  }
;

value parent conf base p a =
  match a.public_name with
  [ n when sou base n <> "" -> sou base n ^ person_title conf base a
  | _ ->
      if conf.hide_names && not (fast_auth_age conf a) then "x x"
      else
        p_first_name base a ^
          (if p.surname <> a.surname then " " ^ p_surname base a else "") ]
;

value print_parent conf base p fath moth =
  let s =
    match (fath, moth) with
    [ (Some fath, None) -> parent conf base p fath
    | (None, Some moth) -> parent conf base p moth
    | (Some fath, Some moth) ->
        parent conf base p fath ^ " " ^ transl_nth conf "and" 0 ^ " " ^
          parent conf base p moth
    | _ -> "" ]
  in
  let is = index_of_sex p.sex in
  Wserver.wprint "%s"
    (transl_a_of_gr_eq_gen_lev conf
       (transl_nth conf "son/daughter/child" is) s)
;

value specify_homonymous conf base p =
  let is = index_of_sex p.sex in
  match (p.public_name, p.qualifiers) with
  [ (n, [nn :: _]) when sou base n <> "" ->
      Wserver.wprint "%s <em>%s</em>" (sou base n) (sou base nn)
  | (_, [nn :: _]) ->
      Wserver.wprint "%s <em>%s</em>" (p_first_name base p) (sou base nn)
  | (n, []) when sou base n <> "" -> Wserver.wprint "%s" (sou base n)
  | (_, []) ->
      let a = aget conf base p.cle_index in
      let ifam =
        match parents a with
        [ Some ifam ->
            let cpl = coi base ifam in
            let fath =
              let fath = pget conf base (father cpl) in
              if p_first_name base fath = "?" then None else Some fath
            in
            let moth =
              let moth = pget conf base (mother cpl) in
              if p_first_name base moth = "?" then None else Some moth
            in
            Some (fath, moth)
        | None -> None ]
      in
      match ifam with
      [ Some (None, None) | None ->
          let u = uget conf base p.cle_index in
          let rec loop i =
            if i < Array.length u.family then
              let des = doi base u.family.(i) in
              let conjoint = spouse p.cle_index (coi base u.family.(i)) in
              let ct = des.children in
              if Array.length ct > 0 then
                let enfant = pget conf base ct.(0) in
                let (child_fn, child_sn) =
                  if conf.hide_names && not (fast_auth_age conf enfant) then
                    ("x", " x")
                  else
                    (p_first_name base enfant,
                     if p.surname <> enfant.surname then
                       " " ^ p_surname base enfant
                     else "")
                in
                Wserver.wprint "%s"
                  (transl_a_of_b conf
                     (transl_nth conf "father/mother" is)
                     (child_fn ^ child_sn))
              else
                let conjoint = pget conf base conjoint in
                if p_first_name base conjoint <> "?" ||
                   p_surname base conjoint <> "?" then
                  Wserver.wprint "%s"
                    (transl_a_of_b conf
                       (transl_nth conf "husband/wife" is)
                       (p_first_name base conjoint ^ " " ^
                          p_surname base conjoint))
                else loop (i + 1)
            else Wserver.wprint "..."
          in
          loop 0
      | Some (fath, moth) -> print_parent conf base p fath moth ] ]
;

(* fix system bug: string_of_float 17.97 = "17.969999999999" *)
value my_string_of_float f = sprintf "%.6g" f;

value string_of_decimal_num conf f =
  let s = my_string_of_float f in
  let b = Buffer.create 20 in
  let rec loop i =
    if i == String.length s then Buffer.contents b
    else do {
      match s.[i] with
      [ '.' ->
          if i == String.length s - 1 then ()
          else Buffer.add_string b (transl conf "(decimal separator)")
      | x -> Buffer.add_char b x ];
      loop (i + 1)
    }
  in
  loop 0
;

value personal_image_file_name bname str =
  Filename.concat (base_path ["images"] bname) str
;

value source_image_file_name bname str =
  let fname1 =
    List.fold_right Filename.concat [base_path ["src"] bname; "images"] str
  in
  let fname2 =
    List.fold_right Filename.concat [Secure.base_dir (); "src"; "images"] str
  in
  if Sys.file_exists fname1 then fname1 else fname2
;

value image_file_name str =
  let fname1 =
    List.fold_right Filename.concat [Secure.base_dir (); "images"] str
  in
  if Sys.file_exists fname1 then fname1
  else search_in_lang_path (Filename.concat "images" str)
;

value png_image_size ic =
  let magic = let s = String.create 4 in do { really_input ic s 0 4; s } in
  if magic = "\137PNG" then do {
    seek_in ic 16;
    let wid = input_binary_int ic in
    let hei = input_binary_int ic in
    Some (wid, hei)
  }
  else None
;

value gif_image_size ic =
  let magic = let s = String.create 4 in do { really_input ic s 0 4; s } in
  if magic = "GIF8" then do {
    seek_in ic 6;
    let wid = let x = input_byte ic in input_byte ic * 256 + x in
    let hei = let x = input_byte ic in input_byte ic * 256 + x in
    Some (wid, hei)
  }
  else None
;

value jpeg_image_size ic =
  let magic =
    let str = String.create 10 in do { really_input ic str 0 10; str }
  in
  if Char.code magic.[0] = 0xff && Char.code magic.[1] = 0xd8 &&
     (let m = String.sub magic 6 4 in m = "JFIF" || m = "Exif") then
    let exif_type = String.sub magic 6 4 = "Exif" in
    let rec loop found =
      do {
        while Char.code (input_char ic) <> 0xFF do { () };
        let ch =
          loop (input_char ic) where rec loop ch =
            if Char.code ch = 0xFF then loop (input_char ic) else ch
        in
        if Char.code ch = 0xC0 || Char.code ch = 0xC3 then
          if exif_type && not found then
            loop True
          else do {
            for i = 1 to 3 do { let _ = input_char ic in () };
            let a = input_char ic in
            let b = input_char ic in
            let c = input_char ic in
            let d = input_char ic in
            let wid = Char.code c lsl 8 lor Char.code d in
            let hei = Char.code a lsl 8 lor Char.code b in
            Some (wid, hei)
          }
        else
          let a = input_char ic in
          let b = input_char ic in
          let len = Char.code a lsl 8 lor Char.code b in
          let len = if len >= 32768 then 0 else len in
          do {
            for i = 1 to len - 2 do { let _ = input_char ic in () };
            if Char.code ch <> 0xDA then loop found else None
          }
      }
    in
    loop False
  else None
;

value image_size fname =
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let r =
        try
          let sz = jpeg_image_size ic in
          let sz =
            if sz = None then do { seek_in ic 0; png_image_size ic } else sz
          in
          if sz = None then do { seek_in ic 0; gif_image_size ic } else sz
        with
        [ End_of_file -> None ]
      in
      do { close_in ic; r }
  | None -> None ]
;

value limited_image_size max_wid max_hei fname size =
  match
    if fname = "" then size
    else image_size fname
  with
  [ Some (wid, hei) ->
      let (wid, hei) =
        if hei > max_hei then
          let wid = wid * max_hei / hei in let hei = max_hei in (wid, hei)
        else (wid, hei)
      in
      let (wid, hei) =
        if wid > max_wid then
          let hei = hei * max_wid / wid in let wid = max_wid in (wid, hei)
        else (wid, hei)
      in
      Some (wid, hei)
  | None -> None ]
;

value up_fname conf = "up.jpg";

value link_to_referer conf =
  let referer = Wserver.extract_param "referer: " '\n' conf.request in
  if referer <> "" then
    let fname = "left.jpg" in
    let wid_hei =
      match image_size (image_file_name fname) with
      [ Some (wid, hei) ->
          " width=" ^ string_of_int wid ^ " height=" ^ string_of_int hei
      | None -> "" ]
    in
    "<a href=\"" ^ referer ^ "\"><img src=\"" ^ image_prefix conf ^ "/" ^
      fname ^ "\"" ^ wid_hei ^ " alt=\"&lt;&lt;\"></a>\n"
  else ""
;

value gen_print_link_to_welcome f conf right_aligned =
  if conf.cancel_links then ()
  else do {
    let fname = up_fname conf in
    let wid_hei =
      match image_size (image_file_name fname) with
      [ Some (wid, hei) ->
          " width=\"" ^ string_of_int wid ^ "\" height=\"" ^
          string_of_int hei ^ "\""
      | None -> "" ]
    in
    if right_aligned then
      Wserver.wprint "<div style=\"float:%s\">\n" conf.right
    else Wserver.wprint "<p>\n";
    f ();
    let str = link_to_referer conf in
    if str = "" then () else Wserver.wprint "%s" str;
    Wserver.wprint "<a href=\"%s\">" (commd_no_params conf);
    Wserver.wprint "<img src=\"%s/%s\"%s alt=\"^^\"%s>" (image_prefix conf)
      fname wid_hei conf.xhs;
    Wserver.wprint "</a>\n";
    if right_aligned then Wserver.wprint "</div>\n"
    else Wserver.wprint "</p>\n"
  }
;

value print_link_to_welcome = gen_print_link_to_welcome (fun () -> ());

value header_link_welcome conf title =
  do {
    header_without_page_title conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<h1 style=\"text-align:center\" class=\"highlight\">";
    title False;
    Wserver.wprint "</h1>\n";
  }
;

value incorrect_request conf =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "incorrect request"))
  in
  do {
    header conf title;
    Wserver.wprint "<p>\n";
    print_link_to_welcome conf False;
    Wserver.wprint "</p>\n";
    trailer conf
  }
;

value find_person_in_env conf base suff =
  match p_getint conf.env ("i" ^ suff) with
  [ Some i ->
      if i >= 0 && i < base.data.persons.len then
        let p = pget conf base (Adef.iper_of_int i) in
        if is_hidden p then None
        else Some p
      else None
  | None ->
      match
        (p_getenv conf.env ("p" ^ suff), p_getenv conf.env ("n" ^ suff))
      with
      [ (Some p, Some n) ->
          let occ =
            match p_getint conf.env ("oc" ^ suff) with
            [ Some oc -> oc
            | None -> 0 ]
          in
          let k = p ^ " " ^ n in
          let xl =
            List.fold_left
              (fun l ip ->
                 let p = pget conf base ip in
                 if is_hidden p then l else [p :: l])
            [] (person_ht_find_all base k)
          in
          try
            let r =
              List.find
                (fun x ->
                   Name.lower (p_first_name base x) = Name.lower p &&
                   Name.lower (p_surname base x) = Name.lower n &&
                   x.occ == occ)
                xl
            in
            if not conf.hide_names || authorized_age conf base r then Some r
            else None
          with
          [ Not_found -> None ]
      | _ -> None ] ]
;

value find_sosa_ref conf base =
  match find_person_in_env conf base "z" with
  [ Some p -> Some p
  | None ->
      match p_getenv conf.base_env "default_sosa_ref" with
      [ Some n ->
          if n = "" then None
          else
            match person_ht_find_all base n with
            [ [ip] ->
                let p = pget conf base ip in
                if is_hidden p then None
                else Some p
            | _ -> None ]
      | None -> None ] ]
;

value create_topological_sort conf base =
  match p_getenv conf.env "opt" with
  [ Some "no_tsfile" ->
      let _ = base.data.ascends.array () in
      let _ = base.data.couples.array () in
      Consang.topological_sort base (aget conf)
  | Some "no_tstab" -> Array.create base.data.persons.len 0
  | _ ->
      let bfile = base_path [] (conf.bname ^ ".gwb") in
      lock (Iobase.lock_file bfile) with
      [ Accept ->
          let tstab_file =
            if conf.use_restrict then Filename.concat bfile "tstab_visitor"
            else Filename.concat bfile "tstab"
          in
          let r =
            match
              try
                Some (Secure.open_in_bin tstab_file) with
                [ Sys_error _ -> None ]
            with
            [ Some ic ->
                let r =
                  try Some (Marshal.from_channel ic) with
                  [ End_of_file | Failure _ -> None ]
                in
                do { close_in ic; r }
            | None -> None ]
          in
          match r with
          [ Some tstab -> tstab
          | None ->
              let _ = base.data.ascends.array () in
              let _ = base.data.couples.array () in
              let tstab = Consang.topological_sort base (aget conf) in
              do {
                if conf.use_restrict then
                  base.data.visible.v_write ()
                else ();
                match
                  try Some (Secure.open_out_bin tstab_file) with
                  [ Sys_error _ -> None ]
                with
                [ Some oc ->
                    do {
                      Marshal.to_channel oc tstab [Marshal.No_sharing];
                      close_out oc;
                    }
                | None -> () ];
                tstab
              } ]
      | Refuse ->
          let _ = base.data.ascends.array () in
          let _ = base.data.couples.array () in
          Consang.topological_sort base (aget conf) ] ]
;

value branch_of_sosa conf base ip n =
  do {
    if Num.eq n Num.zero then invalid_arg "branch_of_sosa" else ();
    let rec expand bl n =
      if Num.eq n Num.one then bl else expand [Num.even n :: bl] (Num.half n)
    in
    let rec loop ipl ip sp =
      fun
      [ [] -> Some [(ip, sp) :: ipl]
      | [goto_fath :: nl] ->
          match parents (aget conf base ip) with
          [ Some ifam ->
              let cpl = coi base ifam in
              if goto_fath then loop [(ip, sp) :: ipl] (father cpl) Male nl
              else loop [(ip, sp) :: ipl] (mother cpl) Female nl
          | _ -> None ] ]
    in
    loop [] ip (pget conf base ip).sex (expand [] n)
  }
;

value sosa_of_branch ipl =
  do {
    if ipl = [] then failwith "sosa_of_branch" else ();
    let ipl = List.tl (List.rev ipl) in
    List.fold_left
      (fun b (ip, sp) ->
         let b = Num.twice b in
         match sp with
         [ Male -> b
         | Female -> Num.inc b 1
         | Neuter -> assert False ])
      Num.one ipl
  }
;

value space_to_unders = Gutil.tr ' ' '_';

value default_image_name_of_key fnam surn occ =
  let f = space_to_unders (Name.lower fnam) in
  let s = space_to_unders (Name.lower surn) in
  f ^ "." ^ string_of_int occ ^ "." ^ s
;

value default_image_name base p =
  default_image_name_of_key (p_first_name base p) (p_surname base p) p.occ
;

value auto_image_file conf base p =
  let s = default_image_name base p in
  let f = Filename.concat (base_path ["images"] conf.bname) s in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None
;

value image_and_size conf base p image_size =
  if not conf.no_image && authorized_age conf base p then
    match sou base p.image with
    [ "" ->
        match auto_image_file conf base p with
        [ Some f -> Some (True, f, image_size f None)
        | None -> None ]
    | s ->
        let (s, size) =
          let l = String.length s - 1 in
          if s.[l] = ')' then
            try
              let pos1 = String.index s '(' in
              let pos2 = String.index_from s pos1 'x' in
              let wid = String.sub s (pos1+1) (pos2-pos1-1) in
              let hei = String.sub s (pos2+1) (l-pos2-1) in
              let size = Some (int_of_string wid, int_of_string hei) in
              (String.sub s 0 pos1, image_size "" size)
            with
            [ Not_found | Failure _ -> (s, None) ]
          else (s, None)
        in
        let http = "http://" in
        if String.length s > String.length http &&
           String.sub s 0 (String.length http) = http then
          Some (False, s, size)
        else if Filename.is_implicit s then
          match
            try Some (List.assoc "images_path" conf.base_env) with
            [ Not_found -> None ]
          with
          [ Some p when p <> "" -> Some (False, p ^ s, size)
          | _ ->
              let fname = personal_image_file_name conf.bname s in
              if Sys.file_exists fname then
                Some (True, fname, image_size fname None)
              else None ]
        else None ]
  else None
;

value has_image conf base p =
  if not conf.no_image && authorized_age conf base p then
    p.image <> Adef.istr_of_int 0 || auto_image_file conf base p <> None
  else False
;

value gen_only_printable or_nl s =
  let s' = String.create (String.length s) in
  do {
    for i = 0 to String.length s - 1 do {
      s'.[i] :=
        if Gutil.utf_8_db.val && Char.code s.[i] > 127 then s.[i]
        else
          match s.[i] with
          [ ' '..'~' | '\160'..'\255' -> s.[i]
          | '\n' -> if or_nl then '\n' else ' '
          | _ -> ' ' ]
    };
    strip_spaces s'
  }
;

value only_printable_or_nl = gen_only_printable True;
value only_printable = gen_only_printable False;

value relation_type_text conf t n =
  match t with
  [ Adoption ->
      transl_nth conf "adoptive father/adoptive mother/adoptive parents" n
  | Recognition ->
      transl_nth conf
        "recognizing father/recognizing mother/recognizing parents" n
  | CandidateParent ->
      transl_nth conf "candidate father/candidate mother/candidate parents" n
  | GodParent -> transl_nth conf "godfather/godmother/godparents" n
  | FosterParent ->
      transl_nth conf "foster father/foster mother/foster parents" n ]
;

value rchild_type_text conf t n =
  match t with
  [ Adoption ->
      transl_nth conf "adoptive son/adoptive daughter/adoptive child" n
  | Recognition ->
      transl_nth conf "recognized son/recognized daughter/recognized child" n
  | CandidateParent ->
      transl_nth conf "candidate son/candidate daughter/candidate child" n
  | GodParent -> transl_nth conf "godson/goddaughter/godchild" n
  | FosterParent ->
      transl_nth conf "foster son/foster daughter/foster child" n ]
;

value wprint_hidden conf pref name valu =
  Wserver.wprint "<input type=\"hidden\" name=\"%s%s\" value=\"%s\"%s>\n"
    pref name (quote_escaped valu) conf.xhs
;

value wprint_hidden_person conf base pref p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if accessible_by_key conf base p first_name surname then do {
    wprint_hidden conf pref "p" (Name.lower first_name);
    wprint_hidden conf pref "n" (Name.lower surname);
    if p.occ > 0 then wprint_hidden conf pref "oc" (string_of_int p.occ)
    else ();
  }
  else
    wprint_hidden conf pref "i" (string_of_int (Adef.int_of_iper p.cle_index))
;

exception Ok;

value has_nephews_or_nieces conf base p =
  try
    let a = aget conf base p.cle_index in
    match parents a with
    [ Some ifam ->
        let des = doi base ifam in
        do {
          Array.iter
            (fun ip ->
               if ip == p.cle_index then ()
               else
                 Array.iter
                   (fun ifam ->
                      if Array.length (doi base ifam).children > 0 then
                        raise Ok
                      else ())
                   (uget conf base ip).family)
            des.children;
          False
        }
    | _ -> False ]
  with
  [ Ok -> True ]
;

value browser_doesnt_have_tables conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase user_agent = "lynx"
;

(* Printing for browsers without tables *)

value pre_text_size txt =
  let rec normal len i =
    if i = String.length txt then len
    else if txt.[i] = '<' then in_tag len (i + 1)
    else if txt.[i] = '&' then in_char (len + 1) (i + 1)
    else normal (len + 1) (i + 1)
  and in_tag len i =
    if i = String.length txt then len
    else if txt.[i] = '>' then normal len (i + 1)
    else in_tag len (i + 1)
  and in_char len i =
    if i = String.length txt then len
    else if txt.[i] = ';' then normal len (i + 1)
    else in_char len (i + 1)
  in
  normal 0 0
;

value print_pre_center sz txt =
  do {
    for i = 1 to (sz - pre_text_size txt) / 2 do { Wserver.wprint " " };
    Wserver.wprint "%s\n" txt;
  }
;

value print_pre_left sz txt =
  let tsz = pre_text_size txt in
  do {
    if tsz < sz / 2 - 1 then
      for i = 2 to (sz / 2 - 1 - tsz) / 2 do { Wserver.wprint " " }
    else ();
    Wserver.wprint " %s\n" txt;
  }
;

value print_pre_right sz txt =
  let tsz = pre_text_size txt in
  do {
    if tsz < sz / 2 - 1 then do {
      for i = 1 to sz / 2 do { Wserver.wprint " " };
      for i = 1 to (sz / 2 - 1 - tsz) / 2 do { Wserver.wprint " " };
      ()
    }
    else for i = 1 to sz - pre_text_size txt - 1 do { Wserver.wprint " " };
    Wserver.wprint " %s\n" txt;
  }
;

value of_course_died conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> conf.today.year - d.year > 120
  | _ -> False ]
;

value relation_txt conf sex fam =
  let is = index_of_sex sex in
  match fam.relation with
  [ NotMarried | NoSexesCheckNotMarried ->
      ftransl_nth conf "relationship%t to" is
  | Married | NoSexesCheckMarried -> ftransl_nth conf "married%t to" is
  | Engaged -> ftransl_nth conf "engaged%t to" is
  | NoMention ->
      let s = "%t " ^ transl conf "with" in
      valid_format "%t" s ]
;

value escache_value conf =
  let bdir = base_path [] (conf.bname ^ ".gwb") in
  let s =
    try Unix.stat (Filename.concat bdir "patches") with
    [ Unix.Unix_error _ _ _ -> Unix.stat (Filename.concat bdir "base") ]
  in
  let v =
    int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int))
  in
  string_of_int v
;

value adm_file f =
  List.fold_right Filename.concat [cnt_dir.val; "cnt"] f
;

value std_date conf =
  let (hour, min, sec) = conf.time in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d" conf.today.year
    conf.today.month conf.today.day hour min sec
;

value read_wf_trace fname =
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let r = ref [] in
      do {
        try while True do { r.val := [input_line ic :: r.val] } with
        [ End_of_file -> close_in ic ];
        List.rev r.val
      }
  | None -> [] ]
;

value write_wf_trace fname wt =
  let oc = Secure.open_out fname in
  do {
    List.iter (fun (dt, u) -> fprintf oc "%s %s\n" dt u) wt;
    close_out oc;
  }
;

value update_wf_trace conf fname =
  let dt = std_date conf in
  let wt =
    let r = read_wf_trace fname in
    let dtlen = String.length dt in
    let rec loop found r =
      fun
      [ [x :: l] ->
          if String.length x > dtlen + 2 then
            let u =
              String.sub x (dtlen + 1) (String.length x - dtlen - 1)
            in
            if u = conf.user then loop True [(dt, u) :: r] l
            else loop found [(String.sub x 0 dtlen, u) :: r] l
          else loop found r l
      | [] -> if found then r else [(dt, conf.user) :: r] ]
    in
    loop False [] r
  in
  write_wf_trace fname (List.sort (fun x y -> compare y x) wt)
;

value commit_patches conf base =
  do {
    base.func.commit_patches ();
    conf.henv :=
      List.map
        (fun (k, v) ->
           if k = "escache" then (k, escache_value conf) else (k, v))
        conf.henv
    ;
    if conf.user <> "" then
      let wpf =
        try List.assoc "wizard_passwd_file" conf.base_env with
        [ Not_found -> "" ]
      in
      if wpf <> "" then
        let fname = adm_file (conf.bname ^ "_u.txt") in
        update_wf_trace conf fname
      else ()
    else ();
  }
;

(* List selection bullets *)

value bullet_sel_txt = "<tt>o</tt>";
value bullet_unsel_txt = "<tt>+</tt>";
value bullet_nosel_txt = "<tt>o</tt>";
value print_selection_bullet conf =
  fun
  [ Some (txt, sel) ->
      let req =
        List.fold_left
          (fun req (k, v) ->
             if not sel && k = "u" && v = txt then req
             else
               let s = k ^ "=" ^ v in
               if req = "" then s else req ^ ";" ^ s)
          "" conf.env
      in
      do {
        Wserver.wprint "<a id=\"i%s\" href=\"%s%s%s%s\">" txt (commd conf) req
          (if sel then ";u=" ^ txt else "")
          (if sel || List.mem_assoc "u" conf.env then "#i" ^ txt else "");
        Wserver.wprint "%s"
          (if sel then bullet_sel_txt else bullet_unsel_txt);
        Wserver.wprint "</a>\n";
      }
  | None -> Wserver.wprint "%s\n" bullet_nosel_txt ]
;

value unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
       try
         if k = "u" then [int_of_string v :: sl]
         else sl
       with
       [ Failure _ -> sl ])
    [] conf.env
;

value short_f_month m =
  match m with
  [ 1 -> "VD"
  | 2 -> "BR"
  | 3 -> "FM"
  | 4 -> "NI"
  | 5 -> "PL"
  | 6 -> "VT"
  | 7 -> "GE"
  | 8 -> "FL"
  | 9 -> "PR"
  | 10 -> "ME"
  | 11 -> "TH"
  | 12 -> "FT"
  | 13 -> "JC"
  | _ -> "" ]
;
