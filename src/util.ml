(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: util.ml,v 3.84 2001-01-25 13:35:16 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;
open Gutil;

value lang_dir = ref Filename.current_dir_name;
value base_dir = ref Filename.current_dir_name;
value doc_dir = ref "";
value cnt_dir = ref "";
value images_url = ref "";

value secure s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '<' | '>' -> True
      | _ -> need_code (i + 1) ]
    else False
  in
  if need_code 0 then
    loop 0 0 where rec loop i len =
      if i = String.length s then Buff.get len
      else
        let (len, next_i) =
          match s.[i] with
          [ '<' -> (Buff.mstore len "&lt;", i + 1)
          | '>' -> (Buff.mstore len "&gt;", i + 1)
          | c -> (Buff.store len c, i + 1) ]
        in
        loop next_i len
  else s
;

value html_br conf =
  do Wserver.wprint "<br>";
     Wserver.wprint "\n";
  return ()
;

value html_p conf =
  do Wserver.wprint "<p>";
     Wserver.wprint "\n";
  return ()
;

value html_li conf =
  do Wserver.wprint "<li>";
     Wserver.wprint "\n";
  return ()
;

value nl () = Wserver.wprint "\013\010";

value html conf =
  let charset = if conf.charset = "" then "iso-8859-1" else conf.charset in
  do if not conf.cgi then
       do Wserver.wprint "HTTP/1.0 200 OK"; nl ();
          Wserver.wprint "Server: GeneWeb/%s" Version.txt; nl ();
       return ()
     else ();
     Wserver.wprint "Content-type: text/html; charset=%s" charset;
     nl (); nl ();
  return ()
;

value unauthorized conf auth_type =
  do if conf.cgi then
       Wserver.wprint "Content-type: text/html; charset=%s" conf.charset
     else
       do Wserver.wprint "HTTP/1.0 401 Unauthorized"; nl ();
          Wserver.wprint "WWW-Authenticate: Basic realm=\"%s\"" auth_type;
       return ();
     nl (); nl ();
     Wserver.wprint "<head><title>Access failed</title></head>\n";
     Wserver.wprint "<body><h1>Access failed</h1>\n";
     Wserver.wprint "<ul><li>%s</ul>\n" auth_type;
     Wserver.wprint "</body>\n";
  return ()
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
        [ '"' -> do String.blit "&#034;" 0 s1 i1 6; return i1 + 6
        | '&' -> do String.blit "&amp;" 0 s1 i1 5; return i1 + 5
        | '<' -> do String.blit "&lt;" 0 s1 i1 4; return i1 + 4
        | '>' -> do String.blit "&gt;" 0 s1 i1 4; return i1 + 4
        | c -> do s1.[i1] := c; return succ i1 ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

value hidden_env conf =
  List.iter
    (fun (k, v) ->
       Wserver.wprint "<input type=hidden name=%s value=\"%s\">\n" k
         (quote_escaped (decode_varenv v)))
    (conf.henv @ conf.senv)
;

value p_getenv env label =
  try Some (decode_varenv (List.assoc (decode_varenv label) env)) with
  [ Not_found -> None ]
;

value p_getint env label =
  match p_getenv env label with
  [ Some s -> try Some (int_of_string (strip_spaces s)) with _ -> None
  | None -> None ]
;

value parent_has_title base p =
  let a = aoi base p.cle_index in
  match a.parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base cpl.father in
      let moth = poi base cpl.mother in
      fath.access <> Private && fath.titles <> [] ||
      moth.access <> Private && moth.titles <> []
  | _ -> False ]
;

value age_autorise conf base p =
  if p.access = Public || conf.friend || conf.wizard then True
  else if
    conf.public_if_titles && p.access = IfTitles &&
    (p.titles <> [] || parent_has_title base p)
  then True
  else
    match
      (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism,
       p.death, date_of_death p.death)
    with
    [ (_, _, NotDead, _) when conf.private_years > 0 -> False
    | (_, _, _, Some (Dgreg d _)) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | (Some (Dgreg d _), _, _, _) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | (_, Some (Dgreg d _), _, _) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | _ ->
        let u = uoi base p.cle_index in
        loop 0 where rec loop i =
          if i >= Array.length u.family then False
          else
            let fam = foi base u.family.(i) in
            match Adef.od_of_codate fam.marriage with
            [ Some (Dgreg d _) ->
                let a = temps_ecoule d conf.today in
                a.year > conf.private_years
            | _ -> loop (i + 1) ] ]
;

value fast_auth_age conf p =
  if p.access = Public || conf.friend || conf.wizard then True
  else if conf.public_if_titles && p.access = IfTitles && p.titles <> []
  then True
  else
    match
      (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism,
       p.death, date_of_death p.death)
    with
    [ (_, _, NotDead, _) when conf.private_years > 0 -> False
    | (_, _, _, Some (Dgreg d _)) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | (Some (Dgreg d _), _, _,  _) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | (_, Some (Dgreg d _), _, _) ->
        let a = temps_ecoule d conf.today in
        a.year > conf.private_years
    | _ -> False ]
;

value nobr_level = ref 0;
value enter_nobr () =
  do if nobr_level.val == 0 then Wserver.wprint "<nobr>" else ();
     incr nobr_level;
  return ()
;
value exit_nobr () =
  do decr nobr_level;
     if nobr_level.val == 0 then Wserver.wprint "</nobr>" else ();
  return ()
;

value start_with_vowel s =
  if String.length s > 0 then
    match Char.lowercase s.[0] with
    [ 'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'h'
    | 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' | 'æ'
    | 'è' | 'é' | 'ê' | 'ë' | 'ì' | 'í' | 'î' | 'ï'
    | 'ò' | 'ó' | 'ô' | 'õ' | 'ö'
    | 'ù' | 'ú' | 'û' | 'ü' | 'ý' | 'ÿ' -> True
    | _ -> False ]
  else False
;

value connais base p =
  sou base p.first_name <> "?" || sou base p.surname <> "?"
;

value acces_n conf base n x =
  let first_name = p_first_name base x in
  let surname = p_surname base x in
  if (conf.wizard && conf.friend || conf.access_by_key)
  && not (first_name = "?" || surname = "?") then
    "p" ^ n ^ "=" ^ code_varenv (Name.lower first_name) ^
    ";n" ^ n ^ "=" ^ code_varenv (Name.lower surname) ^
      (if x.occ > 0 then ";oc" ^ n ^ "=" ^ string_of_int x.occ else "")
  else
    "i" ^ n ^ "=" ^ string_of_int (Adef.int_of_iper x.cle_index)
;

value acces conf base x = acces_n conf base "" x;

value calculer_age conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> Some (temps_ecoule d conf.today)
  | _ -> None ]
;

type p_access = (base -> person -> string * base -> person -> string);
value std_access = (p_first_name, p_surname);
value raw_access =
  (fun base p -> sou base (p.first_name),
   fun base p -> sou base (p.surname))
;

value gen_person_text (p_first_name, p_surname) conf base p =
  let beg =
    match (sou base p.public_name, p.qualifiers) with
    [ ("", [nn :: _]) ->
        p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
    | ("", []) -> p_first_name base p
    | (n, [nn :: _]) -> n ^ " <em>" ^ sou base nn ^ "</em>"
    | (n, []) -> n ]
  in
  beg ^ " " ^ p_surname base p
;

value gen_person_text_no_html (p_first_name, p_surname) conf base p =
  let beg =
    match (sou base p.public_name, p.qualifiers) with
    [ ("", [nn :: _]) -> p_first_name base p ^ " " ^ sou base nn
    | ("", []) -> p_first_name base p
    | (n, [nn :: _]) -> n ^ " " ^ sou base nn
    | (n, []) -> n ]
  in
  beg ^ " " ^ p_surname base p
;

value gen_person_text_without_surname (p_first_name, p_surname) conf base p =
  match (sou base p.public_name, p.qualifiers) with
  [ (n, [nn :: _]) when n <> "" -> n ^ " <em>" ^ sou base nn ^ "</em>"
  | (n, []) when n <> "" -> n
  | (_, [nn :: _]) ->
      p_first_name base p ^ " <em>" ^ sou base nn ^ "</em>"
  | (_, []) -> p_first_name base p ]
;

value person_text = gen_person_text std_access;
value person_text_no_html = gen_person_text_no_html std_access;
value person_text_without_surname = gen_person_text_without_surname std_access;

value afficher_nom_titre_reference conf base p s =
  match p.qualifiers with
  [ [] ->
      Wserver.wprint "<a href=\"%s%s\">%s</a>" (commd conf)
        (acces conf base p) s
  | [nn :: _] ->
      Wserver.wprint "<a href=\"%s%s\">%s <em>%s</em></a>" (commd conf)
        (acces conf base p) s (sou base nn) ]
;

value main_title base p =
  let rec find_main =
    fun
    [ [] -> None
    | [x :: l] ->
        if x.t_name == Tmain then Some x
        else find_main l ]
  in
  match find_main p.titles with
  [ None ->
      match p.titles with
      [ [x :: _] -> Some x
      | _ -> None ]
  | x -> x ]
;

value titled_person_text conf base p t =
  if Name.strip_lower (sou base t.t_place) =
     Name.strip_lower (p_surname base p)
  then
    match (t.t_name, p.qualifiers) with
    [ (Tname n, []) -> sou base n
    | (Tname n, [nn :: _]) ->
        sou base n ^ " <em>" ^ sou base nn ^ "</em>"
    | _ -> person_text_without_surname conf base p ]
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
  let s = if place = "" then s else s ^ " " ^ place in
  ", <em>" ^ s ^ "</em>"
;

value geneweb_link conf href s =
  if conf.cancel_links then s
  else "<a href=\"" ^ commd conf ^ href ^ "\">" ^ s ^ "</a>"
;

value wprint_geneweb_link conf href s =
  Wserver.wprint "%s" (geneweb_link conf href s)
;

value reference conf base p s =
  if conf.cancel_links then s
  else "<a href=\"" ^ commd conf ^ acces conf base p ^ "\">" ^ s ^ "</a>"
;

value gen_referenced_person_title_text p_access conf base p =
  if age_autorise conf base p then
    match main_title base p with
    [ Some t ->
        reference conf base p (titled_person_text conf base p t) ^
        one_title_text conf base p t
    | None -> reference conf base p (gen_person_text p_access conf base p) ]
  else reference conf base p (gen_person_text p_access conf base p)
;

value gen_person_title_text p_access conf base p =
  if age_autorise conf base p then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t ^ one_title_text conf base p t
    | None -> gen_person_text p_access conf base p ]
  else gen_person_text p_access conf base p
;

value referenced_person_title_text =
  gen_referenced_person_title_text std_access
;

value person_title_text = gen_person_title_text std_access;

value gen_person_text_without_title p_access conf base p =
  match main_title base p with
  [ Some t ->
      if t.t_place == p.surname then
        gen_person_text_without_surname p_access conf base p
      else
        match (t.t_name, p.qualifiers) with
        [ (Tname s, [nn :: _]) -> sou base s ^ " <em>" ^ sou base nn ^ "</em>"
        | (Tname s, _) -> sou base s
        | _ -> gen_person_text p_access conf base p ]
  | None -> gen_person_text p_access conf base p ]
;

value person_text_without_title = gen_person_text_without_title std_access;

value person_title conf base p =
  if age_autorise conf base p then
    match main_title base p with
    [ Some t -> one_title_text conf base p t
    | None -> "" ]
  else ""
;

value surname_begin n =
  let i = initiale n in
  if i == 0 then ""
  else
    let i =
      strip_spaces i where rec strip_spaces i =
        if n.[i - 1] == ' ' then strip_spaces (pred i) else i
    in
    " (" ^ String.sub n 0 i ^ ")"
;

value surname_end n =
  let i = initiale n in
  if i == 0 then n else String.sub n i (String.length n - i)
;

value rec skip_spaces s i =
  if i < String.length s && s.[i] == ' ' then skip_spaces s (i + 1)
  else i
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

value match_begin s t =
  loop 0 0 where rec loop i j =
    if i >= String.length s || j >= String.length t then True
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else False
;

value rec capitale s =
  if String.length s == 0 then ""
  else
    match s.[0] with
    [ 'a'..'z' | 'à'..'ö' | 'ø'..'ý' ->
        String.make 1
          (Char.chr (Char.code s.[0] - Char.code 'a' + Char.code 'A')) ^
          String.sub s 1 (String.length s - 1)
    | '&' ->
        if String.length s == 1 then s
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
    | _ -> s ]
;

value fcapitale (a : format 'a 'b 'c) =
  (Obj.magic capitale a : format 'a 'b 'c)
;

value nth_field w n =
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
  let i1 = start 0 n in
  let i2 = stop i1 in
  let (i1, i2) = if i2 == i1 then (0, stop 0) else (i1, i2) in
  String.sub w i1 (i2 - i1)
;

value transl conf w =
  try Hashtbl.find conf.lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
;

value transl_nth conf w n =
  try nth_field (Hashtbl.find conf.lexicon w) n with
  [ Not_found -> "[" ^ nth_field w n ^ "]" ]
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

value gen_decline conf wt s =
  let s1 = if s = "" then "" else " " ^ s in
  let len = String.length wt in
  if len >= 1 && wt.[len - 1] = ''' then
    if String.length s > 0 && start_with_vowel s then
      nth_field wt 1 ^ decline 'n' s
    else nth_field wt 0 ^ decline 'n' s1
  else if len >= 3 && wt.[len-3] == ':' && wt.[len-1] == ':' then
    let start = String.sub wt 0 (len - 3) in
    start ^ decline wt.[len-2] s
  else
    match plus_decl wt with
    [ Some (start, " +before") ->
        if s = "" then start else s ^ " " ^ start
    | _ -> wt ^ decline 'n' s1 ]
;

value transl_decline conf w s = gen_decline conf (transl conf w) s;

value gen_decline2 conf wt s1 s2 =
  let string_of =
    fun
    [ '1' -> Some s1
    | '2' -> Some s2
    | _ -> None ]
  in
  let len = String.length wt in
  loop 0 where rec loop i =
    if i = len then ""
    else
      let (s, i) =
        match wt.[i] with
        [ '%' when i + 1 < len ->
            match string_of wt.[i + 1] with
            [ Some s -> (s, i + 1)
            | None -> ("%", i) ]
        | ':' when i + 4 < len && wt.[i + 2] = ':' && wt.[i + 3] = '%' ->
            let c = wt.[i + 1] in
            match string_of wt.[i + 4] with
            [ Some s -> (decline c s, i + 4)
            | None -> (":", i) ]
        | '[' when i + 5 < len && wt.[i + 3] = ']' && wt.[i + 4] = '%' ->
            match string_of wt.[i + 5] with
            [ Some s ->
                let s =
                  if start_with_vowel s then String.make 1 wt.[i + 2] ^ s
                  else String.make 1 wt.[i + 1] ^ " " ^ s
                in
                (s, i + 5)
            | _ -> ("[", i) ]
        | c -> (String.make 1 c, i) ]
      in
      s ^ loop (i + 1)
;

value transl_decline2 conf w s1 s2 = gen_decline2 conf (transl conf w) s1 s2;

value failed_format s = (Obj.magic ("[" ^ s ^ "]") : format 'a 'b 'c);

value valid_format (ini_fmt : format 'a 'b 'c) (r : string) =
  let s = (Obj.magic ini_fmt : string) in
  loop 0 0 where rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match (s.[i], s.[i+1], r.[j], r.[j+1]) with
      [ ('%', x, '%', y) ->
          if x = y then loop (i+2) (j+2) else failed_format s
      | ('%', _, _, _) -> loop i (j+1)
      | (_, _, '%', _) -> loop (i+1) j
      | _ -> loop (i+1) (j+1) ]
    else if i < String.length s - 1 then
      if s.[i] == '%' then failed_format s else loop (i+1) j
    else if j < String.length r - 1 then
      if r.[j] == '%' then failed_format s else loop i (j+1)
    else (Obj.magic r : format 'a 'b 'c)
;

value cftransl conf fmt =
  let fmt = transl conf fmt in
  loop 0 where rec loop i =
    fun
    [ [] -> String.sub fmt i (String.length fmt - i)
    | ([a :: al] as gal) ->
        if i+4 < String.length fmt && fmt.[i] == ':' && fmt.[i+2] == ':'
        && fmt.[i+3] == '%' && fmt.[i+4] == 's' then
          decline fmt.[i+1] a ^ loop (i+5) al
        else if i+1 < String.length fmt && fmt.[i] == '%' && fmt.[i+1] == 's'
        then
          nominative a ^ loop (i+2) al
        else if i < String.length fmt then
          String.make 1 fmt.[i] ^ loop (i+1) gal
        else "" ]
;

value ftransl conf s =
  valid_format s (transl conf (Obj.magic s : string))
;

value ftransl_nth conf s p =
  valid_format s (transl_nth conf (Obj.magic s : string) p)
;

value fdecline conf w s =
  valid_format w (gen_decline conf (Obj.magic w : string) s)
;

value red_color = "red";
value std_color conf s =
  "<font color=" ^ conf.highlight ^ ">" ^ s ^ "</font>"
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
    if c = ';' then Buff.get len
    else loop (Buff.store len c)
;

value base_len n =
  let n = Filename.concat base_dir.val n in
  match try Some (Iobase.input n) with [ Sys_error _ -> None ] with
  [ Some base ->
      let len = base.data.persons.len in
      do base.func.cleanup (); return
      string_of_int len
  | _ -> "?" ]
;

value open_etc_file fname =
  let fname1 =
    List.fold_right Filename.concat [base_dir.val; "etc"]
      (Filename.basename fname ^ ".txt")
  in
  let fname2 =
    List.fold_right Filename.concat [lang_dir.val; "etc"]
      (Filename.basename fname ^ ".txt")
  in
  try Some (open_in fname1) with
  [ Sys_error _ ->
      try Some (open_in fname2) with
      [ Sys_error _ -> None ] ]
;

value rec copy_from_etc env imcom ic =
  let cnt = ref 0 in
  try
    while True do
      match input_char ic with
      [ '%' ->
          let c = input_char ic in
          try Wserver.wprint "%s" (List.assoc c env ()) with
          [ Not_found ->
              match c with
              [ '%' -> Wserver.wprint "%%"
              | '+' -> incr cnt
              | '#' -> Wserver.wprint "%d" cnt.val
              | 'k' -> Wserver.wprint "%s" imcom
              | 'n' -> Wserver.wprint "%s" (base_len (input_to_semi ic))
              | 'o' ->
                  Wserver.wprint "%s"
                    (if images_url.val <> "" then images_url.val
                     else imcom ^ "m=IM;v=")
              | 'r' ->
                  let name = input_line ic in
                  match open_etc_file name with
                  [ Some ic -> copy_from_etc env imcom ic
                  | None ->
                      Wserver.wprint
                        "<em>... file not found: \"%s.txt\"</em><br>" name ]
              | 'v' -> Wserver.wprint "%s" Version.txt
              | c -> Wserver.wprint "%%%c" c ] ]
      | c -> Wserver.wprint "%c" c ];
    done
  with exc ->
    do close_in ic; return
    match exc with [ End_of_file -> () | exc -> raise exc ]
;

value image_prefix conf =
  if images_url.val <> "" then images_url.val
  else conf.indep_command ^ "m=IM;v="
;

value default_body_prop conf =
  " background=\"" ^ image_prefix conf ^ "/gwback.jpg\""
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
  if not cgi then
    Wserver.extract_param "host: " '\r' request
  else
    let server_name =
      try Sys.getenv "SERVER_NAME" with
      [ Not_found -> "" ]
    in
    let server_port =
      try Sys.getenv "SERVER_PORT" with
      [ Not_found | Failure _ -> "80" ]
    in
    if server_port = "80" then server_name
    else server_name ^ ":" ^ server_port
;

value get_request_string_aux cgi request =
  if not cgi then
    Wserver.extract_param "GET " ' ' request
  else
    let script_name =
      try Sys.getenv "SCRIPT_NAME" with
      [ Not_found -> "" ]
    in
    let query_string =
      try Sys.getenv "QUERY_STRING" with
      [ Not_found -> "" ]
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
          let p = base.data.persons.get i in
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
            let p = poi base cpl.father in
            let f = scratch p.first_name in
            let s = scratch p.surname in
            if f = "" || s = "" then None
            else
              let oc = string_of_int p.occ in
              let u = uoi base cpl.father in
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
      | [("escache", _) :: l] -> loop l
      | [("dsrc", _) :: l] -> loop l
      | [("templ", _) :: l] -> loop l
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
         let sep = if s = "" then "" else ";" in
         x ^ "=" ^ v ^ sep ^ s)
      [("lang", conf.lang) :: env] ""
  in
  let suff =
    if conf.cgi then "b=" ^ conf.bname ^ ";" ^ suff else suff
  in
  addr ^ "?" ^ suff
;

value include_hed_trl conf base_opt suff =
  let hed_fname =
    let fname =
      List.fold_right Filename.concat [base_dir.val; "lang"; conf.lang]
        (conf.bname ^ suff)
    in
    if Sys.file_exists fname then fname
    else
      List.fold_right Filename.concat [base_dir.val; "lang"]
        (conf.bname ^ suff)
  in
  match try Some (open_in hed_fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let url () =
        let r =
          match base_opt with
          [ Some base -> url_no_index conf base
          | None -> get_server_string conf ^ get_request_string conf ]
        in
        code_varenv r
      in
      copy_from_etc [('u', url)] conf.indep_command ic
  | None -> () ]
;

value header_no_page_title conf title =
  do html conf;
     Wserver.wprint "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
 \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";
     Wserver.wprint "<head>\n";
     Wserver.wprint "  <meta name=\"ROBOTS\" content=\"NONE\">\n";
(*
     Wserver.wprint "  \
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=%s\">\n"
       conf.charset;
*)
     Wserver.wprint "  <title>";
     title True;
     Wserver.wprint "</title>\n";
     include_hed_trl conf None ".hed";
     Wserver.wprint "</head>\n";
     let s =
       try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
       [ Not_found -> "" ]
     in
     let s = s ^ body_prop conf in
     Wserver.wprint "<body%s>" s;
     Wserver.wprint "\n";
  return ()
;

value header conf title =
  do header_no_page_title conf title;
     Wserver.wprint "<h1 align=center><font color=%s>" conf.highlight;
     title False;
     Wserver.wprint "</font></h1>\n";
  return ()
;

value rheader conf title =
  do header_no_page_title conf title;
     Wserver.wprint "<center><h1><font color=%s>" red_color;
     title False;
     Wserver.wprint "</font></h1></center>\n";
  return ()
;

value start_with s i p =
  i + String.length p <= String.length s
  && String.lowercase (String.sub s i (String.length p)) = p
;

value http_string s i =
  if start_with s i "http://" then
    let j =
      loop (i + String.length "http://") where rec loop j =
        if j < String.length s then
          match s.[j] with
          [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '/' | ':' | '?' | '%' | ';' | '='
          | '_' | '-' | '&' | '.' | '~' | '#' -> loop (j + 1)
          | _ -> j ]
        else j
    in
    match s.[j-1] with
    [ ':' | ';' | '.' -> Some (j - 1)
    | _ -> Some j ]
  else None
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
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' ->
          after_at False (i + 1)
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
        if len > 0 && s.[i-1] = '.' then (len - 1, i - 1) else (len, i)
      in
      if len = 0 then None else Some i
  | None -> None ]
;

value dangerous_tags_list =
  ["applet"; "embed"; "form"; "input"; "object"; "script"]
;

value dangerous_tag s i =
  let tag_id =
    loop i 0 where rec loop i len =
      if i = String.length s then Buff.get len
      else
        match s.[i] with
        [ 'a'..'z' | 'A'..'Z' as c ->
            loop (i + 1) (Buff.store len (Char.lowercase s.[i]))
        | _ -> if len = 0 then loop (i + 1) 0 else Buff.get len ]
  in
  List.mem tag_id dangerous_tags_list
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

value copy_string_with_macros conf s =
  loop Out 0 where rec loop tt i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then
        match s.[i+1] with
        [ 's' ->
            do Wserver.wprint "%s" (commd conf); return loop tt (i + 2)
        | 'v' ->
            let (k, j) = get_variable s (i + 2) in
            let (v, i) =
              try (List.assoc ("var_" ^ k) conf.base_env, j) with
              [ Not_found -> ("%", i + 1) ]
            in
            do Wserver.wprint "%s" v; return loop tt i
        | '%' ->
            do Wserver.wprint "%%"; return loop tt (i + 2)
        | _ -> do Wserver.wprint "%%"; return loop tt (i + 1) ]
      else if s.[i] = '<' && dangerous_tag s (i + 1) then
        do Wserver.wprint "..."; return loop tt (i + 1)
      else
        match tt with
        [ In_a_href ->
            let tt = if start_with s i "</a>" then Out else In_a_href in
            do Wserver.wprint "%c" s.[i]; return loop tt (i + 1)
        | In_norm ->
            let tt = if s.[i] = '>' then Out else In_norm in
            do Wserver.wprint "%c" s.[i]; return loop tt (i + 1)
        | Out ->
            match http_string s i with
            [ Some j ->
                let x = String.sub s i (j - i) in
                do Wserver.wprint "<a href=%s>%s</a>" x x; return
                loop Out j
            | None ->
                match email_addr s i with
                [ Some j ->
                    let x = String.sub s i (j - i) in
                    do Wserver.wprint "<a href=\"mailto:%s\">%s</a>" x x;
                    return loop Out j
                | None ->
                    let tt =
                      if start_with s i "<a href="
                      || start_with s i "<a\nhref=" then In_a_href
                      else if s.[i] = '<' then In_norm
                      else Out
                    in
                    do Wserver.wprint "%c" s.[i]; return loop tt (i + 1) ] ] ]
    else ()
;

value gen_trailer with_logo conf =
  let env =
    [('s', fun _ -> commd conf);
     ('d',
      fun _ ->
        if conf.cancel_links then ""
        else " - <a href=\"" ^ conf.indep_command ^ "m=DOC\">DOC</a>")]
  in
  do if not with_logo then ()
     else
        Wserver.wprint "<p>
<img src=\"%s/gwlogo.gif\"
alt=... width=64 height=72 align=right>\n<br>\n"
          (image_prefix conf);
     match open_etc_file "copyr" with
     [ Some ic -> copy_from_etc env conf.indep_command ic
     | None ->
         do html_p conf;
            Wserver.wprint "
<hr><font size=-1><em>(c) Copyright 2001 INRIA -
GeneWeb %s</em></font>" Version.txt;
            html_br conf;
         return () ];
     include_hed_trl conf None ".trl";
     Wserver.wprint "</body>\n";
  return ()
;

value trailer = gen_trailer True;

value menu_threshold = 20;

value is_number t =
  match t.[0] with
  [ '1'..'9' -> True
  | _ -> False ]
;

value print_alphab_list conf crit print_elem liste =
  let len = List.length liste in
  do if len > menu_threshold then
       let _ =
         List.fold_left
           (fun last e ->
              let t = crit e in
              let same_than_last =
                match last with
                [ Some t1 -> t = t1
                | _ -> False ]
              in
              do if not same_than_last then
                   Wserver.wprint "<a href=\"#%s\">%s</a>\n" t t
                 else ();
              return Some t)
           None liste
       in
       ()
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
            do if len > menu_threshold || is_number t then
                 do match last with
                    [ Some _ ->
                        if not same_than_last then Wserver.wprint "  </ul>\n"
                        else ()
                    | _ -> () ];
                    if not same_than_last then
                      do html_li conf;
                         Wserver.wprint "<a name=\"%s\">%s</a>\n" t t;
                         Wserver.wprint "  <ul>\n";
                      return ()
                    else ();
                 return ()
               else ();
               html_li conf;
               print_elem e;
            return Some t)
         None liste
     in
     ();
     if len > menu_threshold then Wserver.wprint "  </ul>\n" else ();
     Wserver.wprint "</ul>\n";
  return ()
;

value parent conf base p a =
  match a.public_name with
  [ n when sou base n <> "" -> sou base n ^ person_title conf base a
  | _ -> 
      p_first_name base a ^
        (if p.surname <> a.surname then " " ^ p_surname base a else "") ]
;

value print_parent conf base p fath moth =
  let s =
    match (fath, moth) with
    [ (Some fath, None) -> parent conf base p fath
    | (None, Some moth) -> parent conf base p moth
    | (Some fath, Some moth) ->
        parent conf base p fath ^ " " ^ transl conf "and" ^ " " ^
        parent conf base p moth
    | _ -> "" ]
  in
  let is = index_of_sex p.sex in
  Wserver.wprint "%s"
    (transl_decline2 conf "%1 of (same or greater generation level) %2"
       (transl_nth conf "son/daughter/child" is) s)
;

value preciser_homonyme conf base p =
  let is = index_of_sex p.sex in
  match (p.public_name, p.qualifiers) with
  [ (n, [nn :: _]) when sou base n <> ""->
      Wserver.wprint "%s <em>%s</em>" (sou base n)
        (sou base nn)
  | (_, [nn :: _]) ->
      Wserver.wprint "%s <em>%s</em>" (p_first_name base p) (sou base nn)
  | (n, []) when sou base n <> "" ->
      Wserver.wprint "%s" (sou base n)
  | (_, []) ->
      let a = aoi base p.cle_index in
      let ifam =
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            let fath =
              let fath = poi base cpl.father in
              if p_first_name base fath = "?" then None else Some fath
            in
            let moth =
              let moth = poi base cpl.mother in
              if p_first_name base moth = "?" then None else Some moth
            in
            Some (fath, moth)
        | None -> None ]
      in
      match ifam with
      [ Some (None, None) | None ->
          let u = uoi base p.cle_index in
          let rec loop i =
            if i < Array.length u.family then
              let des = doi base u.family.(i) in
              let conjoint = spouse p.cle_index (coi base u.family.(i)) in
              let ct = des.children in
              if Array.length ct > 0 then
                let enfant = poi base ct.(0) in
                Wserver.wprint "%s"
                  (transl_decline2 conf "%1 of %2"
                     (transl_nth conf "father/mother" is)
                     (p_first_name base enfant ^
                        (if p.surname <> enfant.surname then
                         " " ^ (p_surname base enfant)
                         else "")))
              else
                let conjoint = poi base conjoint in
                if p_first_name base conjoint <> "?" ||
                   p_surname base conjoint <> "?" then
                  Wserver.wprint "%s"
                    (transl_decline2 conf "%1 of %2"
                       (transl_nth conf "husband/wife" is)
                       (p_first_name base conjoint ^ " " ^
                        p_surname base conjoint))
                else loop (i + 1)
            else Wserver.wprint "..."
          in
          loop 0
      | Some (fath, moth) ->
          print_parent conf base p fath moth ] ]
;

value print_decimal_num conf f =
  let s = string_of_float f in
  loop 0 where rec loop i =
    if i == String.length s then ()
    else
      do match s.[i] with
         [ '.' -> Wserver.wprint "%s" (transl conf "(decimal separator)")
         | x -> Wserver.wprint "%c" x ];
      return loop (i + 1)
;

value personal_image_file_name bname str =
  List.fold_right Filename.concat [base_dir.val; "images"; bname] str
;

value source_image_file_name bname str =
  let fname1 =
    List.fold_right Filename.concat [base_dir.val; "src"; bname; "images"]
      str
  in
  let fname2 =
    List.fold_right Filename.concat [base_dir.val; "src"; "images"] str
  in
  if Sys.file_exists fname1 then fname1 else fname2
;

value image_file_name str =
  let fname1 = List.fold_right Filename.concat [base_dir.val; "images"] str in
  let fname2 = List.fold_right Filename.concat [lang_dir.val; "images"] str in
  if Sys.file_exists fname1 then fname1 else fname2
;

value png_image_size ic =
  let magic =
    let s = String.create 4 in
    do really_input ic s 0 4; return s
  in
  if magic = "\137PNG" then
    do seek_in ic 16; return
    let wid = input_binary_int ic in
    let hei = input_binary_int ic in
    Some (wid, hei)
  else None
;

value gif_image_size ic =
  let magic =
    let s = String.create 4 in
    do really_input ic s 0 4; return s
  in
  if magic = "GIF8" then
    do seek_in ic 6; return
    let wid = let x = input_byte ic in input_byte ic * 256 + x in
    let hei = let x = input_byte ic in input_byte ic * 256 + x in
    Some (wid, hei)
  else None
;

value jpeg_image_size ic =
  let magic =
    let str = String.create 10 in
    do really_input ic str 0 10; return str
  in
  if Char.code magic.[0] = 0xff && Char.code magic.[1] = 0xd8
  && String.sub magic 6 4 = "JFIF" then
    loop () where rec loop () =
      do while Char.code (input_char ic) <> 0xFF do done; return
      let ch =
        loop (input_char ic) where rec loop ch =
          if Char.code ch = 0xFF then loop (input_char ic)
          else ch
      in
      if Char.code ch >= 0xC0 && Char.code ch <= 0xC3 then
        do for i = 1 to 3 do let _ = input_char ic in (); done; return
        let a = input_char ic in
        let b = input_char ic in
        let c = input_char ic in
        let d = input_char ic in
        let wid = (Char.code c lsl 8) lor (Char.code d) in
        let hei = (Char.code a lsl 8) lor (Char.code b) in
        Some (wid, hei)
      else
        let a = input_char ic in
        let b = input_char ic in
        let len = (Char.code a lsl 8) lor (Char.code b) in
        if len < 2 then None
        else
          do for i = 1 to len - 2 do let _ = input_char ic in (); done;
          return if Char.code ch <> 0xDA then loop () else None
  else None
;

value image_size fname =
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let r =
        try
          let sz = jpeg_image_size ic in
          let sz =
            if sz = None then do seek_in ic 0; return png_image_size ic
            else sz
          in
          if sz = None then do seek_in ic 0; return gif_image_size ic
          else sz
        with
        [ End_of_file -> None ]
      in
      do close_in ic; return r
  | None -> None ]
;

value limited_image_size max_wid max_hei fname =
  match image_size fname with
  [ Some (wid, hei) ->
      let (wid, hei) =
        if hei > max_hei then
          let wid = wid * max_hei / hei in
          let hei = max_hei in
          (wid, hei)
        else (wid, hei)
      in
      let (wid, hei) =
        if wid > max_wid then
          let hei = hei * max_wid / wid in
          let wid = max_wid in
          (wid, hei)
        else (wid, hei)
      in
      Some (wid, hei)
  | None -> None ]
;

value up_fname conf =
(*
  let s = Wserver.extract_param "accept: " '\n' conf.request in
  let list =
    let insert ibeg i list =
      if i = ibeg then list
      else
        let s = strip_spaces (String.sub s ibeg (i - ibeg)) in
        if s <> "" then [String.lowercase s :: list] else list
    in
    loop [] 0 0 where rec loop list ibeg i =
      if i = String.length s then insert ibeg i list
      else if s.[i] = ',' then loop (insert ibeg i list) (i + 1) (i + 1)
      else loop list ibeg (i + 1)
  in
  if List.mem "image/png" list then "up.png" else "up.jpg"
*)
  "up.jpg"
;

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
    "<a href=\"" ^ referer ^ "\"><img src=\"" ^ image_prefix conf ^
    "/" ^ fname ^ "\"" ^ wid_hei ^ " alt=\"&lt;&lt;\"></a>\n"
  else ""
;

value print_link_to_welcome conf right_aligned =
  if conf.cancel_links then ()
  else
    let fname = up_fname conf in
    let dir = if conf.is_rtl then "left" else "right" in
    let wid_hei =
      match image_size (image_file_name fname) with
      [ Some (wid, hei) ->
          " width=" ^ string_of_int wid ^ " height=" ^ string_of_int hei
      | None -> "" ]
    in
    do if right_aligned then Wserver.wprint "<table align=%s><tr><td>\n" dir
       else ();
       let str = link_to_referer conf in
       if str = "" then () else Wserver.wprint "%s" str;
       Wserver.wprint "<a href=\"%s\">" (commd_no_params conf);
       Wserver.wprint "<img src=\"%s/%s\"%s alt=\"^^\">"
         (image_prefix conf) fname wid_hei;
       Wserver.wprint "</a>\n";
       if right_aligned then Wserver.wprint "</td></tr></table>\n" else ();
    return ()
;

value incorrect_request conf =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "incorrect request"))
  in
  do header conf title;
     print_link_to_welcome conf False;
     trailer conf;
  return ()
;

value find_person_in_env conf base suff =
  match p_getint conf.env ("i" ^ suff) with
  [ Some i ->
      if i >= 0 && i < base.data.persons.len then
        Some (base.data.persons.get i)
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
          let xl = List.map (poi base) (person_ht_find_all base k) in
          let k = Name.lower k in
          try
            let r =
              List.find
                (fun x ->
                   Name.lower
                     (p_first_name base x ^ " " ^ p_surname base x)
                     = k &&
                   x.occ == occ)
                xl
            in
            Some r
          with
          [ Not_found -> None ]
      | _ -> None ] ]
;

value create_topological_sort conf base =
  match p_getenv conf.env "opt" with
  [ Some "no_tsfile" ->
      let _ = base.data.ascends.array () in
      let _ = base.data.couples.array () in
      Consang.topological_sort base
  | Some "no_tstab" -> Array.create base.data.persons.len 0
  | _ ->
      let bfile = Filename.concat base_dir.val conf.bname in
      lock (Iobase.lock_file bfile) with
      [ Accept ->
          let tstab_file = Filename.concat (bfile ^ ".gwb") "tstab" in
          let r =
            match
              try Some (open_in_bin tstab_file) with [ Sys_error _ -> None ]
            with
            [ Some ic ->
                let r =
                  try Some (Marshal.from_channel ic) with
                  [ End_of_file | Failure _ -> None ]
                in
                do close_in ic; return r
            | None -> None ]
          in
          match r with
          [ Some tstab -> tstab
          | None ->
              let _ = base.data.ascends.array () in
              let _ = base.data.couples.array () in
              let oc = open_out_bin tstab_file in
              let tstab = Consang.topological_sort base in
              do Marshal.to_channel oc tstab [Marshal.No_sharing];
                 close_out oc;
              return tstab ]
      | Refuse ->
          let _ = base.data.ascends.array () in
          let _ = base.data.couples.array () in
          Consang.topological_sort base ] ]
;

value branch_of_sosa base ip n =
  do if Num.eq n Num.zero then invalid_arg "branch_of_sosa" else (); return
  let rec expand bl n =
    if Num.eq n Num.one then bl else expand [Num.even n :: bl] (Num.half n)
  in
  let rec loop ipl ip sp =
    fun
    [ [] -> Some [(ip, sp) :: ipl]
    | [goto_fath :: nl] ->
        match (aoi base ip).parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            if goto_fath then loop [(ip, sp) :: ipl] cpl.father Male nl
            else loop [(ip, sp) :: ipl] cpl.mother Female nl
        | _ -> None ] ]
  in
  loop [] ip (poi base ip).sex (expand [] n)
;

value sosa_of_branch ipl =
  do if ipl = [] then failwith "sosa_of_branch" else (); return
  let ipl = List.tl (List.rev ipl) in
  List.fold_left
    (fun b (ip, sp) ->
       let b = Num.twice b in
       match sp with
       [ Male -> b
       | Female -> Num.inc b 1
       | Neuter -> assert False ])
    Num.one ipl
;

value space_to_unders s =
  match rindex s ' ' with
  [ Some _ ->
      let s' = String.create (String.length s) in
      do for i = 0 to String.length s - 1 do
           s'.[i] := if s.[i] = ' ' then '_' else s.[i];
         done;
      return s'
  | None -> s ]
;

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
  let f =
    List.fold_right Filename.concat [base_dir.val; "images"; conf.bname] s
  in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None
;

value image_and_size conf base p image_size =
  if age_autorise conf base p then
    let image_txt = capitale (transl_nth conf "image/images" 0) in
    match sou base p.image with
    [ "" ->
        match auto_image_file conf base p with
        [ Some f -> Some (f, Some (image_size f))
        | None -> None ]
    | s ->
        let http = "http://" in
        if String.length s > String.length http &&
           String.sub s 0 (String.length http) = http then
          Some (s, None)
        else if Filename.is_implicit s then
          match
            try Some (List.assoc "images_path" conf.base_env) with
            [ Not_found -> None ]
          with
          [ Some p when p <> "" -> Some (p ^ s, None)
          | _ ->
              let fname = personal_image_file_name conf.bname s in
              if Sys.file_exists fname then
                Some (fname, Some (image_size fname))
              else None ]
        else None ]
  else None
;

value only_printable s =
  let s = strip_spaces s in
  let s' = String.create (String.length s) in
  do for i = 0 to String.length s - 1 do
       s'.[i] :=
         match s.[i] with
         [ ' '..'~' | '\160'..'\255' -> s.[i]
         | _ -> ' ' ];
     done;
  return s'
;

value relation_type_text conf t n =
  match t with
  [ Adoption ->
      transl_nth conf "adoptive father/adoptive mother/adoptive parents" n
  | Recognition ->
      transl_nth conf
        "recognizing father/recognizing mother/recognizing parents" n
  | CandidateParent ->
      transl_nth conf "candidate father/candidate mother/candidate parents" n
  | GodParent ->
      transl_nth conf "godfather/godmother/godparents" n
  | FosterParent ->
      transl_nth conf "foster father/foster mother/foster parents" n ]
;

value rchild_type_text conf t n =
  match t with
  [ Adoption ->
      transl_nth conf "adoptive son/adoptive daughter/adoptive child" n
  | Recognition ->
      transl_nth conf
        "recognized son/recognized daughter/recognized child" n
  | CandidateParent ->
      transl_nth conf "candidate son/candidate daughter/candidate child" n
  | GodParent ->
      transl_nth conf "godson/goddaughter/godchild" n
  | FosterParent ->
      transl_nth conf "foster son/foster daughter/foster child" n ]
;

value wprint_hidden pref name valu =
  Wserver.wprint "<input type=hidden name=%s%s value=\"%s\">\n" pref name
    (quote_escaped valu)
;

value wprint_hidden_person conf base pref p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if (conf.wizard && conf.friend || conf.access_by_key)
  && not (first_name = "?" || surname = "?") then
    do wprint_hidden pref "p" (Name.lower first_name);
       wprint_hidden pref "n" (Name.lower surname);
       if p.occ > 0 then wprint_hidden pref "oc" (string_of_int p.occ)
       else ();
    return ()
  else wprint_hidden pref "i" (string_of_int (Adef.int_of_iper p.cle_index))
;

exception Ok;

value has_nephews_or_nieces base p =
  try
    let a = aoi base p.cle_index in
    match a.parents with
    [ Some ifam ->
        let des = doi base ifam in
        do Array.iter
             (fun ip ->
                if ip == p.cle_index then ()
                else
                  Array.iter
                    (fun ifam ->
                       if Array.length (doi base ifam).children > 0 then
                         raise Ok
                       else ())
                    (uoi base ip).family)
             des.children;
        return False
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
  do for i = 1 to (sz - pre_text_size txt) / 2 do Wserver.wprint " "; done;
     Wserver.wprint "%s\n" txt;
  return ()
;

value print_pre_left sz txt =
  let tsz = pre_text_size txt in
  do if tsz < sz / 2 - 1 then
       for i = 2 to (sz / 2 - 1 - tsz) / 2 do Wserver.wprint " "; done
     else ();
     Wserver.wprint " %s\n" txt;
  return ()
;

value print_pre_right sz txt =
  let tsz = pre_text_size txt in
  do if tsz < sz / 2 - 1 then
       do for i = 1 to sz / 2 do Wserver.wprint " "; done;
          for i = 1 to (sz / 2 - 1 - tsz) / 2 do Wserver.wprint " "; done;
       return ()
     else
       for i = 1 to sz - pre_text_size txt - 1 do Wserver.wprint " "; done;
     Wserver.wprint " %s\n" txt;
  return ()
;

value of_course_died conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> conf.today.year - d.year > 120
  | _ -> False ]
;

value relation_txt conf sex fam =
  let is = index_of_sex sex in
  match fam.relation with
  [ NotMarried -> ftransl conf "relationship%t to"
  | Married -> ftransl_nth conf "married%t to" is
  | Engaged -> ftransl_nth conf "engaged%t to" is ]
;

(* Deprecated *)

value afficher_personne conf base p =
  Wserver.wprint "%s" (person_text conf base p)
;

value afficher_prenom_de_personne conf base p =
  Wserver.wprint "%s" (person_text_without_surname conf base p)
;

value afficher_prenom_de_personne_referencee conf base p =
  Wserver.wprint "%s"
    (reference conf base p (person_text_without_surname conf base p))
;

value afficher_personne_referencee conf base p =
  Wserver.wprint "\n%s" (reference conf base p (person_text conf base p))
;

value afficher_personne_titre conf base p =
  Wserver.wprint "%s" (person_title_text conf base p)
;

value afficher_personne_titre_referencee conf base p =
  Wserver.wprint "\n%s" (referenced_person_title_text conf base p)
;

value afficher_personne_sans_titre conf base p =
  Wserver.wprint "%s" (person_text_without_title conf base p)
;

value afficher_titre conf base p =
  Wserver.wprint "%s" (person_title conf base p)
;
