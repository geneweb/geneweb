(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: util.ml,v 2.10 1999-04-09 13:29:21 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;
open Gutil;

value lang_dir = ref ".";
value base_dir = ref ".";

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

value ansel_to_ascii s =
  let len =
    loop 0 0 where rec loop i len =
      if i == String.length s then len
      else
        if Char.code s.[i] >= 128 then loop (i + 1) len
        else loop (i + 1) (len + 1)
  in
  if len == String.length s then s
  else
    let s' = String.create len in
    loop 0 0 where rec loop i i' =
      if i == String.length s then s'
      else
        if Char.code s.[i] >= 128 then loop (i + 1) i'
        else do s'.[i'] := s.[i]; return loop (i + 1) (i' + 1)
;

value charset_of_ansel conf s =
  if conf.charset = "iso-8859-1" then Ansel.to_iso_8859_1 s
  else if s = "" then s
  else if String.length s = 2 then
    if Char.code s.[0] >= 128 then s else ansel_to_ascii s
  else if String.length s >= 4 then
    if Char.code s.[0] >= 128 && Char.code s.[2] >= 128 then s
    else ansel_to_ascii s
  else ansel_to_ascii s
;

value ansel_of_charset conf s =
  if conf.charset = "iso-8859-1" then Ansel.of_iso_8859_1 s
  else s
;

value coa = charset_of_ansel;
value aoc = ansel_of_charset;

value nl () = Wserver.wprint "\r\n";

value html conf =
  if conf.cgi then
    do Wserver.wprint "Content-type: text/html; charset=%s" conf.charset;
       nl (); nl ();
    return ()
  else Wserver.html conf.charset
;

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value commd conf =
  let c = conf.command ^ "?" in
  List.fold_left
    (fun c (k, v) -> c ^ k ^ (if v = "" then "" else "=" ^ v) ^ ";") c
    (conf.henv @ conf.senv)
;

value code_varenv = Wserver.encode;
value decode_varenv = Wserver.decode;

value p_getenv env label =
  try Some (decode_varenv (List.assoc (decode_varenv label) env)) with
  [ Not_found -> None ]
;

value p_getint env label =
  match p_getenv env label with
  [ Some s -> try Some (int_of_string (strip_spaces s)) with _ -> None
  | None -> None ]
;

value lendemain (j, m, a) =
  let (jour, r) =
    if j >= nb_jours_dans_mois m a then (1, 1) else (succ j, 0)
  in
  let (mois, r) = if m + r > 12 then (1, 1) else (m + r, 0) in
  let annee = a + r in (jour, mois, annee)
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
  else if p.access = IfTitles && (p.titles <> [] || parent_has_title base p)
  then True
  else
    match (Adef.od_of_codate p.birth, p.death) with
    [ (_, Death _ d) ->
        let a = annee (temps_ecoule (Adef.date_of_cdate d) conf.today) in
         a > 100
    | (Some d, _) ->
        let a = annee (temps_ecoule d conf.today) in
        a > 100
    | _ ->
        loop 0 where rec loop i =
          if i >= Array.length p.family then False
          else
            let fam = foi base p.family.(i) in
            match Adef.od_of_codate fam.marriage with
            [ Some d -> let a = annee (temps_ecoule d conf.today) in a > 100
            | _ -> loop (i + 1) ] ]
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
    match s.[0] with
    [ 'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'h' | 'A' | 'E' | 'I' | 'O' | 'U' |
      'Y' | 'H' | 'Á' | 'È' | 'É' ->
        True
    | _ -> False ]
  else False
;

value connais base p =
  sou base p.first_name <> "?" || sou base p.surname <> "?"
;

value acces_pur conf base x =
  let first_name = sou base x.first_name in
  let surname = sou base x.surname in
  if conf.wizard && conf.friend && not (first_name = "?" || surname = "?") then
    "n=" ^ (code_varenv (Name.lower surname)) ^ ";p=" ^
    (code_varenv (Name.lower first_name)) ^
      (if x.occ > 0 then ";oc=" ^ string_of_int x.occ else "")
  else
  "i=" ^ string_of_int (Adef.int_of_iper x.cle_index)
;

value acces conf base x =
  acces_pur conf base x
;

value calculer_age conf p =
  match Adef.od_of_codate p.birth with
  [ None -> None
  | Some d -> Some (temps_ecoule d conf.today) ]
;

value person_text conf base p =
  let beg =
    match (sou base p.public_name, p.nick_names) with
    [ ("", [nn :: _]) ->
        sou base p.first_name ^ " <em>" ^ sou base nn ^ "</em>"
    | ("", []) -> sou base p.first_name
    | (n, [nn :: _]) -> n ^ " <em>" ^ sou base nn ^ "</em>"
    | (n, []) -> n ]
  in
  coa conf (beg ^ " " ^ sou base p.surname)
;

value person_text_no_html conf base p =
  let beg =
    match (sou base p.public_name, p.nick_names) with
    [ ("", [nn :: _]) -> sou base p.first_name ^ " " ^ sou base nn
    | ("", []) -> sou base p.first_name
    | (n, [nn :: _]) -> n ^ " " ^ sou base nn
    | (n, []) -> n ]
  in
  coa conf (beg ^ " " ^ sou base p.surname)
;

value person_text_without_surname conf base p =
  let s =
    match (sou base p.public_name, p.nick_names) with
    [ (n, [nn :: _]) when n <> "" -> n ^ " <em>" ^ sou base nn ^ "</em>"
    | (n, []) when n <> "" -> n
    | (_, [nn :: _]) ->
        sou base p.first_name ^ " <em>" ^ sou base nn ^ "</em>"
    | (_, []) -> sou base p.first_name ]
  in
  coa conf s
;

value afficher_personne conf base p =
  Wserver.wprint "%s" (person_text conf base p)
;

value afficher_personne_referencee conf base p =
  Wserver.wprint "<a href=\"%s%s\">\n%s</a>" (commd conf) (acces conf base p)
    (person_text conf base p)
;

value afficher_nom_titre_reference conf base p s =
  match p.nick_names with
  [ [] ->
      Wserver.wprint "<a href=\"%s%s\">%s</a>" (commd conf)
        (acces conf base p) (coa conf s)
  | [nn :: _] ->
      Wserver.wprint "<a href=\"%s%s\">%s <em>%s</em></a>" (commd conf)
        (acces conf base p) (coa conf s) (coa conf (sou base nn)) ]
;

value afficher_prenom_de_personne_referencee conf base p =
  Wserver.wprint "<a href=\"%s%s\">%s</a>" (commd conf) (acces conf base p)
    (person_text_without_surname conf base p)
;

value afficher_prenom_de_personne conf base p =
  Wserver.wprint "%s" (person_text_without_surname conf base p)
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
     Name.strip_lower (sou base p.surname)
  then
    match (t.t_name, p.nick_names) with
    [ (Tname n, []) -> coa conf (sou base n)
    | (Tname n, [nn :: _]) ->
        coa conf (sou base n) ^ " <em>" ^ coa conf (sou base nn) ^ "</em>"
    | _ -> person_text_without_surname conf base p ]
  else
    match t.t_name with
    [ Tname s ->
        let s = coa conf (sou base s) in
        match p.nick_names with
        [ [] -> s
        | [nn :: _] -> s ^ " <em>" ^ coa conf (sou base nn) ^ "</em>" ]
    | _ -> person_text conf base p ]
;

value afficher_un_titre conf base p t =
  let place = sou base t.t_place in
  do Wserver.wprint ", <em>%s" (coa conf (sou base t.t_ident));
     if place = "" then () else Wserver.wprint " %s" (coa conf place);
     Wserver.wprint "</em>";
  return ()
;

value afficher_personne_titre_referencee conf base p =
  if p.access <> Private || conf.friend || conf.wizard then
    match main_title base p with
    [ Some t ->
        do stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
             Wserver.wprint "%s" (titled_person_text conf base p t);
           end;
           afficher_un_titre conf base p t;
        return ()
    | None -> afficher_personne_referencee conf base p ]
  else
    afficher_personne_referencee conf base p
;

value afficher_personne_un_titre conf base p t =
  do if t.t_place = p.surname then
       match t.t_name with
       [ Tname n -> Wserver.wprint "%s" (coa conf (sou base n))
       | _ -> afficher_prenom_de_personne conf base p ]
     else
       match t.t_name with
       [ Tname s -> Wserver.wprint "%s" (coa conf (sou base s))
       | _ -> afficher_personne conf base p ];
     Wserver.wprint ", <em>%s %s</em>" (coa conf (sou base t.t_ident))
       (coa conf (sou base t.t_place));
  return ()
;

value afficher_personne_titre conf base p =
  if p.access <> Private || conf.friend || conf.wizard then
    match main_title base p with
    [ Some t -> afficher_personne_un_titre conf base p t
    | None -> afficher_personne conf base p ]
  else afficher_personne conf base p
;

value afficher_personne_sans_titre conf base p =
  match main_title base p with
  [ Some t ->
      do if t.t_place == p.surname then
           afficher_prenom_de_personne conf base p
         else
           match (t.t_name, p.nick_names) with
           [ (Tname s, [nn :: _]) ->
               Wserver.wprint "%s <em>%s</em>" (coa conf (sou base s))
                 (coa conf (sou base nn))
           | (Tname s, _) -> Wserver.wprint "%s" (coa conf (sou base s))
           | _ -> afficher_personne conf base p ];
      return ()
  | None -> afficher_personne conf base p ]
;

value afficher_titre conf base p =
  if p.access <> Private || conf.friend || conf.wizard then
    match main_title base p with
    [ Some t -> afficher_un_titre conf base p t
    | None -> () ]
  else ()
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

value fcapitale a = Obj.magic capitale a;

value nth_field w n =
  let rec start i n =
    if n == 0 then i
    else if i < String.length w then
      if w.[i] == '/' then start (i + 1) (n - 1) else start (i + 1) n
    else i
  in
  let rec stop i =
    if i < String.length w then if w.[i] == '/' then i else stop (i + 1)
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

value decline conf decl s =
  try
    let a = Hashtbl.find conf.lexicon decl in
    let (start, dest) =
      let i = String.index a '=' in
      (String.sub a 0 i, String.sub a (i + 1) (String.length a - i - 1))
    in
    let (w1, ws) =
      try
        let i = String.index s ' ' in
        (String.sub s 0 i, String.sub s i (String.length s - i))
      with
      [ Not_found -> (s, "") ]
    in
    if w1 = start then dest ^ ws
    else if start = "" && dest.[0] = '+' then
      w1 ^ String.sub dest 1 (String.length dest - 1) ^ ws
    else s
  with [ Not_found -> s ]
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

value transl_decline conf w s =
  let s1 = if s = "" then "" else " " ^ s in
  let wt = transl conf w in
  if wt.[String.length wt - 1] = ''' then
    if String.length s > 0 && start_with_vowel s then nth_field wt 1 ^ s
    else nth_field wt 0 ^ s1
  else
    match plus_decl wt with
    [ Some (start, decl) ->
        start ^ (if s = "" then "" else " " ^ decline conf decl s)
    | None -> wt ^ s1 ]
;

value ftransl conf (s : format 'a 'b 'c) : format 'a 'b 'c =
  Obj.magic transl conf s
;

value ftransl_nth conf (s : format 'a 'b 'c) p : format 'a 'b 'c =
  Obj.magic transl_nth conf s p
;

value index_of_sex =
  fun
  [ Male -> 0
  | Female -> 1
  | Neuter -> 2 ]
;

value header_no_page_title conf title =
  do html conf;
     Wserver.wprint "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
 \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";
     Wserver.wprint "<head>\n";
     Wserver.wprint "  <meta name=\"ROBOTS\" content=\"NONE\">\n";
     Wserver.wprint "  <title>";
     title True;
     Wserver.wprint "</title>\n";
     Wserver.wprint "</head>\n";
     let s =
       try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
       [ Not_found -> "" ]
     in
     let s =
       try s ^ " " ^ List.assoc "body_prop" conf.base_env with
       [ Not_found -> s ]
     in
     Wserver.wprint "<body%s>\n" s;
  return ()
;

value header conf title =
  do header_no_page_title conf title;
     Wserver.wprint "<h1>";
     title False;
     Wserver.wprint "</h1>\n";
  return ()
;

value rec copy_from_channel ic =
  try
    while True do
      match input_char ic with
      [ '%' ->
          let c = input_char ic in
          match c with
          [ '%' -> Wserver.wprint "%%"
          | 'v' -> Wserver.wprint "%s" version
          | c -> do Wserver.wprint "%%"; return Wserver.wprint "%c" c ]
      | c -> Wserver.wprint "%c" c ];
    done
  with
  [ End_of_file -> close_in ic ]
;

value copy_from_file fname =
  let fname =
    List.fold_right Filename.concat [lang_dir.val; "lang"]
       (Filename.basename fname ^ ".txt")
  in
  let ic = open_in fname in
  copy_from_channel ic
;

value trailer conf =
  do try copy_from_file "copyr" with _ ->
       do html_p conf;
          Wserver.wprint "
<hr><font size=-1><em>(c) Copyright INRIA 1999 -
GeneWeb %s</em></font>" version;
          html_br conf;
       return ();
     let trl_fname =
       List.fold_right Filename.concat [base_dir.val; "lang"; conf.lang]
         (conf.bname ^ ".trl")
     in
     match try Some (open_in trl_fname) with _ -> None with
     [ Some ic -> copy_from_channel ic
     | None ->
         let trl_fname =
           List.fold_right Filename.concat [base_dir.val; "lang"]
             (conf.bname ^ ".trl")
         in
         try copy_from_channel (open_in trl_fname) with _ -> () ];
     Wserver.wprint "</body>\n";
  return ()
;

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

value print_parent conf base p a =
  let is = index_of_sex p.sex in
  match p.public_name with
  [ n when sou base n <> "" ->
      let n = sou base n in
      do Wserver.wprint "%s %s" (transl_nth conf "son/daughter/child" is)
           (transl_decline conf "of" n);
         afficher_titre conf base a;
      return ()
  | _ ->
      Wserver.wprint "%s %s%s" (transl_nth conf "son/daughter/child" is)
        (transl_decline conf "of" (coa conf (sou base a.first_name)))
        (if p.surname <> a.surname then " " ^ coa conf (sou base a.surname)
         else "") ]
;

value spouse p fam =
  if p.cle_index == fam.father then fam.mother
  else if p.cle_index == fam.mother then fam.father
  else invalid_arg "spouse"
;

value preciser_homonyme conf base p =
  let is = index_of_sex p.sex in
  match (p.public_name, p.nick_names) with
  [ (n, [nn :: _]) when sou base n <> ""->
      Wserver.wprint "%s <em>%s</em>" (coa conf (sou base n))
        (coa conf (sou base nn))
  | (_, [nn :: _]) ->
      Wserver.wprint "%s <em>%s</em>" (coa conf (sou base p.first_name))
        (coa conf (sou base nn))
  | (n, []) when sou base n <> "" ->
      Wserver.wprint "%s" (coa conf (sou base n))
  | (_, []) ->
      let a = aoi base p.cle_index in
      match a.parents with
      [ Some fam
        when sou base (poi base (coi base fam).father).first_name <> "?" ->
          print_parent conf base p (poi base (coi base fam).father)
      | Some fam
        when sou base (poi base (coi base fam).mother).first_name <> "?" ->
          print_parent conf base p (poi base (coi base fam).mother)
      | _ ->
          let rec loop i =
            if i < Array.length p.family then
              let fam = foi base p.family.(i) in
              let conjoint = spouse p (coi base p.family.(i)) in
              let ct = fam.children in
              if Array.length ct > 0 then
                let enfant = poi base ct.(0) in
                Wserver.wprint "%s %s%s" (transl_nth conf "father/mother" is)
                  (transl_decline conf "of"
                     (coa conf (sou base enfant.first_name)))
                  (if p.surname <> enfant.surname then
                     " " ^ coa conf (sou base enfant.surname)
                   else "")
              else
                let conjoint = poi base conjoint in
                if sou base conjoint.first_name <> "?" ||
                   sou base conjoint.surname <> "?" then
                  Wserver.wprint "%s %s %s"
                    (transl_nth conf "husband/wife" is)
                    (transl_decline conf "of"
                       (coa conf (sou base conjoint.first_name)))
                    (coa conf (sou base conjoint.surname))
                else loop (i + 1)
            else Wserver.wprint "..."
          in
          loop 0 ] ]
;

value incorrect_request conf =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "incorrect request"))
  in
  do header conf title; trailer conf; return ()
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

value commd_no_params conf =
  conf.command ^
  List.fold_left
    (fun c (k, v) ->
       c ^ (if c = "" then "?" else ";") ^ k ^
       (if v = "" then "" else "=" ^ v))
    "" conf.henv
;

value image_file_name bname str =
  let fname1 =
    List.fold_right Filename.concat [base_dir.val; "images"; bname] str
  in
  let fname2 =
    List.fold_right Filename.concat [base_dir.val; "images"] str
  in
  let fname3 =
    List.fold_right Filename.concat [lang_dir.val; "images"] str
  in
  if Sys.file_exists fname1 then fname1
  else if Sys.file_exists fname2 then fname2
  else fname3
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
          let sz = gif_image_size ic in
          if sz = None then do seek_in ic 0; return jpeg_image_size ic
          else sz
        with
        [ End_of_file -> None ]
      in
      do close_in ic; return r
  | None -> None ]
;

value print_link_to_welcome conf right_aligned =
  let dir = if conf.is_rtl then "left" else "right" in
  let wid_hei =
    match image_size (image_file_name conf.bname "up.gif") with
    [ Some (wid, hei) ->
        " width=" ^ string_of_int wid ^ " height=" ^ string_of_int hei
    | None -> "" ]
  in
  do Wserver.wprint "<a href=\"%s\">" (commd_no_params conf);
     Wserver.wprint "<img src=\"%sm=IM;v=up.gif\"%s alt=\"^^\"%s>"
       (commd conf) wid_hei (if right_aligned then " align=" ^ dir else "");
     Wserver.wprint "</a>\n";
  return ()
;

value list_find f =
  loop where rec loop =
    fun
    [ [] -> None
    | [x :: l] -> if f x then Some x else loop l ]
;

value find_person_in_env conf base suff =
  match p_getint conf.env ("i" ^ suff) with
  [ Some i -> Some (base.data.persons.get i)
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
          let k = Name.strip_lower k in
          list_find
            (fun x ->
               Name.strip_lower
                 (sou base x.first_name ^ " " ^ sou base x.surname)
                 = k &&
               x.occ == occ)
            xl
      | _ -> None ] ]
;

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

value get_server_string conf =
  if not conf.cgi then
    Wserver.extract_param "host: " '\r' conf.request
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

value get_request_string conf =
  if not conf.cgi then
    Wserver.extract_param "GET " ' ' conf.request
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
          match
            try Some (open_in_bin tstab_file) with [ Sys_error _ -> None ]
          with
          [ Some ic -> Marshal.from_channel ic
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
  default_image_name_of_key (sou base p.first_name) (sou base p.surname) p.occ
;

value auto_image_file conf base p =
  let s = default_image_name base p in
  let f =
    List.fold_right Filename.concat [base_dir.val; "images"; conf.bname] s
  in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else None
;
