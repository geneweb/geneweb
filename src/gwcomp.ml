(* $Id: gwcomp.ml,v 5.12 2008-11-04 13:03:13 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;

value magic_gwo = "GnWo000o";

type key = { pk_first_name : string; pk_surname : string; pk_occ : int };

type somebody = [ Undefined of key | Defined of gen_person iper string ];

type gw_syntax =
  [ Family of gen_couple somebody and sex and sex and
      list (somebody * sex) and gen_family (gen_person iper string) string and
      gen_descend (gen_person iper string)
  | Notes of key and string
  | Relations of somebody and sex and list (gen_relation somebody string)
  | Bnotes of string and string
  | Wnotes of string and string ]
;

type encoding = [ E_utf_8 | E_iso_8859_1 ];

value copy_decode s i1 i2 =
  let len =
    loop 0 i1 where rec loop len i =
      if i >= i2 then len
      else if i = i2 - 1 then len + 1
      else if s.[i] = '\\' then loop (len + 1) (i + 2)
      else loop (len + 1) (i + 1)
  in
  let rec loop_copy t i j =
    if i >= i2 then Bytes.unsafe_to_string t
    else if i = i2 - 1 && s.[i] <> '_' then do {
      Bytes.set t j s.[i];
      Bytes.unsafe_to_string t
    }
    else do {
      let (c, i) =
        match s.[i] with
        [ '_' -> (' ', i)
        | '\\' -> (s.[i + 1], i + 1)
        | x -> (x, i) ]
      in
      Bytes.set t j c;
      loop_copy t (succ i) (succ j)
    }
  in
  loop_copy (Bytes.create len) i1 0
;

value fields str =
  loop 0 0 where rec loop beg i =
    if i < String.length str then
      match str.[i] with
      [ ' ' | '\t' ->
          if beg = i then loop (succ beg) (succ i)
          else [copy_decode str beg i :: loop (succ i) (succ i)]
      | _ -> loop beg (succ i) ]
    else if beg = i then []
    else [copy_decode str beg i]
;

value date_of_string s i =
  let champ i =
    let (neg, i) =
      if i < String.length s && s.[i] = '-' then (True, i + 1)
      else (False, i)
    in
    let rec loop i n =
      if i = String.length s then (if neg then - n else n, i)
      else
        match s.[i] with
        [ '0'..'9' as c ->
            loop (succ i) (10 * n + Char.code c - Char.code '0')
        | _ -> (if neg then - n else n, i) ]
    in
    loop i 0
  in
  let skip_slash i =
    if i < String.length s && s.[i] = '/' then Some (succ i) else None
  in
  let (precision, i) =
    match s.[i] with
    [ '~' -> (About, succ i)
    | '?' -> (Maybe, succ i)
    | '>' -> (After, succ i)
    | '<' -> (Before, succ i)
    | _ -> (Sure, i) ]
  in
  let (undefined, year, i) =
    let (year, j) = champ i in
    if j = i + 1 && s.[i] = '0' then (True, year, j) else (False, year, j)
  in
  let error n = failwith (Printf.sprintf "date_of_string%d %s" n s) in
  let date =
    match skip_slash i with
    [ Some i ->
        let month = year in
        let (year, i) = champ i in
        match skip_slash i with
        [ Some i ->
            let day = month in
            let month = year in
            let (year, i) = champ i in
(*
            if year = 0 then if i = String.length s then None else error 1
            else
*)
            if month < 1 || month > 13 then error 2
            else if day < 1 || day > 31 then error 3
            else
              let d =
                {day = day; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg d Dgregorian, i)
        | None ->
            if year = 0 then None
            else if month < 1 || month > 13 then error 4
            else
              let d =
                {day = 0; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg d Dgregorian, i) ]
    | None ->
        if undefined then
          if i = String.length s then None
          else if s.[i] = '(' && s.[String.length s - 1] = ')' then
            let txt = String.sub s (i + 1) (String.length s - i - 2) in
            Some (Dtext txt, String.length s)
          else failwith ("date_of_string " ^ s)
        else
          let d =
            {day = 0; month = 0; year = year; prec = precision; delta = 0}
          in
          Some (Dgreg d Dgregorian, i) ]
  in
  let date =
    match date with
    [ Some ((Dgreg d cal as dt), i) ->
        if i = String.length s then Some (dt, i)
        else if s.[i] = '|' then
          let (y2, i) = champ (succ i) in
          Some (Dgreg {(d) with prec = OrYear y2} cal, i)
        else if
          i + 1 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
          let (y2, i) = champ (i + 2) in
          Some (Dgreg {(d) with prec = YearInt y2} cal, i)
        else Some (dt, i)
    | Some ((Dtext _ as dt), i) -> Some (dt, i)
    | None -> None ]
  in
  let date =
    match date with
    [ Some (Dgreg d _, i) ->
        if i = String.length s then Some (Dgreg d Dgregorian, i)
        else
          match s.[i] with
          [ 'G' -> Some (Dgreg d Dgregorian, i + 1)
          | 'J' ->
              let d = Calendar.gregorian_of_julian d in
              Some (Dgreg d Djulian, i + 1)
          | 'F' ->
              let d = Calendar.gregorian_of_french d in
              Some (Dgreg d Dfrench, i + 1)
          | 'H' ->
              let d = Calendar.gregorian_of_hebrew d in
              Some (Dgreg d Dhebrew, i + 1)
          | _ -> Some (Dgreg d Dgregorian, i) ]
    | d -> d ]
  in
  match date with
  [ Some (dt, i) -> if i = String.length s then Some dt else error 5
  | None -> None ]
;

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value line_cnt = ref 0;
value no_fail = ref False;
value no_picture = ref False;

value input_line0 ic =
  let line = input_line ic in
  do {
     incr line_cnt;
    if String.length line > 0 && line.[String.length line - 1] = '\r' then
      String.sub line 0 (String.length line - 1)
    else line
  }
;

value input_a_line (ic, encoding) =
  let line = input_line0 ic in
   match encoding with
   [ E_utf_8 -> line
   | E_iso_8859_1 -> Mutil.utf_8_of_iso_8859_1 line ]
;

value rec input_real_line ic =
  let x = input_a_line ic in
  if x = "" || x.[0] = '#' then input_real_line ic else x
;

value get_optional_birthdate l =
  match l with
  [ [x :: l'] ->
      let i = 0 in
      if x.[i] = '!' then (None, l)
      else
        match x.[i] with
        [ '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_of_string x i in (Some d, l')
        | _ -> (None, l) ]
  | _ -> (None, l) ]
;

value get_optional_baptdate l =
  match l with
  [ [x :: l'] ->
      let i = 0 in
      if x.[i] = '!' then
        let i = succ i in
        match x.[i] with
        [ '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_of_string x i in (Some d, l')
        | _ -> (None, l) ]
      else (None, l)
  | _ -> (None, l) ]
;

value get_optional_deathdate l =
  match l with
  [ ["?" :: l'] -> (Some DontKnowIfDead, l')
  | ["mj" :: l'] -> (Some DeadYoung, l')
  | ["od" :: l'] -> (Some OfCourseDead, l')
  | [x :: l'] ->
      let i = 0 in
      let (dr, i) =
        match x.[i] with
        [ 'k' -> (Killed, i + 1)
        | 'm' -> (Murdered, i + 1)
        | 'e' -> (Executed, i + 1)
        | 's' -> (Disappeared, i + 1)
        | _ -> (Unspecified, i) ]
      in
      if i < String.length x then
        match x.[i] with
        [ '~' | '?' | '>' | '<' | '-' | '0'..'9' ->
            let d =
              match date_of_string x i with
              [ None -> DeadDontKnowWhen
              | Some d -> Death dr (Adef.cdate_of_date d) ]
            in
            (Some d, l')
        | _ -> (None, l) ]
      else (None, l)
  | _ -> (None, l) ]
;

value get_burial l =
  match l with
  [ ["#buri" :: l] ->
      match l with
      [ [x :: l'] ->
          let i = 0 in
          let (od, l) =
            match x.[i] with
            [ '~' | '?' | '>' | '<' | '-' | '0'..'9' ->
                (date_of_string x i, l')
            | _ -> (None, l) ]
          in
          (Buried (Adef.codate_of_od od), l)
      | [] -> (Buried Adef.codate_None, l) ]
  | ["#crem" :: l] ->
      match l with
      [ [x :: l'] ->
          let i = 0 in
          let (od, l) =
            match x.[i] with
            [ '~' | '?' | '>' | '<' | '-' | '0'..'9' ->
                (date_of_string x i, l')
            | _ -> (None, l) ]
          in
          (Cremated (Adef.codate_of_od od), l)
      | [] -> (Cremated Adef.codate_None, l) ]
  | _ -> (UnknownBurial, l) ]
;

value cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len - 1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)
;

value get_field lab l =
  match l with
  [ [lab1; x :: l'] when lab1 = lab -> (cut_space x, l')
  | _ -> ("", l) ]
;

value get_optional_sexe =
  fun
  [ ["h" :: l] -> (Male, l)
  | ["f" :: l] -> (Female, l)
  | l -> (Neuter, l) ]
;

value make_int str x =
  loop False 0 where rec loop found n i =
    if i = String.length x then if found then n else raise Not_found
    else
      match x.[i] with
      [ '0'..'9' as c ->
          loop True (10 * n + Char.code c - Char.code '0') (succ i)
      | _ -> raise Not_found ]
;

value get_fst_name str l =
  match l with
  [ [x :: l'] ->
      match x.[0] with
      [ 'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | '[' | '0'..'9' | '?' |
        ' ' ->
          let x = cut_space x in
          let (x, occ) =
            match rindex x '.' with
            [ Some i ->
                try (String.sub x 0 i, make_int str x (succ i)) with
                [ Not_found -> (x, 0) ]
            | None -> (x, 0) ]
          in
          (x, occ, l')
      | _ -> failwith str ]
  | _ -> failwith str ]
;

value rec get_fst_names_aliases str l =
  match l with
  [ [x :: l'] ->
      if x.[0] = '{' && x.[String.length x - 1] = '}' then
        let n = String.sub x 1 (String.length x - 2) in
        let (nl, l) = get_fst_names_aliases str l' in ([cut_space n :: nl], l)
      else ([], l)
  | [] -> ([], l) ]
;

value rec get_surnames_aliases str l =
  match l with
  [ ["#salias"; x :: l'] ->
      let (nl, l) = get_surnames_aliases str l' in ([cut_space x :: nl], l)
  | _ -> ([], l) ]
;

value rec get_qualifiers str l =
  match l with
  [ ["#nick"; x :: l'] ->
      let (nl, l) = get_qualifiers str l' in ([cut_space x :: nl], l)
  | _ -> ([], l) ]
;

value rec get_aliases str l =
  match l with
  [ ["#alias"; x :: l'] ->
      let (nl, l) = get_aliases str l' in ([cut_space x :: nl], l)
  | _ -> ([], l) ]
;

value get_name str l =
  match l with
  [ ["#nick" :: _] | ["#alias" :: _] -> ("", l)
  | [x :: l'] ->
      match x.[0] with
      [ '{' -> ("", l)
      | 'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | '0'..'9' | '?' | ' ' ->
          (cut_space x, l')
      | _ -> ("", l) ]
  | _ -> ("", l) ]
;

value get_pub_name str l =
  match l with
  [ [x :: l'] ->
      if x.[0] = '(' && x.[String.length x - 1] = ')' then
        let a = String.sub x 1 (String.length x - 2) in (cut_space a, l')
      else ("", l)
  | _ -> ("", l) ]
;

value get_image str l =
  match l with
  [ ["#image" | "#photo"; x :: l'] ->
    if no_picture.val then ("", l')
    else (cut_space x, l')
  | _ -> ("", l) ]
;

value get_occu str l =
  match l with
  [ ["#occu"; x :: l'] -> (cut_space x, l')
  | _ -> ("", l) ]
;

value get_sources str l =
  match l with
  [ ["#src"; x :: l'] -> (cut_space x, l')
  | _ -> ("", l) ]
;

value rec get_access str l =
  match l with
  [ ["#apubl" :: l'] -> (Public, l')
  | ["#apriv" :: l'] -> (Private, l')
  | _ -> (IfTitles, l) ]
;

value scan_title t =
  let next_field i =
    loop "" i where rec loop s i =
      if i < String.length t then
        match t.[i] with
        [ ':' -> (s, i + 1)
        | '\\' -> loop (s ^ String.make 1 t.[i + 1]) (i + 2)
        | c -> loop (s ^ String.make 1 c) (i + 1) ]
      else (s, i)
  in
  let i = 0 in
  let (name, i) =
    let (s, i) = next_field i in
    if i = String.length t then failwith t else (s, i)
  in
  let name =
    match name with
    [ "" -> Tnone
    | "*" -> Tmain
    | _ -> Tname name ]
  in
  let (title, i) =
    let (s, i) = next_field i in
    if t.[i - 1] <> ':' then failwith t else (s, i)
  in
  let (place, i) = next_field i in
  let (date_start, i) =
    let (d, i) = next_field i in
    (if d = "" then None else date_of_string d 0, i)
  in
  let (date_end, i) =
    let (d, i) = next_field i in
    (if d = "" then None else date_of_string d 0, i)
  in
  let (nth, i) =
    let (d, i) = next_field i in (if d = "" then 0 else int_of_string d, i)
  in
  if i <> String.length t then failwith t
  else
    {t_name = name; t_ident = title; t_place = place;
     t_date_start = Adef.codate_of_od date_start;
     t_date_end = Adef.codate_of_od date_end; t_nth = nth}
;

value rec get_titles str l =
  match l with
  [ [x :: l'] ->
      if x.[0] = '[' && x.[String.length x - 1] = ']' then
        let t = String.sub x 1 (String.length x - 2) in
        let t = scan_title t in
        let (al, l') = get_titles str l' in
        (if t.t_ident = "" then al else [t :: al], l')
      else ([], l)
  | _ -> ([], l) ]
;

value get_mar_date str =
  fun
  [ [x :: l] ->
      let (mar, l) =
        match x.[0] with
        [ '+' ->
            (if String.length x > 1 then
               Adef.codate_of_od (date_of_string x 1)
             else Adef.codate_None,
             l)
        | _ -> failwith str ]
      in
      let (relation, l) =
        match l with
        [ ["#nm" :: l] -> ((NotMarried, Male, Female), l)
        | ["#eng" :: l] -> ((Engaged, Male, Female), l)
        | ["#nsck"; c :: l] when String.length c = 2 ->
            let decode_sex i =
              match c.[i] with
              [ 'm' -> Male
              | 'f' -> Female
              | _ -> Neuter ]
            in
            ((NoSexesCheckNotMarried, decode_sex 0, decode_sex 1), l)
        | ["#nsckm"; c :: l] when String.length c = 2 ->
            let decode_sex i =
              match c.[i] with
              [ 'm' -> Male
              | 'f' -> Female
              | _ -> Neuter ]
            in
            ((NoSexesCheckMarried, decode_sex 0, decode_sex 1), l)
        | ["#noment" :: l] -> ((NoMention, Male, Female), l)
        | _ -> ((Married, Male, Female), l) ]
      in
      let (place, l) = get_field "#mp" l in
      let (src, l) = get_field "#ms" l in
      let (divorce, l) =
        match l with
        [ [x :: l] when x.[0] = '-' ->
            if String.length x > 1 then
              (Divorced (Adef.codate_of_od (date_of_string x 1)), l)
            else (Divorced Adef.codate_None, l)
        | ["#sep" :: l] -> (Separated, l)
        | _ -> (NotDivorced, l) ]
      in
      (relation, mar, place, src, divorce, l)
  | [] -> failwith str ]
;

value read_line ic =
  try let str = input_real_line ic in Some (str, fields str) with
  [ End_of_file -> None ]
;

value create_person () =
  {first_name = ""; surname = ""; occ = 0; image = ""; public_name = "";
   qualifiers = []; aliases = []; first_names_aliases = [];
   surnames_aliases = []; titles = []; rparents = []; related = [];
   occupation = ""; sex = Neuter; access = IfTitles; birth = Adef.codate_None;
   birth_place = ""; birth_src = ""; baptism = Adef.codate_None;
   baptism_place = ""; baptism_src = ""; death = DontKnowIfDead;
   death_place = ""; death_src = ""; burial = UnknownBurial;
   burial_place = ""; burial_src = ""; notes = ""; psources = "";
   key_index = Adef.iper_of_int (-1)}
;

value bogus_def p n o = p = "?" || n = "?";

value set_infos fn sn occ sex comm_psources comm_birth_place str u l =
  let (first_names_aliases, l) = get_fst_names_aliases str l in
  let (surnames_aliases, l) = get_surnames_aliases str l in
  let (public_name, l) = get_pub_name str l in
  let (image, l) = get_image str l in
  let (qualifiers, l) = get_qualifiers str l in
  let (aliases, l) = get_aliases str l in
  let (titles, l) = get_titles str l in
  let (access, l) = get_access str l in
  let (occupation, l) = get_occu str l in
  let (psources, l) = get_sources str l in
  let (naissance, l) = get_optional_birthdate l in
  let (birth_place, l) = get_field "#bp" l in
  let (birth_src, l) = get_field "#bs" l in
  let (baptism, l) = get_optional_baptdate l in
  let (baptism_place, l) =
    let (pp, l) = get_field "#pp" l in
    if pp = "" then get_field "#bp" l else (pp, l)
  in
  let (bapt_src, l) = get_field "#ps" l in
  let (mort, l) = get_optional_deathdate l in
  let (death_place, l) = get_field "#dp" l in
  let (death_src, l) = get_field "#ds" l in
  let mort =
    match (naissance, mort) with
    [ (None, _) | (_, Some _) | (Some None, _) ->
        match mort with
        [ Some m -> m
        | None -> DontKnowIfDead ]
    | (Some _, None) -> NotDead ]
  in
  let naissance =
    match naissance with
    [ None -> Adef.codate_None
    | Some x -> Adef.codate_of_od x ]
  in
  let baptism =
    match baptism with
    [ None -> Adef.codate_None
    | Some x -> Adef.codate_of_od x ]
  in
  let (burial, l) = get_burial l in
  let (burial_place, l) = get_field "#rp" l in
  let (burial_src, l) = get_field "#rs" l in
  let u =
    {first_name = fn; surname = sn; occ = occ;
     rparents = u.rparents; related = u.related; sex = sex; notes = u.notes;
     key_index = u.key_index; first_names_aliases = first_names_aliases;
     surnames_aliases = surnames_aliases; public_name = public_name;
     image = image; qualifiers = qualifiers; aliases = aliases;
     titles = titles; access = access; occupation = occupation;
     psources = if psources <> "" then psources else comm_psources;
     birth = naissance;
     birth_place = if birth_place <> "" then birth_place else comm_birth_place;
     birth_src = birth_src; baptism = baptism;
     baptism_place = baptism_place; baptism_src = bapt_src;
     death = mort; death_place = death_place; death_src = death_src;
     burial = burial; burial_place = burial_place;
     burial_src = burial_src}
  in
  (u, l)
;

value parse_parent str l =
  let (np, l) = get_name str l in
  let (pp, op, l) = get_fst_name str l in
  let defined =
    if bogus_def pp np op then True
    else
      match l with
      [ [] -> False
      | [s :: _] when s.[0] = '+' -> False
      | _ -> True ]
  in
  if not defined then
    let key = {pk_first_name = pp; pk_surname = np; pk_occ = op} in
    (Undefined key, np, l)
  else do {
    let u = create_person () in
    let (u, l) = set_infos pp np op u.sex "" "" str u l in
    (Defined u, np, l)
  }
;

value parse_child str surname sex csrc cbp l =
  let u = create_person () in
  let (prenom, occ, l) = get_fst_name str l in
  do {
    let (nom, l) =
      match l with
      [ ["?" :: _] -> get_name str l
      | [x :: l'] ->
          match x.[0] with
          [ '<' | '>' | '!' | '~' | '?' | '-' | '0'..'9' | '{' | '#' ->
              (surname, l)
          | '(' | '[' -> (if prenom = "" then "" else surname, l)
          | _ -> get_name str l ]
      | _ -> (surname, []) ]
    in
    set_infos prenom nom occ sex csrc cbp str u l
  }
;

value get_relation str =
  fun
  [ ["-"; x :: l] ->
      let rtyp =
        match x with
        [ "adop" | "adop:" -> Adoption
        | "reco" | "reco:" -> Recognition
        | "cand" | "cand:" -> CandidateParent
        | "godp" | "godp:" -> GodParent
        | "fost" | "fost:" -> FosterParent
        | _ -> failwith str ]
      in
      if String.length x = 5 && x.[4] = ':' then do {
        let (fk, _, l) = parse_parent str l in
        let l =
          match l with
          [ ["+" :: l] -> l
          | _ -> failwith str ]
        in
        let (mk, _, l) = parse_parent str l in
        if l <> [] then failwith str else ();
        {r_type = rtyp; r_fath = Some fk; r_moth = Some mk; r_sources = ""}
      }
      else
        match l with
        [ ["fath:" :: l] ->
            let (fk, _, l) = parse_parent str l in
            do {
              if l <> [] then failwith str else ();
              {r_type = rtyp; r_fath = Some fk; r_moth = None; r_sources = ""}
            }
        | ["moth:" :: l] ->
            let (mk, _, l) = parse_parent str l in
            do {
              if l <> [] then failwith str else ();
              {r_type = rtyp; r_fath = None; r_moth = Some mk; r_sources = ""}
            }
        | _ -> failwith str ]
  | _ -> failwith str ]
;

value read_notes ic =
  let notes =
    try
      let rec loop =
        fun
        [ "end notes" -> ""
        | l -> l ^ "\n" ^ loop (input_a_line ic) ]
      in
      loop (input_a_line ic)
    with
    [ End_of_file -> failwith "end of file" ]
  in
  Mutil.strip_all_trailing_spaces notes
;

(* from version 5.00 *)
value read_notes_db ic end_txt =
  let notes =
    try
      let rec loop s =
        if s = end_txt then ""
        else
          let len = String.length s in
          let s =
            if len > 2 && s.[0] = ' ' && s.[1] = ' ' then
              String.sub s 2 (len - 2)
            else s
          in
          s ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with
    [ End_of_file -> failwith "end of file" ]
  in
  Mutil.strip_all_trailing_spaces notes
;

type read_family 'a =
  [ F_some of 'a
  | F_enc_utf_8
  | F_none
  | F_fail of string ]
;

value read_family ic fname =
  fun
  [ Some (_, ["encoding:"; "utf-8"]) -> F_enc_utf_8
  | Some (str, ["fam" :: l]) ->
      let (fath_key, surname, l) = parse_parent str l in
      let (relation_ss, marriage, marr_place, marr_src, divorce, l) =
        get_mar_date str l
      in
      let (relation, fath_sex, moth_sex) = relation_ss in
      let (moth_key, _, l) = parse_parent str l in
      do {
        if l <> [] then failwith str else ();
        let line = read_line ic in
        let (witn, line) =
          loop line where rec loop =
            fun
            [ Some (str, ["wit" | "wit:" :: l]) ->
                let (sex, l) =
                  match l with
                  [ ["m:" :: l] -> (Male, l)
                  | ["f:" :: l] -> (Female, l)
                  | l -> (Neuter, l) ]
                in
                let (wk, _, l) = parse_parent str l in
                do {
                  if l <> [] then failwith str else ();
                  let (witn, line) = loop (read_line ic) in
                  ([(wk, sex) :: witn], line)
                }
            | line -> ([], line) ]
        in
        let (fsrc, line) =
          match line with
          [ Some (str, ["src"; x]) -> (cut_space x, read_line ic)
          | Some (str, ["src" :: _]) -> failwith str
          | _ -> ("", line) ]
        in
        let (csrc, line) =
          match line with
          [ Some (str, ["csrc"; x]) -> (cut_space x, read_line ic)
          | Some (str, ["csrc" :: _]) -> failwith str
          | _ -> ("", line) ]
        in
        let (cbp, line) =
          match line with
          [ Some (str, ["cbp"; x]) -> (cut_space x, read_line ic)
          | Some (str, ["cbp" :: _]) -> failwith str
          | _ -> ("", line) ]
        in
        let co = Adef.couple fath_key moth_key in
        let (comm, line) =
          match line with
          [ Some (str, ["comm" :: _]) ->
              let comm = String.sub str 5 (String.length str - 5) in
              (comm, read_line ic)
          | _ -> ("", line) ]
        in
        match line with
        [ Some (_, ["beg"]) ->
            let cles_enfants =
              let rec loop children =
                match read_line ic with
                [ Some (str, ["-" :: l]) ->
                    let (sex, l) = get_optional_sexe l in
                    let (child, l) = parse_child str surname sex csrc cbp l in
                    do {
                      if l <> [] then failwith str
                      else loop [child :: children]
                    }
                | Some (str, ["end"]) -> children
                | Some (str, _) -> failwith str
                | _ -> failwith "eof" ]
              in
              List.rev (loop [])
            in
            let fo =
              {marriage = marriage; marriage_place = marr_place;
               marriage_src = marr_src; witnesses = [| |];
               relation = relation; divorce = divorce; comment = comm;
               origin_file = fname; fsources = fsrc;
               fam_index = Adef.ifam_of_int (-1)}
            in
            let deo = {children = Array.of_list cles_enfants} in
            F_some (Family co fath_sex moth_sex witn fo deo, read_line ic)
        | line ->
            let fo =
              {marriage = marriage; marriage_place = marr_place;
               marriage_src = marr_src; witnesses = [| |];
               relation = relation; divorce = divorce; comment = comm;
               origin_file = fname; fsources = fsrc;
               fam_index = Adef.ifam_of_int (-1)}
            in
            let deo = {children = [| |]} in
            F_some (Family co fath_sex moth_sex witn fo deo, line) ]
      }
  | Some (str, ["notes-db"]) ->
      let notes = read_notes_db ic "end notes-db" in
      F_some (Bnotes "" notes, read_line ic)
  | Some (str, ["page-ext"; _]) ->
      let p =
        let len = String.length "page-ext" + 1 in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end page-ext" in
      F_some (Bnotes p notes, read_line ic)
  | Some (str, ["notes"]) ->
      (* used before version 5.00 *)
      let notes = read_notes ic in
      F_some (Bnotes "" notes, read_line ic)
  | Some (str, ["notes" :: l]) ->
      let (surname, l) = get_name str l in
      let (first_name, occ, l) = get_fst_name str l in
      if l <> [] then failwith "str"
      else
        match read_line ic with
        [ Some (_, ["beg"]) ->
            let notes = read_notes ic in
            let key =
              {pk_first_name = first_name; pk_surname = surname; pk_occ = occ}
            in
            F_some (Notes key notes, read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file" ]
  | Some (str, ["wizard-note" :: _]) ->
      let wizid =
        let len = String.length "wizard-note " in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end wizard-note" in
      F_some (Wnotes wizid notes, read_line ic)
  | Some (str, ["rel" :: l]) ->
      let (sb, _, l) = parse_parent str l in
      let (sex, l) =
        match l with
        [ ["#h" :: l] -> (Male, l)
        | ["#f" :: l] -> (Female, l)
        | l -> (Neuter, l) ]
      in
      if l <> [] then failwith "str"
      else
        match read_line ic with
        [ Some (_, ["beg"]) ->
            let rl =
              try
                let rec loop =
                  fun
                  [ "end" -> []
                  | x ->
                      [get_relation x (fields x) :: loop (input_a_line ic)] ]
                in
                loop (input_a_line ic)
              with
              [ End_of_file -> failwith "missing end rel" ]
            in
            F_some (Relations sb sex rl, read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file" ]
  | Some (str, l) -> failwith str
  | None -> F_none ]
;

value read_family_1 ic fname line =
  if no_fail.val then
    try read_family ic fname line with [ Failure str -> F_fail str ]
  else read_family ic fname line
;

value comp_families x =
  let out_file = Filename.chop_suffix x ".gw" ^ ".gwo" in
  do {
    line_cnt.val := 0;
    let oc = open_out_bin out_file in
    try
      let ic = open_in x in
      do {
        output_string oc magic_gwo;
        output_value oc (x : string);
        let rec loop line encoding =
          match read_family_1 (ic, encoding) x line with
          [ F_some (family, line) ->
              do { output_value oc (family : gw_syntax); loop line encoding }
          | F_enc_utf_8 ->
              loop (read_line (ic, E_utf_8)) E_utf_8
          | F_none -> ()
          | F_fail str ->
              do {
                Printf.printf "File \"%s\", line %d:\n" x line_cnt.val;
                Printf.printf "Error: %s\n" str;
                flush stdout;
                loop (read_line (ic, encoding)) encoding
              } ]
        in
        loop (read_line (ic, E_iso_8859_1)) E_iso_8859_1;
        close_in ic;
      }
    with e ->
      do {
        close_out oc;
        try Sys.remove out_file with [ Sys_error _ -> () ];
        raise e
      };
    close_out oc;
  }
;
