(* $Id: gwcomp.ml,v 1.2 1998-09-15 11:16:54 ddr Exp $ *)

open Def;
open Gutil;

value magic_gwo = "GnWo0008";

type key =
  { pk_first_name : string;
    pk_surname : string;
    pk_occ : int }
;

type somebody =
  [ Undefined of key
  | Defined of Def.person string ]
;

type syntax_o =
  [ Family of couple somebody and Def.family (Def.person string) string
  | Notes of key and string ]
;

type gwo = (string * list syntax_o);

value copy_decode s i1 i2 =
  let rec loop_copy t i j =
    if i < i2 then
      let c =
        match s.[i] with
        [ '_' -> ' '
        | x -> x ]
      in
      do t.[j] := c; return loop_copy t (succ i) (succ j)
    else t
  in
  let i1 = if i1 < i2 && s.[i1] == '_' then i1 + 1 else i1 in
  loop_copy (String.create (i2 - i1)) i1 0
;

value fields str =
  loop 0 0 where rec loop beg i =
    if i < String.length str then
      if str.[i] == ' ' then
        if beg == i then loop (succ beg) (succ i)
        else [copy_decode str beg i :: loop (succ i) (succ i)]
      else loop beg (succ i)
    else if beg == i then []
    else [copy_decode str beg i]
;

value date_de_string s i =
  let champ i =
    let (neg, i) =
      if i < String.length s && s.[i] == '-' then (True, i + 1) else (False, i)
    in
    loop i 0 where rec loop i n =
      if i == String.length s then (if neg then - n else n, i)
      else
        match s.[i] with
        [ '0'..'9' as c ->
            loop (succ i) (10 * n + Char.code c - Char.code '0')
        | _ -> (if neg then - n else n, i) ]
  in
  let skip_slash i =
    if s.[i] == '/' then succ i else failwith ("date_de_string " ^ s)
  in
  let (precision, i) =
    match s.[i] with
    [ '~' -> (About, succ i)
    | '?' -> (Maybe, succ i)
    | '>' -> (After, succ i)
    | '<' -> (Before, succ i)
    | _ -> (Sure, i) ]
  in
  let (undef, annee, i) =
    let (annee, j) = champ i in
    if j = i + 1 && s.[i] == '0' then (True, annee, j)
    else (False, annee, j)
  in
  let (precision, i) =
    if i < String.length s && s.[i] == '|' then
      let (y2, i) = champ (succ i) in (OrYear y2, i)
    else (precision, i)
  in
  if i == String.length s then
    if undef then None else Some (Da precision annee)
  else
    let i = skip_slash i in
    let mois = annee in
    let (annee, i) = champ i in
    if i == String.length s then
      if annee == 0 then None
      else if mois < 1 || mois > 12 then failwith ("date_de_string " ^ s)
      else Some (Dma mois annee)
    else
      let i = skip_slash i in
      let jour = mois in
      let mois = annee in
      let (annee, i) = champ i in
      if i == String.length s then
        if annee == 0 then None
        else if mois < 1 || mois > 12 then failwith ("date_de_string " ^ s)
        else if jour < 1 || jour > 31 then failwith ("date_de_string " ^ s)
        else Some (Djma jour mois annee)
      else failwith ("date_de_string " ^ s)
;

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value line_cnt = ref 0;

value input_a_line ic =
  let x = input_line ic in
  do incr line_cnt; return x
;

value rec input_real_line ic =
  let x = input_line ic in
  do incr line_cnt; return
  if x = "" || x.[0] == '#' then input_real_line ic else x
;

value get_optional_birthdate l =
  match l with
  [ ["</h4>" :: _] -> (None, l)
  | [x :: l'] ->
      let i = 0 in
      if x.[i] == '!' then (None, l)
      else
        match x.[i] with
        [ '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_de_string x i in
            (Some d, l')
        | _ -> (None, l) ]
  | _ -> (None, l) ]
;

value get_optional_baptdate l =
  match l with
  [ [x :: l'] ->
      let i = 0 in
      if x.[i] == '!' then
        let i = succ i in
        match x.[i] with
        [ '~' | '?' | '<' | '>' | '-' | '0'..'9' ->
            let d = date_de_string x i in
            (Some d, l')
        | _ -> (None, l) ]
      else (None, l)
  | _ -> (None, l) ]
;

value get_optional_deathdate l =
  match l with
  [ ["?" :: l'] -> (Some DontKnowIfDead, l')
  | ["mj" :: l'] -> (Some DeadYoung, l')
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
              match date_de_string x i with
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
                (date_de_string x i, l')
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
                (date_de_string x i, l')
            | _ -> (None, l) ]
          in
          (Cremated (Adef.codate_of_od od), l)
      | [] -> (Cremated Adef.codate_None, l) ]
  | _ -> (UnknownBurial, l) ]
;              

value get_birth_place l =
  match l with
  [ ["#bp"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value get_bapt_place l =
  match l with
  [ ["#pp"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value get_death_place l =
  match l with
  [ ["#dp"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value get_burial_place l =
  match l with
  [ ["#rp"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value get_optional_sexe =
  fun
  [ ["h" :: l] -> (Masculin, l)
  | ["f" :: l] -> (Feminin, l)
  | l -> (Neutre, l) ]
;

value make_int str x =
  loop False 0 where rec loop found n i =
    if i == String.length x then
      if found then n else raise Not_found
    else
      match x.[i] with
      [ '0'..'9' as c ->
          loop True (10 * n + Char.code c - Char.code '0') (succ i)
      | _ -> raise Not_found ]
;

value cut_space x =
  if String.length x > 0 && x.[0] == ' ' then
    String.sub x 1 (String.length x - 1)
  else x
;

value get_fst_name str l =
  match l with
  [ [x :: l'] ->
      match x.[0] with
      [ 'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | '[' | '0'..'9' | '?'
      | ' ' ->
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
      if x.[0] == '{' && x.[String.length x - 1] == '}' then
        let n = String.sub x 1 (String.length x - 2) in
        let (nl, l) = get_fst_names_aliases str l' in ([n :: nl], l)
      else ([], l)
  | [] -> ([], l) ]
;

value rec get_surnames_aliases str l =
  match l with
  [ ["#salias"; x :: l'] ->
      let (nl, l) = get_surnames_aliases str l' in ([x :: nl], l)
  | _ -> ([], l) ]
;

value rec get_nick_names str l =
  match l with
  [ ["#nick"; x :: l'] ->
      let (nl, l) = get_nick_names str l' in ([x :: nl], l)
  | _ -> ([], l) ]
;

value rec get_aliases str l =
  match l with
  [ ["#alias"; x :: l'] -> let (nl, l) = get_aliases str l' in ([x :: nl], l)
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
      if x.[0] == '(' && x.[String.length x - 1] == ')' then
        let a = String.sub x 1 (String.length x - 2) in (a, l')
      else ("", l)
  | _ -> ("", l) ]
;

value get_photo str l =
  match l with
  [ ["#photo"; x :: l'] ->
      do for i = 0 to String.length x - 1 do
           if x.[i] == ' ' then x.[i] := '_' else ();
         done;
      return (x, l')
  | _ -> ("", l) ]
;

value get_occu str l =
  match l with
  [ ["#occu"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value get_sources str l =
  match l with
  [ ["#src"; x :: l'] -> (x, l')
  | _ -> ("", l) ]
;

value rec get_access str l =
  match l with
  [ ["#apubl" :: l'] -> (Public, l')
  | ["#apriv" :: l'] -> (Private, l')
  | _ -> (IfTitles, l) ]
;

value scan_title t =
  let i = 0 in
  let (name, i) =
    loop "" i where rec loop s i =
      if i < String.length t then
        match t.[i] with
        [ ':' -> (s, i + 1)
        | c -> loop (s ^ String.make 1 c) (i + 1) ]
      else failwith t
  in
  let name =
    match name with
    [ "" -> Tnone
    | "*" -> Tmain
    | _ -> Tname name ]
  in
  let (title, i) =
    loop "" i where rec loop s i =
      if i < String.length t then
        match t.[i] with
        [ ':' -> (s, i + 1)
        | c -> loop (s ^ String.make 1 c) (i + 1) ]
      else failwith t
  in
  let (place, i) =
    loop "" i where rec loop s i =
      if i < String.length t then
        match t.[i] with
        [ ':' -> (s, i)
(*
        | '0'..'9' -> failwith t
*)
        | c -> loop (s ^ String.make 1 c) (i + 1) ]
      else (s, i)
  in
  let (date_start, i) =
    if i < String.length t && t.[i] == ':' then
      let (d, i) =
        loop "" (i + 1) where rec loop s i =
          if i < String.length t then
            match t.[i] with
            [ ':' -> (s, i)
            | c -> loop (s ^ String.make 1 c) (i + 1) ]
          else (s, i)
      in
      (if d <> "" then date_de_string d 0 else None, i)
    else (None, i)
  in
  let (date_end, i) =
    if i < String.length t && t.[i] == ':' then
      let (d, i) =
        loop "" (i + 1) where rec loop s i =
          if i < String.length t then
            match t.[i] with
            [ ':' -> (s, i)
            | c -> loop (s ^ String.make 1 c) (i + 1) ]
          else (s, i)
      in
      (if d <> "" then date_de_string d 0 else None, i)
    else (None, i)
  in
  let (nth, i) =
    if i < String.length t && t.[i] == ':' then
      let (d, i) =
        loop "" (i + 1) where rec loop s i =
          if i < String.length t then
            match t.[i] with
            [ ':' -> (s, i)
            | c -> loop (s ^ String.make 1 c) (i + 1) ]
          else (s, i)
      in
      (if d <> "" then int_of_string d else 0, i)
    else (0, i)
  in
  if i <> String.length t then failwith t
  else
    {t_name = name; t_title = title; t_place = place;
     t_date_start = Adef.codate_of_od date_start;
     t_date_end = Adef.codate_of_od date_end;
     t_nth = nth}
;

value rec get_titles str l =
  match l with
  [ [x :: l'] ->
      if x.[0] == '[' && x.[String.length x - 1] == ']' then
        let t = String.sub x 1 (String.length x - 2) in
        let t = scan_title t in
        let (al, l') = get_titles str l' in ([t :: al], l')
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
               Adef.codate_of_od (date_de_string x 1)
             else Adef.codate_None, l)
        | _ -> failwith str ]
      in
      let (place, l) =
        match l with
        [ ["#mp"; x :: l] -> (x, l)
        | _ -> ("", l) ]
      in
      let (divorce, l) =
        match l with
        [ [x :: l] when x.[0] == '-' ->
            if String.length x > 1 then
              (Divorced (Adef.codate_of_od (date_de_string x 1)), l)
            else (Divorced Adef.codate_None, l)
        | _ -> (NotDivorced, l) ]
      in
      (mar, place, divorce, l)
  | [] -> failwith str ]
;

value lire_ligne ic =
  try let str = input_real_line ic in Some (str, fields str) with
  [ End_of_file -> None ]
;

value create_person () =
  {first_name = ""; surname = ""; occ = 0; photo = "";
   public_name = ""; nick_names = []; aliases = [];
   first_names_aliases = []; surnames_aliases = [];
   titles = []; occupation = ""; sexe = Neutre; access = IfTitles;
   birth = Adef.codate_None; birth_place = "";
   baptism = Adef.codate_None; baptism_place = "";
   death = DontKnowIfDead; death_place = "";
   burial = UnknownBurial; burial_place = "";
   family = [| |]; notes = ""; psources = "";
   cle_index = Adef.iper_of_int (-1)}
;

value bogus_def p n o = p = "?" || n = "?";

value set_infos str u l =
  let (nl, l) = get_fst_names_aliases str l in
  do u.first_names_aliases := nl; return
  let (nl, l) = get_surnames_aliases str l in
  do u.surnames_aliases := nl; return
  let (n, l) = get_pub_name str l in
  do u.public_name := n; return
  let (n, l) = get_photo str l in
  do u.photo := n; return
  let (nl, l) = get_nick_names str l in
  do u.nick_names := nl; return
  let (nl, l) = get_aliases str l in
  do u.aliases := nl; return
  let (tl, l) = get_titles str l in
  do u.titles := tl; return
  let (n, l) = get_access str l in
  do u.access := n; return
  let (n, l) = get_occu str l in
  do u.occupation := n; return
  let (n, l) = get_sources str l in
  do if n <> "" then u.psources := n else (); return
  let (naissance, l) = get_optional_birthdate l in
  let (birth_place, l) = get_birth_place l in
  let (baptism, l) = get_optional_baptdate l in
  let (baptism_place, l) = get_bapt_place l in
  let (mort, l) = get_optional_deathdate l in
  let (death_place, l) = get_death_place l in
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
  do u.birth := naissance;
     u.birth_place := birth_place;
     u.baptism := baptism;
     u.baptism_place := baptism_place;
     u.death := mort;
     u.death_place := death_place;
  return
  let (burial, l) = get_burial l in
  do u.burial := burial; return
  let (burial_place, l) = get_burial_place l in
  do u.burial_place := burial_place; return
  l
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
  else
    let u = create_person () in
    do u.surname := np;
       u.first_name := pp; u.occ := op;
    return
    let l = set_infos str u l in
    (Defined u, np, l)
;

value parse_child str surname csrc l =
  let u = create_person () in
  let (prenom, occ, l) = get_fst_name str l in
  do u.first_name := prenom; u.occ := occ; return
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
  do u.surname := nom;
     u.psources := csrc;
  return
  let l = set_infos str u l in (u, l)
;

value lire_famille ic fname =
  fun
  [ Some (str, ["fam" :: l]) ->
      let (cle_pere, surname, l) = parse_parent str l in
      let (marriage, marriage_place, divorce, l) = get_mar_date str l in
      let (cle_mere, _, l) = parse_parent str l in
      do if l <> [] then failwith str else (); return
      let ligne = lire_ligne ic in
      let (fsrc, ligne) =
        match ligne with
        [ Some (str, ["src"; x]) -> (x, lire_ligne ic)
        | Some (str, ["src" :: _]) -> failwith str
        | _ -> ("", ligne) ]
      in
      let (csrc, ligne) =
        match ligne with
        [ Some (str, ["csrc"; x]) -> (x, lire_ligne ic)
        | Some (str, ["csrc" :: _]) -> failwith str
        | _ -> ("", ligne) ]
      in
      let co = {father = cle_pere; mother = cle_mere} in
      match ligne with
      [ Some (_, ["beg"]) ->
          let cles_enfants =
            let rec loop children =
              match lire_ligne ic with
              [ Some (str, ["-" :: l]) ->
                  let (sexe, l) = get_optional_sexe l in
                  let (child, l) = parse_child str surname csrc l in
                  do child.sexe := sexe; return
                  if l <> [] then failwith str
                  else loop [child :: children]
              | Some (str, ["end"]) -> children
              | Some (str, _) -> failwith str
              | _ -> failwith "eof" ]
            in
            List.rev (loop [])
          in
          let fo =
            {marriage = marriage; marriage_place = marriage_place;
             divorce = divorce; children = Array.of_list cles_enfants;
             comment = ""; origin_file = fname; fsources = fsrc;
             fam_index = Adef.ifam_of_int (-1)}
          in
          Some (Family co fo, lire_ligne ic)
      | Some (str, ["comm" :: _]) ->
          let comm = String.sub str 5 (String.length str - 5) in
          let fo =
            {marriage = marriage; marriage_place = marriage_place;
             divorce = divorce; children = [||]; comment = comm;
             origin_file = fname; fsources = fsrc;
             fam_index = Adef.ifam_of_int (-1)}
          in
          Some (Family co fo, lire_ligne ic)
      | ligne ->
          let fo =
            {marriage = marriage; marriage_place = marriage_place;
             divorce = divorce; children = [||]; comment = "";
             origin_file = fname; fsources = fsrc;
             fam_index = Adef.ifam_of_int (-1)}
          in
          Some (Family co fo, ligne) ]
  | Some (str, ["notes" :: l]) ->
      let (surname, l) = get_name str l in
      let (first_name, occ, l) = get_fst_name str l in
      if l <> [] then failwith "str"
      else
        match lire_ligne ic with
        [ Some (_, ["beg"]) ->
            let notes =
              try
                loop (input_a_line ic) where rec loop =
                  fun
                  [ "end notes" -> ""
                  | l -> l ^ "\n" ^ loop (input_a_line ic) ]
              with
              [ End_of_file -> failwith "end of file" ]
            in
            let key =
              {pk_first_name = first_name;
               pk_surname = surname;
               pk_occ = occ}
            in
            let str = strip_spaces (strip_controls_m notes) in
            Some (Notes key str, lire_ligne ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file" ]
  | Some (str, _) -> failwith str
  | None -> None ]
;

value comp_familles x =
  do line_cnt.val := 0; return
  let ic = open_in x in
  let rec boucle fams ligne =
    match lire_famille ic x ligne with
    [ Some (famille, ligne) -> boucle [famille :: fams] ligne
    | None -> fams ]
  in
  let familles = List.rev (boucle [] (lire_ligne ic)) in
  do close_in ic; return
  let oc = open_out_bin (Filename.chop_suffix x ".gw" ^ ".gwo") in
  do output_string oc magic_gwo;
     output_value oc ((x, familles) : gwo);
     close_out oc;
  return ()
;
