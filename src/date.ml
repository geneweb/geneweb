(* camlp5r ./pa_html.cmo *)
(* $Id: date.ml,v 5.17 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

UNDEF OLD;

open Config;
open Def;
open Util;
open Gutil;
open Gwdb;
open Mutil;
open Printf;
open TemplAst;


(* ********************************************************************** *)
(*  [Fonc] get_wday : config -> Def.date -> string                        *)
(** [Description] : Renvoie le jour de la semaine correspondant à la date
                    donnée en paramètre.
    [Args] :
      - conf : configuration de la base
      - d    : date
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value get_wday conf d =
  let jd = 
    match d with
      [ Dgreg d _  ->
        match d.prec with 
        [ Sure -> 
            if (d.day <> 0 && d.month <> 0) then Calendar.sdn_of_gregorian d
            else -1
        | _ -> -1 ]
      | _ -> -1 ]
  in 
  let wday =
    let jd_today = Calendar.sdn_of_gregorian conf.today in
    let x = conf.today_wd - jd_today + jd in
    if x < 0 then 6 + (x + 1) mod 7 else x mod 7
  in
  if jd <> -1 then " (" ^ (transl_nth conf "(week day)" wday) ^ ")"
  else ""
;

value nbsp = "&nbsp;";

value death_symbol conf =
  match
    try Some (List.assoc "death_symbol" conf.base_env) with
    [ Not_found -> None ]
  with
  [ Some x -> x
  | None -> if utf_8_db.val then "†" else "+" ]
;

value birth_symbol conf =
  match
    try Some (List.assoc "birth_symbol" conf.base_env) with
    [ Not_found -> None ]
  with
  [ Some x -> x
  | None -> if utf_8_db.val then "°" else "°" ]
;

value before_date d d1 =
  if d1.year < d.year then True
  else if d1.year > d.year then False
  else if d1.month < d.month then True
  else if d1.month > d.month then False
  else if d1.prec > d.prec then True
  else if d1.prec < d.prec then False
  else if d1.day < d.day then True
  else if d1.day > d.day then False
  else True
;

value code_date conf encoding d m y =
  let apply_date_code =
    fun
    [ 'd' -> string_of_int d
    | 'm' -> transl_nth conf "(month)" (m - 1)
    | 'y' -> string_of_int y
    | c -> "%" ^ String.make 1 c ]
  in
  let rec loop i =
    if i = String.length encoding then ""
    else
      let (s, i) =
        match encoding.[i] with
        [ '%' when i + 1 < String.length encoding ->
            let s = apply_date_code encoding.[i + 1] in (s, i + 1)
        | '['
          when
            i + 5 < String.length encoding && encoding.[i + 3] = ']' &&
            encoding.[i + 4] = '%' ->
            let s = apply_date_code encoding.[i + 5] in
            let s1 =
              if start_with_vowel s then String.make 1 encoding.[i + 2]
              else String.make 1 encoding.[i + 1] ^ " "
            in
            (s1 ^ s, i + 5)
        | c -> (String.make 1 c, i) ]
      in
      s ^ loop (i + 1)
  in
  loop 0
;

value code_dmy conf d =
  let encoding =
    let n =
      if d.day = 1 then 0
      else if d.day != 0 then 1
      else if d.month != 0 then 2
      else 3
    in
    transl_nth conf "(date)" n
  in
  code_date conf encoding d.day d.month d.year
;

value code_year conf y =
  code_date conf (transl_nth conf "(date)" 3) 0 0 y
;

value default_french_month =
  let tab =
    [| "Vendemiaire"; "Brumaire"; "Frimaire"; "Nivose"; "Pluviose"; "Ventose";
       "Germinal"; "Floreal"; "Prairial"; "Messidor"; "Thermidor";
       "Fructidor"; "Extra" |]
  in
  fun m -> tab.(m)
;

value default_hebrew_month =
  let tab =
    [| "Tishri"; "Heshvan"; "Kislev"; "Tevet"; "Shevat"; "AdarI"; "AdarII";
       "Nisan"; "Iyyar"; "Sivan"; "Tammuz"; "Av"; "Elul" |]
  in
  fun m -> tab.(m)
;

value french_month conf m =
  let r = transl_nth conf "(french revolution month)" m in
  if r = "[(french revolution month)]" then "[" ^ default_french_month m ^ "]"
  else r
;

value hebrew_month conf m =
  let r = transl_nth conf "(hebrew month)" m in
  if r = "[(hebrew month)]" then "[" ^ default_hebrew_month m ^ "]" else r
;

value code_french_year conf y =
  transl_nth conf "year/month/day" 3 ^ " " ^
    (if y >= 1 && y < 4000 then roman_of_arabian y else string_of_int y)
;

value code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d ^ (if d = 1 then "<sup>er</sup>" else "")
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ french_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ code_french_year conf y
;

value code_hebrew_date conf d m y =
  let s = if d = 0 then "" else string_of_int d in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ hebrew_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ string_of_int y
;

value string_of_on_prec_dmy_aux conf code_year sy d =
  match d.prec with
  [ Sure ->
      if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
      else if d.day = 0 then transl_decline conf "in (month year)" sy
      else transl_decline conf "on (day month year)" sy
  | About | Before | After ->
      let s = sy in
      if d.prec = About then transl_decline conf "about (date)" s
      else if d.prec = Before then transl_decline conf "before (date)" s
      else transl_decline conf "after (date)" s
  | Maybe ->
      let s =
        if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl_decline conf "in (month year)" sy
        else transl_decline conf "on (day month year)" sy
      in
      transl_decline conf "possibly (date)" s
  | OrYear z ->
      let s =
        if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl_decline conf "in (month year)" sy
        else transl_decline conf "on (day month year)" sy
      in
      s ^ " " ^ transl conf "or" ^ " " ^ nominative (code_year conf z)
  | YearInt z ->
      let s =
        if d.day = 0 && d.month = 0 then sy
        else if d.day = 0 then sy
        else transl_decline conf "on (day month year)" sy
      in
      transl conf "between (date)" ^ " " ^ s ^ " " ^
        transl_nth conf "and" 0 ^ " " ^ nominative (code_year conf z) ]
;

value replace_spaces_by_nbsp s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then loop (i + 1) (Buff.mstore len "&nbsp;")
    else loop (i + 1) (Buff.store len s.[i])
;

value string_of_on_prec_dmy conf code_dmy sy d =
  let r = string_of_on_prec_dmy_aux conf code_dmy sy d in
  replace_spaces_by_nbsp r
;

value string_of_on_dmy conf d =
  let sy = code_dmy conf d in
  string_of_on_prec_dmy conf code_year sy d
;

value string_of_on_french_dmy conf d =
  let sy = code_french_date conf d.day d.month d.year in
  string_of_on_prec_dmy conf code_french_year sy d
;

value string_of_on_hebrew_dmy conf d =
  let sy = code_hebrew_date conf d.day d.month d.year in
  string_of_on_prec_dmy conf code_year sy d
;

value string_of_prec_dmy conf s d =
  match d.prec with
  [ Sure -> nominative s
  | About -> transl_decline conf "about (date)" s
  | Before -> transl_decline conf "before (date)" s
  | After -> transl_decline conf "after (date)" s
  | Maybe -> transl_decline conf "possibly (date)" s
  | OrYear z ->
      s ^ " " ^ transl conf "or" ^ " " ^
        nominative (code_date conf (transl_nth conf "(date)" 3) 0 0 z)
  | YearInt z ->
      transl conf "between (date)" ^ " " ^ s ^ " " ^
        transl_nth conf "and" 0 ^ " " ^
        nominative (code_date conf (transl_nth conf "(date)" 3) 0 0 z) ]
;

value string_of_dmy conf d =
  let sy = code_dmy conf d in string_of_prec_dmy conf sy d
;


(* ************************************************************************ *)
(*  [Fonc] translate_dmy : config -> (string * string * string) -> 
                             calendar -> bool -> (string * string * string) *)
(** [Description] : Traduit en fonction du calendrier, le mois et/ou l'année
                    d'une date et renvoie le triplet conformément au format
                    de la date.
    [Args] :
      - conf : configuration de la base
      - (fst, snd, trd) : la date au bon format
      - cal : calendar
      - short : booléen pour savoir si on affiche au format court, e.g.
                VD/Vendémiaire
    [Retour] : (string * string * string) : date traduite
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value translate_dmy conf (fst, snd, trd) cal short =
  let translate_month m =
    match cal with
    [ Dfrench when m <> "" -> 
        if short then Util.short_f_month (int_of_string m)
        else french_month conf (int_of_string m)
    | Dhebrew when m <> "" -> 
        if short then 
          String.uppercase 
            (String.sub (hebrew_month conf (int_of_string m)) 0 2)
        else hebrew_month conf (int_of_string m)
    | _ -> m ]
  in
  let translate_year y =
    match cal with
    [ Dfrench -> 
        let y1 = int_of_string y in
        (if y1 >= 1 && y1 < 4000 then roman_of_arabian y1 else y)
    | _ -> y ]
  in
  match transl conf " !dates order" with
  [ "yymmdd" | "yyyymmdd" -> (translate_year fst, translate_month snd, trd)
  | "mmddyyyy" -> (translate_month fst, snd, translate_year trd)
  | _ -> (fst, translate_month snd, translate_year trd) ]
;


(* ************************************************************************ *)
(*  [Fonc] decode_dmy : config -> Def.date -> (string * string * string)    *)
(** [Description] : En fonction du format de la date (donnée par le fichier
                    lex_utf8), on renvoit le triplet correspondant.
                    Si le format est inconnu, alors on renvoit ddmmyyyy.
                    Le triplet est renvoyé dans l'ordre adéquat d'affichage,
                    i.e. (fst, snd, trd).
    [Args] :
      - conf : configuration de la base
      - d    : date
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value decode_dmy conf d =
  match transl conf " !dates order" with
  [ "dmyyyy" -> 
      (string_of_int d.day, string_of_int d.month, string_of_int d.year)
  | "mmddyyyy" ->
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.day, d.month, d.year) with
      [ (0, 0, year) -> ("", "", string_of_int year)
      | (0, month, year) -> 
          let m = Printf.sprintf "%02d" month in
          (m, "", string_of_int year)
      | (day, month, year) ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (m, d, string_of_int year) ]
  | "yyyymmdd" | "yymmdd" (* backward compatibility < 6.05 *) ->
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.day, d.month, d.year) with
      [ (0, 0, year) -> (string_of_int year, "", "")
      | (0, month, year) -> 
          let m = Printf.sprintf "%02d" month in
          (string_of_int year, m, "")
      | (day, month, year) ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (string_of_int year, m, d) ]
  | "ddmmyyyy" | "ddmmyy" (* backward compatibility < 6.05 *) | _ -> 
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.day, d.month, d.year) with
      [ (0, 0, year) -> ("", "", string_of_int year)
      | (0, month, year) -> 
          let m = Printf.sprintf "%02d" month in
          ("", m, string_of_int year)
      | (day, month, year) ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (d, m, string_of_int year) ]
  ]
;

value gregorian_precision conf d =
  if d.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^ string_of_on_dmy conf d ^ " " ^
      transl_nth conf "and" 0 ^ " " ^ string_of_on_dmy conf d2
;

value string_of_ondate_aux conf =
  fun
  [ Dgreg d Dgregorian ->
      let s = string_of_on_dmy conf d in
      if d.day > 0 && not conf.cancel_links then
        sprintf
          "<a href=\"%sm=CAL;yg=%d;mg=%d;dg=%d;tg=1\" class=\"date\">%s</a>"
          (commd conf) d.year d.month d.day s
      else s
  | Dgreg d Djulian ->
      let cal_prec =
        if d.year < 1582 then "" else " (" ^ gregorian_precision conf d ^ ")"
      in
      let d1 = Calendar.julian_of_gregorian d in
      let year_prec =
        if d1.month > 0 && d1.month < 3 ||
           d1.month = 3 && d1.day > 0 && d1.day < 25 then
          sprintf " (%d/%d)" (d1.year - 1) (d1.year mod 10)
        else ""
      in
      let s =
        string_of_on_dmy conf d1 ^ year_prec ^ " " ^
          transl_nth conf "gregorian/julian/french/hebrew" 1 ^ cal_prec
      in
      if d1.day > 0 && not conf.cancel_links then
        sprintf 
          "<a href=\"%sm=CAL;yj=%d;mj=%d;dj=%d;tj=1\" class=\"date\">%s</a>"
          (commd conf) d1.year d1.month d1.day s
      else s
  | Dgreg d Dfrench ->
      let d1 = Calendar.french_of_gregorian d in
      let s = string_of_on_french_dmy conf d1 in
      let s =
        if d1.day > 0 && not conf.cancel_links then
          sprintf
            "<a href=\"%sm=CAL;yf=%d;mf=%d;df=%d;tf=1\" class=\"date\">%s</a>"
            (commd conf) d1.year d1.month d1.day s
        else s
      in
      match d.prec with
      [ Sure -> s ^ " " ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s ]
  | Dgreg d Dhebrew ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let s = string_of_on_hebrew_dmy conf d1 in
      match d.prec with
      [ Sure -> s ^ " " ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s ]
  | Dtext t -> "(" ^ string_with_macros conf [] t ^ ")" ]
;

value string_of_ondate conf d =
  Util.translate_eval (string_of_ondate_aux conf d)
;

value string_of_date conf =
  fun
  [ Dgreg d _ -> string_of_dmy conf d
  | Dtext t -> "(" ^ t ^ ")" ]
;


(* ********************************************************************** *)
(*  [Fonc] string_slash_of_date : config -> Def.date -> string            *)
(** [Description] : Renvoie une date sous la forme jj/mm/aaaa (en 
                    fonction du 'date order').
    [Args] :
      - conf : configuration de la base
      - d  : Def.date
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value string_slash_of_date conf date =
  let slashify_dmy (fst, snd, trd) d =
    let sy = 
      List.fold_right 
        (fun s accu -> if s <> "" then s ^ "/" ^ accu else accu)
        [fst; snd] trd
    in
    string_of_prec_dmy conf sy d 
  in
  match date with
  [ Dgreg d Dgregorian -> 
      slashify_dmy (decode_dmy conf d) d
  | Dgreg d Djulian -> 
      let d1 = Calendar.julian_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Djulian True) d1 ^
        " (" ^ (transl_nth conf "gregorian/julian/french/hebrew" 1) ^ ")"
  | Dgreg d Dfrench -> 
      let d1 = Calendar.french_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Dfrench True) d1 ^ 
        " (" ^ (transl_nth conf "gregorian/julian/french/hebrew" 2) ^ ")"
  | Dgreg d Dhebrew -> 
      let d1 = Calendar.french_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Dhebrew True) d1 ^ 
        " (" ^ (transl_nth conf "gregorian/julian/french/hebrew" 3) ^ ")"
  | Dtext t -> t ]
;

value string_of_age conf a =
  match a with
  [ {day = 0; month = 0; year = y} ->
      if y > 1 then string_of_int y ^ " " ^ transl conf "years old"
      else if y = 1 then transl conf "one year old"
      else transl conf "birth"
  | {day = 0; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else transl conf "less than one month old"
  | {day = d; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else if d >= 2 then string_of_int d ^ " " ^ transl conf "days old"
      else if d = 1 then transl conf "one day old"
      else "0" ]
;


(* ************************************************************************ *)
(*  [Fonc] prec_text : config -> Def.dmy -> string                          *)
(** [Description] : Renvoie la précision d'une date.
    [Args] :
      - conf : configuration de la base
      - d    : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value prec_text conf d =
  match d.prec with
  [ About -> 
      (* On utilise le dictionnaire pour être sur *)
      (* que ce soit compréhensible de tous.      *)
      match transl conf "about (short date)" with
      [ "ca" -> "ca "
      | s -> s ]
  | Maybe -> "?"
  | Before -> "<"
  | After -> ">"
  | OrYear _ -> "|"
  | YearInt _ -> ".."
  | _ -> "" ]
;


(* ************************************************************************ *)
(*  [Fonc] day_text : Def.dmy -> string                                     *)
(** [Description] : Renvoie le jour d'une date.
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value day_text d =
  if d.day = 0 then ""
  else string_of_int d.day
;


(* ************************************************************************ *)
(*  [Fonc] month_text : Def.dmy -> string                                   *)
(** [Description] : Renvoie le mois d'une date.
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value month_text d =
  if d.month = 0 then ""
  else string_of_int d.month
;


(* ************************************************************************ *)
(*  [Fonc] year_text : Def.dmy -> string                                    *)
(** [Description] : Renvoie l'année d'une date.
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value year_text d =
  match d.prec with
  [ OrYear x -> string_of_int d.year ^ "/" ^ string_of_int x
  | YearInt x -> string_of_int d.year ^ ".." ^ string_of_int x
  | _ -> string_of_int d.year ]
;


(* ************************************************************************ *)
(*  [Fonc] prec_year_text : config -> Def.dmy -> string                     *)
(** [Description] : Renvoie la précision d'une date et l'année de la date.
    [Args] :
      - conf : configuration de la base
      - d    : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value prec_year_text conf d = 
  let s = 
    match d.prec with
    [ About -> 
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        match transl conf "about (short date)" with
        [ "ca" -> "ca "
        | s -> s ]
    | Maybe -> "?"
    | Before -> "/" 
    | _ -> "" ]
  in
  let s = s ^ year_text d in
  match d.prec with
  [ After -> s ^ "/"
  | _ -> s ]
;


(* ********************************************************************** *)
(*  [Fonc] get_birth_death : 
             person -> (Adef.date option * Adef.date option * bool)       *)
(** [Description] : Renvoie la date de naissance, la date de décès et un
      booléen pour savoir si la date de naissance et la date décès ont été 
      trouvées. Si on ne trouve pas la date de naissance et la date de 
      décès, alors approx = True.
    [Args] :
      - p    : person
    [Retour] : (Adef.date option * Adef.date option * bool)
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value get_birth_death_date p =
  let (birth_date, approx) =
    match Adef.od_of_codate (get_birth p) with
    [ None -> (Adef.od_of_codate (get_baptism p), True)
    | x -> (x, False) ]
  in
  let (death_date, approx) =
    match CheckItem.date_of_death (get_death p) with
    [ Some d -> (Some d, approx)
    | _ ->
        match get_burial p with
        [ Buried cd -> (Adef.od_of_codate cd, True)
        | Cremated cd -> (Adef.od_of_codate cd, True)
        | _ -> (None, approx) ] ]
  in
  (birth_date, death_date, approx)
;


(* ********************************************************************** *)
(*  [Fonc] short_dates_text : config -> base -> person -> string          *)
(** [Description] : Renvoie la concatenation de l'année de naissance et 
      l'année de décès (si trouvée par get_birth_death_date). La précision 
      de la date est ajoutée pour chaque année. 
      L'affichage est le suivant : 
        * 1700-1780 (date naissance - date décès)
        * 1700-     (naissance - décédé)
        * 1700      (naissance - vivant)
        * †1780     (pas date naissance - date décès)
        * †         (pas date naissance - décédé)
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value short_dates_text conf base p =
  if authorized_age conf base p then
    let (birth_date, death_date, _) = get_birth_death_date p in
    let s =
      match (birth_date, death_date) with
      [ (Some (Dgreg b _), Some (Dgreg d _)) ->
          prec_year_text conf b ^ "-" ^ prec_year_text conf d
      | (Some (Dgreg b _), None) ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          match get_death p with
          [ Death _ _ | DeadDontKnowWhen | DeadYoung -> 
              prec_year_text conf b ^ "-"
          | _ -> prec_year_text conf b ]
      | (None, Some (Dgreg d _)) ->
          match get_death p with
          [ Death _ _ | DeadDontKnowWhen | DeadYoung -> 
              death_symbol conf ^ prec_year_text conf d
          | _ -> "" ]
      | (None, None) ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          match get_death p with
          [ Death _ _ | DeadDontKnowWhen | DeadYoung -> death_symbol conf
          | _ -> "" ]
      (* On ne peut pas traiter les dates au format texte. *)
      | (_, _) -> "" ]
    in
    if s <> "" then " <em><bdo dir=\"ltr\">" ^ s ^ "</bdo></em>" else s
  else ""
;


(* ********************************************************************** *)
(*  [Fonc] short_marriage_date_text : 
             config -> base -> person -> person -> string                 *)
(** [Description] : Renvoie l'année de la date de mariage ansi que la
                    précision de la date.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p1   : conjoint 1
      - p2   : conjoint 2
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value short_marriage_date_text conf base fam p1 p2 =
  if authorized_age conf base p1 && authorized_age conf base p2 then
    match Adef.od_of_codate (get_marriage fam) with
    [ Some (Dgreg d _) ->
        "<span style=\"font-size:70%\">" ^ prec_year_text conf d ^ "</span>"
    | _ -> "" ]
  else ""
;

value string_of_place conf pl = Util.string_with_macros conf [] pl;

value print_dates conf base p =
  let cap s = ", " ^ s in
  let is = index_of_sex (get_sex p) in
  do {
    let birth_place = sou base (get_birth_place p) in
    match Adef.od_of_codate (get_birth p)with
    [ Some d ->
        do {
          Wserver.wprint "%s " (cap (transl_nth conf "born" is));
          Wserver.wprint "%s" (string_of_ondate conf d);
          if birth_place <> "" then Wserver.wprint ",\n" else ();
        }
    | None ->
        if birth_place <> "" then
          Wserver.wprint "%s\n-&nbsp;" (cap (transl_nth conf "born" is))
        else () ];
    if birth_place <> "" then
      Wserver.wprint "%s" (string_of_place conf birth_place)
    else ();
    let baptism = Adef.od_of_codate (get_baptism p) in
    let baptism_place = sou base (get_baptism_place p) in
    match baptism with
    [ Some d ->
        do {
          Wserver.wprint "%s " (cap (transl_nth conf "baptized" is));
          Wserver.wprint "%s" (string_of_ondate conf d);
          if baptism_place <> "" then Wserver.wprint ",\n" else ();
        }
    | None ->
        if baptism_place <> "" then
          Wserver.wprint "%s\n-&nbsp;"
            (cap (transl_nth conf "baptized" is))
        else () ];
    if baptism_place <> "" then
      Wserver.wprint "%s" (string_of_place conf baptism_place)
    else ();
    let death_place = sou base (get_death_place p) in
    match get_death p with
    [ Death dr d ->
        let dr_w =
          match dr with
          [ Unspecified -> transl_nth conf "died" is
          | Murdered -> transl_nth conf "murdered" is
          | Killed -> transl_nth conf "killed (in action)" is
          | Executed -> transl_nth conf "executed (legally killed)" is
          | Disappeared -> transl_nth conf "disappeared" is ]
        in
        let d = Adef.date_of_cdate d in
        do {
          Wserver.wprint "%s " (cap dr_w);
          Wserver.wprint "%s" (string_of_ondate conf d);
          if death_place <> "" then Wserver.wprint ",\n" else ();
        }
    | DeadYoung ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "died young" is));
          if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
        }
    | DeadDontKnowWhen ->
        match (death_place, get_burial p) with
        [ ("", Buried _ | Cremated _) -> ()
        | _ ->
            if death_place <> "" || not (of_course_died conf p) then do {
              Wserver.wprint "%s" (cap (transl_nth conf "died" is));
              if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
            }
            else () ]
    | DontKnowIfDead | NotDead | OfCourseDead -> () ];
    if death_place <> "" then
      Wserver.wprint "%s" (string_of_place conf death_place)
    else ();
    let burial_date_place cod =
      let place = sou base (get_burial_place p) in
      do {
         match Adef.od_of_codate cod with
         [ Some d ->
             do {
               Wserver.wprint " %s" (string_of_ondate conf d);
               if place <> "" then Wserver.wprint ",\n" else ();
             }
         | None -> if place <> "" then Wserver.wprint " -&nbsp;" else () ];
         if place <> "" then
           Wserver.wprint "%s" (string_of_place conf place)
         else ();
      }
    in
    match get_burial p with
    [ Buried cod ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "buried" is));
          burial_date_place cod;
        }
    | Cremated cod ->
        do {
          Wserver.wprint "%s" (cap (transl_nth conf "cremated" is));
          burial_date_place cod;
        }
    | UnknownBurial -> () ];
    let (birth_date, death_date, approx) = get_birth_death_date p in
    match (birth_date, death_date) with
    [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
       Some (Dgreg ({prec = Sure | About | Maybe} as d2) _))
      when d1 <> d2 ->
        let a = CheckItem.time_elapsed d1 d2 in
        if a.year < 0 || a.year = 0 && a.month = 0 then ()
        else do {
          Wserver.wprint "\n(";
          Wserver.wprint "%s " (transl conf "age at death:");
          if not approx && d1.prec = Sure && d2.prec = Sure then ()
          else
            Wserver.wprint "%s " (transl_decline conf "possibly (date)" "");
          Wserver.wprint "%s)" (string_of_age conf a);
        }
    | _ -> () ];
  }
;


(* ********************************************************************** *)
(*  [Fonc] compare_date : date -> date -> int                             *)
(** [Description] : Renvoie 0 si les deux dates sont égales, 1 si la 
                    première date est plus grande et -1 sinon.
                    On ne tiens pas compte de la précision de la date.
    [Args] :
      - d1 : la première date
      - d2 : la deuxième date
    [Retour] : int
    [Rem] : Exporté en clair hors de ce module.                           *)
(* ********************************************************************** *)
value compare_date d1 d2 =
  match (d1, d2) with
  [ (Dgreg dmy1 _, Dgreg dmy2 _) -> 
      match Pervasives.compare dmy1.year dmy2.year with
      [ 0 ->
          match Pervasives.compare dmy1.month dmy2.month with
          [ 0 -> Pervasives.compare dmy1.day dmy2.day
          | x -> x ]
      | x -> x]
  | (Dgreg dmy1 _, Dtext _) -> 1
  | (Dtext _, Dgreg dmy2 _) -> -1
  | (Dtext _, Dtext _) -> 0 ]
;


(* Calendar request *)

value eval_julian_day conf =
  let getint v =
    match p_getint conf.env v with
    [ Some x -> x
    | _ -> 0 ]
  in
  List.fold_left
    (fun d (var, cal, conv, max_month) ->
       let yy =
         match p_getenv conf.env ("y" ^ var) with
         [ Some v ->
            try
              let len = String.length v in
              if cal = Djulian && len > 2 && v.[len-2] = '/' then
                int_of_string (String.sub v 0 (len - 2)) + 1
              else int_of_string v
            with
            [ Failure _ -> 0 ]
         | None -> 0 ]
       in
       let mm = getint ("m" ^ var) in
       let dd = getint ("d" ^ var) in
       let dt = {day = dd; month = mm; year = yy; prec = Sure; delta = 0} in
       match p_getenv conf.env ("t" ^ var) with
       [ Some _ -> conv dt
       | None ->
           match
             (p_getenv conf.env ("y" ^ var ^ "1"),
              p_getenv conf.env ("y" ^ var ^ "2"),
              p_getenv conf.env ("m" ^ var ^ "1"),
              p_getenv conf.env ("m" ^ var ^ "2"),
              p_getenv conf.env ("d" ^ var ^ "1"),
              p_getenv conf.env ("d" ^ var ^ "2"))
           with
           [ (Some _, _, _, _, _, _) -> conv {(dt) with year = yy - 1}
           | (_, Some _, _, _, _, _) -> conv {(dt) with year = yy + 1}
           | (_, _, Some _, _, _, _) ->
               let (yy, mm) =
                 if mm = 1 then (yy - 1, max_month) else (yy, mm - 1)
               in
               conv {(dt) with year = yy; month = mm}
           | (_, _, _, Some _, _, _) ->
               let (yy, mm) =
                 if mm = max_month then (yy + 1, 1) else (yy, mm + 1)
               in
               let r = conv {(dt) with year = yy; month = mm} in
               if r = conv dt then
                 (* turn around problem with Hebrew Adar1/Adar2 *)
                 let (yy, mm) =
                   if mm = max_month then (yy + 1, 1) else (yy, mm + 1)
                 in
                 conv {(dt) with year = yy; month = mm}
               else r
           | (_, _, _, _, Some _, _) -> conv {(dt) with day = dd - 1}
           | (_, _, _, _, _, Some _) -> conv {(dt) with day = dd + 1}
           | _ -> d ] ])
    (Calendar.sdn_of_gregorian conf.today)
    [("g", Dgregorian, Calendar.sdn_of_gregorian, 12);
     ("j", Djulian, Calendar.sdn_of_julian, 12);
     ("f", Dfrench, Calendar.sdn_of_french, 13);
     ("h", Dhebrew, Calendar.sdn_of_hebrew, 13)]
;  

IFDEF OLD THEN declare
value gregorian_month_name conf n =
  capitale (nominative (transl_nth conf "(month)" n))
;
value julian_month_name = gregorian_month_name;
value french_month_name conf n = capitale (nominative (french_month conf n));
value hebrew_month_name conf n = capitale (nominative (hebrew_month conf n));

value print_year conf date cal var =
  do {
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"y%s1\" value=\" &lt; \"" var;
    end;
    stagn "td" begin
      xtag "input" "name=\"y%s\" size=\"6\" maxlength=\"6\" value=\"%s\"" var
        (if cal = Djulian &&
            (date.month > 0 && date.month < 3 ||
             date.month = 3 && date.day > 0 && date.day < 25) then
           sprintf "%d/%d" (date.year - 1) (date.year mod 10)
         else sprintf "%d" date.year);
    end;
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"y%s2\" value=\" &gt; \"" var;
    end;
  }
;

value print_month conf date month_name n_months var =
  do {
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"m%s1\" value=\" &lt; \"" var;
    end;
    tag "td" "align=\"center\"" begin
      tag "select" "name=\"m%s\"" var begin
        for i = 1 to n_months do {
          stagn "option" "value=\"%d\"%s" i
            (if date.month = i then " selected=\"selected\"" else "")
          begin
            Wserver.wprint "%s" (month_name conf (i - 1));
          end
        };
      end;
    end;
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"m%s2\" value=\" &gt; \"" var;
    end;
  }
;

value print_day conf date var =
  do {
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"d%s1\" value=\" &lt; \"" var;
    end;
    stagn "td" begin
      xtag "input" "name=\"d%s\" size=\"2\" maxlength=\"2\" value=\"%d\"" var
        date.day;
    end;
    stagn "td" begin
      xtag "input" "type=\"submit\" name=\"d%s2\" value=\" &gt; \"" var;
    end;
  }
;

value print_some_calendar conf order date cal n month_name n_months var =
  do {
    Wserver.wprint "\n";
    tag "tr" "align=\"%s\"" conf.left begin
      stag "th" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "gregorian/julian/french/hebrew" n));
      end;
      Wserver.wprint "\n";
      match order with
      [ "yymmdd" | "yyyymmdd" ->
          do {
            print_year conf date cal var;
            print_month conf date month_name n_months var;
            print_day conf date var;
          }
      | "mmddyyyy" ->
          do {
            print_month conf date month_name n_months var;
            print_day conf date var;
            print_year conf date cal var;
          }
      | _ ->
          do {
            print_day conf date var;
            print_month conf date month_name n_months var;
            print_year conf date cal var;
          }
      ];
      stagn "td" begin
        xtag "input" "type=\"submit\" name=\"t%s\" value=\" = \"" var;
      end;
    end;
  }
;

value print_calendar_head conf order =
  tag "tr" "align=\"%s\"" conf.left begin
    stag "td" begin Wserver.wprint "&nbsp;"; end;
    Wserver.wprint "\n";
    match order with
    [ "yymmdd" | "yyyymmdd" ->
      for i = 0 to 2 do {
        stag "th" "align=\"center\" colspan=\"3\"" begin
          Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" i));
        end;
        Wserver.wprint "\n";
      }
    | "mmddyyyy" ->
        for i = 0 to 2 do {
          let nth = abs (-i*i + max i 1) in
          stag "th" "align=\"center\" colspan=\"3\"" begin
            Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" nth));
          end;
          Wserver.wprint "\n";
        }
    | _ ->
        for i = 2 downto 0 do {
          stag "th" "align=\"center\" colspan=\"3\"" begin
            Wserver.wprint "%s" (capitale (transl_nth conf "year/month/day" i));
          end;
          Wserver.wprint "\n";
        }
    ];
    stag "td" begin Wserver.wprint "&nbsp;"; end;
    Wserver.wprint "\n";
  end
;

value old_print_calendar conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl_nth conf "calendar/calendars" 1))
  in
  let sdn = eval_julian_day conf in
  let date = Calendar.gregorian_of_sdn Sure sdn in
  let wday =
    let sdn_today = Calendar.sdn_of_gregorian conf.today in
    let x = conf.today_wd - sdn_today + sdn in
    if x < 0 then 6 + (x + 1) mod 7 else x mod 7
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    begin_centered conf;
    tag "table" "border=\"%d\"" conf.border begin
      stag "tbody" begin
        stag "tr" begin
          tag "td" "align=\"%s\"" conf.left begin
            Wserver.wprint "- %s -"
              (capitale (nominative (transl_nth conf "(week day)" wday)));
            if date = conf.today then
              let (hh, mm, ss) = conf.time in
              Wserver.wprint " <tt>%02d:%02d:%02d</tt>" hh mm ss
            else ();
            Wserver.wprint "\n";
          end;
        end;
        stag "tr" begin
          stag "td" "align=\"%s\"" conf.left begin
            Wserver.wprint "&nbsp;";
          end;
        end;
        stag "tr" begin
          tag "td" "align=\"center\"" begin
            tag "form" "method=\"get\" action=\"%s\"" conf.command begin
              tag "p" begin
                List.iter
                  (fun (k, v) ->
                     xtag "input" "type=\"hidden\" name=\"%s\" value=\"%s\"" k
                       (quote_escaped (decode_varenv v)))
                  conf.henv;
                xtag "input" "type=\"hidden\" name=\"m\" value=\"CAL\"";
              end;
              let order = transl conf " !dates order" in
              tag "table" "border=\"1\"" begin
                print_calendar_head conf order;
                print_some_calendar conf order date Dgregorian 0
                  gregorian_month_name 12 "g";
                print_some_calendar conf order
                  (Calendar.julian_of_gregorian date) Djulian 1
                  julian_month_name 12 "j";
                print_some_calendar conf order
                  (Calendar.french_of_gregorian date) Dfrench 2
                  french_month_name 13 "f";
                print_some_calendar conf order
                  (Calendar.hebrew_of_gregorian date) Dhebrew 3
                  hebrew_month_name 13 "h";
              end;
            end;
          end;
        end;
        let moon_txt i =
          transl_nth conf
            "moon age/new moon/first quarter/full moon/last quarter" i
        in
        let mp =
          try Some (Calendar.moon_phase_of_sdn sdn) with [ Failure _ -> None ]
        in
        stag "tr" begin
          stag "td" "align=\"center\"" begin
            match mp with
            [ Some (mp, md) ->
                do {
                  match mp with
                  [ None -> ()
                  | Some (Calendar.NewMoon, hh, mm) ->
                      Wserver.wprint "%s - <tt>%02d:%02d</tt> UT"
                        (capitale (moon_txt 1)) hh mm
                  | Some (Calendar.FirstQuarter, hh, mm) ->
                      Wserver.wprint "%s - <tt>%02d:%02d</tt> UT"
                        (capitale (moon_txt 2)) hh mm
                  | Some (Calendar.FullMoon, hh, mm) ->
                      Wserver.wprint "%s - <tt>%02d:%02d</tt> UT"
                        (capitale (moon_txt 3)) hh mm
                  | Some (Calendar.LastQuarter, hh, mm) ->
                      Wserver.wprint "%s - <tt>%02d:%02d</tt> UT"
                        (capitale (moon_txt 4)) hh mm ];
                  xtag "br";
                  stag "span" "style=\"font-size:80%%\"" begin
                    Wserver.wprint "(%s = %d)" (moon_txt 0) md;
                  end;
                }
            | None -> () ];
            xtag "br";
            xtag "br";
            Wserver.wprint "%s: " (capitale (transl conf "julian day"));
            if sdn < 0 then Wserver.wprint "%d" sdn
            else
              Num.print (fun x -> Wserver.wprint "%s" x)
                (transl conf "(thousand separator)") (Num.of_int sdn);
          end;
        end;
      end;
    end;
    end_centered conf;
    trailer conf;
  }
;
end END;

(* *)

type env 'a =
  [ Vint of int
  | Vother of 'a
  | Vnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value eval_var conf env jd loc =
  fun
  [ ["integer"] ->
      match get_env "integer" env with
      [ Vint i -> VVstring (string_of_int i)
      | _ -> raise Not_found ]
  | ["date" :: sl] -> TemplDate.eval_date_var conf jd sl
  | ["today" :: sl] ->
      TemplDate.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
  | _ -> raise Not_found ]
;

value print_foreach conf print_ast eval_expr =
  let eval_int_expr env jd e =
    let s = eval_expr env jd e in
    try int_of_string s with [ Failure _ -> raise Not_found ]
  in
  let rec print_foreach env jd loc s sl el al =
    match (s, sl) with
    [ ("integer_range", []) -> print_integer_range env jd el al
    | _ -> raise Not_found ]
  and print_integer_range env jd el al =
    let (i1, i2) =
      match el with
      [ [[e1]; [e2]] -> (eval_int_expr env jd e1, eval_int_expr env jd e2)
      | _ -> raise Not_found ]
    in
    for i = i1 to i2 do {
      let env = [("integer", Vint i) :: env] in
      List.iter (print_ast env jd) al;
    }
  in
  print_foreach
;

IFDEF OLD THEN declare
value print_calendar conf base =
  if p_getenv conf.env "old" = Some "on" then old_print_calendar conf base else
  Templ.interp conf base "calendar" (eval_var conf)
    (fun _ -> Templ.eval_transl conf) (fun _ -> raise Not_found)
    get_vother set_vother (print_foreach conf) [] (eval_julian_day conf)
;
end ELSE declare
value print_calendar conf base =
  Hutil.interp conf base "calendar"
    {Templ.eval_var = eval_var conf;
     Templ.eval_transl _ = Templ.eval_transl conf;
     Templ.eval_predefined_apply _ = raise Not_found;
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf}
    [] (eval_julian_day conf)
;
end END;
