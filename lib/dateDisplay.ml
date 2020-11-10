(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open Gwdb

let get_wday conf = function
  | Dgreg ({ prec = Sure ; delta = 0 } as d, _) when d.day <> 0 && d.month <> 0 ->
    let jd = Calendar.sdn_of_gregorian d in
    let wday =
      let jd_today = Calendar.sdn_of_gregorian conf.today in
      let x = conf.today_wd - jd_today + jd in
      if x < 0 then 6 + (x + 1) mod 7 else x mod 7
    in
    " (" ^ transl_nth conf "(week day)" wday ^ ")"
  | _ -> ""

let death_symbol conf =
  try List.assoc "death_symbol" conf.base_env
  with Not_found -> "†"

let code_date conf encoding d m y =
  let apply_date_code =
    function
      'd' -> string_of_int d
    | 'm' -> transl_nth conf "(month)" (m - 1)
    | 'y' -> string_of_int y
    | c -> "%" ^ String.make 1 c
  in
  let rec loop i =
    if i = String.length encoding then ""
    else
      let (s, i) =
        match encoding.[i] with
          '%' when i + 1 < String.length encoding ->
            let s = apply_date_code encoding.[i+1] in s, i + 1
        | '[' ->
            begin try (* code similar to Util.gen_decline *)
              let len = String.length encoding in
              let j = String.index_from encoding i ']' in
              let k = String.index_from encoding i '|' in
              if k < j && j + 2 < len && encoding.[j + 1] = '%' then
                let s = apply_date_code encoding.[j + 2] in
                let s1 =
                  if start_with_vowel s then String.sub encoding (k + 1) (j - k - 1)
                  else String.sub encoding (i + 1) (k - i - 1)
                in
                s1 ^ s, j + 2
              else
                String.make 1 '[', i
            with Not_found -> String.make 1 '[', i
            end
        | c -> String.make 1 c, i
      in
      s ^ loop (i + 1)
  in
  loop 0

let code_dmy conf d =
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

let default_french_month =
  let tab =
    [| "Vendemiaire"; "Brumaire"; "Frimaire"; "Nivose"; "Pluviose"; "Ventose";
       "Germinal"; "Floreal"; "Prairial"; "Messidor"; "Thermidor";
       "Fructidor"; "Extra" |]
  in
  fun m -> tab.(m)

let default_hebrew_month =
  let tab =
    [| "Tishri"; "Heshvan"; "Kislev"; "Tevet"; "Shevat"; "AdarI"; "AdarII";
       "Nisan"; "Iyyar"; "Sivan"; "Tammuz"; "Av"; "Elul" |]
  in
  fun m -> tab.(m)

let french_month conf m =
  let r = transl_nth conf "(french revolution month)" m in
  if r = "[(french revolution month)]" then "[" ^ default_french_month m ^ "]"
  else r

let hebrew_month conf m =
  let r = transl_nth conf "(hebrew month)" m in
  if r = "[(hebrew month)]" then "[" ^ default_hebrew_month m ^ "]" else r

let code_french_year conf y =
  transl_nth conf "year/month/day" 3 ^ " " ^
  (if y >= 1 && y < 4000 then Mutil.roman_of_arabian y else string_of_int y)

let code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d ^ (if d = 1 then "<sup>er</sup>" else "")
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ french_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ code_french_year conf y

let code_hebrew_date conf d m y =
  let s = if d = 0 then "" else string_of_int d in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ hebrew_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ string_of_int y

let string_of_on_prec_dmy_aux conf sy sy2 d =
  match d.prec with
    Sure ->
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
  | OrYear d2 ->
      let s =
        if d.day = 0 && d.month = 0 then transl conf "in (year)" ^ " " ^ sy
        else if d.day = 0 then transl_decline conf "in (month year)" sy
        else transl_decline conf "on (day month year)" sy
      in
      let s2 =
        if d2.day2 = 0 && d2.month2 = 0 then
          transl conf "in (year)" ^ " " ^ sy2
        else if d2.day2 = 0 then transl_decline conf "in (month year)" sy2
        else transl_decline conf "on (day month year)" sy2
      in
      s ^ " " ^ transl conf "or" ^ " " ^ Mutil.nominative s2
  | YearInt d2 ->
      let s =
        if d.day = 0 && d.month = 0 then sy
        else if d.day = 0 then sy
        else transl_decline conf "on (day month year)" sy
      in
      let s2 =
        if d2.day2 = 0 && d2.month2 = 0 then sy2
        else if d2.day2 = 0 then sy2
        else transl_decline conf "on (day month year)" sy2
      in
      transl conf "between (date)" ^ " " ^ s ^ " " ^ transl_nth conf "and" 0 ^
      " " ^ Mutil.nominative s2

let replace_spaces_by_nbsp s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then loop (i + 1) (Buff.mstore len "&nbsp;")
    else loop (i + 1) (Buff.store len s.[i])
  in
  loop 0 0

let string_of_on_prec_dmy conf sy sy2 d =
  let r = string_of_on_prec_dmy_aux conf sy sy2 d in
  replace_spaces_by_nbsp r

let string_of_on_french_dmy conf d =
  let sy = code_french_date conf d.day d.month d.year in
  let sy2 =
    match d.prec with
      OrYear d2 | YearInt d2 ->
        code_french_date conf d2.day2 d2.month2 d2.year2
    | _ -> ""
  in
  string_of_on_prec_dmy conf sy sy2 d

let string_of_on_hebrew_dmy conf d =
  let sy = code_hebrew_date conf d.day d.month d.year in
  let sy2 =
    match d.prec with
      OrYear d2 | YearInt d2 ->
        code_hebrew_date conf d2.day2 d2.month2 d2.year2
    | _ -> ""
  in
  string_of_on_prec_dmy conf sy sy2 d

let string_of_prec_dmy conf s s2 d =
  match d.prec with
    Sure -> Mutil.nominative s
  | About -> transl_decline conf "about (date)" s
  | Before -> transl_decline conf "before (date)" s
  | After -> transl_decline conf "after (date)" s
  | Maybe -> transl_decline conf "possibly (date)" s
  | OrYear _ -> s ^ " " ^ transl conf "or" ^ " " ^ Mutil.nominative s2
  | YearInt _ ->
    transl conf "between (date)" ^ " " ^ s ^ " " ^
    transl_nth conf "and" 0 ^ " " ^ Mutil.nominative s2

let string_of_dmy_aux fn conf d =
  let sy = code_dmy conf d in
  let sy2 =
    match d.prec with
    | OrYear d2 | YearInt d2 -> code_dmy conf (Date.dmy_of_dmy2 d2)
    | _ -> ""
  in
  fn conf sy sy2 d

let string_of_on_dmy conf d =
  string_of_dmy_aux string_of_on_prec_dmy conf d

let string_of_dmy conf d =
  string_of_dmy_aux string_of_prec_dmy conf d

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
let translate_dmy conf (fst, snd, trd) cal short =
  let translate_month m =
    match cal with
      Dfrench when m <> "" ->
        if short then Util.short_f_month (int_of_string m)
        else french_month conf (int_of_string m)
    | Dhebrew when m <> "" ->
        if short then
          String.uppercase_ascii
            (String.sub (hebrew_month conf (int_of_string m)) 0 2)
        else hebrew_month conf (int_of_string m)
    | _ -> m
  in
  let translate_year y =
    match cal with
      Dfrench ->
        let y1 = int_of_string y in
        if y1 >= 1 && y1 < 4000 then Mutil.roman_of_arabian y1 else y
    | _ -> y
  in
  match transl conf "!dates order" with
    "yymmdd" | "yyyymmdd" -> translate_year fst, translate_month snd, trd
  | "mmddyyyy" -> translate_month fst, snd, translate_year trd
  | _ -> fst, translate_month snd, translate_year trd

(** [decode_dmy conf dmy]
    Returns a triplet corresponding to day/month/year, arranged in
    the order defined by [!dates order] keyword in the lexicon.
    Supported formats are: "dmyyyy" / "mmddyyyy" / "yyyymmdd" / "ddmmyyyy" and "ddmmyy".
    NB: "yy" and "yyyy" variants will produce the same output ([string_of_int] without padding)
    If the format is not supported "ddmmyyyy" is used.
*)
let decode_dmy conf d =
  match transl conf "!dates order" with
    "dmyyyy" ->
      string_of_int d.day, string_of_int d.month, string_of_int d.year
  | "mmddyyyy" ->
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      begin match d.day, d.month, d.year with
        0, 0, year -> "", "", string_of_int year
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in m, "", string_of_int year
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in m, d, string_of_int year
      end
  | "yyyymmdd" | "yymmdd" ->
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      begin match d.day, d.month, d.year with
        0, 0, year -> string_of_int year, "", ""
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in string_of_int year, m, ""
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in string_of_int year, m, d
      end
  | "ddmmyyyy" | "ddmmyy" | _ ->
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match d.day, d.month, d.year with
        0, 0, year -> "", "", string_of_int year
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in "", m, string_of_int year
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in d, m, string_of_int year

let gregorian_precision conf d =
  if d.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^ string_of_on_dmy conf d ^ " " ^
    transl_nth conf "and" 0 ^ " " ^ string_of_on_dmy conf d2

let string_of_date_aux ?(link = true) ?(dmy = string_of_dmy) ?(sep = " ") conf =
  function
    Dgreg (d, Dgregorian) ->
      let s = dmy conf d in
      if link && d.day > 0 then
        Printf.sprintf
          "<a href=\"%sm=CAL&yg=%d&mg=%d&dg=%d&tg=1\" class=\"date\">%s</a>"
          (commd conf) d.year d.month d.day s
      else s
  | Dgreg (d, Djulian) ->
      let cal_prec =
        if d.year < 1582 then "" else " (" ^ gregorian_precision conf d ^ ")"
      in
      let d1 = Calendar.julian_of_gregorian d in
      let year_prec =
        if d1.month > 0 && d1.month < 3 ||
           d1.month = 3 && d1.day > 0 && d1.day < 25
        then
          Printf.sprintf " (%d/%d)" (d1.year - 1) (d1.year mod 10)
        else ""
      in
      let s =
        dmy conf d1 ^ year_prec ^ sep ^
        transl_nth conf "gregorian/julian/french/hebrew" 1 ^ cal_prec
      in
      if link && d1.day > 0 then
        Printf.sprintf
          "<a href=\"%sm=CAL&yj=%d&mj=%d&dj=%d&tj=1\" class=\"date\">%s</a>"
          (commd conf) d1.year d1.month d1.day s
      else s
  | Dgreg (d, Dfrench) ->
      let d1 = Calendar.french_of_gregorian d in
      let s = string_of_on_french_dmy conf d1 in
      let s =
        if link && d1.day > 0 then
          Printf.sprintf
            "<a href=\"%sm=CAL&yf=%d&mf=%d&df=%d&tf=1\" class=\"date\">%s</a>"
            (commd conf) d1.year d1.month d1.day s
        else s
      in
      begin match d.prec with
        Sure -> s ^ sep ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s
      end
  | Dgreg (d, Dhebrew) ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let s = string_of_on_hebrew_dmy conf d1 in
      begin match d.prec with
        Sure -> s ^ sep ^ " (" ^ gregorian_precision conf d ^ ")"
      | About | Before | After | Maybe | OrYear _ | YearInt _ -> s
      end
  | Dtext t -> "(" ^ string_with_macros conf [] t ^ ")"

let string_of_ondate ?link conf d =
  string_of_date_aux ?link ~dmy:string_of_on_dmy conf d
  |> Util.translate_eval

let string_of_date conf =
  function
    Dgreg (d, _) -> string_of_dmy conf d
  | Dtext t -> t

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
let string_slash_of_date conf date =
  let rec slashify_dmy (fst, snd, trd) d =
    let code fst snd trd =
      List.fold_right (fun s accu -> if s <> "" then s ^ "/" ^ accu else accu)
        [fst; snd] trd
    in
    match d.prec with
      OrYear d2 ->
        let sy = code fst snd trd in
        let d2 = Date.dmy_of_dmy2 d2 in
        let sy2 = slashify_dmy (decode_dmy conf d2) d2 in
        sy ^ " " ^ transl conf "or" ^ " " ^ sy2
    | YearInt d2 ->
        let sy = code fst snd trd in
        let d2 = Date.dmy_of_dmy2 d2 in
        let sy2 = slashify_dmy (decode_dmy conf d2) d2 in
        transl conf "between (date)" ^ " " ^ sy ^ " " ^
        transl_nth conf "and" 0 ^ " " ^ sy2
    | _ -> let sy = code fst snd trd in string_of_prec_dmy conf sy "" d
  in
  match date with
    Dgreg (d, Dgregorian) -> slashify_dmy (decode_dmy conf d) d
  | Dgreg (d, Djulian) ->
      let d1 = Calendar.julian_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Djulian true) d1 ^
      " (" ^ transl_nth conf "gregorian/julian/french/hebrew" 1 ^ ")"
  | Dgreg (d, Dfrench) ->
      let d1 = Calendar.french_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Dfrench true) d1
  | Dgreg (d, Dhebrew) ->
      let d1 = Calendar.french_of_gregorian d in
      slashify_dmy (translate_dmy conf (decode_dmy conf d1) Dhebrew true) d1 ^
      " (" ^ transl_nth conf "gregorian/julian/french/hebrew" 3 ^ ")"
  | Dtext t -> t

let string_of_age conf a =
  match a with
    {day = 0; month = 0; year = y} ->
      if y > 1 then string_of_int y ^ " " ^ transl conf "years old"
      else if y = 1 then transl conf "one year old"
      else transl conf "birth"
  | {day = 0; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else transl conf "less than one month old"
  | {day = d; month = m; year = y} ->
      if y >= 2 then string_of_int y ^ " " ^ transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int (y * 12 + m) ^ " " ^ transl conf "months old"
      else if m = 1 then transl conf "one month old"
      else if d >= 2 then string_of_int d ^ " " ^ transl conf "days old"
      else if d = 1 then transl conf "one day old"
      else "0"


(* ************************************************************************ *)
(*  [Fonc] prec_text : config -> Def.dmy -> string                          *)
(** [Description] : Renvoie la précision d'une date.
    [Args] :
      - conf : configuration de la base
      - d    : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let prec_text conf d =
  match d.prec with
    About ->
      (* On utilise le dictionnaire pour être sur *)
      (* que ce soit compréhensible de tous.      *)
      begin match transl conf "about (short date)" with
        "ca" -> "ca "
      | s -> s
      end
  | Maybe -> "?"
  | Before -> "<"
  | After -> ">"
  | OrYear _ -> "|"
  | YearInt _ -> ".."
  | Sure -> ""


(* ************************************************************************ *)
(*  [Fonc] day_text : Def.dmy -> string                                     *)
(** [Description] : Renvoie le jour d'une date.
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let day_text d = if d.day = 0 then "" else string_of_int d.day


(* ************************************************************************ *)
(*  [Fonc] month_text : Def.dmy -> string                                   *)
(** [Description] : Renvoie le mois d'une date.
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let month_text d = if d.month = 0 then "" else string_of_int d.month


(* ************************************************************************ *)
(*  [Fonc] year_text : Def.dmy -> string                                    *)
(** [Description] : Renvoie l'année d'une date.
    si la précision de la date est OrYear ou YearInt, alors la date est
    renvoyée sous la forme yyy1/yyy2 ou yyy1..yyy2
    [Args] :
      - d : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let year_text d =
  match d.prec with
    OrYear d2 ->
        if d.year = d2.year2 then string_of_int d.year
        else (string_of_int d.year) ^ "/" ^ string_of_int d2.year2
  | YearInt d2 ->
        if d.year = d2.year2 then string_of_int d.year
        else (string_of_int d.year) ^ ".." ^ string_of_int d2.year2
  | _ -> string_of_int d.year

(* ************************************************************************ *)
(*  [Fonc] prec_year_text : config -> Def.dmy -> string                     *)
(** [Description] : Renvoie la précision d'une date et l'année de la date.
    [Args] :
      - conf : configuration de la base
      - d    : Def.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let prec_year_text conf d =
  let s =
    match d.prec with
      About ->
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        begin match transl conf "about (short date)" with
          "ca" -> "ca "
        | s -> s
        end
    | Maybe -> "?"
    | Before -> "/"
    | _ -> ""
  in
  let s = s ^ year_text d in
  match d.prec with
    After -> s ^ "/"
  | _ -> s

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
let short_dates_text conf base p =
  if authorized_age conf base p then
    let (birth_date, death_date, _) = Date.get_birth_death_date p in
    let s =
      match birth_date, death_date with
        Some (Dgreg (b, _)), Some (Dgreg (d, _)) ->
          prec_year_text conf b ^ "-" ^ prec_year_text conf d
      | Some (Dgreg (b, _)), _ ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          begin match get_death p with
            Death (_, _) | DeadDontKnowWhen | DeadYoung ->
              prec_year_text conf b ^ "-"
          | _ -> prec_year_text conf b
          end
      | _, Some (Dgreg (d, _)) -> death_symbol conf ^ prec_year_text conf d
      | _, _ ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          match get_death p with
            Death (_, _) | DeadDontKnowWhen | DeadYoung -> death_symbol conf
          | _ -> ""
    in
    if s <> "" then " <bdo dir=\"ltr\">" ^ s ^ "</bdo>"
    else s
  else ""

(* ********************************************************************** *)
(*  [Fonc] short_family_dates_text : config -> base -> fam -> string      *)
(** [Description] : Renvoie la concatenation de l'année de relation ou
      mariage et l'année de divorce ou séparation. La précision
      de la date est ajoutée pour chaque année.
      L'affichage est le suivant :
      * 1700      (date relation/mariage - pas de séparation/divorce)
      * 1700-1780 (date relation/mariage - date séparation/divorce)
      * 1700-     (date relation/mariage - séparation/divorce sans date)
      * -1700     (pas date relation/mariage - séparation/divorce)
      * -         (pas date relation/mariage - séparation/divorce sans date)
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - fam  : famille
    [Retour] : string
    [Rem] :                                                               *)
(* ********************************************************************** *)
let short_family_dates_text conf _base fam =
    let marr_dates =
      match Adef.od_of_cdate (get_marriage fam) with
      | Some d ->
        begin match d with
        | Dgreg (dmy, _) -> Some (prec_year_text conf dmy)
        | _ -> Some " "
        end
      | _ -> Some " "
    in
    let sep_dates =
      match List.find_opt (fun e ->
            e.efam_name = Efam_Divorce ||
            e.efam_name = Efam_Annulation ||
            e.efam_name = Efam_Separated)
          (get_fevents fam) with
      | Some e ->
        begin match Adef.od_of_cdate e.efam_date with
        | Some d ->
          begin match d with
          | Dgreg (dmy, _) -> Some (prec_year_text conf dmy)
          | _ -> Some " "
          end
        | _ -> Some " "
        end
      | _ -> None
    in
    match marr_dates, sep_dates with
    | Some m, Some s -> m ^ "-" ^ s
    | Some m, None -> m
    | _, _ -> ""

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
let short_marriage_date_text conf base fam p1 p2 =
  if authorized_age conf base p1 && authorized_age conf base p2 then
    match Adef.od_of_cdate (get_marriage fam) with
      Some (Dgreg (d, _)) ->
        "<span style=\"font-size:70%\">" ^ prec_year_text conf d ^ "</span>"
    | _ -> ""
  else ""

let string_of_place conf pl = Util.string_with_macros conf [] pl

let print_dates conf base p =
  let cap s = ", " ^ s in
  let is = index_of_sex (get_sex p) in
  let birth_place = sou base (get_birth_place p) in
  begin match Adef.od_of_cdate (get_birth p) with
    Some d ->
      Output.print_string conf (cap (transl_nth conf "born" is));
      Output.print_string conf " ";
      Output.print_string conf (string_of_ondate conf d);
      if birth_place <> "" then Output.print_string conf ",\n"
  | None ->
      if birth_place <> "" then begin
        Output.print_string conf (cap (transl_nth conf "born" is)) ;
        Output.print_string conf "\n-&nbsp;"
      end
  end;
  if birth_place <> "" then
    Output.print_string conf (string_of_place conf birth_place);
  let baptism = Adef.od_of_cdate (get_baptism p) in
  let baptism_place = sou base (get_baptism_place p) in
  begin match baptism with
    Some d ->
      Output.print_string conf (cap (transl_nth conf "baptized" is));
      Output.print_string conf " ";
      Output.print_string conf (string_of_ondate conf d);
      if baptism_place <> "" then Output.print_string conf ",\n"
  | None ->
      if baptism_place <> "" then begin
        Output.print_string conf (cap (transl_nth conf "baptized" is)) ;
        Output.print_string conf "\n-&nbsp;"
      end
  end;
  if baptism_place <> "" then
    Output.print_string conf (string_of_place conf baptism_place);
  let death_place = sou base (get_death_place p) in
  begin match get_death p with
    Death (dr, d) ->
      let dr_w =
        match dr with
          Unspecified -> transl_nth conf "died" is
        | Murdered -> transl_nth conf "murdered" is
        | Killed -> transl_nth conf "killed (in action)" is
        | Executed -> transl_nth conf "executed (legally killed)" is
        | Disappeared -> transl_nth conf "disappeared" is
      in
      let d = Adef.date_of_cdate d in
      Output.print_string conf (cap dr_w);
      Output.print_string conf " ";
      Output.print_string conf (string_of_ondate conf d);
      if death_place <> "" then Output.print_string conf ",\n"
  | DeadYoung ->
      Output.print_string conf (cap (transl_nth conf "died young" is));
      if death_place <> "" then Output.print_string conf "\n-&nbsp;"
  | DeadDontKnowWhen ->
      begin match death_place, get_burial p with
        "", (Buried _ | Cremated _) -> ()
      | _ ->
          if death_place <> "" || not (of_course_died conf p) then
            begin
              Output.print_string conf (cap (transl_nth conf "died" is));
              if death_place <> "" then Output.print_string conf "\n-&nbsp;"
            end
      end
  | DontKnowIfDead | NotDead | OfCourseDead -> ()
  end;
  if death_place <> "" then
    Output.print_string conf (string_of_place conf death_place);
  let burial_date_place cod =
    let place = sou base (get_burial_place p) in
    begin match Adef.od_of_cdate cod with
      Some d ->
        Output.print_string conf " ";
        Output.print_string conf (string_of_ondate conf d);
        if place <> "" then Output.print_string conf ",\n"
    | None -> if place <> "" then Output.print_string conf " -&nbsp;"
    end;
    if place <> "" then Output.print_string conf (string_of_place conf place)
  in
  begin match get_burial p with
    Buried cod ->
      Output.print_string conf (cap (transl_nth conf "buried" is));
      burial_date_place cod
  | Cremated cod ->
      Output.print_string conf (cap (transl_nth conf "cremated" is));
      burial_date_place cod
  | UnknownBurial -> ()
  end;
  let (birth_date, death_date, approx) = Date.get_birth_death_date p in
  match birth_date, death_date with
    Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
    Some (Dgreg (({prec = Sure | About | Maybe} as d2), _))
    when d1 <> d2 ->
      let a = Date.time_elapsed d1 d2 in
      if a.year < 0 || a.year = 0 && a.month = 0 then ()
      else
        begin
          Output.print_string conf "\n(";
          Output.print_string conf (transl conf "age at death:");
          Output.print_string conf " ";
          if not approx && d1.prec = Sure && d2.prec = Sure then ()
          else begin
            Output.print_string conf (transl_decline conf "possibly (date)" "");
            Output.print_string conf " ";
          end ;
          Output.print_string conf (string_of_age conf a) ;
          Output.print_string conf ")"
        end
  | _ -> ()
