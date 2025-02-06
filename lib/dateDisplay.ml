(* Copyright (c) 1998-2007 INRIA *)

let get_wday conf = function
  | Date.Dgreg (({ Date.prec = Date.Sure; Date.delta = 0 } as d), _)
    when d.Date.day <> 0 && d.Date.month <> 0 ->
      let jd = Date.to_sdn ~from:Date.Dgregorian d in
      let wday =
        let jd_today = Date.to_sdn ~from:Date.Dgregorian conf.Config.today in
        let x = conf.Config.today_wd - jd_today + jd in
        if x < 0 then 6 + ((x + 1) mod 7) else x mod 7
      in
      " (" ^ Util.transl_nth conf "(week day)" wday ^ ")"
  | _ -> ""

let death_symbol conf =
  try List.assoc "death_symbol" conf.Config.base_env with Not_found -> "†"

let code_date conf encoding d m y =
  let apply_date_code = function
    | 'd' -> string_of_int d
    | 'm' -> Util.transl_nth conf "(month)" (m - 1)
    | 'y' -> string_of_int y
    | c -> "%" ^ String.make 1 c
  in
  let rec loop i =
    if i = String.length encoding then ""
    else
      let s, i =
        match encoding.[i] with
        | '%' when i + 1 < String.length encoding ->
            let s = apply_date_code encoding.[i + 1] in
            (s, i + 1)
        | '[' -> (
            try
              (* code similar to Util.gen_decline *)
              let len = String.length encoding in
              let j = String.index_from encoding i ']' in
              let k = String.index_from encoding i '|' in
              if k < j && j + 2 < len && encoding.[j + 1] = '%' then
                let s = apply_date_code encoding.[j + 2] in
                let s1 =
                  if Util.start_with_vowel s then
                    String.sub encoding (k + 1) (j - k - 1)
                  else String.sub encoding (i + 1) (k - i - 1)
                in
                (s1 ^ s, j + 2)
              else (String.make 1 '[', i)
            with Not_found -> (String.make 1 '[', i))
        | c -> (String.make 1 c, i)
      in
      s ^ loop (i + 1)
  in
  loop 0

let code_dmy conf d =
  let encoding =
    let n =
      if d.Date.day = 1 then 0
      else if d.Date.day != 0 then 1
      else if d.Date.month != 0 then 2
      else 3
    in
    Util.transl_nth conf "(date)" n
  in
  code_date conf encoding d.Date.day d.Date.month d.Date.year

let default_french_month =
  let tab =
    [|
      "Vendemiaire";
      "Brumaire";
      "Frimaire";
      "Nivose";
      "Pluviose";
      "Ventose";
      "Germinal";
      "Floreal";
      "Prairial";
      "Messidor";
      "Thermidor";
      "Fructidor";
      "Extra";
    |]
  in
  fun m -> tab.(m)

let default_hebrew_month =
  let tab =
    [|
      "Tishri";
      "Heshvan";
      "Kislev";
      "Tevet";
      "Shevat";
      "AdarI";
      "AdarII";
      "Nisan";
      "Iyyar";
      "Sivan";
      "Tammuz";
      "Av";
      "Elul";
    |]
  in
  fun m -> tab.(m)

let french_month conf m =
  let r = Util.transl_nth conf "(french revolution month)" m in
  if r = "[(french revolution month)]" then "[" ^ default_french_month m ^ "]"
  else r

let hebrew_month conf m =
  let r = Util.transl_nth conf "(hebrew month)" m in
  if r = "[(hebrew month)]" then "[" ^ default_hebrew_month m ^ "]" else r

let code_french_year conf y =
  Util.transl_nth conf "year/month/day" 3
  ^ " "
  ^ if y >= 1 && y < 4000 then Mutil.roman_of_arabian y else string_of_int y

let code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d ^ if d = 1 then "<sup>er</sup>" else ""
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
  match d.Date.prec with
  | Date.Sure ->
      if d.Date.day = 0 && d.Date.month = 0 then
        Util.transl conf "in (year)" ^ " " ^ sy
      else if d.Date.day = 0 then Util.transl_decline conf "in (month year)" sy
      else Util.transl_decline conf "on (day month year)" sy
  | Date.About | Date.Before | Date.After ->
      let s = sy in
      if d.Date.prec = Date.About then Util.transl_decline conf "about (date)" s
      else if d.Date.prec = Date.Before then
        Util.transl_decline conf "before (date)" s
      else Util.transl_decline conf "after (date)" s
  | Date.Maybe ->
      let s =
        if d.Date.day = 0 && d.Date.month = 0 then
          Util.transl conf "in (year)" ^ " " ^ sy
        else if d.Date.day = 0 then
          Util.transl_decline conf "in (month year)" sy
        else Util.transl_decline conf "on (day month year)" sy
      in
      Util.transl_decline conf "possibly (date)" s
  | Date.OrYear d2 ->
      let s =
        if d.Date.day = 0 && d.Date.month = 0 then
          Util.transl conf "in (year)" ^ " " ^ sy
        else if d.Date.day = 0 then
          Util.transl_decline conf "in (month year)" sy
        else Util.transl_decline conf "on (day month year)" sy
      in
      let s2 =
        if d2.Date.day2 = 0 && d2.Date.month2 = 0 then
          Util.transl conf "in (year)" ^ " " ^ sy2
        else if d2.Date.day2 = 0 then
          Util.transl_decline conf "in (month year)" sy2
        else Util.transl_decline conf "on (day month year)" sy2
      in
      s ^ " " ^ Util.transl conf "or" ^ " " ^ Mutil.nominative s2
  | Date.YearInt d2 ->
      let s =
        if d.Date.day = 0 && d.Date.month = 0 then sy
        else if d.Date.day = 0 then sy
        else Util.transl_decline conf "on (day month year)" sy
      in
      let s2 =
        if d2.Date.day2 = 0 && d2.Date.month2 = 0 then sy2
        else if d2.Date.day2 = 0 then sy2
        else Util.transl_decline conf "on (day month year)" sy2
      in
      Util.transl conf "between (date)"
      ^ " " ^ s ^ " "
      ^ Util.transl_nth conf "and" 0
      ^ " " ^ Mutil.nominative s2

let replace_spaces_by_nbsp s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then loop (i + 1) (Buff.mstore len "&nbsp;")
    else loop (i + 1) (Buff.store len s.[i])
  in
  loop 0 0

let string_of_on_prec_dmy conf sy sy2 d =
  Adef.safe
  @@
  let r = string_of_on_prec_dmy_aux conf sy sy2 d in
  replace_spaces_by_nbsp r

let string_of_on_french_dmy conf d =
  let sy = code_french_date conf d.Date.day d.Date.month d.Date.year in
  let sy2 =
    match d.Date.prec with
    | Date.OrYear d2 | Date.YearInt d2 ->
        code_french_date conf d2.Date.day2 d2.Date.month2 d2.Date.year2
    | _ -> ""
  in
  string_of_on_prec_dmy conf sy sy2 d

let string_of_on_hebrew_dmy conf d =
  let sy = code_hebrew_date conf d.Date.day d.Date.month d.Date.year in
  let sy2 =
    match d.Date.prec with
    | Date.OrYear d2 | Date.YearInt d2 ->
        code_hebrew_date conf d2.Date.day2 d2.Date.month2 d2.Date.year2
    | _ -> ""
  in
  string_of_on_prec_dmy conf sy sy2 d

let string_of_prec_dmy conf s s2 d =
  Adef.safe
  @@
  match d.Date.prec with
  | Date.Sure -> Mutil.nominative s
  | Date.About -> Util.transl_decline conf "about (date)" s
  | Date.Before -> Util.transl_decline conf "before (date)" s
  | Date.After -> Util.transl_decline conf "after (date)" s
  | Date.Maybe -> Util.transl_decline conf "possibly (date)" s
  | Date.OrYear _ -> s ^ " " ^ Util.transl conf "or" ^ " " ^ Mutil.nominative s2
  | Date.YearInt _ ->
      Util.transl conf "between (date)"
      ^ " " ^ s ^ " "
      ^ Util.transl_nth conf "and" 0
      ^ " " ^ Mutil.nominative s2

let string_of_dmy_aux fn conf d =
  let sy = code_dmy conf d in
  let sy2 =
    match d.Date.prec with
    | Date.OrYear d2 | Date.YearInt d2 -> code_dmy conf (Date.dmy_of_dmy2 d2)
    | _ -> ""
  in
  fn conf sy sy2 d

let string_of_on_dmy conf d = string_of_dmy_aux string_of_on_prec_dmy conf d
let string_of_dmy conf d = string_of_dmy_aux string_of_prec_dmy conf d

(* ************************************************************************ *)
(* [Fonc] translate_dmy : config -> (string * string * string) ->
                            calendar -> bool -> (string * string * string) *)

(* ************************************************************************ *)

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
let translate_dmy conf (fst, snd, trd) cal short =
  let translate_month m =
    match cal with
    | Date.Dfrench when m <> "" ->
        if short then Util.short_f_month (int_of_string m)
        else french_month conf (int_of_string m)
    | Date.Dhebrew when m <> "" ->
        if short then
          String.uppercase_ascii
            (String.sub (hebrew_month conf (int_of_string m)) 0 2)
        else hebrew_month conf (int_of_string m)
    | _ -> m
  in
  let translate_year y =
    match cal with
    | Date.Dfrench ->
        let y1 = int_of_string y in
        if y1 >= 1 && y1 < 4000 then Mutil.roman_of_arabian y1 else y
    | _ -> y
  in
  match Util.transl conf "!dates order" with
  | "yymmdd" | "yyyymmdd" -> (translate_year fst, translate_month snd, trd)
  | "mmddyyyy" -> (translate_month fst, snd, translate_year trd)
  | _ -> (fst, translate_month snd, translate_year trd)

(** [decode_dmy conf dmy]
    Returns a triplet corresponding to day/month/year, arranged in
    the order defined by [!dates order] keyword in the lexicon.
    Supported formats are: "dmyyyy" / "mmddyyyy" / "yyyymmdd" / "ddmmyyyy" and "ddmmyy".
    NB: "yy" and "yyyy" variants will produce the same output ([string_of_int] without padding)
    If the format is not supported "ddmmyyyy" is used.
*)
let decode_dmy conf d =
  match Util.transl conf "!dates order" with
  | "dmyyyy" ->
      ( string_of_int d.Date.day,
        string_of_int d.Date.month,
        string_of_int d.Date.year )
  | "mmddyyyy" -> (
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.Date.day, d.Date.month, d.Date.year) with
      | 0, 0, year -> ("", "", string_of_int year)
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in
          (m, "", string_of_int year)
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (m, d, string_of_int year))
  | "yyyymmdd" | "yymmdd" -> (
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.Date.day, d.Date.month, d.Date.year) with
      | 0, 0, year -> (string_of_int year, "", "")
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in
          (string_of_int year, m, "")
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (string_of_int year, m, d))
  | "ddmmyyyy" | "ddmmyy" | _ -> (
      (* Si le jour et/ou le mois n'est pas sur 2 caractères, *)
      (* on rajoute les 0 nécessaires.                        *)
      match (d.Date.day, d.Date.month, d.Date.year) with
      | 0, 0, year -> ("", "", string_of_int year)
      | 0, month, year ->
          let m = Printf.sprintf "%02d" month in
          ("", m, string_of_int year)
      | day, month, year ->
          let d = Printf.sprintf "%02d" day in
          let m = Printf.sprintf "%02d" month in
          (d, m, string_of_int year))

let gregorian_precision conf d =
  if d.Date.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      let sdn = d.Date.delta + Date.to_sdn ~from:Date.Dgregorian d in
      Date.gregorian_of_sdn ~prec:d.Date.prec sdn
    in
    Adef.safe
    @@ Util.transl conf "between (date)"
    ^ " "
    ^ (string_of_on_dmy conf d :> string)
    ^ " "
    ^ Util.transl_nth conf "and" 0
    ^ " "
    ^ (string_of_on_dmy conf d2 :> string)

let string_of_date_aux ?(link = true) ?(dmy = string_of_dmy)
    ?(sep = Adef.safe " ") conf =
  let mk_link c d (s : Adef.safe_string) =
    Adef.safe
    @@ Printf.sprintf
         {|<a href="%sm=CAL&y%c=%d&m%c=%d&d%c=%d&t%c=1" class="date">%s</a>|}
         (Util.commd conf :> string)
         c d.Date.year c d.Date.month c d.Date.day c
         (s :> string)
  in
  function
  | Date.Dtext t ->
      let open Def in
      "(" ^<^ (Util.escape_html t :> Adef.safe_string) ^>^ ")"
  | Date.Dgreg (d, calendar) -> (
      let d1 = Date.convert ~from:Date.Dgregorian ~to_:calendar d in
      match calendar with
      | Date.Dgregorian ->
          let s = dmy conf d in
          if link && d.Date.day > 0 then mk_link 'g' d s else s
      | Date.Djulian ->
          let cal_prec =
            if d.Date.year < 1582 then Adef.safe ""
            else
              let open Def in
              " (" ^<^ gregorian_precision conf d ^>^ ")"
          in
          (* Julian calendar new year's date changes across time and space *)
          let year_prec =
            if
              (d1.Date.month > 0 && d1.Date.month < 3)
              || (d1.Date.month = 3 && d1.Date.day > 0 && d1.Date.day < 25)
            then
              Printf.sprintf " (%d/%d)" (d1.Date.year - 1) (d1.Date.year mod 10)
            else ""
          in
          let s =
            let open Def in
            dmy conf d1 ^^^ year_prec ^<^ sep
            ^^^ Util.transl_nth conf "gregorian/julian/french/hebrew" 1
            ^<^ cal_prec
          in
          if link && d1.Date.day > 0 then mk_link 'j' d1 s else s
      | Date.Dfrench -> (
          let s = string_of_on_french_dmy conf d1 in
          let s = if link && d1.Date.day > 0 then mk_link 'f' d1 s else s in
          match d.Date.prec with
          | Date.Sure | Date.About | Date.Before | Date.After | Date.Maybe ->
              let open Def in
              s ^^^ sep ^^^ " (" ^<^ gregorian_precision conf d ^>^ ")"
          | Date.OrYear _ | Date.YearInt _ -> s)
      | Date.Dhebrew -> (
          let s = string_of_on_hebrew_dmy conf d1 in
          match d.Date.prec with
          | Date.Sure | Date.About | Date.Before | Date.After | Date.Maybe ->
              let open Def in
              s ^^^ sep ^^^ " (" ^<^ gregorian_precision conf d ^>^ ")"
          | Date.OrYear _ | Date.YearInt _ -> s))

let string_of_ondate ?link conf d =
  (string_of_date_aux ?link ~dmy:string_of_on_dmy conf d :> string)
  |> Util.translate_eval |> Adef.safe

let string_of_date conf = function
  | Date.Dgreg (d, _) -> string_of_dmy conf d
  | Date.Dtext t -> (Util.escape_html t :> Adef.safe_string)

let string_slash_of_date conf date =
  let rec slashify_dmy (fst, snd, trd) d =
    let code fst snd trd =
      List.fold_right
        (fun s accu -> if s <> "" then s ^ "/" ^ accu else accu)
        [ fst; snd ] trd
    in
    match d.Date.prec with
    | Date.OrYear d2 ->
        let sy = code fst snd trd in
        let d2 = Date.dmy_of_dmy2 d2 in
        let sy2 = slashify_dmy (decode_dmy conf d2) d2 in
        sy ^ " " ^ Util.transl conf "or" ^ " " ^ sy2
    | Date.YearInt d2 ->
        let sy = code fst snd trd in
        let d2 = Date.dmy_of_dmy2 d2 in
        let sy2 = slashify_dmy (decode_dmy conf d2) d2 in
        Util.transl conf "between (date)"
        ^ " " ^ sy ^ " "
        ^ Util.transl_nth conf "and" 0
        ^ " " ^ sy2
    | _ ->
        let sy = code fst snd trd in
        (string_of_prec_dmy conf sy "" d :> string)
  in
  match date with
  | Date.Dtext t -> (Util.escape_html t :> Adef.safe_string)
  | Date.Dgreg (d, cal) -> (
      let d1 = Date.convert ~from:Date.Dgregorian ~to_:cal d in
      Adef.safe
      @@
      match cal with
      | Date.Dgregorian -> slashify_dmy (decode_dmy conf d) d
      | Date.Djulian ->
          slashify_dmy
            (translate_dmy conf (decode_dmy conf d1) Date.Djulian true)
            d1
          ^ " ("
          ^ Util.transl_nth conf "gregorian/julian/french/hebrew" 1
          ^ ")"
      | Date.Dfrench ->
          slashify_dmy
            (translate_dmy conf (decode_dmy conf d1) Date.Dfrench true)
            d1
      | Date.Dhebrew ->
          slashify_dmy
            (translate_dmy conf (decode_dmy conf d1) Date.Dhebrew true)
            d1
          ^ " ("
          ^ Util.transl_nth conf "gregorian/julian/french/hebrew" 3
          ^ ")")

let string_of_age conf a =
  Adef.safe
  @@
  match a with
  | { Date.day = 0; Date.month = 0; Date.year = y } ->
      if y > 1 then string_of_int y ^ " " ^ Util.transl conf "years old"
      else if y = 1 then Util.transl conf "one year old"
      else Util.transl conf "birth"
  | { Date.day = 0; Date.month = m; Date.year = y } ->
      if y >= 2 then string_of_int y ^ " " ^ Util.transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int ((y * 12) + m) ^ " " ^ Util.transl conf "months old"
      else if m = 1 then Util.transl conf "one month old"
      else Util.transl conf "less than one month old"
  | { Date.day = d; Date.month = m; Date.year = y } ->
      if y >= 2 then string_of_int y ^ " " ^ Util.transl conf "years old"
      else if y > 0 || m > 1 then
        string_of_int ((y * 12) + m) ^ " " ^ Util.transl conf "months old"
      else if m = 1 then Util.transl conf "one month old"
      else if d >= 2 then string_of_int d ^ " " ^ Util.transl conf "days old"
      else if d = 1 then Util.transl conf "one day old"
      else "0"

(* ************************************************************************ *)
(*  [Fonc] prec_text : config -> Date.dmy -> string                          *)

(* ************************************************************************ *)

(** [Description] : Renvoie la précision d'une date.
    [Args] :
      - conf : configuration de la base
      - d    : Date.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
let prec_text conf d =
  match d.Date.prec with
  | Date.About -> (
      (* On utilise le dictionnaire pour être sur *)
      (* que ce soit compréhensible de tous.      *)
      match Util.transl conf "about (short date)" with "ca" -> "ca " | s -> s)
  | Date.Maybe -> "?"
  | Date.Before -> "<"
  | Date.After -> ">"
  | Date.OrYear _ -> "|"
  | Date.YearInt _ -> ".."
  | Date.Sure -> ""

(* ************************************************************************ *)
(*  [Fonc] month_text : Date.dmy -> string                                   *)

(* ************************************************************************ *)

(** [Description] : Renvoie le mois d'une date.
    [Args] :
      - d : Date.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
let month_text d = if d.Date.month = 0 then "" else string_of_int d.Date.month

(* ************************************************************************ *)
(*  [Fonc] year_text : Date.dmy -> string                                    *)

(* ************************************************************************ *)

(** [Description] : Renvoie l'année d'une date.
    [Args] :
      - d : Date.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
let year_text d =
  match d.Date.prec with
  | Date.OrYear d2 when d.Date.year <> d2.Date.year2 ->
      string_of_int d.Date.year ^ "/" ^ string_of_int d2.Date.year2
  | Date.YearInt d2 when d.Date.year <> d2.Date.year2 ->
      string_of_int d.Date.year ^ ".." ^ string_of_int d2.Date.year2
  | _ -> string_of_int d.Date.year

(* ************************************************************************ *)
(*  [Fonc] prec_year_text : config -> Date.dmy -> string                     *)

(* ************************************************************************ *)

(** [Description] : Renvoie la précision d'une date et l'année de la date.
    [Args] :
      - conf : configuration de la base
      - d    : Date.dmy
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                             *)
let prec_year_text conf d =
  let s =
    match d.Date.prec with
    | Date.About -> (
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        match Util.transl conf "about (short date)" with
        | "ca" -> "ca "
        | s -> s)
    | Date.Maybe -> "?"
    | Date.Before -> "/"
    | _ -> ""
  in
  let s = s ^ year_text d in
  match d.Date.prec with Date.After -> s ^ "/" | _ -> s

(* ********************************************************************** *)
(*  [Fonc] short_dates_text : config -> base -> person -> string          *)

(* ********************************************************************** *)

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
let short_dates_text conf base p =
  Adef.safe
  @@
  if Util.authorized_age conf base p then
    let birth_date, death_date, _ = Gutil.get_birth_death_date p in
    let s =
      match (birth_date, death_date) with
      | Some (Date.Dgreg (b, _)), Some (Date.Dgreg (d, _)) ->
          prec_year_text conf b ^ "-" ^ prec_year_text conf d
      | Some (Date.Dgreg (b, _)), _ -> (
          (* La personne peut être décédée mais ne pas avoir de date. *)
          match Gwdb.get_death p with
          | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
              prec_year_text conf b ^ "-"
          | _ -> prec_year_text conf b)
      | _, Some (Date.Dgreg (d, _)) -> death_symbol conf ^ prec_year_text conf d
      | _, _ -> (
          (* La personne peut être décédée mais ne pas avoir de date. *)
          match Gwdb.get_death p with
          | Death (_, _) | DeadDontKnowWhen | DeadYoung -> death_symbol conf
          | _ -> "")
    in
    if s <> "" then " <bdo dir=\"ltr\">" ^ s ^ "</bdo>" else s
  else ""

(* ********************************************************************** *)
(* [Fonc] short_marriage_date_text :
            config -> base -> person -> person -> string *)

(* ********************************************************************** *)

(** [Description] : Renvoie l'année de la date de mariage ansi que la
                    précision de la date.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p1   : conjoint 1
      - p2   : conjoint 2
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                           *)
let short_marriage_date_text conf base fam p1 p2 =
  Adef.safe
  @@
  if Util.authorized_age conf base p1 && Util.authorized_age conf base p2 then
    match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
    | Some d ->
        "<span style=\"font-size:70%\">" ^ prec_year_text conf d ^ "</span>"
    | None -> ""
  else ""

(* For public interfce, force [string_of_prec_dmy] args to be safe strings *)
let string_of_prec_dmy conf s s2 d =
  let s = (s : Adef.safe_string :> string) in
  let s2 = (s2 : Adef.safe_string :> string) in
  string_of_prec_dmy conf s s2 d
