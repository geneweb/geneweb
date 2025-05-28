open Config
open Def
open Util

type create_info = Update.create_info = {
  ci_birth_date : date option;
  ci_birth_place : string;
  ci_death : death;
  ci_death_date : date option;
  ci_death_place : string;
  ci_occupation : string;
  ci_public : bool;
}

let ci_empty =
  {
    ci_birth_date = None;
    ci_birth_place = "";
    ci_death = DontKnowIfDead;
    ci_death_date = None;
    ci_death_place = "";
    ci_occupation = "";
    ci_public = false;
  }

let get conf key =
  match p_getenv conf.env key with
  | Some v -> v
  | None -> failwith (key ^ " unbound")

let get_nth conf key cnt = p_getenv conf.env (key ^ string_of_int cnt)

let getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
  | Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let get_purged_fn_sn removed_string first_name surname =
  if
    Name.contains_forbidden_char first_name
    || Name.contains_forbidden_char surname
  then (
    removed_string :=
      (Name.purge first_name ^ " " ^ Name.purge surname) :: !removed_string;
    (Name.purge first_name, Name.purge surname))
  else (first_name, surname)

let getenv_sex conf var =
  match p_getenv conf.env (var ^ "_sex") with
  | Some "M" -> Male
  | Some "F" -> Female
  | Some _ | None -> Neuter

let getn_p conf var ?create_info sex =
  match getn conf var "p" with
  | "create" -> Update.Create (sex, create_info)
  | _s -> Update.Link

let rec reconstitute_sorted_events conf cnt =
  match (get_nth conf "e_id" cnt, get_nth conf "e_pos" cnt) with
  | Some id, Some pos ->
      let id, pos =
        try (int_of_string id, int_of_string pos) with Failure _ -> (0, 0)
      in
      let el = reconstitute_sorted_events conf (cnt + 1) in
      (id, pos) :: el
  | _ -> []

let reconstitute_somebody removed_string conf var =
  let first_name = only_printable (getn conf var "fn") in
  let surname = only_printable (getn conf var "sn") in
  (* S'il y a des caractères interdits, on les supprime *)
  let first_name, surname =
    get_purged_fn_sn removed_string first_name surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let sex = getenv_sex conf var in
  let create = getn_p conf var sex in
  (first_name, surname, occ, create, var)

let update_ci conf create key =
  let public =
    match p_getenv conf.env (key ^ "_pub") with Some "on" -> true | _ -> false
  in
  let death =
    match p_getenv conf.env (key ^ "_od") with
    | Some "on" -> OfCourseDead
    | _ -> DontKnowIfDead
  in
  let occupation =
    match p_getenv conf.env (key ^ "_occu") with Some s -> s | _ -> ""
  in
  let x =
    match create with
    | Update.Create (s, Some ci) ->
        Some
          ( s,
            {
              ci with
              ci_public = public;
              ci_occupation = occupation;
              ci_death = death;
            } )
    | Update.Create (s, None) ->
        Some
          ( s,
            {
              ci_empty with
              ci_public = public;
              ci_occupation = occupation;
              ci_death = death;
            } )
    | Update.Link -> None
  in
  match x with
  | Some (s, ci) -> Update.Create (s, Some ci)
  | None -> Update.Link

(* -- Template stuff -- *)

let bool_val x = Templ.VVbool x
let str_val x = Templ.VVstring x
let safe_val (x : Adef.safe_string) = Templ.VVstring (x :> string)

(* TODO : rewrite, looks bad + find a better name *)
let eval_default_var conf s =
  let extract_var sini s =
    let len = String.length sini in
    if String.length s > len && String.sub s 0 (String.length sini) = sini then
      String.sub s len (String.length s - len)
    else ""
  in

  let v = extract_var "evar_" s in
  if v <> "" then
    match p_getenv (conf.env @ conf.henv) v with
    | Some vv -> safe_val (Util.escape_html vv :> Adef.safe_string)
    | None -> str_val ""
  else
    let v = extract_var "bvar_" s in
    let v = if v = "" then extract_var "cvar_" s else v in
    if v <> "" then
      str_val (try List.assoc v conf.base_env with Not_found -> "")
    else raise Not_found

let eval_date_field = function
  | None -> None
  | Some d -> (
      match d with
      | Dgreg (d, Dgregorian) -> Some d
      | Dgreg (d, Djulian) -> Some (Calendar.julian_of_gregorian d)
      | Dgreg (d, Dfrench) -> Some (Calendar.french_of_gregorian d)
      | Dgreg (d, Dhebrew) -> Some (Calendar.hebrew_of_gregorian d)
      | Dtext _ -> None)

let eval_is_cal cal = function
  | Some (Dgreg (_, x)) -> if x = cal then "1" else ""
  | Some (Dtext _) | None -> ""

let eval_is_prec cond = function
  | Some (Dgreg ({ prec = x; _ }, _)) -> if cond x then "1" else ""
  | Some (Dtext _) | None -> ""

let add_precision s p =
  match p with
  | Maybe -> "?" ^ s
  | Before -> "&lt;" ^ s
  | After -> "&gt;" ^ s
  | About -> "/" ^ s ^ "/"
  | Sure | OrYear _ | YearInt _ -> s

(* TODO : rewrite, looks bad *)
let eval_date_var od s =
  str_val
  @@
  match s with
  | "calendar" -> (
      match od with
      | Some (Dgreg (_, Dgregorian)) -> "gregorian"
      | Some (Dgreg (_, Djulian)) -> "julian"
      | Some (Dgreg (_, Dfrench)) -> "french"
      | Some (Dgreg (_, Dhebrew)) -> "hebrew"
      | _ -> "")
  | "day" -> (
      match eval_date_field od with
      | Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "")
  | "month" -> (
      match eval_date_field od with
      | Some d -> (
          if d.month = 0 then ""
          else
            match od with
            | Some (Dgreg (_, Dfrench)) -> short_f_month d.month
            | _ -> string_of_int d.month)
      | None -> "")
  | "orday" -> (
      match eval_date_field od with
      | Some d -> (
          match d.prec with
          | OrYear d2 | YearInt d2 ->
              if d2.day2 = 0 then "" else string_of_int d2.day2
          | _ -> "")
      | None -> "")
  | "ormonth" -> (
      match eval_date_field od with
      | Some d -> (
          match d.prec with
          | OrYear d2 | YearInt d2 -> (
              if d2.month2 = 0 then ""
              else
                match od with
                | Some (Dgreg (_, Dfrench)) -> short_f_month d2.month2
                | _ -> string_of_int d2.month2)
          | _ -> "")
      | None -> "")
  | "oryear" -> (
      match eval_date_field od with
      | Some d -> (
          match d.prec with
          | OrYear d2 | YearInt d2 -> string_of_int d2.year2
          | _ -> "")
      | None -> "")
  | "prec" -> (
      match od with
      | Some (Dgreg ({ prec = Sure; _ }, _)) -> "sure"
      | Some (Dgreg ({ prec = About; _ }, _)) -> "about"
      | Some (Dgreg ({ prec = Maybe; _ }, _)) -> "maybe"
      | Some (Dgreg ({ prec = Before; _ }, _)) -> "before"
      | Some (Dgreg ({ prec = After; _ }, _)) -> "after"
      | Some (Dgreg ({ prec = OrYear _; _ }, _)) -> "oryear"
      | Some (Dgreg ({ prec = YearInt _; _ }, _)) -> "yearint"
      | _ -> "")
  | "text" -> (
      match od with
      | Some (Dtext s) -> Util.safe_html s |> Adef.as_string
      | Some (Dgreg _) | None -> "")
  | "year" -> (
      match eval_date_field od with
      | Some d -> string_of_int d.year
      | None -> "")
  | "cal_french" -> eval_is_cal Dfrench od
  | "cal_gregorian" -> eval_is_cal Dgregorian od
  | "cal_hebrew" -> eval_is_cal Dhebrew od
  | "cal_julian" -> eval_is_cal Djulian od
  | "prec_no" -> if od = None then "1" else ""
  | "prec_sure" -> eval_is_prec (function Sure -> true | _ -> false) od
  | "prec_about" -> eval_is_prec (function About -> true | _ -> false) od
  | "prec_maybe" -> eval_is_prec (function Maybe -> true | _ -> false) od
  | "prec_before" -> eval_is_prec (function Before -> true | _ -> false) od
  | "prec_after" -> eval_is_prec (function After -> true | _ -> false) od
  | "prec_oryear" -> eval_is_prec (function OrYear _ -> true | _ -> false) od
  | "prec_yearint" ->
      eval_is_prec (function YearInt _ -> true | _ -> false) od
  | _ -> raise Not_found

(* TODO : looks bad *)
let eval_create c = function
  | "birth_day" -> (
      str_val
      @@
      match c with
      | Update.Create
          (_, Some { ci_birth_date = Some (Dgreg (dmy, Dfrench)); _ }) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create
          (_, Some { ci_birth_date = Some (Dgreg ({ day = d; _ }, _)); _ })
        when d <> 0 ->
          string_of_int d
      | _ -> "")
  | "birth_month" -> (
      str_val
      @@
      match c with
      | Update.Create
          (_, Some { ci_birth_date = Some (Dgreg (dmy, Dfrench)); _ }) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.month <> 0 then short_f_month dmy.month else ""
      | Update.Create
          (_, Some { ci_birth_date = Some (Dgreg ({ month = m; _ }, _)); _ })
        when m <> 0 ->
          string_of_int m
      | _ -> "")
  | "birth_place" -> (
      match c with
      | Update.Create (_, Some { ci_birth_place = pl; _ }) ->
          safe_val (Util.escape_html pl :> Adef.safe_string)
      | _ -> str_val "")
  | "birth_year" -> (
      str_val
      @@
      match c with
      | Update.Create (_, Some ci) -> (
          match ci.ci_birth_date with
          | Some (Dgreg (dmy, Dfrench)) ->
              let dmy = Calendar.french_of_gregorian dmy in
              add_precision (string_of_int dmy.year) dmy.prec
          | Some (Dgreg ({ year = y; prec = p; _ }, _)) ->
              add_precision (string_of_int y) p
          | Some _ -> ""
          | None -> if ci.ci_public then "p" else "")
      | _ -> "")
  | "death_day" -> (
      str_val
      @@
      match c with
      | Update.Create
          (_, Some { ci_death_date = Some (Dgreg (dmy, Dfrench)); _ }) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create
          (_, Some { ci_death_date = Some (Dgreg ({ day = d; _ }, _)); _ })
        when d <> 0 ->
          string_of_int d
      | _ -> "")
  | "death_month" -> (
      str_val
      @@
      match c with
      | Update.Create
          (_, Some { ci_death_date = Some (Dgreg (dmy, Dfrench)); _ }) ->
          let dmy = Calendar.french_of_gregorian dmy in
          short_f_month dmy.month
      | Update.Create
          (_, Some { ci_death_date = Some (Dgreg ({ month = m; _ }, _)); _ })
        when m <> 0 ->
          string_of_int m
      | _ -> "")
  | "death_place" -> (
      match c with
      | Update.Create (_, Some { ci_death_place = pl; _ }) ->
          safe_val (Util.escape_html pl :> Adef.safe_string)
      | _ -> str_val "")
  | "death_year" -> (
      str_val
      @@
      match c with
      | Update.Create
          (_, Some { ci_death_date = Some (Dgreg (dmy, Dfrench)); _ }) ->
          let dmy = Calendar.french_of_gregorian dmy in
          add_precision (string_of_int dmy.year) dmy.prec
      | Update.Create
          ( _,
            Some
              { ci_death_date = Some (Dgreg ({ year = y; prec = p; _ }, _)); _ }
          ) ->
          add_precision (string_of_int y) p
      | Update.Create (_, Some { ci_death = death; ci_death_date = None; _ })
        -> (
          match death with DeadDontKnowWhen -> "+" | NotDead -> "-" | _ -> "")
      | _ -> "")
  | "occupation" -> (
      match c with
      | Update.Create (_, Some { ci_occupation = occupation; _ }) ->
          safe_val (Util.escape_html occupation :> Adef.safe_string)
      | _ -> str_val "")
  | "sex" -> (
      str_val
      @@
      match c with
      | Update.Create (Male, _) -> "male"
      | Update.Create (Female, _) -> "female"
      | Update.Create (Neuter, _) -> "neuter"
      | _ -> "")
  | _ -> raise Not_found
