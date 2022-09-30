open Config
open Def
open Util

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
  if Name.contains_forbidden_char first_name ||
     Name.contains_forbidden_char surname
  then
    begin
      removed_string :=
        (Name.purge first_name ^ " " ^ Name.purge surname) ::
        !removed_string;
      Name.purge first_name, Name.purge surname
    end
  else first_name, surname

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
  match get_nth conf "e_id" cnt, get_nth conf "e_pos" cnt with
  | Some id, Some pos ->
      let (id, pos) =
        try int_of_string id, int_of_string pos with Failure _ -> 0, 0
      in
      let el = reconstitute_sorted_events conf (cnt + 1) in (id, pos) :: el
  | _ -> []

let reconstitute_somebody removed_string conf var =
  let first_name = only_printable (getn conf var "fn") in
  let surname = only_printable (getn conf var "sn") in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) = get_purged_fn_sn removed_string first_name surname in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let sex = getenv_sex conf var in
  let create = getn_p conf var sex in
  first_name, surname, occ, create, var

(* -- Template stuff -- *)

let bool_val x = TemplAst.VVbool x
let str_val x = TemplAst.VVstring x
let safe_val (x : Adef.safe_string) = TemplAst.VVstring (x :> string)

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

let eval_date_field =
  function
    | None -> None
    | Some d ->
      match d with
        Dgreg (d, Dgregorian) -> Some d
      | Dgreg (d, Djulian) -> Some (Calendar.julian_of_gregorian d)
      | Dgreg (d, Dfrench) -> Some (Calendar.french_of_gregorian d)
      | Dgreg (d, Dhebrew) -> Some (Calendar.hebrew_of_gregorian d)
      | Dtext _ -> None

let eval_is_cal cal =
  function
  | Some (Dgreg (_, x)) -> if x = cal then "1" else ""
  | Some (Dtext _) | None -> ""

let eval_is_prec cond =
  function
  | Some (Dgreg ({prec = x}, _)) -> if cond x then "1" else ""
  | Some (Dtext _) | None -> ""

(* TODO : rewrite, looks bad *)
let eval_date_var od s =
  str_val @@
  match s with
  | "calendar" ->
      begin match od with
        Some (Dgreg (_, Dgregorian)) -> "gregorian"
      | Some (Dgreg (_, Djulian)) -> "julian"
      | Some (Dgreg (_, Dfrench)) -> "french"
      | Some (Dgreg (_, Dhebrew)) -> "hebrew"
      | _ -> ""
      end
  | "day" ->
      begin match eval_date_field od with
        Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> ""
      end
  | "month" ->
      begin match eval_date_field od with
        Some d ->
          if d.month = 0 then ""
          else
            begin match od with
              Some (Dgreg (_, Dfrench)) -> short_f_month d.month
            | _ -> string_of_int d.month
            end
      | None -> ""
      end
  | "orday" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 ->
              if d2.day2 = 0 then "" else string_of_int d2.day2
          | _ -> ""
          end
      | None -> ""
      end
  | "ormonth" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 ->
              if d2.month2 = 0 then ""
              else
                begin match od with
                  Some (Dgreg (_, Dfrench)) -> short_f_month d2.month2
                | _ -> string_of_int d2.month2
                end
          | _ -> ""
          end
      | None -> ""
      end
  | "oryear" ->
      begin match eval_date_field od with
        Some d ->
          begin match d.prec with
            OrYear d2 | YearInt d2 -> string_of_int d2.year2
          | _ -> ""
          end
      | None -> ""
      end
  | "prec" ->
      begin match od with
        Some (Dgreg ({prec = Sure}, _)) -> "sure"
      | Some (Dgreg ({prec = About}, _)) -> "about"
      | Some (Dgreg ({prec = Maybe}, _)) -> "maybe"
      | Some (Dgreg ({prec = Before}, _)) -> "before"
      | Some (Dgreg ({prec = After}, _)) -> "after"
      | Some (Dgreg ({prec = OrYear _}, _)) -> "oryear"
      | Some (Dgreg ({prec = YearInt _}, _)) -> "yearint"
      | _ -> ""
      end
  | "text" ->
      begin match od with
      | Some (Dtext s) -> Util.safe_html s |> Adef.as_string
      | Some (Dgreg _) | None -> ""
      end
  | "year" ->
      begin match eval_date_field od with
      | Some d -> string_of_int d.year
      | None -> ""
      end
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
  | "prec_yearint" -> eval_is_prec (function YearInt _ -> true | _ -> false) od
  | _ -> raise Not_found
