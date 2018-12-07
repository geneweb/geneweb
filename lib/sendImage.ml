(* $Id: sendImage.ml,v 5.7 2018/11/17 09:58:44 hg Exp $ *)

open Config
open Def
open Gwdb
open TemplAst
open Util

let bogus_person_index = Adef.iper_of_int (-1)

let string_person_of base p =
  let fp ip =
    let p = poi base ip in
    sou base (get_first_name p), sou base (get_surname p), get_occ p,
    Update.Link, ""
  in
  Futil.map_person_ps fp (sou base) (gen_person_of_person p)

(* Interpretation of template file taken from 'updind.txt' *)

type 'a env =
    Vstring of string
  | Vint of int
  | Vother of 'a
  | Vcnt of int ref
  | Vbool of bool
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""

let obsolete_list = ref []

let obsolete version var new_var r =
  if List.mem var !obsolete_list then r
  else if Sys.unix then
    begin
      Printf.eprintf "*** <W> updind.txt: \"%s\" obsolete since v%s%s\n" var
        version
        (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
      flush stderr;
      obsolete_list := var :: !obsolete_list;
      r
    end
  else r

let bool_val x = VVbool x
let str_val x = VVstring x

let strip_br str =
  let len = String.length str in
  if len > 4 && (String.sub str (len - 4) 4) = "<br>" then
    (String.sub str 0 (len - 4)) else str

let include_hed_trl conf name =
  match Util.open_etc_file_name conf name with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()

let rec eval_var conf base env p _loc sl =
  try eval_special_var conf base sl with
    Not_found -> eval_simple_var conf base env p sl
and eval_simple_var conf base env p =
  function
    ["alias"] -> eval_string_env "alias" env
  | ["acc_if_titles"] -> bool_val (p.access = IfTitles)
  | ["acc_private"] -> bool_val (p.access = Private)
  | ["acc_public"] -> bool_val (p.access = Public)
  | ["bapt_place"] -> str_val (quote_escaped p.baptism_place)
  | ["bapt_note"] -> str_val (quote_escaped p.baptism_note)
  | ["bapt_src"] -> str_val (quote_escaped p.baptism_src)
  | ["birth"; s] -> eval_date_var (Adef.od_of_cdate p.birth) s
  | ["birth_place"] -> str_val (quote_escaped p.birth_place)
  | ["birth_note"] -> str_val (quote_escaped p.birth_note)
  | ["birth_src"] -> str_val (quote_escaped p.birth_src)
  | ["bapt"; s] -> eval_date_var (Adef.od_of_cdate p.baptism) s
  | ["base"; "name"] -> str_val conf.bname
  | ["bt_buried"] ->
      bool_val
        (match p.burial with
           Buried _ -> true
         | _ -> false)
  | ["bt_cremated"] ->
      bool_val
        (match p.burial with
           Cremated _ -> true
         | _ -> false)
  | ["bt_unknown_burial"] -> bool_val (p.burial = UnknownBurial)
  | ["burial"; s] ->
      let od =
        match p.burial with
          Buried cod -> Adef.od_of_cdate cod
        | Cremated cod -> Adef.od_of_cdate cod
        | _ -> None
      in
      eval_date_var od s
  | ["burial_place"] -> str_val (quote_escaped p.burial_place)
  | ["burial_note"] -> str_val (quote_escaped p.burial_note)
  | ["burial_src"] -> str_val (quote_escaped p.burial_src)
  | ["cnt"] -> eval_int_env "cnt" env
  | ["dead_dont_know_when"] -> bool_val (p.death = DeadDontKnowWhen)
  | ["death"; s] ->
      let od =
        match p.death with
          Death (_, cd) -> Some (Adef.date_of_cdate cd)
        | _ -> None
      in
      eval_date_var od s
  | ["death_place"] -> str_val (quote_escaped p.death_place)
  | ["death_note"] -> str_val (quote_escaped p.death_note)
  | ["death_src"] -> str_val (quote_escaped p.death_src)
  | ["died_young"] -> bool_val (p.death = DeadYoung)
  (*| ["digest"] -> eval_string_env "digest" env*)
  | ["digest"] -> str_val (default_image_name base (poi base p.key_index))
  | ["dont_know_if_dead"] -> bool_val (p.death = DontKnowIfDead)
  | ["dr_disappeared"] -> eval_is_death_reason Disappeared p.death
  | ["dr_executed"] -> eval_is_death_reason Executed p.death
  | ["dr_killed"] -> eval_is_death_reason Killed p.death
  | ["dr_murdered"] -> eval_is_death_reason Murdered p.death
  | ["dr_unspecified"] -> eval_is_death_reason Unspecified p.death
  | "event" :: sl ->
      let e =
        match get_env "cnt" env with
          Vint i ->
            (try Some (List.nth p.pevents (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_event_var e sl
  | ["event_date"; s] ->
      let od =
        match get_env "cnt" env with
          Vint i ->
            begin try
              let e = List.nth p.pevents (i - 1) in
              Adef.od_of_cdate e.epers_date
            with Failure _ -> None
            end
        | _ -> None
      in
      eval_date_var od s
  | ["event_str"] ->
      begin match get_env "cnt" env with
        Vint i ->
          begin try
            let p = poi base p.key_index in
            let e = List.nth (get_pevents p) (i - 1) in
            let name =
              capitale (Util.string_of_pevent_name conf base e.epers_name)
            in
            let date =
              match Adef.od_of_cdate e.epers_date with
                Some d -> Date.string_of_date conf d
              | None -> ""
            in
            let place = Util.string_of_place conf (sou base e.epers_place) in
            let note = sou base e.epers_note in
            let src = sou base e.epers_src in
            let wit =
              List.fold_right
                (fun (w, wk) accu ->
                   (Util.string_of_witness_kind conf p wk ^ ": " ^
                    Util.person_text conf base (poi base w)) ::
                   accu)
                (Array.to_list e.epers_witnesses) []
            in
            let s = String.concat ", " [name; date; place; note; src] in
            let sw = String.concat ", " wit in str_val (s ^ ", " ^ sw)
          with Failure _ -> str_val ""
          end
      | _ -> str_val ""
      end
  | ["first_name"] -> str_val (quote_escaped p.first_name)
  | ["first_name_alias"] -> eval_string_env "first_name_alias" env
  | ["has_aliases"] -> bool_val (p.aliases <> [])
  | ["has_birth_date"] -> bool_val (Adef.od_of_cdate p.birth <> None)
  | ["has_image"] -> bool_val (Util.has_image conf base (poi base p.key_index))
  | ["has_old_image"] ->
      begin match Util.auto_image_file conf base (poi base p.key_index) "saved" with
        Some _s -> bool_val true
      | _ -> bool_val false
      end
  | ["has_keydir"] ->
        let p = poi base p.key_index in
        bool_val (Util.has_keydir conf base p)
  | ["has_pevent_birth"] ->
      let rec loop pevents =
        match pevents with
          [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Birth then bool_val true else loop l
      in
      loop p.pevents
  | ["has_pevent_baptism"] ->
      let rec loop pevents =
        match pevents with
          [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Baptism then bool_val true else loop l
      in
      loop p.pevents
  | ["has_pevent_death"] ->
      let rec loop pevents =
        match pevents with
          [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Death then bool_val true else loop l
      in
      loop p.pevents
  | ["has_pevent_burial"] ->
      let rec loop pevents =
        match pevents with
          [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Burial then bool_val true else loop l
      in
      loop p.pevents
  | ["has_pevent_cremation"] ->
      let rec loop pevents =
        match pevents with
          [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Cremation then bool_val true else loop l
      in
      loop p.pevents
  | ["has_pevents"] -> bool_val (p.pevents <> [])
  | ["has_primary_pevents"] ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | evt :: l ->
            match evt.epers_name with
              Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
              Epers_Cremation ->
                true
            | _ -> loop l
      in
      bool_val (loop p.pevents)
  | ["has_secondary_pevents"] ->
      let rec loop pevents =
        match pevents with
          [] -> false
        | evt :: l ->
            match evt.epers_name with
              Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
              Epers_Cremation ->
                loop l
            | _ -> true
      in
      bool_val (loop p.pevents)
  | ["has_first_names_aliases"] -> bool_val (p.first_names_aliases <> [])
  | ["has_qualifiers"] -> bool_val (p.qualifiers <> [])
  | ["has_relations"] -> bool_val (p.rparents <> [])
  | ["has_surnames_aliases"] -> bool_val (p.surnames_aliases <> [])
  | ["has_titles"] -> bool_val (p.titles <> [])
  | ["image"] -> str_val (quote_escaped p.image)
  | ["portrait"] -> (* see auto_image_file_name in perso.ml *)
      begin match auto_image_file conf base (poi base p.key_index) "" with
        Some s -> str_val (Filename.basename s)
      | _ -> str_val ""
      end
  | ["portrait_saved"] ->
      begin match auto_image_file conf base (poi base p.key_index) "saved" with
        Some s -> str_val (Filename.basename s)
      | _ -> str_val ""
      end
  | ["index"] -> str_val (string_of_int (Adef.int_of_iper p.key_index))
  | ["is_female"] -> bool_val (p.sex = Female)
  | ["is_male"] -> bool_val (p.sex = Male)
  | ["is_first"] ->
      begin match get_env "first" env with
        Vbool x -> bool_val x
      | _ -> raise Not_found
      end
  | ["is_last"] ->
      begin match get_env "last" env with
        Vbool x -> bool_val x
      | _ -> raise Not_found
      end
  | ["keydir"] -> str_val (default_image_name base (poi base p.key_index))
  | ["keydir_img"] ->
      begin match get_env "keydir_img" env with
        Vstring s -> str_val s
      | _ -> raise Not_found
      end
  | ["keydir_notes"] ->
      begin match get_env "keydir_img" env with
        Vstring s -> str_val ((Filename.remove_extension s) ^ ".txt")
      | _ -> raise Not_found
      end
  | ["keydir_img_key"] ->
      begin match get_env "keydir_img" env with
        Vstring s -> str_val (Mutil.tr '+' ' ' (code_varenv s))
      | _ -> raise Not_found
      end
  | ["keydir_img_old"] ->
      begin match get_env "keydir_img_old" env with
        Vstring s -> str_val s
      | _ -> raise Not_found
      end
  | ["keydir_img_old_key"] ->
      begin match get_env "keydir_img_old" env with
        Vstring s -> str_val (Mutil.tr '+' ' ' (code_varenv s))
      | _ -> raise Not_found
      end
  | ["keydir_img_nbr"] ->
      str_val (string_of_int
        (List.length (get_keydir conf base (poi base p.key_index))))
  | ["keydir_old_img_nbr"] ->
      str_val (string_of_int
        (List.length (get_keydir_old conf base (poi base p.key_index))))
  | ["keydir_img_notes"] ->
      begin match get_env "keydir_img_notes" env with
        Vstring f ->
          let ext = Filename.extension f in
          let fname = Filename.chop_suffix f ext in
          str_val (Util.replace_quotes
            (get_keydir_img_notes conf base (poi base p.key_index) fname))
      | _ -> raise Not_found
      end
  | ["keydir_img_src"] ->
      begin match get_env "keydir_img_src" env with
        Vstring f ->
          let ext = Filename.extension f in
          let fname = Filename.chop_suffix f ext in
          let str =
            get_keydir_img_notes conf base (poi base p.key_index) fname
          in
          let str =
            try
              let i = String.index str '\n' in
              let len = String.length str in
              if i < len then String.sub str (i + 1) (len - i - 2)
              else ""
            with Not_found -> ""
          in
          let src =
            try
              let i = String.index str '\n' in
              String.sub str 0 i
            with Not_found -> ""
          in
          str_val (Util.replace_quotes (strip_br src))
      | _ -> raise Not_found
      end
  | ["keydir_img_title"] ->
      begin match get_env "keydir_img_title" env with
        Vstring f ->
          let ext = Filename.extension f in
          let fname = Filename.chop_suffix f ext in
          let str =
            get_keydir_img_notes conf base (poi base p.key_index) fname
          in
          let title =
            try
              let i = String.index str '\n' in
              String.sub str 0 i
            with Not_found -> ""
          in
          let title = if title = "" && str <> "" then str else title in
          str_val (Util.replace_quotes (strip_br title))
      | _ -> raise Not_found
      end
  | ["nb_pevents"] -> str_val (string_of_int (List.length p.pevents))
  | ["not_dead"] -> bool_val (p.death = NotDead)
  | ["notes"] -> str_val (quote_escaped p.notes)
  | ["next_pevent"] ->
      begin match get_env "next_pevent" env with
        Vcnt c -> str_val (string_of_int !c)
      | _ -> str_val ""
      end
  | ["incr_next_pevent"] ->
      begin match get_env "next_pevent" env with
        Vcnt c -> incr c; str_val ""
      | _ -> str_val ""
      end
  | ["occ"] -> str_val (if p.occ <> 0 then string_of_int p.occ else "")
  | ["occupation"] -> str_val (quote_escaped p.occupation)
  | ["of_course_dead"] -> bool_val (p.death = OfCourseDead)
  | ["public_name"] -> str_val (quote_escaped p.public_name)
  | ["qualifier"] -> eval_string_env "qualifier" env
  | "relation" :: sl ->
      let r =
        match get_env "cnt" env with
          Vint i ->
            (try Some (List.nth p.rparents (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_relation_var r sl
  | ["X"] -> str_val (Filename.dir_sep)
  | ["sources"] -> str_val (quote_escaped p.psources)
  | ["surname"] -> str_val (quote_escaped p.surname)
  | ["surname_alias"] -> eval_string_env "surname_alias" env
  | "title" :: sl ->
      let t =
        match get_env "cnt" env with
          Vint i ->
            (try Some (List.nth p.titles (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_title_var t sl
  | ["title_date_start"; s] ->
      let od =
        match get_env "cnt" env with
          Vint i ->
            begin try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_cdate t.t_date_start
            with Failure _ -> None
            end
        | _ -> None
      in
      eval_date_var od s
  | ["title_date_end"; s] ->
      let od =
        match get_env "cnt" env with
          Vint i ->
            begin try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_cdate t.t_date_end
            with Failure _ -> None
            end
        | _ -> None
      in
      eval_date_var od s
  | ["wcnt"] -> eval_int_env "wcnt" env
  | ["has_witness"] ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e -> bool_val (e.epers_witnesses <> [| |])
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | "witness" :: sl ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e ->
              begin match get_env "wcnt" env with
                Vint i ->
                  let i = i - 1 in
                  let k =
                    if i >= 0 && i < Array.length e.epers_witnesses then
                      fst e.epers_witnesses.(i)
                    else if
                      i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                    then
                      "", "", 0, Update.Create (Neuter, None), ""
                    else raise Not_found
                  in
                  eval_person_var k sl
              | _ -> raise Not_found
              end
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | ["witness_kind"] ->
      begin match get_env "cnt" env with
        Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          begin match e with
            Some e ->
              begin match get_env "wcnt" env with
                Vint i ->
                  let i = i - 1 in
                  if i >= 0 && i < Array.length e.epers_witnesses then
                    match snd e.epers_witnesses.(i) with
                      Witness_GodParent -> str_val "godp"
                    | Witness_Officer -> str_val "offi"
                    | _ -> str_val ""
                  else if
                    i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                  then
                    str_val ""
                  else raise Not_found
              | _ -> raise Not_found
              end
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | [s] ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
          Some vv -> str_val (quote_escaped vv)
        | None -> str_val ""
      else
        let v = extract_var "bvar_" s in
        let v = if v = "" then extract_var "cvar_" s else v in
        if v <> "" then
          str_val (try List.assoc v conf.base_env with Not_found -> "")
        else raise Not_found
  | _ -> raise Not_found
and eval_date_var od s = str_val (eval_date_var_aux od s)
and eval_date_var_aux od =
  function
    "calendar" ->
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
        Some (Dtext s) -> s
      | _ -> ""
      end
  | "year" ->
      begin match eval_date_field od with
        Some d -> string_of_int d.year
      | None -> ""
      end
  | x ->
      let r =
        match x with
          "cal_french" -> eval_is_cal Dfrench od
        | "cal_gregorian" -> eval_is_cal Dgregorian od
        | "cal_hebrew" -> eval_is_cal Dhebrew od
        | "cal_julian" -> eval_is_cal Djulian od
        | "prec_no" -> if od = None then "1" else ""
        | "prec_sure" ->
            eval_is_prec
              (function
                 Sure -> true
               | _ -> false)
              od
        | "prec_about" ->
            eval_is_prec
              (function
                 About -> true
               | _ -> false)
              od
        | "prec_maybe" ->
            eval_is_prec
              (function
                 Maybe -> true
               | _ -> false)
              od
        | "prec_before" ->
            eval_is_prec
              (function
                 Before -> true
               | _ -> false)
              od
        | "prec_after" ->
            eval_is_prec
              (function
                 After -> true
               | _ -> false)
              od
        | "prec_oryear" ->
            eval_is_prec
              (function
                 OrYear _ -> true
               | _ -> false)
              od
        | "prec_yearint" ->
            eval_is_prec
              (function
                 YearInt _ -> true
               | _ -> false)
              od
        | _ -> raise Not_found
      in
      obsolete "5.00" x (if x.[0] = 'c' then "calendar" else "prec") r
and eval_date_field =
  function
    Some d ->
      begin match d with
        Dgreg (d, Dgregorian) -> Some d
      | Dgreg (d, Djulian) -> Some (Calendar.julian_of_gregorian d)
      | Dgreg (d, Dfrench) -> Some (Calendar.french_of_gregorian d)
      | Dgreg (d, Dhebrew) -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None
      end
  | None -> None
and eval_event_var e =
  function
    ["e_name"] ->
      begin match e with
        Some {epers_name = name} ->
          begin match name with
            Epers_Birth -> str_val "#birt"
          | Epers_Baptism -> str_val "#bapt"
          | Epers_Death -> str_val "#deat"
          | Epers_Burial -> str_val "#buri"
          | Epers_Cremation -> str_val "#crem"
          | Epers_Accomplishment -> str_val "#acco"
          | Epers_Acquisition -> str_val "#acqu"
          | Epers_Adhesion -> str_val "#adhe"
          | Epers_BaptismLDS -> str_val "#bapl"
          | Epers_BarMitzvah -> str_val "#barm"
          | Epers_BatMitzvah -> str_val "#basm"
          | Epers_Benediction -> str_val "#bles"
          | Epers_ChangeName -> str_val "#chgn"
          | Epers_Circumcision -> str_val "#circ"
          | Epers_ConfirmationLDS -> str_val "#conl"
          | Epers_Confirmation -> str_val "#conf"
          | Epers_Decoration -> str_val "#awar"
          | Epers_DemobilisationMilitaire -> str_val "#demm"
          | Epers_Diploma -> str_val "#degr"
          | Epers_Distinction -> str_val "#dist"
          | Epers_DotationLDS -> str_val "#dotl"
          | Epers_Dotation -> str_val "#endl"
          | Epers_Education -> str_val "#educ"
          | Epers_Election -> str_val "#elec"
          | Epers_Emigration -> str_val "#emig"
          | Epers_Excommunication -> str_val "#exco"
          | Epers_FamilyLinkLDS -> str_val "#flkl"
          | Epers_FirstCommunion -> str_val "#fcom"
          | Epers_Funeral -> str_val "#fune"
          | Epers_Graduate -> str_val "#grad"
          | Epers_Hospitalisation -> str_val "#hosp"
          | Epers_Illness -> str_val "#illn"
          | Epers_Immigration -> str_val "#immi"
          | Epers_ListePassenger -> str_val "#lpas"
          | Epers_MilitaryDistinction -> str_val "#mdis"
          | Epers_MilitaryPromotion -> str_val "#mpro"
          | Epers_MilitaryService -> str_val "#mser"
          | Epers_MobilisationMilitaire -> str_val "#mobm"
          | Epers_Naturalisation -> str_val "#natu"
          | Epers_Occupation -> str_val "#occu"
          | Epers_Ordination -> str_val "#ordn"
          | Epers_Property -> str_val "#prop"
          | Epers_Recensement -> str_val "#cens"
          | Epers_Residence -> str_val "#resi"
          | Epers_Retired -> str_val "#reti"
          | Epers_ScellentChildLDS -> str_val "#slgc"
          | Epers_ScellentParentLDS -> str_val "#slgp"
          | Epers_ScellentSpouseLDS -> str_val "#slgs"
          | Epers_VenteBien -> str_val "#vteb"
          | Epers_Will -> str_val "#will"
          | Epers_Name x -> str_val (quote_escaped x)
          end
      | _ -> str_val ""
      end
  | ["e_place"] ->
      begin match e with
        Some {epers_place = x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | ["e_note"] ->
      begin match e with
        Some {epers_note = x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | ["e_src"] ->
      begin match e with
        Some {epers_src = x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | _ -> raise Not_found
and eval_title_var t =
  function
    ["t_estate"] ->
      begin match t with
        Some {t_place = x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | ["t_ident"] ->
      begin match t with
        Some {t_ident = x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | ["t_main"] ->
      begin match t with
        Some {t_name = Tmain} -> bool_val true
      | _ -> bool_val false
      end
  | ["t_name"] ->
      begin match t with
        Some {t_name = Tname x} -> str_val (quote_escaped x)
      | _ -> str_val ""
      end
  | ["t_nth"] ->
      begin match t with
        Some {t_nth = x} -> str_val (if x = 0 then "" else string_of_int x)
      | _ -> str_val ""
      end
  | _ -> raise Not_found
and eval_relation_var r =
  function
    "r_father" :: sl ->
      let x =
        match r with
          Some {r_fath = Some x} -> x
        | _ -> "", "", 0, Update.Create (Neuter, None), ""
      in
      eval_person_var x sl
  | "r_mother" :: sl ->
      let x =
        match r with
          Some {r_moth = Some x} -> x
        | _ -> "", "", 0, Update.Create (Neuter, None), ""
      in
      eval_person_var x sl
  | ["rt_adoption"] -> eval_is_relation_type Adoption r
  | ["rt_candidate_parent"] -> eval_is_relation_type CandidateParent r
  | ["rt_empty"] ->
      begin match r with
        Some {r_fath = None; r_moth = None} | None -> bool_val true
      | _ -> bool_val false
      end
  | ["rt_foster_parent"] -> eval_is_relation_type FosterParent r
  | ["rt_godparent"] -> eval_is_relation_type GodParent r
  | ["rt_recognition"] -> eval_is_relation_type Recognition r
  | _ -> raise Not_found
and eval_person_var (fn, sn, oc, create, _) =
  function
    ["create"] ->
      begin match create with
        Update.Create (_, _) -> bool_val true
      | _ -> bool_val false
      end
  | ["create"; "sex"] ->
      begin match create with
        Update.Create (Male, _) -> str_val "male"
      | Update.Create (Female, _) -> str_val "female"
      | Update.Create (Neuter, _) -> str_val "neuter"
      | _ -> str_val ""
      end
  | ["first_name"] -> str_val (quote_escaped fn)
  | ["link"] -> bool_val (create = Update.Link)
  | ["occ"] -> str_val (if oc = 0 then "" else string_of_int oc)
  | ["surname"] -> str_val (quote_escaped sn)
  | _ -> raise Not_found
and eval_is_cal cal =
  function
    Some (Dgreg (_, x)) -> if x = cal then "1" else ""
  | _ -> ""
and eval_is_prec cond =
  function
    Some (Dgreg ({prec = x}, _)) -> if cond x then "1" else ""
  | _ -> ""
and eval_is_death_reason dr =
  function
    Death (dr1, _) -> bool_val (dr = dr1)
  | _ -> bool_val false
and eval_is_relation_type rt =
  function
    Some {r_fath = None; r_moth = None} -> bool_val false
  | Some {r_type = x} -> bool_val (x = rt)
  | _ -> bool_val false
and eval_special_var conf base =
  function
    ["include_perso_header"] ->
      begin match p_getint conf.env "i" with
        Some i ->
          let has_base_loop =
            try let _ = Util.create_topological_sort conf base in false with
              Consang.TopologicalSortError _ -> true
          in
          if has_base_loop then VVstring ""
          else
            let p = poi base (Adef.iper_of_int i) in
            Perso.interp_templ_with_menu (fun _ -> ()) "perso_header" conf
              base p;
            VVstring ""
      | None -> VVstring ""
      end
  | ["base_header"] -> include_hed_trl conf "hed"; VVstring ""
  | ["base_trailer"] -> include_hed_trl conf "trl"; VVstring ""
  | _ -> raise Not_found
and eval_int_env var env =
  match get_env var env with
    Vint x -> str_val (string_of_int x)
  | _ -> raise Not_found
and eval_string_env var env =
  match get_env var env with
    Vstring x -> str_val (quote_escaped x)
  | _ -> str_val ""

(* print *)

let print_foreach conf base print_ast _eval_expr =
  let rec print_foreach conf base env p _loc s sl _ al =
    match s :: sl with
      ["alias"] -> print_foreach_string env p al p.aliases s
    | ["first_name_alias"] ->
        print_foreach_string env p al p.first_names_aliases s
    | ["img_in_keydir"] ->
        print_foreach_img_in_keydir env p al
          (get_keydir conf base (poi base p.key_index))
    | ["img_in_keydir_old"] ->
        print_foreach_img_in_keydir_old env p al
          (get_keydir_old conf base (poi base p.key_index))
    | ["qualifier"] -> print_foreach_string env p al p.qualifiers s
    | ["surname_alias"] -> print_foreach_string env p al p.surnames_aliases s
    | ["relation"] -> print_foreach_relation env p al p.rparents
    | ["title"] -> print_foreach_title env p al p.titles
    | ["pevent"] -> print_foreach_pevent env p al p.pevents
    | ["witness"] -> print_foreach_witness env p al p.pevents
    | _ -> raise Not_found
  and print_foreach_img_in_keydir env p al list =
    let rec loop cnt =
      function
        a :: l ->
          let env =
            ("keydir_img", Vstring a) ::
            ("keydir_img_notes", Vstring a) ::
            ("keydir_img_src", Vstring a) ::
            ("keydir_img_title", Vstring a) ::
            ("cnt", Vint cnt) :: env
          in
          List.iter (print_ast env p) al; loop (cnt + 1) l
      | [] -> ()
    in
    loop 1 list
  and print_foreach_img_in_keydir_old env p al list =
    let rec loop cnt =
      function
        a :: l ->
          let env =
            ("keydir_img_old", Vstring a) ::
            ("cnt", Vint cnt) :: env
          in
          List.iter (print_ast env p) al; loop (cnt + 1) l
      | [] -> ()
    in
    loop 1 list
  and print_foreach_string env p al list lab =
    let _ =
      List.fold_left
        (fun cnt nn ->
           let env = (lab, Vstring nn) :: env in
           let env = ("cnt", Vint cnt) :: env in
           List.iter (print_ast env p) al; cnt + 1)
        0 list
    in
    ()
  and print_foreach_relation env p al list =
    let _ =
      List.fold_left
        (fun cnt _ ->
           let env = ("cnt", Vint cnt) :: env in
           List.iter (print_ast env p) al; cnt + 1)
        1 list
    in
    ()
  and print_foreach_title env p al list =
    let _ =
      List.fold_left
        (fun cnt _ ->
           let env = ("cnt", Vint cnt) :: env in
           List.iter (print_ast env p) al; cnt + 1)
        1 list
    in
    ()
  and print_foreach_pevent env p al list =
    let rec loop first cnt =
      function
        _ :: l ->
          let env =
            ("cnt", Vint cnt) :: ("first", Vbool first) ::
            ("last", Vbool (l = [])) :: env
          in
          List.iter (print_ast env p) al; loop false (cnt + 1) l
      | [] -> ()
    in
    loop true 1 list
  and print_foreach_witness env p al list =
    match get_env "cnt" env with
      Vint i ->
        begin match
          (try Some (List.nth list (i - 1)) with Failure _ -> None)
        with
          Some e ->
            let rec loop first wcnt =
              function
                _ :: l ->
                  let env =
                    ("wcnt", Vint wcnt) :: ("first", Vbool first) ::
                    ("last", Vbool (l = [])) :: env
                  in
                  List.iter (print_ast env p) al; loop false (wcnt + 1) l
              | [] -> ()
            in
            loop true 1 (Array.to_list e.epers_witnesses)
        | None -> ()
        end
    | _ -> ()
  in
  print_foreach conf base

(* ****************************************************** *)

(*
let pp_env conf mess =
  let _ = Printf.eprintf "\n%s\n" mess in
  let _ =  List.iter
    (fun (k, v) ->
       if k <> "file" then Printf.eprintf "Env_var: %s, %s\n" k v)
    conf.env
  in
  let _ = Printf.eprintf "-----------\n" in
  flush stderr
*)

type image_type = JPEG | GIF | PNG
let image_types = [JPEG; GIF; PNG]

let extension_of_type =
  function
  | JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let incorrect conf = Hutil.incorrect_request conf; raise Update.ModErr

let incorrect_content_type conf base p s =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p>\n";
  Wserver.printf "<em style=\"font-size:smaller\">";
  Wserver.printf "Error: incorrect image content type: %s" s;
  Wserver.printf "</em>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s" (referenced_person_title_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf;
  raise Update.ModErr

let error_too_big_image conf base p len max_len =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p><em style=\"font-size:smaller\">";
  Wserver.printf "Error: this image is too big: %d bytes<br%s>\n" len
    conf.xhs;
  Wserver.printf "Maximum authorized in this database: %d bytes<br%s>\n"
    max_len conf.xhs;
  Wserver.printf "</em></p>\n";
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s" (referenced_person_title_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf;
  raise Update.ModErr

let raw_get conf key =
  try List.assoc key conf.env with Not_found ->
    incorrect conf

(* old code
  let title _ =
    Wserver.printf "%s" (capitale (transl conf message))
  in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  html_li conf;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n<li>";
  Wserver.printf "\n<a href=\"%sm=IMAGE&i=%d\">" (commd conf)
          (Adef.int_of_iper (get_key_index p));
  Wserver.printf "%s/%s %s" (capitale (transl conf "add"))
    (transl conf "delete")(transl_nth conf "image/images" 0);
  Wserver.printf "</a></li><br>";
  Wserver.printf "\n</ul>\n";
  Hutil.trailer conf
*)

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content; flush oc; close_out oc

let clean_old_portrait conf bfname =
  let file_name = Filename.remove_extension
    (String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "portraits"; "saved"; bfname])
  in
  (try Sys.remove (file_name ^ ".jpg") with Sys_error _ -> ());
  (try Sys.remove (file_name ^ ".png") with Sys_error _ -> ());
  (try Sys.remove (file_name ^ ".gif") with Sys_error _ -> ())

let delete_old_file conf folder bfname =
  let file_name =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; folder; "saved"; bfname]
  in
  let file_name_t = (Filename.remove_extension file_name) ^ ".txt" in
  (try Sys.remove file_name with Sys_error _ -> ());
  (try Sys.remove file_name_t with Sys_error _ -> ());
  1

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then
    Some JPEG
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then Some PNG
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then Some GIF
  else None

let string_search s v =
  let rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

(* get the image type, possibly removing spurious header *)

let image_type s =
  match normal_image_type s with
    Some t -> Some (t, s)
  | None ->
      match string_search s "JFIF" with
        Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ ->
          match string_search s "\137PNG" with
            Some i ->
              let s = String.sub s i (String.length s - i) in
              Some (PNG, s)
          | _ ->
              match string_search s "GIF8" with
                Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None

let dump_bad_image conf s =
  match p_getenv conf.base_env "dump_bad_images" with
    Some "yes" ->
      begin try
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s; flush oc; close_out oc
      with Sys_error _ -> ()
      end
  | _ -> ()

let space_to_unders = Mutil.tr ' ' '_'

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_extension conf keydir =
  let f =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "portraits"; keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let get_extension_old conf keydir =
  let f =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "portraits"; "saved"; keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

(*Util.has_image conf base (poi base p.key_index)*)
(* bfname is basename or keydir/basename *)
let move_file_to_old conf fname bfname mode keydir =
  let old_dir_p =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "portraits"; "saved"]
  in
  let old_dir_i0 =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "images"; keydir;]
  in
  let old_dir_i =
    String.concat
      Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "images"; keydir; "saved"]
  in
  (try Unix.mkdir old_dir_p 0o777 with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir old_dir_i0 0o777 with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir old_dir_i 0o777 with Unix.Unix_error (_, _, _) -> ());
  let saved_file =
    if mode = "portraits" then Filename.concat old_dir_p bfname
    else Filename.concat old_dir_i bfname
  in
  (try Sys.rename fname saved_file with Sys_error _ -> ());
  let fname_t = (Filename.remove_extension fname) ^ ".txt" in
  let saved_file_t = (Filename.remove_extension saved_file) ^ ".txt" in
  (try Sys.rename fname_t saved_file_t with Sys_error _ -> ());
  if Sys.file_exists saved_file then 1 else 0

let print_confirm conf base save_m report =
  match p_getint conf.env "i" with
  | Some ip ->
      let p = poi base (Adef.iper_of_int ip) in
      let sp = string_person_of base p in
      let digest = (default_image_name base p) in
      let new_env =
        List.fold_left
          (fun accu (k, v) ->
             if k = "m" then ("m", "IMAGE") :: accu
             else if k = "digest" || k = "" then accu
             else (k, v) :: accu)
          [] conf.env
      in
      let new_env =
        if save_m = "IMAGE" then new_env
        else ("em", save_m) :: new_env
      in
      let new_env = ("digest", digest) :: new_env in
      let new_env = ("report", report) :: new_env in
      let conf = { (conf) with env = new_env } in
      Hutil.interp conf "images"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = (fun _ -> raise Not_found);
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach conf base}
        [] sp
    | None -> Hutil.incorrect_request conf

let print_image conf base = print_confirm conf base ""

(* ************************************************************************ *)
(*  send, delete and reset functions                                        *)
(*                                                                          *)
(* ************************************************************************ *)


(* depending on mode, write file/file_name into portraits or others area *)

let effective_send_ok conf base p file file_name mode =
  let notes =
    Util.sanitize_html
      (only_printable_or_nl
        (Mutil.strip_all_trailing_spaces (get conf "notes")))
  in
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    if mode = "comment" then ""
    else
      let s =
        let rec loop len (strm__ : _ Stream.t) =
          match Stream.peek strm__ with
          | Some x -> Stream.junk strm__; loop (Buff.store len x) strm
          | _ -> Buff.get len
        in
        loop 0 strm
      in
      content ^ s
  in
  let (typ, content) =
    if content <> "" then
      match image_type content with
      | None ->
          let ct = Wserver.extract_param "content-type: " '\n' request in
          dump_bad_image conf content; incorrect_content_type conf base p ct
      | Some (typ, content) ->
          match p_getint conf.base_env "max_images_size" with
          | Some len when String.length content > len ->
              error_too_big_image conf base p (String.length content) len
          | _ -> typ, content
    else GIF, content (* we dont care which type, content = "" *)
  in
  let keydir = default_image_name base p in
  let full_dir =
    if mode = "portraits" then
      String.concat Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "portraits";]
    else
      String.concat Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "images"; keydir]
  in
  let _ =
    if not (Sys.file_exists full_dir) then
      let d = Filename.concat (Util.base_path conf.bname) "documents" in
      let d1 = Filename.concat d "portraits" in
      let d2 = Filename.concat d "images" in
      let d3 = Filename.concat d2 keydir in
      (try Unix.mkdir d 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d1 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d2 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d3 0o777 with Unix.Unix_error (_, _, _) -> ());
  in
  let full_name = Filename.concat full_dir
     (if mode = "portraits" then
      keydir ^ (extension_of_type typ)
     else file_name)
  in
  if mode = "portraits" then
    begin match Util.auto_image_file conf base p "" with
    | Some f ->
        clean_old_portrait conf (Filename.basename f);
        if (move_file_to_old conf f (Filename.basename f) mode keydir) = 0 then
          incorrect conf;
    | None -> ()
    end
  else
    begin
    if (Sys.file_exists full_name) && content <> "" then
      let bfname = Filename.concat keydir (Filename.basename full_name) in
      if (move_file_to_old conf full_name bfname mode keydir) = 0 then
        incorrect conf
    end;
  if content <> "" then write_file full_name content;
  write_file ((Filename.remove_extension full_name) ^ ".txt") notes;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "si" else
      if file_name <> "" && notes <> "" then "sb"
      else if file_name <> "" then "so"
      else if notes <> "" then "sc" else "sn");
  file_name

(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_ok conf base p =
  let keydir = default_image_name base p in
  let file_name = try List.assoc "file_name" conf.env with Not_found -> "" in
  let file_name = Wserver.decode file_name in
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let delete =
    try (List.assoc "delete" conf.env = "on") with Not_found -> false
  in
  let file_name =
    if file_name = "" then
      keydir ^
        (if delete then get_extension_old conf keydir
         else get_extension conf keydir)
    else file_name
  in
  let bfname = file_name in
  let folder =
    if mode = "portraits" then "portraits"
    else (Filename.concat "images" keydir)
  in
  let saved = if delete then "saved" else "" in
  let full_name =
    String.concat Filename.dir_sep
      [Util.base_path conf.bname; "documents"; folder; saved; bfname]
  in
  let res =
    if (Sys.file_exists full_name) && delete then
      delete_old_file conf folder bfname
    else if (Sys.file_exists full_name) then
    (* TODO verify we dont destroy a saved image having the same name as portrait! *)
      begin
        begin match Util.auto_image_file conf base p "saved" with
        | Some _ -> clean_old_portrait conf bfname
        | None -> ()
        end;
        move_file_to_old conf full_name bfname mode keydir;
      end
    else 1
  in
  if res = 0 then incorrect conf else ();
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "di" else "do");
  file_name

(* reset portrait or image from old folder to portrait or others *)

let swap_files conf file1 file2 txt =
  let tmp_file =
    String.concat Filename.dir_sep
      [Util.base_path conf.bname; "documents"; "tempfile.tmp"]
  in
  let ext_1 = Filename.extension file1 in
  let ext_2 = Filename.extension file2 in
  (try Sys.rename file1 tmp_file with Sys_error _ -> ());
  (try Sys.rename file2
    ((Filename.remove_extension file1) ^ ext_2) with Sys_error _ -> ());
  (try Sys.rename tmp_file
    ((Filename.remove_extension file2) ^ ext_1) with Sys_error _ -> ());
  if txt then
    let tmp_file_t =
      String.concat Filename.dir_sep
        [Util.base_path conf.bname; "documents"; "tempfile.tmp"]
    in
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    (try Sys.rename file1_t tmp_file_t with Sys_error _ -> ());
    (try Sys.rename file2_t file1_t with Sys_error _ -> ());
    (try Sys.rename tmp_file_t file2_t with Sys_error _ -> ())

let rename_files file1 file2 txt =
  (try Sys.rename file1 file2 with Sys_error _ -> ());
  if txt then
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    (try Sys.rename file1_t file2_t with Sys_error _ -> ())

let effective_reset_ok conf base p =
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let keydir = default_image_name base p in
  if mode = "portraits" then
    begin
      let file_name = keydir in
      let ext_o = get_extension_old conf keydir in
      let ext = get_extension conf keydir in
      let ext = if ext = "." then ext_o else ext in
      let file_in_old =
        String.concat Filename.dir_sep
          [Util.base_path conf.bname; "documents"; "portraits"; "saved";
            (file_name ^ ext_o)]
      in
      let file_in_portraits =
        String.concat Filename.dir_sep
          [Util.base_path conf.bname; "documents"; "portraits";
            (file_name ^ ext)]
      in
      if Sys.file_exists file_in_portraits then
        swap_files conf file_in_old file_in_portraits false
      else
        rename_files file_in_old file_in_portraits false
    end
  else
    begin
      let file_name =
        try List.assoc "file_name" conf.env with Not_found -> ""
      in
      let file_name = Wserver.decode file_name in
      let file_in_old =
        String.concat Filename.dir_sep
          [Util.base_path conf.bname; "documents"; "images";
            keydir; "saved"; file_name]
      in
      let new_file =
        String.concat Filename.dir_sep
          [Util.base_path conf.bname; "documents"; "images"; keydir; file_name]
      in
      if Sys.file_exists new_file then
        swap_files conf file_in_old new_file true
      else
        rename_files file_in_old new_file true
    end;
  let file_name =
    try List.assoc "file_name" conf.env with Not_found -> ""
  in
  file_name

let print conf base =
  (* if em="" this is the first pass, do it *)
  begin match p_getenv conf.env "em" with
  | None ->
    begin match p_getenv conf.env "m" with
    | Some m ->
      let save_m = m in
      begin match p_getint conf.env "i" with
      | Some ip ->
          let p = poi base (Adef.iper_of_int ip) in
          let digest = (default_image_name base p) in
          let (conf, report) =
            begin match m with
            | "SND_IMAGE_OK" ->
                let mode =
                  try List.assoc "mode" conf.env with Not_found -> "portraits"
                in
                let file_name =
                    (try List.assoc "file_name" conf.env with Not_found -> "")
                in
                let file_name =
                  if file_name = "" then
                    (try List.assoc "file_name_2" conf.env with Not_found -> "")
                  else file_name
                in
                let file_name_2 = Filename.remove_extension file_name in
                let new_env =
                  List.fold_left
                    (fun accu (k, v) ->
                       if k = "file_name_2" then (k, file_name_2) :: accu
                       else (k, v) :: accu)
                    [] conf.env
                in
                let conf = { (conf) with env = new_env } in
                let file_name = Wserver.decode file_name in
                let file =
                  if mode <> "comment" then raw_get conf "file"
                  else "file_name"
                in
                if digest = raw_get conf "digest" then
                  conf, effective_send_ok conf base p file file_name mode
                else conf, "digest error";
            | "DEL_IMAGE_OK" ->
                if digest = raw_get conf "digest" then
                  conf, effective_delete_ok conf base p
                else conf, "digest error"
            | "RESET_IMAGE_OK" ->
                let digest = (default_image_name base p) in
                if digest = raw_get conf "digest" then
                  conf, effective_reset_ok conf base p
                else conf, "digest error"
            | "IMAGE" ->
                conf, "image"
            | _ -> conf, "incorrect request"
            end;
          in
          begin match report with
          | "digest error" -> Update.error_digest conf
          | "incorrect request" -> Hutil.incorrect_request conf
          | _ -> print_confirm conf base save_m report
          end
      | None -> Hutil.incorrect_request conf
      end
    | None -> Hutil.incorrect_request conf
    end
  (* em!="" second pass, ignore *)
  | Some _ -> print_confirm conf base "IMAGE" ""
  end

(* TODO need to lock base (keydir!!),
see request.ml list appels which need lock *)
