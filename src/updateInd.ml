(* camlp4r ./pa_html.cmo *)
(* $Id: updateInd.ml,v 4.3 2001-06-13 08:01:45 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value bogus_person_index = Adef.iper_of_int (-1);

value string_person_of base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper p.cle_index
    else p.occ
  in
  let fp ip =
    let p = poi base ip in
    (sou base p.first_name, sou base p.surname, p.occ, Update.Link, "")
  in
  Gutil.map_person_ps fp (sou base) p
;

(* Interpretation of template file 'updind.txt' *)

type ast =
  Templ.ast ==
    [ Atext of string
    | Avar of string and list string
    | Atransl of bool and string and char
    | Awid_hei of string
    | Aif of ast_expr and list ast and list ast
    | Aforeach of string and list string and list ast
    | Adefine of string and list string and list ast and list ast
    | Aapply of string and list ast_expr ]
and ast_expr =
  Templ.ast_expr ==
    [ Eor of ast_expr and ast_expr
    | Eand of ast_expr and ast_expr
    | Eop of string and ast_expr and ast_expr
    | Enot of ast_expr
    | Estr of string
    | Eint of string
    | Evar of string and list string
    | Etransl of bool and string and char ]
;

type env =
  [ Vstring of string
  | Vfun of list string and list ast
  | Vint of int
  | Vnone ]
;

type variable_value =
  [ VVgen of string
  | VVdate of option date and string
  | VVrelation of option (gen_relation Update.key string) and list string
  | VVtitle of option (gen_title string) and list string
  | VVcvar of string
  | VVnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value rec eval_variable conf base env p =
  fun
  [ ["bapt"; s] -> VVdate (Adef.od_of_codate p.baptism) s
  | ["birth"; s] -> VVdate (Adef.od_of_codate p.birth) s
  | ["burial"; s] ->
      let d =
        match p.burial with
        [ Buried cod -> Adef.od_of_codate cod
        | Cremated cod -> Adef.od_of_codate cod
        | _ -> None ]
      in
      VVdate d s
  | ["death"; s] ->
      let d =
        match p.death with
        [ Death _ cd -> Some (Adef.date_of_cdate cd)
        | _ -> None ]
      in
      VVdate d s
  | ["relation" :: sl] ->
      let r =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.rparents (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      VVrelation r sl
  | ["title" :: sl] ->
      let t =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.titles (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      VVtitle t sl
  | ["title_date_start"; s] ->
      let d =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_start
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      VVdate d s
  | ["title_date_end"; s] ->
      let d =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_end
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      VVdate d s
  | [] -> VVgen ""
  | [s] ->
      let v = extract_var "cvar_" s in if v <> "" then VVcvar v else VVgen s
  | [s :: sl] -> VVnone ]
;

(* string values *)

value eval_base_env_variable conf v =
  try List.assoc v conf.base_env with [ Not_found -> "" ]
;

value eval_string_env var env =
  match get_env var env with
  [ Vstring x -> quote_escaped x
  | _ -> "" ]
;

value eval_int_env var env =
  match get_env var env with
  [ Vint x -> string_of_int x
  | _ -> "" ]
;

value try_eval_gen_variable conf base env p =
  fun
  [ "alias" -> eval_string_env "alias" env
  | "bapt_place" -> quote_escaped p.baptism_place
  | "bapt_src" -> quote_escaped p.baptism_src
  | "birth_place" -> quote_escaped p.birth_place
  | "birth_src" -> quote_escaped p.birth_src
  | "burial_place" -> quote_escaped p.burial_place
  | "burial_src" -> quote_escaped p.burial_src
  | "death_place" -> quote_escaped p.death_place
  | "death_src" -> quote_escaped p.death_src
  | "cnt" -> eval_int_env "cnt" env
  | "first_name_alias" -> eval_string_env "first_name_alias" env
  | "digest" -> eval_string_env "digest" env
  | "first_name" -> quote_escaped p.first_name
  | "image" -> quote_escaped p.image
  | "index" -> string_of_int (Adef.int_of_iper p.cle_index)
  | "notes" -> quote_escaped p.notes
  | "occ" -> if p.occ <> 0 then string_of_int p.occ else ""
  | "occupation" -> quote_escaped p.occupation
  | "public_name" -> quote_escaped p.public_name
  | "qualifier" -> eval_string_env "qualifier" env
  | "sources" -> quote_escaped p.psources
  | "surname" -> quote_escaped p.surname
  | "surname_alias" -> eval_string_env "surname_alias" env
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value eval_date_field =
  fun
  [ Some d ->
      match d with
      [ Dgreg d Dgregorian -> Some d
      | Dgreg d Djulian -> Some (Calendar.julian_of_gregorian d)
      | Dgreg d Dfrench -> Some (Calendar.french_of_gregorian d)
      | Dgreg d Dhebrew -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None ]
  | None -> None ]
;

value eval_date_text =
  fun
  [ Some (Dtext s) -> s
  | _ -> "" ]
;

value eval_date_variable conf base env od =
  fun
  [ "day" ->
      match eval_date_field od with
      [ Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "" ]
  | "month" ->
      match eval_date_field od with
      [ Some d -> if d.month = 0 then "" else string_of_int d.month
      | None -> "" ]
  | "text" -> eval_date_text od
  | "year" ->
      match eval_date_field od with
      [ Some d -> string_of_int d.year
      | None -> "" ]
  | "oryear" ->
      match od with
      [ Some (Dgreg {prec = OrYear y} _) -> string_of_int y
      | Some (Dgreg {prec = YearInt y} _) -> string_of_int y
      | _ -> "" ]
  | v -> ">%" ^ v ^ "???" ]
;

value eval_key_variable conf base env (fn, sn, oc, create, var) =
  fun
  [ "first_name" -> quote_escaped fn
  | "occ" -> if oc = 0 then "" else string_of_int oc
  | "surname" -> quote_escaped sn
  | s -> ">%" ^ s ^ "???" ]
;

value eval_relation_variable conf base env p r =
  fun
  [ ["r_father"; s] ->
      match r with
      [ Some {r_fath = Some x} -> eval_key_variable conf base env x s
      | _ -> "" ]
  | ["r_mother"; s] ->
      match r with
      [ Some {r_moth = Some x} -> eval_key_variable conf base env x s
      | _ -> "" ]
  | [s :: _] -> ">%" ^ s ^ "???"
  | _ -> ">???" ]
;

value eval_title_variable conf base env p t =
  fun
  [ ["t_ident"] ->
      match t with
      [ Some {t_ident = x} -> quote_escaped x
      | _ -> "" ]
  | ["t_estate"] ->
      match t with
      [ Some {t_place = x} -> quote_escaped x
      | _ -> "" ]
  | ["t_name"] ->
      match t with
      [ Some {t_name = Tname x} -> quote_escaped x
      | _ -> "" ]
  | ["t_nth"] ->
      match t with
      [ Some {t_nth = x} -> if x = 0 then "" else string_of_int x
      | _ -> "" ]
  | [s :: _] -> ">%" ^ s ^ "???"
  | _ -> ">???" ]
;

value eval_expr conf base env p =
  fun
  [ Estr s -> s
  | Evar s [] ->
      try try_eval_gen_variable conf base env p s with
      [ Not_found -> ">" ^ s ^ "???" ]
  | Etransl upp s c -> Templ.eval_transl conf base env upp s c
  | _ -> ">parse_error" ]
;

(* bool values *)

value is_death_reason dr =
  fun
  [ Death dr1 _ -> dr = dr1
  | _ -> False ]
;

value eval_gen_bool_variable conf base env p =
  fun
  [ "acc_if_titles" -> p.access = IfTitles
  | "acc_private" -> p.access = Private
  | "acc_public" -> p.access = Public
  | "dead_dont_know_when" -> p.death = DeadDontKnowWhen
  | "died_young" -> p.death = DeadYoung
  | "dont_know_if_dead" -> p.death = DontKnowIfDead
  | "bt_buried" -> match p.burial with [ Buried _ -> True | _ -> False ]
  | "bt_cremated" -> match p.burial with [ Cremated _ -> True | _ -> False ]
  | "bt_unknown_burial" -> p.burial = UnknownBurial
  | "dr_killed" -> is_death_reason Killed p.death
  | "dr_murdered" -> is_death_reason Murdered p.death
  | "dr_executed" -> is_death_reason Executed p.death
  | "dr_disappeared" -> is_death_reason Disappeared p.death
  | "dr_unspecified" -> is_death_reason Unspecified p.death
  | "has_aliases" -> p.aliases <> []
  | "has_birth_date" -> Adef.od_of_codate p.birth <> None
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_qualifiers" -> p.qualifiers <> []
  | "has_relations" -> p.rparents <> []
  | "has_surnames_aliases" -> p.surnames_aliases <> []
  | "has_titles" -> p.titles <> []
  | "is_female" -> p.sex = Female
  | "is_male" -> p.sex = Male
  | "not_dead" -> p.death = NotDead
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv conf.env v with
        [ Some "" | None -> False
        | _ -> True ]
      else do { Wserver.wprint ">%%%s???" s; False } ]
;

value is_calendar cal =
  fun
  [ Some (Dgreg _ x) -> x = cal
  | _ -> False ]
;

value is_precision cond =
  fun
  [ Some (Dgreg {prec = x} _) -> cond x
  | _ -> False ]
;

value eval_date_bool_variable conf base env od =
  fun
  [ "cal_gregorian" -> is_calendar Dgregorian od
  | "cal_julian" -> is_calendar Djulian od
  | "cal_french" -> is_calendar Dfrench od
  | "cal_hebrew" -> is_calendar Dhebrew od
  | "prec_no" -> od = None
  | "prec_sure" -> is_precision (fun [ Sure -> True | _ -> False ]) od
  | "prec_about" -> is_precision (fun [ About -> True | _ -> False ]) od
  | "prec_maybe" -> is_precision (fun [ Maybe -> True | _ -> False ]) od
  | "prec_before" -> is_precision (fun [ Before -> True | _ -> False ]) od
  | "prec_after" -> is_precision (fun [ After -> True | _ -> False ]) od
  | "prec_oryear" -> is_precision (fun [ OrYear _ -> True | _ -> False ]) od
  | "prec_yearint" -> is_precision (fun [ YearInt _ -> True | _ -> False ]) od
  | s -> do { Wserver.wprint ">%%%s???" s; False } ]
;

value is_relation_type rt =
  fun
  [ Some {r_type = x} -> x = rt
  | _ -> False ]
;

value
  eval_relation_person_bool_variable conf base env (fn, sn, oc, create, var) =
  fun
  [ ["create"] ->
      match create with
      [ Update.Create _ _ -> True
      | _ -> False ]
  | ["link"] -> create = Update.Link
  | [s] -> do { Wserver.wprint ">%%%s???" s; False }
  | _ -> do { Wserver.wprint ">???"; False } ]
;

value eval_relation_bool_variable conf base env r =
  fun
  [ ["r_father" :: sl] ->
      match r with
      [ Some {r_fath = Some x} ->
          eval_relation_person_bool_variable conf base env x sl
      | _ -> False ]
  | ["r_mother" :: sl] ->
      match r with
      [ Some {r_moth = Some x} ->
          eval_relation_person_bool_variable conf base env x sl
      | _ -> False ]
  | ["rt_adoption"] -> is_relation_type Adoption r
  | ["rt_candidate_parent"] -> is_relation_type CandidateParent r
  | ["rt_empty"] ->
      match r with
      [ Some {r_fath = None; r_moth = None} | None -> True
      | _ -> False ]
  | ["rt_foster_parent"] -> is_relation_type FosterParent r
  | ["rt_godparent"] -> is_relation_type GodParent r
  | ["rt_regognition"] -> is_relation_type Recognition r
  | [s] -> do { Wserver.wprint ">%%%s???" s; False }
  | _ -> do { Wserver.wprint ">???"; False } ]
;

value eval_title_bool_variable conf base env t =
  fun
  [ ["t_main"] ->
      match t with
      [ Some {t_name = Tmain} -> True
      | _ -> False ]
  | _ -> do { Wserver.wprint ">???"; False } ]
;

value eval_bool_variable conf base env p s sl =
  match eval_variable conf base env p [s :: sl] with
  [ VVgen s -> eval_gen_bool_variable conf base env p s
  | VVdate od s -> eval_date_bool_variable conf base env od s
  | VVrelation r sl -> eval_relation_bool_variable conf base env r sl
  | VVtitle t sl -> eval_title_bool_variable conf base env t sl
  | VVcvar _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVnone -> do { Wserver.wprint ">%%%s???" s; False } ]
;

value eval_bool_value conf base env p =
  let rec bool_eval =
    fun
    [ Eor e1 e2 -> bool_eval e1 || bool_eval e2
    | Eand e1 e2 -> bool_eval e1 && bool_eval e2
    | Eop op e1 e2 ->
        match op with
        [ "=" -> string_eval e1 = string_eval e2
        | "!=" -> string_eval e1 <> string_eval e2
        | _ -> do { Wserver.wprint "op %s???" op; False } ]
    | Enot e -> not (bool_eval e)
    | Evar s sl -> eval_bool_variable conf base env p s sl
    | Estr s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Eint s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Etransl _ s _ -> do { Wserver.wprint "[%s]???" s; False } ]
  and string_eval =
    fun
    [ Estr s -> s
    | Evar s sl ->
        try
          match eval_variable conf base env p [s :: sl] with
          [ VVgen s -> try_eval_gen_variable conf base env p s
          | VVdate od s -> eval_date_variable conf base env od s
          | VVcvar s -> eval_base_env_variable conf s
          | VVrelation _ _ -> do { Wserver.wprint ">%%%s???" s; "" }
          | VVtitle _ _ -> do { Wserver.wprint ">%%%s???" s; "" }
          | VVnone -> do { Wserver.wprint ">%%%s???" s; "" } ]
        with
        [ Not_found -> do { Wserver.wprint ">%%%s???" s; "" } ]
    | x -> do { Wserver.wprint "val???"; "" } ]
  in
  bool_eval
;

(* print *)

value print_variable conf base env p sl =
  match eval_variable conf base env p sl with
  [ VVgen s ->
      try Wserver.wprint "%s" (try_eval_gen_variable conf base env p s) with
      [ Not_found -> Templ.print_variable conf base s ]
  | VVdate od s -> Wserver.wprint "%s" (eval_date_variable conf base env od s)
  | VVcvar s ->
      try Wserver.wprint "%s" (List.assoc s conf.base_env) with
      [ Not_found -> () ]
  | VVrelation r sl ->
      Wserver.wprint "%s" (eval_relation_variable conf base env p r sl)
  | VVtitle t sl ->
      Wserver.wprint "%s" (eval_title_variable conf base env p t sl)
  | VVnone ->
      do {
        Wserver.wprint ">%%";
        list_iter_first
          (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
          sl;
        Wserver.wprint "???";
      } ]
;

value subst_text x v s =
  if String.length x = 0 then s
  else
    let rec loop len i i_ok =
      if i = String.length s then
        if i_ok > 0 then loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
        else Buff.get len
      else if s.[i] = x.[i_ok] then
        if i_ok = String.length x - 1 then loop (Buff.mstore len v) (i + 1) 0
        else loop len (i + 1) (i_ok + 1)
      else if i_ok > 0 then
        loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
      else loop (Buff.store len s.[i]) (i + 1) 0
    in
    loop 0 0 0
;

value rec subst sf =
  fun
  [ Atext s -> Atext (sf s)
  | Avar s sl -> Avar (sf s) (List.map sf sl)
  | Atransl b s c -> Atransl b (sf s) c
  | Awid_hei s -> Awid_hei (sf s)
  | Aif e alt ale -> Aif (subste sf e) (substl sf alt) (substl sf ale)
  | Aforeach s sl al -> Aforeach (sf s) (List.map sf sl) (substl sf al)
  | Adefine f xl al alk ->
      Adefine (sf f) (List.map sf xl) (substl sf al) (substl sf alk)
  | Aapply f el -> Aapply (sf f) (substel sf el) ]
and substl sf al = List.map (subst sf) al
and subste sf =
  fun
  [ Eor e1 e2 -> Eor (subste sf e1) (subste sf e2)
  | Eand e1 e2 -> Eand (subste sf e1) (subste sf e2)
  | Eop op e1 e2 -> Eop (sf op) (subste sf e1) (subste sf e2)
  | Enot e -> Enot (subste sf e)
  | Estr s -> Estr (sf s)
  | Eint s -> Eint s
  | Evar s sl -> Evar (sf s) (List.map sf sl)
  | Etransl upp s c -> Etransl upp s c ]
and substel sf el = List.map (subste sf) el;

value rec print_ast conf base env p =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n ->
      Wserver.wprint "%s" (Templ.eval_transl conf base env upp s n)
  | Avar s sl -> print_variable conf base env p [s :: sl]
  | Awid_hei s -> Wserver.wprint "Awid_hei"
  | Aif e alt ale -> print_if conf base env p e alt ale
  | Aforeach s sl al -> print_foreach conf base env p s sl al
  | Adefine f xl al alk -> print_define conf base env p f xl al alk
  | Aapply f el -> print_apply conf base env p f el ]
and print_define conf base env p f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] p) alk
and print_apply conf base env p f el =
  match get_env f env with
  [ Vfun xl al ->
      let vl = List.map (eval_expr conf base env p) el in
      List.iter
        (fun a ->
           let a =
             loop a xl vl where rec loop a xl vl =
               match (xl, vl) with
               [ ([x :: xl], [v :: vl]) ->
                   loop (subst (subst_text x v) a) xl vl
               | ([], []) -> a
               | _ -> Atext "parse_error" ]
           in
           print_ast conf base env p a)
        al
  | _ -> Wserver.wprint ">%%%s???" f ]
and print_if conf base env p e alt ale =
  let al = if eval_bool_value conf base env p e then alt else ale in
  List.iter (print_ast conf base env p) al
and print_foreach conf base env p s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env p sl with
  [ VVgen "" -> print_simple_foreach conf base env p al s
  | VVgen _ ->
      do {
        Wserver.wprint "foreach ";
        List.iter (fun s -> Wserver.wprint "%s." s) sl;
        Wserver.wprint "%s???" s;
      }
  | VVcvar _ | VVdate _ _ | VVrelation _ _ | VVtitle _ _ | VVnone -> () ]
and print_simple_foreach conf base env p al s =
  match s with
  [ "alias" -> print_foreach_string conf base env p al p.aliases s
  | "first_name_alias" ->
      print_foreach_string conf base env p al p.first_names_aliases s
  | "qualifier" -> print_foreach_string conf base env p al p.qualifiers s
  | "surname_alias" ->
      print_foreach_string conf base env p al p.surnames_aliases s
  | "relation" -> print_foreach_relation conf base env p al p.rparents
  | "title" -> print_foreach_title conf base env p al p.titles
  | _ -> Wserver.wprint "foreach %s???" s ]
and print_foreach_string conf base env p al list lab =
  let _ =
    List.fold_left
      (fun cnt nn ->
         let env = [(lab, Vstring nn) :: env] in
         let env = [("cnt", Vint cnt) :: env] in
         do { List.iter (print_ast conf base env p) al; cnt + 1 })
      0 list
  in
  ()
and print_foreach_relation conf base env p al list =
  let _ =
    List.fold_left
      (fun cnt nn ->
         let env = [("cnt", Vint cnt) :: env] in
         do { List.iter (print_ast conf base env p) al; cnt + 1 })
      1 list
  in
  ()
and print_foreach_title conf base env p al list =
  let _ =
    List.fold_left
      (fun cnt nn ->
         let env = [("cnt", Vint cnt) :: env] in
         do { List.iter (print_ast conf base env p) al; cnt + 1 })
      1 list
  in
  ()
;

value interp_templ conf base p digest astl =
  let env = [("digest", Vstring digest)] in
  List.iter (print_ast conf base env p) astl
;

value print_update_ind conf base p digest =
  match p_getenv conf.env "m" with
  [ Some ("MRG_IND_OK" | "MRG_MOD_IND_OK") | Some ("MOD_IND" | "MOD_IND_OK") |
    Some ("ADD_IND" | "ADD_IND_OK") ->
      let astl = Templ.input conf base "updind" in
      do { html conf; interp_templ conf base p digest astl }
  | _ -> incorrect_request conf ]
;

value print_del1 conf base p =
  let title _ =
    let s = transl_nth conf "person/persons" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=DEL_IND_OK>\n";
      Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
        (Adef.int_of_iper p.cle_index);
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print_add conf base =
  let p =
    {first_name = ""; surname = ""; occ = 0; image = "";
     first_names_aliases = []; surnames_aliases = []; public_name = "";
     qualifiers = []; aliases = []; titles = []; rparents = []; related = [];
     occupation = ""; sex = Neuter; access = IfTitles;
     birth = Adef.codate_None; birth_place = ""; birth_src = "";
     baptism = Adef.codate_None; baptism_place = ""; baptism_src = "";
     death = DontKnowIfDead; death_place = ""; death_src = "";
     burial = UnknownBurial; burial_place = ""; burial_src = ""; notes = "";
     psources = ""; cle_index = bogus_person_index}
  in
  print_update_ind conf base p ""
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      let digest = Update.digest_person p in
      print_update_ind conf base (string_person_of base p) digest
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      print_del1 conf base (string_person_of base p)
  | _ -> incorrect_request conf ]
;
