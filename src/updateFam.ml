(* camlp5r ./pa_html.cmo *)
(* $Id: updateFam.ml,v 5.24 2008-01-09 03:34:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open TemplAst;
open Util;

type create_info =
  Update.create_info ==
    { ci_birth_date : option date;
      ci_birth_place : string;
      ci_death : death;
      ci_death_date : option date;
      ci_death_place : string;
      ci_occupation : string;
      ci_public : bool }
;

value bogus_family_index = Adef.ifam_of_int (-1);

value default_source conf =
  match p_getenv conf.env "dsrc" with
  [ Some s -> s
  | None -> "" ]
;

value person_key base ip =
  let p = poi base ip in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip
    else get_occ p
  in
  (first_name, surname, occ, Update.Link, "")
;

value string_family_of conf base ifam =
  let fam = foi base ifam in
  let sfam =
    Futil.map_family_ps (person_key base) (sou base)
      (gen_family_of_family fam)
  in
  let scpl =
    Futil.map_couple_p conf.multi_parents (person_key base)
      (gen_couple_of_couple fam)
  in
  let sdes =
    Futil.map_descend_p (person_key base)
      (gen_descend_of_descend fam)
  in
  (sfam, scpl, sdes)
;

(* Interpretation of template file 'updfam.txt' *)

type env 'a =
  [ Vstring of string
  | Vint of int
  | Vother of 'a
  | Vnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  Wserver.wprint ">%s<p>\n" ("UpdateFam." ^ func ^ ": not impl " ^ desc)
;

value obsolete_list = ref [];

value obsolete version var new_var r =
  if List.mem var obsolete_list.val then r
  else IFDEF UNIX THEN do {
    Printf.eprintf "*** <W> updfam.txt: \"%s\" obsolete since v%s%s\n"
      var version
      (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
    flush stderr;
    obsolete_list.val := [var :: obsolete_list.val];
    r
  }
  ELSE r END
;

value bool_val x = VVbool x;
value str_val x = VVstring x;

value rec eval_var conf base env (fam, cpl, des) loc =
  fun
  [ ["bvar"; v] ->
      try VVstring (List.assoc v conf.base_env) with
      [ Not_found -> VVstring "" ]
  | ["child" :: sl] ->
        let k =
          match get_env "cnt" env with
          [ Vint i ->
              let i = i - 1 in
              if i >= 0 && i < Array.length des.children then des.children.(i)
              else if i >= 0 && i < 1 && Array.length des.children = 0 then
                ("", "", 0, Update.Create Neuter None, "")
              else raise Not_found
          | _ -> raise Not_found ]
        in
        eval_key k sl
  | ["cnt"] -> eval_int_env "cnt" env
  | ["comment"] -> str_val (quote_escaped fam.comment)
  | ["digest"] -> eval_string_env "digest" env
  | ["divorce"] ->
      let s =
        match fam.divorce with
        [ Divorced _ -> "divorced"
        | NotDivorced -> "not_divorced"
        | Separated -> "separated" ]
      in
      str_val s
  | ["divorce"; s] ->
      let d =
        match fam.divorce with
        [ Divorced d -> Adef.od_of_codate d
        | _ -> None ]
      in
      eval_date_var d s
  | ["father" :: sl] -> eval_key (father cpl) sl
  | ["fsources"] -> str_val (quote_escaped fam.fsources)
  | ["marriage"; s] -> eval_date_var (Adef.od_of_codate fam.marriage) s
  | ["marriage_place"] -> str_val (quote_escaped fam.marriage_place)
  | ["marriage_src"] -> str_val (quote_escaped fam.marriage_src)
  | ["mrel"] -> str_val (eval_relation_kind fam.relation)
  | ["origin_file"] -> str_val (quote_escaped fam.origin_file)
  | ["parent" :: sl] ->
        let k =
          match get_env "cnt" env with
          [ Vint i ->
              let arr = parent_array cpl in
              let i = i - 1 in
              if i >= 0 && i < Array.length arr  then arr.(i)
              else if i >= 0 && i < 1 && Array.length arr = 0 then
                ("", "", 0, Update.Create Neuter None, "")
              else raise Not_found
          | _ -> raise Not_found ]
        in
        eval_parent conf base env k sl
  | ["witness" :: sl] ->
        let k =
          match get_env "cnt" env with
          [ Vint i ->
              let i = i - 1 in
              if i >= 0 && i < Array.length fam.witnesses then
                fam.witnesses.(i)
              else if i >= 0 && i < 2 && Array.length fam.witnesses < 2 then
                ("", "", 0, Update.Create Neuter None, "")
              else raise Not_found
          | _ -> raise Not_found ]
        in
        eval_key k sl
  | [s] ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> str_val (quote_escaped vv)
        | None -> str_val "" ]
      else
        let v = extract_var "bvar_" s in
        let v =
          if v = "" then extract_var "cvar_" s (* deprecated since 5.00 *)
          else v
        in
        if v <> "" then
          str_val (try List.assoc v conf.base_env with [ Not_found -> "" ])
        else raise Not_found
  | _ -> raise Not_found ]
and eval_date_var od s = str_val (eval_date_var_aux od s)
and eval_date_var_aux od =
  fun
  [ "calendar" ->
      match od with
      [ Some (Dgreg _ Dgregorian) -> "gregorian"
      | Some (Dgreg _ Djulian) -> "julian"
      | Some (Dgreg _ Dfrench) -> "french"
      | Some (Dgreg _ Dhebrew) -> "hebrew"
      | _ -> "" ]
  | "day" ->
      match eval_date_field od with
      [ Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "" ]
  | "month" ->
      match eval_date_field od with
      [ Some d ->
          if d.month = 0 then ""
          else
            match od with
            [ Some (Dgreg _ Dfrench) -> short_f_month d.month
            | _ -> string_of_int d.month ]
      | None -> "" ]
  | "oryear" ->
      match od with
      [ Some (Dgreg {prec = OrYear y} _) -> string_of_int y
      | Some (Dgreg {prec = YearInt y} _) -> string_of_int y
      | _ -> "" ]
  | "prec" ->
      match od with
      [ Some (Dgreg {prec = Sure} _) -> "sure"
      | Some (Dgreg {prec = About} _) -> "about"
      | Some (Dgreg {prec = Maybe} _) -> "maybe"
      | Some (Dgreg {prec = Before} _) -> "before"
      | Some (Dgreg {prec = After} _) -> "after"
      | Some (Dgreg {prec = OrYear _} _) -> "oryear"
      | Some (Dgreg {prec = YearInt _} _) -> "yearint"
      | _ -> "" ]
  | "text" ->
      match od with
      [ Some (Dtext s) -> s
      | _ -> "" ]
  | "year" ->
      match eval_date_field od with
      [ Some d -> string_of_int d.year
      | None -> "" ]
  | _ -> raise Not_found ]
and eval_date_field =
  fun
  [ Some d ->
      match d with
      [ Dgreg d Dgregorian -> Some d
      | Dgreg d Djulian -> Some (Calendar.julian_of_gregorian d)
      | Dgreg d Dfrench -> Some (Calendar.french_of_gregorian d)
      | Dgreg d Dhebrew -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None ]
  | None -> None ]
and eval_parent conf base env k =
  fun
  [ ["himher"] ->
      let s =
        match get_env "cnt" env with
        [ Vint 1 -> capitale (transl_nth conf "him/her" 0)
        | Vint 2 -> capitale (transl_nth conf "him/her" 1)
        | Vint n -> transl conf "him/her"
        | _ -> "???" ]
      in
      str_val s
  | sl -> eval_key k sl ]
and eval_key (fn, sn, oc, create, var) =
  fun 
  [ ["create"] -> str_val (if create <> Update.Link then "create" else "link")
  | ["create"; s] -> str_val (eval_create create s)
  | ["first_name"] -> str_val (quote_escaped fn)
  | ["occ"] -> str_val (if oc = 0 then "" else string_of_int oc)
  | ["surname"] -> str_val (quote_escaped sn)
  | x ->
      match x with
      [ ["sex"] ->
          obsolete "5.00" "sex" "create.sex"
            (str_val (eval_create create "sex"))
      | _ -> raise Not_found ] ]
and eval_create c =
  fun
  [ "birth_day" ->
      match c with
      [ Update.Create _ (Some {ci_birth_date = Some (Dgreg dmy Dfrench)}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create _ (Some {ci_birth_date = Some (Dgreg {day = d} _)})
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "birth_month" ->
      match c with
      [ Update.Create _ (Some {ci_birth_date = Some (Dgreg dmy Dfrench)}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.month <> 0 then short_f_month dmy.month else ""
      | Update.Create _ (Some {ci_birth_date = Some (Dgreg {month = m} _)})
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "birth_place" ->
      match c with
      [ Update.Create _ (Some {ci_birth_place = pl}) -> quote_escaped pl
      | _ -> "" ]
  | "birth_year" ->
      match c with
      [ Update.Create _ (Some ci) ->
          match ci.ci_birth_date with
          [ Some (Dgreg dmy Dfrench) ->
              let dmy = Calendar.french_of_gregorian dmy in
              add_precision (string_of_int dmy.year) dmy.prec
          | Some (Dgreg {year = y; prec = p} _) ->
              add_precision (string_of_int y) p
          | Some _ -> ""
          | None -> if ci.ci_public then "p" else "" ]
      | _ -> "" ]
  | "death_day" ->
      match c with
      [ Update.Create _ 
          (Some {ci_death_date = Some (Dgreg dmy Dfrench)})
        ->
          let dmy = Calendar.french_of_gregorian dmy in
          if dmy.day <> 0 then string_of_int dmy.day else ""
      | Update.Create _ (Some {ci_death_date = Some (Dgreg {day = d} _)})
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "death_month" ->
      match c with
      [ Update.Create _ (Some {ci_death_date = Some (Dgreg dmy Dfrench)}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          short_f_month dmy.month
      | Update.Create _ (Some {ci_death_date = Some (Dgreg {month = m} _)})
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "death_place" ->
      match c with
      [ Update.Create _ (Some {ci_death_place = pl}) -> quote_escaped pl
      | _ -> "" ]
  | "death_year" ->
      match c with
      [ Update.Create _ (Some {ci_death_date = Some (Dgreg dmy Dfrench)}) ->
          let dmy = Calendar.french_of_gregorian dmy in
          add_precision (string_of_int dmy.year) dmy.prec
      | Update.Create _
          (Some {ci_death_date = Some (Dgreg {year = y; prec = p} _)})
        ->
          add_precision (string_of_int y) p
      | Update.Create _ (Some {ci_death = death; ci_death_date = None}) ->
          match death with
          [ DeadDontKnowWhen -> "+"
          | NotDead -> "-"
          | _ -> "" ]
      | _ -> "" ]
  | "occupation" ->
      match c with
      [ Update.Create _ (Some {ci_occupation = occupation}) -> 
          quote_escaped occupation
      | _ -> "" ]
  | "sex" ->
      match c with
      [ Update.Create Male _ -> "male"
      | Update.Create Female _ -> "female"
      | Update.Create Neuter _ -> "neuter"
      | _ -> "" ]
  | _ -> raise Not_found ]
and add_precision s p =
  match p with
  [ Maybe -> "?" ^ s
  | Before -> "&lt;" ^ s
  | After -> "&gt;" ^ s
  | About -> "/" ^ s ^ "/"
  | _ -> s ]
and eval_relation_kind =
  fun
  [ Married -> "marr"
  | NotMarried -> "not_marr"
  | Engaged -> "engaged"
  | NoSexesCheckNotMarried -> "nsck"
  | NoSexesCheckMarried -> "nsckm"
  | NoMention -> "no_ment" ]
and eval_int_env var env =
  match get_env var env with
  [ Vint x -> str_val (string_of_int x)
  | _ -> raise Not_found ]
and eval_string_env var env =
  match get_env var env with
  [ Vstring x -> str_val (quote_escaped x)
  | _ -> str_val "" ]
;

(* print *)

value print_foreach print_ast eval_expr =
  let rec print_foreach env ((fam, cpl, des) as fcd) _ s sl _ al =
    match [s :: sl] with
    [ ["child"] -> print_foreach_child env fcd al des.children s
    | ["witness"] -> print_foreach_witness env fcd al fam.witnesses s
    | ["parent"] -> print_foreach_parent env fcd al (parent_array cpl) s
    | _ -> raise Not_found ]
  and print_foreach_child env fcd al arr lab =
    for i = 0 to max 1 (Array.length arr) - 1 do {
      let env = [("cnt", Vint (i + 1)) :: env] in
      List.iter (print_ast env fcd) al
    }
  and print_foreach_witness env fcd al arr lab =
    for i = 0 to max 2 (Array.length arr) - 1 do {
      let env = [("cnt", Vint (i + 1)) :: env] in
      List.iter (print_ast env fcd) al
    }
  and print_foreach_parent env fcd al arr lab =
    for i = 0 to Array.length arr - 1 do {
      let env = [("cnt", Vint (i + 1)) :: env] in
      List.iter (print_ast env fcd) al
    }
  in
  print_foreach
;

value print_update_fam conf base fcd digest =
  match p_getenv conf.env "m" with
  [ Some
      ("ADD_FAM" | "ADD_FAM_OK" | "ADD_PAR" | "MOD_FAM" | "MOD_FAM_OK" |
       "MRG_DUP_FAM_Y_N" | "MRG_FAM" | "MRG_FAM_OK" | "MRG_MOD_FAM_OK") ->
      let env = [("digest", Vstring digest)] in
      Hutil.interp conf base "updfam"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl _ = Templ.eval_transl conf;
         Templ.eval_predefined_apply _ = raise Not_found;
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach}
        env fcd
  | _ -> incorrect_request conf ]
;

value print_del1 conf base ifam =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n";
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
          (Adef.int_of_ifam ifam);
        match p_getenv conf.env "ip" with
        [ Some ip -> xtag "input" "type=\"hidden\" name=\"ip\" value=\"%s\"" ip
        | None -> () ];
        xtag "input" "type=\"hidden\" name=\"m\" value=\"DEL_FAM_OK\"";
      end;
      tag "p" begin
        xtag "input" "type=\"submit\" value=\"Ok\"";
      end;
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value print_inv1 conf base p ifam1 ifam2 =
  let title _ =
    Wserver.wprint "%s" (capitale (transl_decline conf "invert" ""))
  in
  let cpl1 = foi base ifam1 in
  let cpl2 = foi base ifam2 in
  do {
    header conf title;
    Wserver.wprint "%s:"
      (capitale (transl conf "invert the order of the following families"));
    tag "ul" begin
      tag "li" begin
        Update.print_someone conf base (poi base (get_father cpl1));
        Wserver.wprint " %s " (transl_nth conf "and" 0);
        Update.print_someone conf base (poi base (get_mother cpl1));
      end;
      tag "li" begin
        Update.print_someone conf base (poi base (get_father cpl2));
        Wserver.wprint " %s " (transl_nth conf "and" 0);
        Update.print_someone conf base (poi base (get_mother cpl2));
      end;
    end;
    Wserver.wprint "\n";
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
          (Adef.int_of_iper (get_key_index p));
        xtag "input" "type=\"hidden\" name=\"f\" value=\"%d\""
          (Adef.int_of_ifam ifam2);
        xtag "input" "type=\"hidden\" name=\"m\" value=\"INV_FAM_OK\"";
      end;
      tag "p" begin
        xtag "input" "type=\"submit\" value=\"Ok\"";
      end;
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value print_add conf base =
  let (fath, moth, digest) =
    match p_getint conf.env "ip" with
    [ Some i ->
        let p = poi base (Adef.iper_of_int i) in
        let fath =
          if get_sex p = Male ||
             get_sex p = Neuter && p_getenv conf.env "sex" = Some "M" then
            person_key base (get_key_index p)
          else ("", "", 0, Update.Create Male None, "")
        in
        let moth =
          if get_sex p = Female ||
             get_sex p = Neuter && p_getenv conf.env "sex" = Some "F" then
            person_key base (get_key_index p)
          else ("", "", 0, Update.Create Female None, "")
        in
        let digest = string_of_int (Array.length (get_family p)) in
        (fath, moth, digest)
    | None ->
        (("", "", 0, Update.Create Male None, ""),
         ("", "", 0, Update.Create Female None, ""), "") ]
  in
  let fam =
    {marriage = Adef.codate_None; marriage_place = ""; marriage_src = "";
     witnesses = [| |]; relation = Married; divorce = NotDivorced;
     comment = ""; origin_file = ""; fsources = default_source conf;
     fam_index = bogus_family_index}
  and cpl = couple conf.multi_parents fath moth
  and des = {children = [| |]} in
  print_update_fam conf base (fam, cpl, des) digest
;

value print_add_parents conf base =
  match p_getint conf.env "ip" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      let fam =
        {marriage = Adef.codate_None; marriage_place = ""; marriage_src = "";
         witnesses = [| |]; relation = Married; divorce = NotDivorced;
         comment = ""; origin_file = ""; fsources = default_source conf;
         fam_index = bogus_family_index}
      and cpl =
        couple conf.multi_parents
          ("", sou base (get_surname p), 0, Update.Create Neuter None, "")
          ("", "", 0, Update.Create Neuter None, "")
      and des =
        {children =
           [| (sou base (get_first_name p), sou base (get_surname p),
               get_occ p, Update.Link, "") |]}
      in
      print_update_fam conf base (fam, cpl, des) ""
  | _ -> incorrect_request conf ]
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let sfam = string_family_of conf base (Adef.ifam_of_int i) in
      let digest = Update.digest_family sfam in
      print_update_fam conf base sfam digest
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i -> print_del1 conf base (Adef.ifam_of_int i)
  | _ -> incorrect_request conf ]
;

value rec find_families ifam =
  fun
  [ [ifam1; ifam2 :: ifaml] ->
      if ifam2 = ifam then Some (ifam1, ifam2)
      else find_families ifam [ifam2 :: ifaml]
  | _ -> None ]
;

value print_inv conf base =
  match (p_getint conf.env "i", p_getint conf.env "f") with
  [ (Some ip, Some ifam) ->
      let u = poi base (Adef.iper_of_int ip) in
      match
        find_families (Adef.ifam_of_int ifam) (Array.to_list (get_family u))
      with
      [ Some (ifam1, ifam2) ->
          let p = poi base (Adef.iper_of_int ip) in
          print_inv1 conf base p ifam1 ifam2
      | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
