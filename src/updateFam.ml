(* camlp4r ./pa_html.cmo *)
(* $Id: updateFam.ml,v 4.37 2003-12-23 11:56:05 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Util;
open Config;
open TemplAst;

value bogus_family_index = Adef.ifam_of_int (-1);

value default_source conf =
  match p_getenv conf.env "dsrc" with
  [ Some s -> s
  | None -> "" ]
;

value person_key base ip =
  let p = poi base ip in
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip else p.occ
  in
  (first_name, surname, occ, Update.Link, "")
;

value string_family_of base fam cpl des =
  let sfam = Gutil.map_family_ps (person_key base) (sou base) fam in
  let scpl = Gutil.map_couple_p (person_key base) cpl in
  let sdes = Gutil.map_descend_p (person_key base) des in
  (sfam, scpl, sdes)
;

(* Interpretation of template file 'updfam.txt' *)

type env = 
  [ Vstring of string
  | Vfun of list string and list ast
  | Vint of int
  | Vnone ]
;

type variable_value =
  [ VVgen of string
  | VVcreate of Update.create and string
  | VVdate of option date and string
  | VVcvar of string
  | VVind of Update.key and string
  | VVnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

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

(* string values *)

value eval_base_env_variable conf v =
  try List.assoc v conf.base_env with [ Not_found -> "" ]
;

value get_create (fn, sn, oc, create, var) = create;

value rec eval_variable conf base env ((fam, cpl, des) as fcd) =
  fun
  [ ["father"; s] -> VVind cpl.father s
  | ["father"; "create"; s] -> VVcreate (get_create cpl.father) s
  | ["mother"; s] -> VVind cpl.mother s
  | ["mother"; "create"; s] -> VVcreate (get_create cpl.mother) s
  | ["marriage"; s] -> VVdate (Adef.od_of_codate fam.marriage) s
  | ["divorce"; s] ->
      let d =
        match fam.divorce with
        [ Divorced d -> Adef.od_of_codate d
        | _ -> None ]
      in
      VVdate d s
  | ["witness"; s] ->
        match get_env "cnt" env with
        [ Vint i ->
            let i = i - 1 in
            if i >= 0 && i < Array.length fam.witnesses then
              VVind fam.witnesses.(i) s
            else if i >= 0 && i < 2 && Array.length fam.witnesses < 2 then
              VVind ("", "", 0, Update.Create Neuter None, "") s
            else VVnone
        | _ -> VVnone ]
  | ["child" :: sl] ->
        let r =
          match get_env "cnt" env with
          [ Vint i ->
              let i = i - 1 in
              if i >= 0 && i < Array.length des.children then
                Some des.children.(i)
              else if i >= 0 && i < 1 && Array.length des.children = 0 then
                Some ("", "", 0, Update.Create Neuter None, "")
              else None
          | _ -> None ]
        in
        match (r, sl) with
        [ (Some ind, [s]) -> VVind ind s
        | (Some ind, ["create"; s]) -> VVcreate (get_create ind) s
        | _ -> VVnone ]
  | [] -> VVgen ""
  | [s] ->
      let v = extract_var "cvar_" s in if v <> "" then VVcvar v else VVgen s
  | [s :: sl] -> VVnone ]
;

value eval_relation_kind =
  fun
  [ Married -> "marr"
  | NotMarried -> "not_marr"
  | Engaged -> "engaged"
  | NoSexesCheck -> "nsck"
  | NoMention -> "no_ment" ]
;

value eval_divorce =
  fun
  [ Divorced _ -> "divorced"
  | NotDivorced -> "not_divorced"
  | Separated -> "separated" ]
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

value try_eval_gen_variable conf base env ((fam, cpl, des) as fcd) =
  fun
  [ "cnt" -> eval_int_env "cnt" env
  | "comment" -> quote_escaped fam.comment
  | "digest" -> eval_string_env "digest" env
  | "divorce" -> eval_divorce fam.divorce
  | "mrel" -> eval_relation_kind fam.relation
  | "marriage_place" -> quote_escaped fam.marriage_place
  | "marriage_src" -> quote_escaped fam.marriage_src
  | "fsources" -> quote_escaped fam.fsources
  | "origin_file" -> quote_escaped fam.origin_file
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value eval_key_variable (fn, sn, oc, create, var) =
  fun
  [ "first_name" -> quote_escaped fn
  | "occ" -> if oc = 0 then "" else string_of_int oc
  | "surname" -> quote_escaped sn
  | "create" -> if create <> Update.Link then "create" else "link"
  | "sex" ->
      match create with
      [ Update.Create Male _ -> "male"
      | Update.Create Female _ -> "female"
      | Update.Create Neuter _ -> "neuter"
      | _ -> "" ]
  | s -> ">%" ^ s ^ "???" ]
;

value eval_create_variable c =
  fun
  [ "birth_year" ->
      match c with
      [ Update.Create _
        (Some (Some (Dgreg {year = y; prec = p} _), _, _, _, _)) ->
          let s = string_of_int y in
          match p with
          [ Maybe -> "?" ^ s
          | Before -> "<" ^ s
          | After -> ">" ^ s
          | About -> "/" ^ s ^ "/"
          | _ -> s ]
      | _ -> "" ]
  | "birth_month" ->
      match c with
      [ Update.Create _ (Some (Some (Dgreg {month = m} _), _, _, _, _))
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "birth_day" ->
      match c with
      [ Update.Create _ (Some (Some (Dgreg {day = d} _), _, _, _, _))
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "birth_place" ->
      match c with
      [ Update.Create _ (Some (_, pl, _, _, _)) -> quote_escaped pl
      | _ -> "" ]
  | "death_year" ->
      match c with
      [ Update.Create _
        (Some (_, _, _, Some (Dgreg {year = y; prec = p} _), _)) ->
          let s = string_of_int y in
          match p with
          [ Maybe -> "?" ^ s
          | Before -> "<" ^ s
          | After -> ">" ^ s
          | About -> "/" ^ s ^ "/"
          | _ -> s ]
      | Update.Create _ (Some (_, _, death, None, _)) ->
          match death with
          [ DeadDontKnowWhen -> "+"
          | NotDead -> "-"
          | _ -> "" ]
      | _ -> "" ]
  | "death_month" ->
      match c with
      [ Update.Create _ (Some (_, _, _, Some (Dgreg {month = m} _), _))
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "death_day" ->
      match c with
      [ Update.Create _ (Some (_, _, _, Some (Dgreg {day = d} _), _))
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "death_place" ->
      match c with
      [ Update.Create _ (Some (_, _, _, _, pl)) -> quote_escaped pl
      | _ -> "" ]
  | s -> ">%" ^ s ^ "???" ]
;

value eval_expr conf base env p =
  fun
  [ Estr s -> s
  | Evar s [] ->
      try try_eval_gen_variable conf base env p s with
      [ Not_found -> ">" ^ s ^ "???" ]
  | Etransl upp s c -> Templ.eval_transl conf upp s c
  | _ -> ">parse_error" ]
;

(* bool values *)

value substring_mem ss s =
  loop 0 0 0 where rec loop j_ini i j =
    if i = String.length ss then True
    else if j = String.length s then False
    else if ss.[i] = s.[j] then loop j_ini (i + 1) (j + 1)
    else loop (j_ini + 1) 0 (j_ini + 1)
;

value eval_gen_bool_variable conf base env fcd =
  fun
  [ s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv conf.env v with
        [ Some "" | None -> False
        | _ -> True ]
      else do { Wserver.wprint ">%%%s???" s; False } ]
;

value eval_bool_variable conf base env fcd s sl =
  match eval_variable conf base env fcd [s :: sl] with
  [ VVgen s -> eval_gen_bool_variable conf base env fcd s
  | VVcreate _ _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVdate _ _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVcvar _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVind _ _ -> do { Wserver.wprint ">%%VVind???"; False }
  | VVnone -> do { Wserver.wprint ">%%%s???" s; False } ]
;

value eval_bool_value conf base env fcd =
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
    | Evar s sl -> eval_bool_variable conf base env fcd s sl
    | Estr s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Eint s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Etransl _ s _ -> do { Wserver.wprint "[%s]???" s; False } ]
  and string_eval =
    fun
    [ Estr s -> s
    | Evar s sl ->
        try
          match eval_variable conf base env fcd [s :: sl] with
          [ VVgen s -> try_eval_gen_variable conf base env fcd s
          | VVcreate c s -> do { Wserver.wprint ">%%%s???" s; "" }
          | VVdate od s -> Templ.eval_date_variable od s
          | VVcvar s -> eval_base_env_variable conf s
          | VVind pk s -> eval_key_variable pk s
          | VVnone -> do { Wserver.wprint ">%%%s???" s; "" } ]
        with
        [ Not_found -> do { Wserver.wprint ">%%%s???" s; "" } ]
    | Etransl upp s c -> Templ.eval_transl conf upp s c
    | x -> do { Wserver.wprint "val???"; "" } ]
  in
  bool_eval
;

(* print *)

value print_variable conf base env fcd sl =
  match eval_variable conf base env fcd sl with 
  [ VVgen s ->
      try Wserver.wprint "%s" (try_eval_gen_variable conf base env fcd s) with
      [ Not_found -> Templ.print_variable conf base s ]
  | VVcreate c s -> Wserver.wprint "%s" (eval_create_variable c s)
  | VVdate od s -> Wserver.wprint "%s" (Templ.eval_date_variable od s)
  | VVcvar s ->
      try Wserver.wprint "%s" (List.assoc s conf.base_env) with
      [ Not_found -> () ]
  | VVind pk s -> Wserver.wprint "%s" (eval_key_variable pk s)
  | VVnone ->
      do {
        Wserver.wprint ">%%";
        list_iter_first
          (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
          sl;
        Wserver.wprint "???";
      } ]
;

value rec print_ast conf base env fcd =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n -> Wserver.wprint "%s" (Templ.eval_transl conf upp s n)
  | Avar s sl -> print_variable conf base env fcd [s :: sl]
  | Aif e alt ale -> print_if conf base env fcd e alt ale
  | Aforeach s sl al -> print_foreach conf base env fcd s sl al
  | Adefine f xl al alk -> print_define conf base env fcd f xl al alk
  | Aapply f el -> print_apply conf base env fcd f el
  | x -> not_impl "print_ast" x ]
and print_define conf base env fcd f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] fcd) alk
and print_apply conf base env fcd f el =
  match get_env f env with
  [ Vfun xl al ->
      let vl = List.map (eval_expr conf base env fcd) el in
      List.iter
        (fun a ->
           let a =
             loop a xl vl where rec loop a xl vl =
               match (xl, vl) with
               [ ([x :: xl], [v :: vl]) ->
                   loop (Templ.subst (Templ.subst_text x v) a) xl vl
               | ([], []) -> a
               | _ -> Atext "parse_error" ]
           in
           print_ast conf base env fcd a)
        al
  | _ -> Wserver.wprint ">%%%s???" f ]
and print_if conf base env fcd e alt ale =
  let al = if eval_bool_value conf base env fcd e then alt else ale in
  List.iter (print_ast conf base env fcd) al
and print_foreach conf base env fcd s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env fcd sl with
  [ VVgen "" -> print_simple_foreach conf base env fcd al s
  | VVgen _ ->
      do {
        Wserver.wprint "foreach ";
        List.iter (fun s -> Wserver.wprint "%s." s) sl;
        Wserver.wprint "%s???" s;
      }
  | VVcvar _ | VVdate _ _ | VVind _ _ | VVcreate _ _ | VVnone -> () ]
and print_simple_foreach conf base env ((fam, cpl, des) as fcd) al s =
  match s with
  [ "child" -> print_foreach_child conf base env fcd al des.children s
  | "witness" -> print_foreach_witness conf base env fcd al fam.witnesses s
  | _ -> Wserver.wprint "foreach %s???" s ]
and print_foreach_child conf base env fcd al arr lab =
  for i = 0 to max 1 (Array.length arr) - 1 do {
    let env = [("cnt", Vint (i + 1)) :: env] in
    List.iter (print_ast conf base env fcd) al
  }
and print_foreach_witness conf base env fcd al arr lab =
  for i = 0 to max 2 (Array.length arr) - 1 do {
    let env = [("cnt", Vint (i + 1)) :: env] in
    List.iter (print_ast conf base env fcd) al
  }
;

value interp_templ conf base fcd digest astl =
  let env = [("digest", Vstring digest)] in
  List.iter (print_ast conf base env fcd) astl
;

value print_update_fam conf base fcd digest =
  match p_getenv conf.env "m" with
  [ Some
      ("ADD_FAM" | "ADD_FAM_OK" | "ADD_PAR" | "MOD_FAM" | "MOD_FAM_OK" |
       "MRG_FAM" | "MRG_FAM_OK" | "MRG_MOD_FAM_OK") ->
      let astl = Templ.input conf "updfam" in
      do { html1 conf; nl (); interp_templ conf base fcd digest astl }
  | _ -> incorrect_request conf ]
;

value print_add1 conf base fam cpl des digest force_children_surnames =
  print_update_fam conf base (fam, cpl, des) digest
;

value print_mod1 conf base fam cpl des digest =
  print_update_fam conf base (fam, cpl, des) digest
;

value print_merge1 conf base fam des fam2 digest =
  let cpl = Gutil.map_couple_p (person_key base) (coi base fam.fam_index) in
  print_update_fam conf base (fam, cpl, des) digest
;

value print_del1 conf base fam =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
        (Adef.int_of_ifam fam.fam_index);
      match p_getenv conf.env "ip" with
      [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
      | None -> () ];
      Wserver.wprint "<input type=hidden name=m value=DEL_FAM_OK>\n";
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value print_inv1 conf base p fam1 fam2 =
  let title _ =
    Wserver.wprint "%s" (capitale (transl_decline conf "invert" ""))
  in
  let cpl1 = coi base fam1.fam_index in
  let cpl2 = coi base fam2.fam_index in
  do {
    header conf title;
    Wserver.wprint "%s:"
      (capitale (transl conf "invert the order of the following families"));
    tag "ul" begin
      html_li conf;
      Update.print_someone conf base (poi base cpl1.father);
      Wserver.wprint " %s " (transl_nth conf "and" 0);
      Update.print_someone conf base (poi base cpl1.mother);
      html_li conf;
      Update.print_someone conf base (poi base cpl2.father);
      Wserver.wprint " %s " (transl_nth conf "and" 0);
      Update.print_someone conf base (poi base cpl2.mother);
    end;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
        (Adef.int_of_iper p.cle_index);
      Wserver.wprint "<input type=hidden name=f value=%d>\n\n"
        (Adef.int_of_ifam fam2.fam_index);
      Wserver.wprint "<input type=hidden name=m value=INV_FAM_OK>\n";
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value print_add conf base =
  let (fath, moth, digest) =
    match p_getint conf.env "ip" with
    [ Some i ->
        let p = base.data.persons.get i in
        let fath =
          if p.sex = Male ||
             p.sex = Neuter && p_getenv conf.env "sex" = Some "M" then
            person_key base p.cle_index
          else ("", "", 0, Update.Create Male None, "")
        in
        let moth =
          if p.sex = Female ||
             p.sex = Neuter && p_getenv conf.env "sex" = Some "F" then
            person_key base p.cle_index
          else ("", "", 0, Update.Create Female None, "")
        in
        let digest =
          string_of_int (Array.length (uoi base p.cle_index).family)
        in
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
  and cpl = {father = fath; mother = moth}
  and des = {children = [| |]} in
  print_add1 conf base fam cpl des digest False
;

value print_add_parents conf base =
  match p_getint conf.env "ip" with
  [ Some i ->
      let p = base.data.persons.get i in
      let fam =
        {marriage = Adef.codate_None; marriage_place = ""; marriage_src = "";
         witnesses = [| |]; relation = Married; divorce = NotDivorced;
         comment = ""; origin_file = ""; fsources = default_source conf;
         fam_index = bogus_family_index}
      and cpl =
        {father = ("", sou base p.surname, 0, Update.Create Neuter None, "");
         mother = ("", "", 0, Update.Create Neuter None, "")}
      and des =
        {children =
           [| (sou base p.first_name, sou base p.surname, p.occ, Update.Link,
               "") |]}
      in
      print_add1 conf base fam cpl des "" True
  | _ -> incorrect_request conf ]
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      let cpl = coi base (Adef.ifam_of_int i) in
      let des = doi base (Adef.ifam_of_int i) in
      let (sfam, scpl, sdes) = string_family_of base fam cpl des in
      let digest = Update.digest_family fam cpl des in
      print_mod1 conf base sfam scpl sdes digest
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      print_del1 conf base fam
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
      let u = base.data.unions.get ip in
      match
        find_families (Adef.ifam_of_int ifam) (Array.to_list u.family)
      with
      [ Some (ifam1, ifam2) ->
          let p = base.data.persons.get ip in
          print_inv1 conf base p (foi base ifam1) (foi base ifam2)
      | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
