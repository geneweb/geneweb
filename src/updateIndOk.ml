(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: updateIndOk.ml,v 3.15 2000-10-28 21:52:32 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

value raw_get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value get_nth conf key cnt =
  p_getenv conf.env (key ^ string_of_int cnt)
;

value getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
  [ Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound") ]
;

value rec reconstitute_string_list conf var ext cnt =
  match get_nth conf var cnt with
  [ Some s ->
      let s = only_printable s in
      let (sl, ext) = reconstitute_string_list conf var ext (cnt + 1) in
      match get_nth conf ("add_" ^ var) cnt with
      [ Some "on" -> ([s; "" :: sl], True)
      | _ -> ([s :: sl], ext) ]
  | _ -> ([], ext) ]
;

value reconstitute_insert_title conf ext cnt tl =
  let var = "ins_title" ^ string_of_int cnt in
  let n =
    match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
    [ (_, Some n) when n > 1 -> n
    | (Some "on", _) -> 1
    | _ -> 0 ]
  in
  if n > 0 then
    let tl =
      loop tl n where rec loop tl n =
        if n > 0 then 
          let t1 =
            {t_name = Tnone; t_ident = ""; t_place = "";
             t_date_start = Adef.codate_None;
             t_date_end = Adef.codate_None;
             t_nth = 0}
          in
          loop [t1 :: tl] (n - 1)
        else tl
    in
    (tl, True)
  else (tl, ext)
;

value rec reconstitute_titles conf ext cnt =
  match
    (get_nth conf "t_ident" cnt, get_nth conf "t_place" cnt,
     get_nth conf "t_name" cnt)
  with
  [ (Some t_ident, Some t_place, Some t_name) ->
      let t_name =
        match (get_nth conf "t_main_title" cnt, t_name) with
        [ (Some "on", _) -> Tmain
        | (_, "") -> Tnone
        | (_, _) -> Tname (only_printable t_name) ]
      in
      let t_date_start =
        Update.reconstitute_date conf ("t_date_start" ^ string_of_int cnt)
      in
      let t_date_end =
        Update.reconstitute_date conf ("t_date_end" ^ string_of_int cnt)
      in
      let t_nth =
        match get_nth conf "t_nth" cnt with
        [ Some s -> try int_of_string s with [ Failure _ -> 0 ]
        | _ -> 0 ]
      in
      let t =
        {t_name = t_name; t_ident = only_printable t_ident;
         t_place = only_printable t_place;
         t_date_start = Adef.codate_of_od t_date_start;
         t_date_end = Adef.codate_of_od t_date_end;
         t_nth = t_nth}
      in
      let (tl, ext) = reconstitute_titles conf ext (cnt + 1) in
      let (tl, ext) = reconstitute_insert_title conf ext cnt tl in
      ([t :: tl], ext)
  | _ -> ([], ext) ]
;

value reconstitute_add_relation conf ext cnt rl =
  match get_nth conf "add_relation" cnt with
  [ Some "on" ->
      let r =
        {r_type = Adoption; r_fath = None; r_moth = None; r_sources = ""}
      in
      ([r :: rl], True)
  | _ -> (rl, ext) ]
;

value reconstitute_relation_parent conf var key sex =
  match (getn conf var (key ^ "_fn"), getn conf var (key ^ "_sn")) with
  [ ("", _) | ("?", _) | (_, "?") -> None
  | (fn, sn) ->
      let occ =
        try int_of_string (getn conf var (key ^ "_occ")) with
        [ Failure _ -> 0 ]
      in
      let create =
        match getn conf var (key ^ "_p") with
        [ "create" -> Update.Create sex None
        | _ -> Update.Link ]
      in
      Some (fn, sn, occ, create) ]
;

value reconstitute_relation conf var =
  try
    let r_fath = reconstitute_relation_parent conf var "fath" Male in
    let r_moth = reconstitute_relation_parent conf var "moth" Female in
    let r_type =
      match getn conf var "type"  with
      [ "Adoption" -> Adoption
      | "Recognition" -> Recognition
      | "CandidateParent" -> CandidateParent
      | "GodParent" -> GodParent
      | "FosterParent" -> FosterParent
      | _ -> Adoption ]
    in
    Some {r_type = r_type; r_fath = r_fath; r_moth = r_moth; r_sources = ""}
  with
  [ Failure _ -> None ]
;

value rec reconstitute_relations conf ext cnt =
  match reconstitute_relation conf ("r" ^ string_of_int cnt) with
  [ Some r ->
      let (rl, ext) = reconstitute_relations conf ext (cnt + 1) in
      let (rl, ext) = reconstitute_add_relation conf ext cnt rl in
      ([r :: rl], ext)
  | _ -> ([], ext) ]
;

value reconstitute_death conf birth death_place burial burial_place =
  let d = Update.reconstitute_date conf "death" in
  let dr =
    match p_getenv conf.env "death_reason" with
    [ Some "Killed" -> Killed
    | Some "Murdered" -> Murdered
    | Some "Executed" -> Executed
    | Some "Disappeared" -> Disappeared
    | Some "Unspecified" | None -> Unspecified
    | Some x -> failwith ("bad death reason type " ^ x) ]
  in
  match get conf "death" with
  [ "Auto" when d = None ->
      if death_place <> "" || burial <> UnknownBurial || burial_place <> ""
      || dr <> Unspecified then DeadDontKnowWhen
      else Update.infer_death conf birth
  | "DeadYoung" when d = None -> DeadYoung
  | "DontKnowIfDead" when d = None -> DontKnowIfDead
  | "NotDead" -> NotDead
  | _ ->
      match d with
      [ Some d -> Death dr (Adef.cdate_of_date d)
      | _ -> DeadDontKnowWhen ] ]
;

value reconstitute_burial conf burial_place =
  let d = Update.reconstitute_date conf "burial" in
  match p_getenv conf.env "burial" with
  [ Some "UnknownBurial" | None ->
      match (d, burial_place) with
      [ (None, "") -> UnknownBurial
      | _ -> Buried (Adef.codate_of_od d) ]
  | Some "Buried" -> Buried (Adef.codate_of_od d)
  | Some "Cremated" -> Cremated (Adef.codate_of_od d)
  | Some x -> failwith ("bad burial type " ^ x) ]
;

value reconstitute_person conf =
  let ext = False in
  let cle_index =
    match p_getenv conf.env "i" with
    [ Some s -> try int_of_string (strip_spaces s) with [ Failure _ -> -1 ]
    | _ -> -1 ]
  in
  let first_name = only_printable (get conf "first_name") in
  let surname = only_printable (get conf "surname") in
  let occ =
(*
    if first_name = "?" || surname = "?" then 0
    else
*)
      try int_of_string (strip_spaces (get conf "occ")) with
      [ Failure _ -> 0 ]
  in
  let image = only_printable (get conf "image") in
  let (first_names_aliases, ext) =
    reconstitute_string_list conf "first_name_alias" ext 0
  in
  let (surnames_aliases, ext) =
    reconstitute_string_list conf "surname_alias" ext 0
  in
  let public_name = only_printable (get conf "public_name") in
  let (qualifiers, ext) = reconstitute_string_list conf "qualifier" ext 0 in
  let (aliases, ext) = reconstitute_string_list conf "alias" ext 0 in
  let (titles, ext) = reconstitute_titles conf ext 1 in
  let (titles, ext) = reconstitute_insert_title conf ext 0 titles in
  let (rparents, ext) = reconstitute_relations conf ext 1 in
  let (rparents, ext) = reconstitute_add_relation conf ext 0 rparents in
  let access =
    match p_getenv conf.env "access" with
    [ Some "Public" -> Public
    | Some "Private" -> Private
    | _ -> IfTitles ]
  in
  let occupation = only_printable (get conf "occu") in
  let sex =
    match p_getenv conf.env "sex" with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let public = False in
  let birth = Update.reconstitute_date conf "birth" in
  let birth_place = only_printable (get conf "birth_place") in
  let bapt = Adef.codate_of_od (Update.reconstitute_date conf "bapt") in
  let bapt_place = only_printable (get conf "bapt_place") in
  let burial_place = only_printable (get conf "burial_place") in
  let burial = reconstitute_burial conf burial_place in
  let death_place = get conf "death_place" in
  let death = reconstitute_death conf birth death_place burial burial_place in
  let death_place =
    match death with
    [ Death _ _ | DeadYoung | DeadDontKnowWhen -> death_place
    | _ -> "" ]
  in
  let death =
    match (death, burial) with
    [ (NotDead | DontKnowIfDead, Buried _ | Cremated _) -> DeadDontKnowWhen
    | _ -> death ]
  in
  let notes =
    if first_name = "?" || surname = "?" then ""
    else strip_spaces (strip_controls_m (get conf "notes"))
  in
  let psources = only_printable (get conf "src") in
  let p =
    {first_name = first_name; surname = surname; occ = occ;
     image = image;
     first_names_aliases = first_names_aliases;
     surnames_aliases = surnames_aliases;
     public_name = public_name;
     qualifiers = qualifiers; aliases = aliases; titles = titles;
     rparents = rparents; occupation = occupation;
     related = []; sex = sex; access = access;
     birth = Adef.codate_of_od birth; birth_place = birth_place;
     birth_src = only_printable (get conf "birth_src");
     baptism = bapt; baptism_place = bapt_place;
     baptism_src = only_printable (get conf "bapt_src");
     death = death; death_place = death_place;
     death_src = only_printable (get conf "death_src");
     burial = burial; burial_place = burial_place;
     burial_src = only_printable (get conf "burial_src");
     notes = notes; psources = psources;
     cle_index = Adef.iper_of_int cle_index}
  in
  (p, ext)
;

value check_person conf base p =
  if p.first_name = "" || p.first_name = "?" then
    Some (transl conf "first name missing")
  else if p.surname = "" || p.surname = "?" then
    Some (transl conf "surname missing")
  else None
;

value error_person conf base p err =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do rheader conf title;
     Wserver.wprint "%s\n" (capitale err);
     trailer conf;
  return ()
;

value strip_list = list_filter (fun s -> s <> "");

value strip_person p =
  do p.first_names_aliases := strip_list p.first_names_aliases;
     p.surnames_aliases := strip_list p.surnames_aliases;
     p.qualifiers := strip_list p.qualifiers;
     p.aliases := strip_list p.aliases;
     p.titles := list_filter (fun t -> t.t_ident <> "") p.titles;
     p.rparents :=
       list_filter (fun r -> r.r_fath <> None || r.r_moth <> None) p.rparents;
  return ()
;

value print_conflict conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do rheader conf title;
     Update.print_error conf base (AlreadyDefined p);
     html_p conf;
  return
  let free_n =
    Update.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  do tag "ul" begin
       html_li conf;
       Wserver.wprint "%s: %d.\n"
         (capitale (transl conf "first free number")) free_n;
       Wserver.wprint "%s " (capitale (transl conf "click"));
       Wserver.wprint "<a href=\n\"%s" (commd conf);
       list_iter_first
         (fun first (v, x) ->
            do Wserver.wprint "%s" (if first then "" else ";");
               Wserver.wprint "%s=" v;
               if v = "occ" then Wserver.wprint "%d" free_n
               else Wserver.wprint "%s" x;
            return ())
         conf.env;
       Wserver.wprint "\">%s</a>" (transl conf "here");
       Wserver.wprint "%s.\n" (transl conf " to try again with this number");
       html_li conf;
       Wserver.wprint "%s " (capitale (transl conf "or"));
       Wserver.wprint (ftransl conf "click on \"%s\"") (transl conf "back");
       Wserver.wprint "%s %s." (transl conf "and")
         (transl conf "change it (the number) yourself");
     end;
     Update.print_return conf;
     Update.print_same_name conf base p;
     trailer conf;
  return raise Update.ModErr
;

value print_cannot_change_sex conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do rheader conf title;
     Update.print_error conf base (BadSexOfMarriedPerson p);
     tag "ul" begin
       html_li conf;
       afficher_personne_referencee conf base p;
       Wserver.wprint "\n";
     end;
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value check_conflict conf base sp ipl =
  let name = Name.lower (sp.first_name ^ " " ^ sp.surname) in
  List.iter
    (fun ip ->
       let p1 = poi base ip in
       if p1.cle_index <> sp.cle_index
       && Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1)
          = name
       && p1.occ = sp.occ then
         print_conflict conf base p1
       else ())
    ipl
;

value check_sex_married conf base sp op =
  if sp.sex <> op.sex then
    let u = uoi base op.cle_index in
    if Array.length u.family != 0 then print_cannot_change_sex conf base op
    else ()
  else ()
;

value rename_image_file conf base op sp =
  match auto_image_file conf base op with
  [ Some old_f ->
      let s = default_image_name_of_key sp.first_name sp.surname sp.occ in
      let f =
        List.fold_right Filename.concat [base_dir.val; "images"; conf.bname] s
      in
      let new_f =
        if Filename.check_suffix old_f ".gif" then f ^ ".gif"
        else f ^ ".jpg"
      in
      try Sys.rename old_f new_f with [ Sys_error _ -> () ]
  | _ -> () ]
;

value rparents_of p =
  List.fold_left
    (fun ipl r ->
       match (r.r_fath, r.r_moth) with
       [ (Some ip1, Some ip2) -> [ip1; ip2 :: ipl]
       | (Some ip, _) -> [ip :: ipl]
        | (_, Some ip) -> [ip :: ipl]
       | _ -> ipl ])
    [] p.rparents
;

value is_witness_at_marriage base ip p =
(*
do Printf.eprintf "cherche si %s est temoin au mariage de %s\n" (denomination base (poi base ip)) (denomination base p); flush stderr; return
*)
  let u = uoi base ip in
  List.exists
    (fun ifam ->
       let fam = foi base ifam in
       array_memq ip fam.witnesses)
    (Array.to_list u.family)
;

value update_relation_parents base op np =
  let op_rparents = rparents_of op in
  let np_rparents = rparents_of np in
  let pi = np.cle_index in
  let mod_ippl = [] in
  let mod_ippl =
    List.fold_left
      (fun ippl ip ->
         if List.mem ip op_rparents then ippl
         else
           let p = poi base ip in
           if not (List.mem pi p.related) then
             do p.related := [pi :: p.related]; return
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           else ippl)
      mod_ippl np_rparents
  in
  let mod_ippl =
    List.fold_left
      (fun ippl ip ->
         let p = poi base ip in
         if List.mem ip np_rparents
         || np.sex = Male && is_witness_at_marriage base ip np then
           ippl
         else
           if List.mem pi p.related then
             do p.related := list_filter (\<> pi) p.related; return
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           else ippl)
      mod_ippl op_rparents
  in
  List.iter (fun (ip, p) -> base.func.patch_person ip p) mod_ippl
;

value rec enrich_relation pos lrel =
  let enrich_rel pos rel =
    let prefix = "r" ^ string_of_int pos in
    let r_fath_typ = Update.R_father pos in
    let r_moth_typ = Update.R_mother pos in
    { r_type = rel.r_type;
      r_fath = match rel.r_fath with
               [ Some key -> Some (r_fath_typ, key)
               | None -> None ];
      r_moth = match rel.r_moth with
               [ Some key -> Some (r_moth_typ, key)
               | None -> None ];
      r_sources = rel.r_sources }
  in
  match lrel with
  [ [] -> []
  | [ head :: lrest ] ->
      [ enrich_rel pos head :: enrich_relation (pos + 1) lrest ]
  ]
;

value enrich_person sp =
  { first_name = sp.first_name;
    surname = sp.surname;
    occ = sp.occ;
    image = sp.image;
    aliases = sp.aliases;
    first_names_aliases = sp.first_names_aliases;
    surnames_aliases = sp.surnames_aliases;
    public_name = sp.public_name;
    qualifiers = sp.qualifiers;
    titles = sp.titles;
    rparents = enrich_relation 1 sp.rparents;
    related = sp.related;
    occupation = sp.occupation;
    sex = sp.sex;
    access = sp.access;
    birth = sp.birth;
    birth_place = sp.birth_place;
    birth_src = sp.birth_src;
    baptism = sp.baptism;
    baptism_place = sp.baptism_place;
    baptism_src = sp.baptism_src;
    death = sp.death;
    death_place = sp.death_place;
    death_src = sp.death_src;
    burial = sp.burial;
    burial_place = sp.burial_place;
    burial_src = sp.burial_src;
    notes = sp.notes;
    psources = sp.psources;
    cle_index = sp.cle_index }
;

value effective_mod conf base sp =
  let pi = sp.cle_index in
  let op = poi base pi in
  let key = sp.first_name ^ " " ^ sp.surname in
  let ofn = p_first_name base op in
  let osn = p_surname base op in
  do if Name.lower ofn = Name.lower sp.first_name
     && Name.lower osn = Name.lower sp.surname
     && op.occ == sp.occ then ()
     else
       let ipl = person_ht_find_all base key in
       do check_conflict conf base sp ipl;
          rename_image_file conf base op sp;
       return ();
     if Name.crush_lower (ofn ^ " " ^ osn) <> Name.crush_lower key
     || (ofn = "?" || osn = "?") && sp.first_name <> "?" && sp.surname <> "?"
     then
       person_ht_add base key pi
     else ();
     check_sex_married conf base sp op;
  return
  let created_p = ref [] in
  let np =
    map_person_ps (Update.insert_person conf base sp.psources created_p)
      (Update.insert_string conf base) (enrich_person sp)
  in
  do np.related := op.related; return
  let op_misc_names = person_misc_names base op in
  let np_misc_names = person_misc_names base np in
  do List.iter
       (fun key ->
          if List.mem key op_misc_names then ()
          else person_ht_add base key pi)
       np_misc_names;
     update_relation_parents base op np;
  return np
;

value effective_add conf base sp =
  let pi = Adef.iper_of_int (base.data.persons.len) in
  let key = nominative (sp.first_name ^ " " ^ sp.surname) in
  let ipl = person_ht_find_all base key in
  do check_conflict conf base sp ipl;
     person_ht_add base key pi;
  return
  let created_p = ref [] in
  let np =
    map_person_ps (Update.insert_person conf base sp.psources created_p)
      (Update.insert_string conf base) (enrich_person sp)
  in
  let na = {parents = None; consang = Adef.fix (-1)} in
  let nu = {family = [| |]} in
  do np.cle_index := pi;
     base.func.patch_person pi np;
     base.func.patch_ascend pi na;
     base.func.patch_union pi nu;
  return
  let np_misc_names = person_misc_names base np in
  do List.iter (fun key -> person_ht_add base key pi) np_misc_names; return
  (np, na)
;

value array_except v a =
  loop 0 where rec loop i =
    if i == Array.length a then a
    else if a.(i) = v then
      Array.append (Array.sub a 0 i)
        (Array.sub a (i + 1) (Array.length a - i - 1))
    else loop (i + 1)
;

value effective_del conf base p =
  let none = Update.insert_string conf base "?" in
  let empty = Update.insert_string conf base "" in
  let asc = aoi base p.cle_index in
  do match asc.parents with
     [ Some ifam ->
         let des = doi base ifam in
         do des.children := array_except p.cle_index des.children;
            asc.parents := None;
            asc.consang := Adef.fix (-1);
            base.func.patch_descend ifam des;
            base.func.patch_ascend p.cle_index asc;
         return ()
     | None -> () ];
     p.first_name := none;
     p.surname := none;
     p.occ := 0;
     p.image := empty;
     p.public_name := empty;
     p.qualifiers := [];
     p.aliases := [];
     p.first_names_aliases := [];
     p.surnames_aliases := [];
     p.titles := [];
     p.rparents := [];
     p.related := [];
     p.occupation := empty;
     p.access := IfTitles;
     p.birth := Adef.codate_None;
     p.birth_place := empty;
     p.birth_src := empty;
     p.baptism := Adef.codate_None;
     p.baptism_place := empty;
     p.baptism_src := empty;
     p.death := DontKnowIfDead;
     p.death_place := empty;
     p.death_src := empty;
     p.burial := UnknownBurial;
     p.burial_place := empty;
     p.burial_src := empty;
     p.notes := empty;
     p.psources := empty;
  return ()
;

value print_mod_ok conf base wl p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "person modified"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     Update.print_warnings conf base wl;
     trailer conf;
  return ()
;

(*
value print_mod_ok conf base wl p =
  if wl = [] then Perso.print conf base p
  else print_mod_ok_aux conf base wl p
;
*)

value all_checks_person conf base p a u =
  let wl = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  do Gutil.check_person base error warning p;
     match a.parents with
     [ Some ifam ->
         Gutil.check_family base error warning (foi base ifam)
           (coi base ifam) (doi base ifam)
     | _ -> () ];
     Array.iter
       (fun ifam ->
          Gutil.check_family base error warning (foi base ifam)
            (coi base ifam) (doi base ifam))
       u.family;
     List.iter
       (fun
        [ ChangedOrderOfChildren ifam des _ -> base.func.patch_descend ifam des
        | _ -> () ])
       wl.val;
  return List.rev wl.val
;

value print_add_ok conf base wl p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "person added")) in
  do header conf title;
     print_link_to_welcome conf True;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     Update.print_warnings conf base wl;
     trailer conf;
  return ()
;

(*
value print_add_ok conf base wl p =
  if wl = [] then Perso.print conf base p
  else print_add_ok_aux conf base wl p
;
*)

value print_del_ok conf base wl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "person deleted"))
  in
  do header conf title;
     print_link_to_welcome conf False;
     Update.print_warnings conf base wl;
     trailer conf;
  return ()
;

value print_add o_conf base =
  let conf = Update.update_conf o_conf in
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let (sp, ext) = reconstitute_person conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some _ -> True
          | _ -> False ]
        in
        if ext || redisp then UpdateInd.print_add1 conf base sp
        else
          do strip_person sp; return
          match check_person conf base sp with
          [ Some err -> error_person conf base sp err
          | None ->
              let (p, a) = effective_add conf base sp in
              let u = uoi base p.cle_index in
              let wl = all_checks_person conf base p a u in
              let k = (sp.first_name, sp.surname, sp.occ) in
              do base.func.commit_patches ();
                 History.record conf base k "ap";
                 print_add_ok conf base wl p;
              return () ]
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_del conf base =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      match p_getint conf.env "i" with
      [ Some i ->
          let p = base.data.persons.get i in
          let k = (sou base p.first_name, sou base p.surname, p.occ) in
          do effective_del conf base p;
             base.func.patch_person p.cle_index p;
             base.func.commit_patches ();
             History.record conf base k "dp";
             print_del_ok conf base [];
          return ()
      | _ -> incorrect_request conf ]
  | Refuse -> Update.error_locked conf base ]
;

value print_mod_aux conf base callback =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let (p, ext) = reconstitute_person conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some _ -> True
          | _ -> False ]
        in
        let digest = Update.digest_person (poi base p.cle_index) in
        if digest = raw_get conf "digest" then
          if ext || redisp then UpdateInd.print_mod1 conf base p digest
          else
            do strip_person p; return
            match check_person conf base p with
            [ Some err -> error_person conf base p err
            | None -> callback p ]
        else Update.error_digest conf base
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_mod o_conf base =
  let conf = Update.update_conf o_conf in
  let callback sp =
    let p = effective_mod conf base sp in
    let u = uoi base p.cle_index in
    do base.func.patch_person p.cle_index p;
       Update.update_misc_names_of_family base p u;
    return
    let wl = all_checks_person conf base p (aoi base p.cle_index) u in
    let k = (sp.first_name, sp.surname, sp.occ) in
    do base.func.commit_patches ();
       History.record conf base k "mp";
       print_mod_ok conf base wl p;
    return ()
  in
  print_mod_aux conf base callback
;
