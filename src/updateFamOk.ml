(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: updateFamOk.ml,v 2.16 1999-07-22 13:56:54 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

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

value getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
  [ Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound") ]
;

value reconstitute_parent conf var =
  let first_name = strip_spaces (getn conf var "first_name") in
  let surname = strip_spaces (getn conf var "surname") in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let create =
    match getn conf var "p" with
    [ "create" -> UpdateFam.Create Neuter None
    | _ -> UpdateFam.Link ]
  in
  (first_name, surname, occ, create)
;

value reconstitute_child conf var default_surname =
  let first_name = getn conf var "first_name" in
  let surname =
    let surname = getn conf var "surname" in
    if surname = "" then default_surname else surname
  in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let birth = Update.reconstitute_date conf var in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let create =
    match getn conf var "p" with
    [ "create" -> UpdateFam.Create sex birth
    | _ -> UpdateFam.Link ]
  in
  (first_name, surname, occ, create)
;

value reconstitute_family conf =
  let ext = False in
  let father = reconstitute_parent conf "his" in
  let mother = reconstitute_parent conf "her" in
  let not_married =
    match p_getenv conf.env "not_married" with
    [ Some "true" -> True
    | _ -> False ]
  in
  let marriage = Update.reconstitute_date conf "marriage" in
  let marriage_place = strip_spaces (get conf "marriage_place") in
  let divorce =
    match p_getenv conf.env "divorce" with
    [ Some "not_divorced" -> NotDivorced
    | _ ->
        Divorced
          (Adef.codate_of_od
             (Update.reconstitute_date conf "divorce")) ]
  in
  let surname = getn conf "his" "surname" in
  let (children, ext) =
    loop 1 False where rec loop i ext =
      match
        try
          Some (reconstitute_child conf ("child" ^ string_of_int i) surname)
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (children, ext) = loop (i + 1) ext in
          match p_getenv conf.env ("ins_child" ^ string_of_int i) with
          [ Some "on" ->
              let new_child = ("", "", 0, UpdateFam.Create Neuter None) in
              ([c; new_child :: children], True)
          | _ -> ([c :: children ], ext) ]
      | None -> ([], ext) ]
  in
  let (children, ext) =
    match p_getenv conf.env "ins_child0" with
    [ Some "on" ->
        let new_child = ("", "", 0, UpdateFam.Create Neuter None) in
        ([new_child :: children], True)
    | _ -> (children, ext) ]
  in
  let comment = strip_spaces (get conf "comment") in
  let fsources = strip_spaces (get conf "src") in
  let fam_index =
    match p_getint conf.env "i" with
    [ Some i -> i
    | None -> 0 ]
  in
  let fam =
    {marriage = Adef.codate_of_od marriage;
     marriage_place = marriage_place;
     marriage_src = strip_spaces (get conf "marr_src");
     not_married = not_married;
     divorce = divorce; children = Array.of_list children; comment = comment;
     origin_file = ""; fsources = fsources;
     fam_index = Adef.ifam_of_int fam_index}
  and cpl =
    {father = father; mother = mother}
  in
  (fam, cpl, ext)
;

value new_persons = ref [];

value add_misc_names_for_new_persons base =
  do List.iter
       (fun p ->
          List.iter (fun n -> person_ht_add base n p.cle_index)
            (person_misc_names base p))
       new_persons.val;
     new_persons.val := [];
  return ()
;

value print_err_unknown conf base (f, s, o) =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     Wserver.wprint "%s: <strong>%s.%d %s</strong>\n"
       (capitale (transl conf "unknown person")) f o s;
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value print_create_conflict conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  let n =
    Update.find_free_occ base (sou base p.first_name) (sou base p.surname) 0
  in
  do header conf title;
     Update.print_error conf base (AlreadyDefined p);
     html_p conf;
     Wserver.wprint "<ul>\n";
     html_li conf;
     Wserver.wprint "%s: %d\n" (capitale (transl conf "first free number")) n;
     html_li conf;
     Wserver.wprint "%s\n"
       (capitale (transl conf "or use \"link\" instead of \"create\""));
     Wserver.wprint "</ul>\n";
     Update.print_same_name conf base p;
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value insert_person conf base src (f, s, o, create) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  [ UpdateFam.Create sex birth ->
      try
        if f = "?" || s = "?" then
          if o <= 0 || o >= base.data.persons.len then raise Not_found
          else
            let ip = Adef.iper_of_int o in
            let p = poi base ip in
            if sou base p.first_name = f && sou base p.surname = s then ip
            else raise Not_found
        else
          let ip = person_ht_find_unique base f s o in
          print_create_conflict conf base (poi base ip)
      with
      [ Not_found ->
          let o = if f = "?" || s = "?" then 0 else o in
          let ip = Adef.iper_of_int (base.data.persons.len) in
          let death = Update.infer_death conf birth in
          let empty_string = Update.insert_string conf base "" in
          let p =
            {first_name = Update.insert_string conf base f;
             surname = Update.insert_string conf base s;
             occ = o; image = empty_string;
             first_names_aliases = []; surnames_aliases = [];
             public_name = empty_string;
             nick_names = []; aliases = []; titles = [];
             occupation = empty_string;
             sex = sex; access = IfTitles;
             birth = Adef.codate_of_od birth; birth_place = empty_string;
             birth_src = empty_string;
             baptism = Adef.codate_None; baptism_place = empty_string;
             baptism_src = empty_string;
             death = death; death_place = empty_string;
             death_src = empty_string;
             burial = UnknownBurial; burial_place = empty_string;
             burial_src = empty_string;
             family = [| |];
             notes = empty_string;
             psources = Update.insert_string conf base src;
             cle_index = ip}
          and a =
            {parents = None;
             consang = Adef.fix (-1)}
          in
          do base.func.patch_person p.cle_index p;
             base.func.patch_ascend p.cle_index a;
             if f <> "?" && s <> "?" then
               do person_ht_add base (f ^ " " ^ s) ip;
                  new_persons.val := [p :: new_persons.val];
               return ()
             else ();
          return ip ]
  | UpdateFam.Link ->
      if f = "?" || s = "?" then
        if o < 0 || o >= base.data.persons.len then
          print_err_unknown conf base (f, s, o)
        else
          let ip = Adef.iper_of_int o in
          let p = poi base ip in
          if sou base p.first_name = f && sou base p.surname = s then ip
          else print_err_unknown conf base (f, s, o)
      else
        try person_ht_find_unique base f s o with
        [ Not_found -> print_err_unknown conf base (f, s, o) ] ]
;

value strip_children pl =
  let pl =
    List.fold_right
      (fun ((f, s, o, c) as p) pl -> if f = "" then pl else [p :: pl])
      (Array.to_list pl) []
  in
  Array.of_list pl
;

value strip_family fam =
  fam.children := strip_children fam.children
;

value print_err_parents conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     Wserver.wprint "\n";
     Wserver.wprint (fcapitale (ftransl conf "%t already has parents"))
       (fun _ -> afficher_personne_referencee conf base p);
     Wserver.wprint "\n";
     html_p conf;
     tag "ul" begin
       html_li conf;
       Wserver.wprint "%s: %d"
         (capitale (transl conf "first free number"))
         (Update.find_free_occ base (sou base p.first_name)
            (sou base p.surname) 0);
     end;
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value print_err_father_sex conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n%s\n" (transl conf "should be male");
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value print_err_mother_sex conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n%s\n" (transl conf "should be female");
     Update.print_return conf;
     trailer conf;
  return raise Update.ModErr
;

value family_exclude pfams efam =
  let pfaml =
    List.fold_right
      (fun fam faml -> if fam == efam then faml else [fam :: faml])
      (Array.to_list pfams) []
  in
  Array.of_list pfaml
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

value infer_origin_file conf base ncpl nfam =
  let afath = aoi base ncpl.father in
  let amoth = aoi base ncpl.mother in
  match (afath.parents, amoth.parents) with
  [ (Some if1, _) when sou base (foi base if1).origin_file <> "" ->
      (foi base if1).origin_file
  | (_, Some if2) when sou base (foi base if2).origin_file <> "" ->
      (foi base if2).origin_file
  | _ ->
      loop 0 where rec loop i =
        if i == Array.length nfam.children then
          Update.insert_string conf base ""
        else
          let cifams = (poi base nfam.children.(i)).family in
          if Array.length cifams == 0 then loop (i + 1)
          else if sou base (foi base cifams.(0)).origin_file <> "" then
            (foi base cifams.(0)).origin_file
          else loop (i + 1) ]
;

value effective_mod conf base sfam scpl =
  let fi = sfam.fam_index in
  let ofam = foi base fi in
  let ocpl = coi base fi in
  let nfam =
    map_family_ps (insert_person conf base sfam.fsources)
      (Update.insert_string conf base) sfam
  in
  let ncpl = map_couple_p (insert_person conf base sfam.fsources) scpl in
  let ofath = poi base ocpl.father in
  let omoth = poi base ocpl.mother in
  let nfath = poi base ncpl.father in
  let nmoth = poi base ncpl.mother in
  do match nfath.sex with
     [ Female -> print_err_father_sex conf base nfath
     | _ -> nfath.sex := Male ];
     match nmoth.sex with
     [ Male -> print_err_mother_sex conf base nmoth
     | _ -> nmoth.sex := Female ];
     nfam.origin_file :=
       if sou base ofam.origin_file <> "" then ofam.origin_file
       else infer_origin_file conf base ncpl nfam;
     nfam.fam_index := fi;
     base.func.patch_family fi nfam;
     base.func.patch_couple fi ncpl;
     if nfath.cle_index != ofath.cle_index then
       do ofath.family := family_exclude ofath.family ofam.fam_index;
          nfath.family := Array.append nfath.family [| fi |];
          base.func.patch_person ofath.cle_index ofath;
          base.func.patch_person nfath.cle_index nfath;
       return ()
     else ();
     if nmoth.cle_index != omoth.cle_index then
       do omoth.family := family_exclude omoth.family ofam.fam_index;
          nmoth.family := Array.append nmoth.family [| fi |];
          base.func.patch_person omoth.cle_index omoth;
          base.func.patch_person nmoth.cle_index nmoth;
       return ()
     else ();
  return
  let find_asc =
    let cache = Hashtbl.create 101 in
    fun ip ->
      try Hashtbl.find cache ip with
      [ Not_found ->
          let a = aoi base ip in
          do Hashtbl.add cache ip a; return a ]
  in
  let same_parents =
    nfath.cle_index = ofath.cle_index &&
    nmoth.cle_index = omoth.cle_index
  in
  do Array.iter
       (fun ip ->
          let a = find_asc ip in
          do a.parents := None; return
          if not (array_memq ip nfam.children) then a.consang := Adef.fix (-1)
          else ())
       ofam.children;
     Array.iter
       (fun ip ->
          let a = find_asc ip in
          match a.parents with
          [ Some _ -> print_err_parents conf base (poi base ip)
          | None ->
              do a.parents := Some fi; return
              if not (array_memq ip ofam.children) || not same_parents then
                a.consang := Adef.fix (-1)
              else () ])
       nfam.children;
     Array.iter
       (fun ip ->
          if not (array_memq ip nfam.children) then
            base.func.patch_ascend ip (find_asc ip)
          else ())
       ofam.children;
     Array.iter
       (fun ip ->
          if not (array_memq ip ofam.children) || not same_parents then
            base.func.patch_ascend ip (find_asc ip)
          else ())
       nfam.children;
     add_misc_names_for_new_persons base;
     Update.update_misc_names_of_family base nfath;
  return (nfam, ncpl)
;

value effective_add conf base sfam scpl =
  let fi = Adef.ifam_of_int (base.data.families.len) in
  let nfam =
    map_family_ps (insert_person conf base sfam.fsources)
      (Update.insert_string conf base) sfam
  in
  let ncpl = map_couple_p (insert_person conf base sfam.fsources) scpl in
  let origin_file = infer_origin_file conf base ncpl nfam in
  let nfath = poi base ncpl.father in
  let nmoth = poi base ncpl.mother in
  do match nfath.sex with
     [ Female -> print_err_father_sex conf base nfath
     | _ -> nfath.sex := Male ];
     match nmoth.sex with
     [ Male -> print_err_mother_sex conf base nmoth
     | _ -> nmoth.sex := Female ];
     nfam.fam_index := fi;
     nfam.origin_file := origin_file;
     base.func.patch_family fi nfam;
     base.func.patch_couple fi ncpl;
     nfath.family := Array.append nfath.family [| fi |];
     nmoth.family := Array.append nmoth.family [| fi |];
     base.func.patch_person nfath.cle_index nfath;
     base.func.patch_person nmoth.cle_index nmoth;
     Array.iter
       (fun ip ->
          let a = aoi base ip in
          let p = poi base ip in
          match a.parents with
          [ Some _ -> print_err_parents conf base p
          | None ->
              do a.parents := Some fi;
                 a.consang := Adef.fix (-1);
                 base.func.patch_ascend p.cle_index a;
              return () ])
       nfam.children;
     add_misc_names_for_new_persons base;
     Update.update_misc_names_of_family base nfath;
  return (nfam, ncpl)
;

value effective_swi conf base p ifam =
  let rec loop =
    fun
    [ [ifam1; ifam2 :: ifaml] ->
        if ifam2 = ifam then [ifam2; ifam1 :: ifaml]
        else [ifam1 :: loop [ifam2 :: ifaml]]
    | _ -> do incorrect_request conf; return raise Update.ModErr ]
  in
  do p.family := Array.of_list (loop (Array.to_list p.family));
     base.func.patch_person p.cle_index p;
  return ()
;

value kill_family base fam ip =
  let p = poi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam == fam.fam_index then ifaml else [ifam :: ifaml])
      (Array.to_list p.family) []
  in
  do p.family := Array.of_list l;
     base.func.patch_person ip p;
  return ()
;

value kill_parents base ip =
  let a = aoi base ip in
  do a.parents := None;
     a.consang := Adef.fix (-1);
     base.func.patch_ascend ip a;
  return ()
;

value effective_del conf base fam =
  let ifam = fam.fam_index in
  let cpl = coi base ifam in
  do kill_family base fam cpl.father;
     kill_family base fam cpl.mother;
     Array.iter (kill_parents base) fam.children;
     cpl.father := Adef.iper_of_int (-1);
     cpl.mother := Adef.iper_of_int (-1);
     fam.children := [| |];
     fam.comment := Update.insert_string conf base "";
     fam.fam_index := Adef.ifam_of_int (-1);
     base.func.patch_family ifam fam;
     base.func.patch_couple ifam cpl;
  return ()
;

value all_checks_family conf base fam cpl =
  let wl = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  do Gutil.check_noloop_for_person_list base error [cpl.father; cpl.mother];
     Gutil.check_family base error warning fam;
  return List.rev wl.val
;

value print_family conf base wl fam cpl =
  do Wserver.wprint "<ul>\n";
     html_li conf;
     afficher_personne_referencee conf base (poi base cpl.father);
     Wserver.wprint "\n";
     html_li conf;
     afficher_personne_referencee conf base (poi base cpl.mother);
     Wserver.wprint "</ul>\n";
     if fam.children <> [||] then
       do html_p conf;
          Wserver.wprint "<ul>\n";
          Array.iter
            (fun ip ->
               do html_li conf;
                  afficher_personne_referencee conf base (poi base ip);
                  Wserver.wprint "\n";
               return ())
            fam.children;
          Wserver.wprint "</ul>\n";
       return ()
     else ();
     Update.print_warnings conf base wl;
  return ()
;

value print_mod_ok conf base wl fam cpl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family modified"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     print_family conf base wl fam cpl;
     trailer conf;
  return ()
;

(*
value print_mod_ok conf base wl fam cpl =
  if wl = [] then
    match p_getenv conf.env "ip" with
    [ Some ip ->
        Perso.print conf base (base.data.persons.get (int_of_string ip))
    | None -> print_mod_ok_aux conf base wl fam cpl ]
  else print_mod_ok_aux conf base wl fam cpl
;
*)

value print_add_ok conf base wl fam cpl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family added"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     print_family conf base wl fam cpl;
     trailer conf;
  return ()
;

(*
value print_add_ok conf base wl fam cpl =
  if wl = [] then
    match p_getenv conf.env "i" with
    [ Some ip ->
        Perso.print conf base (base.data.persons.get (int_of_string ip))
    | None -> print_add_ok_aux conf base wl fam cpl ]
  else print_add_ok_aux conf base wl fam cpl
;
*)

value print_del_ok conf base wl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family deleted"))
  in
  do header conf title;
     print_link_to_welcome conf False;
     Update.print_warnings conf base wl;
     trailer conf;
  return ()
;

(*
value print_del_ok conf base wl =
  if wl = [] then
    match p_getenv conf.env "ip" with
    [ Some ip ->
        Perso.print conf base (base.data.persons.get (int_of_string ip))
    | None -> print_del_ok_aux conf base wl ]
  else print_del_ok_aux conf base wl
;
*)

value print_swi_ok conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "switch done"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

(*
value print_swi_ok conf base p =
  Perso.print conf base p
;
*)

value delete_topological_sort conf base =
  let bfile = Filename.concat base_dir.val conf.bname in
  let tstab_file = Filename.concat (bfile ^ ".gwb") "tstab" in
  try Sys.remove tstab_file with [ Sys_error _ -> () ]
;

value print_add conf base =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let (sfam, scpl, ext) = reconstitute_family conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some "on" -> True
          | _ -> False ]
        in
        if ext || redisp then UpdateFam.print_add1 conf base sfam scpl False
        else
          do strip_family sfam; return
          let (fam, cpl) = effective_add conf base sfam scpl in
          let wl = all_checks_family conf base fam cpl in
          do base.func.commit_patches ();
             delete_topological_sort conf base;
             print_add_ok conf base wl fam cpl;
          return ()
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
          let fam = foi base (Adef.ifam_of_int i) in
          do if not (is_deleted_family fam) then
               do effective_del conf base fam;
                  base.func.commit_patches ();
                  delete_topological_sort conf base;
               return ()
             else ();
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
        let (sfam, scpl, ext) = reconstitute_family conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some "on" -> True
          | _ -> False ]
        in
        let digest = Update.digest_family (foi base sfam.fam_index) in
        if digest = raw_get conf "digest" then
          if ext || redisp then UpdateFam.print_mod1 conf base sfam scpl digest
          else
            do strip_family sfam; return
            callback sfam scpl
          else Update.error_digest conf base
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_mod conf base =
  let callback sfam scpl =
    let (fam, cpl) = effective_mod conf base sfam scpl in
    let wl = all_checks_family conf base fam cpl in
    do base.func.commit_patches ();
       delete_topological_sort conf base;
       print_mod_ok conf base wl fam cpl;
    return ()
  in
  print_mod_aux conf base callback
;

value print_swi conf base =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      match (p_getint conf.env "i", p_getint conf.env "f") with
      [ (Some ip, Some ifam)  ->
          let p = base.data.persons.get ip in
          try
            do effective_swi conf base p (Adef.ifam_of_int ifam);
               base.func.commit_patches ();
               print_swi_ok conf base p;
            return ()
          with [ Update.ModErr -> () ]
      | _ -> incorrect_request conf ]
  | Refuse -> Update.error_locked conf base ]
;
