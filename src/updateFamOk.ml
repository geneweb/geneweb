(* camlp4r ./pa_lock.cmo *)
(* $Id: updateFamOk.ml,v 1.1 1998-09-01 14:32:07 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value get env key =
  match p_getenv env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value getn var key env =
  match p_getenv env (var ^ "_" ^ key) with
  [ Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound") ]
;

value reconstitute_person env var =
  let first_name = getn var "first_name" env in
  let surname = getn var "surname" env in
  let occ = try int_of_string (getn var "occ" env) with [ Failure _ -> 0 ] in
  let create =
    match getn var "p" env with
    [ "create" -> UpdateFam.Create Neutre
    | "create_M" -> UpdateFam.Create Masculin
    | "create_F" -> UpdateFam.Create Feminin
    | _ -> UpdateFam.Link ]
  in
  (first_name, surname, occ, create)
;

value reconstitute_child env var default_surname =
  let first_name = getn var "first_name" env in
  let surname =
    let surname = getn var "surname" env in
    if surname = "" then default_surname else surname
  in
  let occ = try int_of_string (getn var "occ" env) with [ Failure _ -> 0 ] in
  let create =
    match getn var "p" env with
    [ "create" -> UpdateFam.Create Neutre
    | "create_M" -> UpdateFam.Create Masculin
    | "create_F" -> UpdateFam.Create Feminin
    | _ -> UpdateFam.Link ]
  in
  (first_name, surname, occ, create)
;

value reconstitute_family conf =
  let ext = False in
  let father = reconstitute_person conf.env "his" in
  let mother = reconstitute_person conf.env "her" in
  let marriage = Update.reconstitute_date conf "marriage" in
  let marriage_place =
    match p_getenv conf.env "marriage_place" with
    [ Some s -> s
    | None -> "" ]
  in
  let divorce =
    match p_getenv conf.env "divorce" with
    [ Some "not_divorced" -> NotDivorced
    | _ ->
        Divorced
          (Adef.codate_of_od
             (Update.reconstitute_date conf "divorce")) ]
  in
  let surname = getn "his" "surname" conf.env in
  let (children, ext) =
    loop 1 False where rec loop i ext =
      match
        try
          Some
            (reconstitute_child conf.env ("child" ^ string_of_int i) surname)
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (children, ext) = loop (i + 1) ext in
          match p_getenv conf.env ("add_child" ^ string_of_int i) with
          [ Some "on" ->
              ([c; ("", "", 0, UpdateFam.Create Neutre) :: children], True)
          | _ -> ([c :: children ], ext) ]
      | None -> ([], ext) ]
  in
  let (children, ext) =
    match p_getenv conf.env "add_child0" with
    [ Some "on" -> ([("", "", 0, UpdateFam.Create Neutre) :: children], True)
    | _ -> (children, ext) ]
  in
  let comment =
    match p_getenv conf.env "comment" with
    [ Some s -> s
    | None -> "" ]
  in
  let fsources =
    match p_getenv conf.env "src" with
    [ Some s -> s
    | None -> "" ]
  in
  let fam_index =
    match p_getint conf.env "i" with
    [ Some i -> i
    | None -> 0 ]
  in
  let fam =
    {marriage = Adef.codate_of_od marriage;
     marriage_place = marriage_place;
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
     trailer conf;
  return ()
;

value print_create_conflict conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Update.print_error conf base (AlreadyDefined p);
     Wserver.wprint "<p>\n";
     Wserver.wprint "<ul>\n";
     Wserver.wprint "<li>%s: %d\n"
       (capitale (transl conf "first free number"))
       (Update.find_free_occ base (sou base p.first_name) (sou base p.surname)
          0);
     Wserver.wprint "<li>%s\n"
       (capitale (transl conf "or use \"link\" instead of \"create\""));
     Wserver.wprint "</ul>\n";
     Update.print_same_name conf base p;
     trailer conf;
  return ()
;

value insert_person conf base (f, s, o, create) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  [ UpdateFam.Create sexe ->
      try
        if f = "?" || s = "?" then
          if o <= 0 || o >= base.persons.len then raise Not_found
          else
            let ip = Adef.iper_of_int o in
            let p = poi base ip in
            if sou base p.first_name = f && sou base p.surname = s then ip
            else raise Not_found
        else
          let ip = person_ht_find_unique base f s o in
          do print_create_conflict conf base (poi base ip); return
          raise Update.ModErr
      with
      [ Not_found ->
          let o = if f = "?" || s = "?" then 0 else o in
          let ip = Adef.iper_of_int (base.persons.len) in
          let empty_string = Update.insert_string conf base "" in
          let p =
            {first_name = Update.insert_string conf base f;
             surname = Update.insert_string conf base s;
             occ = o; photo = empty_string;
             first_names_aliases = []; surnames_aliases = [];
             public_name = empty_string;
             nick_names = []; aliases = []; titles = [];
             occupation = empty_string;
             sexe = sexe; access = IfTitles;
             birth = Adef.codate_None; birth_place = empty_string;
             baptism = Adef.codate_None; baptism_place = empty_string;
             death = DontKnowIfDead; death_place = empty_string;
             burial = UnknownBurial; burial_place = empty_string;
             family = [| |];
             notes = empty_string;
             psources = empty_string;
             cle_index = ip}
          and a =
            {parents = None;
             consang = Adef.fix (-1)}
          in
          do base.patch_person p.cle_index p;
             base.patch_ascend p.cle_index a;
             if f <> "?" && s <> "?" then
               do person_ht_add base (f ^ " " ^ s) ip;
                  new_persons.val := [p :: new_persons.val];
               return ()
             else ();
          return ip ]
  | UpdateFam.Link ->
      if f = "?" || s = "?" then
        if o < 0 || o >= base.persons.len then
          do print_err_unknown conf base (f, s, o); return
          raise Update.ModErr
        else
          let ip = Adef.iper_of_int o in
          let p = poi base ip in
          if sou base p.first_name = f && sou base p.surname = s then ip
          else
            do print_err_unknown conf base (f, s, o); return
            raise Update.ModErr
      else
        try person_ht_find_unique base f s o with
        [ Not_found ->
            do print_err_unknown conf base (f, s, o); return
            raise Update.ModErr ] ]
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
  do fam.children := strip_children fam.children;
     if Array.length fam.children <> 0 then fam.comment := "" else ();
  return ()
;

value print_err_parents conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     Wserver.wprint "\n";
     Wserver.wprint (fcapitale (ftransl conf "%t already has parents"))
       (fun _ -> afficher_personne_referencee conf base p);
     Wserver.wprint "\n<p>\n";
     Wserver.wprint "<ul><li>%s: %d</ul>\n"
       (capitale (transl conf "first free number"))
       (Update.find_free_occ base (sou base p.first_name) (sou base p.surname)
          0);
     trailer conf;
  return ()
;

value print_err_father_sex conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n%s\n" (transl conf "should be of sex masculine");
     trailer conf;
  return ()
;

value print_err_mother_sex conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n%s\n" (transl conf "should be of sex feminine");
     trailer conf;
  return ()
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

value effective_mod conf base sfam scpl =
  let fi = sfam.fam_index in
  let ofam = foi base fi in
  let ocpl = coi base fi in
  let nfam =
    map_family_ps (insert_person conf base) (Update.insert_string conf base)
      sfam
  in
  let ncpl = map_couple_p (insert_person conf base) scpl in
  let ofath = poi base ocpl.father in
  let omoth = poi base ocpl.mother in
  let nfath = poi base ncpl.father in
  let nmoth = poi base ncpl.mother in
  do match nfath.sexe with
     [ Feminin ->
         do print_err_father_sex conf base nfath; return raise Update.ModErr
     | _ -> nfath.sexe := Masculin ];
     match nmoth.sexe with
     [ Masculin ->
         do print_err_mother_sex conf base nmoth; return raise Update.ModErr
     | _ -> nmoth.sexe := Feminin ];
     nfam.origin_file := ofam.origin_file;
     nfam.fam_index := fi;
     base.patch_family fi nfam;
     base.patch_couple fi ncpl;
     if nfath.cle_index != ofath.cle_index then
       do ofath.family := family_exclude ofath.family ofam.fam_index;
          nfath.family := Array.append nfath.family [| fi |];
          base.patch_person ofath.cle_index ofath;
          base.patch_person nfath.cle_index nfath;
       return ()
     else ();
     if nmoth.cle_index != omoth.cle_index then
       do omoth.family := family_exclude omoth.family ofam.fam_index;
          nmoth.family := Array.append nmoth.family [| fi |];
          base.patch_person omoth.cle_index omoth;
          base.patch_person nmoth.cle_index nmoth;
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
  do Array.iter
       (fun ip ->
          let a = find_asc ip in
          do a.parents := None; return
          if not (array_memq ip nfam.children) then base.patch_ascend ip a
          else ())
       ofam.children;
     Array.iter
       (fun ip ->
          let a = find_asc ip in
          match a.parents with
          [ Some _ ->
              do print_err_parents conf base (poi base ip); return
              raise Update.ModErr
          | None ->
              do a.parents := Some fi; return
              if not (array_memq ip ofam.children) then
                base.patch_ascend ip a
              else () ])
       nfam.children;
     add_misc_names_for_new_persons base;
     Update.update_misc_names_of_family base nfath;
  return (nfam, ncpl)
;

value effective_add conf base sfam scpl =
  let fi = Adef.ifam_of_int (base.families.len) in
  let nfam =
    map_family_ps (insert_person conf base) (Update.insert_string conf base)
      sfam
  in
  let ncpl = map_couple_p (insert_person conf base) scpl in
  let origin_file =
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
  in
  let nfath = poi base ncpl.father in
  let nmoth = poi base ncpl.mother in
  do match nfath.sexe with
     [ Feminin ->
         do print_err_father_sex conf base nfath; return raise Update.ModErr
     | _ -> nfath.sexe := Masculin ];
     match nmoth.sexe with
     [ Masculin ->
         do print_err_mother_sex conf base nmoth; return raise Update.ModErr
     | _ -> nmoth.sexe := Feminin ];
     nfam.fam_index := fi;
     nfam.origin_file := origin_file;
     base.patch_family fi nfam;
     base.patch_couple fi ncpl;
     nfath.family := Array.append nfath.family [| fi |];
     nmoth.family := Array.append nmoth.family [| fi |];
     base.patch_person nfath.cle_index nfath;
     base.patch_person nmoth.cle_index nmoth;
     Array.iter
       (fun ip ->
          let a = aoi base ip in
          let p = poi base ip in
          match a.parents with
          [ Some _ ->
              do print_err_parents conf base p; return raise Update.ModErr
          | None ->
              do base.patch_ascend p.cle_index a;
                 a.parents := Some fi;
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
     base.patch_person p.cle_index p;
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
     base.patch_person ip p;
  return ()
;

value kill_parents base ip =
  let a = aoi base ip in
  do a.parents := None;
     base.patch_ascend ip a;
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
     base.patch_family ifam fam;
     base.patch_couple ifam cpl;
  return ()
;

value all_checks_family conf base fam cpl =
  let wl = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  do Gutil.check_noloop_for_person_list base error
       [poi base cpl.father; poi base cpl.mother];
     Gutil.check_family base error warning fam;
  return List.rev wl.val
;

value print_family conf base wl fam cpl =
  do Wserver.wprint "<ul>";
     Wserver.wprint "\n<li>\n";
     afficher_personne_referencee conf base (poi base cpl.father);
     Wserver.wprint "\n";
     Wserver.wprint "\n<li>\n";
     afficher_personne_referencee conf base (poi base cpl.mother);
     Wserver.wprint "</ul>\n";
     if fam.children <> [||] then
       do Wserver.wprint "<p>\n<ul>\n";
          Array.iter
            (fun ip ->
               do Wserver.wprint "<li>\n";
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
     print_family conf base wl fam cpl;
     trailer conf;
  return ()
;

value print_add_ok conf base wl fam cpl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family added"))
  in
  do header conf title;
     print_family conf base wl fam cpl;
     trailer conf;
  return ()
;

value print_del_ok conf base wl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family deleted"))
  in
  do header conf title;
     Update.print_warnings conf base wl;
     trailer conf;
  return ()
;

value print_swi_ok conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "switch done"))
  in
  do header conf title;
     afficher_personne_referencee conf base p;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add conf base =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let (sfam, scpl, ext) = reconstitute_family conf in
        if ext then UpdateFam.print_add1 conf base sfam scpl False
        else
          do strip_family sfam; return
          let (fam, cpl) = effective_add conf base sfam scpl in
          let wl = all_checks_family conf base fam cpl in
          do base.commit_patches ();
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
          do if fam.fam_index <> Adef.ifam_of_int (-1) then
               do effective_del conf base fam;
                  base.commit_patches ();
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
        let digest = Update.digest_family (foi base sfam.fam_index) in
        if digest = get conf.env "digest" then
          if ext then UpdateFam.print_mod1 conf base sfam scpl digest
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
    do base.commit_patches ();
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
          let p = base.persons.get ip in
          try
            do effective_swi conf base p (Adef.ifam_of_int ifam);
               base.commit_patches ();
               print_swi_ok conf base p;
            return ()
          with [ Update.ModErr -> () ]
      | _ -> incorrect_request conf ]
  | Refuse -> Update.error_locked conf base ]
;
