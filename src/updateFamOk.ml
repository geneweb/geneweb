(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: updateFamOk.ml,v 4.6 2001-06-13 08:01:29 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

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

value reconstitute_somebody conf var =
  let first_name = strip_spaces (getn conf var "fn") in
  let surname = strip_spaces (getn conf var "sn") in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let create =
    match getn conf var "p" with
    [ "create" -> Update.Create Neuter None
    | _ -> Update.Link ]
  in
  (first_name, surname, occ, create, var)
;

value reconstitute_parent_or_child conf var default_surname =
  let first_name = getn conf var "fn" in
  let surname =
    let surname = getn conf var "sn" in
    if surname = "" then default_surname else surname
  in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    (b, bpl, d, dpl)
  in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let create =
    match getn conf var "p" with
    [ "create" -> Update.Create sex (Some create_info)
    | _ -> Update.Link ]
  in
  (first_name, surname, occ, create, var)
;

value insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  [ (_, Some n) when n > 1 ->
      let children =
        loop children n where rec loop children n =
          if n > 0 then
            let new_child = ("", "", 0, Update.Create Neuter None, "") in
            loop [new_child :: children] (n - 1)
          else children
      in
      (children, True)
  | (Some "on", _) ->
      let new_child = ("", "", 0, Update.Create Neuter None, "") in
      ([new_child :: children], True)
  | _ -> (children, ext) ]
;

value reconstitute_family conf =
  let ext = False in
  let father = reconstitute_parent_or_child conf "him" "" in
  let mother = reconstitute_parent_or_child conf "her" "" in
  let relation =
    match p_getenv conf.env "mrel" with
    [ Some "not_marr" -> NotMarried
    | Some "engaged" -> Engaged
    | Some "nsck" -> NoSexesCheck
    | _ -> Married ]
  in
  let marriage = Update.reconstitute_date conf "marriage" in
  let marriage_place = only_printable (get conf "marriage_place") in
  let (witnesses, ext) =
    loop 1 ext where rec loop i ext =
      match
        try Some (reconstitute_somebody conf ("witn" ^ string_of_int i)) with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (witnesses, ext) = loop (i + 1) ext in
          match p_getenv conf.env ("ins_witn" ^ string_of_int i) with
          [ Some "on" ->
              let new_witn = ("", "", 0, Update.Create Neuter None, "") in
              ([c; new_witn :: witnesses], True)
          | _ -> ([c :: witnesses], ext) ]
      | None -> ([], ext) ]
  in
  let (witnesses, ext) =
    match p_getenv conf.env "ins_witn0" with
    [ Some "on" ->
        let new_witn = ("", "", 0, Update.Create Neuter None, "") in
        ([new_witn :: witnesses], True)
    | _ -> (witnesses, ext) ]
  in
  let divorce =
    match p_getenv conf.env "divorce" with
    [ Some "not_divorced" -> NotDivorced
    | Some "separated" -> Separated
    | _ ->
        Divorced
          (Adef.codate_of_od (Update.reconstitute_date conf "divorce")) ]
  in
  let surname = getn conf "him" "sn" in
  let (children, ext) =
    loop 1 ext where rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i)
               surname)
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (children, ext) = loop (i + 1) ext in
          let (children, ext) = insert_child conf (children, ext) i in
          ([c :: children], ext)
      | None -> ([], ext) ]
  in
  let (children, ext) = insert_child conf (children, ext) 0 in
  let comment = strip_spaces (get conf "comment") in
  let fsources = strip_spaces (get conf "src") in
  let origin_file =
    match p_getenv conf.env "origin_file" with
    [ Some x -> x
    | None -> "" ]
  in
  let fam_index =
    match p_getint conf.env "i" with
    [ Some i -> i
    | None -> 0 ]
  in
  let fam =
    {marriage = Adef.codate_of_od marriage; marriage_place = marriage_place;
     marriage_src = strip_spaces (get conf "marr_src");
     witnesses = Array.of_list witnesses; relation = relation;
     divorce = divorce; comment = comment; origin_file = origin_file;
     fsources = fsources; fam_index = Adef.ifam_of_int fam_index}
  and cpl = {father = father; mother = mother}
  and des = {children = Array.of_list children} in
  (fam, cpl, des, ext)
;

value strip_array_persons pl =
  let pl =
    List.fold_right
      (fun ((f, s, o, c, _) as p) pl -> if f = "" then pl else [p :: pl])
      (Array.to_list pl) []
  in
  Array.of_list pl
;

value strip_family fam des =
  do {
    des.children := strip_array_persons des.children;
    fam.witnesses := strip_array_persons fam.witnesses
  }
;

value print_err_parents conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "\n";
    Wserver.wprint (fcapitale (ftransl conf "%t already has parents"))
      (fun _ -> afficher_personne_referencee conf base p);
    Wserver.wprint "\n";
    html_p conf;
    tag "ul" begin
      html_li conf;
      Wserver.wprint "%s: %d" (capitale (transl conf "first free number"))
        (Update.find_free_occ base (p_first_name base p) (p_surname base p)
           0);
    end;
    Update.print_return conf;
    trailer conf;
    raise Update.ModErr
  }
;

value print_err_father_sex conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    afficher_personne_referencee conf base p;
    Wserver.wprint "\n%s\n" (transl conf "should be male");
    Update.print_return conf;
    trailer conf;
    raise Update.ModErr
  }
;

value print_err_mother_sex conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    afficher_personne_referencee conf base p;
    Wserver.wprint "\n%s\n" (transl conf "should be female");
    Update.print_return conf;
    trailer conf;
    raise Update.ModErr
  }
;

value print_err conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Update.print_return conf;
    trailer conf;
    raise Update.ModErr
  }
;

value print_error_disconnected conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Util.print_link_to_welcome conf True;
    Wserver.wprint "\
Sorry, you can add only families connected to the rest.<br>
This restriction has been added by this data base owner.
<p>
Désolé, vous ne pouvez ajouter que des familles connectées au reste.<br>
Cette restriction a été ajoutée par le propriétaire de cette base de
données.
";
    trailer conf;
    raise Update.ModErr
  }
;

value family_exclude pfams efam =
  let pfaml =
    List.fold_right
      (fun fam faml -> if fam == efam then faml else [fam :: faml])
      (Array.to_list pfams) []
  in
  Array.of_list pfaml
;

value infer_origin_file_from_other_marriages conf base ifam ip =
  let u = uoi base ip in
  let rec loop i =
    if i = Array.length u.family then None
    else if u.family.(i) = ifam then loop (i + 1)
    else
      let r = (foi base u.family.(i)).origin_file in
      if sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0
;

value infer_origin_file conf base ifam ncpl ndes =
  let r = infer_origin_file_from_other_marriages conf base ifam ncpl.father in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages conf base ifam ncpl.mother
    else r
  in
  let r =
    match r with
    [ Some r -> r
    | None ->
        let afath = aoi base ncpl.father in
        let amoth = aoi base ncpl.mother in
        match (afath.parents, amoth.parents) with
        [ (Some if1, _) when sou base (foi base if1).origin_file <> "" ->
            (foi base if1).origin_file
        | (_, Some if2) when sou base (foi base if2).origin_file <> "" ->
            (foi base if2).origin_file
        | _ ->
            let rec loop i =
              if i == Array.length ndes.children then
                Update.insert_string base ""
              else
                let cifams = (uoi base ndes.children.(i)).family in
                if Array.length cifams == 0 then loop (i + 1)
                else if sou base (foi base cifams.(0)).origin_file <> "" then
                  (foi base cifams.(0)).origin_file
                else loop (i + 1)
            in
            loop 0 ] ]
  in
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
    [ Not_found -> False ]
  in
  if no_dec && sou base r = "" then print_error_disconnected conf
  else r
;

value update_related_witnesses base ofam_witn nfam_witn ncpl =
  let mod_ippl = [] in
  let mod_ippl =
    List.fold_left
      (fun ippl ip ->
         if List.memq ip ofam_witn then ippl
         else
           let p = poi base ip in
           if not (List.mem ncpl.father p.related) then do {
             p.related := [ncpl.father :: p.related];
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           }
           else ippl)
      mod_ippl nfam_witn
  in
  let mod_ippl =
    List.fold_left
      (fun ippl ip ->
         if List.memq ip nfam_witn then ippl
         else
           let p = poi base ip in
           if List.mem ncpl.father p.related then do {
             p.related := List.filter ( \<> ncpl.father) p.related;
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           }
           else ippl)
      mod_ippl ofam_witn
  in
  List.iter (fun (ip, p) -> base.func.patch_person ip p) mod_ippl
;

value effective_mod conf base sfam scpl sdes =
  let fi = sfam.fam_index in
  let ofam = foi base fi in
  let ocpl = coi base fi in
  let odes = doi base fi in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
    [ Some s -> strip_spaces s
    | None -> "" ]
  in
  let ncpl =
    map_couple_p (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Update.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath = poi base ncpl.father in
  let nmoth = poi base ncpl.mother in
  let nfath_u = uoi base ncpl.father in
  let nmoth_u = uoi base ncpl.mother in
  do {
    if sfam.relation <> NoSexesCheck then do {
      match nfath.sex with
      [ Female -> print_err_father_sex conf base nfath
      | _ -> nfath.sex := Male ];
      match nmoth.sex with
      [ Male -> print_err_mother_sex conf base nmoth
      | _ -> nmoth.sex := Female ]
    }
    else if ncpl.father == ncpl.mother then print_err conf base
    else ();
    if sfam.origin_file = "" then 
      nfam.origin_file :=
        if sou base ofam.origin_file <> "" then ofam.origin_file
        else infer_origin_file conf base fi ncpl ndes
    else ();
    nfam.fam_index := fi;
    base.func.patch_family fi nfam;
    base.func.patch_couple fi ncpl;
    base.func.patch_descend fi ndes;
    if ncpl.father != ocpl.father && ncpl.father != ocpl.mother then do {
      let ofath_u = uoi base ocpl.father in
      ofath_u.family := family_exclude ofath_u.family ofam.fam_index;
      nfath_u.family := Array.append nfath_u.family [| fi |];
      base.func.patch_union ocpl.father ofath_u;
      base.func.patch_union ncpl.father nfath_u
    }
    else ();
    if ncpl.mother != ocpl.mother && ncpl.mother != ocpl.father then do {
      let omoth_u = uoi base ocpl.mother in
      omoth_u.family := family_exclude omoth_u.family ofam.fam_index;
      nmoth_u.family := Array.append nmoth_u.family [| fi |];
      base.func.patch_union ocpl.mother omoth_u;
      base.func.patch_union ncpl.mother nmoth_u
    }
    else ();
    let find_asc =
      let cache = Hashtbl.create 101 in
      fun ip ->
        try Hashtbl.find cache ip with
        [ Not_found ->
            let a = aoi base ip in
            do { Hashtbl.add cache ip a; a } ]
    in
    let same_parents =
      ncpl.father = ocpl.father && ncpl.mother = ocpl.mother
    in
    Array.iter
      (fun ip ->
         let a = find_asc ip in
         do {
           a.parents := None;
           if not (array_memq ip ndes.children) then
             a.consang := Adef.fix (-1)
           else ()
         })
      odes.children;
    Array.iter
      (fun ip ->
         let a = find_asc ip in
         match a.parents with
         [ Some _ -> print_err_parents conf base (poi base ip)
         | None ->
             do {
               a.parents := Some fi;
               if not (array_memq ip odes.children) || not same_parents then
                 a.consang := Adef.fix (-1)
               else ()
             } ])
      ndes.children;
    Array.iter
      (fun ip ->
         if not (array_memq ip ndes.children) then
           base.func.patch_ascend ip (find_asc ip)
         else ())
      odes.children;
    Array.iter
      (fun ip ->
         if not (array_memq ip odes.children) || not same_parents then
           base.func.patch_ascend ip (find_asc ip)
         else ())
      ndes.children;
    Update.add_misc_names_for_new_persons base created_p.val;
    Update.update_misc_names_of_family base nfath nfath_u;
    update_related_witnesses base (Array.to_list ofam.witnesses)
      (Array.to_list nfam.witnesses) ncpl;
    (nfam, ncpl, ndes)
  }
;

value effective_add conf base sfam scpl sdes =
  let fi = Adef.ifam_of_int base.data.families.len in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
    [ Some s -> strip_spaces s
    | None -> "" ]
  in
  let ncpl =
    map_couple_p (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Update.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let origin_file = infer_origin_file conf base fi ncpl ndes in
  let nfath_p = poi base ncpl.father in
  let nmoth_p = poi base ncpl.mother in
  let nfath_u = uoi base ncpl.father in
  let nmoth_u = uoi base ncpl.mother in
  do {
    if sfam.relation <> NoSexesCheck then do {
      match nfath_p.sex with
      [ Female -> print_err_father_sex conf base nfath_p
      | Male -> ()
      | _ ->
          do {
            nfath_p.sex := Male; base.func.patch_person ncpl.father nfath_p
          } ];
      match nmoth_p.sex with
      [ Male -> print_err_mother_sex conf base nmoth_p
      | Female -> ()
      | _ ->
          do {
            nmoth_p.sex := Female; base.func.patch_person ncpl.mother nmoth_p
          } ]
    }
    else if ncpl.father == ncpl.mother then print_err conf base
    else ();
    nfam.fam_index := fi;
    nfam.origin_file := origin_file;
    base.func.patch_family fi nfam;
    base.func.patch_couple fi ncpl;
    base.func.patch_descend fi ndes;
    nfath_u.family := Array.append nfath_u.family [| fi |];
    nmoth_u.family := Array.append nmoth_u.family [| fi |];
    base.func.patch_union ncpl.father nfath_u;
    base.func.patch_union ncpl.mother nmoth_u;
    Array.iter
      (fun ip ->
         let a = aoi base ip in
         let p = poi base ip in
         match a.parents with
         [ Some _ -> print_err_parents conf base p
         | None ->
             do {
               a.parents := Some fi;
               a.consang := Adef.fix (-1);
               base.func.patch_ascend p.cle_index a
             } ])
      ndes.children;
    Update.add_misc_names_for_new_persons base created_p.val;
    Update.update_misc_names_of_family base nfath_p nfath_u;
    update_related_witnesses base [] (Array.to_list nfam.witnesses) ncpl;
    (nfam, ncpl, ndes)
  }
;

value effective_swi conf base ip u ifam =
  let rec loop =
    fun
    [ [ifam1; ifam2 :: ifaml] ->
        if ifam2 = ifam then [ifam2; ifam1 :: ifaml]
        else [ifam1 :: loop [ifam2 :: ifaml]]
    | _ -> do { incorrect_request conf; raise Update.ModErr } ]
  in
  do {
    u.family := Array.of_list (loop (Array.to_list u.family));
    base.func.patch_union ip u
  }
;

value kill_family base fam ip =
  let u = uoi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam == fam.fam_index then ifaml else [ifam :: ifaml])
      (Array.to_list u.family) []
  in
  do { u.family := Array.of_list l; base.func.patch_union ip u }
;

value kill_parents base ip =
  let a = aoi base ip in
  do {
    a.parents := None; a.consang := Adef.fix (-1); base.func.patch_ascend ip a
  }
;

value effective_del conf base fam =
  let ifam = fam.fam_index in
  let cpl = coi base ifam in
  let des = doi base ifam in
  do {
    kill_family base fam cpl.father;
    kill_family base fam cpl.mother;
    Array.iter (kill_parents base) des.children;
    cpl.father := Adef.iper_of_int (-1);
    cpl.mother := Adef.iper_of_int (-1);
    fam.witnesses := [| |];
    des.children := [| |];
    fam.comment := Update.insert_string base "";
    fam.fam_index := Adef.ifam_of_int (-1);
    base.func.patch_family ifam fam;
    base.func.patch_couple ifam cpl;
    base.func.patch_descend ifam des
  }
;

value all_checks_family conf base fam cpl des =
  let wl = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  do {
    Gutil.check_noloop_for_person_list base error [cpl.father; cpl.mother];
    Gutil.check_family base error warning fam cpl des;
    List.rev wl.val
  }
;

value print_family conf base wl cpl des =
  let rdsrc =
    match p_getenv conf.env "rdsrc" with
    [ Some "on" -> p_getenv conf.env "src"
    | _ -> p_getenv conf.env "dsrc" ]
  in
  do {
    match rdsrc with
    [ Some x ->
        do {
          conf.henv := List.remove_assoc "dsrc" conf.henv;
          if x <> "" then conf.henv := [("dsrc", code_varenv x) :: conf.henv]
          else ()
        }
    | None -> () ];
    Wserver.wprint "<ul>\n";
    html_li conf;
    afficher_personne_referencee conf base (poi base cpl.father);
    Wserver.wprint "\n";
    html_li conf;
    afficher_personne_referencee conf base (poi base cpl.mother);
    Wserver.wprint "</ul>\n";
    if des.children <> [| |] then do {
      html_p conf;
      Wserver.wprint "<ul>\n";
      Array.iter
        (fun ip ->
           do {
             html_li conf;
             afficher_personne_referencee conf base (poi base ip);
             Wserver.wprint "\n"
           })
        des.children;
      Wserver.wprint "</ul>\n"
    }
    else ();
    Update.print_warnings conf base wl
  }
;

value print_mod_ok conf base wl cpl des =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family modified"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    print_family conf base wl cpl des;
    trailer conf
  }
;

value print_add_ok conf base wl cpl des =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "family added")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    print_family conf base wl cpl des;
    trailer conf
  }
;

value print_del_ok conf base wl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family deleted"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    match p_getint conf.env "ip" with
    [ Some i ->
        let p = base.data.persons.get i in
        tag "ul" begin
          Wserver.wprint "<li>\n";
          Wserver.wprint "%s\n"
            (reference conf base p (person_text conf base p));
        end
    | _ -> () ];
    Update.print_warnings conf base wl;
    trailer conf
  }
;

value print_swi_ok conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "switch done")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    afficher_personne_referencee conf base p;
    Wserver.wprint "\n";
    trailer conf
  }
;

value delete_topological_sort conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab" in
  try Sys.remove tstab_file with [ Sys_error _ -> () ]
;

value get_create (_, _, _, create, _) = create;

value forbidden_disconnected conf sfam scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
    [ Not_found -> False ]
  in
  if no_dec then
    if get_create scpl.father = Update.Link ||
       get_create scpl.mother = Update.Link then
      False
    else
      List.for_all (fun p -> get_create p <> Update.Link)
        (Array.to_list sdes.children)
  else False
;

value print_add o_conf base =
  let conf = Update.update_conf o_conf in
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock Iobase.lock_file bfile with
  [ Accept ->
      try
        let (sfam, scpl, sdes, ext) = reconstitute_family conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some _ -> True
          | _ -> False ]
        in
        if ext || redisp then
          UpdateFam.print_add1 conf base sfam scpl sdes False
        else if forbidden_disconnected conf sfam scpl sdes then
          print_error_disconnected conf
        else do {
          strip_family sfam sdes;
          let (fam, cpl, des) = effective_add conf base sfam scpl sdes in
          let wl = all_checks_family conf base fam cpl des in
          let ((fn, sn, occ, _, _), act) =
            match p_getint conf.env "ip" with
            [ Some i ->
                if Adef.int_of_iper cpl.mother = i then (scpl.mother, "af")
                else
                  let a = base.data.ascends.get i in
                  match a.parents with
                  [ Some x when x = fam.fam_index ->
                      let p = base.data.persons.get i in
                      let key =
                        (sou base p.first_name, sou base p.surname, p.occ,
                         Update.Link, "")
                      in
                      (key, "aa")
                  | _ -> (scpl.father, "af") ]
            | _ -> (scpl.father, "af") ]
          in
          base.func.commit_patches ();
          History.record conf base (fn, sn, occ) act;
          delete_topological_sort conf base;
          print_add_ok conf base wl cpl des
        }
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_del conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock Iobase.lock_file bfile with
  [ Accept ->
      match p_getint conf.env "i" with
      [ Some i ->
          let fam = foi base (Adef.ifam_of_int i) in
          let k =
            let cpl = coi base (Adef.ifam_of_int i) in
            let ip =
              match p_getint conf.env "ip" with
              [ Some i when Adef.int_of_iper cpl.mother = i -> cpl.mother
              | _ -> cpl.father ]
            in
            let p = poi base ip in
            (sou base p.first_name, sou base p.surname, p.occ)
          in
          do {
            if not (is_deleted_family fam) then do {
              effective_del conf base fam;
              base.func.commit_patches ();
              History.record conf base k "df";
              delete_topological_sort conf base
            }
            else ();
            print_del_ok conf base []
          }
      | _ -> incorrect_request conf ]
  | Refuse -> Update.error_locked conf base ]
;

value print_mod_aux conf base callback =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock Iobase.lock_file bfile with
  [ Accept ->
      try
        let (sfam, scpl, sdes, ext) = reconstitute_family conf in
        let redisp =
          match p_getenv conf.env "return" with
          [ Some _ -> True
          | _ -> False ]
        in
        let digest =
          Update.digest_family (foi base sfam.fam_index)
            (coi base sfam.fam_index) (doi base sfam.fam_index)
        in
        if digest = raw_get conf "digest" then
          if ext || redisp then
            UpdateFam.print_mod1 conf base sfam scpl sdes digest
          else do { strip_family sfam sdes; callback sfam scpl sdes }
        else Update.error_digest conf base
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_mod o_conf base =
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let (fam, cpl, des) = effective_mod conf base sfam scpl sdes in
    let wl = all_checks_family conf base fam cpl des in
    let (fn, sn, occ, _, _) =
      match p_getint conf.env "ip" with
      [ Some i when Adef.int_of_iper cpl.mother = i -> scpl.mother
      | _ -> scpl.father ]
    in
    do {
      base.func.commit_patches ();
      History.record conf base (fn, sn, occ) "mf";
      delete_topological_sort conf base;
      print_mod_ok conf base wl cpl des
    }
  in
  print_mod_aux conf base callback
;

value print_swi conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock Iobase.lock_file bfile with
  [ Accept ->
      match (p_getint conf.env "i", p_getint conf.env "f") with
      [ (Some ip, Some ifam) ->
          let p = base.data.persons.get ip in
          let u = base.data.unions.get ip in
          let k = (sou base p.first_name, sou base p.surname, p.occ) in
          try
            do {
              effective_swi conf base p.cle_index u (Adef.ifam_of_int ifam);
              base.func.commit_patches ();
              History.record conf base k "sf";
              print_swi_ok conf base p
            }
          with
          [ Update.ModErr -> () ]
      | _ -> incorrect_request conf ]
  | Refuse -> Update.error_locked conf base ]
;
