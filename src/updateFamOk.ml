(* camlp4r ./pa_html.cmo *)
(* $Id: updateFamOk.ml,v 5.30 2006-11-15 11:49:48 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Futil;
open Gutil;
open Gwdb;
open Mutil;
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
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname = no_html_tags (only_printable (getn conf var "sn")) in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let create =
    match getn conf var "p" with
    [ "create" -> Update.Create sex None
    | _ -> Update.Link ]
  in
  (first_name, surname, occ, create, var)
;

value reconstitute_parent_or_child conf var default_surname =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname =
    let surname = no_html_tags (only_printable (getn conf var "sn")) in
    if surname = "" then default_surname else surname
  in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let death =
      match p_getenv conf.env (var ^ "d_yyyy") with
      [ Some "+" -> DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | _ -> DontKnowIfDead ]
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    (b, bpl, death, d, dpl)
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

value invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match (p_getenv conf.env var, children) with
  [ (Some "on", [c1 :: children]) -> (c1, [c :: children], True)
  | _ -> (c, children, ext) ]
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

value insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  [ (_, Some n) when n > 1 ->
      let parents =
        loop parents n where rec loop parents n =
          if n > 0 then
            let new_parent = ("", "", 0, Update.Create Neuter None, "") in
            loop [new_parent :: parents] (n - 1)
          else parents
      in
      (parents, True)
  | (Some "on", _) ->
      let new_parent = ("", "", 0, Update.Create Neuter None, "") in
      ([new_parent :: parents], True)
  | _ -> (parents, ext) ]
;

value reconstitute_family conf =
  let ext = False in
  let relation =
    match (p_getenv conf.env "mrel", p_getenv conf.env "nsck") with
    [ (Some "marr", Some "on") -> NoSexesCheckMarried
    | (Some "marr", Some _ | None) -> Married
    | (Some "not_marr", Some "on") -> NoSexesCheckNotMarried
    | (Some "not_marr", Some _ | None) -> NotMarried
    | (Some "engaged", _) -> Engaged
    | (Some "nsck", _) -> NoSexesCheckNotMarried
    | (Some "nsckm", _) -> NoSexesCheckMarried
    | (Some "no_ment", _) -> NoMention
    | _ -> Married ]
  in
  let marriage = Update.reconstitute_date conf "marr" in
  let marriage_place = only_printable (get conf "marr_place") in
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
    match p_getenv conf.env "div" with
    [ Some "not_divorced" -> NotDivorced
    | Some "separated" -> Separated
    | _ ->
        Divorced (Adef.codate_of_od (Update.reconstitute_date conf "div")) ]
  in
  let surname = getn conf "pa1" "sn" in
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
          let (c, children, ext) = invert_children conf (c, children, ext) i in
          let (children, ext) = insert_child conf (children, ext) i in
          ([c :: children], ext)
      | None -> ([], ext) ]
  in
  let (children, ext) = insert_child conf (children, ext) 0 in
  let (parents, ext) =
    loop 1 ext where rec loop i ext =
      match
        try
          Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (parents, ext) = loop (i + 1) ext in
          let (parents, ext) = insert_parent conf (parents, ext) i in
          ([c :: parents], ext)
      | None -> ([], ext) ]
  in
  let comment = only_printable (get conf "comment") in
  let fsources = only_printable (get conf "src") in
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
  and cpl = parent conf.multi_parents (Array.of_list parents)
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
  let fam = {(fam) with witnesses = strip_array_persons fam.witnesses} in
  let des = {children = strip_array_persons des.children} in
  (fam, des)
;

value print_err_parents conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "\n";
    Wserver.wprint (fcapitale (ftransl conf "%t already has parents"))
      (fun _ -> Wserver.wprint "\n%s" (referenced_person_text conf base p));
    Wserver.wprint "\n";
    html_p conf;
    tag "ul" begin
      html_li conf;
      Wserver.wprint "%s: %d" (capitale (transl conf "first free number"))
        (Gutil.find_free_occ base (p_first_name base p) (p_surname base p)
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
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
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
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
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
This restriction has been added by this database owner.
<p>
D&eacute;sol&eacute;, vous ne pouvez ajouter que des familles
connect&eacute;es au reste.<br>
Cette restriction a &eacute;t&eacute; ajout&eacute;e par le
propri&eacute;taire de cette base de donn&eacute;es.
";
    trailer conf;
    raise Update.ModErr
  }
;

value family_exclude pfams efam =
  let pfaml =
    List.fold_right
      (fun fam faml -> if fam = efam then faml else [fam :: faml])
      (Array.to_list pfams) []
  in
  Array.of_list pfaml
;

value infer_origin_file_from_other_marriages conf base ifam ip =
  let u = uoi base ip in
  let ufams = get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = get_origin_file (foi base ufams.(i)) in
      if sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0
;

value infer_origin_file conf base ifam ncpl ndes =
  let r =
    infer_origin_file_from_other_marriages conf base ifam (get_father ncpl)
  in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages conf base ifam (get_mother ncpl)
    else r
  in
  let r =
    match r with
    [ Some r -> r
    | None ->
        let afath = aoi base (get_father ncpl) in
        let amoth = aoi base (get_mother ncpl) in
        match (get_parents afath, get_parents amoth) with
        [ (Some if1, _)
          when sou base (get_origin_file (foi base if1)) <> "" ->
            (get_origin_file (foi base if1))
        | (_, Some if2)
          when sou base (get_origin_file (foi base if2)) <> "" ->
            (get_origin_file (foi base if2))
        | _ ->
            let rec loop i =
              if i = Array.length (get_children ndes) then
                Gwdb.insert_string base ""
              else
                let cifams = get_family (uoi base (get_children ndes).(i)) in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  sou base (get_origin_file (foi base cifams.(0))) <> ""
                then
                  get_origin_file (foi base cifams.(0))
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
         if List.mem ip ofam_witn then ippl
         else
           let p = poi base ip in
           if not (List.mem (get_father ncpl) (get_related p)) then
             let p =
               person_with_related p [get_father ncpl :: get_related p]
             in
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           else ippl)
      mod_ippl nfam_witn
  in
  let mod_ippl =
    List.fold_left
      (fun ippl ip ->
         if List.mem ip nfam_witn then ippl
         else
           let p = try List.assoc ip ippl with [ Not_found -> poi base ip ] in
           if List.mem (get_father ncpl) (get_related p) then
             let p =
               person_with_related p
                 (List.filter ( \<> (get_father ncpl)) (get_related p))
             in
             if List.mem_assoc ip ippl then ippl else [(ip, p) :: ippl]
           else ippl)
      mod_ippl ofam_witn
  in
  List.iter (fun (ip, p) -> patch_person base ip p) mod_ippl
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
    couple_of_gen_couple base
      (map_couple_p conf.multi_parents
         (Update.insert_person conf base psrc created_p) scpl)
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    descend_of_gen_descend base
      (map_descend_p (Update.insert_person conf base psrc created_p) sdes)
  in
  let nfath = poi base (get_father ncpl) in
  let nmoth = poi base (get_mother ncpl) in
  let nfath_u = uoi base (get_father ncpl) in
  let (nfath, nmoth) =
    if sfam.relation <> NoSexesCheckNotMarried &&
       sfam.relation <> NoSexesCheckMarried then
      let nfath =
        match get_sex nfath with
        [ Female -> print_err_father_sex conf base nfath
        | Male -> nfath
        | Neuter -> do {
            let nfath = person_with_sex nfath Male in
            patch_person base (get_key_index nfath) nfath;
            nfath
          } ]
      in
      let nmoth =
        match get_sex nmoth with
        [ Male -> print_err_mother_sex conf base nmoth
        | Female -> nmoth
        | Neuter -> do {
            let nmoth = person_with_sex nmoth Female in
            patch_person base (get_key_index nmoth) nmoth;
            nmoth
          } ]
      in
      (nfath, nmoth)
    else (nfath, nmoth)
  in
  do {
    if get_father ncpl = get_mother ncpl then print_err conf base else ();
    let nfam =
      let origin_file =
        if sfam.origin_file = "" then 
          if sou base (get_origin_file ofam) <> "" then
            get_origin_file ofam
          else infer_origin_file conf base fi ncpl ndes
        else nfam.origin_file
      in
      family_with_origin_file (family_of_gen_family base nfam) origin_file fi
    in
    patch_family base fi nfam;
    patch_couple base fi ncpl;
    patch_descend base fi ndes;
    let oarr = get_parent_array ocpl in
    let narr = get_parent_array ncpl in
    for i = 0 to Array.length oarr - 1 do {
      if not (array_mem oarr.(i) narr) then do {
        let ou = uoi base oarr.(i) in
        let ou =
          union_of_gen_union base
            {family = family_exclude (get_family ou) fi}
        in
        patch_union base oarr.(i) ou
      }
      else ()
    };
    for i = 0 to Array.length narr - 1 do {
      if not (array_mem narr.(i) oarr) then do {
        let nu = uoi base narr.(i) in
        let nu =
          union_of_gen_union base
            {family = Array.append (get_family nu) [| fi |]}
        in
        patch_union base narr.(i) nu;
      }
      else ()
    };
    let cache = Hashtbl.create 101 in
    let find_asc =
      fun ip ->
        try Hashtbl.find cache ip with
        [ Not_found ->
            let a = aoi base ip in
            do { Hashtbl.add cache ip a; a } ]
    in
    let same_parents =
      get_father ncpl = get_father ocpl && get_mother ncpl = get_mother ocpl
    in
    Array.iter
      (fun ip ->
         let a = find_asc ip in
         let a =
           ascend_of_gen_ascend base
             {parents = None;
              consang =
                if not (array_mem ip (get_children ndes)) then Adef.fix (-1)
                else get_consang a}
         in
         Hashtbl.replace cache ip a)
      (get_children odes);
    Array.iter
      (fun ip ->
         let a = find_asc ip in
         match get_parents a with
         [ Some _ -> print_err_parents conf base (poi base ip)
         | None ->
             let a =
               ascend_of_gen_ascend base
                 {parents = Some fi;
                  consang =
                    if not (array_mem ip (get_children odes)) ||
                       not same_parents
                    then Adef.fix (-1)
                    else get_consang a}
             in
             Hashtbl.replace cache ip a ])
      (get_children ndes);
    Array.iter
      (fun ip ->
         if not (array_mem ip (get_children ndes)) then
           patch_ascend base ip (find_asc ip)
         else ())
      (get_children odes);
    Array.iter
      (fun ip ->
         if not (array_mem ip (get_children odes)) || not same_parents then
           patch_ascend base ip (find_asc ip)
         else ())
      (get_children ndes);
    Update.add_misc_names_for_new_persons conf base created_p.val;
    Update.update_misc_names_of_family conf base nfath nfath_u;
    update_related_witnesses base (Array.to_list (get_witnesses ofam))
      (Array.to_list (get_witnesses nfam)) ncpl;
    (fi, nfam, ncpl, ndes)
  }
;

value effective_add conf base sfam scpl sdes =
  let fi = Adef.ifam_of_int (nb_of_families base) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
    [ Some s -> strip_spaces s
    | None -> "" ]
  in
  let ncpl =
    couple_of_gen_couple base
      (map_couple_p conf.multi_parents
         (Update.insert_person conf base psrc created_p) scpl)
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    descend_of_gen_descend base
      (map_descend_p (Update.insert_person conf base psrc created_p) sdes)
  in
  let origin_file = infer_origin_file conf base fi ncpl ndes in
  let nfath_p = poi base (get_father ncpl) in
  let nmoth_p = poi base (get_mother ncpl) in
  let nfath_u = uoi base (get_father ncpl) in
  let nmoth_u = uoi base (get_mother ncpl) in
  let (nfath_p, nmoth_p) =
    if sfam.relation <> NoSexesCheckNotMarried &&
       sfam.relation <> NoSexesCheckMarried then
      let nfath_p =
        match get_sex nfath_p with
        [ Female -> print_err_father_sex conf base nfath_p
        | Male -> nfath_p
        | _ -> do {
            let nfath_p = person_with_sex nfath_p Male in
            patch_person base (get_father ncpl) nfath_p;
            nfath_p
          } ]
      in
      let nmoth_p =
        match get_sex nmoth_p with
        [ Male -> print_err_mother_sex conf base nmoth_p
        | Female -> nmoth_p
        | _ -> do {
            let nmoth_p = person_with_sex nmoth_p Female in
            patch_person base (get_mother ncpl) nmoth_p;
            nmoth_p
          } ]
      in
      (nfath_p, nmoth_p)
    else if get_father ncpl = get_mother ncpl then print_err conf base
    else (nfath_p, nmoth_p)
  in
  let nfam =
    family_with_origin_file (family_of_gen_family base nfam) origin_file fi
  in
  do {
    patch_family base fi nfam;
    patch_couple base fi ncpl;
    patch_descend base fi ndes;
    let nfath_u =
      union_of_gen_union base
        {family = Array.append (get_family nfath_u) [| fi |]}
    in
    let nmoth_u =
      union_of_gen_union base
        {family = Array.append (get_family nmoth_u) [| fi |]}
    in
    patch_union base (get_father ncpl) nfath_u;
    patch_union base (get_mother ncpl) nmoth_u;
    Array.iter
      (fun ip ->
         let a = aoi base ip in
         let p = poi base ip in
         match get_parents a with
         [ Some _ -> print_err_parents conf base p
         | None ->
             let a =
               ascend_of_gen_ascend base
                 {parents = Some fi; consang = Adef.fix (-1)}
             in
             patch_ascend base (get_key_index p) a ])
      (get_children ndes);
    Update.add_misc_names_for_new_persons conf base created_p.val;
    Update.update_misc_names_of_family conf base nfath_p nfath_u;
    update_related_witnesses base [] (Array.to_list (get_witnesses nfam))
      ncpl;
    (fi, nfam, ncpl, ndes)
  }
;

value effective_inv conf base ip u ifam =
  let rec loop =
    fun
    [ [ifam1; ifam2 :: ifaml] ->
        if ifam2 = ifam then [ifam2; ifam1 :: ifaml]
        else [ifam1 :: loop [ifam2 :: ifaml]]
    | _ -> do { incorrect_request conf; raise Update.ModErr } ]
  in
  let u =
    union_of_gen_union base
      {family = Array.of_list (loop (Array.to_list (get_family u)))}
  in
  patch_union base ip u
;

value kill_family base ifam1 ip =
  let u = uoi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam = ifam1 then ifaml else [ifam :: ifaml])
      (Array.to_list (get_family u)) []
  in
  let u = union_of_gen_union base {family = Array.of_list l} in
  patch_union base ip u
;

value kill_parents base ip =
  let a = no_ascend base in
  patch_ascend base ip a
;

value effective_del conf base (ifam, fam) = do {
  let cpl = coi base ifam in
  let des = doi base ifam in
  kill_family base ifam (get_father cpl);
  kill_family base ifam (get_mother cpl);
  Array.iter (kill_parents base) (get_children des);
  let cpl =
    couple_of_gen_couple base
      (couple False (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)))
  in
  let fam =
    family_of_gen_family base
      {(gen_family_of_family fam) with
       witnesses = [| |];
       comment = Gwdb.insert_string base "";
       fam_index = Adef.ifam_of_int (-1)}
  in
  let des = descend_of_gen_descend base {children = [| |]} in
  patch_family base ifam fam;
  patch_couple base ifam cpl;
  patch_descend base ifam des
};

value array_forall2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then invalid_arg "array_forall2"
  else
    loop 0 where rec loop i =
      if i = Array.length a1 then True
      else if f a1.(i) a2.(i) then loop (i + 1)
      else False
;

value array_exists f a =
  loop 0 where rec loop i =
    if i = Array.length a then False
    else if f a.(i) then True
    else loop (i + 1)
;

value is_a_link =
  fun
  [ (_, _, _, Update.Link, _) -> True
  | _ -> False ]
;

value is_created_or_already_there ochil_arr nchil schil =
  not (is_a_link schil) || array_mem nchil ochil_arr
;

(* need_check_noloop: optimization
     The no-loop check being a big work on large databases, this
   optimization tests if this is really necessary or not. It is not
   necessary if:
   1/ either all parents are created,
   2/ or all children are created,
   3/ or the new family have the same parents than the old one *and*
      all linked (not created) new children were already children.
*)

value need_check_noloop (scpl, sdes, onfs) =
  if array_exists is_a_link (parent_array scpl) &&
     array_exists is_a_link sdes.children
  then
    match onfs with
    [ Some ((opar, ochil), (npar, nchil)) ->
        not
          (array_forall2 (is_created_or_already_there opar) npar
             (parent_array scpl)) ||
        not
          (array_forall2 (is_created_or_already_there ochil) nchil
             sdes.children)
    | None -> True ]
  else False
;

value all_checks_family conf base ifam fam cpl des scdo =
  let wl = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  do {
    if need_check_noloop scdo then
      Consang.check_noloop_for_person_list base error
        (Array.to_list (get_parent_array cpl))
    else ();
    CheckItem.family base error warning ifam fam cpl des;
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
    Wserver.wprint "\n%s"
      (referenced_person_text conf base (poi base (get_father cpl)));
    Wserver.wprint "\n";
    html_li conf;
    Wserver.wprint "\n%s"
      (referenced_person_text conf base (poi base (get_mother cpl)));
    Wserver.wprint "</ul>\n";
    if get_children des <> [| |] then do {
      html_p conf;
      Wserver.wprint "<ul>\n";
      Array.iter
        (fun ip ->
           do {
             html_li conf;
             Wserver.wprint "\n%s"
               (referenced_person_text conf base (poi base ip));
             Wserver.wprint "\n"
           })
        (get_children des);
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
        let p = poi base (Adef.iper_of_int i) in
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

value print_inv_ok conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "inversion done"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
    Wserver.wprint "\n";
    trailer conf
  }
;

value get_create (_, _, _, create, _) = create;

value forbidden_disconnected conf sfam scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
    [ Not_found -> False ]
  in
  if no_dec then
    if get_create (father scpl) = Update.Link ||
       get_create (mother scpl) = Update.Link then
      False
    else
      List.for_all (fun p -> get_create p <> Update.Link)
        (Array.to_list sdes.children)
  else False
;

value print_add o_conf base =
  let conf = Update.update_conf o_conf in
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf in
    let redisp =
      match p_getenv conf.env "return" with
      [ Some _ -> True
      | _ -> False ]
    in
    let digest =
      match p_getint conf.env "ip" with
      [ Some ip ->
          string_of_int
            (Array.length (get_family (uoi base (Adef.iper_of_int ip))))
      | None -> "" ]
    in
    let sdigest = raw_get conf "digest" in
    if digest <> "" && sdigest <> "" && digest <> sdigest then
      Update.error_digest conf
    else if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
    else if forbidden_disconnected conf sfam scpl sdes then
      print_error_disconnected conf
    else do {
      let (sfam, sdes) = strip_family sfam sdes in
      let (ifam, fam, cpl, des) = effective_add conf base sfam scpl sdes in
      let wl =
        all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
      in
      let ((fn, sn, occ, _, _), i, act) =
        match p_getint conf.env "ip" with
        [ Some i ->
            if Adef.int_of_iper (get_mother cpl) = i then
              (mother scpl, i, "af")
            else
              let a = aoi base (Adef.iper_of_int i) in
              match get_parents a with
              [ Some x when x = ifam ->
                  let p = poi base (Adef.iper_of_int i) in
                  let key =
                    (sou base (get_first_name p), sou base (get_surname p),
                     get_occ p, Update.Link, "")
                  in
                  (key, i, "aa")
              | _ -> (father scpl, i, "af") ]
        | _ -> (father scpl, -1, "af") ]
      in
      Util.commit_patches conf base;
      History.record conf base (fn, sn, occ, Adef.iper_of_int i) act;
      Update.delete_topological_sort conf base;
      print_add_ok conf base wl cpl des
    }
  with
  [ Update.ModErr -> () ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let ifam = Adef.ifam_of_int i in
      let fam = foi base ifam in
      let k =
        let cpl = coi base ifam in
        let ip =
          match p_getint conf.env "ip" with
          [ Some i when Adef.int_of_iper (get_mother cpl) = i ->
              get_mother cpl
          | _ -> get_father cpl ]
        in
        let p = poi base ip in
        (sou base (get_first_name p), sou base (get_surname p), get_occ p,
         get_key_index p)
      in
      do {
        if not (is_deleted_family fam) then do {
          effective_del conf base (ifam, fam);
          Util.commit_patches conf base;
          History.record conf base k "df";
          Update.delete_topological_sort conf base
        }
        else ();
        print_del_ok conf base []
      }
  | _ -> incorrect_request conf ]
;

value print_mod_aux conf base callback =
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf in
    let redisp =
      match p_getenv conf.env "return" with
      [ Some _ -> True
      | _ -> False ]
    in
    let digest =
      let ini_sfam = UpdateFam.string_family_of conf base sfam.fam_index in
      Update.digest_family ini_sfam
    in
    if digest = raw_get conf "digest" then
      if ext || redisp then
        UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
      else
        let (sfam, sdes) = strip_family sfam sdes in
        callback sfam scpl sdes
    else Update.error_digest conf
  with
  [ Update.ModErr -> () ]
;

value family_structure conf base ifam =
  let cpl = coi base ifam in
  let des = doi base ifam in
  (get_parent_array cpl, get_children des)
;

value print_mod o_conf base =
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let ofs = family_structure conf base sfam.fam_index in
    let (ifam, fam, cpl, des) = effective_mod conf base sfam scpl sdes in
    let nfs = (get_parent_array cpl, get_children des) in
    let onfs = Some (ofs, nfs) in
    let wl =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    let ((fn, sn, occ, _, _), ip) =
      match p_getint conf.env "ip" with
      [ Some i ->
          let ip = Adef.iper_of_int i in
          (if get_mother cpl = ip then mother scpl else father scpl, ip)
      | None -> (father scpl, Adef.iper_of_int (-1)) ]
    in
    do {
      Util.commit_patches conf base;
      History.record conf base (fn, sn, occ, ip) "mf";
      Update.delete_topological_sort conf base;
      print_mod_ok conf base wl cpl des
    }
  in
  print_mod_aux conf base callback
;

value print_inv conf base =
  match (p_getint conf.env "i", p_getint conf.env "f") with
  [ (Some ip, Some ifam) ->
      let p = poi base (Adef.iper_of_int ip) in
      let u = uoi base (Adef.iper_of_int ip) in
      let k =
        (sou base (get_first_name p), sou base (get_surname p), get_occ p,
         get_key_index p)
      in
      try
        do {
          effective_inv conf base (get_key_index p) u (Adef.ifam_of_int ifam);
          Util.commit_patches conf base;
          History.record conf base k "if";
          print_inv_ok conf base p
        }
      with
      [ Update.ModErr -> () ]
  | _ -> incorrect_request conf ]
;
