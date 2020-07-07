open Def
open Gwdb

type patch =
  | Fix_NBDS of iper
  | Fix_AddedUnion of iper
  | Fix_AddedParents of iper
  | Fix_ParentDeleted of iper
  | Fix_AddedChild of ifam
  | Fix_RemovedUnion of iper * ifam
  | Fix_RemovedDuplicateUnion of iper * ifam
  | Fix_AddedRelatedFromPevent of iper * iper
  | Fix_AddedRelatedFromFevent of iper * iper
  | Fix_MarriageDivorce of ifam
  | Fix_MissingSpouse of ifam * iper
  | Fix_WrongUTF8Encoding of Gwdb.ifam option * Gwdb.iper option * (Gwdb.istr * Gwdb.istr) option
  | Fix_UpdatedOcc of iper * int * int

let mk_pevent name date place note src =
  { epers_name = name
  ; epers_date = date
  ; epers_place = place
  ; epers_reason = empty_string
  ; epers_note = note
  ; epers_src = src
  ; epers_witnesses = [||]
  }

let of_pevent e =
  e.epers_date, e.epers_place, e.epers_note, e.epers_src

let find_pevent names pevents =
  List.find_opt (fun x -> List.mem x.epers_name names) pevents

let fix_pevents ?report base pp =
  (* Should it use UpdateIndOk.reconstitute_from_pevents? *)
  let p = gen_person_of_person pp in
  let empty_bi = (Adef.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string) in
  let empty_bp = (Adef.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string) in
  let empty_de = (Adef.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string) in
  let empty_bu = (Adef.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string) in
  let pevents = p.pevents in
  let aux name date place note src empty pevents =
    match find_pevent [ name ] pevents with
    | None ->
      let pevents =
        if (date, place, note, src) <> empty
        then mk_pevent name date place note src :: pevents
        else pevents
      in (date, place, note, src), pevents
    | Some e ->
      let e' =
        { epers_name = e.epers_name
        ; epers_date =
            if e.epers_date = Adef.cdate_None then date else e.epers_date
        ; epers_place =
            if e.epers_place = Gwdb.empty_string then place else e.epers_place
        ; epers_reason = e.epers_reason
        ; epers_note =
            if e.epers_note = Gwdb.empty_string then note else e.epers_note
        ; epers_src =
            if e.epers_src = Gwdb.empty_string then src else e.epers_src
        ; epers_witnesses = e.epers_witnesses
        }
      in
      (of_pevent e', Mutil.list_replace e e' pevents)
  in
  let (birth, birth_place, birth_note, birth_src), pevents =
    aux Epers_Birth p.birth p.birth_place p.birth_note p.birth_src empty_bi pevents
  in
  let (baptism, baptism_place, baptism_note, baptism_src), pevents =
    aux Epers_Baptism p.baptism p.baptism_place p.baptism_note p.baptism_src empty_bp pevents
  in
  let (death, death_place, death_note, death_src), pevents =
    let death =
      match p.death with
      | Death (_, d) -> d
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead -> Adef.cdate_None
    in
    aux Epers_Death death p.death_place p.death_note p.death_src empty_de pevents
  in
  let death =
    match p.death with
    | Death _ -> p.death
    | NotDead when death <> Adef.cdate_None -> Death (Unspecified, death)
    | x -> x
  in
  let (burial, burial_place, burial_note, burial_src), pevents =
    match p.burial with
    | UnknownBurial ->
      begin match find_pevent [ Epers_Burial ; Epers_Cremation ] pevents with
        | None -> (UnknownBurial, p.burial_place, p.burial_note, p.burial_src), pevents
        | Some e ->
          let (bu, bu_place, bu_note, bu_src) = of_pevent e in
          ( begin match e.epers_name with
              | Epers_Burial -> Buried bu
              | Epers_Cremation -> Cremated bu
              | _ -> assert false end
          , bu_place, bu_note, bu_src)
        , pevents
      end
    | Buried d ->
      let (d, bu_place, bu_note, bu_src), pevents =
        aux Epers_Burial d p.burial_place p.burial_note p.burial_src empty_bu pevents
      in
      (Buried d, bu_place, bu_note, bu_src), pevents
    | Cremated d ->
      let (d, bu_place, bu_note, bu_src), pevents =
        aux Epers_Cremation d p.burial_place p.burial_note p.burial_src empty_bu pevents
      in
      (Cremated d, bu_place, bu_note, bu_src), pevents
  in
  let p' =
    { p with birth
           ; birth_place
           ; birth_note
           ; birth_src
           ; baptism
           ; baptism_place
           ; baptism_note
           ; baptism_src
           ; death
           ; death_place
           ; death_note
           ; death_src
           ; burial
           ; burial_place
           ; burial_note
           ; burial_src
           ; pevents
    }
  in
  if p <> p' then begin
    patch_person base p.key_index p' ;
    match report with
    | Some fn -> fn (Fix_NBDS p.key_index)
    | None -> ()
  end

let check_NBDS ?report progress base =
  let nb_ind = nb_of_persons base in
  Gwdb.Collection.iteri begin fun i p ->
    progress i nb_ind ;
    fix_pevents ?report base p
  end (Gwdb.persons base)

let check_families_parents ?report progress base =
  let nb_fam = nb_of_families base in
  Gwdb.Collection.iteri begin fun i fam ->
    progress i nb_fam ;
    let ifam = get_ifam fam in
    Array.iter begin fun ip ->
      let unions = get_family (poi base ip) in
      if not @@ Array.mem ifam unions then begin
        patch_union base ip { family = Array.append unions [|ifam|] } ;
        begin match report with
        | Some fn -> fn (Fix_AddedUnion ip)
        | None -> () end ;
      end
    end (get_parent_array fam)
  end (Gwdb.families base)

let check_families_children ?report progress base =
  let nb_fam = nb_of_families base in
  Gwdb.Collection.iteri begin fun i fam ->
    let ifam = get_ifam fam in
    progress i nb_fam;
    let children = get_children fam in
    for j = 0 to Array.length children - 1 do
      let ip = children.(j) in
      let a = poi base ip in
      let parents = get_parents a in
      if parents = Some dummy_ifam || parents = None then begin
        patch_ascend base ip {parents = Some ifam; consang = get_consang a};
        begin match report with
          | Some fn -> fn (Fix_AddedParents ip)
          | None -> ()
        end ;
      end
      (* else if parents <> Some ifam && verbosity1 then begin
       *   (\* FIXME: what to do here ? *\)
       *   Printf.printf "\tbad parents : %s\n" (string_of_p base ip);
       *   flush stdout
       * end *)
    done
  end (Gwdb.families base)

let check_persons_parents ?report progress base =
  let nb_ind = nb_of_persons base in
  Gwdb.Collection.iteri begin fun i p ->
    progress i nb_ind;
    get_parents p |> Opt.iter @@ fun ifam ->
    let ip = get_iper p in
    let fam = Gwdb.foi base ifam in
    if get_ifam fam = dummy_ifam then begin
      patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
      match report with
      | Some fn -> fn (Fix_ParentDeleted ip)
      | None -> ()
    end else
      let children = get_children fam in
      if not @@ Array.mem ip children then
        begin
          let children = Array.append children [| ip |] in
          patch_descend base ifam {children = children} ;
          match report with
          | Some fn -> fn (Fix_AddedChild ifam)
          | None -> ()
        end
  end (Gwdb.persons base)

let check_persons_families ?report progress base =
  let nb_ind = nb_of_persons base in
  Gwdb.Collection.iteri begin fun i p ->
    progress i nb_ind ;
    let ip = get_iper p in
    let ifams = get_family p in
    let ifams' =
      Array.of_list @@
      Array.fold_right begin fun ifam acc ->
        let cpl = foi base ifam in
        if List.mem ifam acc then begin
          match report with
          | Some fn ->
            fn (Fix_RemovedDuplicateUnion (ip, ifam)) ;
            acc
          | None -> acc
        end else if not @@ Array.mem ip (get_parent_array cpl) then
          match report with
          | Some fn ->
            fn (Fix_RemovedUnion (ip, ifam)) ;
            acc
          | None -> acc
        else ifam :: acc
      end ifams []
    in
    if ifams' <> ifams then patch_union base ip {family = ifams'} ;
  end (Gwdb.persons base)

let check_pevents_witnesses ?report progress base =
  let nb_ind = nb_of_persons base in
  Gwdb.Collection.iteri begin fun i p ->
    progress i nb_ind ;
    let ip = get_iper p in
    List.iter begin fun evt ->
      let witn = Array.map fst evt.epers_witnesses in
      for j = 0 to Array.length witn - 1 do
        let ip2 = witn.(j) in
        let p2 = poi base ip2 in
        if not (List.memq ip (get_related p2)) then
          begin
            patch_person base ip2
              {(gen_person_of_person p2) with related = ip :: get_related p2} ;
            match report with
            | Some fn -> fn (Fix_AddedRelatedFromPevent (ip2, ip)) ;
            | None -> ()
          end
      done
    end (get_pevents p)
  end (Gwdb.persons base)

let check_fevents_witnesses ?report progress base =
  let nb_fam = nb_of_families base in
  Gwdb.Collection.iteri begin fun i fam ->
    progress i nb_fam ;
    let ifath = get_father fam in
    List.iter begin fun evt ->
      let witn = Array.map fst evt.efam_witnesses in
      for j = 0 to Array.length witn - 1 do
        let ip = witn.(j) in
        let p = poi base ip in
        if not (List.memq ifath (get_related p)) then begin
          patch_person base ip {(gen_person_of_person p) with related = ifath :: get_related p} ;
          match report with
          | Some fn -> fn (Fix_AddedRelatedFromFevent (ip, ifath)) ;
          | None -> ()
          end
      done
    end (get_fevents fam)
  end (Gwdb.families base)

let fix_marriage_divorce ?report progress base =
  let nb_fam = nb_of_families base in
  Gwdb.Collection.iteri begin fun i fam ->
    progress i nb_fam ;
    let fevents = get_fevents fam in
    let relation0 = get_relation fam in
    let marriage0 = get_marriage fam in
    let marriage_place0 = get_marriage_place fam in
    let marriage_note0 = get_marriage_note fam in
    let marriage_src0 = get_marriage_src fam in
    let divorce0 = get_divorce fam in
    let marr_data0 = (relation0, marriage0, marriage_place0, marriage_note0, marriage_src0) in
    let (relation, marriage, marriage_place, marriage_note, marriage_src) as marr_data, divorce, _ =
      UpdateFamOk.reconstitute_from_fevents false (insert_string base "") fevents
    in
    if marr_data0 <> marr_data || divorce0 <> divorce then begin
      let fam' =
        { (gen_family_of_family fam)
          with relation ; marriage ; marriage_place ; marriage_note ; marriage_src ; divorce }
      in
      patch_family base (get_ifam fam) fam' ;
      match report with
      | Some fn -> fn (Fix_MarriageDivorce (get_ifam fam))
      | None -> ()
    end
  end (Gwdb.families base)

let fix_missing_spouses ?report progress base =
  let nb_fam = nb_of_families base in
  Gwdb.Collection.iteri begin fun i fam ->
    progress i nb_fam ;
    let aux i =
      let p = poi base i in
      if get_iper p = Gwdb.dummy_iper then begin
        Gwdb.patch_union
          base i { family = [| get_ifam fam |] } ;
        Gwdb.patch_person
          base i { (gen_person_of_person p) with key_index = i } ;
        match report with
        | Some fn -> fn (Fix_MissingSpouse (get_ifam fam, i))
        | None -> ()
      end
    in
    aux @@ get_father fam ;
    aux @@ get_mother fam ;
  end (Gwdb.families base)

let fix_utf8_sequence ?report progress base =
  let normalize_utf_8_date ifam iper s =
    let s' = Mutil.normalize_utf_8 s in
    if s <> s'
    then begin match report with
      | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, None))
      | None -> ()
    end ;
    s'
  in
  let normalize_utf_8 ifam iper i =
    let s = Gwdb.sou base i in
    let s' = Mutil.normalize_utf_8 s in
    let i' = Gwdb.insert_string base s' in
    if i <> i'
    then begin match report with
      | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, Some (i, i')))
      | None -> ()
    end ;
    i'
  in
  let nbf = nb_of_families base in
  let nbp = nb_of_persons base in
  let nb = nbp + nbf in
  let fp i = i in
  let ff i = i in
  let fs ifam iper i = normalize_utf_8 ifam iper i in
  let fd ifam iper = function
    | Dtext d -> Dtext (normalize_utf_8_date ifam iper d)
    | d -> d
  in
  Gwdb.Collection.iteri begin fun i fam ->
    progress i nb ;
    let ifam = Gwdb.get_ifam fam in
    let f = Gwdb.gen_family_of_family fam in
    let f' = Futil.map_family_ps ~fd:(fd (Some ifam) None) fp ff (fs (Some ifam) None) f in
    if f' <> f then Gwdb.patch_family base ifam f' ;
  end (Gwdb.families base) ;
  Gwdb.Collection.iteri begin fun i per ->
    progress (nbf + i) nb ;
    let iper = Gwdb.get_iper per in
    let p = Gwdb.gen_person_of_person per in
    let p' = Futil.map_person_ps ~fd:(fd None (Some iper)) fp (fs None (Some iper)) p in
    if p' <> p then Gwdb.patch_person base iper p' ;
  end (Gwdb.persons base)

let fix_key ?report progress base =
  let nb_ind = nb_of_persons base in
  let ipers = Gwdb.ipers base in
  let skip = Gwdb.iper_marker ipers false in
  Gwdb.Collection.iteri begin fun i ip ->
    progress i nb_ind ;
    let p = poi base ip in
    let f = Gwdb.p_first_name base p in
    let s = Gwdb.p_surname base p in
    if f <> "?" && s <> "?" then begin
      let key = Name.concat f s in
      let ipers = Gwdb.persons_of_name base key in
      let f = Name.lower f in
      let s = Name.lower s in
      let list =
        let rec loop acc = function
          | ip :: tl ->
            let p = poi base ip in
            if Name.lower @@ p_first_name base p = f
            && Name.lower @@ p_surname base p = s
            then loop ((get_iper p, get_occ p) :: acc) tl
            else loop acc tl
          | [] -> acc
        in
        loop [] ipers
      in
      let rev_list = List.sort (fun a b -> compare b a) list in
      let cnt = ref 0 in
      let mem_occ occ acc list =
        List.exists (fun (_, o) -> o = occ) acc
        || List.exists (fun (_, o) -> o = occ) list
      in
      let rec new_occ acc list =
        if mem_occ !cnt acc list then (incr cnt ; new_occ acc list)
        else !cnt
      in
      let rec loop acc list =
        match acc with
        | [] -> begin
            match list with
            | [] -> failwith key
            | (ip, occ):: tl ->
              Gwdb.Marker.set skip ip true ;
              loop [ (ip, occ) ] tl
          end
        | acc ->
          match list with
          | [] -> acc
          | (ip, occ) :: tl ->
            if not @@ Gwdb.Marker.get skip ip then begin
              Gwdb.Marker.set skip ip true ;
              if mem_occ occ acc tl then begin
                let occ' = new_occ acc list in
                Gwdb.patch_person base ip
                  { (Gwdb.gen_person_of_person (poi base ip) ) with occ = occ' } ;
                begin match report with
                  | Some fn -> fn (Fix_UpdatedOcc (ip, occ, occ'))
                  | None -> ()
                end ;
                loop ((ip, occ') :: acc) tl
              end else
                loop ((ip, occ) :: acc) tl
            end else loop ((ip, occ) :: acc) tl
      in
      ignore @@ loop [] rev_list
    end
  end ipers
