open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type patch =
  | Fix_NBDS of Driver.iper
  | Fix_AddedUnion of Driver.iper
  | Fix_AddedParents of Driver.iper
  | Fix_ParentDeleted of Driver.iper
  | Fix_AddedChild of Driver.ifam
  | Fix_RemovedUnion of Driver.iper * Driver.ifam
  | Fix_RemovedDuplicateUnion of Driver.iper * Driver.ifam
  | Fix_AddedRelatedFromPevent of Driver.iper * Driver.iper
  | Fix_AddedRelatedFromFevent of Driver.iper * Driver.iper
  | Fix_MarriageDivorce of Driver.ifam
  | Fix_MissingSpouse of Driver.ifam * Driver.iper
  | Fix_WrongUTF8Encoding of
      Driver.ifam option
      * Driver.iper option
      * (Driver.istr * Driver.istr) option
  | Fix_UpdatedOcc of Driver.iper * int * int

let mk_pevent name date place note src =
  {
    epers_name = name;
    epers_date = date;
    epers_place = place;
    epers_reason = Driver.Istr.empty;
    epers_note = note;
    epers_src = src;
    epers_witnesses = [||];
  }

let of_pevent e = (e.epers_date, e.epers_place, e.epers_note, e.epers_src)

let find_pevent names pevents =
  List.find_opt (fun x -> List.mem x.epers_name names) pevents

let fix_pevents ?report base pp =
  (* Should it use UpdateIndOk.reconstitute_from_pevents? *)
  (* TODO clean up *)
  let p = Driver.gen_person_of_person pp in
  let empty_bi =
    (Date.cdate_None, Driver.Istr.empty, Driver.Istr.empty, Driver.Istr.empty)
  in
  let empty_bp =
    (Date.cdate_None, Driver.Istr.empty, Driver.Istr.empty, Driver.Istr.empty)
  in
  let empty_de =
    (Date.cdate_None, Driver.Istr.empty, Driver.Istr.empty, Driver.Istr.empty)
  in
  let empty_bu =
    (Date.cdate_None, Driver.Istr.empty, Driver.Istr.empty, Driver.Istr.empty)
  in
  let pevents = p.pevents in
  let aux name date place note src empty pevents =
    match find_pevent [ name ] pevents with
    | None ->
        let pevents =
          if (date, place, note, src) <> empty then
            mk_pevent name date place note src :: pevents
          else pevents
        in
        ((date, place, note, src), pevents)
    | Some e ->
        let e' =
          {
            epers_name = e.epers_name;
            epers_date =
              (if e.epers_date = Date.cdate_None then date else e.epers_date);
            epers_place =
              (if e.epers_place = Driver.Istr.empty then place
               else e.epers_place);
            epers_reason = e.epers_reason;
            epers_note =
              (if e.epers_note = Driver.Istr.empty then note else e.epers_note);
            epers_src =
              (if e.epers_src = Driver.Istr.empty then src else e.epers_src);
            epers_witnesses = e.epers_witnesses;
          }
        in
        (of_pevent e', Mutil.list_replace e e' pevents)
  in
  let (birth, birth_place, birth_note, birth_src), pevents =
    aux Epers_Birth p.birth p.birth_place p.birth_note p.birth_src empty_bi
      pevents
  in
  let (baptism, baptism_place, baptism_note, baptism_src), pevents =
    aux Epers_Baptism p.baptism p.baptism_place p.baptism_note p.baptism_src
      empty_bp pevents
  in
  let (death, death_place, death_note, death_src), pevents =
    let death =
      match p.death with
      | Death (_, d) -> d
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
        ->
          Date.cdate_None
    in
    aux Epers_Death death p.death_place p.death_note p.death_src empty_de
      pevents
  in
  let death =
    match p.death with
    | Death _ -> p.death
    | NotDead when death <> Date.cdate_None -> Death (Unspecified, death)
    | x -> x
  in
  let (burial, burial_place, burial_note, burial_src), pevents =
    match p.burial with
    | UnknownBurial -> (
        match find_pevent [ Epers_Burial; Epers_Cremation ] pevents with
        | None ->
            ( (UnknownBurial, p.burial_place, p.burial_note, p.burial_src),
              pevents )
        | Some e ->
            let bu, bu_place, bu_note, bu_src = of_pevent e in
            ( ( (match e.epers_name with
                | Epers_Burial -> Buried bu
                | Epers_Cremation -> Cremated bu
                | _ -> assert false),
                bu_place,
                bu_note,
                bu_src ),
              pevents ))
    | Buried d ->
        let (d, bu_place, bu_note, bu_src), pevents =
          aux Epers_Burial d p.burial_place p.burial_note p.burial_src empty_bu
            pevents
        in
        ((Buried d, bu_place, bu_note, bu_src), pevents)
    | Cremated d ->
        let (d, bu_place, bu_note, bu_src), pevents =
          aux Epers_Cremation d p.burial_place p.burial_note p.burial_src
            empty_bu pevents
        in
        ((Cremated d, bu_place, bu_note, bu_src), pevents)
  in
  let p' =
    {
      p with
      birth;
      birth_place;
      birth_note;
      birth_src;
      baptism;
      baptism_place;
      baptism_note;
      baptism_src;
      death;
      death_place;
      death_note;
      death_src;
      burial;
      burial_place;
      burial_note;
      burial_src;
      pevents;
    }
  in
  if p <> p' then (
    Driver.patch_person base p.key_index p';
    match report with Some fn -> fn (Fix_NBDS p.key_index) | None -> ())

let check_NBDS ?report progress base =
  let nb_ind = Driver.nb_of_persons base in
  Collection.iteri
    (fun i p ->
      progress i nb_ind;
      fix_pevents ?report base p)
    (Driver.persons base)

let check_families_parents ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let ifam = Driver.get_ifam fam in
      Array.iter
        (fun ip ->
          let unions = Driver.get_family (Driver.poi base ip) in
          if not @@ Array.mem ifam unions then (
            Driver.patch_union base ip
              { family = Array.append unions [| ifam |] };
            match report with Some fn -> fn (Fix_AddedUnion ip) | None -> ()))
        (Driver.get_parent_array fam))
    (Driver.families base)

let check_families_children ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      let ifam = Driver.get_ifam fam in
      progress i nb_fam;
      let children = Driver.get_children fam in
      for j = 0 to Array.length children - 1 do
        let ip = children.(j) in
        let a = Driver.poi base ip in
        let parents = Driver.get_parents a in
        if parents = Some Driver.Ifam.dummy || parents = None then (
          Driver.patch_ascend base ip
            { parents = Some ifam; consang = Driver.get_consang a };
          match report with Some fn -> fn (Fix_AddedParents ip) | None -> ())
        (* else if parents <> Some ifam && verbosity1 then begin
         *   (\* FIXME: what to do here ? *\)
         *   Printf.printf "\tbad parents : %s\n" (string_of_p base ip);
         *   flush stdout
         * end *)
      done)
    (Driver.families base)

let check_persons_parents ?report progress base =
  let nb_ind = Driver.nb_of_persons base in
  Collection.iteri
    (fun i p ->
      progress i nb_ind;
      Driver.get_parents p
      |> Option.iter @@ fun ifam ->
         let ip = Driver.get_iper p in
         let fam = Driver.foi base ifam in
         if Driver.get_ifam fam = Driver.Ifam.dummy then (
           Driver.patch_ascend base ip
             { parents = None; consang = Adef.no_consang };
           match report with Some fn -> fn (Fix_ParentDeleted ip) | None -> ())
         else
           let children = Driver.get_children fam in
           if not @@ Array.mem ip children then (
             let children = Array.append children [| ip |] in
             Driver.patch_descend base ifam { children };
             match report with
             | Some fn -> fn (Fix_AddedChild ifam)
             | None -> ()))
    (Driver.persons base)

let check_persons_families ?report progress base =
  let nb_ind = Driver.nb_of_persons base in
  Collection.iteri
    (fun i p ->
      progress i nb_ind;
      let ip = Driver.get_iper p in
      let ifams = Driver.get_family p in
      let ifams' =
        Array.of_list
        @@ Array.fold_right
             (fun ifam acc ->
               let cpl = Driver.foi base ifam in
               if List.mem ifam acc then
                 match report with
                 | Some fn ->
                     fn (Fix_RemovedDuplicateUnion (ip, ifam));
                     acc
                 | None -> acc
               else if not @@ Array.mem ip (Driver.get_parent_array cpl) then
                 match report with
                 | Some fn ->
                     fn (Fix_RemovedUnion (ip, ifam));
                     acc
                 | None -> acc
               else ifam :: acc)
             ifams []
      in
      if ifams' <> ifams then Driver.patch_union base ip { family = ifams' })
    (Driver.persons base)

let check_pevents_witnesses ?report progress base =
  let nb_ind = Driver.nb_of_persons base in
  Collection.iteri
    (fun i p ->
      progress i nb_ind;
      let ip = Driver.get_iper p in
      List.iter
        (fun evt ->
          let witn = Array.map fst evt.epers_witnesses in
          for j = 0 to Array.length witn - 1 do
            let ip2 = witn.(j) in
            let p2 = Driver.poi base ip2 in
            if not (List.memq ip (Driver.get_related p2)) then (
              Driver.patch_person base ip2
                {
                  (Driver.gen_person_of_person p2) with
                  related = ip :: Driver.get_related p2;
                };
              match report with
              | Some fn -> fn (Fix_AddedRelatedFromPevent (ip2, ip))
              | None -> ())
          done)
        (Driver.get_pevents p))
    (Driver.persons base)

let check_fevents_witnesses ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let ifath = Driver.get_father fam in
      List.iter
        (fun evt ->
          let witn = Array.map fst evt.efam_witnesses in
          for j = 0 to Array.length witn - 1 do
            let ip = witn.(j) in
            let p = Driver.poi base ip in
            if not (List.memq ifath (Driver.get_related p)) then (
              Driver.patch_person base ip
                {
                  (Driver.gen_person_of_person p) with
                  related = ifath :: Driver.get_related p;
                };
              match report with
              | Some fn -> fn (Fix_AddedRelatedFromFevent (ip, ifath))
              | None -> ())
          done)
        (Driver.get_fevents fam))
    (Driver.families base)

let fix_marriage_divorce ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let fevents = Driver.get_fevents fam in
      let relation0 = Driver.get_relation fam in
      let marriage0 = Driver.get_marriage fam in
      let marriage_place0 = Driver.get_marriage_place fam in
      let marriage_note0 = Driver.get_marriage_note fam in
      let marriage_src0 = Driver.get_marriage_src fam in
      (* TODO is divorce treated as fevent! *)
      let divorce0 = Driver.get_divorce fam in
      let marr_data0 =
        (relation0, marriage0, marriage_place0, marriage_note0, marriage_src0)
      in
      let ( ((relation, marriage, marriage_place, marriage_note, marriage_src)
             as marr_data),
            divorce,
            _ ) =
        UpdateFamOk.reconstitute_from_fevents false
          (Driver.insert_string base "")
          fevents
      in
      if marr_data0 <> marr_data || divorce0 <> divorce then (
        let fam' =
          {
            (Driver.gen_family_of_family fam) with
            relation;
            marriage;
            marriage_place;
            marriage_note;
            marriage_src;
            divorce;
          }
        in
        Driver.patch_family base (Driver.get_ifam fam) fam';
        match report with
        | Some fn -> fn (Fix_MarriageDivorce (Driver.get_ifam fam))
        | None -> ()))
    (Driver.families base)

let fix_missing_spouses ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let aux i =
        let p = Driver.poi base i in
        if Driver.get_iper p = Driver.Iper.dummy then (
          Driver.patch_union base i { family = [| Driver.get_ifam fam |] };
          Driver.patch_person base i
            { (Driver.gen_person_of_person p) with key_index = i };
          match report with
          | Some fn -> fn (Fix_MissingSpouse (Driver.get_ifam fam, i))
          | None -> ())
      in
      aux @@ Driver.get_father fam;
      aux @@ Driver.get_mother fam)
    (Driver.families base)

let fix_utf8_sequence ?report progress base =
  let normalize_utf_8_date ifam iper s =
    let s' = Mutil.normalize_utf_8 s in
    (if s <> s' then
       match report with
       | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, None))
       | None -> ());
    s'
  in
  let normalize_utf_8 ifam iper i =
    let s = Driver.sou base i in
    let s' = Mutil.normalize_utf_8 s in
    let i' = Driver.insert_string base s' in
    (if i <> i' then
       match report with
       | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, Some (i, i')))
       | None -> ());
    i'
  in
  let nbf = Driver.nb_of_families base in
  let nbp = Driver.nb_of_persons base in
  let nb = nbp + nbf in
  let fp i = i in
  let ff i = i in
  let fs ifam iper i = normalize_utf_8 ifam iper i in
  let fd ifam iper = function
    | Dtext d -> Dtext (normalize_utf_8_date ifam iper d)
    | d -> d
  in
  Collection.iteri
    (fun i fam ->
      progress i nb;
      let ifam = Driver.get_ifam fam in
      let f = Driver.gen_family_of_family fam in
      let f' =
        Futil.map_family_ps ~fd:(fd (Some ifam) None) fp ff
          (fs (Some ifam) None) f
      in
      if f' <> f then Driver.patch_family base ifam f')
    (Driver.families base);
  Collection.iteri
    (fun i per ->
      progress (nbf + i) nb;
      let iper = Driver.get_iper per in
      let p = Driver.gen_person_of_person per in
      let p' =
        Futil.map_person_ps ~fd:(fd None (Some iper)) fp (fs None (Some iper)) p
      in
      if p' <> p then Driver.patch_person base iper p')
    (Driver.persons base)

let fix_key ?report progress base =
  let nb_ind = Driver.nb_of_persons base in
  let ipers = Driver.ipers base in
  let skip = Driver.iper_marker ipers false in
  Collection.iteri
    (fun i ip ->
      progress i nb_ind;
      let p = Driver.poi base ip in
      let f = Driver.p_first_name base p in
      let s = Driver.p_surname base p in
      if f <> "?" && s <> "?" then
        let key = Name.concat f s in
        let ipers = Driver.persons_of_name base key in
        let f = Name.lower f in
        let s = Name.lower s in
        let list =
          let rec loop acc = function
            | ip :: tl ->
                let p = Driver.poi base ip in
                if
                  Name.lower @@ Driver.p_first_name base p = f
                  && Name.lower @@ Driver.p_surname base p = s
                then loop ((Driver.get_iper p, Driver.get_occ p) :: acc) tl
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
          if mem_occ !cnt acc list then (
            incr cnt;
            new_occ acc list)
          else !cnt
        in
        let rec loop acc list =
          match acc with
          | [] -> (
              match list with
              | [] -> failwith key
              | (ip, occ) :: tl ->
                  Collection.Marker.set skip ip true;
                  loop [ (ip, occ) ] tl)
          | acc -> (
              match list with
              | [] -> acc
              | (ip, occ) :: tl ->
                  if not @@ Collection.Marker.get skip ip then (
                    Collection.Marker.set skip ip true;
                    if mem_occ occ acc tl then (
                      let occ' = new_occ acc list in
                      Driver.patch_person base ip
                        {
                          (Driver.gen_person_of_person (Driver.poi base ip)) with
                          occ = occ';
                        };
                      (match report with
                      | Some fn -> fn (Fix_UpdatedOcc (ip, occ, occ'))
                      | None -> ());
                      loop ((ip, occ') :: acc) tl)
                    else loop ((ip, occ) :: acc) tl)
                  else loop ((ip, occ) :: acc) tl)
        in
        ignore @@ loop [] rev_list)
    ipers
