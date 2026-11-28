open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type patch =
  | Fix_NBDS of Driver.iper * string list
  | Unfixed_UnknownBurial of Driver.iper
  | Fix_AddedUnion of Driver.iper
  | Fix_AddedParents of Driver.iper
  | Fix_ParentDeleted of Driver.iper
  | Fix_AddedChild of Driver.ifam
  | Fix_RemovedUnion of Driver.iper * Driver.ifam
  | Fix_RemovedDuplicateUnion of Driver.iper * Driver.ifam
  | Fix_RemovedDuplicateChild of Driver.ifam * Driver.iper
  | Fix_AddedRelatedFromPevent of Driver.iper * Driver.iper
  | Fix_AddedRelatedFromFevent of Driver.iper * Driver.iper
  | Fix_MarriageDivorce of
      Driver.ifam * relation_kind * relation_kind * string list
  | Fix_MissingSpouse of Driver.ifam * Driver.iper
  | Fix_WrongUTF8Encoding of
      Driver.ifam option
      * Driver.iper option
      * (Driver.istr * Driver.istr) option
  | Fix_UpdatedOcc of Driver.iper * int * int
  | Fix_ChildrenOrder of Driver.ifam * Driver.iper array * Driver.iper array

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
        | Some e
          when e.epers_date <> Date.cdate_None
               || not (Driver.Istr.is_empty e.epers_place) ->
            let bu, bu_place, bu_note, bu_src = of_pevent e in
            ( ( (match e.epers_name with
                | Epers_Burial -> Buried bu
                | Epers_Cremation -> Cremated bu
                | _ -> assert false),
                bu_place,
                bu_note,
                bu_src ),
              pevents )
        | e ->
            if Option.is_some e then
              Option.iter
                (fun fn -> fn (Unfixed_UnknownBurial p.key_index))
                report;
            ( (UnknownBurial, p.burial_place, p.burial_note, p.burial_src),
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
  let fields =
    List.filter_map
      (fun (name, changed) -> if changed then Some name else None)
      [
        ( "birth",
          (p.birth, p.birth_place, p.birth_note, p.birth_src)
          <> (birth, birth_place, birth_note, birth_src) );
        ( "baptism",
          (p.baptism, p.baptism_place, p.baptism_note, p.baptism_src)
          <> (baptism, baptism_place, baptism_note, baptism_src) );
        ( "death",
          (p.death, p.death_place, p.death_note, p.death_src)
          <> (death, death_place, death_note, death_src) );
        ( "burial",
          (p.burial, p.burial_place, p.burial_note, p.burial_src)
          <> (burial, burial_place, burial_note, burial_src) );
        ("events", p.pevents <> pevents);
      ]
  in
  if fields <> [] then (
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
    Driver.patch_person base p.key_index p';
    match report with
    | Some fn -> fn (Fix_NBDS (p.key_index, fields))
    | None -> ())

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
      let children =
        let seen = Hashtbl.create (Array.length children) in
        let uniq =
          Array.fold_left
            (fun acc ip ->
              if Hashtbl.mem seen ip then (
                (match report with
                | Some fn -> fn (Fix_RemovedDuplicateChild (ifam, ip))
                | None -> ());
                acc)
              else (
                Hashtbl.add seen ip ();
                ip :: acc))
            [] children
        in
        if List.length uniq = Array.length children then children
        else
          let children = Array.of_list (List.rev uniq) in
          Driver.patch_descend base ifam { children };
          children
      in
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

(* Inclusive SDN interval of the birth date of [p], or of its baptism date
   when there is no birth date; [None] when neither is a Gregorian-comparable
   date (no date, or text date). Partial non-Gregorian dates are normalized
   first, as in [CheckItem]. *)
let child_date_interval p =
  let interval cd =
    match Option.map Date.date_to_gregorian_bounds (Date.od_of_cdate cd) with
    | Some (Adef.Dgreg (d, _)) -> Some (Date.gregorian_interval d)
    | Some (Adef.Dtext _) | None -> None
  in
  match interval (Driver.get_birth p) with
  | Some _ as i -> i
  | None -> interval (Driver.get_baptism p)

(* Puts the dated children of [children] in birth order, moving few of them;
   children without a comparable date are never moved and keep their place
   relative to the children that do not move.

   1. Target order of the dated children: each rank takes the first remaining
      child, in the original order, that no other remaining child is strictly
      born before. Children whose dates cannot be strictly compared
      (overlapping intervals: « about 1800 » and « 1800 »...) thus keep their
      original relative order when possible.
   2. The dated children that stay are a longest subsequence already in target
      order (the earliest one in the list when there are several). The target
      order being fixed first, overlapping dates may cost an extra move.
   3. Each other dated child, in target order, is removed and reinserted
      between its neighbours in the target order: just before its successor
      when it comes from below it, just after its predecessor otherwise, so
      that it moves as little as possible.
   Ex.: [A(1800) ? B(1790)] becomes [B A ?].
   Returns [None] when nothing has to move. *)
let sort_dated_children base children =
  let dated =
    Array.to_list children
    |> List.filter_map (fun ip ->
        Option.map
          (fun itv -> (ip, itv))
          (child_date_interval (Driver.poi base ip)))
    |> Array.of_list
  in
  let n = Array.length dated in
  if n < 2 then None
  else
    let before i j =
      let _, (_, hi) = dated.(i) and _, (lo, _) = dated.(j) in
      hi < lo
    in
    (* 1. target order: order.(k) is the index in [dated] of rank [k] *)
    let used = Array.make n false in
    let has_predecessor i =
      let rec loop j =
        if j >= n then false
        else if j <> i && (not used.(j)) && before j i then true
        else loop (j + 1)
      in
      loop 0
    in
    let rec pick i =
      (* strict order on intervals is acyclic: a minimal remaining child
         always exists, so [pick] never runs past [n] *)
      if used.(i) || has_predecessor i then pick (i + 1) else i
    in
    let order =
      Array.init n (fun _ ->
          let i = pick 0 in
          used.(i) <- true;
          i)
    in
    if Array.for_all2 ( = ) order (Array.init n Fun.id) then None
    else
      let rank = Array.make n 0 in
      Array.iteri (fun k i -> rank.(i) <- k) order;
      (* 2. earliest longest increasing subsequence of [rank] *)
      let best = Array.make n 1 in
      for i = n - 1 downto 0 do
        for j = i + 1 to n - 1 do
          if rank.(j) > rank.(i) && best.(j) + 1 > best.(i) then
            best.(i) <- best.(j) + 1
        done
      done;
      let placed = Array.make n false in
      let need = ref (Array.fold_left max 0 best) and last = ref (-1) in
      for i = 0 to n - 1 do
        if !need > 0 && best.(i) = !need && rank.(i) > !last then (
          placed.(i) <- true;
          decr need;
          last := rank.(i))
      done;
      (* 3. reinsert the other dated children *)
      let cur =
        let k = ref (-1) in
        Array.to_list children
        |> List.filter_map (fun ip ->
            if !k + 1 < n && fst dated.(!k + 1) = ip then (
              incr k;
              if placed.(!k) then Some (Either.Right !k) else None)
            else Some (Either.Left ip))
        |> ref
      in
      let insert ~at ~after x =
        let rec loop = function
          | [] -> [ x ]
          | (Either.Right j as e) :: l when j = at ->
              if after then e :: x :: l else x :: e :: l
          | e :: l -> e :: loop l
        in
        cur := loop !cur
      in
      Array.iteri
        (fun k i ->
          if not placed.(i) then (
            let rec find k' step =
              if k' < 0 || k' >= n then None
              else if placed.(order.(k')) then Some order.(k')
              else find (k' + step) step
            in
            (* coming from below its successor: just before it; otherwise
               just after its predecessor *)
            (match (find (k - 1) (-1), find (k + 1) 1) with
            | None, Some succ -> insert ~at:succ ~after:false (Either.Right i)
            | Some pred, None -> insert ~at:pred ~after:true (Either.Right i)
            | Some pred, Some succ ->
                if i > succ then insert ~at:succ ~after:false (Either.Right i)
                else insert ~at:pred ~after:true (Either.Right i)
            | None, None -> assert false (* at least one child is placed *));
            placed.(i) <- true))
        order;
      Some
        (List.map
           (Either.fold ~left:Fun.id ~right:(fun i -> fst dated.(i)))
           !cur
        |> Array.of_list)

let fix_children_order ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let ifam = Driver.get_ifam fam in
      let before = Driver.get_children fam in
      match sort_dated_children base before with
      | None -> ()
      | Some children -> (
          Driver.patch_descend base ifam { children };
          match report with
          | Some fn -> fn (Fix_ChildrenOrder (ifam, before, children))
          | None -> ()))
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
          let witn = Array.map (fun (ip, _, _) -> ip) evt.epers_witnesses in
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
          let witn = Array.map (fun (ip, _, _) -> ip) evt.efam_witnesses in
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
  let same_string i i' =
    Driver.Istr.equal i i' || Driver.sou base i = Driver.sou base i'
  in
  let is_marriage_kind e =
    match e.efam_name with
    | Efam_Divorce | Efam_Separated | Efam_Name _ -> false
    | Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
    | Efam_Annulation | Efam_MarriageBann | Efam_MarriageContract
    | Efam_MarriageLicense | Efam_PACS | Efam_Residence ->
        true
  in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let fevents = Driver.get_fevents fam in
      if List.exists is_marriage_kind fevents then
        let relation0 = Driver.get_relation fam in
        let nsck =
          match relation0 with
          | NoSexesCheckMarried | NoSexesCheckNotMarried -> true
          | _ -> false
        in
        let ( (relation, marriage, marriage_place, marriage_note, marriage_src),
              divorce,
              _ ) =
          UpdateFamOk.reconstitute_from_fevents nsck Driver.Istr.empty fevents
        in
        let fields =
          List.filter_map
            (fun (name, changed) -> if changed then Some name else None)
            [
              ( "marriage",
                Date.od_of_cdate (Driver.get_marriage fam)
                <> Date.od_of_cdate marriage );
              ( "marriage_place",
                not (same_string (Driver.get_marriage_place fam) marriage_place)
              );
              ( "marriage_note",
                not (same_string (Driver.get_marriage_note fam) marriage_note)
              );
              ( "marriage_src",
                not (same_string (Driver.get_marriage_src fam) marriage_src) );
            ]
        in
        if relation0 <> relation || fields <> [] then (
          let ifam = Driver.get_ifam fam in
          Driver.patch_family base ifam
            {
              (Driver.gen_family_of_family fam) with
              relation;
              marriage;
              marriage_place;
              marriage_note;
              marriage_src;
              divorce;
            };
          match report with
          | Some fn ->
              fn (Fix_MarriageDivorce (ifam, relation0, relation, fields))
          | None -> ()))
    (Driver.families base)

let fix_missing_spouses ?report progress base =
  let nb_fam = Driver.nb_of_families base in
  let restored = Driver.Iper.Table.create 16 in
  Collection.iteri
    (fun i fam ->
      progress i nb_fam;
      let ifam = Driver.get_ifam fam in
      let aux i =
        let p = Driver.poi base i in
        if Driver.get_iper p = Driver.Iper.dummy then (
          Driver.patch_union base i { family = [| ifam |] };
          Driver.patch_person base i
            { (Driver.gen_person_of_person p) with key_index = i };
          Driver.Iper.Table.replace restored i ();
          match report with
          | Some fn -> fn (Fix_MissingSpouse (ifam, i))
          | None -> ())
        else if Driver.Iper.Table.mem restored i then
          let family = Driver.get_family p in
          if not (Array.exists (Driver.Ifam.equal ifam) family) then (
            Driver.patch_union base i
              { family = Array.append family [| ifam |] };
            match report with
            | Some fn -> fn (Fix_MissingSpouse (ifam, i))
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
    | Adef.Dtext d -> Adef.Dtext (normalize_utf_8_date ifam iper d)
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
  let names = Hashtbl.create 1024 in
  let ids = Driver.Istr.Table.create 1024 in
  let name_id istr =
    match Driver.Istr.Table.find_opt ids istr with
    | Some id -> id
    | None ->
        let s = Name.lower (Driver.sou base istr) in
        let id =
          match Hashtbl.find_opt names s with
          | Some id -> id
          | None ->
              let id = Hashtbl.length names in
              Hashtbl.replace names s id;
              id
        in
        Driver.Istr.Table.replace ids istr id;
        id
  in
  let blank = name_id Driver.Istr.empty in
  let groups = Hashtbl.create 1024 in
  Collection.iteri
    (fun i p ->
      progress i nb_ind;
      let f = name_id (Driver.get_first_name p) in
      let s = name_id (Driver.get_surname p) in
      if f <> blank && s <> blank then
        let l = Option.value ~default:[] (Hashtbl.find_opt groups (f, s)) in
        Hashtbl.replace groups (f, s)
          ((Driver.get_occ p, Driver.get_iper p) :: l))
    (Driver.persons base);
  let renumber l =
    let l = List.sort (fun (_, a) (_, b) -> Driver.Iper.compare a b) l in
    let kept = Hashtbl.create 8 in
    let moved =
      List.fold_left
        (fun moved (occ, ip) ->
          if occ >= 0 && not (Hashtbl.mem kept occ) then (
            Hashtbl.replace kept occ ();
            moved)
          else (occ, ip) :: moved)
        [] l
    in
    let rec free occ = if Hashtbl.mem kept occ then free (occ + 1) else occ in
    ignore
    @@ List.fold_left
         (fun occ (old_occ, ip) ->
           let occ = free occ in
           Driver.patch_person base ip
             { (Driver.gen_person_of_person (Driver.poi base ip)) with occ };
           Option.iter (fun fn -> fn (Fix_UpdatedOcc (ip, old_occ, occ))) report;
           occ + 1)
         0 (List.rev moved)
  in
  Hashtbl.iter
    (fun _ l ->
      match l with
      | [ (occ, _) ] when occ >= 0 -> ()
      | [] | _ :: _ -> renumber l)
    groups
