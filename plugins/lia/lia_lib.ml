#ifdef API
module MLink = Lia_api_link_tree_piqi

open Geneweb
open Config
open Def
open Gwdb
open Util

let iper i = Gwdb.iper_of_string (Int32.to_string i)
let ifam i = Gwdb.ifam_of_string (Int32.to_string i)

let person_aux fn _conf base bsrc ip =
  match fn bsrc ip with
  | Some x -> Some (Lia_perso_link.make_ep_link base x, x.MLink.Person.baseprefix)
  | None -> None

let get_father conf base bsrc ip = person_aux Lia_perso_link.get_father_link conf base bsrc ip

let get_mother conf base bsrc ip = person_aux Lia_perso_link.get_mother_link conf base bsrc ip

let get_person conf base bsrc ip = person_aux Lia_perso_link.get_person_link conf base bsrc ip

let person_aux' father conf base ip =
  match Lia_perso_link.get_parents_link conf.command ip with
  | None -> None
  | Some family ->
    let ifath = iper family.MLink.Family.ifath in
    let imoth = iper family.MLink.Family.imoth in
    let ifam = ifam family.MLink.Family.ifam in
    match Lia_perso_link.get_person_link family.MLink.Family.baseprefix (if father then ifath else imoth) with
    | Some p ->
      let ep = Lia_perso_link.make_ep_link base p in
      let cpl = ifath, imoth, (if father then imoth else ifath) in
      let (_, fam, _, _) = Lia_perso_link.make_efam_link conf base family in
      Some (p.MLink.Person.baseprefix, ep, ifam, fam, cpl)
    | None -> None

let get_father' = person_aux' true
let get_mother' = person_aux' false

let init_cache = Lia_perso_link.init_cache

let max_ancestor_level conf base ip bname max_lev level =
  let x = ref level in
  let rec loop_lia level (ip, base_prefix) =
    x := max !x level;
    if !x <> max_lev
    then match Lia_perso_link.get_parents_link base_prefix ip with
      | Some family ->
        let ifath = iper family.MLink.Family.ifath in
        let imoth = iper family.MLink.Family.imoth in
        let base_prefix = family.MLink.Family.baseprefix in
        if Lia_perso_link.get_person_link base_prefix ifath <> None
        then loop_lia (succ level) (ifath, base_prefix) ;
        if Lia_perso_link.get_person_link base_prefix imoth <> None
        then loop_lia (succ level) (imoth, base_prefix)
      | None -> ()
  in
  loop_lia level (ip, bname) ;
  !x

let max_descendant_level conf base ip max =
  Lia_perso_link.max_interlinks_descendancy_level conf base ip max

let tree_generation_list conf base base_prefix p =
  match Lia_perso_link.get_parents_link base_prefix (get_iper p) with
  | None -> None, None
  | Some family ->
    let ifam = ifam family.MLink.Family.ifam in
    let aux i =
      match Lia_perso_link.get_person_link family.MLink.Family.baseprefix (iper i) with
      | Some x ->
        let (x, _) = Lia_perso_link.make_ep_link base x in
        if not @@ is_empty_name x
        then Some (x, ifam, family.MLink.Family.baseprefix)
        else None
      | None -> None
    in
    aux family.MLink.Family.ifath, aux family.MLink.Family.imoth

let get_family conf base base_prefix p ifam =
  let ip = get_iper p in
  let rec loop = function
    | [] -> None
    | fam_link :: l ->
      let fam_link_ifam = Gwdb.ifam_of_string @@ Int32.to_string fam_link.MLink.Family.ifam in
      if fam_link_ifam = ifam then
        let (_, fam, _, _) = Lia_perso_link.make_efam_link conf base fam_link in
        let ifath = iper fam_link.MLink.Family.ifath in
        let imoth = iper fam_link.MLink.Family.imoth in
        let cpl =
          if ip <> ifath && ip <> imoth then
            match Lia_perso_link.get_person_link_with_base conf.command ip base_prefix with
            | Some p ->
              let ip = iper p.MLink.Person.ip in
              ifath, imoth, (if ip = ifath then imoth else ifath)
            | None ->
              ifath, imoth, (if ip = ifath then imoth else ifath)
          else
            ifath, imoth, (if ip = ifath then imoth else ifath)
        in
        Some (fam, cpl, true)
      else loop l
  in loop (Lia_perso_link.get_family_link conf.command ip)

let get_children_of_parents base baseprefix ifam ifath imoth =
  Lia_perso_link.get_children_of_parents baseprefix ifam ifath imoth
  |> List.map (fun p -> (Lia_perso_link.make_ep_link base p |> fst, p.MLink.Person.baseprefix))

let has_children conf base p fam =
  let ip = get_iper p in
  let rec loop = function
    | [] -> false
    | fam_link :: faml ->
      let ifath = iper fam_link.MLink.Family.ifath in
      let imoth = iper fam_link.MLink.Family.imoth in
      let cpl =
        if ip <> ifath && ip <> imoth then
          match Lia_perso_link.get_person_link_with_base conf.command ip fam_link.MLink.Family.baseprefix with
          | Some p ->
            let ip = iper p.MLink.Person.ip in
            ifath, imoth, (if ip = ifath then imoth else ifath)
          | None ->
            ifath, imoth, (if ip = ifath then imoth else ifath)
        else ifath, imoth, (if ip = ifath then imoth else ifath)
      in
      if Lia_perso_link.can_merge_family conf.command ip [fam] fam_link cpl then
        let ifam = ifam fam_link.MLink.Family.ifam in
        [] <> get_children_of_parents base fam_link.MLink.Family.baseprefix ifam ifath imoth
      else loop faml
  in
  loop (Lia_perso_link.get_family_link conf.command ip)

let family conf base f =
  let _, fam, _, _ = Lia_perso_link.make_efam_link conf base f in
  fam

let has_family_correspondance baseprefix ip =
  [] <> Lia_perso_link.get_family_correspondance baseprefix ip

let has_parents_link baseprefix ip =
  None <> Lia_perso_link.get_parents_link baseprefix ip

let has_siblings baseprefix ip =
  match Lia_perso_link.get_parents_link baseprefix ip with
  | None -> false
  | Some family -> List.length family.MLink.Family.children > 1

let nb_children baseprefix ifam =
  Lia_perso_link.get_children_of_fam baseprefix ifam
  |> List.length

let nb_families baseprefix ip =
  Lia_perso_link.get_family_correspondance baseprefix ip
  |> List.length

let get_children base baseprefix ifam ifath imoth  =
  Lia_perso_link.get_children_of_parents baseprefix ifam ifath imoth
  |> List.map (fun c -> Lia_perso_link.make_ep_link base c, c.MLink.Person.baseprefix)

let cpl conf ip fam_link =
  let baseprefix = fam_link.MLink.Family.baseprefix in
  let ifath = iper fam_link.MLink.Family.ifath in
  let imoth = iper fam_link.MLink.Family.imoth in
  if ip <> ifath && ip <> imoth then
    match Lia_perso_link.get_person_link_with_base conf.command ip baseprefix with
    | Some p ->
      ifath, imoth, (if iper p.MLink.Person.ip = ifath then imoth else ifath)
    | None ->
      ifath, imoth, (if ip = ifath then imoth else ifath)
  else ifath, imoth, (if ip = ifath then imoth else ifath)

let get_families conf base p =
  let ip = get_iper p in
  let can_merge =
    let fam = List.map (foi base) (Array.to_list (Gwdb.get_family p)) in
    Lia_perso_link.can_merge_family conf.command ip fam
  in
  List.filter_map begin fun fam_link ->
    let (_, fam, _, _) = Lia_perso_link.make_efam_link conf base fam_link in
    let ifam = ifam fam_link.MLink.Family.ifam in
    let (ifath, imoth, isp) as cpl = cpl conf ip fam_link in
    match Lia_perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
    | Some sp ->
      Some ( ifam
           , fam
           , (ifath, imoth, Lia_perso_link.make_ep_link base sp |> fst)
           , fam_link.MLink.Family.baseprefix
           , can_merge fam_link cpl)
    | None -> None
  end (Lia_perso_link.get_family_link conf.command ip)

let get_children' conf base ip fam isp  =
  let can_merge =
    Lia_perso_link.can_merge_child conf.command (Gwdb.get_children fam)
  in
  List.map begin fun fam_link ->
    ( fam_link.MLink.Family.baseprefix
    , cpl conf ip fam_link
    , List.filter_map begin fun c ->
        let baseprefix = c.MLink.Person_link.baseprefix in
        let ip_c = iper c.MLink.Person_link.ip in
        match Lia_perso_link.get_person_link baseprefix ip_c with
        | Some c ->
          Some (Lia_perso_link.make_ep_link base c, c.MLink.Person.baseprefix, can_merge c)
        | None -> None
      end fam_link.MLink.Family.children
    )
  end (Lia_perso_link.get_families_of_parents conf.command ip isp)

#endif
