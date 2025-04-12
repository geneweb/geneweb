open Dbdisk

(* copied from Gwdb_driver *)
let dummy_ifam = -1
let empty_string = 0
let quest_string = 1

let empty_person (p : _ Def.gen_person) =
  (p.first_name = empty_string || p.first_name = quest_string)
  && (p.surname = empty_string || p.surname = quest_string)
  (* && p.occ = 0 *)
  && p.image = empty_string
  && p.first_names_aliases = [] && p.surnames_aliases = []
  && p.public_name = empty_string
  && p.qualifiers = [] && p.titles = [] && p.rparents = [] && p.related = []
  && p.aliases = []
  && p.occupation = empty_string
  && p.sex = Neuter
  (* && p.access = Private *)
  && p.birth = Date.cdate_None
  && p.birth_place = empty_string
  && p.birth_note = empty_string
  && p.birth_src = empty_string
  && p.baptism = Date.cdate_None
  && p.baptism_place = empty_string
  && p.baptism_note = empty_string
  && p.baptism_src = empty_string
  && p.death = DontKnowIfDead
  && p.death_place = empty_string
  && p.death_note = empty_string
  && p.death_src = empty_string && p.burial = UnknownBurial
  && p.burial_place = empty_string
  && p.burial_note = empty_string
  && p.burial_src = empty_string
  && p.pevents = [] && p.notes = empty_string && p.psources = empty_string

let gc ?(dry_run = true) base =
  base.data.persons.load_array ();
  base.data.ascends.load_array ();
  base.data.unions.load_array ();
  base.data.families.load_array ();
  base.data.couples.load_array ();
  base.data.descends.load_array ();
  base.data.strings.load_array ();
  let mp = Array.make base.data.persons.len false in
  let mf = Array.make base.data.families.len false in
  let ms = Array.make base.data.strings.len false in
  let markp i = Array.set mp i true in
  let markf i = Array.set mf i true in
  let marks i = Array.set ms i true in
  marks 0;
  marks 1;
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    if not (empty_person p) then (
      markp i;
      let _ = Futil.map_person_ps markp marks p in
      let _ = Futil.map_union_f markf @@ base.data.unions.get i in
      let _ = Futil.map_ascend_f markf @@ base.data.ascends.get i in
      ())
  done;
  for i = 0 to base.data.families.len - 1 do
    if Array.get mf i then
      let f = base.data.families.get i in
      (* if family wasn't deleted *)
      if f.fam_index <> dummy_ifam then
        let _ = Futil.map_family_ps markp markf marks f in
        let _ = Futil.map_couple_p false markp @@ base.data.couples.get i in
        let _ = Futil.map_descend_p markp @@ base.data.descends.get i in
        ()
  done;
  (* [p1;p2:p3;p4] [true;false;true;false] -> [0;0;1;1] *)
  let dst_i src m =
    let off = ref 0 in
    Array.init src.len (fun i ->
        if Array.get m i then i - !off
        else (
          incr off;
          i - !off))
  in
  (* 2 [true;false;true;false] -> [0;2]  *)
  let src_i len m =
    let off = ref 0 in
    let a = Array.make len (-1) in
    let rec loop i =
      if i = len then ()
      else if Array.get m (i + !off) then (
        Array.set a i (i + !off);
        loop (i + 1))
      else (
        incr off;
        loop i)
    in
    loop 0;
    a
  in
  let aux arr =
    let rec loop i (sum, acc) =
      if i < 0 then (sum, acc)
      else if Array.get arr i then loop (pred i) (succ sum, acc)
      else loop (pred i) (sum, i :: acc)
    in
    loop (Array.length arr - 1) (0, [])
  in
  let lenp, deletedp = aux mp in
  let lenf, deletedf = aux mf in
  let lens, deleteds = aux ms in
  if dry_run then (deletedp, deletedf, deleteds)
  else
    let dst_ipers = dst_i base.data.persons mp in
    let dst_ifams = dst_i base.data.families mf in
    let dst_istrs = dst_i base.data.strings ms in
    let dst_iper = Array.get dst_ipers in
    let dst_ifam = Array.get dst_ifams in
    let dst_istr = Array.get dst_istrs in
    let src_ipers = src_i lenp mp in
    let src_ifams = src_i lenf mf in
    let src_istrs = src_i lens ms in
    let src_iper = Array.get src_ipers in
    let src_ifam = Array.get src_ifams in
    let src_istr = Array.get src_istrs in
    let persons =
      Array.init lenp @@ fun i ->
      {
        (Futil.map_person_ps dst_iper dst_istr
        @@ base.data.persons.get @@ src_iper i)
        with
        key_index = i;
      }
    in
    let ascends =
      Array.init lenp @@ fun i ->
      Futil.map_ascend_f dst_ifam @@ base.data.ascends.get @@ src_iper i
    in
    let unions =
      Array.init lenp @@ fun i ->
      Futil.map_union_f dst_ifam @@ base.data.unions.get @@ src_iper i
    in
    let families =
      Array.init lenf @@ fun i ->
      Futil.map_family_ps dst_iper (fun _ -> i) dst_istr
      @@ base.data.families.get @@ src_ifam i
    in
    let couples =
      Array.init lenf @@ fun i ->
      Futil.map_couple_p false dst_iper @@ base.data.couples.get @@ src_ifam i
    in
    let descends =
      Array.init lenf @@ fun i ->
      Futil.map_descend_p dst_iper @@ base.data.descends.get @@ src_ifam i
    in
    let strings =
      Array.init lens (fun i -> base.data.strings.get @@ src_istr i)
    in
    let bnotes = base.data.bnotes in
    let particles = base.data.particles_txt in
    let bname = base.data.bdir in
    base.data.persons.clear_array ();
    base.data.ascends.clear_array ();
    base.data.unions.clear_array ();
    base.data.families.clear_array ();
    base.data.couples.clear_array ();
    base.data.descends.clear_array ();
    base.data.strings.clear_array ();
    Database.make bname particles
      ( (persons, ascends, unions),
        (families, couples, descends),
        strings,
        bnotes )
    @@ fun base' ->
    base'.data.persons.load_array ();
    base'.data.ascends.load_array ();
    base'.data.unions.load_array ();
    base'.data.families.load_array ();
    base'.data.couples.load_array ();
    base'.data.descends.load_array ();
    base'.data.strings.load_array ();
    Outbase.output base';
    base'.data.persons.clear_array ();
    base'.data.ascends.clear_array ();
    base'.data.unions.clear_array ();
    base'.data.families.clear_array ();
    base'.data.couples.clear_array ();
    base'.data.descends.clear_array ();
    base'.data.strings.clear_array ();
    (deletedp, deletedf, deleteds)
