(* camlp4r ./pa_lock.cmo *)
(* $Id: gwc.ml,v 4.21 2004-08-05 14:01:30 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Check;
open Gutil;
open Gwcomp;
open Printf;

value default_source = ref "";

value poi base i = base.c_persons.(Adef.int_of_iper i);
value aoi base i = base.c_ascends.(Adef.int_of_iper i);
value uoi base i = base.c_unions.(Adef.int_of_iper i);
value coi base i = base.c_couples.(Adef.int_of_ifam i);
value sou base i = base.c_strings.(Adef.int_of_istr i);
value p_first_name base p = nominative (sou base p.m_first_name);
value p_surname base p = nominative (sou base p.m_surname);
value designation base p =
  let prenom = p_first_name base p in
  let nom = p_surname base p in
  prenom ^ "." ^ string_of_int p.m_occ ^ " " ^ nom
;

value check_magic =
  let b = String.create (String.length magic_gwo) in
  fun fname ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_gwo then
        if String.sub magic_gwo 0 4 = String.sub b 0 4 then
          failwith
            ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
        else
          failwith
            ("\"" ^ fname ^
               "\" is not a GeneWeb object file, or it is a very old version")
      else ()
    }
;

value no_string gen = "";

value unique_string gen x =
  try Hashtbl.find gen.g_strings x with
  [ Not_found ->
      do {
        if gen.g_scnt == Array.length gen.g_base.c_strings then do {
          let arr = gen.g_base.c_strings in
          let new_size = 2 * Array.length arr + 1 in
          let new_arr = Array.create new_size (no_string gen) in
          Array.blit arr 0 new_arr 0 (Array.length arr);
          gen.g_base.c_strings := new_arr;
        }
        else ();
        let u = Adef.istr_of_int gen.g_scnt in
        gen.g_base.c_strings.(gen.g_scnt) := x;
        gen.g_scnt := gen.g_scnt + 1;
        Hashtbl.add gen.g_strings x u;
        u
      } ]
;

value no_family gen =
  let empty = unique_string gen "" in
  let fam =
    {marriage = Adef.codate_None; marriage_place = empty;
     marriage_src = empty; witnesses = [| |]; relation = Married;
     divorce = NotDivorced; comment = empty; origin_file = empty;
     fsources = empty; fam_index = Adef.ifam_of_int 0}
  and cpl = couple False (Adef.iper_of_int 0) (Adef.iper_of_int 0)
  and des = {children = [| |]} in
  (fam, cpl, des)
;

value make_person gen p n occ i =
  let empty_string = unique_string gen "" in
  let p =
    {m_first_name = unique_string gen p; m_surname = unique_string gen n;
     m_occ = occ; m_rparents = []; m_related = []; m_sex = Neuter;
     m_notes = empty_string; m_cle_index = Adef.iper_of_int i}
  and a = no_ascend ()
  and u = {family = [| |]} in
  (p, a, u)
;

value no_person gen = make_person gen "" "" 0 0;

value new_iper gen =
  if gen.g_pcnt == Array.length gen.g_base.c_persons then do {
    let per_arr = gen.g_base.c_persons in
    let asc_arr = gen.g_base.c_ascends in
    let uni_arr = gen.g_base.c_unions in
    let new_size = 2 * Array.length per_arr + 1 in
    let (per_bidon, asc_bidon, uni_bidon) = no_person gen in
    let new_per_arr = Array.create new_size per_bidon in
    let new_asc_arr = Array.create new_size asc_bidon in
    let new_uni_arr = Array.create new_size uni_bidon in
    let new_def = Array.create new_size False in
    Array.blit per_arr 0 new_per_arr 0 (Array.length per_arr);
    gen.g_base.c_persons := new_per_arr;
    Array.blit asc_arr 0 new_asc_arr 0 (Array.length asc_arr);
    gen.g_base.c_ascends := new_asc_arr;
    Array.blit uni_arr 0 new_uni_arr 0 (Array.length uni_arr);
    gen.g_base.c_unions := new_uni_arr;
    Array.blit gen.g_def 0 new_def 0 (Array.length gen.g_def);
    gen.g_def := new_def;
  }
  else ()
;

value new_ifam gen =
  if gen.g_fcnt == Array.length gen.g_base.c_families then do {
    let fam_arr = gen.g_base.c_families in
    let cpl_arr = gen.g_base.c_couples in
    let des_arr = gen.g_base.c_descends in
    let new_size = 2 * Array.length fam_arr + 1 in
    let (phony_fam, phony_cpl, phony_des) = no_family gen in
    let new_fam_arr = Array.create new_size phony_fam in
    let new_cpl_arr = Array.create new_size phony_cpl in
    let new_des_arr = Array.create new_size phony_des in
    Array.blit fam_arr 0 new_fam_arr 0 (Array.length fam_arr);
    gen.g_base.c_families := new_fam_arr;
    Array.blit cpl_arr 0 new_cpl_arr 0 (Array.length cpl_arr);
    gen.g_base.c_couples := new_cpl_arr;
    Array.blit des_arr 0 new_des_arr 0 (Array.length des_arr);
    gen.g_base.c_descends := new_des_arr;
  }
  else ()
;

value title_name_unique_string gen =
  fun
  [ Tmain -> Tmain
  | Tname n -> Tname (unique_string gen n)
  | Tnone -> Tnone ]
;

value title_unique_string gen t =
  {t_name = title_name_unique_string gen t.t_name;
   t_ident = unique_string gen t.t_ident;
   t_place = unique_string gen t.t_place; t_date_start = t.t_date_start;
   t_date_end = t.t_date_end; t_nth = t.t_nth}
;

value person_hash first_name surname =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in Hashtbl.hash s
;

value find_person_by_global_name gen first_name surname occ =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_names key in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop =
    fun
    [ [] -> raise Not_found
    | [ip :: ipl] ->
        let p = poi gen.g_base ip in
        if Name.lower (p_first_name gen.g_base p) = first_name &&
           Name.lower (p_surname gen.g_base p) = surname && p.m_occ == occ then
          ip
        else loop ipl ]
  in
  loop ipl
;

value find_person_by_local_name gen first_name surname occ =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_local_names (key, occ) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop =
    fun
    [ [] -> raise Not_found
    | [ip :: ipl] ->
        let p = poi gen.g_base ip in
        if Name.lower (p_first_name gen.g_base p) = first_name &&
           Name.lower (p_surname gen.g_base p) = surname then
          ip
        else loop ipl ]
  in
  loop ipl
;

value find_person_by_name gen first_name surname occ =
  if gen.g_separate then find_person_by_local_name gen first_name surname occ
  else find_person_by_global_name gen first_name surname occ
;

value add_person_by_name gen first_name surname occ iper =
  let s = Name.crush_lower (nominative (first_name ^ " " ^ surname)) in
  let key = Hashtbl.hash s in Hashtbl.add gen.g_names key iper
;

value find_first_available_occ gen fn sn occ =
  loop occ where rec loop occ =
    match
      try Some (find_person_by_global_name gen fn sn occ) with
      [ Not_found -> None ]
    with
    [ Some _ -> loop (occ + 1)
    | None -> occ ]
;

value insert_undefined gen key =
  let occ = key.pk_occ + gen.g_shift in
  let x =
    try
      if key.pk_first_name = "?" || key.pk_surname = "?" then raise Not_found
      else
        let x =
          find_person_by_name gen key.pk_first_name key.pk_surname occ
        in
        poi gen.g_base x
    with
    [ Not_found ->
        let new_occ =
          if gen.g_separate && key.pk_first_name <> "?" &&
             key.pk_surname <> "?" then
            find_first_available_occ gen key.pk_first_name key.pk_surname occ
          else occ
        in
        let (x, a, u) =
          make_person gen key.pk_first_name key.pk_surname new_occ gen.g_pcnt
        in
        do {
          if key.pk_first_name <> "?" && key.pk_surname <> "?" then
            add_person_by_name gen key.pk_first_name key.pk_surname new_occ
              x.m_cle_index
          else ();
          new_iper gen;
          gen.g_base.c_persons.(gen.g_pcnt) := x;
          gen.g_base.c_ascends.(gen.g_pcnt) := a;
          gen.g_base.c_unions.(gen.g_pcnt) := u;
          gen.g_pcnt := gen.g_pcnt + 1;
          let h = person_hash key.pk_first_name key.pk_surname in
          Hashtbl.add gen.g_local_names (h, occ) x.m_cle_index;
          x
        } ]
  in
  do {
    if not gen.g_errored then
      if sou gen.g_base x.m_first_name <> key.pk_first_name ||
         sou gen.g_base x.m_surname <> key.pk_surname then
         do {
        printf "\nPerson defined with two spellings:\n";
        printf "  \"%s%s %s\"\n" key.pk_first_name
          (match x.m_occ with
           [ 0 -> ""
           | n -> "." ^ string_of_int n ])
          key.pk_surname;
        printf "  \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
           [ 0 -> ""
           | n -> "." ^ string_of_int n ])
          (p_surname gen.g_base x);
        gen.g_def.(Adef.int_of_iper x.m_cle_index) := True;
        Check.error gen
      }
      else ()
    else ();
    x
  }
;

value insert_person gen so =
  let occ = so.occ + gen.g_shift in
  let x =
    try
      if so.first_name = "?" || so.surname = "?" then raise Not_found
      else
        let x = find_person_by_name gen so.first_name so.surname occ in
        poi gen.g_base x
    with
    [ Not_found ->
        let new_occ =
          if gen.g_separate && so.first_name <> "?" && so.surname <> "?" then
            find_first_available_occ gen so.first_name so.surname occ
          else occ
        in
        let (x, a, u) =
          make_person gen so.first_name so.surname new_occ gen.g_pcnt
        in
        do {
          if so.first_name <> "?" && so.surname <> "?" then
            add_person_by_name gen so.first_name so.surname new_occ
              x.m_cle_index
          else ();
          new_iper gen;
          gen.g_base.c_persons.(gen.g_pcnt) := x;
          gen.g_base.c_ascends.(gen.g_pcnt) := a;
          gen.g_base.c_unions.(gen.g_pcnt) := u;
          gen.g_pcnt := gen.g_pcnt + 1;
          if so.first_name <> "?" && so.surname <> "?" then
            let h = person_hash so.first_name so.surname in
            Hashtbl.add gen.g_local_names (h, occ) x.m_cle_index
          else ();
          x
        } ]
  in
  do {
    if gen.g_def.(Adef.int_of_iper x.m_cle_index) then do {
      printf "\nPerson already defined: \"%s%s %s\"\n" so.first_name
        (match x.m_occ with
         [ 0 -> ""
         | n -> "." ^ string_of_int n ])
        so.surname;
      if p_first_name gen.g_base x <> so.first_name ||
         p_surname gen.g_base x <> so.surname then
        printf "as name: \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
           [ 0 -> ""
           | n -> "." ^ string_of_int n ])
          (p_surname gen.g_base x)
      else ();
(*
      x.birth := Adef.codate_None;
      x.death := DontKnowIfDead;
*)
      Check.error gen
    }
    else gen.g_def.(Adef.int_of_iper x.m_cle_index) := True;
    if not gen.g_errored then
      if sou gen.g_base x.m_first_name <> so.first_name ||
         sou gen.g_base x.m_surname <> so.surname then
         do {
        printf "\nPerson defined with two spellings:\n";
        printf "  \"%s%s %s\"\n" so.first_name
          (match x.m_occ with
           [ 0 -> ""
           | n -> "." ^ string_of_int n ])
          so.surname;
        printf "  \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
           [ 0 -> ""
           | n -> "." ^ string_of_int n ])
          (p_surname gen.g_base x);
        gen.g_def.(Adef.int_of_iper x.m_cle_index) := True;
        Check.error gen
      }
      else ()
    else ();
    if not gen.g_errored then do {
      let empty_string = unique_string gen "" in
      let x =
        {first_name = empty_string; surname = empty_string;
         occ = 0; image = empty_string; first_names_aliases = [];
         surnames_aliases = []; public_name = empty_string; qualifiers = [];
         aliases = []; titles = []; rparents = []; related = [];
         occupation = empty_string; sex = Neuter; access = IfTitles;
         birth = Adef.codate_None; birth_place = empty_string;
         birth_src = empty_string; baptism = Adef.codate_None;
         baptism_place = empty_string; baptism_src = empty_string;
         death = DontKnowIfDead; death_place = empty_string;
         death_src = empty_string; burial = UnknownBurial;
         burial_place = empty_string; burial_src = empty_string;
         notes = empty_string; psources = empty_string;
         cle_index = x.m_cle_index}
      in
      x.birth := so.birth;
      x.birth_place := unique_string gen so.birth_place;
      x.birth_src := unique_string gen so.birth_src;
      x.baptism := so.baptism;
      x.baptism_place := unique_string gen so.baptism_place;
      x.baptism_src := unique_string gen so.baptism_src;
      x.death := so.death;
      x.death_place := unique_string gen so.death_place;
      x.death_src := unique_string gen so.death_src;
      x.burial := so.burial;
      x.burial_place := unique_string gen so.burial_place;
      x.burial_src := unique_string gen so.burial_src;
      x.first_names_aliases :=
        List.map (unique_string gen) so.first_names_aliases;
      x.surnames_aliases := List.map (unique_string gen) so.surnames_aliases;
      x.public_name := unique_string gen so.public_name;
      x.image := unique_string gen so.image;
      x.qualifiers := List.map (unique_string gen) so.qualifiers;
      x.aliases := List.map (unique_string gen) so.aliases;
      x.titles := List.map (title_unique_string gen) so.titles;
      x.access := so.access;
      x.occupation := unique_string gen so.occupation;
      x.psources :=
        unique_string gen
          (if so.psources = "" then default_source.val else so.psources);
      seek_out gen.g_per_index
	(Iovalue.sizeof_long * (Adef.int_of_iper x.cle_index));
      output_binary_int gen.g_per_index (pos_out gen.g_per);
      output_value gen.g_per (x : person);
    }
    else ();
    x
  }
;

value insert_somebody gen =
  fun
  [ Undefined key -> insert_undefined gen key
  | Defined so -> insert_person gen so ]
;

value verif_parents_non_deja_definis gen x pere mere =
  match parents (aoi gen.g_base x.m_cle_index) with
  [ Some ifam ->
      let cpl = coi gen.g_base ifam in
      let p = (father cpl) in
      let m = (mother cpl) in
      do {
        printf "
I cannot add \"%s\", child of
    - \"%s\"
    - \"%s\",
because this persons still exists as child of
    - \"%s\"
    - \"%s\".
" (designation gen.g_base x) (designation gen.g_base pere)
          (designation gen.g_base mere)
          (designation gen.g_base (poi gen.g_base p))
          (designation gen.g_base (poi gen.g_base m));
        flush stdout;
(*
        x.birth := Adef.codate_None;
        x.death := DontKnowIfDead;
*)
        Check.error gen
      }
  | _ -> () ]
;

value notice_sex gen p s =
  if p.m_sex == Neuter then p.m_sex := s
  else if p.m_sex == s || s == Neuter then ()
  else do {
    printf "\nInconcistency about the sex of\n  %s %s\n"
      (p_first_name gen.g_base p) (p_surname gen.g_base p);
    Check.error gen
  }
;

value insert_family gen co fath_sex moth_sex witl fo deo =
  let pere = insert_somebody gen (father co) in
  let mere = insert_somebody gen (mother co) in
  let witl =
    List.map
      (fun (wit, sex) ->
         let p = insert_somebody gen wit in
         do {
           notice_sex gen p sex;
           if not (List.mem pere.m_cle_index p.m_related) then
             p.m_related := [pere.m_cle_index :: p.m_related]
           else ();
           p.m_cle_index
         })
      witl
  in
  let children =
    Array.map
      (fun cle ->
         let e = insert_person gen cle in
         do { notice_sex gen e cle.sex; e.m_cle_index })
      deo.children
  in
  let comment = unique_string gen fo.comment in
  let fsources =
    unique_string gen
      (if fo.fsources = "" then default_source.val else fo.fsources)
  in
  do {
    new_ifam gen;
    let fam =
      {marriage = fo.marriage;
       marriage_place = unique_string gen fo.marriage_place;
       marriage_src = unique_string gen fo.marriage_src;
       witnesses = Array.of_list witl; relation = fo.relation;
       divorce = fo.divorce; comment = comment;
       origin_file = unique_string gen fo.origin_file; fsources = fsources;
       fam_index = Adef.ifam_of_int gen.g_fcnt}
    and cpl = couple False pere.m_cle_index mere.m_cle_index
    and des = {children = children} in
    let fath_uni = uoi gen.g_base pere.m_cle_index in
    let moth_uni = uoi gen.g_base mere.m_cle_index in
    gen.g_base.c_families.(gen.g_fcnt) := fam;
    gen.g_base.c_couples.(gen.g_fcnt) := cpl;
    gen.g_base.c_descends.(gen.g_fcnt) := des;
    gen.g_fcnt := gen.g_fcnt + 1;
    fath_uni.family := Array.append fath_uni.family [| fam.fam_index |];
    moth_uni.family := Array.append moth_uni.family [| fam.fam_index |];
    notice_sex gen pere fath_sex;
    notice_sex gen mere moth_sex;
    Array.iter
      (fun ix ->
         let x = poi gen.g_base ix in
         let a = aoi gen.g_base ix in
         do {
           verif_parents_non_deja_definis gen x pere mere;
           set_parents a (Some fam.fam_index);
           ()
         })
      children;
  }
;

value insert_notes fname gen key str =
  let occ = key.pk_occ + gen.g_shift in
  match
    try
      Some (find_person_by_name gen key.pk_first_name key.pk_surname occ)
    with
    [ Not_found -> None ]
  with
  [ Some ip ->
      let p = poi gen.g_base ip in
      if sou gen.g_base p.m_notes <> "" then do {
        printf "\nFile \"%s\"\n" fname;
        printf "Notes already defined for \"%s%s %s\"\n"
          key.pk_first_name (if occ == 0 then "" else "." ^ string_of_int occ)
          key.pk_surname;
        Check.error gen
      }
      else p.m_notes := unique_string gen str
  | None ->
      do {
        printf "File \"%s\"\n" fname;
        printf "*** warning: undefined person: \"%s%s %s\"\n"
          key.pk_first_name (if occ == 0 then "" else "." ^ string_of_int occ)
          key.pk_surname;
        flush stdout;
      } ]
;

value insert_bnotes fname gen str =
  do {
    gen.g_base.c_bnotes.nread := fun _ -> str;
    gen.g_base.c_bnotes.norigin_file := fname;
  }
;

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value insert_relation_parent gen p s k =
  let par = insert_somebody gen k in
  do {
    if not (List.mem p.m_cle_index par.m_related) then
      par.m_related := [p.m_cle_index :: par.m_related]
    else ();
    if par.m_sex = Neuter then par.m_sex := s else ();
    par.m_cle_index
  }
;

value insert_relation gen p r =
  {r_type = r.r_type;
   r_fath = map_option (insert_relation_parent gen p Neuter) r.r_fath;
   r_moth = map_option (insert_relation_parent gen p Neuter) r.r_moth;
   r_sources = unique_string gen r.r_sources}
;

value insert_relations fname gen sb sex rl =
  let p = insert_somebody gen sb in
  if p.m_rparents <> [] then do {
    printf "\nFile \"%s\"\n" fname;
    printf "Relations already defined for \"%s%s %s\"\n"
      (sou gen.g_base p.m_first_name)
      (if p.m_occ == 0 then "" else "." ^ string_of_int p.m_occ)
      (sou gen.g_base p.m_surname);
    Check.error gen
  }
  else do {
    notice_sex gen p sex;
    let rl = List.map (insert_relation gen p) rl in
    p.m_rparents := rl
  }
;

value insert_syntax fname gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family gen cpl fs ms witl fam des
  | Notes key str -> insert_notes fname gen key str
  | Relations sb sex rl -> insert_relations fname gen sb sex rl
  | Bnotes str -> insert_bnotes fname gen str ]
;

value insert_comp_families gen (x, separate, shift) =
  let ic = open_in_bin x in
  do {
    check_magic x ic;
    gen.g_separate := separate;
    gen.g_shift := shift;
    Hashtbl.clear gen.g_local_names;
    let src : string = input_value ic in
    try
      while True do {
        let fam : syntax_o = input_value ic in insert_syntax src gen fam
      }
    with
    [ End_of_file -> close_in ic ]
  }
;

value just_comp = ref False;
value do_check = ref True;
value out_file = ref (Filename.concat Filename.current_dir_name "a");
value force = ref False;
value do_consang = ref False;
value pr_stats = ref False;

value cache_of tab =
  let c =
    {array = fun _ -> tab; get = fun []; len = Array.length tab;
     clear_array = fun _ -> ()}
  in
  do { c.get := fun i -> (c.array ()).(i); c }
;

value no_istr_iper_index = {find = fun []; cursor = fun []; next = fun []};

value persons_cache per_index_ic per_ic persons =
  let get_fun i =
    let mp = persons.(i) in
    do {
      seek_in per_index_ic
        (Iovalue.sizeof_long * Adef.int_of_iper mp.m_cle_index);
      let pos = input_binary_int per_index_ic in
      seek_in per_ic pos;
      let p : person = input_value per_ic in
      p.first_name := mp.m_first_name;
      p.surname := mp.m_surname;
      p.occ := mp.m_occ;
      p.rparents := mp.m_rparents;
      p.related := mp.m_related;
      p.sex := mp.m_sex;
      p.notes := mp.m_notes;
      p.cle_index := mp.m_cle_index;
      p
    }
  in
  {array = fun _ -> failwith "bug: accessing persons array";
   get = get_fun; len = Array.length persons;
   clear_array = fun _ -> ()}
;

value empty_base : Check.cbase =
  {c_persons = [| |]; c_ascends = [| |];
   c_unions = [| |]; c_families = [| |];
   c_couples = [| |]; c_descends = [| |];
   c_strings = [| |];
   c_bnotes = {nread = fun _ -> ""; norigin_file = ""}}
;

value linked_base gen per_index_ic per_ic : Def.base =
  let persons =
    let a = Array.sub gen.g_base.c_persons 0 gen.g_pcnt in
    do { gen.g_base.c_persons := [| |]; a }
  in
  let ascends =
    let a = Array.sub gen.g_base.c_ascends 0 gen.g_pcnt in
    do { gen.g_base.c_ascends := [| |]; a }
  in
  let unions =
    let a = Array.sub gen.g_base.c_unions 0 gen.g_pcnt in
    do { gen.g_base.c_unions := [| |]; a }
  in
  let families =
    let a = Array.sub gen.g_base.c_families 0 gen.g_fcnt in
    do { gen.g_base.c_families := [| |]; a }
  in
  let couples =
    let a = Array.sub gen.g_base.c_couples 0 gen.g_fcnt in
    do { gen.g_base.c_couples := [| |]; a }
  in
  let descends =
    let a = Array.sub gen.g_base.c_descends 0 gen.g_fcnt in
    do { gen.g_base.c_descends := [| |]; a }
  in
  let strings =
    let a = Array.sub gen.g_base.c_strings 0 gen.g_scnt in
    do { gen.g_base.c_strings := [| |]; a }
  in
  let bnotes = gen.g_base.c_bnotes in
  let base_data =
    {persons = persons_cache per_index_ic per_ic persons;
     ascends = cache_of ascends;
     unions = cache_of unions; families = cache_of families;
     visible = { v_write = fun []; v_get = fun [] };
     couples = cache_of couples; descends = cache_of descends;
     strings = cache_of strings; bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = fun []; strings_of_fsname = fun [];
     index_of_string = fun []; persons_of_surname = no_istr_iper_index;
     persons_of_first_name = no_istr_iper_index;
     patch_person = fun []; patch_ascend = fun []; patch_union = fun [];
     patch_family = fun []; patch_couple = fun []; patch_descend = fun [];
     patch_string = fun []; patch_name = fun []; commit_patches = fun [];
     commit_notes = fun []; patched_ascends = fun []; cleanup = fun () -> ()}
  in
  {data = base_data; func = base_func}
;

value link gwo_list =
  let gen =
    {g_strings = Hashtbl.create 20011; g_names = Hashtbl.create 20011;
     g_local_names = Hashtbl.create 20011; g_pcnt = 0; g_fcnt = 0;
     g_scnt = 0; g_base = empty_base; g_def = [| |]; g_separate = False;
     g_shift = 0; g_errored = False;
     g_per_index = open_out_bin "gwc_per_index";
     g_per = open_out_bin "gwc_per" }
  in
  let per_index_ic = open_in_bin "gwc_per_index" in
  let per_ic = open_in_bin "gwc_per" in
  let istr_empty = unique_string gen "" in
  let istr_quest = unique_string gen "?" in
  do {
    assert (istr_empty = Adef.istr_of_int 0);
    assert (istr_quest = Adef.istr_of_int 1);
    ifdef UNIX then Sys.remove "gwc_per_index" else ();
    ifdef UNIX then Sys.remove "gwc_per" else ();
    List.iter (insert_comp_families gen) gwo_list;
    close_out gen.g_per_index;
    close_out gen.g_per;
    let base = linked_base gen per_index_ic per_ic in
    if do_check.val && gen.g_pcnt > 0 then do {
      Check.check_base base gen pr_stats.val; flush stdout;
    }
    else ();
    if not gen.g_errored then
      if do_consang.val then ConsangAll.compute base True False else ()
    else exit 2;
    base
  }
;

value output_command_line bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let oc = open_out (Filename.concat bdir "command.txt") in
  do {
    fprintf oc "%s" Sys.argv.(0);
    for i = 1 to Array.length Sys.argv - 1 do {
      fprintf oc " %s" Sys.argv.(i)
    };
    fprintf oc "\n";
    close_out oc;
  }
;

value separate = ref False;
value shift = ref 0;
value files = ref [];

value speclist =
  [("-c", Arg.Set just_comp, "Only compiling");
   ("-o", Arg.String (fun s -> out_file.val := s),
    "<file> Output database (default: a.gwb)");
   ("-f", Arg.Set force, " Remove database if already existing");
   ("-stats", Arg.Set pr_stats, "Print statistics");
   ("-nc", Arg.Clear do_check, "No consistency check");
   ("-cg", Arg.Set do_consang, "Compute consanguinity");
   ("-sep", Arg.Set separate, " Separate all persons in next file");
   ("-sh", Arg.Int (fun x -> shift.val := x),
    "<int> Shift all persons numbers in next files");
   ("-ds", Arg.String (fun s -> default_source.val := s), "\
     set the source field for persons and families without source data");
   ("-mem", Arg.Set Iobase.save_mem, " Save memory, but slower");
   ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.")]
;

value anonfun x =
  let sep = separate.val in
  do {
    if Filename.check_suffix x ".gw" then ()
    else if Filename.check_suffix x ".gwo" then ()
    else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
    separate.val := False;
    files.val := [(x, sep, shift.val) :: files.val]
  }
;

value errmsg =
  "\
Usage: gwc [options] [files]
where [files] are a list of files:
  source files end with .gw
  object files end with .gwo
and [options] are:"
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    Secure.set_base_dir (Filename.dirname out_file.val);
    let gwo = ref [] in
    List.iter
      (fun (x, separate, shift) ->
         if Filename.check_suffix x ".gw" then do {
           try Gwcomp.comp_families x with e ->
             do {
               printf "File \"%s\", line %d:\n" x line_cnt.val; raise e
             };
           gwo.val := [(x ^ "o", separate, shift) :: gwo.val];
         }
         else if Filename.check_suffix x ".gwo" then
           gwo.val := [(x, separate, shift) :: gwo.val]
         else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
      (List.rev files.val);
    if not just_comp.val then do {
      let bdir =
        if Filename.check_suffix out_file.val ".gwb" then out_file.val
        else out_file.val ^ ".gwb"
      in
      if not force.val && Sys.file_exists bdir then do {
        printf "\
The database \"%s\" already exists. Use option -f to overwrite it.
" out_file.val;
        flush stdout;
        exit 2
      }
      else ();
      lock (Iobase.lock_file out_file.val) with
      [ Accept ->
          let base = link (List.rev gwo.val) in
          do { Gc.compact (); Iobase.output out_file.val base; }
      | Refuse ->
          do {
            printf "Base is locked: cannot write it\n";
            flush stdout;
            exit 2
          } ];
      output_command_line out_file.val;
      ()
    }
    else ();
  }
;

value print_exc =
  fun
  [ Failure txt ->
      do { printf "Failed: %s\n" txt; flush stdout; exit 2 }
  | exc -> Printexc.catch raise exc ]
;

try main () with exc -> print_exc exc;
