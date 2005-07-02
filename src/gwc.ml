(* camlp4r ./pa_lock.cmo *)
(* $Id: gwc.ml,v 4.41 2005-07-02 13:49:13 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;
open Gwcomp;
open Printf;

type gen_min_person 'person 'string =
  { m_first_name : mutable 'string;
    m_surname : mutable 'string;
    m_occ : mutable int;
    m_rparents : mutable list (gen_relation 'person 'string);
    m_related : mutable list iper;
    m_sex : mutable sex;
    m_notes : mutable 'string }
;

type min_person = gen_min_person iper istr;

type cbase =
  { c_persons : mutable array min_person;
    c_ascends : mutable array ascend;
    c_unions : mutable array union;
    c_couples : mutable array couple;
    c_descends : mutable array descend;
    c_strings : mutable array string;
    c_bnotes : notes }
;

type gen =
  { g_strings : mutable Hashtbl.t string istr;
    g_names : mutable Hashtbl.t int iper;
    g_local_names : mutable Hashtbl.t (int * int) iper;
    g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;
    g_base : cbase;
    g_patch_p : Hashtbl.t int person;
    g_def : mutable array bool;
    g_separate : mutable bool;
    g_shift : mutable int;
    g_errored : mutable bool;
    g_per_index : out_channel;
    g_per : out_channel;
    g_fam_index : out_channel;
    g_fam : out_channel }
;

value check_error gen = gen.g_errored := True;

value set_error base gen x =
  do {
    printf "\nError: ";
    Check.print_base_error stdout base x;
    check_error gen;
  }
;

value set_warning base =
  fun
  [ UndefinedSex _ -> ()
  | x ->
      do {
        printf "\nWarning: ";
        Check.print_base_warning stdout base x;
      } ]
;

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

(*
value output_item_value oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]
;
value input_item_value ic =
  input_value ic
;
*)
value output_item_value = Iovalue.output;
value input_item_value = Iovalue.input;
(**)

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
  let _ (*empty*) = unique_string gen "" in
  let cpl = couple False (Adef.iper_of_int 0) (Adef.iper_of_int 0)
  and des = {children = [| |]} in
  (cpl, des)
;

value make_person gen p n occ =
  let empty_string = unique_string gen "" in
  let p =
    {m_first_name = unique_string gen p; m_surname = unique_string gen n;
     m_occ = occ; m_rparents = []; m_related = []; m_sex = Neuter;
     m_notes = empty_string}
  and a = no_ascend ()
  and u = {family = [| |]} in
  (p, a, u)
;

value no_person gen = make_person gen "" "" 0;

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
  if gen.g_fcnt == Array.length gen.g_base.c_couples then do {
    let cpl_arr = gen.g_base.c_couples in
    let des_arr = gen.g_base.c_descends in
    let new_size = 2 * Array.length cpl_arr + 1 in
    let (phony_cpl, phony_des) = no_family gen in
    let new_cpl_arr = Array.create new_size phony_cpl in
    let new_des_arr = Array.create new_size phony_des in
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
  let (x, ip) =
    try
      if key.pk_first_name = "?" || key.pk_surname = "?" then raise Not_found
      else
        let ip =
          find_person_by_name gen key.pk_first_name key.pk_surname occ
        in
        (poi gen.g_base ip, ip)
    with
    [ Not_found ->
        let new_occ =
          if gen.g_separate && key.pk_first_name <> "?" &&
             key.pk_surname <> "?" then
            find_first_available_occ gen key.pk_first_name key.pk_surname occ
          else occ
        in
        let i = gen.g_pcnt in
        let (x, a, u) =
          make_person gen key.pk_first_name key.pk_surname new_occ
        in
        do {
          if key.pk_first_name <> "?" && key.pk_surname <> "?" then
            add_person_by_name gen key.pk_first_name key.pk_surname new_occ
              (Adef.iper_of_int i)
          else ();
          new_iper gen;
          gen.g_base.c_persons.(i) := x;
          gen.g_base.c_ascends.(i) := a;
          gen.g_base.c_unions.(i) := u;
          gen.g_pcnt := gen.g_pcnt + 1;
          if key.pk_first_name <> "?" && key.pk_surname <> "?" then
            let h = person_hash key.pk_first_name key.pk_surname in
            Hashtbl.add gen.g_local_names (h, occ) (Adef.iper_of_int i)
          else ();
          seek_out gen.g_per_index (Iovalue.sizeof_long * i);
          output_binary_int gen.g_per_index (pos_out gen.g_per);
          output_char gen.g_per 'U';
          (x, Adef.iper_of_int i)
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
        gen.g_def.(Adef.int_of_iper ip) := True;
        check_error gen
      }
      else ()
    else ();
    (x, ip)
  }
;

value insert_person gen so =
  let occ = so.occ + gen.g_shift in
  let (x, ip) =
    try
      if so.first_name = "?" || so.surname = "?" then raise Not_found
      else
        let ip = find_person_by_name gen so.first_name so.surname occ in
        (poi gen.g_base ip, ip)
    with
    [ Not_found ->
        let new_occ =
          if gen.g_separate && so.first_name <> "?" && so.surname <> "?" then
            find_first_available_occ gen so.first_name so.surname occ
          else occ
        in
        let i = gen.g_pcnt in
        let (x, a, u) =
          make_person gen so.first_name so.surname new_occ
        in
        do {
          if so.first_name <> "?" && so.surname <> "?" then
            add_person_by_name gen so.first_name so.surname new_occ
              (Adef.iper_of_int i)
          else ();
          new_iper gen;
          gen.g_base.c_persons.(i) := x;
          gen.g_base.c_ascends.(i) := a;
          gen.g_base.c_unions.(i) := u;
          gen.g_pcnt := gen.g_pcnt + 1;
          if so.first_name <> "?" && so.surname <> "?" then
            let h = person_hash so.first_name so.surname in
            Hashtbl.add gen.g_local_names (h, occ) (Adef.iper_of_int i)
          else ();
          (x, Adef.iper_of_int i)
        } ]
  in
  do {
    if gen.g_def.(Adef.int_of_iper ip) then do {
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
      check_error gen
    }
    else gen.g_def.(Adef.int_of_iper ip) := True;
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
        gen.g_def.(Adef.int_of_iper ip) := True;
        check_error gen
      }
      else ()
    else ();
    if not gen.g_errored then do {
      let empty_string = unique_string gen "" in
      let x =
        {first_name = empty_string; surname = empty_string;
         occ = 0; image = unique_string gen so.image;
         first_names_aliases =
           List.map (unique_string gen) so.first_names_aliases;
         surnames_aliases = List.map (unique_string gen) so.surnames_aliases;
         public_name = unique_string gen so.public_name;
         qualifiers = List.map (unique_string gen) so.qualifiers;
         aliases = List.map (unique_string gen) so.aliases;
         titles = List.map (title_unique_string gen) so.titles;
         rparents = []; related = [];
         occupation = unique_string gen so.occupation;
         sex = Neuter; access = so.access;
         birth = so.birth; birth_place = unique_string gen so.birth_place;
         birth_src =  unique_string gen so.birth_src;
         baptism = so.baptism;
         baptism_place = unique_string gen so.baptism_place;
         baptism_src = unique_string gen so.baptism_src;
         death = so.death; death_place = unique_string gen so.death_place;
         death_src = unique_string gen so.death_src; burial = so.burial;
         burial_place = unique_string gen so.burial_place;
         burial_src = unique_string gen so.burial_src;
         notes = empty_string;
         psources =
           unique_string gen
             (if so.psources = "" then default_source.val else so.psources);
         cle_index = ip}
      in
      seek_out gen.g_per_index (Iovalue.sizeof_long * Adef.int_of_iper ip);
      output_binary_int gen.g_per_index (pos_out gen.g_per);
      output_char gen.g_per 'D';
      output_item_value gen.g_per (x : person);
    }
    else ();
    (x, ip)
  }
;

value insert_somebody gen =
  fun
  [ Undefined key -> insert_undefined gen key
  | Defined so -> insert_person gen so ]
;

value verif_parents_non_deja_definis gen ix pere mere =
  let x = poi gen.g_base ix in
  match parents (aoi gen.g_base ix) with
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
        check_error gen
      }
  | _ -> () ]
;

value notice_sex gen p s =
  if p.m_sex == Neuter then p.m_sex := s
  else if p.m_sex == s || s == Neuter then ()
  else do {
    printf "\nInconcistency about the sex of\n  %s %s\n"
      (p_first_name gen.g_base p) (p_surname gen.g_base p);
    check_error gen
  }
;

value insert_family gen co fath_sex moth_sex witl fo deo =
  let (pere, ipere) = insert_somebody gen (father co) in
  let (mere, imere) = insert_somebody gen (mother co) in
  let witl =
    List.map
      (fun (wit, sex) ->
         let (p, ip) = insert_somebody gen wit in
         do {
           notice_sex gen p sex;
           if not (List.mem ipere p.m_related) then
             p.m_related := [ipere :: p.m_related]
           else ();
           ip
         })
      witl
  in
  let children =
    Array.map
      (fun cle ->
         let (e, ie) = insert_person gen cle in
         do { notice_sex gen e cle.sex; ie })
      deo.children
  in
  let comment = unique_string gen fo.comment in
  let fsources =
    unique_string gen
      (if fo.fsources = "" then default_source.val else fo.fsources)
  in
  do {
    new_ifam gen;
    let i = gen.g_fcnt in
    let fam =
      {marriage = fo.marriage;
       marriage_place = unique_string gen fo.marriage_place;
       marriage_src = unique_string gen fo.marriage_src;
       witnesses = Array.of_list witl; relation = fo.relation;
       divorce = fo.divorce; comment = comment;
       origin_file = unique_string gen fo.origin_file; fsources = fsources;
       fam_index = Adef.ifam_of_int i}
    and cpl = couple False ipere imere
    and des = {children = children} in
    let fath_uni = uoi gen.g_base ipere in
    let moth_uni = uoi gen.g_base imere in
    seek_out gen.g_fam_index (Iovalue.sizeof_long * i);
    output_binary_int gen.g_fam_index (pos_out gen.g_fam);
    output_item_value gen.g_fam (fam : family);
    gen.g_base.c_couples.(gen.g_fcnt) := cpl;
    gen.g_base.c_descends.(gen.g_fcnt) := des;
    gen.g_fcnt := gen.g_fcnt + 1;
    fath_uni.family := Array.append fath_uni.family [| Adef.ifam_of_int i |];
    moth_uni.family := Array.append moth_uni.family [| Adef.ifam_of_int i |];
    notice_sex gen pere fath_sex;
    notice_sex gen mere moth_sex;
    Array.iter
      (fun ix ->
         let a = aoi gen.g_base ix in
         do {
           verif_parents_non_deja_definis gen ix pere mere;
           set_parents a (Some (Adef.ifam_of_int i));
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
        check_error gen
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

value insert_bnotes fname gen nfname str =
  let old_nread = gen.g_base.c_bnotes.nread in
  do {
    gen.g_base.c_bnotes.nread :=
      fun f n -> if f = nfname then str else old_nread f n;
    gen.g_base.c_bnotes.norigin_file := fname;
    if nfname <> "" then
      let efiles = gen.g_base.c_bnotes.efiles () in
      gen.g_base.c_bnotes.efiles := fun () -> [nfname :: efiles]
    else ();
  }
;

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value insert_relation_parent gen ip s k =
  let (par, ipar) = insert_somebody gen k in
  do {
    if not (List.mem ip par.m_related) then
      par.m_related := [ip :: par.m_related]
    else ();
    if par.m_sex = Neuter then par.m_sex := s else ();
    ipar
  }
;

value insert_relation gen ip r =
  {r_type = r.r_type;
   r_fath = map_option (insert_relation_parent gen ip Neuter) r.r_fath;
   r_moth = map_option (insert_relation_parent gen ip Neuter) r.r_moth;
   r_sources = unique_string gen r.r_sources}
;

value insert_relations fname gen sb sex rl =
  let (p, ip) = insert_somebody gen sb in
  if p.m_rparents <> [] then do {
    printf "\nFile \"%s\"\n" fname;
    printf "Relations already defined for \"%s%s %s\"\n"
      (sou gen.g_base p.m_first_name)
      (if p.m_occ == 0 then "" else "." ^ string_of_int p.m_occ)
      (sou gen.g_base p.m_surname);
    check_error gen
  }
  else do {
    notice_sex gen p sex;
    let rl = List.map (insert_relation gen ip) rl in
    p.m_rparents := rl
  }
;

value insert_syntax fname gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family gen cpl fs ms witl fam des
  | Notes key str -> insert_notes fname gen key str
  | Relations sb sex rl -> insert_relations fname gen sb sex rl
  | Bnotes nfname str -> insert_bnotes fname gen nfname str ]
;

value insert_comp_families gen (x, separate, shift) =
(**)
  let _ = do { Printf.eprintf "%s " x; flush stderr; } in
(**)
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

value persons_cache gen per_index_ic per_ic persons =
  let read_person_in_temp_file i =
    let mp = persons.(i) in
    do {
      seek_in per_index_ic (Iovalue.sizeof_long * i);
      let pos = input_binary_int per_index_ic in
      seek_in per_ic pos;
      let p : person =
        match input_char per_ic with
        [ 'D' -> input_item_value per_ic
        | 'U' ->
            let empty_string = Adef.istr_of_int 0 in
            {first_name = empty_string; surname = empty_string;
             occ = 0; image = empty_string; first_names_aliases = [];
             surnames_aliases = []; public_name = empty_string;
             qualifiers = [];
             aliases = []; titles = []; rparents = []; related = [];
             occupation = empty_string; sex = Neuter; access = IfTitles;
             birth = Adef.codate_None; birth_place = empty_string;
             birth_src = empty_string; baptism = Adef.codate_None;
             baptism_place = empty_string; baptism_src = empty_string;
             death = DontKnowIfDead; death_place = empty_string;
             death_src = empty_string; burial = UnknownBurial;
             burial_place = empty_string; burial_src = empty_string;
             notes = empty_string; psources = empty_string;
             cle_index = Adef.iper_of_int 0}
        | _ -> assert False ]
      in
      p.first_name := mp.m_first_name;
      p.surname := mp.m_surname;
      p.occ := mp.m_occ;
      p.rparents := mp.m_rparents;
      p.related := mp.m_related;
      p.sex := mp.m_sex;
      p.notes := mp.m_notes;
      p.cle_index := Adef.iper_of_int i;
      p
    }
  in
  let get_fun i =
    try Hashtbl.find gen.g_patch_p i with
    [ Not_found -> read_person_in_temp_file i ]
  in
  {array = fun _ -> failwith "bug: accessing persons array";
   get = get_fun; len = Array.length persons;
   clear_array = fun _ -> ()}
;

value part_file = ref "";

value families_cache fam_index_ic fam_ic len =
  let get_fun i =
    do {
      seek_in fam_index_ic (Iovalue.sizeof_long * i);
      let pos = input_binary_int fam_index_ic in
      seek_in fam_ic pos;
      let fam : family = input_item_value fam_ic in
      fam
    }
  in
  {array = fun _ -> failwith "bug: accessing family array";
   get = get_fun; len = len; clear_array = fun _ -> ()}
;

value input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur ";
     "AF "; "D'"; "DAL "; "DE "; "DI "; "DU "; "OF "; "VAN ";
     "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Iobase.input_particles part_file
;

value empty_base : cbase =
  {c_persons = [| |]; c_ascends = [| |]; c_unions = [| |];
   c_couples = [| |]; c_descends = [| |]; c_strings = [| |];
   c_bnotes = {nread = fun _ _ -> ""; norigin_file = ""; efiles _ = []}}
;

value linked_base gen per_index_ic per_ic fam_index_ic fam_ic : Def.base =
(**)
  let _ =
    do {
      Printf.eprintf "pcnt %d persons %d\n" gen.g_pcnt
        (Array.length gen.g_base.c_persons);
      flush stderr;
    }
  in
(**)
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
(**)
  let _ =
    do {
      Printf.eprintf "fcnt %d families %d\n" gen.g_fcnt
        (Array.length gen.g_base.c_couples);
      flush stderr;
    }
  in
(**)
  let couples =
    let a = Array.sub gen.g_base.c_couples 0 gen.g_fcnt in
    do { gen.g_base.c_couples := [| |]; a }
  in
  let descends =
    let a = Array.sub gen.g_base.c_descends 0 gen.g_fcnt in
    do { gen.g_base.c_descends := [| |]; a }
  in
(**)
  let _ =
    do {
      Printf.eprintf "scnt %d strings %d\n" gen.g_scnt
        (Array.length gen.g_base.c_strings);
      flush stderr;
    }
  in
(**)
  let strings =
    let a = Array.sub gen.g_base.c_strings 0 gen.g_scnt in
    do { gen.g_base.c_strings := [| |]; a }
  in
  let particles = input_particles part_file.val in
  let bnotes = gen.g_base.c_bnotes in
  let base_data =
    {persons = persons_cache gen per_index_ic per_ic persons;
     ascends = cache_of ascends;
     unions = cache_of unions;
     families = families_cache fam_index_ic fam_ic gen.g_fcnt;
     visible = { v_write = fun []; v_get = fun [] };
     couples = cache_of couples; descends = cache_of descends;
     strings = cache_of strings; particles = particles; bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = fun []; strings_of_fsname = fun [];
     index_of_string = fun []; persons_of_surname = no_istr_iper_index;
     persons_of_first_name = no_istr_iper_index;
     patch_person = fun []; patch_ascend = fun []; patch_union = fun [];
     patch_family = fun []; patch_couple = fun []; patch_descend = fun [];
     patch_string = fun []; patch_name = fun []; commit_patches = fun [];
     commit_notes = fun []; patched_ascends = fun [];
     is_patched_person _ = False; cleanup = fun () -> ()}
  in
  {data = base_data; func = base_func}
;

value link gwo_list bname =
  let gwb_dir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let tmp_per_index = Filename.concat gwb_dir "gwc_per_index" in
  let tmp_per = Filename.concat gwb_dir "gwc_per" in
  let tmp_fam_index = Filename.concat gwb_dir "gwc_fam_index" in
  let tmp_fam = Filename.concat gwb_dir "gwc_fam" in
  do {
    try Unix.mkdir gwb_dir 0o755 with _ -> ();
    let gen =
      {g_strings = Hashtbl.create 20011; g_names = Hashtbl.create 20011;
       g_local_names = Hashtbl.create 20011; g_pcnt = 0; g_fcnt = 0;
       g_scnt = 0; g_base = empty_base; g_patch_p = Hashtbl.create 20011;
       g_def = [| |]; g_separate = False; g_shift = 0; g_errored = False;
       g_per_index = open_out_bin tmp_per_index;
       g_per = open_out_bin tmp_per;
       g_fam_index = open_out_bin tmp_fam_index;
       g_fam = open_out_bin tmp_fam }
    in
    let per_index_ic = open_in_bin tmp_per_index in
    let per_ic = open_in_bin tmp_per in
    let fam_index_ic = open_in_bin tmp_fam_index in
    let fam_ic = open_in_bin tmp_fam in
    let istr_empty = unique_string gen "" in
    let istr_quest = unique_string gen "?" in
    assert (istr_empty = Adef.istr_of_int 0);
    assert (istr_quest = Adef.istr_of_int 1);
    IFDEF UNIX THEN Sys.remove tmp_per_index ELSE () END;
    IFDEF UNIX THEN Sys.remove tmp_per ELSE () END;
    IFDEF UNIX THEN Sys.remove tmp_fam_index ELSE () END;
    IFDEF UNIX THEN Sys.remove tmp_fam ELSE () END;
    List.iter (insert_comp_families gen) gwo_list;
    Printf.eprintf "\n"; flush stderr;
    close_out gen.g_per_index;
    close_out gen.g_per;
    close_out gen.g_fam_index;
    close_out gen.g_fam;
    let base = linked_base gen per_index_ic per_ic fam_index_ic fam_ic in
    let changed_p p =
      let i = Adef.int_of_iper p.cle_index in
      Hashtbl.replace gen.g_patch_p i p
    in
    if do_check.val && gen.g_pcnt > 0 then do {
      Check.check_base base (set_error base gen) (set_warning base)
        (fun i -> gen.g_def.(i)) changed_p pr_stats.val;
      flush stdout;
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

value output_particles_file bname particles =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let oc = open_out (Filename.concat bdir "particles.txt") in
  do {
    List.iter (fun s -> fprintf oc "%s\n" (Gutil.tr ' ' '_' s)) particles;
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
     <str> Set the source field for persons and families without source data");
   ("-part", Arg.String (fun s -> part_file.val := s), "\
     <file> Particles file (default = predefined particles)");
   ("-mem", Arg.Set Iobase.save_mem, " Save memory, but slower");
   ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
   ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.")]
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
          let base = link (List.rev gwo.val) out_file.val in
          do {
            Gc.compact ();
            Iobase.output out_file.val base;
            output_command_line out_file.val;
            output_particles_file out_file.val base.data.particles;
          }
      | Refuse ->
          do {
            printf "Base is locked: cannot write it\n";
            flush stdout;
            exit 2
          } ];
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
