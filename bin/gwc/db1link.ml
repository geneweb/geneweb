(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Gwcomp
open Def
module Driver = Geneweb_db.Driver

(* From OCaml manual, integer in binary format is 4 bytes long. *)
let sizeof_long = 4

(** Default source field for persons and families without source data *)
let default_source = ref ""

(** Base consistency check *)
let do_check = ref true

(** Compute consanguinity *)
let do_consang = ref false

(** Print base's statistics *)
let pr_stats = ref false

type person = (int, int, int) Def.gen_person
(** Extended person's entry in the base *)

type ascend = int Def.gen_ascend
(** Person's ascendants entry in the base *)

type union = int Def.gen_union
(** Person's union entry in the base *)

type family = (int, int, int) Def.gen_family
(** Family's entry in the base *)

type couple = int Def.gen_couple
(** Family's couple entry in the base *)

type descend = int Def.gen_descend
(** Family's descendants entry in the base *)

type ('person, 'string) gen_min_person = {
  mutable m_first_name : 'string;
  mutable m_surname : 'string;
  mutable m_occ : int;
  mutable m_rparents : ('person, 'string) gen_relation list;
  mutable m_related : int list;
  mutable m_pevents : ('person, 'string) gen_pers_event list;
  mutable m_sex : sex;
  mutable m_notes : 'string;
}
(** Restricted to the minimum [Def.gen_person] data type. *)

type min_person = (int, int) gen_min_person
(** Person's entry in the base *)

type cbase = {
  (* Array of persons. Person at position [i] has corresponding to him [ascend]
     and [union] at position [i] in [c_ascends] and [c_unions] respectively. *)
  mutable c_persons : min_person array; (* Array of ascendants of persons *)
  mutable c_ascends : ascend array; (* Array of unions of persons *)
  mutable c_unions : union array;
      (* Array of families. Family at position [i] has corresponding to it [couple]
         and [descend] at position [i] in [c_couples] and [c_descends] respectively. *)
  mutable c_families : family array;  (** Array of couples of families *)
  mutable c_couples : couple array; (* Array of descendants of families *)
  mutable c_descends : descend array;
      (* Array of unique strings. Stores every string encoded information
         (like person's name, bithplace, etc.) for other entries in the base. *)
  mutable c_strings : string array;
      (* Data base notes and extended page structure *)
  mutable c_bnotes : Def.base_notes;
}
(** State of the base collecting all information at link time used to create
    further Geneweb database *)

type file_info = {
  (* current .gw filename *)
  mutable f_curr_src_file : string; (* current .gwo filename *)
  mutable f_curr_gwo_file : string;
      (* all persons from current file should be separated  *)
  mutable f_separate : bool; (* behavior for base notes from current file *)
  mutable f_bnotes : [ `merge | `erase | `first | `drop ];
      (* shift all persons from the current file with the given number *)
  mutable f_shift : int;
      (* Table that associates person's names hash and its occurence number
         with the index of person's entry inside the [base]. Contains only
         persons from the local file. *)
  mutable f_local_names : (int * int, int) Hashtbl.t;
}
(** Information about current .gwo file. *)

type gen = {
  (* Table that associates unique string to its position inside
     [g_base]'s unique string array *)
  mutable g_strings : (string, int) Hashtbl.t;
  (* Table that associates person's names hash with the index of
     person's entry inside the [g_base].*)
  mutable g_names : (int, int) Hashtbl.t;
  (* Counter of persons inside [g_base] *)
  mutable g_pcnt : int;
  (* Counter of families inside [g_base] *)
  mutable g_fcnt : int;
  (* Counter of unique strings inside [g_base] *)
  mutable g_scnt : int;
  (* Current file info *)
  g_file_info : file_info;
  (* Base of collected information *)
  g_base : cbase;
  (* Wizard notes (wizard id and note's content) *)
  mutable g_wiznotes : (string * string) list;
  g_patch_p : (int, person) Hashtbl.t;
      (** Array that for every person from [g_base] says if he was defined
          before *)
  mutable g_def : bool array;
  (* Table that associates person's first and last name with
     the next availiable occurence number for the person with
     the same names.*)
  g_first_av_occ : (string * string, int) Hashtbl.t;
  (* Indicates if an error was occured  *)
  mutable g_errored : bool;
  (* Temprary output chanel containing [g_pcnt] integers where [i]nth integer
     corresponds to the position in [g_per] where [i]nth person is defined. *)
  g_per_index : out_channel;
  (* Temprary output chanel containing person's definition (or non-definition
     marker) *)
  g_per : out_channel;
  (* Temprary output chanel containing [g_fcnt] integers where [i]nth integer
     corresponds to the position in [g_fam] where [i]nth family is defined. *)
  g_fam_index : out_channel;
  (* Temprary output chanel containing family's definition *)
  g_fam : out_channel;
}
(** Global linker state *)

(** Set [gen.g_errored] telling that an error was occured *)
let check_error gen = gen.g_errored <- true

(** Function that will be called if base's checker will find an error *)
let set_error base gen x =
  Printf.printf "\nError: ";
  Check.print_base_error stdout base x;
  check_error gen

(** Function that will be called if base's checker will find a warning *)
let set_warning base x =
  Printf.printf "Warning: ";
  Check.print_base_warning stdout base x

(** Returns person's entry from [base] at position [i] *)
let poi base i = base.c_persons.(i)

(** Returns ascendant's entry from [base] at position [i] *)
let aoi base i = base.c_ascends.(i)

(** Returns union's entry from [base] at position [i] *)
let uoi base i = base.c_unions.(i)

(** Returns couple's entry from [base] at position [i] *)
let coi base i = base.c_couples.(i)

(** Returns string in [base]'s unque string array at position [i] *)
let sou base i = base.c_strings.(i)

(** Returns first name of a [base]'s person entry [p]. [p.m_first_name] contains
    index where first name string representation is stored. *)
let p_first_name base p = Mutil.nominative (sou base p.m_first_name)

(** Returns surname of a [base]'s person entry [p]. [p.m_first_name] contains
    index where surname string representation is stored. *)
let p_surname base p = Mutil.nominative (sou base p.m_surname)

(** Returns string designation of person {i firstname.occ surname}. *)
let designation base p =
  let prenom = p_first_name base p in
  let nom = p_surname base p in
  prenom ^ "." ^ string_of_int p.m_occ ^ " " ^ nom

(** Same as [Marshal.to_channel oc v [Marshal.No_sharing]] *)
let output_item_value oc v = Marshal.to_channel oc v [ Marshal.No_sharing ]

(** Same as [input_value] *)
let input_item_value ic = input_value ic

(** Empty string *)
let no_string = ""

(** Stores unique string (if not already present) inside the base's string array
    and associate this string to its index in mentioned array. Extens array if
    needed. Returns associated index. *)
let unique_string gen x =
  try Hashtbl.find gen.g_strings x
  with Not_found ->
    (* string not found *)
    if gen.g_scnt = Array.length gen.g_base.c_strings then (
      (* extend arrray of strings and copy previus elements *)
      let arr = gen.g_base.c_strings in
      let new_size = (2 * Array.length arr) + 1 in
      let new_arr = Array.make new_size no_string in
      Array.blit arr 0 new_arr 0 (Array.length arr);
      gen.g_base.c_strings <- new_arr);
    let u = gen.g_scnt in
    gen.g_base.c_strings.(gen.g_scnt) <- x;
    gen.g_scnt <- gen.g_scnt + 1;
    Hashtbl.add gen.g_strings x u;
    u

(** Dummy [family] with its empty [couple] and [descendants]. *)
let no_family gen =
  let empty_string = unique_string gen "" in
  let fam =
    {
      marriage = Date.cdate_None;
      marriage_place = empty_string;
      marriage_note = empty_string;
      marriage_src = empty_string;
      witnesses = [||];
      relation = NoMention;
      divorce = NotDivorced;
      fevents = [];
      comment = empty_string;
      origin_file = empty_string;
      fsources = empty_string;
      fam_index = -1;
    }
  in
  let cpl = Adef.couple 0 0 in
  let des = { children = [||] } in
  (fam, cpl, des)

(** Initialises [min_person] with occurence number and index of [p] for first
    name and index of [n] for surname in [base]. Other fields are initialised
    with default value. Returns also empty [ascend] and [union] attached to the
    considered person. *)
let make_person gen p n occ : min_person * ascend * union =
  let empty_string = unique_string gen "" in
  let p =
    {
      m_first_name = unique_string gen p;
      m_surname = unique_string gen n;
      m_occ = occ;
      m_rparents = [];
      m_related = [];
      m_pevents = [];
      m_sex = Neuter;
      m_notes = empty_string;
    }
  and a = { parents = None; consang = Adef.fix (-1) }
  and u = { family = [||] } in
  (p, a, u)

(** Dummy [min_person] with its empty [ascend] and [union]. *)
let no_person gen = make_person gen "" "" 0

(** Extends person's acendant's and union's arrays inside [gen.g_base] if
    needed. *)
let new_iper gen =
  if gen.g_pcnt = Array.length gen.g_base.c_persons then (
    let per_arr = gen.g_base.c_persons in
    let asc_arr = gen.g_base.c_ascends in
    let uni_arr = gen.g_base.c_unions in
    let new_size = (2 * Array.length per_arr) + 1 in
    let phony_per, phony_asc, phony_uni = no_person gen in
    let new_per_arr = Array.make new_size phony_per in
    let new_asc_arr = Array.make new_size phony_asc in
    let new_uni_arr = Array.make new_size phony_uni in
    let new_def = Array.make new_size false in
    Array.blit per_arr 0 new_per_arr 0 (Array.length per_arr);
    gen.g_base.c_persons <- new_per_arr;
    Array.blit asc_arr 0 new_asc_arr 0 (Array.length asc_arr);
    gen.g_base.c_ascends <- new_asc_arr;
    Array.blit uni_arr 0 new_uni_arr 0 (Array.length uni_arr);
    gen.g_base.c_unions <- new_uni_arr;
    Array.blit gen.g_def 0 new_def 0 (Array.length gen.g_def);
    gen.g_def <- new_def)

(** Extends family's couple's and decendant's arrays inside [gen.g_base] if
    needed. *)
let new_ifam gen =
  if gen.g_fcnt = Array.length gen.g_base.c_families then (
    let fam_arr = gen.g_base.c_families in
    let cpl_arr = gen.g_base.c_couples in
    let des_arr = gen.g_base.c_descends in
    let new_size = (2 * Array.length cpl_arr) + 1 in
    let phony_fam, phony_cpl, phony_des = no_family gen in
    let new_fam_arr = Array.make new_size phony_fam in
    let new_cpl_arr = Array.make new_size phony_cpl in
    let new_des_arr = Array.make new_size phony_des in
    Array.blit fam_arr 0 new_fam_arr 0 (Array.length fam_arr);
    gen.g_base.c_families <- new_fam_arr;
    Array.blit cpl_arr 0 new_cpl_arr 0 (Array.length cpl_arr);
    gen.g_base.c_couples <- new_cpl_arr;
    Array.blit des_arr 0 new_des_arr 0 (Array.length des_arr);
    gen.g_base.c_descends <- new_des_arr)

(** Convert [string Def.gen_title_name] to [int Def.gen_title_name]. If title is
    [Tname] stores title name as a string in the base. *)
let title_name_unique_string gen = function
  | Tmain -> Tmain
  | Tname n -> Tname (unique_string gen n)
  | Tnone -> Tnone

(** Convert [(string Def.gen_title] to [int Def.gen_title] and insert all
    related to title information in the base. *)
let title_unique_string gen t =
  {
    t_name = title_name_unique_string gen t.t_name;
    t_ident = unique_string gen t.t_ident;
    t_place = unique_string gen t.t_place;
    t_date_start = t.t_date_start;
    t_date_end = t.t_date_end;
    t_nth = t.t_nth;
  }

(** Hash of person's first and last names. *)
let person_hash first_name surname =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  Hashtbl.hash s

(** Returns index of a person's entry inside the [gen.base] that has the same
    first name, surname and occurence number. Raises [Not_found] if person is
    not found. *)
let find_person_by_global_name gen first_name surname occ =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_names key in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop = function
    | [] -> raise Not_found
    | ip :: ipl ->
        let p = poi gen.g_base ip in
        (* refine search by fullnames comparison (without crushlower) and with
           occurence comparison *)
        if
          p.m_occ = occ
          && Name.lower (p_first_name gen.g_base p) = first_name
          && Name.lower (p_surname gen.g_base p) = surname
        then ip
        else loop ipl
  in
  loop ipl

(** Returns index of a person's entry inside the [gen.base] that has the same
    first name, surname and occurence number. Searches only persons defined in
    the current file. Raises [Not_found] if person is not found. *)
let find_person_by_local_name gen first_name surname occ =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_file_info.f_local_names (key, occ) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop = function
    | [] -> raise Not_found
    | ip :: ipl ->
        let p = poi gen.g_base ip in
        (* refine search by fullnames comparison (without crushlower) *)
        if
          Name.lower (p_first_name gen.g_base p) = first_name
          && Name.lower (p_surname gen.g_base p) = surname
        then ip
        else loop ipl
  in
  loop ipl

(** Returns index of a person's entry inside the [gen.base] that has the same
    first name, surname and occurence number. Calls [find_person_by_local_name]
    if option [f_separate] is enabled for the current file, otherwise calls
    [find_person_by_global_name]. Raises [Not_found] if person is not found. *)
let find_person_by_name gen first_name surname occ =
  if gen.g_file_info.f_separate then
    find_person_by_local_name gen first_name surname occ
  else find_person_by_global_name gen first_name surname occ

(** Add entry in the global names table [gen.g_names] for the giving first and
    last names associated to the index of their person's entry in [base]. *)
let add_person_by_name gen first_name surname int =
  let s = Name.crush_lower (Mutil.nominative (first_name ^ " " ^ surname)) in
  let key = Hashtbl.hash s in
  Hashtbl.add gen.g_names key int

(** Returns first available occurence number that is >= [occ] for the person
    with the giving information. *)
let find_first_available_occ gen fn sn occ =
  let occ =
    try max occ (Hashtbl.find gen.g_first_av_occ (fn, sn))
    with Not_found -> occ
  in
  let rec loop occ =
    match
      try Some (find_person_by_global_name gen fn sn occ)
      with Not_found -> None
    with
    | Some _ -> loop (occ + 1)
    | None ->
        Hashtbl.add gen.g_first_av_occ (fn, sn) occ;
        occ
  in
  loop occ

(** Insert person's reference in the base and modifies all coresponding fields
    in [gen] and returns its entry and entry's index in the base. In details:

    - if considered person doesn't exists in the base (wasn't defined or
      referenced before) then function:

    - maps its key (names and occurence number) within the varius hash tables
    - creates entry (of type [min_gen]) for the giving person and his ascendants
      and union in the base.
    - initialises its entry (with key information)
    - stores marker in the [gen.g_per] channel telling that person wasn't
      defined.
    - stores in [gen.g_per_index] position where marker was stored in
      [gen.g_per].

    - if considered person was referenced or defined before then doesn't do
      anything (just returns its entry and entry's index in the base) *)
let insert_undefined gen key =
  (* shift person's occurence *)
  let occ = key.pk_occ + gen.g_file_info.f_shift in
  (* person with its position in the base *)
  let x, ip =
    try
      if key.pk_first_name = "?" || key.pk_surname = "?" then raise Not_found
      else
        let ip = find_person_by_name gen key.pk_first_name key.pk_surname occ in
        (poi gen.g_base ip, ip) (* if person not found *)
    with Not_found ->
      (* available occurence number *)
      let new_occ =
        if
          gen.g_file_info.f_separate && key.pk_first_name <> "?"
          && key.pk_surname <> "?"
        then find_first_available_occ gen key.pk_first_name key.pk_surname occ
        else occ
      in
      (* person's entry index *)
      let i = gen.g_pcnt in
      let x, a, u = make_person gen key.pk_first_name key.pk_surname new_occ in
      (* strore names globally *)
      if key.pk_first_name <> "?" && key.pk_surname <> "?" then
        add_person_by_name gen key.pk_first_name key.pk_surname i
      else if !Gwcomp.create_all_keys then
        add_person_by_name gen key.pk_first_name key.pk_surname i;
      (* extend arrays if needed *)
      new_iper gen;
      (* add person to array *)
      gen.g_base.c_persons.(i) <- x;
      (* add associated to person ascendants to array *)
      gen.g_base.c_ascends.(i) <- a;
      (* add associated to person union to array *)
      gen.g_base.c_unions.(i) <- u;
      gen.g_pcnt <- gen.g_pcnt + 1;
      (* strore names locally *)
      (if key.pk_first_name <> "?" && key.pk_surname <> "?" then
         let h = person_hash key.pk_first_name key.pk_surname in
         Hashtbl.add gen.g_file_info.f_local_names (h, occ) i);
      (* write start position of person in [g_per] *)
      seek_out gen.g_per_index (sizeof_long * i);
      output_binary_int gen.g_per_index (pos_out gen.g_per);
      (* write marker *)
      output_char gen.g_per 'U';
      (x, i)
  in
  if not gen.g_errored then
    if
      sou gen.g_base x.m_first_name <> key.pk_first_name
      || sou gen.g_base x.m_surname <> key.pk_surname
    then (
      Printf.printf "\nError: Person defined with two spellings:\n";
      Printf.printf "  \"%s%s %s\"\n" key.pk_first_name
        (match x.m_occ with 0 -> "" | n -> "." ^ string_of_int n)
        key.pk_surname;
      Printf.printf "  \"%s%s %s\"\n"
        (p_first_name gen.g_base x)
        (match occ with 0 -> "" | n -> "." ^ string_of_int n)
        (p_surname gen.g_base x);
      gen.g_def.(ip) <- true;
      check_error gen);
  (x, ip)

(** Insert person's definition in the base and modifies all coresponding fields
    in [gen] and returns its entry and entry's index in the base. In details:

    - if considered person doesn't exists in the base (wasn't defined or
      referenced before) then function:

    - maps its key (names and occurence number) within the varius hash tables
    - creates entry (of type [min_gen]) for the giving person and his ascendants
      and union in the base.
    - initialises its entry (with key information)
    - marks it as defined
    - convert [(_,_,string) gen_person] to [person] (sex, events, titles and
      related persons stays uninitialised) and stores it in the [gen.g_per]
      channel.
    - stores in [gen.g_per_index] position where person was stored in
      [gen.g_per].

    - if considered person was referenced before (but not defined) then
      function:

    - get person's entry and its index from the base
    - marks it as defined
    - convert [(_,_,string) gen_person] to [person] (sex, events and related
      persons stays uninitialised) and stores it in the [gen.g_per] channel.
    - updates previus index in [gen.g_per_index] in order to point to the
      definition instead of pointing to the reference. *)
let insert_person gen so =
  (* shift person's occurence *)
  let occ = so.occ + gen.g_file_info.f_shift in
  (* person with its position in the base *)
  let x, ip =
    try
      if so.first_name = "?" || so.surname = "?" then raise Not_found
      else
        let ip = find_person_by_name gen so.first_name so.surname occ in
        (poi gen.g_base ip, ip) (* if person not found *)
    with Not_found ->
      (* available occurence number *)
      let new_occ =
        if
          gen.g_file_info.f_separate && so.first_name <> "?"
          && so.surname <> "?"
        then find_first_available_occ gen so.first_name so.surname occ
        else occ
      in
      (* person's entry index *)
      let i = gen.g_pcnt in
      let x, a, u = make_person gen so.first_name so.surname new_occ in
      (* strore names globally *)
      if so.first_name <> "?" && so.surname <> "?" then
        add_person_by_name gen so.first_name so.surname i
      else if !Gwcomp.create_all_keys then
        add_person_by_name gen so.first_name so.surname i;
      (* extend arrays if needed *)
      new_iper gen;
      (* add person to array *)
      gen.g_base.c_persons.(i) <- x;
      (* add associated to person ascendants to array *)
      gen.g_base.c_ascends.(i) <- a;
      (* add associated to person union to array *)
      gen.g_base.c_unions.(i) <- u;
      gen.g_pcnt <- gen.g_pcnt + 1;
      (* strore names locally *)
      (if so.first_name <> "?" && so.surname <> "?" then
         let h = person_hash so.first_name so.surname in
         Hashtbl.add gen.g_file_info.f_local_names (h, occ) i);
      (x, i)
  in
  (* if person wad defined before (not just referenced) *)
  if gen.g_def.(ip) then (
    (* print error about person beeing already defined *)
    Printf.printf "\nError: Person already defined: \"%s%s %s\"\n" so.first_name
      (match x.m_occ with 0 -> "" | n -> "." ^ string_of_int n)
      so.surname;
    if
      p_first_name gen.g_base x <> so.first_name
      || p_surname gen.g_base x <> so.surname
    then
      Printf.printf "as name: \"%s%s %s\"\n"
        (p_first_name gen.g_base x)
        (match occ with 0 -> "" | n -> "." ^ string_of_int n)
        (p_surname gen.g_base x);
    flush stdout;
    check_error gen)
  else (* else set it as defined *)
    gen.g_def.(ip) <- true;
  if not gen.g_errored then
    if
      sou gen.g_base x.m_first_name <> so.first_name
      || sou gen.g_base x.m_surname <> so.surname
    then (
      (* print error about person defined with two spellings *)
      Printf.printf "\nError: Person defined with two spellings:\n";
      Printf.printf "  \"%s%s %s\"\n" so.first_name
        (match x.m_occ with 0 -> "" | n -> "." ^ string_of_int n)
        so.surname;
      Printf.printf "  \"%s%s %s\"\n"
        (p_first_name gen.g_base x)
        (match occ with 0 -> "" | n -> "." ^ string_of_int n)
        (p_surname gen.g_base x);
      gen.g_def.(ip) <- true;
      check_error gen);
  if not gen.g_errored then (
    let empty_string = unique_string gen "" in
    (* Convert [(_,_,string) gen_person] to [person]. Save all strings in base *)
    let x =
      {
        first_name = empty_string;
        surname = empty_string;
        occ = 0;
        image = unique_string gen so.image;
        first_names_aliases =
          List.map (unique_string gen) so.first_names_aliases;
        surnames_aliases = List.map (unique_string gen) so.surnames_aliases;
        public_name = unique_string gen so.public_name;
        qualifiers = List.map (unique_string gen) so.qualifiers;
        aliases = List.map (unique_string gen) so.aliases;
        titles = List.map (title_unique_string gen) so.titles;
        rparents = [];
        related = [];
        occupation = unique_string gen so.occupation;
        sex = Neuter;
        access = so.access;
        birth = so.birth;
        birth_place = unique_string gen so.birth_place;
        birth_note = unique_string gen so.birth_note;
        birth_src = unique_string gen so.birth_src;
        baptism = so.baptism;
        baptism_place = unique_string gen so.baptism_place;
        baptism_note = unique_string gen so.baptism_note;
        baptism_src = unique_string gen so.baptism_src;
        death = so.death;
        death_place = unique_string gen so.death_place;
        death_note = unique_string gen so.death_note;
        death_src = unique_string gen so.death_src;
        burial = so.burial;
        burial_place = unique_string gen so.burial_place;
        burial_note = unique_string gen so.burial_note;
        burial_src = unique_string gen so.burial_src;
        pevents = [];
        notes = empty_string;
        psources =
          unique_string gen
            (if so.psources = "" then !default_source else so.psources);
        key_index = ip;
      }
    in
    (* write/update start position of person in [g_per] *)
    seek_out gen.g_per_index (sizeof_long * ip);
    output_binary_int gen.g_per_index (pos_out gen.g_per);
    (* write person *)
    output_char gen.g_per 'D';
    output_item_value gen.g_per (x : person));
  (x, ip)

(** Insert definition or reference in [gen] and returns its entry and entry's
    index in the [gen.g_base]. Calls [insert_person] for definition and
    [insert_undefined] for reference. *)
let insert_somebody gen = function
  | Undefined key -> insert_undefined gen key
  | Defined so -> insert_person gen so

(** Checks if children [ix] doesn't have another parents *)
let check_parents_not_already_defined gen ix fath moth =
  let x = poi gen.g_base ix in
  match (aoi gen.g_base ix).parents with
  | Some int ->
      let cpl = coi gen.g_base int in
      let p = Adef.father cpl in
      let m = Adef.mother cpl in
      Printf.printf
        "\n\
         Error: I cannot add \"%s\", child of\n\
        \    - \"%s\"\n\
        \    - \"%s\",\n\
         because this persons still exists as child of\n\
        \    - \"%s\"\n\
        \    - \"%s\"."
        (designation gen.g_base x)
        (designation gen.g_base fath)
        (designation gen.g_base moth)
        (designation gen.g_base (poi gen.g_base p))
        (designation gen.g_base (poi gen.g_base m));
      flush stdout;
      (*
              x.birth := Adef.cdate_None;
              x.death := DontKnowIfDead;
      *)
      check_error gen
  | _ -> ()

(** Assign sex to the person's entry if it's unitialised. Print message if sexes
    are different. *)
let notice_sex gen p s =
  if p.m_sex = Neuter then p.m_sex <- s
  else if p.m_sex <> s && s <> Neuter then
    Printf.printf "\nInconsistency about the sex of\n  %s %s\n"
      (p_first_name gen.g_base p)
      (p_surname gen.g_base p)

(** Convert [string Def.gen_fam_event_name] to [int Def.gen_fam_event_name]. If
    event is [Efam_Name] stores event name as a string in the base. *)
let fevent_name_unique_string gen = function
  | ( Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
    | Efam_Divorce | Efam_Separated | Efam_Annulation | Efam_MarriageBann
    | Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS | Efam_Residence
      ) as evt ->
      evt
  | Efam_Name n -> Efam_Name (unique_string gen n)

(** Update family by looking up information inferred from family events *)
let update_family_with_fevents _gen fam =
  let found_marriage = ref false in
  let found_divorce = ref false in
  let found_separation = ref false in
  let nsck_std_fields =
    match fam.relation with
    | NoSexesCheckNotMarried | NoSexesCheckMarried -> true
    | _ -> false
  in
  (* On veut cette fois ci que ce soit le dernier évènement *)
  (* qui soit mis dans les évènements principaux.           *)
  let convert relation =
    match relation with
    | Efam_Marriage ->
        if nsck_std_fields then Some NoSexesCheckMarried else Some Married
    | Efam_NoMarriage ->
        if nsck_std_fields then Some NoSexesCheckNotMarried else Some NotMarried
    | Efam_Engage -> Some Engaged
    | Efam_NoMention -> Some NoMention
    | Efam_MarriageBann -> Some MarriageBann
    | Efam_MarriageContract -> Some MarriageContract
    | Efam_MarriageLicense -> Some MarriageLicense
    | Efam_PACS -> Some Pacs
    | Efam_Residence -> Some Residence
    | _ -> None
  in
  let rec loop fevents fam =
    match fevents with
    | [] -> fam
    | evt :: l -> (
        match convert evt.efam_name with
        | Some relation' ->
            if !found_marriage then loop l fam
            else
              let witnesses = Array.map fst evt.efam_witnesses in
              let fam =
                {
                  fam with
                  relation = relation';
                  marriage = evt.efam_date;
                  marriage_place = evt.efam_place;
                  marriage_note = evt.efam_note;
                  marriage_src = evt.efam_src;
                  witnesses;
                }
              in
              let () = found_marriage := true in
              loop l fam
        | None -> (
            match evt.efam_name with
            | Efam_Divorce ->
                if !found_divorce then loop l fam
                else
                  let fam = { fam with divorce = Divorced evt.efam_date } in
                  let () = found_divorce := true in
                  loop l fam
            | Efam_Separated ->
                if !found_separation then loop l fam
                else
                  let fam = { fam with divorce = Separated evt.efam_date } in
                  let () = found_separation := true in
                  loop l fam
            | _ -> loop l fam))
  in
  loop (List.rev fam.fevents) fam

(** Update family event list by looking up inferred family information. *)
let update_fevents_with_family gen fam =
  let empty_string = 0 in
  let evt_marr =
    let name =
      match fam.relation with
      | Married -> Efam_Marriage
      | NotMarried -> Efam_NoMarriage
      | Engaged -> Efam_Engage
      | NoSexesCheckNotMarried -> Efam_NoMarriage
      | NoMention -> Efam_NoMention
      | NoSexesCheckMarried -> Efam_Marriage
      | MarriageBann -> Efam_MarriageBann
      | MarriageContract -> Efam_MarriageContract
      | MarriageLicense -> Efam_MarriageLicense
      | Pacs -> Efam_PACS
      | Residence -> Efam_Residence
    in
    let witnesses = Array.map (fun ip -> (ip, Witness)) fam.witnesses in
    let evt =
      {
        efam_name = name;
        efam_date = fam.marriage;
        efam_place = fam.marriage_place;
        efam_reason = empty_string;
        efam_note = fam.marriage_note;
        efam_src = fam.marriage_src;
        efam_witnesses = witnesses;
      }
    in
    Some evt
  in
  let evt_div =
    match fam.divorce with
    | NotDivorced -> None
    | Divorced cd ->
        let evt =
          {
            efam_name = Efam_Divorce;
            efam_date = cd;
            efam_place = unique_string gen "";
            efam_reason = unique_string gen "";
            efam_note = unique_string gen "";
            efam_src = unique_string gen "";
            efam_witnesses = [||];
          }
        in
        Some evt
    | NotSeparated -> None
    | Separated_old ->
        let evt =
          {
            efam_name = Efam_Separated;
            efam_date = Date.cdate_None;
            efam_place = unique_string gen "";
            efam_reason = unique_string gen "";
            efam_note = unique_string gen "";
            efam_src = unique_string gen "";
            efam_witnesses = [||];
          }
        in
        Some evt
    | Separated cd ->
        let evt =
          {
            efam_name = Efam_Separated;
            efam_date = cd;
            efam_place = unique_string gen "";
            efam_reason = unique_string gen "";
            efam_note = unique_string gen "";
            efam_src = unique_string gen "";
            efam_witnesses = [||];
          }
        in
        Some evt
  in
  let fevents = [ evt_marr; evt_div ] in
  let fevents =
    List.fold_right
      (fun evt fevents ->
        match evt with Some evt -> evt :: fevents | None -> fevents)
      fevents []
  in
  { fam with fevents }

(** Insert family in the base and modifies all coresponding fields in [gen] and
    returns its entry and entry's index in the base. In details function does:

    - inserts father and mother in the person's base
    - insert every witness in the person's base and associate father as a
      related person.
    - order, convert, adjust and insert events
    - insert every children in the person's base
    - creates entry (of type [family]) for the giving family and its couple and
      descendants in the base.
    - associate father's and mother's union to the current family
    - associate every children's ascendants to the current family (current
      couple, since it has the same index)
    - stores family in the [gen.g_fam] channel.
    - stores in [gen.g_fam_index] position where person was stored in
      [gen.g_index]. *)
let insert_family gen co fath_sex moth_sex witl fevtl fo deo =
  let fath, ifath, moth, imoth =
    match
      ( insert_somebody gen (Adef.father co),
        insert_somebody gen (Adef.mother co) )
    with
    (* Look for inverted WIFE/HUSB *)
    | (fath, ifath), (moth, imoth) when fath.m_sex = Female && moth.m_sex = Male
      ->
        (moth, imoth, fath, ifath)
    | (fath, ifath), (moth, imoth) -> (fath, ifath, moth, imoth)
  in
  (* insert all family witnesses *)
  let witl =
    List.map
      (fun (wit, sex) ->
        let p, ip = insert_somebody gen wit in
        notice_sex gen p sex;
        (* add father to witness' related persons *)
        p.m_related <- ifath :: p.m_related;
        ip)
      witl
  in
  (* Events are sorted by chronological order (if equal then by alphabetical order) *)
  let fevents =
    Event.sort_events
      (fun (name, _, _, _, _, _, _) -> Event.Fevent name)
      (fun (_, date, _, _, _, _, _) -> date)
      fevtl
  in
  (* Create [int Def.gen_fam_event_name] list from [fevents]*)
  let fevents =
    List.map
      (fun (name, date, place, reason, src, notes, witl) ->
        (* insert all event witnesses *)
        let witnesses =
          List.map
            (fun (wit, sex, wk) ->
              let p, ip = insert_somebody gen wit in
              notice_sex gen p sex;
              p.m_related <- ifath :: p.m_related;
              (ip, wk))
            witl
        in
        {
          efam_name = fevent_name_unique_string gen name;
          efam_date = date;
          efam_place = unique_string gen place;
          efam_reason = unique_string gen reason;
          efam_note = unique_string gen notes;
          efam_src = unique_string gen src;
          efam_witnesses = Array.of_list witnesses;
        })
      fevents
  in
  (* insert all children *)
  let children =
    Array.map
      (fun key ->
        let e, ie = insert_person gen key in
        notice_sex gen e key.sex;
        ie)
      deo.children
  in
  (* insert family comment *)
  let comment = unique_string gen fo.comment in
  (* insert sources comment *)
  let fsources =
    unique_string gen
      (if fo.fsources = "" then !default_source else fo.fsources)
  in
  (* extend arrays if needed *)
  new_ifam gen;
  (* family's entry index *)
  let i = gen.g_fcnt in
  (* Convert [(_,_,string) gen_family] to [family]. Save all strings in base *)
  let fam =
    {
      marriage = fo.marriage;
      marriage_place = unique_string gen fo.marriage_place;
      marriage_note = unique_string gen fo.marriage_note;
      marriage_src = unique_string gen fo.marriage_src;
      witnesses = Array.of_list witl;
      relation = fo.relation;
      divorce = fo.divorce;
      fevents;
      comment;
      origin_file = unique_string gen fo.origin_file;
      fsources;
      fam_index = i;
    }
  (* create couple *)
  and cpl = Adef.couple ifath imoth
  (* created descandants *)
  and des = { children } in
  (* On mets à jour les fevents et events normaux *)
  let fam =
    if fevents <> [] then update_family_with_fevents gen fam
    else update_fevents_with_family gen fam
  in
  (* father's union *)
  let fath_uni = uoi gen.g_base ifath in
  (* mother's union *)
  let moth_uni = uoi gen.g_base imoth in
  (* write start position of family in [g_fam] *)
  seek_out gen.g_fam_index (sizeof_long * i);
  output_binary_int gen.g_fam_index (pos_out gen.g_fam);
  (* write family *)
  output_item_value gen.g_fam (fam : family);
  (* add family to array *)
  gen.g_base.c_families.(gen.g_fcnt) <- fam;
  (* add couple to array *)
  gen.g_base.c_couples.(gen.g_fcnt) <- cpl;
  (* add descendants to array *)
  gen.g_base.c_descends.(gen.g_fcnt) <- des;
  gen.g_fcnt <- gen.g_fcnt + 1;
  (* append this family to father's and mother's union  *)
  let fath_uni = { family = Array.append fath_uni.family [| i |] } in
  gen.g_base.c_unions.(ifath) <- fath_uni;
  let moth_uni = { family = Array.append moth_uni.family [| i |] } in
  gen.g_base.c_unions.(imoth) <- moth_uni;
  notice_sex gen fath fath_sex;
  notice_sex gen moth moth_sex;
  (* Append familly to the children's ascendant *)
  Array.iter
    (fun ix ->
      let a = gen.g_base.c_ascends.(ix) in
      (* check if children has no another parents *)
      check_parents_not_already_defined gen ix fath moth;
      let a = { a with parents = Some i } in
      gen.g_base.c_ascends.(ix) <- a)
    children

(** Convert [string Def.gen_pers_event_name] to [int Def.gen_pers_event_name].
    If event is [Epers_Name] stores event name as a string in the base. *)
let pevent_name_unique_string gen = function
  | ( Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial | Epers_Cremation
    | Epers_Accomplishment | Epers_Acquisition | Epers_Adhesion
    | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah | Epers_Benediction
    | Epers_ChangeName | Epers_Circumcision | Epers_Confirmation
    | Epers_ConfirmationLDS | Epers_Decoration | Epers_DemobilisationMilitaire
    | Epers_Diploma | Epers_Distinction | Epers_Dotation | Epers_DotationLDS
    | Epers_Education | Epers_Election | Epers_Emigration
    | Epers_Excommunication | Epers_FamilyLinkLDS | Epers_FirstCommunion
    | Epers_Funeral | Epers_Graduate | Epers_Hospitalisation | Epers_Illness
    | Epers_Immigration | Epers_ListePassenger | Epers_MilitaryDistinction
    | Epers_MilitaryPromotion | Epers_MilitaryService
    | Epers_MobilisationMilitaire | Epers_Naturalisation | Epers_Occupation
    | Epers_Ordination | Epers_Property | Epers_Recensement | Epers_Residence
    | Epers_Retired | Epers_ScellentChildLDS | Epers_ScellentParentLDS
    | Epers_ScellentSpouseLDS | Epers_VenteBien | Epers_Will ) as evt ->
      evt
  | Epers_Name n -> Epers_Name (unique_string gen n)

(** Insert all related to the event information and add it to the person's entry
    in the [gen.g_base] *)
let insert_pevents fname gen sb pevtl =
  (* insert concered person *)
  let p, ip = insert_somebody gen sb in
  if p.m_pevents <> [] then (
    Printf.printf "\nFile \"%s\"" fname;
    Printf.printf "\nError: Individual events already defined for \"%s%s %s\"\n"
      (sou gen.g_base p.m_first_name)
      (if p.m_occ = 0 then "" else "." ^ string_of_int p.m_occ)
      (sou gen.g_base p.m_surname);
    check_error gen)
  else
    (* sort evenets *)
    let pevents =
      Event.sort_events
        (fun (name, _, _, _, _, _, _) -> Event.Pevent name)
        (fun (_, date, _, _, _, _, _) -> date)
        pevtl
    in
    (* convert evenets. Insert all related to evenet information in the base *)
    let pevents =
      List.map
        (fun (name, date, place, reason, src, notes, witl) ->
          let witnesses =
            List.map
              (fun (wit, sex, wk) ->
                (* insert witnesses *)
                let wp, wip = insert_somebody gen wit in
                notice_sex gen wp sex;
                (* add concerned person as witness' relation *)
                wp.m_related <- ip :: wp.m_related;
                (wip, wk))
              witl
          in
          {
            epers_name = pevent_name_unique_string gen name;
            epers_date = date;
            epers_place = unique_string gen place;
            epers_reason = unique_string gen reason;
            epers_note = unique_string gen notes;
            epers_src = unique_string gen src;
            epers_witnesses = Array.of_list witnesses;
          })
        pevents
    in
    (* add events to the person's entry in the base *)
    p.m_pevents <- pevents

(** Insert person's notes in the base and associate it to the referenced with
    [key] person *)
let insert_notes fname gen key str =
  let occ = key.pk_occ + gen.g_file_info.f_shift in
  match
    try Some (find_person_by_name gen key.pk_first_name key.pk_surname occ)
    with Not_found -> None
  with
  | Some ip ->
      let p = poi gen.g_base ip in
      if sou gen.g_base p.m_notes <> "" then (
        Printf.printf "\nFile \"%s\"" fname;
        Printf.printf "\nError: Notes already defined for \"%s%s %s\"\n"
          key.pk_first_name
          (if occ = 0 then "" else "." ^ string_of_int occ)
          key.pk_surname;
        check_error gen)
      else p.m_notes <- unique_string gen str
  | None ->
      Printf.printf "File \"%s\"\n" fname;
      Printf.printf "*** warning: undefined person: \"%s%s %s\"\n"
        key.pk_first_name
        (if occ = 0 then "" else "." ^ string_of_int occ)
        key.pk_surname;
      flush stdout

(** Changes [gen.g_base.c_bnotes] to take into account [nfname] page and its
    content [str] that is treated by the way mentioned in
    [gen.g_file_info.f_bnotes]. *)
let insert_bnotes fname gen nfname str =
  if gen.g_file_info.f_bnotes <> `drop then
    let old_nread = gen.g_base.c_bnotes.nread in
    (* Convert path notation from 'dir1:dir2:file' to 'dir1/dir2/file'
       (if a valid path) *)
    let nfname =
      if nfname = "" then ""
      else
        match NotesLinks.check_file_name nfname with
        | Some (dl, f) -> List.fold_right Filename.concat dl f
        | None -> "bad"
    in
    let bnotes =
      let str =
        match gen.g_file_info.f_bnotes with
        | `drop -> assert false
        | `erase -> str
        | `merge -> old_nread nfname RnAll ^ str
        | `first -> (
            match old_nread nfname RnAll with "" -> str | str -> str)
      in
      {
        nread = (fun f n -> if f = nfname then str else old_nread f n);
        norigin_file = fname;
        efiles =
          (if nfname <> "" then
             let efiles = gen.g_base.c_bnotes.efiles () in
             fun () -> nfname :: efiles
           else gen.g_base.c_bnotes.efiles);
      }
    in
    gen.g_base.c_bnotes <- bnotes

(** Add wizard and his note to the [gen] *)
let insert_wiznote gen wizid str =
  gen.g_wiznotes <- (wizid, str) :: gen.g_wiznotes

let map_option f = function Some x -> Some (f x) | None -> None

(** Insert parent in the base and adjust his sex if needed. Concerned person is
    added in the list of parent's related persons. *)
let insert_relation_parent gen ip s k =
  let par, ipar = insert_somebody gen k in
  par.m_related <- ip :: par.m_related;
  if par.m_sex = Neuter then par.m_sex <- s;
  ipar

(** Convert [(Dune__exe.Gwcomp.somebody, string) Def.gen_relation] to
    [(int, int) Def.gen_relation] and insert all related to relation information
    in the base. *)
let insert_relation gen ip r =
  {
    r_type = r.r_type;
    r_fath = map_option (insert_relation_parent gen ip Male) r.r_fath;
    r_moth = map_option (insert_relation_parent gen ip Female) r.r_moth;
    r_sources = unique_string gen r.r_sources;
  }

(** Insert all information related to the person's relations and add those
    relations to the person's list of related parents. *)
let insert_relations fname gen sb sex rl =
  (* insert concerned person *)
  let p, ip = insert_somebody gen sb in
  if p.m_rparents <> [] then (
    Printf.printf "\nFile \"%s\"" fname;
    Printf.printf "\nError: Relations already defined for \"%s%s %s\"\n"
      (sou gen.g_base p.m_first_name)
      (if p.m_occ = 0 then "" else "." ^ string_of_int p.m_occ)
      (sou gen.g_base p.m_surname);
    check_error gen)
  else (
    notice_sex gen p sex;
    let rl = List.map (insert_relation gen ip) rl in
    p.m_rparents <- rl)

(** Insert syntax element read from .gwo file. *)
let insert_syntax fname gen = function
  | Family (cpl, fs, ms, witl, fevents, fam, des) ->
      insert_family gen cpl fs ms witl fevents fam des
  | Notes (key, str) -> insert_notes fname gen key str
  | Relations (sb, sex, rl) -> insert_relations fname gen sb sex rl
  | Pevent (sb, _, pevents) -> insert_pevents fname gen sb pevents
  | Bnotes (nfname, str) -> insert_bnotes fname gen nfname str
  | Wnotes (wizid, str) -> insert_wiznote gen wizid str

(** Update person by looking up information inferred from person events *)
let update_person_with_pevents p =
  let found_birth = ref false in
  let found_baptism = ref false in
  let found_death = ref false in
  let found_burial = ref false in
  let death_std_fields = p.death in
  let death_reason_std_fields =
    match death_std_fields with Death (dr, _) -> dr | _ -> Unspecified
  in
  let rec loop pevents p =
    match pevents with
    | [] -> p
    | evt :: l -> (
        match evt.epers_name with
        | Epers_Birth ->
            if !found_birth then loop l p
            else
              let p =
                {
                  p with
                  birth = evt.epers_date;
                  birth_place = evt.epers_place;
                  birth_note = evt.epers_note;
                  birth_src = evt.epers_src;
                }
              in
              let () = found_birth := true in
              loop l p
        | Epers_Baptism ->
            if !found_baptism then loop l p
            else
              let p =
                {
                  p with
                  baptism = evt.epers_date;
                  baptism_place = evt.epers_place;
                  baptism_note = evt.epers_note;
                  baptism_src = evt.epers_src;
                }
              in
              let () = found_baptism := true in
              loop l p
        | Epers_Death ->
            if !found_death then loop l p
            else
              let death =
                match Date.od_of_cdate evt.epers_date with
                | Some d -> Death (death_reason_std_fields, Date.cdate_of_date d)
                | None -> (
                    match death_std_fields with
                    | OfCourseDead -> OfCourseDead
                    | DeadYoung -> DeadYoung
                    | _ -> DeadDontKnowWhen)
              in
              let p =
                {
                  p with
                  death;
                  death_place = evt.epers_place;
                  death_note = evt.epers_note;
                  death_src = evt.epers_src;
                }
              in
              let () = found_death := true in
              loop l p
        | Epers_Burial ->
            if !found_burial then loop l p
            else
              let p =
                {
                  p with
                  burial = Buried evt.epers_date;
                  burial_place = evt.epers_place;
                  burial_note = evt.epers_note;
                  burial_src = evt.epers_src;
                }
              in
              let () = found_burial := true in
              loop l p
        | Epers_Cremation ->
            if !found_burial then loop l p
            else
              let p =
                {
                  p with
                  burial = Cremated evt.epers_date;
                  burial_place = evt.epers_place;
                  burial_note = evt.epers_note;
                  burial_src = evt.epers_src;
                }
              in
              let () = found_burial := true in
              loop l p
        | _ -> loop l p)
  in
  loop p.pevents p

(** Update person's event list by looking up inferred personal information. *)
let update_pevents_with_person p =
  let empty_string = 0 in
  let evt_birth =
    match Date.od_of_cdate p.birth with
    | Some _ ->
        let evt =
          {
            epers_name = Epers_Birth;
            epers_date = p.birth;
            epers_place = p.birth_place;
            epers_reason = empty_string;
            epers_note = p.birth_note;
            epers_src = p.birth_src;
            epers_witnesses = [||];
          }
        in
        Some evt
    | None ->
        if p.birth_place = 0 && p.birth_src = 0 then None
        else
          let evt =
            {
              epers_name = Epers_Birth;
              epers_date = p.birth;
              epers_place = p.birth_place;
              epers_reason = empty_string;
              epers_note = p.birth_note;
              epers_src = p.birth_src;
              epers_witnesses = [||];
            }
          in
          Some evt
  in
  let evt_bapt =
    match Date.od_of_cdate p.baptism with
    | Some _ ->
        let evt =
          {
            epers_name = Epers_Baptism;
            epers_date = p.baptism;
            epers_place = p.baptism_place;
            epers_reason = empty_string;
            epers_note = p.baptism_note;
            epers_src = p.baptism_src;
            epers_witnesses = [||];
          }
        in
        Some evt
    | None ->
        if p.baptism_place = 0 && p.baptism_src = 0 then None
        else
          let evt =
            {
              epers_name = Epers_Baptism;
              epers_date = p.baptism;
              epers_place = p.baptism_place;
              epers_reason = empty_string;
              epers_note = p.baptism_note;
              epers_src = p.baptism_src;
              epers_witnesses = [||];
            }
          in
          Some evt
  in
  let evt_death =
    match p.death with
    | NotDead | DontKnowIfDead ->
        if p.death_place = 0 && p.death_src = 0 then None
        else
          let evt =
            {
              epers_name = Epers_Death;
              epers_date = Date.cdate_None;
              epers_place = p.death_place;
              epers_reason = empty_string;
              epers_note = p.death_note;
              epers_src = p.death_src;
              epers_witnesses = [||];
            }
          in
          Some evt
    | Death (_, cd) ->
        let date = Date.cdate_of_od (Some (Date.date_of_cdate cd)) in
        let evt =
          {
            epers_name = Epers_Death;
            epers_date = date;
            epers_place = p.death_place;
            epers_reason = empty_string;
            epers_note = p.death_note;
            epers_src = p.death_src;
            epers_witnesses = [||];
          }
        in
        Some evt
    | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
        let evt =
          {
            epers_name = Epers_Death;
            epers_date = Date.cdate_None;
            epers_place = p.death_place;
            epers_reason = empty_string;
            epers_note = p.death_note;
            epers_src = p.death_src;
            epers_witnesses = [||];
          }
        in
        Some evt
  in
  let evt_burial =
    match p.burial with
    | UnknownBurial ->
        if p.burial_place = 0 && p.burial_src = 0 then None
        else
          let evt =
            {
              epers_name = Epers_Burial;
              epers_date = Date.cdate_None;
              epers_place = p.burial_place;
              epers_reason = empty_string;
              epers_note = p.burial_note;
              epers_src = p.burial_src;
              epers_witnesses = [||];
            }
          in
          Some evt
    | Buried cd ->
        let evt =
          {
            epers_name = Epers_Burial;
            epers_date = cd;
            epers_place = p.burial_place;
            epers_reason = empty_string;
            epers_note = p.burial_note;
            epers_src = p.burial_src;
            epers_witnesses = [||];
          }
        in
        Some evt
    | Cremated cd ->
        let evt =
          {
            epers_name = Epers_Cremation;
            epers_date = cd;
            epers_place = p.burial_place;
            epers_reason = empty_string;
            epers_note = p.burial_note;
            epers_src = p.burial_src;
            epers_witnesses = [||];
          }
        in
        Some evt
  in
  let pevents = [ evt_birth; evt_bapt; evt_death; evt_burial ] in
  let pevents =
    List.fold_right
      (fun evt pevents ->
        match evt with Some evt -> evt :: pevents | None -> pevents)
      pevents []
  in
  { p with pevents }

(** Returns list of persons from [min_person list] where some absent information
    was extracted from file [per_ic]. Adjusts person with inffered information
    from events or inversely. *)
let convert_persons per_index_ic per_ic persons =
  Array.mapi
    (fun i mp ->
      let p =
        let c =
          try
            seek_in per_index_ic (sizeof_long * i);
            let pos = input_binary_int per_index_ic in
            seek_in per_ic pos;
            input_char per_ic
          with End_of_file -> 'U'
        in
        match c with
        (* if person is defined read person *)
        | 'D' -> (input_item_value per_ic : person)
        (* if read person is undefined then create dummy person *)
        | 'U' ->
            let empty_string = 0 in
            {
              first_name = empty_string;
              surname = empty_string;
              occ = 0;
              image = empty_string;
              first_names_aliases = [];
              surnames_aliases = [];
              public_name = empty_string;
              qualifiers = [];
              aliases = [];
              titles = [];
              rparents = [];
              related = [];
              occupation = empty_string;
              sex = Neuter;
              access = IfTitles;
              birth = Date.cdate_None;
              birth_place = empty_string;
              birth_note = empty_string;
              birth_src = empty_string;
              baptism = Date.cdate_None;
              baptism_place = empty_string;
              baptism_note = empty_string;
              baptism_src = empty_string;
              death = DontKnowIfDead;
              death_place = empty_string;
              death_note = empty_string;
              death_src = empty_string;
              burial = UnknownBurial;
              burial_place = empty_string;
              burial_note = empty_string;
              burial_src = empty_string;
              pevents = [];
              notes = empty_string;
              psources = empty_string;
              key_index = 0;
            }
        | _ -> assert false
      in
      let p =
        {
          p with
          first_name = mp.m_first_name;
          surname = mp.m_surname;
          occ = mp.m_occ;
          rparents = mp.m_rparents;
          related = mp.m_related;
          pevents = mp.m_pevents;
          sex = mp.m_sex;
          notes = mp.m_notes;
          key_index = i;
        }
      in
      (* Si on a trouvé des évènements, on mets à jour *)
      if p.pevents <> [] then update_person_with_pevents p
      else update_pevents_with_person p)
    persons

(** File containing the particles to use *)
let particules_file = ref ""

(** Returns list of particles from the file. If filename is empty string then
    returns default particles list *)
let input_particles = function
  | "" -> Mutil.default_particles
  | file -> Mutil.input_particles file

(** Empty base *)
let empty_base : cbase =
  {
    c_persons = [||];
    c_ascends = [||];
    c_unions = [||];
    c_families = [||];
    c_couples = [||];
    c_descends = [||];
    c_strings = [||];
    c_bnotes =
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun _ -> []) };
  }

(** Extract information from the [gen.g_base] and create database *)
let make_base bname gen per_index_ic per_ic k =
  let _ =
    Printf.eprintf "pcnt %d persons %d\n" gen.g_pcnt
      (Array.length gen.g_base.c_persons);
    flush stderr
  in
  (* get full persons information *)
  let persons =
    let a = Array.sub gen.g_base.c_persons 0 gen.g_pcnt in
    gen.g_base.c_persons <- [||];
    convert_persons per_index_ic per_ic a
  in
  let ascends =
    let a = Array.sub gen.g_base.c_ascends 0 gen.g_pcnt in
    gen.g_base.c_ascends <- [||];
    a
  in
  let unions =
    let a = Array.sub gen.g_base.c_unions 0 gen.g_pcnt in
    gen.g_base.c_unions <- [||];
    a
  in
  let _ =
    Printf.eprintf "fcnt %d families %d\n" gen.g_fcnt
      (Array.length gen.g_base.c_couples);
    flush stderr
  in
  let families =
    let a = Array.sub gen.g_base.c_families 0 gen.g_fcnt in
    gen.g_base.c_families <- [||];
    a
  in
  let couples =
    let a = Array.sub gen.g_base.c_couples 0 gen.g_fcnt in
    gen.g_base.c_couples <- [||];
    a
  in
  let descends =
    let a = Array.sub gen.g_base.c_descends 0 gen.g_fcnt in
    gen.g_base.c_descends <- [||];
    a
  in
  let _ =
    Printf.eprintf "scnt %d strings %d\n" gen.g_scnt
      (Array.length gen.g_base.c_strings);
    flush stderr
  in
  let strings =
    let a = Array.sub gen.g_base.c_strings 0 gen.g_scnt in
    gen.g_base.c_strings <- [||];
    a
  in
  Driver.make bname
    (input_particles !particules_file)
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      gen.g_base.c_bnotes )
    k

(** Write content in the file *)
let write_file_contents fname text =
  let oc = open_out fname in
  output_string oc text;
  close_out oc

(** Create and fill a file for every wizard note *)
let output_wizard_notes bdir wiznotes =
  let wizdir = Filename.concat bdir "wiznotes" in
  if wiznotes <> [] then (
    (* FIXME ad hoc solution. wiznotes should be handled same as notes_d *)
    (* not necessarily true! notes_d is part of base, wiznotes is not *)
    if not @@ Sys.file_exists wizdir then Unix.mkdir wizdir 0o755;
    List.iter
      (fun (wizid, text) ->
        let fname = Filename.concat wizdir wizid ^ ".txt" in
        write_file_contents fname text)
      wiznotes)

(** Create file that contains command used to call this program *)
let output_command_line bdir =
  let oc = open_out (Filename.concat bdir "command.txt") in
  Printf.fprintf oc "%s" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.fprintf oc " %s" Sys.argv.(i)
  done;
  Printf.fprintf oc "\n";
  close_out oc

(** Link .gwo files and create a database. *)
let link next_family_fun bdir =
  let tmp_dir = Filename.concat bdir "gw_tmp" in
  Mutil.mkdir_p tmp_dir;
  let tmp_per_index = Filename.concat tmp_dir "gwc_per_index" in
  let tmp_per = Filename.concat tmp_dir "gwc_per" in
  let tmp_fam_index = Filename.concat tmp_dir "gwc_fam_index" in
  let tmp_fam = Filename.concat tmp_dir "gwc_fam" in
  let fi =
    {
      f_local_names = Hashtbl.create 20011;
      f_curr_src_file = "";
      f_curr_gwo_file = "";
      f_separate = false;
      f_shift = 0;
      f_bnotes = `merge;
    }
  in
  let gen =
    {
      g_strings = Hashtbl.create 20011;
      g_names = Hashtbl.create 20011;
      g_pcnt = 0;
      g_fcnt = 0;
      g_scnt = 0;
      g_file_info = fi;
      g_base = empty_base;
      g_patch_p = Hashtbl.create 20011;
      g_wiznotes = [];
      g_def = [||];
      g_first_av_occ = Hashtbl.create 1;
      g_errored = false;
      g_per_index = open_out_bin tmp_per_index;
      g_per = open_out_bin tmp_per;
      g_fam_index = open_out_bin tmp_fam_index;
      g_fam = open_out_bin tmp_fam;
    }
  in
  let per_index_ic = open_in_bin tmp_per_index in
  let per_ic = open_in_bin tmp_per in
  let istr_empty = unique_string gen "" in
  let istr_quest = unique_string gen "?" in
  assert (istr_empty = 0);
  assert (istr_quest = 1);
  let next_family = next_family_fun fi in
  let rec loop () =
    match next_family () with
    | Some fam ->
        insert_syntax fi.f_curr_src_file gen fam;
        loop ()
    | None -> ()
  in
  loop ();
  close_out gen.g_per_index;
  close_out gen.g_per;
  close_out gen.g_fam_index;
  close_out gen.g_fam;
  Hashtbl.clear gen.g_strings;
  Hashtbl.clear gen.g_names;
  Hashtbl.clear fi.f_local_names;
  make_base bdir gen per_index_ic per_ic @@ fun base ->
  Hashtbl.clear gen.g_patch_p;
  Gc.full_major ();
  close_in per_index_ic;
  close_in per_ic;
  if !do_check && gen.g_pcnt > 0 then (
    Check.check_base base (set_error base gen) (set_warning base) ignore;
    if !pr_stats then Stats.(print_stats base @@ stat_base base));
  if not gen.g_errored then (
    if !do_consang then ignore @@ ConsangAll.compute base true;
    Driver.sync base;
    output_wizard_notes bdir gen.g_wiznotes;
    output_command_line bdir;
    Mutil.rm_rf tmp_dir;
    true)
  else (
    Mutil.rm_rf bdir;
    false)
