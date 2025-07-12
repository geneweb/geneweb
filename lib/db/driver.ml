(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk

type istr = int
type ifam = int
type iper = int

module type Indexed = sig
  type t

  val dummy : t
  val is_dummy : t -> bool
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t
  val pp : t Fmt.t

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Table : Hashtbl.S with type key = t
end

module I = struct
  type t = int

  let dummy = -1
  let[@inline always] hash x = x
  let equal = Int.equal
  let[@inline always] is_dummy t = equal t dummy
  let compare = Int.compare
  let to_string = string_of_int
  let of_string = int_of_string
  let pp = Fmt.int

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Table = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal
    let hash = hash
  end)
end

module Istr = struct
  include I

  let empty = 0
  let quest = 1
  let[@inline always] is_empty i = i = 0
  let[@inline always] is_quest i = i = 1
end

module Ifam = I
module Iper = I

type string_person_index = Dbdisk.string_person_index

let spi_find spi = spi.find
let spi_first spi = spi.cursor
let spi_next (spi : string_person_index) istr = spi.next istr

type base = dsk_base

let sou base i = base.data.strings.get i
let bname base = Filename.(remove_extension @@ basename base.data.bdir)
let nb_of_persons base = base.data.persons.len
let nb_of_real_persons base = base.func.nb_of_real_persons ()
let nb_of_families base = base.data.families.len

let insert_string base s =
  base.func.Dbdisk.insert_string @@ Mutil.normalize_utf_8 s

let commit_patches base = base.func.Dbdisk.commit_patches ()
let commit_notes base s = base.func.Dbdisk.commit_notes s
let commit_wiznotes base s = base.func.Dbdisk.commit_wiznotes s
let person_of_key base = base.func.Dbdisk.person_of_key
let persons_of_name base = base.func.Dbdisk.persons_of_name
let persons_of_first_name base = base.func.Dbdisk.persons_of_first_name
let persons_of_surname base = base.func.Dbdisk.persons_of_surname
let base_particles base = Lazy.force base.data.particles
let base_strings_of_first_name base s = base.func.strings_of_fname s
let base_strings_of_surname base s = base.func.strings_of_sname s
let load_ascends_array base = base.data.ascends.load_array ()
let load_unions_array base = base.data.unions.load_array ()
let load_couples_array base = base.data.couples.load_array ()
let load_descends_array base = base.data.descends.load_array ()
let load_strings_array base = base.data.strings.load_array ()
let load_persons_array base = base.data.persons.load_array ()
let load_families_array base = base.data.families.load_array ()
let clear_ascends_array base = base.data.ascends.clear_array ()
let clear_unions_array base = base.data.unions.clear_array ()
let clear_couples_array base = base.data.couples.clear_array ()
let clear_descends_array base = base.data.descends.clear_array ()
let clear_strings_array base = base.data.strings.clear_array ()
let clear_persons_array base = base.data.persons.clear_array ()
let clear_families_array base = base.data.families.clear_array ()

let clear_base base =
  clear_ascends_array base;
  clear_unions_array base;
  clear_couples_array base;
  clear_descends_array base;
  clear_strings_array base;
  clear_persons_array base;
  clear_families_array base

(* Map of loaded read-only databases in memory. *)
let loaded_databases : (string, dsk_base) Hashtbl.t = Hashtbl.create 17

let load_database bname =
  match Hashtbl.find loaded_databases bname with
  | exception Not_found ->
      Database.with_database ~read_only:true bname (fun base ->
          Hashtbl.add loaded_databases bname base;
          load_persons_array base;
          load_ascends_array base;
          load_unions_array base;
          load_couples_array base;
          load_descends_array base;
          load_families_array base;
          load_strings_array base)
  | _base -> Fmt.failwith "'%s' is already loaded in memory" bname

let with_database bname k =
  match Hashtbl.find loaded_databases bname with
  | exception Not_found ->
      Database.with_database ~read_only:false bname (fun base ->
          Fun.protect ~finally:(fun () -> clear_base base) @@ fun () -> k base)
  | _base ->
      (* FIXME: We cannot reuse [_base] because it contains closures that
         captured old versions of patches. It is important to read patches
         on disk before each request to process the latest version of the
         base. Otherwise, workers could keep different old versions in
         memory. *)
      Database.with_database ~read_only:true bname k

let date_of_last_change base =
  let s =
    let bdir = base.data.bdir in
    try Unix.stat (Filename.concat bdir "patches")
    with Unix.Unix_error (_, _, _) -> Unix.stat (Filename.concat bdir "base")
  in
  s.Unix.st_mtime

let gen_gen_person_misc_names = Dutil.dsk_person_misc_names

let patch_misc_names base ip (p : (iper, iper, istr) Def.gen_person) =
  let p = { p with Def.key_index = ip } in
  List.iter
    (fun s -> base.func.Dbdisk.patch_name s ip)
    (gen_gen_person_misc_names base p (fun p -> p.Def.titles))

let patch_person base ip (p : (iper, iper, istr) Def.gen_person) =
  base.func.Dbdisk.patch_person ip p;
  let s = sou base p.first_name ^ " " ^ sou base p.surname in
  base.func.Dbdisk.patch_name s ip;
  patch_misc_names base ip p;
  Array.iter
    (fun i ->
      let cpl = base.data.couples.get i in
      let m = Adef.mother cpl in
      let f = Adef.father cpl in
      patch_misc_names base m (base.data.persons.get m);
      patch_misc_names base f (base.data.persons.get f);
      Array.iter
        (fun i -> patch_misc_names base i (base.data.persons.get i))
        (base.data.descends.get i).children)
    (base.data.unions.get ip).family

let patch_ascend base ip a = base.func.Dbdisk.patch_ascend ip a
let patch_union base ip u = base.func.Dbdisk.patch_union ip u
let patch_family base ifam f = base.func.Dbdisk.patch_family ifam f
let patch_couple base ifam c = base.func.Dbdisk.patch_couple ifam c
let patch_descend base ifam d = base.func.Dbdisk.patch_descend ifam d
let insert_person = patch_person
let insert_ascend = patch_ascend
let insert_union = patch_union
let insert_family = patch_family
let insert_couple = patch_couple
let insert_descend = patch_descend

let delete_person base ip =
  patch_person base ip
    {
      first_name = Istr.quest;
      surname = Istr.quest;
      occ = 0;
      image = Istr.empty;
      first_names_aliases = [];
      surnames_aliases = [];
      public_name = Istr.empty;
      qualifiers = [];
      titles = [];
      rparents = [];
      related = [];
      aliases = [];
      occupation = Istr.empty;
      sex = Neuter;
      access = Private;
      birth = Date.cdate_None;
      birth_place = Istr.empty;
      birth_note = Istr.empty;
      birth_src = Istr.empty;
      baptism = Date.cdate_None;
      baptism_place = Istr.empty;
      baptism_note = Istr.empty;
      baptism_src = Istr.empty;
      death = DontKnowIfDead;
      death_place = Istr.empty;
      death_note = Istr.empty;
      death_src = Istr.empty;
      burial = UnknownBurial;
      burial_place = Istr.empty;
      burial_note = Istr.empty;
      burial_src = Istr.empty;
      pevents = [];
      notes = Istr.empty;
      psources = Istr.empty;
      key_index = ip;
    }

let delete_ascend base ip =
  patch_ascend base ip { parents = None; consang = Adef.no_consang }

let delete_union base ip = patch_union base ip { family = [||] }

let delete_family base ifam =
  patch_family base ifam
    {
      marriage = Date.cdate_None;
      marriage_place = Istr.empty;
      marriage_note = Istr.empty;
      marriage_src = Istr.empty;
      relation = Married;
      divorce = NotDivorced;
      fevents = [];
      witnesses = [||];
      comment = Istr.empty;
      origin_file = Istr.empty;
      fsources = Istr.empty;
      fam_index = Ifam.dummy;
    }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple Iper.dummy Iper.dummy)

let delete_descend base ifam = patch_descend base ifam { children = [||] }
let new_iper base = base.data.persons.len
let new_ifam base = base.data.families.len

(* FIXME: lock *)
let sync ?(scratch = false) base =
  if base.data.perm = RDONLY && not scratch then
    raise Def.(HttpExn (Forbidden, __LOC__))
  else Outbase.output base

let make bname particles arrays k =
  Database.make bname particles arrays (sync ~scratch:true);
  with_database bname k

let bfname base fname = Filename.concat base.data.bdir fname

module NLDB = struct
  let magic = "GWNL0010"

  let read base =
    let fname = bfname base "notes_links" in
    match try Some (open_in_bin fname) with Sys_error _ -> None with
    | Some ic ->
        let r =
          if Mutil.check_magic magic ic then
            (input_value ic : (iper, iper) Def.NLDB.t)
          else failwith "unsupported nldb format"
        in
        close_in ic;
        r
    | None -> []

  let write base db =
    if base.data.perm = RDONLY then raise Def.(HttpExn (Forbidden, __LOC__))
    else
      let fname_tmp = bfname base "1notes_links" in
      let fname_def = bfname base "notes_links" in
      let fname_back = bfname base "notes_links~" in
      let oc = open_out_bin fname_tmp in
      output_string oc magic;
      output_value oc (db : (iper, ifam) Def.NLDB.t);
      close_out oc;
      Mutil.rm fname_back;
      Mutil.mv fname_def fname_back;
      Sys.rename fname_tmp fname_def
end

let read_nldb = NLDB.read
let write_nldb = NLDB.write
let base_notes_origin_file base = base.data.bnotes.Def.norigin_file
let base_notes_dir _base = "notes_d"
let base_wiznotes_dir _base = "wiznotes"

let base_notes_read_file base fname mode =
  try
    let ic = Secure.open_in @@ Filename.concat base.data.bdir fname in
    let str =
      match mode with
      | Def.RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Def.Rn1Ln -> ( try input_line ic with End_of_file -> "")
      | Def.RnAll -> Mutil.input_file_ic ic
    in
    close_in ic;
    str
  with Sys_error _ -> ""

let base_notes_read_aux base fnotes mode =
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat (base_notes_dir base) (fnotes ^ ".txt")
  in
  base_notes_read_file base fname mode

let base_wiznotes_read base fwnotes =
  let fname = Filename.concat (base_wiznotes_dir base) (fwnotes ^ ".txt") in
  base_notes_read_file base fname Def.RnAll

let base_notes_read base fnotes = base_notes_read_aux base fnotes Def.RnAll

let base_notes_read_first_line base fnotes =
  base_notes_read_aux base fnotes Def.Rn1Ln

let base_notes_are_empty base fnotes =
  base_notes_read_aux base fnotes Def.RnDeg = ""

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

let cache f a get set x =
  match get x with
  | Some v -> v
  | None ->
      let v = f a in
      set x (Some v);
      v

(** Persons *)

type person = {
  base : base;
  iper : iper;
  mutable p : (iper, iper, istr) Def.gen_person option;
  mutable a : ifam Def.gen_ascend option;
  mutable u : ifam Def.gen_union option;
}

let cache_per f ({ base; iper; _ } as p) =
  f (cache base.data.persons.get iper (fun p -> p.p) (fun p v -> p.p <- v) p)

let cache_asc f ({ base; iper; _ } as p) =
  f (cache base.data.ascends.get iper (fun p -> p.a) (fun p v -> p.a <- v) p)

let cache_uni f ({ base; iper; _ } as p) =
  f (cache base.data.unions.get iper (fun p -> p.u) (fun p v -> p.u <- v) p)

let gen_person_of_person = cache_per (fun p -> p)
let gen_ascend_of_person = cache_asc (fun p -> p)
let gen_union_of_person = cache_uni (fun p -> p)
let get_access = cache_per (fun p -> p.Def.access)
let get_aliases = cache_per (fun p -> p.Def.aliases)
let get_baptism = cache_per (fun p -> p.Def.baptism)
let get_baptism_note = cache_per (fun p -> p.Def.baptism_note)
let get_baptism_place = cache_per (fun p -> p.Def.baptism_place)
let get_baptism_src = cache_per (fun p -> p.Def.baptism_src)
let get_birth = cache_per (fun p -> p.Def.birth)
let get_birth_note = cache_per (fun p -> p.Def.birth_note)
let get_birth_place = cache_per (fun p -> p.Def.birth_place)
let get_birth_src = cache_per (fun p -> p.Def.birth_src)
let get_burial = cache_per (fun p -> p.Def.burial)
let get_burial_note = cache_per (fun p -> p.Def.burial_note)
let get_burial_place = cache_per (fun p -> p.Def.burial_place)
let get_burial_src = cache_per (fun p -> p.Def.burial_src)
let get_consang = cache_asc (fun a -> a.Def.consang)
let get_death = cache_per (fun p -> p.Def.death)
let get_death_note = cache_per (fun p -> p.Def.death_note)
let get_death_place = cache_per (fun p -> p.Def.death_place)
let get_death_src = cache_per (fun p -> p.Def.death_src)
let get_family = cache_uni (fun u -> u.Def.family)
let get_first_name = cache_per (fun p -> p.Def.first_name)
let get_first_names_aliases = cache_per (fun p -> p.Def.first_names_aliases)
let get_image = cache_per (fun p -> p.Def.image)
let get_iper = cache_per (fun p -> p.Def.key_index)
let get_notes = cache_per (fun p -> p.Def.notes)
let get_occ = cache_per (fun p -> p.Def.occ)
let get_occupation = cache_per (fun p -> p.Def.occupation)
let get_parents = cache_asc (fun a -> a.Def.parents)
let get_pevents = cache_per (fun p -> p.Def.pevents)
let get_psources = cache_per (fun p -> p.Def.psources)
let get_public_name = cache_per (fun p -> p.Def.public_name)
let get_qualifiers = cache_per (fun p -> p.Def.qualifiers)
let get_related = cache_per (fun p -> p.Def.related)
let get_rparents = cache_per (fun p -> p.Def.rparents)
let get_sex = cache_per (fun p -> p.Def.sex)
let get_surname = cache_per (fun p -> p.Def.surname)
let get_surnames_aliases = cache_per (fun p -> p.Def.surnames_aliases)
let get_titles = cache_per (fun p -> p.Def.titles)

(** Families *)

type family = {
  base : base;
  ifam : ifam;
  mutable f : (iper, ifam, istr) Def.gen_family option;
  mutable c : iper Def.gen_couple option;
  mutable d : iper Def.gen_descend option;
}

let cache_fam f ({ base; ifam; _ } as fam) =
  f (cache base.data.families.get ifam (fun f -> f.f) (fun f v -> f.f <- v) fam)

let cache_cpl f ({ base; ifam; _ } as fam) =
  f (cache base.data.couples.get ifam (fun f -> f.c) (fun f v -> f.c <- v) fam)

let cache_des f ({ base; ifam; _ } as fam) =
  f (cache base.data.descends.get ifam (fun f -> f.d) (fun f v -> f.d <- v) fam)

let gen_couple_of_family = cache_cpl (fun c -> c)
let gen_descend_of_family = cache_des (fun d -> d)
let gen_family_of_family = cache_fam (fun f -> f)
let get_children = cache_des (fun d -> d.Def.children)
let get_comment = cache_fam (fun f -> f.Def.comment)
let get_ifam = cache_fam (fun f -> f.Def.fam_index)

(* let get_divorce = cache_fam (fun f -> f.Def.divorce) *)
let get_father = cache_cpl (fun c -> Adef.father c)
let get_fevents = cache_fam (fun f -> f.Def.fevents)
let get_fsources = cache_fam (fun f -> f.Def.fsources)
let get_marriage = cache_fam (fun f -> f.Def.marriage)
let get_marriage_note = cache_fam (fun f -> f.Def.marriage_note)
let get_marriage_place = cache_fam (fun f -> f.Def.marriage_place)
let get_marriage_src = cache_fam (fun f -> f.Def.marriage_src)
let get_mother = cache_cpl (fun c -> Adef.mother c)
let get_origin_file = cache_fam (fun f -> f.Def.origin_file)
let get_parent_array = cache_cpl (fun c -> Adef.parent_array c)
let get_relation = cache_fam (fun f -> f.Def.relation)
let get_witnesses = cache_fam (fun f -> f.Def.witnesses)
let empty_person = Mutil.empty_person Istr.empty Istr.empty
let no_person ip = Def.{ empty_person with key_index = ip }
let no_ascend = Def.{ parents = None; consang = Adef.no_consang }
let no_union = Def.{ family = [||] }

let empty_person base iper =
  {
    base;
    iper;
    p = Some (no_person iper);
    a = Some no_ascend;
    u = Some no_union;
  }

let person_of_gen_person base (p, a, u) =
  Def.{ base; iper = p.key_index; p = Some p; a = Some a; u = Some u }

let family_of_gen_family base (f, c, d) =
  Def.{ base; ifam = f.fam_index; f = Some f; c = Some c; d = Some d }

let iper_exists base = base.func.iper_exists
let ifam_exists base = base.func.ifam_exists

let poi base iper =
  if Iper.is_dummy iper then empty_person base iper
  else { base; iper; p = None; a = None; u = None }

let no_family ifam = { (Mutil.empty_family Istr.empty) with fam_index = ifam }
let no_couple = Adef.couple Iper.dummy Iper.dummy
let no_descend = { Def.children = [||] }

let empty_family base ifam =
  {
    base;
    ifam;
    f = Some (no_family ifam);
    c = Some no_couple;
    d = Some no_descend;
  }

let foi base ifam =
  if ifam = Ifam.dummy then empty_family base ifam
  else { base; ifam; f = None; c = None; d = None }

let persons base =
  Collection.make ~len:(nb_of_persons base) (fun i -> Some (poi base i))

let ipers base = Collection.make ~len:(nb_of_persons base) (fun i -> Some i)
let iper_marker c i = Collection.Marker.make (fun i -> i) c i

let ifams ?(select = fun _ -> true) base =
  Collection.make ~len:(nb_of_families base) (fun i ->
      if select i then
        if get_ifam (foi base i) = Ifam.dummy then None else Some i
      else None)

let families ?(select = fun _ -> true) base =
  Collection.make ~len:(nb_of_families base) (fun i ->
      let f = foi base i in
      if get_ifam f <> Ifam.dummy && select f then Some f else None)

let ifam_marker c i = Collection.Marker.make (fun i -> i) c i

(* Restrict file *)

(* FIXME: these values should not be global *)
let visible_ref : (iper, bool) Hashtbl.t option ref = ref None

let read_or_create_visible base =
  let fname = Filename.concat base.data.bdir "restrict" in
  let visible =
    if Sys.file_exists fname then (
      let ic = Secure.open_in fname in
      let visible =
        if Mutil.check_magic Mutil.executable_magic ic then input_value ic
        else Hashtbl.create (nb_of_persons base)
      in
      close_in ic;
      visible)
    else Hashtbl.create (nb_of_persons base)
  in
  visible_ref := Some visible;
  visible

let base_visible_write base =
  if base.data.perm = RDONLY then raise Def.(HttpExn (Forbidden, __LOC__))
  else
    let fname = Filename.concat base.data.bdir "restrict" in
    match !visible_ref with
    | Some visible ->
        let oc = Secure.open_out fname in
        output_string oc Mutil.executable_magic;
        output_value oc visible;
        close_out oc
    | None -> ()

let base_visible_get base fct i =
  let visible =
    match !visible_ref with
    | Some visible -> visible
    | None -> read_or_create_visible base
  in
  match Hashtbl.find_opt visible i with
  | None ->
      let status = fct (poi base i) in
      Hashtbl.add visible i status;
      visible_ref := Some visible;
      status
  | Some b -> b

(*
type 'a event_name =
  | Pevent of 'a gen_pers_event_name
  | Fevent of 'a gen_fam_event_name
type ('person, 'string) gen_fam_event = {
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind) array;
}


  let fam_fevents =
    if m_auth then
      List.fold_right
        (fun evt fam_fevents ->
          let name = Fevent evt.efam_name in
          let date = evt.efam_date in
          let place = evt.efam_place in
          let note = evt.efam_note in
          let src = evt.efam_src in
          let wl = evt.efam_witnesses in
          let x = (name, date, place, note, src, wl, Some isp) in
          x :: fam_fevents)
        (get_fevents fam) []
    else []
  in
  fam_fevents @ fevents)
(get_family p) []
*)

let get_divorce fam =
  let divorce, separated =
    List.fold_right
      (fun evt (divorce, separated) ->
        let name = evt.Def.efam_name in
        let date = evt.efam_date in
        let place = evt.efam_place in
        let note = evt.efam_note in
        let src = evt.efam_src in
        let wl = evt.efam_witnesses in
        let x = (name, date, place, note, src, wl) in
        if name = Efam_Divorce then (x :: divorce, separated)
        else if name = Efam_Separated then (divorce, x :: separated)
        else (divorce, separated))
      (get_fevents fam) ([], [])
  in
  match (divorce, separated) with
  | [ (Efam_Divorce, date, _, _, _, _) ], _ -> Def.Divorced date
  | _, _ -> NotDivorced

(*let get_divorce = cache_fam (fun f -> get_divorce_aux)
*)

let get_separation fam =
  let divorce, separated =
    List.fold_right
      (fun evt (divorce, separated) ->
        let name = evt.Def.efam_name in
        let date = evt.efam_date in
        let place = evt.efam_place in
        let note = evt.efam_note in
        let src = evt.efam_src in
        let wl = evt.efam_witnesses in
        let x = (name, date, place, note, src, wl) in
        if name = Efam_Divorce then (x :: divorce, separated)
        else if name = Efam_Separated then (divorce, x :: separated)
        else (divorce, separated))
      (get_fevents fam) ([], [])
  in
  match (divorce, separated) with
  | _, [ (Efam_Separated, date, _, _, _, _) ] -> Def.Separated date
  | _, _ -> NotSeparated

(*let get_separation = cache_fam (fun f -> get_separation_aux)
*)

(** Returns array of surnames of person's husbands. First element of a couple in
    the array is husband's surname, second - is a husband's surname aliases *)
let husbands base (gp : _ Def.gen_person) =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
      let fam = foi base ifam in
      let husband = poi base (get_father fam) in
      let husband_surname = get_surname husband in
      let husband_surnames_aliases = get_surnames_aliases husband in
      (husband_surname, husband_surnames_aliases))
    (get_family p)

(** Return person's father titles *)
let father_titles_places base p (nobtit : person -> title list) =
  match get_parents (poi base p.Def.key_index) with
  | Some ifam ->
      let fam = foi base ifam in
      let fath = poi base (get_father fam) in
      nobtit fath
  | None -> []

let gen_gen_person_misc_names base (p : _ Def.gen_person) nobtit nobtit_fun =
  Futil.gen_person_misc_names (sou base) Istr.empty Istr.quest p.first_name
    p.surname p.public_name p.qualifiers p.aliases p.first_names_aliases
    p.surnames_aliases nobtit
    (if p.sex = Female then husbands base p else [||])
    (father_titles_places base p nobtit_fun)
  |> List.map Name.lower

(** DELETE *)

let getp fn b i = fn @@ poi b i
let get_gen_person = getp gen_person_of_person
let get_gen_ascend = getp gen_ascend_of_person
let get_gen_union = getp gen_union_of_person
let getf fn b i = fn @@ foi b i

(* let get_gen_family = getf gen_family_of_family *)
let get_gen_couple = getf gen_couple_of_family
let get_gen_descend = getf gen_descend_of_family

let rec delete_person_aux excl base ip =
  let iexcl, fexcl = excl in
  if Istr.is_dummy ip || List.mem ip iexcl then
    failwith
      ("gwdb.delete_person(" ^ Iper.to_string ip ^ ",["
      ^ (List.map Iper.to_string iexcl |> String.concat ",")
      ^ "])");
  let a = get_gen_ascend base ip in
  (* if person is the single child and their parents are empty persons
     then [ipers] contains father and mother and [ifams] contains family *)
  let ipers, ifams =
    match a.parents with
    | Some ifam ->
        (* delete ascendants *)
        delete_ascend base ip;
        (* remove person id from family descendants *)
        let children =
          (get_gen_descend base ifam).children |> Mutil.array_except ip
        in
        patch_descend base ifam { children };
        if children = [| ip |] then
          let c = get_gen_couple base ifam in
          let fath = Adef.father c in
          let moth = Adef.mother c in
          if is_empty_p base fath ~ifam && is_empty_p base moth ~ifam then
            ([ fath; moth ], [ ifam ])
          else ([], [])
        else ([], [])
    | None -> ([], [])
  in
  let del, ipers, ifams =
    let u = get_gen_union base ip in
    if u.family = [||] then (true, [], [])
    else
      Array.fold_left
        (fun (del, ipers, ifams) ifam ->
          let cpl = get_gen_couple base ifam in
          (* Test if ip is really in union in order to prevent "false positive" *)
          let fath = Adef.father cpl in
          let moth = Adef.mother cpl in
          if fath = ip || moth = ip then
            let d = get_gen_descend base ifam in
            if Array.length d.children > 1 then (false, ipers, ifams)
            else
              let sp = if ip = fath then moth else fath in
              if List.mem sp iexcl then (del, ipers, ifams)
              else if is_empty_p base sp ~ifam then
                (del, sp :: ipers, ifam :: ifams)
              else (false, ipers, ifams)
          else (
            (* Data are probably partially deleted.
               It is likely to happen when merging persons. *)
            rm_union base ifam ip;
            (del, ipers, ifams)))
        (true, ipers, ifams) u.family
  in
  if del then delete_person base ip
  else
    patch_person base ip
      { (no_person ip) with first_name = Istr.quest; surname = Istr.quest };
  let iexcl = if del then ip :: iexcl else iexcl in
  let excl = (iexcl, fexcl) in
  let excl =
    List.fold_left (fun excl ip -> delete_person_aux excl base ip) excl ipers
  in
  List.fold_left (fun excl ifam -> delete_family_aux excl base ifam) excl ifams

and is_empty_p ?ifam base sp =
  (get_gen_ascend base sp).parents = None
  && ((get_gen_union base sp).family
     = match ifam with Some i -> [| i |] | None -> [||])
  && get_gen_person base sp
     = { (no_person sp) with first_name = Istr.quest; surname = Istr.quest }

and delete_family_aux excl base ifam =
  let iexcl, fexcl = excl in
  if ifam = Ifam.dummy || List.mem ifam fexcl then
    failwith
      ("gwdb.delete_family(" ^ Ifam.to_string ifam ^ ",["
      ^ (List.map Ifam.to_string fexcl |> String.concat ",")
      ^ "])");
  let fam = foi base ifam in
  let fath = get_father fam in
  let moth = get_mother fam in
  let children = get_children fam in
  rm_union base ifam fath;
  rm_union base ifam moth;
  Array.iter (fun i -> patch_ascend base i no_ascend) children;
  delete_family base ifam;
  delete_couple base ifam;
  delete_descend base ifam;
  let fexcl = ifam :: fexcl in
  let excl = (iexcl, fexcl) in
  let excl =
    if (not (List.mem fath iexcl)) && is_empty_p base fath then
      delete_person_aux excl base fath
    else excl
  in
  let excl =
    if (not (List.mem moth iexcl)) && is_empty_p base moth then
      delete_person_aux excl base moth
    else excl
  in
  Array.fold_left
    (fun excl i ->
      if (not (List.mem i iexcl)) && is_empty_p base i then
        delete_person_aux excl base i
      else excl)
    excl children

and rm_union base ifam iper =
  Def.
    { family = (get_gen_union base iper).Def.family |> Mutil.array_except ifam }
  |> patch_union base iper

let insert_person_with_union_and_ascendants base p a u =
  let iper = new_iper base in
  let p = Def.{ p with key_index = iper } in
  insert_ascend base iper a;
  insert_union base iper u;
  insert_person base iper p;
  iper

let insert_family_with_couple_and_descendants base f c d =
  let ifam = new_ifam base in
  insert_family base ifam f;
  insert_couple base ifam c;
  insert_descend base ifam d;
  ifam

let delete_person_rec base iper = ignore @@ delete_person_aux ([], []) base iper
let delete_family_rec base ifam = ignore @@ delete_family_aux ([], []) base ifam
let p_first_name base p = Mutil.nominative (sou base (get_first_name p))
let p_surname base p = Mutil.nominative (sou base (get_surname p))

let children_of_p base p =
  Array.fold_right
    (fun ifam -> Array.fold_right List.cons (get_children @@ foi base ifam))
    (get_family p) []

let nobtitles base allowed_titles denied_titles p =
  let list = get_titles p in
  match Lazy.force allowed_titles with
  | [] -> list
  | allowed_titles -> (
      let list =
        List.fold_right
          (fun (t : _ Def.gen_title) l ->
            let id = Name.lower (sou base t.t_ident) in
            let pl = Name.lower (sou base t.t_place) in
            if pl = "" then if List.mem id allowed_titles then t :: l else l
            else if
              List.mem (id ^ "/" ^ pl) allowed_titles
              || List.mem (id ^ "/*") allowed_titles
            then t :: l
            else l)
          list []
      in
      match Lazy.force denied_titles with
      | [] -> list
      | denied_titles ->
          List.filter
            (fun (t : _ Def.gen_title) ->
              let id = Name.lower (sou base t.t_ident) in
              let pl = Name.lower (sou base t.t_place) in
              if
                List.mem (id ^ "/" ^ pl) denied_titles
                || List.mem ("*/" ^ pl) denied_titles
              then false
              else true)
            list)

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit
