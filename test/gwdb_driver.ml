type ifam = int
type iper = int
type istr = int

let string_of_ifam = string_of_int
let string_of_iper = string_of_int
let int_of_iper = Fun.id
let string_of_istr = string_of_int
let ifam_of_string = int_of_string
let iper_of_string = int_of_string
let istr_of_string = int_of_string
let dummy_ifam = -1
let dummy_iper = -1
let empty_string = 0
let quest_string = 1
let eq_istr = ( = )
let eq_ifam = ( = )
let eq_iper = ( = )
let is_empty_string = eq_istr empty_string
let is_quest_string = eq_istr quest_string
let compare_iper = Int.compare
let compare_ifam = Int.compare
let compare_istr = Int.compare

type base = {
  mutable persons : (iper, iper, istr) Def.gen_person array;
  mutable ascends : ifam Def.gen_ascend array;
  mutable unions : ifam Def.gen_union array;
  mutable families : (iper, ifam, istr) Def.gen_family array;
  mutable couples : iper Def.gen_couple array;
  mutable descends : iper Def.gen_descend array;
  mutable strings : string array;
  bdir : string;
}

type family = base * (iper, ifam, istr) Def.gen_family
type person = base * (iper, iper, istr) Def.gen_person
type relation = (iper, istr) Def.gen_relation
type string_person_index
type title = istr Def.gen_title

module Utils = struct
  let is_empty_name p =
    (is_empty_string p.Def.surname || is_quest_string p.surname)
    && (is_empty_string p.first_name || is_quest_string p.first_name)

  let nbp persons_array =
    Array.fold_left
      (fun n p -> if is_empty_name p then n else succ n)
      0 persons_array

  let is_in_range arr i = 0 <= i && i < Array.length arr
end

let no_ascend = { Def.parents = None; consang = Adef.no_consang }
let no_union = { Def.family = [||] }

module Person = struct
  let create_empty_person empty what iper =
    {
      Def.first_name = what;
      surname = what;
      occ = 0;
      public_name = empty;
      image = empty;
      qualifiers = [];
      aliases = [];
      first_names_aliases = [];
      surnames_aliases = [];
      titles = [];
      rparents = [];
      related = [];
      occupation = empty;
      sex = Neuter;
      access = IfTitles;
      birth = Date.cdate_None;
      birth_place = empty;
      birth_note = empty;
      birth_src = empty;
      baptism = Date.cdate_None;
      baptism_place = empty;
      baptism_note = empty;
      baptism_src = empty;
      death = DontKnowIfDead;
      death_place = empty;
      death_note = empty;
      death_src = empty;
      burial = UnknownBurial;
      burial_place = empty;
      burial_note = empty;
      burial_src = empty;
      pevents = [];
      notes = empty;
      psources = empty;
      key_index = iper;
    }

  let empty_person (base : base) (iper : iper) : person =
    (base, create_empty_person empty_string empty_string iper)

  let get_first_name (_, p) = p.Def.first_name
  let get_surname (_, p) = p.Def.surname
  let get_occ (_, p) = p.Def.occ
  let get_public_name (_, p) = p.Def.public_name
  let get_image (_, p) = p.Def.image
  let get_qualifiers (_, p) = p.Def.qualifiers
  let get_first_names_aliases (_, p) = p.Def.first_names_aliases
  let get_surnames_aliases (_, p) = p.Def.surnames_aliases
  let get_aliases (_, p) = p.Def.aliases
  let get_access (_, p) = p.Def.access
  let get_birth (_, p) = p.Def.birth
  let get_birth_note (_, p) = p.Def.birth_note
  let get_birth_place (_, p) = p.Def.birth_place
  let get_birth_src (_, p) = p.Def.birth_src
  let get_baptism (_, p) = p.Def.baptism
  let get_baptism_note (_, p) = p.Def.baptism_note
  let get_baptism_place (_, p) = p.Def.baptism_place
  let get_baptism_src (_, p) = p.Def.baptism_src
  let get_burial (_, p) = p.Def.burial
  let get_burial_note (_, p) = p.Def.burial_note
  let get_burial_place (_, p) = p.Def.burial_place
  let get_burial_src (_, p) = p.Def.burial_src
  let get_death (_, p) = p.Def.death
  let get_death_note (_, p) = p.Def.death_note
  let get_death_place (_, p) = p.Def.death_place
  let get_death_src (_, p) = p.Def.death_src
  let get_iper (_, p) = p.Def.key_index
  let is_dummy p = eq_iper (get_iper p) dummy_iper
  let get_notes (_, p) = p.Def.notes
  let get_occupation (_, p) = p.Def.occupation

  let get_consang ((base, p) as person) =
    if is_dummy person then no_ascend.consang
    else base.ascends.(p.Def.key_index).consang

  let get_family ((base, p) as person) =
    if is_dummy person then no_union.family
    else base.unions.(p.Def.key_index).family

  let get_parents ((base, p) as person) =
    if is_dummy person then no_ascend.parents
    else base.ascends.(p.Def.key_index).parents

  let get_pevents (_, p) = p.Def.pevents
  let get_psources (_, p) = p.Def.psources
  let get_related (_, p) = p.Def.related
  let get_rparents (_, p) = p.Def.rparents
  let get_sex (_, p) = p.Def.sex
  let get_titles (_, p) = p.Def.titles
  let gen_person_of_person (_, p) = p

  let gen_ascend_of_person ((base, p) as person) =
    if is_dummy person then no_ascend else base.ascends.(p.Def.key_index)

  let gen_union_of_person ((base, p) as person) =
    if is_dummy person then no_union else base.unions.(p.Def.key_index)

  let person_of_gen_person base (p, _a, _u) = (base, p)
  let poi base iper = (base, base.persons.(iper))
  let no_person ip = create_empty_person empty_string empty_string ip
end

module Family = struct
  let create_empty_family empty ifam =
    {
      Def.marriage = Date.cdate_None;
      marriage_place = empty;
      marriage_note = empty;
      marriage_src = empty;
      witnesses = [||];
      relation = Def.NoMention;
      divorce = Def.NotDivorced;
      fevents = [];
      comment = empty;
      origin_file = empty;
      fsources = empty;
      fam_index = ifam;
    }

  let empty_family base ifam = (base, create_empty_family empty_string ifam)
  let get_marriage (_, f) = f.Def.marriage
  let get_marriage_place (_, f) = f.Def.marriage_place
  let get_marriage_note (_, f) = f.Def.marriage_note
  let get_marriage_src (_, f) = f.Def.marriage_src
  let get_witnesses (_, f) = f.Def.witnesses
  let get_relation (_, f) = f.Def.relation
  let get_divorce (_, f) = f.Def.divorce
  let get_fevents (_, f) = f.Def.fevents
  let get_comment (_, f) = f.Def.comment
  let get_origin_file (_, f) = f.Def.origin_file
  let get_fsources (_, f) = f.Def.fsources
  let get_ifam (_, f) = f.Def.fam_index
  let get_children (base, f) = base.descends.(f.Def.fam_index).children
  let get_father (base, f) = base.couples.(f.Def.fam_index) |> Adef.father
  let get_mother (base, f) = base.couples.(f.Def.fam_index) |> Adef.mother

  let get_parent_array (base, f) =
    base.couples.(f.Def.fam_index) |> Adef.parent_array

  let gen_couple_of_family (base, f) = base.couples.(f.Def.fam_index)
  let gen_descend_of_family (base, f) = base.descends.(f.Def.fam_index)
  let gen_family_of_family (_, f) = f
  let family_of_gen_family base (f, _c, _d) = (base, f)
  let foi base ifam = (base, base.families.(ifam))
  let no_family ifam = create_empty_family empty_string ifam
end

module PersEvent = struct
  type pers_event = (iper, istr) Def.gen_pers_event

  let get_pevent_name pe = pe.Def.epers_name
  let get_pevent_date pe = pe.Def.epers_date
  let get_pevent_place pe = pe.Def.epers_place
  let get_pevent_reason pe = pe.Def.epers_reason
  let get_pevent_note pe = pe.Def.epers_note
  let get_pevent_src pe = pe.Def.epers_src

  let get_pevent_witnesses pe =
    Array.map (fun (w, wk, _) -> (w, wk)) pe.Def.epers_witnesses

  let get_pevent_witness_notes pe =
    Array.map (fun (_, _, note) -> note) pe.Def.epers_witnesses

  let get_pevent_witnesses_and_notes pe = pe.Def.epers_witnesses
  let gen_pevent_of_pers_event pe = pe
  let pers_event_of_gen_pevent _base pe = pe
  let eq_pevent _ _ = assert false
end

module FamEvent = struct
  type fam_event = (iper, istr) Def.gen_fam_event

  let get_fevent_name pe = pe.Def.efam_name
  let get_fevent_date pe = pe.Def.efam_date
  let get_fevent_place pe = pe.Def.efam_place
  let get_fevent_reason pe = pe.Def.efam_reason
  let get_fevent_note pe = pe.Def.efam_note
  let get_fevent_src pe = pe.Def.efam_src

  let get_fevent_witnesses pe =
    Array.map (fun (w, wk, _) -> (w, wk)) pe.Def.efam_witnesses

  let get_fevent_witness_notes pe =
    Array.map (fun (_, _, note) -> note) pe.Def.efam_witnesses

  let get_fevent_witnesses_and_notes pe = pe.Def.efam_witnesses
  let gen_fevent_of_fam_event pe = pe
  let fam_event_of_gen_fevent _base pe = pe
  let eq_fevent _ _ = assert false
end

include PersEvent
include Person
include FamEvent
include Family

let close_base _ = ()
let sou base istr = base.strings.(istr)
let iper_exists base iper = Utils.is_in_range base.persons iper
let ifam_exists base ifam = Utils.is_in_range base.families ifam
let no_descend = { Def.children = [||] }
let no_couple = Adef.couple dummy_iper dummy_iper
let nb_of_persons base = Array.length base.persons
let nb_of_real_persons base = Utils.nbp base.persons
let nb_of_families base = Array.length base.families
let bname base = base.bdir

let insert array index data =
  let len = Array.length array in
  assert ((index < len && index >= 0) || index = len);
  if index = len then (
    let a = Array.init (len + 1) (fun i -> array.(i)) in
    a.(len) <- data;
    a)
  else (
    array.(index) <- data;
    array)

let insert_string base s =
  let s = Utf8.normalize s in
  let len = Array.length base.strings in
  let a = Array.init (len + 1) (fun i -> base.strings.(i)) in
  a.(len) <- s;
  base.strings <- a;
  len

let find_opt_string_istr base s =
  let rec loop =
    let length = Array.length base.strings in
    fun i ->
      if i >= length then None
      else if base.strings.(i) = s then Some i
      else loop (i + 1)
  in
  loop 0

let commit_patches _base = ()
let commit_notes _base _fname _s = () (* TODO *)
let new_iper base = Array.length base.persons
let new_ifam base = Array.length base.families

let patch_person base ip p =
  let a = insert base.persons ip p in
  base.persons <- a

let patch_family base ifam f =
  let a = insert base.families ifam f in
  base.families <- a

let patch_ascend base ip ascend =
  let a = insert base.ascends ip ascend in
  base.ascends <- a

let patch_union base ip u =
  let a = insert base.unions ip u in
  base.unions <- a

let patch_descend base ifam d =
  let a = insert base.descends ifam d in
  base.descends <- a

let patch_couple base ifam c =
  let a = insert base.couples ifam c in
  base.couples <- a

let delete_person base ip =
  let delp = Person.create_empty_person empty_string empty_string dummy_iper in
  patch_person base ip delp

let delete_family base ifam =
  let delf = Family.create_empty_family empty_string dummy_ifam in
  patch_family base ifam delf

let delete_ascend base ip =
  patch_ascend base ip { parents = None; consang = Adef.no_consang }

let delete_union base ip = patch_union base ip { family = [||] }
let delete_descend base ifam = patch_descend base ifam { Def.children = [||] }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple dummy_iper dummy_iper)

(* TODO : implement *)

let persons_of_first_name _base = assert false
let persons_of_surname _base = assert false
let persons_stream_of_surname_prefix _ _ = assert false
let persons_stream_of_first_name_prefix _ _ = assert false
let persons_of_lower_surname _ = assert false
let persons_of_lower_first_name _ = assert false
let spi_first _spi _str = assert false
let spi_next _spi _istr = assert false
let spi_find _spi _istr = assert false
let base_visible_get _base _pf _iper = assert false
let base_visible_write _base = assert false
let base_particles _base = assert false
let base_strings_of_first_name _base _str = assert false
let base_strings_of_surname _base _str = assert false

module Collection = struct
  type 'a t = { length : int; get : int -> 'a option }

  let length c = c.length

  let map f c =
    let get i = Option.map f (c.get i) in
    let length = c.length in
    { length; get }

  let iteri f c =
    let rec aux i =
      if i = c.length then ()
      else
        let _ = Option.map (f i) (c.get i) in
        aux (i + 1)
    in
    aux 0

  let iter f c = iteri (fun _ -> f) c

  let fold ?from ?until fn acc { length; get } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from

  let fold_until continue fn acc { get; length } =
    let rec loop acc i =
      if (not (continue acc)) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0

  let iterator { get; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then (
        match get !cursor with
        | None ->
            incr cursor;
            next ()
        | v ->
            incr cursor;
            v)
      else None
    in
    next
end

module Marker = struct
  type ('k, 'v) t = { get : 'k -> 'v; set : 'k -> 'v -> unit }

  let make (k : 'a -> int) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    {
      get = (fun x -> Array.get a (k x));
      set = (fun x v -> Array.set a (k x) v);
    }

  let get ({ get; _ } : _ t) k = get k
  let set ({ set; _ } : _ t) k = set k
end

let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  { Marker.get = (fun _ -> v); set = (fun _ _ -> ()) }

let ifam_marker c i = Marker.make Fun.id c i
let iper_marker c i = Marker.make Fun.id c i

let families ?(select = fun _ -> true) base =
  Collection.
    {
      length = Array.length base.families;
      get =
        (fun i ->
          let f = foi base i in
          if select f then Some f else None);
    }

let persons _ = assert false
let dummy_collection _ = assert false

let ifams ?(select = fun _ -> true) base =
  Collection.
    {
      length = Array.length base.families;
      get = (fun i -> if select i then Some i else None);
    }

let ipers base =
  Collection.{ length = nb_of_persons base; get = (fun i -> Some i) }

let persons_from_patch _base = assert false
let families_from_patch _base = assert false

exception Found of int

let array_find f arr =
  try
    Array.iteri (fun i v -> if f v then raise (Found i)) arr;
    None
  with Found i -> Some i

let person_of_key base fn sn occ =
  array_find
    (fun p ->
      fn = sou base p.Def.first_name && sn = sou base p.surname && occ = p.occ)
    base.persons

let persons_of_name _base _name = assert false
let date_of_last_change _ = assert false
let base_wiznotes_dir _ = "wiznotes"
let base_notes_dir _ = "notes_d"
let base_notes_origin_file _base = "origin_file" (* TODO FIX *)
let base_notes_are_empty _base _fname = true (* TODO FIX *)
let base_notes_read_first_line _ = assert false
let base_notes_read _base _fname = ""
let read_nldb _ = assert false
let write_nldb _ = assert false
let base_store = Hashtbl.create 1

(* Should not be necessary but some of the code requires to write
   without using gwdb and expects to find the base directory. *)
let sync ?scratch:_ ?tasks:_ ~save_mem:_ base =
  let bname = base.bdir in
  if not (Sys.file_exists bname) then Unix.mkdir bname 0o755;
  Hashtbl.replace base_store bname base

let open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  match Hashtbl.find_opt base_store bname with
  | Some b -> b
  | None -> raise (Invalid_argument bname)

let make bname _particles
    ((persons, ascends, unions), (families, couples, descends), strings, _) =
  let base =
    let bname =
      let suffix = ".gwb" in
      if Filename.check_suffix bname suffix then bname else bname ^ suffix
    in
    {
      persons;
      ascends;
      unions;
      families;
      couples;
      descends;
      strings;
      bdir = bname;
    }
  in
  sync ~save_mem:false base;
  base

let load_ascends_array _base = ()
let load_unions_array _base = ()
let load_couples_array _base = ()
let load_descends_array _base = ()
let load_strings_array _base = ()
let load_persons_array _base = ()
let load_families_array _base = ()
let clear_ascends_array _base = ()
let clear_unions_array _base = ()
let clear_couples_array _base = ()
let clear_descends_array _base = ()
let clear_strings_array _base = ()
let clear_persons_array _base = ()
let clear_families_array _base = ()
let set_fpoi_cache _ = assert false
let initialize_lowercase_name_index ?on_lock_error:_ ~kind:_ _ = ()
