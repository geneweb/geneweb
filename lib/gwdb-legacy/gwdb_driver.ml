(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk

type istr = int
type ifam = int
type iper = int

let string_of_iper = string_of_int
let string_of_ifam = string_of_int
let string_of_istr = string_of_int

let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string

let dummy_iper = -1
let dummy_ifam = -1

let empty_string = 0
let quest_string = 1

let eq_istr i1 i2 = i1 = i2
let is_empty_string istr = istr = 0
let is_quest_string istr = istr = 1

type string_person_index = Dbdisk.string_person_index

let spi_find spi = spi.find
let spi_first spi = spi.cursor
let spi_next (spi : string_person_index) istr = spi.next istr

type base = dsk_base

let base_strings_of_first_name_or_surname base s = base.func.strings_of_fsname s

let open_base bname : base = Database.opendb bname

let close_base base = base.func.cleanup ()

let sou base i = base.data.strings.get i

let bname base = Filename.(remove_extension @@ basename base.data.bdir)
let nb_of_persons base = base.data.persons.len
let nb_of_real_persons base = base.func.nb_of_real_persons ()
let nb_of_families base = base.data.families.len

let insert_string base s = base.func.Dbdisk.insert_string s

let commit_patches base = base.func.Dbdisk.commit_patches ()

let commit_notes base s = base.func.Dbdisk.commit_notes s

let person_of_key base = base.func.Dbdisk.person_of_key
let persons_of_name base = base.func.Dbdisk.persons_of_name
let persons_of_first_name base = base.func.Dbdisk.persons_of_first_name
let persons_of_surname base = base.func.Dbdisk.persons_of_surname

let base_particles base = base.data.particles
let base_strings_of_first_name = base_strings_of_first_name_or_surname
let base_strings_of_surname = base_strings_of_first_name_or_surname

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

let date_of_last_change base =
  let s =
    let bdir = base.data.bdir in
    try Unix.stat (Filename.concat bdir "patches")
    with Unix.Unix_error (_, _, _) -> Unix.stat (Filename.concat bdir "base")
  in
  s.Unix.st_mtime

let husbands base (gp : (iper, iper, istr) Def.gen_person) =
  let p = base.data.unions.get gp.Def.key_index in
  Array.map begin fun ifam ->
    let fam = base.data.couples.get ifam in
    let husband = base.data.persons.get (Adef.father fam) in
    let husband_surname = Mutil.nominative (sou base husband.surname) in
    let husband_surnames_aliases =
      List.map (sou base) husband.surnames_aliases
    in
    husband_surname, husband_surnames_aliases
  end p.family

let father_titles_places base (p : (iper, iper, istr) Def.gen_person) nobtit =
  match (base.data.ascends.get p.Def.key_index).parents with
  | Some ifam ->
    let fam = base.data.couples.get ifam in
    let fath = base.data.persons.get (Adef.father fam) in
    List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base (p : (iper, iper, istr) Def.gen_person) nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.Def.first_name) (sou p.Def.surname)
    (sou p.Def.public_name) (List.map sou p.Def.qualifiers) (List.map sou p.Def.aliases)
    (List.map sou p.Def.first_names_aliases) (List.map sou p.Def.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.Def.sex = Def.Female then Array.to_list (husbands base p) else [])
    (father_titles_places base p nobtit_fun)

let patch_misc_names base ip (p : (iper, iper, istr) Def.gen_person) =
  let p = { p with Def.key_index = ip } in
  List.iter
    (fun s -> base.func.Dbdisk.patch_name s ip)
    (gen_gen_person_misc_names base p p.Def.titles (fun p -> p.titles))

let patch_person base ip (p : (iper, iper, istr) Def.gen_person) =
  base.func.Dbdisk.patch_person ip p ;
  let s = sou base p.first_name ^ " " ^ sou base p.surname in
  base.func.Dbdisk.patch_name s ip ;
  patch_misc_names base ip p ;
  Array.iter
    begin fun i ->
      let cpl = base.data.couples.get i in
      let m = Adef.mother cpl in
      let f = Adef.father cpl in
      patch_misc_names base m (base.data.persons.get m) ;
      patch_misc_names base f (base.data.persons.get f) ;
      Array.iter
        begin
          fun i -> patch_misc_names base i (base.data.persons.get i)
        end
        (base.data.descends.get i).children
    end
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
    { first_name = quest_string
    ; surname = quest_string
    ; occ = 0
    ; image = empty_string
    ; first_names_aliases = []
    ; surnames_aliases = []
    ; public_name = empty_string
    ; qualifiers = []
    ; titles = []
    ; rparents = []
    ; related = []
    ; aliases = []
    ; occupation = empty_string
    ; sex = Neuter
    ; access = Private
    ; birth = Adef.cdate_None
    ; birth_place = empty_string
    ; birth_note = empty_string
    ; birth_src = empty_string
    ; baptism = Adef.cdate_None
    ; baptism_place = empty_string
    ; baptism_note = empty_string
    ; baptism_src = empty_string
    ; death = DontKnowIfDead
    ; death_place = empty_string
    ; death_note = empty_string
    ; death_src = empty_string
    ; burial = UnknownBurial
    ; burial_place = empty_string
    ; burial_note = empty_string
    ; burial_src = empty_string
    ; pevents = []
    ; notes = empty_string
    ; psources = empty_string
    ; key_index = dummy_iper
    }

let delete_ascend base ip =
  patch_ascend base ip { parents = None ; consang = Adef.no_consang }

let delete_union base ip =
  patch_union base ip { family = [||] }

let delete_family base ifam =
  patch_family base ifam
    { marriage = Adef.cdate_None
    ; marriage_place = empty_string
    ; marriage_note = empty_string
    ; marriage_src = empty_string
    ; relation = Married
    ; divorce = NotDivorced
    ; fevents = []
    ; witnesses = [||]
    ; comment = empty_string
    ; origin_file = empty_string
    ; fsources = empty_string
    ; fam_index = dummy_ifam
    }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple dummy_iper dummy_iper)

let delete_descend base ifam =
  patch_descend base ifam { children = [||] }

let new_iper base = base.data.persons.len

let new_ifam base = base.data.families.len

let record_access_of tab =
  { Dbdisk.load_array = (fun () -> ())
  ; get = (fun i -> tab.(i))
  ; get_nopending = (fun i -> tab.(i))
  ; set = (fun i v -> tab.(i) <- v)
  ; output_array = (fun oc -> Dutil.output_value_no_sharing oc (tab : _ array))
  ; len = Array.length tab
  ; clear_array = fun () -> () }

(* FIXME: lock *)
let sync ?scratch:_ base =
  Outbase.output base

let make bname particles ((persons, families, strings, bnotes) as _arrays) : Dbdisk.dsk_base =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let (persons, ascends, unions) = persons in
  let (families, couples, descends) = families in
  let data : Dbdisk.base_data =
    { persons = record_access_of persons
    ; ascends = record_access_of ascends
    ; unions = record_access_of unions
    ; families = record_access_of families
    ; visible =
       { v_write = (fun _ -> assert false)
       ; v_get = (fun _ -> assert false)
       }
    ; couples = record_access_of couples
    ; descends = record_access_of descends
    ; strings = record_access_of strings
    ; particles
    ; bnotes = bnotes
    ; bdir = bdir }
  in
  let func : Dbdisk.base_func =
    { person_of_key = (fun _ -> assert false)
    ; persons_of_name = (fun _ -> assert false)
    ; strings_of_fsname = (fun _ -> assert false)
    ; persons_of_surname =
       { find = (fun _ -> assert false)
       ; cursor = (fun _ -> assert false)
       ; next = (fun _ -> assert false)
       }
    ; persons_of_first_name =
       { find = (fun _ -> assert false)
       ; cursor = (fun _ -> assert false)
       ; next = (fun _ -> assert false)
       }
    ; patch_person = (fun _ -> assert false)
    ; patch_ascend = (fun _ -> assert false)
    ; patch_union = (fun _ -> assert false)
    ; patch_family = (fun _ -> assert false)
    ; patch_couple = (fun _ -> assert false)
    ; patch_descend = (fun _ -> assert false)
    ; patch_name = (fun _ -> assert false)
    ; insert_string = (fun _ -> assert false)
    ; commit_patches = (fun _ -> assert false)
    ; commit_notes = (fun _ -> assert false)
    ; cleanup = (fun _ -> ())
    ; nb_of_real_persons = (fun _ -> assert false)
    }
  in
  sync ~scratch:true { data ; func } ;
  open_base bname

let bfname base fname =
  Filename.concat base.data.bdir fname

module NLDB = struct

  let magic = "GWNL0010"

  let read base =
    let fname = bfname base "notes_links" in
    match try Some (open_in_bin fname) with Sys_error _ -> None with
    | Some ic ->
      let r =
        if Mutil.check_magic magic ic
        then (input_value ic : (iper, iper) Def.NLDB.t)
        else failwith "unsupported nldb format"
      in
      close_in ic ; r
    | None -> []

  let write base db =
    let fname_tmp = bfname base "1notes_links" in
    let fname_def = bfname base "notes_links" in
    let fname_back = bfname base "notes_links~" in
    let oc = open_out_bin fname_tmp in
    output_string oc magic ;
    output_value oc (db : (iper, ifam) Def.NLDB.t) ;
    close_out oc ;
    Mutil.rm fname_back;
    Mutil.mv fname_def fname_back ;
    Sys.rename fname_tmp fname_def

end

let read_nldb = NLDB.read
let write_nldb = NLDB.write

let base_notes_origin_file base = base.data.bnotes.Def.norigin_file
let base_notes_dir _base = "notes_d"
let base_wiznotes_dir _base = "wiznotes"

let base_notes_read_aux base fnotes mode =
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  try
    let ic = Secure.open_in @@ Filename.concat base.data.bdir fname in
    let str =
      match mode with
      | Def.RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Def.Rn1Ln -> (try input_line ic with End_of_file -> "")
      | Def.RnAll ->
        let n = in_channel_length ic in
        let s = Bytes.create n in
        really_input ic s 0 n ;
        Bytes.unsafe_to_string s
    in
    close_in ic ;
    str
  with Sys_error _ -> ""

let base_notes_read base fnotes = base_notes_read_aux base fnotes Def.RnAll
let base_notes_read_first_line base fnotes = base_notes_read_aux base fnotes Def.Rn1Ln
let base_notes_are_empty base fnotes = base_notes_read_aux base fnotes Def.RnDeg = ""

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

let cache f a get set x =
  match get x with
  | Some v -> v
  | None -> let v = f a in set x (Some v) ; v

(** Persons *)

type person =
  { base : base
  ; iper : iper
  ; mutable p : (iper, iper, istr) gen_person option
  ; mutable a : ifam gen_ascend option
  ; mutable u : ifam gen_union option }

let cache_per f ({ base ; iper ; _ } as p) =
  f (cache base.data.persons.get iper (fun p -> p.p) (fun p v -> p.p <- v) p)

let cache_asc f ({ base ; iper ; _ } as p) =
  f (cache base.data.ascends.get iper (fun p -> p.a) (fun p v -> p.a <- v) p)

let cache_uni f ({ base ; iper ; _ } as p) =
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

type family =
  { base : base
  ; ifam : ifam
  ; mutable f : (iper, ifam, istr) gen_family option
  ; mutable c : iper gen_couple option
  ; mutable d : iper gen_descend option
  }

let cache_fam f ({ base ; ifam ; _ } as fam)  =
  f (cache base.data.families.get ifam (fun f -> f.f) (fun f v -> f.f <- v) fam)

let cache_cpl f ({ base ; ifam ; _ } as fam) =
  f (cache base.data.couples.get ifam (fun f -> f.c) (fun f v -> f.c <- v) fam)

let cache_des f ({ base ; ifam ; _ } as fam) =
  f (cache base.data.descends.get ifam (fun f -> f.d) (fun f v -> f.d <- v) fam)

let gen_couple_of_family = cache_cpl (fun c -> c)
let gen_descend_of_family = cache_des (fun d -> d)
let gen_family_of_family = cache_fam (fun f -> f)
let get_children = cache_des (fun d -> d.Def.children)
let get_comment = cache_fam (fun f -> f.Def.comment)
let get_ifam = cache_fam (fun f -> f.Def.fam_index)
let get_divorce = cache_fam (fun f -> f.Def.divorce)
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

let no_person ip =
  { first_name = empty_string
  ; surname = empty_string
  ; occ = 0
  ; image = empty_string
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; public_name = empty_string
  ; qualifiers = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; aliases = []
  ; occupation = empty_string
  ; sex = Neuter
  ; access = Private
  ; birth = Adef.cdate_None
  ; birth_place = empty_string
  ; birth_note = empty_string
  ; birth_src = empty_string
  ; baptism = Adef.cdate_None
  ; baptism_place = empty_string
  ; baptism_note = empty_string
  ; baptism_src = empty_string
  ; death = DontKnowIfDead
  ; death_place = empty_string
  ; death_note = empty_string
  ; death_src = empty_string
  ; burial = UnknownBurial
  ; burial_place = empty_string
  ; burial_note = empty_string
  ; burial_src = empty_string
  ; pevents = []
  ; notes = empty_string
  ; psources = empty_string
  ; key_index = ip }

let no_ascend = { parents = None ; consang = Adef.no_consang }

let no_union = { family = [||] }

let empty_person base iper =
  { base
  ; iper
  ; p = Some (no_person iper)
  ; a = Some no_ascend
  ; u = Some no_union
  } [@ocaml.warning "-42"]

let person_of_gen_person base (p, a, u) =
  { base
  ; iper = p.key_index
  ; p = Some p
  ; a = Some a
  ; u = Some u
  } [@ocaml.warning "-42"]

let family_of_gen_family base (f, c, d) =
  { base
  ; ifam = f.fam_index
  ; f = Some f
  ; c = Some c
  ; d = Some d
  } [@ocaml.warning "-42"]

let poi base iper =
  if iper = dummy_iper then empty_person base iper
  else { base ; iper ; p = None ; a = None ; u = None } [@ocaml.warning "-42"]

let no_family ifam =
  { marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [||]
  ; relation = Def.NoMention
  ; divorce = Def.NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = ifam
  }

let no_couple = Adef.couple dummy_iper dummy_iper

let no_descend = { Def.children = [||] }

let empty_family base ifam =
  { base
  ; ifam
  ; f = Some (no_family ifam)
  ; c = Some no_couple
  ; d = Some no_descend }

let foi base ifam =
  if ifam = dummy_ifam then empty_family base ifam
  else { base ; ifam ; f = None ; c = None ; d = None }

module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a option
    }

  let map (fn : 'a -> 'b) c =
    { length = c.length
    ; get = (fun i ->  match c.get i with Some x -> Some (fn x) | None -> None)
    }

  let length { length ; _ } = length

  let iter fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn x | None -> () done

  let iteri fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn i x | None -> () done

  let fold ?from ?until fn acc { get ; length } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then
        match get !cursor with
        | None -> incr cursor ; next ()
        |  v -> incr cursor ; v
      else None
    in
    next

end

module Marker = struct

  type ('k, 'v) t =
    { get : 'k -> 'v
    ; set : 'k -> 'v -> unit
    }

  let make (k : 'a -> 'k) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    { get = (fun x -> Array.get a (k x) )
    ; set = (fun x v -> Array.set a (k x) v) }

  let get ({ get ; _ } : _ t) k = get k
  let set ({ set ; _ } : _ t) k = set k

end

let pers_colletion fn base =
  { Collection.length = nb_of_persons base
  ; get = (fun i ->
        let p = poi base i in
        if get_iper p = dummy_iper then None else Some (fn p))
  }

let ipers = pers_colletion get_iper
let persons = pers_colletion (fun p -> p)

let iper_marker c i = Marker.make (fun i -> i) c i

let fam_collection fn base =
  { Collection.length = nb_of_families base
  ; get = (fun i ->
        let f = foi base i in
        if get_ifam f = dummy_ifam then None else Some (fn f))
  }

let ifams = fam_collection get_ifam
let families = fam_collection (fun f -> f)

let dummy_collection _ =
  { Collection.length = -1
  ; get = fun _ -> None
  }

let ifam_marker c i = Marker.make (fun i -> i) c i

let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  { Marker.get = begin fun _ -> v end
  ; set = begin fun _ _ -> () end
  }


(* Restrict file *)

(* FIXME: these values should not be global *)
let visible_ref : (iper, bool) Hashtbl.t option ref = ref None

let read_or_create_visible base =
  let fname = Filename.concat base.data.bdir "restrict" in
  let visible =
    if Sys.file_exists fname then
      let ic = Secure.open_in fname in
      let visible =
        if Mutil.check_magic Mutil.executable_magic ic
        then input_value ic
        else Hashtbl.create (nb_of_persons base)
      in
      close_in ic ;
      visible
    else Hashtbl.create (nb_of_persons base)
  in
  visible_ref := Some visible ;
  visible

let base_visible_write base =
  let fname = Filename.concat base.data.bdir "restrict" in
  match !visible_ref with
  | Some visible ->
    let oc = Secure.open_out fname in
    output_string oc Mutil.executable_magic ;
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
    Hashtbl.add visible i status ;
    visible_ref := Some visible;
    status
  | Some b -> b
