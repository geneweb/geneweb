(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

type istr = int
type ifam = int
type iper = int

type person = (iper, iper, istr) gen_person
type family = (iper, ifam, istr) gen_family

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

let get_person base i = base.data.persons.get i
let get_ascend base i = base.data.ascends.get i
let get_union base i = base.data.unions.get i

let get_family base i = base.data.families.get i
let get_couple base i = base.data.couples.get i
let get_descend base i = base.data.descends.get i

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

let husbands base gp =
  let p = get_union base gp.key_index in
  Array.map begin fun ifam ->
    let fam = get_couple base ifam in
    let husband = get_person base (Adef.father fam) in
    let husband_surname = Mutil.nominative (sou base husband.surname) in
    let husband_surnames_aliases =
      List.map (sou base) husband.surnames_aliases
    in
    husband_surname, husband_surnames_aliases
  end p.family

let father_titles_places base p nobtit =
  match (get_ascend base p.key_index).parents with
  | Some ifam ->
    let fam = get_couple base ifam in
    let fath = get_person base (Adef.father fam) in
    List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.sex = Female then Array.to_list (husbands base p) else [])
    (father_titles_places base p nobtit_fun)

let patch_misc_names base ip p =
  let p = { p with key_index = ip } in
  List.iter
    (fun s -> base.func.Dbdisk.patch_name s ip)
    (gen_gen_person_misc_names base p p.titles (fun p -> p.titles))

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
      patch_misc_names base m (get_person base m) ;
      patch_misc_names base f (get_person base f) ;
      Array.iter
        begin
          fun i -> patch_misc_names base i (get_person base i)
        end
        (base.data.descends.get i).children
    end
    (base.data.unions.get ip).Def.family

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

type 'a cursor = { length : int ; get : int -> 'a option }

let persons base =
  { length = base.data.persons.len
  ; get = begin fun i ->
      (* FIXME: avoid fetching *)
      let p = base.data.persons.get i in
      if p.key_index = dummy_iper then None else Some p.key_index
    end
  } [@ocaml.warning "-42"]

let families base =
  { length = base.data.families.len
  ; get = begin fun i ->
      (* FIXME: avoid fetching *)
      let f = base.data.families.get i in
      if f.fam_index = dummy_ifam then None else Some f.fam_index
    end
  } [@ocaml.warning "-42"]

let record_access_of tab =
  { Dbdisk.load_array = (fun () -> ())
  ; get = (fun i -> tab.(i))
  ; set = (fun i v -> tab.(i) <- v)
  ; output_array = (fun oc -> Mutil.output_value_no_sharing oc (tab : _ array))
  ; len = Array.length tab
  ; clear_array = fun () -> () }

(* FIXME: lock *)
let sync ?scratch:_ base =
  Outbase.output base

let make bname particles ((persons, families, strings, bnotes) as _arrays) =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let (persons, ascends, unions) = persons in
  let (families, couples, descends) = families in
  let data =
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
  let func =
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
    Mutil.remove_file fname_back;
    if Sys.file_exists fname_def then Sys.rename fname_def fname_back ;
    Sys.rename fname_tmp fname_def

end

let read_nldb = NLDB.read
let write_nldb = NLDB.write

let base_notes_origin_file base = base.data.bnotes.norigin_file
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
      | RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Rn1Ln -> (try input_line ic with End_of_file -> "")
      | RnAll ->
        let n = in_channel_length ic in
        let s = Bytes.create n in
        really_input ic s 0 n ;
        Bytes.unsafe_to_string s
    in
    close_in ic ;
    str
  with Sys_error _ -> ""

let base_notes_read base fnotes = base_notes_read_aux base fnotes RnAll
let base_notes_read_first_line base fnotes = base_notes_read_aux base fnotes Rn1Ln
let base_notes_are_empty base fnotes = base_notes_read_aux base fnotes RnDeg = ""

