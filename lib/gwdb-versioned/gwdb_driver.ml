


module GLegacy = (Gwdb_legacy.Gwdb_driver (*: Gwdb_driver_sig.Gwdb_driver_S*))
module G25 = (Gwdb_gnwb25 (*: Gwdb_driver_sig.Gwdb_driver_S*))

(*include GLegacy*)

let not_impl _ = assert false
               
(* TODO : implement correctly *)
type base =
  | Legacy of GLegacy.base
  | G25 of G25.base

type iper = int
type istr = int
type ifam = int

(** Database implementation for [Def.gen_relation] *)
type relation = (iper, istr) Def.gen_relation

(** Database implementation for [Def.gen_title] *)
type title = istr Def.gen_title

(** Database implementation for [Def.pers_event] *)
type pers_event = (iper, istr) Def.gen_pers_event

(** Database implementation for [Def.fam_event] *)
type fam_event = (iper, istr) Def.gen_fam_event

(** Data structure for optimised search throughout index by name
    (surname or first name). *)
type string_person_index =
  | Legacy_spi of GLegacy.string_person_index
  | G25_spi of G25.string_person_index
                
type person =
  | Legacy_person of GLegacy.person
  | G25_person of G25.person

type 'a collection =
  | Legacy_collection of 'a GLegacy.Collection.t
  | G25_collection of 'a G25.Collection.t

type ('k, 'v) marker =
  | Legacy_marker of ('k, 'v) GLegacy.Marker.t
  | G25_marker of ('k, 'v) G25.Marker.t

type family =
  | Legacy_family of GLegacy.family
  | G25_family of G25.family

let open_base bname =
  
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let ic = Secure.open_in_bin (Filename.concat bname "base") in
  
  let version_opt =
    try
      let v = really_input_string ic 8 in
      print_endline ("VERSION FOUND : " ^ v);
      Version.check_version v
    with _ -> failwith "could not find version number in base"
  in
  match version_opt with
  | Some Version.GnWb25 ->
     print_endline "====================YATA25================";
     assert false
  | _ ->
     print_endline "====================YATA24================";
(*     Dsk_format.test bname;
     print_endline "====================TEST PASSED================";*)
     Legacy (GLegacy.open_base bname)
     
(* TODO wrong *)
let wrap_base base f g = match base with
  | Legacy b -> f b
  | G25 b -> g b

(* TODO wrong *)
let close_base = function
  | Legacy base ->
     GLegacy.close_base base
  | G25 base ->
     G25.close_base base

let dummy_iper = GLegacy.dummy_iper
let dummy_ifam = GLegacy.dummy_ifam
let empty_string = GLegacy.empty_string
let quest_string = GLegacy.quest_string
let eq_istr = GLegacy.eq_istr
let is_empty_string = GLegacy.is_empty_string
let is_quest_string = GLegacy.is_quest_string
let no_person = GLegacy.no_person
let no_ascend = GLegacy.no_ascend
let no_union = GLegacy.no_union
let no_family = GLegacy.no_family
let no_descend = GLegacy.no_descend
let no_couple = GLegacy.no_couple

let spi_first spi s = match spi with
  | Legacy_spi spi -> GLegacy.spi_first spi s
  | G25_spi spi -> G25.spi_first spi s

let spi_next spi istr = match spi with
  | Legacy_spi spi -> GLegacy.spi_next spi istr
  | G25_spi spi -> G25.spi_next spi istr
                 
let spi_find spi istr = match spi with
  | Legacy_spi spi -> GLegacy.spi_find spi istr
  | G25_spi spi -> G25.spi_find spi istr

module Collection = struct

  type 'a t = 'a collection

  let map (fn : 'a -> 'b) c = match c with
    | Legacy_collection c -> Legacy_collection (GLegacy.Collection.map fn c)
    | G25_collection c -> G25_collection (G25.Collection.map fn c)

  let length = function
    | Legacy_collection c -> GLegacy.Collection.length c
    | G25_collection c -> G25.Collection.length c

  let iter fn = function
    | Legacy_collection c -> GLegacy.Collection.iter fn c
    | G25_collection c -> G25.Collection.iter fn c

  let iteri fn = function
    | Legacy_collection c -> GLegacy.Collection.iteri fn c
    | G25_collection c -> G25.Collection.iteri fn c

  (* TODO verify index from and until ? *)
  let fold ?from ?until fn acc c =
    print_endline "lets fold";
    let r = match c with
      | Legacy_collection c ->
         GLegacy.Collection.fold ~from ~until fn acc c
      | G25_collection c ->
       G25.Collection.fold ~from ~until fn acc c
    in
    print_endline "folded"; r
    
  let fold_until continue fn acc = function
    | Legacy_collection c ->
       GLegacy.Collection.fold_until continue fn acc c
    | G25_collection c ->
       G25.Collection.fold_until continue fn acc c

  let iterator = function
    | Legacy_collection c -> GLegacy.Collection.iterator c
    | G25_collection c -> G25.Collection.iterator c
end

module Marker = struct

  type ('k, 'v) t = ('k, 'v) marker

  let get m k = match m with
    | Legacy_marker m -> GLegacy.Marker.get m k
    | G25_marker m -> G25.Marker.get m k

  let set m k = match m with
    | Legacy_marker m -> GLegacy.Marker.set m k
    | G25_marker m -> G25.Marker.set m k

end

let string_of_iper = string_of_int
let string_of_ifam = string_of_int
let string_of_istr = string_of_int
let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string

let empty_person base iper = match base with
    Legacy b -> Legacy_person (GLegacy.empty_person b iper)
  | G25 b -> G25_person (G25.empty_person b iper)

let get_access person = match person with
    Legacy_person p -> GLegacy.get_access p
  | G25_person p -> G25.get_access p

let get_aliases person = match person with
    Legacy_person p -> GLegacy.get_aliases p
  | G25_person p -> G25.get_aliases p

let get_baptism person = match person with
    Legacy_person p -> GLegacy.get_baptism p
  | G25_person p -> G25.get_baptism p

let get_baptism_note person = match person with
    Legacy_person p -> GLegacy.get_baptism_note p
  | G25_person p -> G25.get_baptism_note p

let get_baptism_place person = match person with
    Legacy_person p -> GLegacy.get_baptism_place p
  | G25_person p -> G25.get_baptism_place p

let get_baptism_src person = match person with
    Legacy_person p -> GLegacy.get_baptism_src p
  | G25_person p -> G25.get_baptism_src p

let get_birth person = match person with
    Legacy_person p -> GLegacy.get_birth p
  | G25_person p -> G25.get_birth p

let get_birth_note person = match person with
    Legacy_person p -> GLegacy.get_birth_note p
  | G25_person p -> G25.get_birth_note p

let get_birth_place person = match person with
    Legacy_person p -> GLegacy.get_birth_place p
  | G25_person p -> G25.get_birth_place p

let get_birth_src person = match person with
    Legacy_person p -> GLegacy.get_birth_src p
  | G25_person p -> G25.get_birth_src p

let get_burial person = match person with
    Legacy_person p -> GLegacy.get_burial p
  | G25_person p -> G25.get_burial p

let get_burial_note person = match person with
    Legacy_person p -> GLegacy.get_burial_note p
  | G25_person p -> G25.get_burial_note p

let get_burial_place person = match person with
    Legacy_person p -> GLegacy.get_burial_place p
  | G25_person p -> G25.get_burial_place p

let get_burial_src person = match person with
    Legacy_person p -> GLegacy.get_burial_src p
  | G25_person p -> G25.get_burial_src p

let get_consang person = match person with
    Legacy_person p -> GLegacy.get_consang p
  | G25_person p -> G25.get_consang p


let get_death person = match person with
    Legacy_person p -> GLegacy.get_death p
  | G25_person p -> G25.get_death p


let get_death_note person = match person with
    Legacy_person p -> GLegacy.get_death_note p
  | G25_person p -> G25.get_death_note p

let get_death_place person = match person with
    Legacy_person p -> GLegacy.get_death_place p
  | G25_person p -> G25.get_death_place p

let get_death_src person = match person with
    Legacy_person p -> GLegacy.get_death_src p
  | G25_person p -> G25.get_death_src p

let get_family person = match person with
    Legacy_person p -> GLegacy.get_family p
  | G25_person p -> G25.get_family p

let get_first_name person = match person with
    Legacy_person p -> GLegacy.get_first_name p
  | G25_person p -> G25.get_first_name p

let get_first_names_aliases person = match person with
    Legacy_person p -> GLegacy.get_first_names_aliases p
  | G25_person p -> G25.get_first_names_aliases p

let get_image person = match person with
    Legacy_person p -> GLegacy.get_image p
  | G25_person p -> G25.get_image p

let get_iper person = match person with
    Legacy_person p -> GLegacy.get_iper p
  | G25_person p -> G25.get_iper p

let get_notes person = match person with
    Legacy_person p -> GLegacy.get_notes p
  | G25_person p -> G25.get_notes p

let get_occ person = match person with
    Legacy_person p -> GLegacy.get_occ p
  | G25_person p -> G25.get_occ p

let get_occupation person = match person with
    Legacy_person p -> GLegacy.get_occupation p
  | G25_person p -> G25.get_occupation p

let get_parents person = match person with
    Legacy_person p -> GLegacy.get_parents p
  | G25_person p -> G25.get_parents p

let get_pevents person = match person with
    Legacy_person p -> GLegacy.get_pevents p
  | G25_person p -> G25.get_pevents p

let get_psources person = match person with
    Legacy_person p -> GLegacy.get_psources p
  | G25_person p -> G25.get_psources p

let get_public_name person = match person with
    Legacy_person p -> GLegacy.get_public_name p
  | G25_person p -> G25.get_public_name p

let get_qualifiers person = match person with
    Legacy_person p -> GLegacy.get_qualifiers p
  | G25_person p -> G25.get_qualifiers p

let get_qualifiers person = match person with
    Legacy_person p -> GLegacy.get_qualifiers p
  | G25_person p -> G25.get_qualifiers p

let get_related person = match person with
    Legacy_person p -> GLegacy.get_related p
  | G25_person p -> G25.get_related p

let get_rparents person = match person with
    Legacy_person p -> GLegacy.get_rparents p
  | G25_person p -> G25.get_rparents p

let get_sex person = match person with
    Legacy_person p -> GLegacy.get_sex p
  | G25_person p -> G25.get_sex p

let get_surname person = match person with
    Legacy_person p -> GLegacy.get_surname p
  | G25_person p -> G25.get_surname p

let get_surnames_aliases person = match person with
    Legacy_person p -> GLegacy.get_surnames_aliases p
  | G25_person p -> G25.get_surnames_aliases p

let get_titles person = match person with
    Legacy_person p -> GLegacy.get_titles p
  | G25_person p -> G25.get_titles p
                  
let gen_person_of_person person = match person with
    Legacy_person p -> GLegacy.gen_person_of_person p
  | G25_person p -> G25.gen_person_of_person p

let gen_ascend_of_person person = match person with
    Legacy_person p -> GLegacy.gen_ascend_of_person p
  | G25_person p -> G25.gen_ascend_of_person p

let gen_union_of_person person = match person with
    Legacy_person p -> GLegacy.gen_union_of_person p
  | G25_person p -> G25.gen_union_of_person p

let person_of_gen_person base (p, a, u) = match base with
  | Legacy b -> Legacy_person (GLegacy.person_of_gen_person b (p, a, u))
  | G25 b -> G25_person (G25.person_of_gen_person b (p, a, u))
(*let empty_person base = wrap_base base GLegacy.empty_person not_impl*)

let poi base iper = match base with
  | Legacy b -> Legacy_person (GLegacy.poi b iper)
  | G25 b -> G25_person (G25.poi b iper)

let base_visible_get base personf iper = match base with
  | Legacy b -> GLegacy.base_visible_get b (fun p -> personf (Legacy_person p)) iper
  | G25 b ->  G25.base_visible_get b (fun p -> personf (G25_person p)) iper

let persons base = match base with
  | Legacy b -> Legacy_collection (GLegacy.Collection.map (fun p -> Legacy_person p) (GLegacy.persons b))
  | G25 b -> G25_collection (G25.Collection.map (fun p -> G25_person p) (G25.persons b))
           
let empty_family base ifam = match base with
  | Legacy b -> Legacy_family (GLegacy.empty_family b ifam)
  | G25 b -> G25_family (G25.empty_family b ifam)

let get_children family = match family with
  | Legacy_family fam -> GLegacy.get_children fam
  | G25_family fam -> G25.get_children fam

let get_comment family = match family with
  | Legacy_family fam -> GLegacy.get_comment fam
  | G25_family fam -> G25.get_comment fam

let get_divorce family = match family with
  | Legacy_family fam -> GLegacy.get_divorce fam
  | G25_family fam -> G25.get_divorce fam

let get_father family = match family with
  | Legacy_family fam -> GLegacy.get_father fam
  | G25_family fam -> G25.get_father fam

let get_mother family = match family with
  | Legacy_family fam -> GLegacy.get_mother fam
  | G25_family fam -> G25.get_mother fam
                    
let get_fevents family = match family with
  | Legacy_family fam -> GLegacy.get_fevents fam
  | G25_family fam -> G25.get_fevents fam

let get_fsources family = match family with
  | Legacy_family fam -> GLegacy.get_fsources fam
  | G25_family fam -> G25.get_fsources fam

let get_ifam family = match family with
  | Legacy_family fam -> GLegacy.get_ifam fam
  | G25_family fam -> G25.get_ifam fam

let get_marriage family = match family with
  | Legacy_family fam -> GLegacy.get_marriage fam
  | G25_family fam -> G25.get_marriage fam

let get_marriage_note = function
  | Legacy_family fam -> GLegacy.get_marriage_note fam
  | G25_family fam -> G25.get_marriage_note fam

let get_marriage_place = function
  | Legacy_family fam -> GLegacy.get_marriage_place fam
  | G25_family fam -> G25.get_marriage_place fam

let get_marriage_src = function
  | Legacy_family fam -> GLegacy.get_marriage_place fam
  | G25_family fam -> G25.get_marriage_src fam

let get_origin_file = function
  | Legacy_family fam -> GLegacy.get_origin_file fam
  | G25_family fam -> G25.get_origin_file fam

let get_parent_array = function
  | Legacy_family fam -> GLegacy.get_parent_array fam
  | G25_family fam -> G25.get_parent_array fam

let get_relation = function
  | Legacy_family fam -> GLegacy.get_relation fam
  | G25_family fam -> G25.get_relation fam

let get_witnesses = function
  | Legacy_family fam -> GLegacy.get_witnesses fam
  | G25_family fam -> G25.get_witnesses fam

let gen_couple_of_family = function
  | Legacy_family fam -> GLegacy.gen_couple_of_family fam
  | G25_family fam -> G25.gen_couple_of_family fam

let gen_descend_of_family = function
  | Legacy_family fam -> GLegacy.gen_descend_of_family fam
  | G25_family fam -> G25.gen_descend_of_family fam

let gen_family_of_family = function
  | Legacy_family fam -> GLegacy.gen_family_of_family fam
  | G25_family fam -> G25.gen_family_of_family fam

let family_of_gen_family base (fam, cpl, des) = match base with
  | Legacy b -> Legacy_family (GLegacy.family_of_gen_family b (fam, cpl, des))
  | G25 b -> G25_family (G25.family_of_gen_family b (fam, cpl, des))

let foi base ifam = match base with
  | Legacy b -> Legacy_family (GLegacy.foi b ifam)
  | G25 b -> G25_family (G25.foi b ifam)

let persons = function
  | Legacy b -> Legacy_collection (GLegacy.Collection.map (fun p -> Legacy_person p) @@ GLegacy.persons b)
  | G25 b -> G25_collection (G25.Collection.map (fun p -> G25_person p) @@ G25.persons b)

let ipers : base -> iper Collection.t = function
  | Legacy b -> Legacy_collection (GLegacy.ipers b)
  | G25 b -> G25_collection (G25.ipers b)

let ifams ?(select = fun _ -> true)  = function
  | Legacy b -> Legacy_collection (GLegacy.ifams ~select b)
  | G25 b -> G25_collection (G25.ifams ~select b)

let families ?(select = fun _ -> true) base : family collection = match base with
  | Legacy b ->
     let select fam = select (Legacy_family fam) in
     Legacy_collection (GLegacy.Collection.map (fun fam -> Legacy_family fam) (GLegacy.families ~select b))
  | G25 b ->
     let select fam = select (G25_family fam) in
     G25_collection (G25.Collection.map (fun fam -> G25_family fam) (G25.families ~select b))

(* Is it used ? *)
let dummy_collection = not_impl
let dummy_marker = not_impl

let iper_marker c v = match c with
  | Legacy_collection c -> Legacy_marker (GLegacy.iper_marker c v)
  | G25_collection c -> G25_marker (G25.iper_marker c v)

let ifam_marker c v = match c with
  | Legacy_collection c -> Legacy_marker (GLegacy.ifam_marker c v)
  | G25_collection c -> G25_marker (G25.ifam_marker c v)
                      
let iper_exists base = wrap_base base GLegacy.iper_exists G25.iper_exists
let ifam_exists base = wrap_base base GLegacy.ifam_exists not_impl

let sou base = wrap_base base GLegacy.sou not_impl
let nb_of_persons base = wrap_base base GLegacy.nb_of_persons not_impl
let nb_of_real_persons base = wrap_base base GLegacy.nb_of_real_persons not_impl
let nb_of_families base = wrap_base base GLegacy.nb_of_families not_impl
let bname base = wrap_base base GLegacy.bname not_impl
let patch_person base = wrap_base base GLegacy.patch_person not_impl
let patch_ascend base = wrap_base base GLegacy.patch_ascend not_impl
let patch_union base = wrap_base base GLegacy.patch_union not_impl
let patch_family base = wrap_base base GLegacy.patch_family not_impl
let patch_descend base = wrap_base base GLegacy.patch_descend not_impl
let patch_couple base = wrap_base base GLegacy.patch_couple not_impl
let insert_string base = wrap_base base GLegacy.insert_string not_impl
let commit_patches base = wrap_base base GLegacy.commit_patches not_impl
let commit_notes base = wrap_base base GLegacy.commit_notes not_impl
let new_iper base = wrap_base base GLegacy.new_iper not_impl
let new_ifam base = wrap_base base GLegacy.new_ifam not_impl
let insert_person base = wrap_base base GLegacy.insert_person not_impl
let insert_ascend base = wrap_base base GLegacy.insert_ascend not_impl
let insert_union base = wrap_base base GLegacy.insert_union not_impl
let insert_family base = wrap_base base GLegacy.insert_family not_impl
let insert_descend base = wrap_base base GLegacy.insert_descend not_impl
let insert_couple base = wrap_base base GLegacy.insert_couple not_impl
let delete_person base = wrap_base base GLegacy.delete_person not_impl
let delete_ascend base = wrap_base base GLegacy.delete_ascend not_impl
let delete_union base = wrap_base base GLegacy.delete_union not_impl
let delete_family base = wrap_base base GLegacy.delete_family not_impl
let delete_descend base = wrap_base base GLegacy.delete_descend not_impl
let delete_couple base = wrap_base base GLegacy.delete_couple not_impl
let person_of_key base = wrap_base base GLegacy.person_of_key not_impl
let persons_of_name base = wrap_base base GLegacy.persons_of_name not_impl

let persons_of_first_name = function
  | Legacy b -> Legacy_spi (GLegacy.persons_of_first_name b)
  | G25 b -> G25_spi (G25.persons_of_first_name b)

let persons_of_surname = function
  | Legacy b -> Legacy_spi (GLegacy.persons_of_surname b)
  | G25 b -> G25_spi (G25.persons_of_surname b)

let base_visible_write base = wrap_base base GLegacy.base_visible_write not_impl
let base_particles base = wrap_base base GLegacy.base_particles not_impl
let base_strings_of_first_name base = wrap_base base GLegacy.base_strings_of_first_name not_impl
let base_strings_of_surname base = wrap_base base GLegacy.base_strings_of_surname not_impl
let load_ascends_array base = wrap_base base GLegacy.load_ascends_array not_impl
let load_unions_array base = wrap_base base GLegacy.load_unions_array not_impl
let load_couples_array base = wrap_base base GLegacy.load_couples_array not_impl
let load_descends_array base = wrap_base base GLegacy.load_descends_array not_impl
let load_strings_array base = wrap_base base GLegacy.load_strings_array not_impl
let load_persons_array base = wrap_base base GLegacy.load_persons_array not_impl
let load_families_array base = wrap_base base GLegacy.load_families_array not_impl
let load_ascends_array base = wrap_base base GLegacy.load_ascends_array not_impl
let clear_unions_array base = wrap_base base GLegacy.clear_unions_array not_impl
let clear_ascends_array base = wrap_base base GLegacy.clear_ascends_array not_impl
let clear_couples_array base = wrap_base base GLegacy.clear_couples_array not_impl
let clear_descends_array base = wrap_base base GLegacy.clear_descends_array not_impl
let clear_strings_array base = wrap_base base GLegacy.clear_strings_array not_impl
let clear_persons_array base = wrap_base base GLegacy.clear_persons_array not_impl
let clear_families_array base = wrap_base base GLegacy.clear_families_array not_impl
let base_notes_read base = wrap_base base GLegacy.base_notes_read not_impl
let base_notes_read_first_line base = wrap_base base GLegacy.base_notes_read_first_line not_impl
let base_notes_are_empty base = wrap_base base GLegacy.base_notes_are_empty not_impl
let base_notes_origin_file base = wrap_base base GLegacy.base_notes_origin_file not_impl
let base_notes_dir base = wrap_base base GLegacy.base_notes_dir not_impl
let base_wiznotes_dir base = wrap_base base GLegacy.base_wiznotes_dir not_impl
let date_of_last_change base = wrap_base base GLegacy.date_of_last_change not_impl

let make bname particles arrays = Legacy (GLegacy.make bname particles arrays)
let read_nldb base = wrap_base base GLegacy.read_nldb not_impl
let write_nldb base = wrap_base base GLegacy.write_nldb not_impl

let sync ?(scratch = false) ~save_mem base = wrap_base base (GLegacy.sync ~scratch ~save_mem) not_impl
let gc ?(dry_run = false) ~save_mem base = wrap_base base (GLegacy.gc ~dry_run ~save_mem) not_impl
