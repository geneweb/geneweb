
module G = (Gwdb_legacy.Gwdb_driver : Gwdb_legacy.Gwdb_driver.Gwdb_driver
        with type base = Gwdb_legacy.Gwdb_driver.base
        )

(*type base = Gwdb_legacy.Gwdb_driver.base*)

include G
         
type base =
  | Legacy of G.base
  | Current

      
      
let open_base bname =
  
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let ic = Secure.open_in_bin (Filename.concat bname "base") in
  
  let version_opt =
    try
      let v = really_input_string ic 8 in
      Version.check_version v
    with _ -> failwith "could not find version number in base"
  in
  match version_opt with
  | Some Version.GnWb25 ->
     print_endline "====================YATA25================";
     assert false
  | _ ->
     print_endline "====================YATA24================";
     Dsk_format.test bname;
     print_endline "====================TEST PASSED================";
     Legacy (Gwdb_legacy.Gwdb_driver.open_base bname)
  

let wrap_base base f g = match base with
  | Legacy b -> f b
  | _ -> assert false
     
let close_base = function
  | Legacy base ->
     G.close_base base
  | _ -> assert false


let not_impl _ = assert false
(* let & base = wrap_base base G.& not_impl *)
let empty_person base = wrap_base base G.empty_person not_impl
let empty_family base = wrap_base base G.empty_family not_impl
let iper_exists base = wrap_base base G.iper_exists not_impl
let ifam_exists base = wrap_base base G.ifam_exists not_impl
let family_of_gen_family base = wrap_base base G.family_of_gen_family not_impl
let person_of_gen_person base = wrap_base base G.person_of_gen_person not_impl
let poi base = wrap_base base G.poi not_impl
let foi base = wrap_base base G.foi not_impl
let sou base = wrap_base base G.sou not_impl
let nb_of_persons base = wrap_base base G.nb_of_persons not_impl
let nb_of_real_persons base = wrap_base base G.nb_of_real_persons not_impl
let nb_of_families base = wrap_base base G.nb_of_families not_impl
let bname base = wrap_base base G.bname not_impl
let patch_person base = wrap_base base G.patch_person not_impl
let patch_ascend base = wrap_base base G.patch_ascend not_impl
let patch_union base = wrap_base base G.patch_union not_impl
let patch_family base = wrap_base base G.patch_family not_impl
let patch_descend base = wrap_base base G.patch_descend not_impl
let patch_couple base = wrap_base base G.patch_couple not_impl
let insert_string base = wrap_base base G.insert_string not_impl
let commit_patches base = wrap_base base G.commit_patches not_impl
let commit_notes base = wrap_base base G.commit_notes not_impl
let new_iper base = wrap_base base G.new_iper not_impl
let new_ifam base = wrap_base base G.new_ifam not_impl
let insert_person base = wrap_base base G.insert_person not_impl
let insert_ascend base = wrap_base base G.insert_ascend not_impl
let insert_union base = wrap_base base G.insert_union not_impl
let insert_family base = wrap_base base G.insert_family not_impl
let insert_descend base = wrap_base base G.insert_descend not_impl
let insert_couple base = wrap_base base G.insert_couple not_impl
let delete_person base = wrap_base base G.delete_person not_impl
let delete_ascend base = wrap_base base G.delete_ascend not_impl
let delete_union base = wrap_base base G.delete_union not_impl
let delete_family base = wrap_base base G.delete_family not_impl
let delete_descend base = wrap_base base G.delete_descend not_impl
let delete_couple base = wrap_base base G.delete_couple not_impl
let person_of_key base = wrap_base base G.person_of_key not_impl
let persons_of_name base = wrap_base base G.persons_of_name not_impl
let persons_of_first_name base = wrap_base base G.persons_of_first_name not_impl
let persons_of_surname base = wrap_base base G.persons_of_surname not_impl
let base_visible_get base = wrap_base base G.base_visible_get not_impl
let base_visible_write base = wrap_base base G.base_visible_write not_impl
let base_particles base = wrap_base base G.base_particles not_impl
let base_strings_of_first_name base = wrap_base base G.base_strings_of_first_name not_impl
let base_strings_of_surname base = wrap_base base G.base_strings_of_surname not_impl
let load_ascends_array base = wrap_base base G.load_ascends_array not_impl
let load_unions_array base = wrap_base base G.load_unions_array not_impl
let load_couples_array base = wrap_base base G.load_couples_array not_impl
let load_descends_array base = wrap_base base G.load_descends_array not_impl
let load_strings_array base = wrap_base base G.load_strings_array not_impl
let load_persons_array base = wrap_base base G.load_persons_array not_impl
let load_families_array base = wrap_base base G.load_families_array not_impl
let load_ascends_array base = wrap_base base G.load_ascends_array not_impl
let clear_unions_array base = wrap_base base G.clear_unions_array not_impl
let clear_ascends_array base = wrap_base base G.clear_ascends_array not_impl
let clear_couples_array base = wrap_base base G.clear_couples_array not_impl
let clear_descends_array base = wrap_base base G.clear_descends_array not_impl
let clear_strings_array base = wrap_base base G.clear_strings_array not_impl
let clear_persons_array base = wrap_base base G.clear_persons_array not_impl
let clear_families_array base = wrap_base base G.clear_families_array not_impl
let base_notes_read base = wrap_base base G.base_notes_read not_impl
let base_notes_read_first_line base = wrap_base base G.base_notes_read_first_line not_impl
let base_notes_are_empty base = wrap_base base G.base_notes_are_empty not_impl
let base_notes_origin_file base = wrap_base base G.base_notes_origin_file not_impl
let base_notes_dir base = wrap_base base G.base_notes_dir not_impl
let base_wiznotes_dir base = wrap_base base G.base_wiznotes_dir not_impl
let date_of_last_change base = wrap_base base G.date_of_last_change not_impl
let ipers base = wrap_base base G.ipers not_impl
let persons base = wrap_base base G.persons not_impl
let ifams ?(select = fun _ -> true) base = wrap_base base (G.ifams ~select) not_impl
let families ?(select = fun _ -> true) base = wrap_base base (G.families ~select) not_impl

let make bname particles arrays = Legacy (G.make bname particles arrays)
let read_nldb base = wrap_base base G.read_nldb not_impl
let write_nldb base = wrap_base base G.write_nldb not_impl

let sync ?(scratch = false) ~save_mem base = wrap_base base (G.sync ~scratch ~save_mem) not_impl
let gc ?(dry_run = false) ~save_mem base = wrap_base base (G.gc ~dry_run ~save_mem) not_impl
