type cache = {
  lastname : Gwdb.IstrSet.t;
  first_name : Gwdb.IstrSet.t;
  source : Gwdb.IstrSet.t;
  occupation : Gwdb.IstrSet.t;
  place : Gwdb.IstrSet.t;
}

let add istr_set istr =
  if Gwdb.is_empty_string istr then istr_set else Gwdb.IstrSet.add istr istr_set

let add_lastname cache istr = { cache with lastname = add cache.lastname istr }

let add_first_name cache istr =
  { cache with first_name = add cache.first_name istr }

let add_source cache istr = { cache with source = add cache.source istr }

let add_occupation cache istr =
  { cache with occupation = add cache.occupation istr }

let add_place cache istr = { cache with place = add cache.place istr }

let add_place_from_pevent cache pevent =
  add_place cache (Gwdb.get_pevent_place pevent)

let add_place_from_fevent cache fevent =
  add_place cache (Gwdb.get_fevent_place fevent)

let add_source_from_pevent cache pevent =
  add_source cache (Gwdb.get_pevent_src pevent)

let add_source_from_fevent cache fevent =
  add_source cache (Gwdb.get_fevent_src fevent)

let empty_cache =
  {
    lastname = Gwdb.IstrSet.empty;
    first_name = Gwdb.IstrSet.empty;
    source = Gwdb.IstrSet.empty;
    occupation = Gwdb.IstrSet.empty;
    place = Gwdb.IstrSet.empty;
  }

let add_person_infos_to_cache cache person =
  let cache = add_lastname cache (Gwdb.get_surname person) in
  let cache = add_first_name cache (Gwdb.get_first_name person) in
  let cache = add_occupation cache (Gwdb.get_occupation person) in
  let pevents = Gwdb.get_pevents person in
  let cache = List.fold_left add_place_from_pevent cache pevents in
  let cache = List.fold_left add_source_from_pevent cache pevents in
  add_source cache (Gwdb.get_psources person)

let add_family_infos_to_cache cache family =
  let fevents = Gwdb.get_fevents family in
  let cache = List.fold_left add_place_from_fevent cache fevents in
  let cache = List.fold_left add_source_from_fevent cache fevents in
  add_source cache (Gwdb.get_fsources family)

(** Create cache files used by autocomplete *)
let create_cache_data base =
  let cache =
    Gwdb.Collection.fold add_person_infos_to_cache empty_cache
      (Gwdb.persons base)
  in
  let families = Gwdb.families base in
  Gwdb.Collection.fold add_family_infos_to_cache cache families

let lastname_cache_fname base_file = Filename.concat base_file "cache_surname"

let first_name_cache_fname base_file =
  Filename.concat base_file "cache_first_name"

let place_cache_fname base_file = Filename.concat base_file "cache_place"
let source_cache_fname base_file = Filename.concat base_file "cache_src"

let occupation_cache_fname base_file =
  Filename.concat base_file "cache_occupation"

let sorted_list_of_istr_set base cmp istr_set =
  let str_list =
    List.rev_map (Gwdb.sou base) (Gwdb.IstrSet.elements istr_set)
  in
  List.sort cmp str_list

let write_cache_data fname cache_data =
  let oc = Secure.open_out_bin fname in
  Marshal.to_channel oc cache_data [ Marshal.No_sharing ];
  close_out oc

let node_threshold = 20_000
let tmp_file fname = fname ^ ".tmp"

let write_caches base =
  let base_dir = GWPARAM.bpath (Gwdb.bname base ^ ".gwb") in
  let lastname_fname = lastname_cache_fname base_dir in
  let first_name_fname = first_name_cache_fname base_dir in
  let occupation_fname = occupation_cache_fname base_dir in
  let source_fname = source_cache_fname base_dir in
  let place_fname = place_cache_fname base_dir in
  if Gwdb.nb_of_persons base > node_threshold then (
    let cache = create_cache_data base in
    let lastname =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.lastname
    in
    let lastname_fname_tmp = tmp_file lastname_fname in
    let first_name =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.first_name
    in
    let first_name_fname_tmp = tmp_file first_name_fname in
    let occupation =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.occupation
    in
    let occupation_fname_tmp = tmp_file occupation_fname in
    let source =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.source
    in
    let source_fname_tmp = tmp_file source_fname in
    let place = sorted_list_of_istr_set base Place.compare_places cache.place in
    let place_fname_tmp = tmp_file place_fname in
    write_cache_data lastname_fname_tmp lastname;
    write_cache_data first_name_fname_tmp first_name;
    write_cache_data occupation_fname_tmp occupation;
    write_cache_data source_fname_tmp source;
    write_cache_data place_fname_tmp place;
    Files.mv lastname_fname_tmp lastname_fname;
    Files.mv first_name_fname_tmp first_name_fname;
    Files.mv occupation_fname_tmp occupation_fname;
    Files.mv source_fname_tmp source_fname;
    Files.mv place_fname_tmp place_fname)
  else
    List.iter Files.rm
      [
        lastname_fname;
        first_name_fname;
        occupation_fname;
        source_fname;
        place_fname;
      ]

let cache_file_of_cache_data base_file = function
  | `lastname -> lastname_cache_fname base_file
  | `firstname -> first_name_cache_fname base_file
  | `place -> place_cache_fname base_file
  | `source -> source_cache_fname base_file
  | `occupation -> occupation_cache_fname base_file

let has_cache ?(with_up_to_date_state = false) ~conf ~mode () =
  let base_file = GWPARAM.bpath (conf.Config.bname ^ ".gwb") in
  let file = cache_file_of_cache_data base_file mode in
  let has_up_to_date_state () =
    let patches_file = Filename.concat base_file "patches" in
    let base_file = Filename.concat base_file "base" in
    if Files.exists patches_file then
      (Unix.stat file).st_mtime > (Unix.stat patches_file).st_mtime
    else (Unix.stat file).st_mtime > (Unix.stat base_file).st_mtime
  in
  Sys.file_exists file
  && ((not with_up_to_date_state) || has_up_to_date_state ())

let add_if_valid base filter istr list =
  let s = Gwdb.sou base istr in
  if filter s then s :: list else list

let add_places_of_person base filter l person =
  let l = add_if_valid base filter (Gwdb.get_birth_place person) l in
  let l = add_if_valid base filter (Gwdb.get_death_place person) l in
  let l = add_if_valid base filter (Gwdb.get_baptism_place person) l in
  let l = add_if_valid base filter (Gwdb.get_burial_place person) l in
  List.fold_left
    (fun l pe -> add_if_valid base filter (Gwdb.get_pevent_place pe) l)
    l (Gwdb.get_pevents person)

let add_places_of_family base filter l family =
  let l = add_if_valid base filter (Gwdb.get_marriage_place family) l in
  List.fold_left
    (fun l fe -> add_if_valid base filter (Gwdb.get_fevent_place fe) l)
    l (Gwdb.get_fevents family)

let add_sources_of_person base filter l person =
  let l = add_if_valid base filter (Gwdb.get_birth_src person) l in
  let l = add_if_valid base filter (Gwdb.get_death_src person) l in
  let l = add_if_valid base filter (Gwdb.get_baptism_src person) l in
  let l = add_if_valid base filter (Gwdb.get_burial_src person) l in
  let l = add_if_valid base filter (Gwdb.get_psources person) l in
  List.fold_left
    (fun l pe -> add_if_valid base filter (Gwdb.get_pevent_src pe) l)
    l (Gwdb.get_pevents person)

let add_sources_of_family base filter l family =
  let l = add_if_valid base filter (Gwdb.get_marriage_src family) l in
  let l = add_if_valid base filter (Gwdb.get_fsources family) l in
  List.fold_left
    (fun l fe -> add_if_valid base filter (Gwdb.get_fevent_src fe) l)
    l (Gwdb.get_fevents family)

let add_lastname_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_surname person) l

let add_first_name_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_first_name person) l

let add_occupation_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_occupation person) l

let complete_with_persons_patch mode base filter data =
  let persons = Gwdb.persons_from_patch base in
  match mode with
  | `lastname ->
      Gwdb.Collection.fold (add_lastname_of_person base filter) data persons
  | `firstname ->
      Gwdb.Collection.fold (add_first_name_of_person base filter) data persons
  | `place ->
      Gwdb.Collection.fold (add_places_of_person base filter) data persons
  | `source ->
      Gwdb.Collection.fold (add_sources_of_person base filter) data persons
  | `occupation ->
      Gwdb.Collection.fold (add_occupation_of_person base filter) data persons

let complete_with_families_patch mode base filter data =
  match mode with
  | `place ->
      Gwdb.Collection.fold
        (add_places_of_family base filter)
        data
        (Gwdb.families_from_patch base)
  | `source ->
      Gwdb.Collection.fold
        (add_sources_of_family base filter)
        data
        (Gwdb.families_from_patch base)
  | `lastname | `firstname | `occupation -> data

let complete_with_patch mode base filter data =
  let data = complete_with_persons_patch mode base filter data in
  complete_with_families_patch mode base filter data

let read_cache ~conf kind =
  let bfile = GWPARAM.bpath (conf.Config.bname ^ ".gwb") in
  let cache_file = cache_file_of_cache_data bfile kind in
  let ic = Secure.open_in_bin cache_file in
  try (Marshal.from_channel ic : string list)
  with e ->
    GWPARAM.syslog `LOG_ERR
      (Printf.sprintf "Error while reading api autocomplete cache %s %s"
         cache_file (Printexc.to_string e));
    close_in ic;
    []
