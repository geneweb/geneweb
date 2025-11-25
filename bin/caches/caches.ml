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
  let persons = Gwdb.persons base in
  let cache =
    Gwdb.Collection.fold add_person_infos_to_cache empty_cache persons
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
  if Gwdb.nb_of_persons base > node_threshold then (
    let cache = create_cache_data base in
    let base_dir = Geneweb.GWPARAM.bpath (Gwdb.bname base ^ ".gwb") in
    let lastname =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.lastname
    in
    let lastname_fname = lastname_cache_fname base_dir in
    let lastname_fname_tmp = tmp_file lastname_fname in
    write_cache_data lastname_fname_tmp lastname;
    let first_name =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.first_name
    in
    let first_name_fname = first_name_cache_fname base_dir in
    let first_name_fname_tmp = tmp_file first_name_fname in
    write_cache_data first_name_fname_tmp first_name;
    let occupation =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.occupation
    in
    let occupation_fname = occupation_cache_fname base_dir in
    let occupation_fname_tmp = tmp_file occupation_fname in
    write_cache_data occupation_fname_tmp occupation;
    let source =
      sorted_list_of_istr_set base Utf8.alphabetic_order cache.source
    in
    let source_fname = source_cache_fname base_dir in
    let source_fname_tmp = tmp_file source_fname in
    write_cache_data source_fname_tmp source;
    let place =
      sorted_list_of_istr_set base Geneweb.Place.compare_places cache.place
    in
    let place_fname = place_cache_fname base_dir in
    let place_fname_tmp = tmp_file place_fname in
    write_cache_data place_fname_tmp place;
    Files.mv lastname_fname_tmp lastname_fname;
    Files.mv first_name_fname_tmp first_name_fname;
    Files.mv occupation_fname_tmp occupation_fname;
    Files.mv source_fname_tmp source_fname;
    Files.mv place_fname_tmp place_fname)
