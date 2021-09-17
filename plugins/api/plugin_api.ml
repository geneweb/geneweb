open Geneweb
open Config
open Plugin_api_lib

module GWD = Gwd_lib

let ns = "api"

let friend fn conf base =
  if conf.wizard || conf.friend
  then fn conf base
  else Gwd_lib.Request.incorrect_request conf

let wiz fn conf base =
  if conf.wizard then
    fn conf base
  else if conf.just_friend_wizard then
    Api_util.print_error conf `forbidden ""
  else
    (* FIXME: Needs auth headers *)
    Api_util.print_error conf `unauthorized ""

let w_lock = GWD.Request.w_lock ~onerror:(fun conf _ -> Update.error_locked conf)

let w_base =
  let none conf =
    if conf.bname = "" then Api_util.print_error conf `bad_request ""
    else Api_util.print_error conf `not_found ""
  in
  GWD.Request.w_base ~none

let () =
  let assets = !GWD.GwdPlugin.assets in
  let aux s lang =
    let e k =
      match Api_search.dico_fname assets lang k with
      | None -> false
      | Some fn -> not (Sys.file_exists fn)
    in
    if e `town || e `area_code || e `county || e `region || e `country
    then Api_marshal_dico_place.write_dico_place_set assets (Filename.concat assets s) lang
  in
  Array.iter begin fun s ->
    try Scanf.sscanf s "dico_place_%[a-z].csv" (aux s) with _ -> ()
  end (Sys.readdir assets) ;
  let aux fn _assets conf base =
    fn { conf with api_mode = true } base ; true
  in
  let aux' fn conf base =
    fn { conf with api_mode = true } base ; true
  in
  GWD.GwdPlugin.register ~ns
    [ ( "API_ADD_FIRST_FAM"
      , aux @@ fun conf _ -> Api_saisie_write.print_add_first_fam conf)
    ; ( "API_ALL_PERSONS"
      , aux @@ w_base @@ Api.print_all_persons)
    ; ( "API_ALL_FAMILIES"
      , aux @@ w_base @@ Api.print_all_families)
    ; ( "API_BASE_WARNINGS"
      , aux @@ friend @@ w_base @@ w_lock @@ Api.print_base_warnings)
    ; ( "API_CLOSE_PERSONS"
      , aux @@ w_base @@ Api_graph.print_close_person_relations)
    ; ( "API_CPL_REL"
      , aux @@ w_base @@ Api_graph.print_cpl_relation)
    ; ( "API_GRAPH_ASC"
      , aux @@ w_base @@ Api_graph.print_graph_asc)
    ; ( "API_GRAPH_DESC"
      , aux @@ w_base @@ Api_graph.print_graph_desc)
    ; ( "API_GRAPH_REL"
      , aux @@ w_base @@ Api_graph.print_graph_rel)
    ; ( "API_FIRST_AVAILABLE_PERSON"
      , aux @@ w_base @@ Api.print_first_available_person)
    ; ( "API_FIND_SOSA"
      , aux @@ w_base @@ Api.print_find_sosa)
    ; ( "API_INFO_BASE"
      , aux @@ w_base @@ Api.print_info_base)
    ; ( "API_INFO_IND"
      , aux @@ w_base @@ Api.print_info_ind)
    ; ( "API_IMAGE"
      , aux @@ w_base @@ Api.print_img)
    ; ( "API_IMAGE_ALL"
      , aux @@ w_base @@ Api.print_img_all)
    ; ( "API_IMAGE_PERSON"
      , aux @@ w_base @@ Api.print_img_person)
    ; ( "API_IMAGE_UPDATE"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api.print_updt_image)
    ; ( "API_LAST_MODIFIED_PERSONS"
      , aux @@ w_base @@ Api.print_last_modified_persons)
    ; ( "API_LAST_VISITED_PERSONS"
      , aux @@ w_base @@ Api.print_last_visited_persons)
    ; ( "API_LIST_PERSONS"
      , aux @@ w_base @@ Api.print_list_ref_person)
    ; ( "API_LOOP_BASE"
      , aux @@ w_base @@ Api.print_loop)
    ; ( "API_MAX_ANCESTORS"
      , aux @@ wiz @@ w_base @@ Api.print_max_ancestors)
    ; ( "API_NB_ANCESTORS"
      , aux @@ w_base @@ Api_saisie_read.print_nb_ancestors)
    ; ( "API_REF_PERSON_FROM_ID"
      , aux @@ w_base @@ Api.print_ref_person_from_ip)
    ; ( "API_SEARCH"
      , aux @@ w_base @@ Api_search.print_search)
    ; ( "API_GRAPH_TREE_V2"
      , aux @@ w_base @@ Api_saisie_read.print_graph_tree_v2)
    ; ( "API_PERSON_TREE"
      , aux @@ w_base @@ Api_saisie_read.print_person_tree)
    ; ( "API_FICHE_PERSON"
      , aux @@ w_base @@ Api_saisie_read.print_fiche_person)
    ; ( "API_AUTO_COMPLETE"
      , fun a -> aux' @@ wiz @@ w_base @@ Api_saisie_write.print_auto_complete a)
    ; ( "API_GET_CONFIG"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_config)
    ; ( "API_PERSON_SEARCH_LIST"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_person_search_list)
    ; ( "API_GET_PERSON_SEARCH_INFO"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_person_search_info)
    ; ( "API_ADD_CHILD"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_add_child)
    ; ( "API_ADD_CHILD_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_child_ok)
    ; ( "API_ADD_FAMILY"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_add_family)
    ; ( "API_ADD_FAMILY_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_family_ok)
    ; ( "API_ADD_FIRST_FAM_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_first_fam_ok)
    ; ( "API_ADD_PARENTS"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_add_parents)
    ; ( "API_ADD_PARENTS_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_parents_ok)
    ; ( "API_ADD_PERSON_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_ind_ok)
    ; ( "API_ADD_PERSON_START_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_ind_start_ok)
    ; ( "API_ADD_SIBLING"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_add_sibling)
    ; ( "API_ADD_SIBLING_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_add_sibling_ok)
    ; ( "API_EDIT_FAMILY_REQUEST"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_mod_family_request)
    ; ( "API_EDIT_FAMILY"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_mod_family)
    ; ( "API_EDIT_FAMILY_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_mod_family_ok)
    ; ( "API_EDIT_PERSON"
      , aux @@ wiz @@ w_base @@ Api_saisie_write.print_mod_ind)
    ; ( "API_EDIT_PERSON_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_mod_ind_ok)
    ; ( "API_DEL_FAMILY_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_del_fam_ok)
    ; ( "API_DEL_PERSON_OK"
      , aux @@ wiz @@ w_base @@ w_lock @@ Api_saisie_write.print_del_ind_ok)
    ; ( "API_NAME_FREQUENCY"
      , aux @@ w_base @@ Api.name_frequency)
    ; ( "API_PERSON_WARNINGS"
      , aux @@ w_base @@ Api.print_person_warnings)
    ; ( "API_STATS"
      , aux @@ w_base @@ Api_stats.print_stats)
    ; ( "API_SELECT_EVENTS"
      , aux @@ w_base @@ Api_graph.print_select_events)
    ]
