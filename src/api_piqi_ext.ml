let piqi = Api_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _infos_base_piqi_type = Piqirun_ext.find_piqi_type "api/infos-base"
let _reference_person_piqi_type = Piqirun_ext.find_piqi_type "api/reference-person"
let _list_reference_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-reference-persons"
let _relation_parent_piqi_type = Piqirun_ext.find_piqi_type "api/relation-parent"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api/title"
let _spouse_piqi_type = Piqirun_ext.find_piqi_type "api/spouse"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api/person"
let _full_person_piqi_type = Piqirun_ext.find_piqi_type "api/full-person"
let _full_family_piqi_type = Piqirun_ext.find_piqi_type "api/full-family"
let _internal_int32_piqi_type = Piqirun_ext.find_piqi_type "api/internal-int32"
let _list_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-persons"
let _list_full_persons_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-persons"
let _list_full_families_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-families"
let _search_params_piqi_type = Piqirun_ext.find_piqi_type "api/search-params"
let _image_piqi_type = Piqirun_ext.find_piqi_type "api/image"
let _full_image_piqi_type = Piqirun_ext.find_piqi_type "api/full-image"
let _list_images_piqi_type = Piqirun_ext.find_piqi_type "api/list-images"
let _list_full_images_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-images"
let _pers_img_piqi_type = Piqirun_ext.find_piqi_type "api/pers-img"
let _list_pers_img_piqi_type = Piqirun_ext.find_piqi_type "api/list-pers-img"
let _index_piqi_type = Piqirun_ext.find_piqi_type "api/index"
let _image_address_piqi_type = Piqirun_ext.find_piqi_type "api/image-address"
let _close_persons_params_piqi_type = Piqirun_ext.find_piqi_type "api/close-persons-params"
let _person_relation_piqi_type = Piqirun_ext.find_piqi_type "api/person-relation"
let _full_person_relation_piqi_type = Piqirun_ext.find_piqi_type "api/full-person-relation"
let _list_person_relation_piqi_type = Piqirun_ext.find_piqi_type "api/list-person-relation"
let _list_full_person_relation_piqi_type = Piqirun_ext.find_piqi_type "api/list-full-person-relation"
let _anniversary_params_piqi_type = Piqirun_ext.find_piqi_type "api/anniversary-params"
let _graph_params_piqi_type = Piqirun_ext.find_piqi_type "api/graph-params"
let _graph_rel_params_piqi_type = Piqirun_ext.find_piqi_type "api/graph-rel-params"
let _cpl_rel_params_piqi_type = Piqirun_ext.find_piqi_type "api/cpl-rel-params"
let _node_piqi_type = Piqirun_ext.find_piqi_type "api/node"
let _full_node_piqi_type = Piqirun_ext.find_piqi_type "api/full-node"
let _edge_piqi_type = Piqirun_ext.find_piqi_type "api/edge"
let _graph_piqi_type = Piqirun_ext.find_piqi_type "api/graph"
let _full_graph_piqi_type = Piqirun_ext.find_piqi_type "api/full-graph"
let _all_persons_params_piqi_type = Piqirun_ext.find_piqi_type "api/all-persons-params"
let _all_families_params_piqi_type = Piqirun_ext.find_piqi_type "api/all-families-params"
let _warning_already_defined_piqi_type = Piqirun_ext.find_piqi_type "api/warning-already-defined"
let _warning_own_ancestor_piqi_type = Piqirun_ext.find_piqi_type "api/warning-own-ancestor"
let _warning_bad_sex_of_married_person_piqi_type = Piqirun_ext.find_piqi_type "api/warning-bad-sex-of-married-person"
let _warning_birth_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-birth-after-death"
let _warning_incoherent_sex_piqi_type = Piqirun_ext.find_piqi_type "api/warning-incoherent-sex"
let _warning_changed_order_of_children_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-children"
let _warning_changed_order_of_marriages_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-marriages"
let _warning_children_not_in_order_piqi_type = Piqirun_ext.find_piqi_type "api/warning-children-not-in-order"
let _warning_dead_too_early_to_be_father_piqi_type = Piqirun_ext.find_piqi_type "api/warning-dead-too-early-to-be-father"
let _warning_incoherent_ancestor_date_piqi_type = Piqirun_ext.find_piqi_type "api/warning-incoherent-ancestor-date"
let _warning_marriage_date_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-marriage-date-after-death"
let _warning_marriage_date_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-marriage-date-before-birth"
let _warning_mother_dead_before_child_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-mother-dead-before-child-birth"
let _warning_parent_born_after_child_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-born-after-child"
let _warning_parent_too_young_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-too-young"
let _warning_title_dates_error_piqi_type = Piqirun_ext.find_piqi_type "api/warning-title-dates-error"
let _warning_undefined_sex_piqi_type = Piqirun_ext.find_piqi_type "api/warning-undefined-sex"
let _warning_young_for_marriage_piqi_type = Piqirun_ext.find_piqi_type "api/warning-young-for-marriage"
let _warning_parent_too_old_piqi_type = Piqirun_ext.find_piqi_type "api/warning-parent-too-old"
let _warning_close_children_piqi_type = Piqirun_ext.find_piqi_type "api/warning-close-children"
let _warning_big_age_between_spouses_piqi_type = Piqirun_ext.find_piqi_type "api/warning-big-age-between-spouses"
let _warning_dead_old_piqi_type = Piqirun_ext.find_piqi_type "api/warning-dead-old"
let _warning_old_individual_piqi_type = Piqirun_ext.find_piqi_type "api/warning-old-individual"
let _warning_witness_date_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-witness-date-after-death"
let _warning_witness_date_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-witness-date-before-birth"
let _warning_changed_order_of_family_events_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-family-events"
let _warning_changed_order_of_person_events_piqi_type = Piqirun_ext.find_piqi_type "api/warning-changed-order-of-person-events"
let _warning_fevent_order_piqi_type = Piqirun_ext.find_piqi_type "api/warning-fevent-order"
let _warning_fwitness_event_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-fwitness-event-after-death"
let _warning_fwitness_event_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-fwitness-event-before-birth"
let _warning_pevent_order_piqi_type = Piqirun_ext.find_piqi_type "api/warning-pevent-order"
let _warning_pwitness_event_after_death_piqi_type = Piqirun_ext.find_piqi_type "api/warning-pwitness-event-after-death"
let _warning_pwitness_event_before_birth_piqi_type = Piqirun_ext.find_piqi_type "api/warning-pwitness-event-before-birth"
let _base_warnings_piqi_type = Piqirun_ext.find_piqi_type "api/base-warnings"
let _filter_date_piqi_type = Piqirun_ext.find_piqi_type "api/filter-date"
let _filter_date_range_piqi_type = Piqirun_ext.find_piqi_type "api/filter-date-range"
let _filters_piqi_type = Piqirun_ext.find_piqi_type "api/filters"
let _modification_status_piqi_type = Piqirun_ext.find_piqi_type "api/modification-status"
let _notification_birthday_params_piqi_type = Piqirun_ext.find_piqi_type "api/notification-birthday-params"
let _notification_birthday_piqi_type = Piqirun_ext.find_piqi_type "api/notification-birthday"
let _person_start_piqi_type = Piqirun_ext.find_piqi_type "api/person-start"
let _synchro_params_piqi_type = Piqirun_ext.find_piqi_type "api/synchro-params"
let _last_modifications_piqi_type = Piqirun_ext.find_piqi_type "api/last-modifications"
let _last_visits_piqi_type = Piqirun_ext.find_piqi_type "api/last-visits"
let _correspondance_family_piqi_type = Piqirun_ext.find_piqi_type "api/correspondance-family"
let _correspondance_piqi_type = Piqirun_ext.find_piqi_type "api/correspondance"
let _correspondance_list_piqi_type = Piqirun_ext.find_piqi_type "api/correspondance-list"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api/death-type"
let _marriage_type_piqi_type = Piqirun_ext.find_piqi_type "api/marriage-type"
let _divorce_type_piqi_type = Piqirun_ext.find_piqi_type "api/divorce-type"
let _relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api/relation-parent-type"
let _title_type_piqi_type = Piqirun_ext.find_piqi_type "api/title-type"
let _search_type_piqi_type = Piqirun_ext.find_piqi_type "api/search-type"
let _relation_type_piqi_type = Piqirun_ext.find_piqi_type "api/relation-type"
let _notif_birthday_params_piqi_type = Piqirun_ext.find_piqi_type "api/notif-birthday-params"


let parse_infos_base ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _infos_base_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_infos_base buf

let parse_reference_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _reference_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_reference_person buf

let parse_list_reference_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_reference_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_reference_persons buf

let parse_relation_parent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_relation_parent buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_title buf

let parse_spouse ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _spouse_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_spouse buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_person buf

let parse_full_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_person buf

let parse_full_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_family buf

let parse_internal_int32 ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _internal_int32_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_internal_int32 buf

let parse_list_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_persons buf

let parse_list_full_persons ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_persons_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_persons buf

let parse_list_full_families ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_families_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_families buf

let parse_search_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_search_params buf

let parse_image ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_image buf

let parse_full_image ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_image_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_image buf

let parse_list_images ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_images_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_images buf

let parse_list_full_images ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_images_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_images buf

let parse_pers_img ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pers_img_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_pers_img buf

let parse_list_pers_img ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_pers_img_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_pers_img buf

let parse_index ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_index buf

let parse_image_address ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_address_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_image_address buf

let parse_close_persons_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _close_persons_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_close_persons_params buf

let parse_person_relation ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_relation_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_person_relation buf

let parse_full_person_relation ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_person_relation_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_person_relation buf

let parse_list_person_relation ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_person_relation_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_person_relation buf

let parse_list_full_person_relation ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_full_person_relation_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_list_full_person_relation buf

let parse_anniversary_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _anniversary_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_anniversary_params buf

let parse_graph_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph_params buf

let parse_graph_rel_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_rel_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph_rel_params buf

let parse_cpl_rel_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _cpl_rel_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_cpl_rel_params buf

let parse_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_node buf

let parse_full_node ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_node_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_node buf

let parse_edge ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_edge buf

let parse_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_graph buf

let parse_full_graph ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_graph_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_full_graph buf

let parse_all_persons_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _all_persons_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_all_persons_params buf

let parse_all_families_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _all_families_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_all_families_params buf

let parse_warning_already_defined ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_already_defined_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_already_defined buf

let parse_warning_own_ancestor ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_own_ancestor_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_own_ancestor buf

let parse_warning_bad_sex_of_married_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_bad_sex_of_married_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_bad_sex_of_married_person buf

let parse_warning_birth_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_birth_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_birth_after_death buf

let parse_warning_incoherent_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_incoherent_sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_incoherent_sex buf

let parse_warning_changed_order_of_children ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_children_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_children buf

let parse_warning_changed_order_of_marriages ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_marriages_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_marriages buf

let parse_warning_children_not_in_order ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_children_not_in_order_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_children_not_in_order buf

let parse_warning_dead_too_early_to_be_father ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_dead_too_early_to_be_father buf

let parse_warning_incoherent_ancestor_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_incoherent_ancestor_date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_incoherent_ancestor_date buf

let parse_warning_marriage_date_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_marriage_date_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_marriage_date_after_death buf

let parse_warning_marriage_date_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_marriage_date_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_marriage_date_before_birth buf

let parse_warning_mother_dead_before_child_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_mother_dead_before_child_birth buf

let parse_warning_parent_born_after_child ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_born_after_child_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_born_after_child buf

let parse_warning_parent_too_young ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_too_young_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_too_young buf

let parse_warning_title_dates_error ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_title_dates_error_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_title_dates_error buf

let parse_warning_undefined_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_undefined_sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_undefined_sex buf

let parse_warning_young_for_marriage ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_young_for_marriage_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_young_for_marriage buf

let parse_warning_parent_too_old ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_parent_too_old_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_parent_too_old buf

let parse_warning_close_children ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_close_children_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_close_children buf

let parse_warning_big_age_between_spouses ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_big_age_between_spouses_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_big_age_between_spouses buf

let parse_warning_dead_old ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_dead_old_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_dead_old buf

let parse_warning_old_individual ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_old_individual_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_old_individual buf

let parse_warning_witness_date_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_witness_date_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_witness_date_after_death buf

let parse_warning_witness_date_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_witness_date_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_witness_date_before_birth buf

let parse_warning_changed_order_of_family_events ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_family_events_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_family_events buf

let parse_warning_changed_order_of_person_events ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_changed_order_of_person_events_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_changed_order_of_person_events buf

let parse_warning_fevent_order ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_fevent_order_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_fevent_order buf

let parse_warning_fwitness_event_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_fwitness_event_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_fwitness_event_after_death buf

let parse_warning_fwitness_event_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_fwitness_event_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_fwitness_event_before_birth buf

let parse_warning_pevent_order ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_pevent_order_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_pevent_order buf

let parse_warning_pwitness_event_after_death ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_pwitness_event_after_death_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_pwitness_event_after_death buf

let parse_warning_pwitness_event_before_birth ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _warning_pwitness_event_before_birth_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_warning_pwitness_event_before_birth buf

let parse_base_warnings ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _base_warnings_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_base_warnings buf

let parse_filter_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filter_date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filter_date buf

let parse_filter_date_range ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filter_date_range_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filter_date_range buf

let parse_filters ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filters_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_filters buf

let parse_modification_status ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modification_status_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_modification_status buf

let parse_notification_birthday_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _notification_birthday_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_notification_birthday_params buf

let parse_notification_birthday ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _notification_birthday_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_notification_birthday buf

let parse_person_start ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_start_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_person_start buf

let parse_synchro_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _synchro_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_synchro_params buf

let parse_last_modifications ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _last_modifications_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_last_modifications buf

let parse_last_visits ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _last_visits_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_last_visits buf

let parse_correspondance_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _correspondance_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_correspondance_family buf

let parse_correspondance ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _correspondance_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_correspondance buf

let parse_correspondance_list ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _correspondance_list_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_correspondance_list buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_death_type buf

let parse_marriage_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_marriage_type buf

let parse_divorce_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_divorce_type buf

let parse_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_relation_parent_type buf

let parse_title_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_title_type buf

let parse_search_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_search_type buf

let parse_relation_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_relation_type buf

let parse_notif_birthday_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _notif_birthday_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_piqi.parse_notif_birthday_params buf


let gen_infos_base ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_infos_base x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _infos_base_piqi_type `pb format x_pb ?opts

let gen_reference_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_reference_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _reference_person_piqi_type `pb format x_pb ?opts

let gen_list_reference_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_reference_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_reference_persons_piqi_type `pb format x_pb ?opts

let gen_relation_parent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_spouse ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_spouse x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _spouse_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_full_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_person_piqi_type `pb format x_pb ?opts

let gen_full_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_family_piqi_type `pb format x_pb ?opts

let gen_internal_int32 ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_internal_int32 x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _internal_int32_piqi_type `pb format x_pb ?opts

let gen_list_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_persons_piqi_type `pb format x_pb ?opts

let gen_list_full_persons ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_persons x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_persons_piqi_type `pb format x_pb ?opts

let gen_list_full_families ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_families x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_families_piqi_type `pb format x_pb ?opts

let gen_search_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _search_params_piqi_type `pb format x_pb ?opts

let gen_image ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _image_piqi_type `pb format x_pb ?opts

let gen_full_image ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_image x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_image_piqi_type `pb format x_pb ?opts

let gen_list_images ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_images x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_images_piqi_type `pb format x_pb ?opts

let gen_list_full_images ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_images x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_images_piqi_type `pb format x_pb ?opts

let gen_pers_img ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_pers_img x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _pers_img_piqi_type `pb format x_pb ?opts

let gen_list_pers_img ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_pers_img x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_pers_img_piqi_type `pb format x_pb ?opts

let gen_index ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_index x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_piqi_type `pb format x_pb ?opts

let gen_image_address ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image_address x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _image_address_piqi_type `pb format x_pb ?opts

let gen_close_persons_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_close_persons_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _close_persons_params_piqi_type `pb format x_pb ?opts

let gen_person_relation ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person_relation x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_relation_piqi_type `pb format x_pb ?opts

let gen_full_person_relation ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_person_relation x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_person_relation_piqi_type `pb format x_pb ?opts

let gen_list_person_relation ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_person_relation x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_person_relation_piqi_type `pb format x_pb ?opts

let gen_list_full_person_relation ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_person_relation x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _list_full_person_relation_piqi_type `pb format x_pb ?opts

let gen_anniversary_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_anniversary_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _anniversary_params_piqi_type `pb format x_pb ?opts

let gen_graph_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_params_piqi_type `pb format x_pb ?opts

let gen_graph_rel_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_rel_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_rel_params_piqi_type `pb format x_pb ?opts

let gen_cpl_rel_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_cpl_rel_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _cpl_rel_params_piqi_type `pb format x_pb ?opts

let gen_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _node_piqi_type `pb format x_pb ?opts

let gen_full_node ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_node x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_node_piqi_type `pb format x_pb ?opts

let gen_edge ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edge_piqi_type `pb format x_pb ?opts

let gen_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _graph_piqi_type `pb format x_pb ?opts

let gen_full_graph ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_graph x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _full_graph_piqi_type `pb format x_pb ?opts

let gen_all_persons_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_persons_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _all_persons_params_piqi_type `pb format x_pb ?opts

let gen_all_families_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_families_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _all_families_params_piqi_type `pb format x_pb ?opts

let gen_warning_already_defined ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_already_defined x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_already_defined_piqi_type `pb format x_pb ?opts

let gen_warning_own_ancestor ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_own_ancestor x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_own_ancestor_piqi_type `pb format x_pb ?opts

let gen_warning_bad_sex_of_married_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_bad_sex_of_married_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_bad_sex_of_married_person_piqi_type `pb format x_pb ?opts

let gen_warning_birth_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_birth_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_birth_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_incoherent_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_incoherent_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_incoherent_sex_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_children ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_children x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_children_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_marriages ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_marriages x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_marriages_piqi_type `pb format x_pb ?opts

let gen_warning_children_not_in_order ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_children_not_in_order x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_children_not_in_order_piqi_type `pb format x_pb ?opts

let gen_warning_dead_too_early_to_be_father ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_dead_too_early_to_be_father x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqi_type `pb format x_pb ?opts

let gen_warning_incoherent_ancestor_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_incoherent_ancestor_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_incoherent_ancestor_date_piqi_type `pb format x_pb ?opts

let gen_warning_marriage_date_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_marriage_date_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_marriage_date_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_marriage_date_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_marriage_date_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_marriage_date_before_birth_piqi_type `pb format x_pb ?opts

let gen_warning_mother_dead_before_child_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_mother_dead_before_child_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqi_type `pb format x_pb ?opts

let gen_warning_parent_born_after_child ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_born_after_child x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_born_after_child_piqi_type `pb format x_pb ?opts

let gen_warning_parent_too_young ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_too_young x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_too_young_piqi_type `pb format x_pb ?opts

let gen_warning_title_dates_error ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_title_dates_error x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_title_dates_error_piqi_type `pb format x_pb ?opts

let gen_warning_undefined_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_undefined_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_undefined_sex_piqi_type `pb format x_pb ?opts

let gen_warning_young_for_marriage ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_young_for_marriage x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_young_for_marriage_piqi_type `pb format x_pb ?opts

let gen_warning_parent_too_old ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_too_old x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_parent_too_old_piqi_type `pb format x_pb ?opts

let gen_warning_close_children ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_close_children x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_close_children_piqi_type `pb format x_pb ?opts

let gen_warning_big_age_between_spouses ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_big_age_between_spouses x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_big_age_between_spouses_piqi_type `pb format x_pb ?opts

let gen_warning_dead_old ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_dead_old x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_dead_old_piqi_type `pb format x_pb ?opts

let gen_warning_old_individual ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_old_individual x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_old_individual_piqi_type `pb format x_pb ?opts

let gen_warning_witness_date_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_witness_date_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_witness_date_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_witness_date_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_witness_date_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_witness_date_before_birth_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_family_events ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_family_events x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_family_events_piqi_type `pb format x_pb ?opts

let gen_warning_changed_order_of_person_events ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_changed_order_of_person_events x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_changed_order_of_person_events_piqi_type `pb format x_pb ?opts

let gen_warning_fevent_order ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_fevent_order x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_fevent_order_piqi_type `pb format x_pb ?opts

let gen_warning_fwitness_event_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_fwitness_event_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_fwitness_event_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_fwitness_event_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_fwitness_event_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_fwitness_event_before_birth_piqi_type `pb format x_pb ?opts

let gen_warning_pevent_order ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_pevent_order x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_pevent_order_piqi_type `pb format x_pb ?opts

let gen_warning_pwitness_event_after_death ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_pwitness_event_after_death x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_pwitness_event_after_death_piqi_type `pb format x_pb ?opts

let gen_warning_pwitness_event_before_birth ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_pwitness_event_before_birth x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _warning_pwitness_event_before_birth_piqi_type `pb format x_pb ?opts

let gen_base_warnings ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_base_warnings x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _base_warnings_piqi_type `pb format x_pb ?opts

let gen_filter_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filter_date_piqi_type `pb format x_pb ?opts

let gen_filter_date_range ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date_range x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filter_date_range_piqi_type `pb format x_pb ?opts

let gen_filters ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filters x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _filters_piqi_type `pb format x_pb ?opts

let gen_modification_status ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _modification_status_piqi_type `pb format x_pb ?opts

let gen_notification_birthday_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notification_birthday_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _notification_birthday_params_piqi_type `pb format x_pb ?opts

let gen_notification_birthday ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notification_birthday x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _notification_birthday_piqi_type `pb format x_pb ?opts

let gen_person_start ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person_start x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_start_piqi_type `pb format x_pb ?opts

let gen_synchro_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_synchro_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _synchro_params_piqi_type `pb format x_pb ?opts

let gen_last_modifications ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_last_modifications x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _last_modifications_piqi_type `pb format x_pb ?opts

let gen_last_visits ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_last_visits x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _last_visits_piqi_type `pb format x_pb ?opts

let gen_correspondance_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _correspondance_family_piqi_type `pb format x_pb ?opts

let gen_correspondance ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _correspondance_piqi_type `pb format x_pb ?opts

let gen_correspondance_list ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance_list x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _correspondance_list_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_marriage_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _marriage_type_piqi_type `pb format x_pb ?opts

let gen_divorce_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _divorce_type_piqi_type `pb format x_pb ?opts

let gen_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_title_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_type_piqi_type `pb format x_pb ?opts

let gen_search_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _search_type_piqi_type `pb format x_pb ?opts

let gen_relation_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_type_piqi_type `pb format x_pb ?opts

let gen_notif_birthday_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notif_birthday_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _notif_birthday_params_piqi_type `pb format x_pb ?opts


let print_infos_base ?opts x =
  Pervasives.print_endline (gen_infos_base x `piq ?opts)
let prerr_infos_base ?opts x =
  Pervasives.prerr_endline (gen_infos_base x `piq ?opts)

let print_reference_person ?opts x =
  Pervasives.print_endline (gen_reference_person x `piq ?opts)
let prerr_reference_person ?opts x =
  Pervasives.prerr_endline (gen_reference_person x `piq ?opts)

let print_list_reference_persons ?opts x =
  Pervasives.print_endline (gen_list_reference_persons x `piq ?opts)
let prerr_list_reference_persons ?opts x =
  Pervasives.prerr_endline (gen_list_reference_persons x `piq ?opts)

let print_relation_parent ?opts x =
  Pervasives.print_endline (gen_relation_parent x `piq ?opts)
let prerr_relation_parent ?opts x =
  Pervasives.prerr_endline (gen_relation_parent x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_spouse ?opts x =
  Pervasives.print_endline (gen_spouse x `piq ?opts)
let prerr_spouse ?opts x =
  Pervasives.prerr_endline (gen_spouse x `piq ?opts)

let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_full_person ?opts x =
  Pervasives.print_endline (gen_full_person x `piq ?opts)
let prerr_full_person ?opts x =
  Pervasives.prerr_endline (gen_full_person x `piq ?opts)

let print_full_family ?opts x =
  Pervasives.print_endline (gen_full_family x `piq ?opts)
let prerr_full_family ?opts x =
  Pervasives.prerr_endline (gen_full_family x `piq ?opts)

let print_internal_int32 ?opts x =
  Pervasives.print_endline (gen_internal_int32 x `piq ?opts)
let prerr_internal_int32 ?opts x =
  Pervasives.prerr_endline (gen_internal_int32 x `piq ?opts)

let print_list_persons ?opts x =
  Pervasives.print_endline (gen_list_persons x `piq ?opts)
let prerr_list_persons ?opts x =
  Pervasives.prerr_endline (gen_list_persons x `piq ?opts)

let print_list_full_persons ?opts x =
  Pervasives.print_endline (gen_list_full_persons x `piq ?opts)
let prerr_list_full_persons ?opts x =
  Pervasives.prerr_endline (gen_list_full_persons x `piq ?opts)

let print_list_full_families ?opts x =
  Pervasives.print_endline (gen_list_full_families x `piq ?opts)
let prerr_list_full_families ?opts x =
  Pervasives.prerr_endline (gen_list_full_families x `piq ?opts)

let print_search_params ?opts x =
  Pervasives.print_endline (gen_search_params x `piq ?opts)
let prerr_search_params ?opts x =
  Pervasives.prerr_endline (gen_search_params x `piq ?opts)

let print_image ?opts x =
  Pervasives.print_endline (gen_image x `piq ?opts)
let prerr_image ?opts x =
  Pervasives.prerr_endline (gen_image x `piq ?opts)

let print_full_image ?opts x =
  Pervasives.print_endline (gen_full_image x `piq ?opts)
let prerr_full_image ?opts x =
  Pervasives.prerr_endline (gen_full_image x `piq ?opts)

let print_list_images ?opts x =
  Pervasives.print_endline (gen_list_images x `piq ?opts)
let prerr_list_images ?opts x =
  Pervasives.prerr_endline (gen_list_images x `piq ?opts)

let print_list_full_images ?opts x =
  Pervasives.print_endline (gen_list_full_images x `piq ?opts)
let prerr_list_full_images ?opts x =
  Pervasives.prerr_endline (gen_list_full_images x `piq ?opts)

let print_pers_img ?opts x =
  Pervasives.print_endline (gen_pers_img x `piq ?opts)
let prerr_pers_img ?opts x =
  Pervasives.prerr_endline (gen_pers_img x `piq ?opts)

let print_list_pers_img ?opts x =
  Pervasives.print_endline (gen_list_pers_img x `piq ?opts)
let prerr_list_pers_img ?opts x =
  Pervasives.prerr_endline (gen_list_pers_img x `piq ?opts)

let print_index ?opts x =
  Pervasives.print_endline (gen_index x `piq ?opts)
let prerr_index ?opts x =
  Pervasives.prerr_endline (gen_index x `piq ?opts)

let print_image_address ?opts x =
  Pervasives.print_endline (gen_image_address x `piq ?opts)
let prerr_image_address ?opts x =
  Pervasives.prerr_endline (gen_image_address x `piq ?opts)

let print_close_persons_params ?opts x =
  Pervasives.print_endline (gen_close_persons_params x `piq ?opts)
let prerr_close_persons_params ?opts x =
  Pervasives.prerr_endline (gen_close_persons_params x `piq ?opts)

let print_person_relation ?opts x =
  Pervasives.print_endline (gen_person_relation x `piq ?opts)
let prerr_person_relation ?opts x =
  Pervasives.prerr_endline (gen_person_relation x `piq ?opts)

let print_full_person_relation ?opts x =
  Pervasives.print_endline (gen_full_person_relation x `piq ?opts)
let prerr_full_person_relation ?opts x =
  Pervasives.prerr_endline (gen_full_person_relation x `piq ?opts)

let print_list_person_relation ?opts x =
  Pervasives.print_endline (gen_list_person_relation x `piq ?opts)
let prerr_list_person_relation ?opts x =
  Pervasives.prerr_endline (gen_list_person_relation x `piq ?opts)

let print_list_full_person_relation ?opts x =
  Pervasives.print_endline (gen_list_full_person_relation x `piq ?opts)
let prerr_list_full_person_relation ?opts x =
  Pervasives.prerr_endline (gen_list_full_person_relation x `piq ?opts)

let print_anniversary_params ?opts x =
  Pervasives.print_endline (gen_anniversary_params x `piq ?opts)
let prerr_anniversary_params ?opts x =
  Pervasives.prerr_endline (gen_anniversary_params x `piq ?opts)

let print_graph_params ?opts x =
  Pervasives.print_endline (gen_graph_params x `piq ?opts)
let prerr_graph_params ?opts x =
  Pervasives.prerr_endline (gen_graph_params x `piq ?opts)

let print_graph_rel_params ?opts x =
  Pervasives.print_endline (gen_graph_rel_params x `piq ?opts)
let prerr_graph_rel_params ?opts x =
  Pervasives.prerr_endline (gen_graph_rel_params x `piq ?opts)

let print_cpl_rel_params ?opts x =
  Pervasives.print_endline (gen_cpl_rel_params x `piq ?opts)
let prerr_cpl_rel_params ?opts x =
  Pervasives.prerr_endline (gen_cpl_rel_params x `piq ?opts)

let print_node ?opts x =
  Pervasives.print_endline (gen_node x `piq ?opts)
let prerr_node ?opts x =
  Pervasives.prerr_endline (gen_node x `piq ?opts)

let print_full_node ?opts x =
  Pervasives.print_endline (gen_full_node x `piq ?opts)
let prerr_full_node ?opts x =
  Pervasives.prerr_endline (gen_full_node x `piq ?opts)

let print_edge ?opts x =
  Pervasives.print_endline (gen_edge x `piq ?opts)
let prerr_edge ?opts x =
  Pervasives.prerr_endline (gen_edge x `piq ?opts)

let print_graph ?opts x =
  Pervasives.print_endline (gen_graph x `piq ?opts)
let prerr_graph ?opts x =
  Pervasives.prerr_endline (gen_graph x `piq ?opts)

let print_full_graph ?opts x =
  Pervasives.print_endline (gen_full_graph x `piq ?opts)
let prerr_full_graph ?opts x =
  Pervasives.prerr_endline (gen_full_graph x `piq ?opts)

let print_all_persons_params ?opts x =
  Pervasives.print_endline (gen_all_persons_params x `piq ?opts)
let prerr_all_persons_params ?opts x =
  Pervasives.prerr_endline (gen_all_persons_params x `piq ?opts)

let print_all_families_params ?opts x =
  Pervasives.print_endline (gen_all_families_params x `piq ?opts)
let prerr_all_families_params ?opts x =
  Pervasives.prerr_endline (gen_all_families_params x `piq ?opts)

let print_warning_already_defined ?opts x =
  Pervasives.print_endline (gen_warning_already_defined x `piq ?opts)
let prerr_warning_already_defined ?opts x =
  Pervasives.prerr_endline (gen_warning_already_defined x `piq ?opts)

let print_warning_own_ancestor ?opts x =
  Pervasives.print_endline (gen_warning_own_ancestor x `piq ?opts)
let prerr_warning_own_ancestor ?opts x =
  Pervasives.prerr_endline (gen_warning_own_ancestor x `piq ?opts)

let print_warning_bad_sex_of_married_person ?opts x =
  Pervasives.print_endline (gen_warning_bad_sex_of_married_person x `piq ?opts)
let prerr_warning_bad_sex_of_married_person ?opts x =
  Pervasives.prerr_endline (gen_warning_bad_sex_of_married_person x `piq ?opts)

let print_warning_birth_after_death ?opts x =
  Pervasives.print_endline (gen_warning_birth_after_death x `piq ?opts)
let prerr_warning_birth_after_death ?opts x =
  Pervasives.prerr_endline (gen_warning_birth_after_death x `piq ?opts)

let print_warning_incoherent_sex ?opts x =
  Pervasives.print_endline (gen_warning_incoherent_sex x `piq ?opts)
let prerr_warning_incoherent_sex ?opts x =
  Pervasives.prerr_endline (gen_warning_incoherent_sex x `piq ?opts)

let print_warning_changed_order_of_children ?opts x =
  Pervasives.print_endline (gen_warning_changed_order_of_children x `piq ?opts)
let prerr_warning_changed_order_of_children ?opts x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_children x `piq ?opts)

let print_warning_changed_order_of_marriages ?opts x =
  Pervasives.print_endline (gen_warning_changed_order_of_marriages x `piq ?opts)
let prerr_warning_changed_order_of_marriages ?opts x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_marriages x `piq ?opts)

let print_warning_children_not_in_order ?opts x =
  Pervasives.print_endline (gen_warning_children_not_in_order x `piq ?opts)
let prerr_warning_children_not_in_order ?opts x =
  Pervasives.prerr_endline (gen_warning_children_not_in_order x `piq ?opts)

let print_warning_dead_too_early_to_be_father ?opts x =
  Pervasives.print_endline (gen_warning_dead_too_early_to_be_father x `piq ?opts)
let prerr_warning_dead_too_early_to_be_father ?opts x =
  Pervasives.prerr_endline (gen_warning_dead_too_early_to_be_father x `piq ?opts)

let print_warning_incoherent_ancestor_date ?opts x =
  Pervasives.print_endline (gen_warning_incoherent_ancestor_date x `piq ?opts)
let prerr_warning_incoherent_ancestor_date ?opts x =
  Pervasives.prerr_endline (gen_warning_incoherent_ancestor_date x `piq ?opts)

let print_warning_marriage_date_after_death ?opts x =
  Pervasives.print_endline (gen_warning_marriage_date_after_death x `piq ?opts)
let prerr_warning_marriage_date_after_death ?opts x =
  Pervasives.prerr_endline (gen_warning_marriage_date_after_death x `piq ?opts)

let print_warning_marriage_date_before_birth ?opts x =
  Pervasives.print_endline (gen_warning_marriage_date_before_birth x `piq ?opts)
let prerr_warning_marriage_date_before_birth ?opts x =
  Pervasives.prerr_endline (gen_warning_marriage_date_before_birth x `piq ?opts)

let print_warning_mother_dead_before_child_birth ?opts x =
  Pervasives.print_endline (gen_warning_mother_dead_before_child_birth x `piq ?opts)
let prerr_warning_mother_dead_before_child_birth ?opts x =
  Pervasives.prerr_endline (gen_warning_mother_dead_before_child_birth x `piq ?opts)

let print_warning_parent_born_after_child ?opts x =
  Pervasives.print_endline (gen_warning_parent_born_after_child x `piq ?opts)
let prerr_warning_parent_born_after_child ?opts x =
  Pervasives.prerr_endline (gen_warning_parent_born_after_child x `piq ?opts)

let print_warning_parent_too_young ?opts x =
  Pervasives.print_endline (gen_warning_parent_too_young x `piq ?opts)
let prerr_warning_parent_too_young ?opts x =
  Pervasives.prerr_endline (gen_warning_parent_too_young x `piq ?opts)

let print_warning_title_dates_error ?opts x =
  Pervasives.print_endline (gen_warning_title_dates_error x `piq ?opts)
let prerr_warning_title_dates_error ?opts x =
  Pervasives.prerr_endline (gen_warning_title_dates_error x `piq ?opts)

let print_warning_undefined_sex ?opts x =
  Pervasives.print_endline (gen_warning_undefined_sex x `piq ?opts)
let prerr_warning_undefined_sex ?opts x =
  Pervasives.prerr_endline (gen_warning_undefined_sex x `piq ?opts)

let print_warning_young_for_marriage ?opts x =
  Pervasives.print_endline (gen_warning_young_for_marriage x `piq ?opts)
let prerr_warning_young_for_marriage ?opts x =
  Pervasives.prerr_endline (gen_warning_young_for_marriage x `piq ?opts)

let print_warning_parent_too_old ?opts x =
  Pervasives.print_endline (gen_warning_parent_too_old x `piq ?opts)
let prerr_warning_parent_too_old ?opts x =
  Pervasives.prerr_endline (gen_warning_parent_too_old x `piq ?opts)

let print_warning_close_children ?opts x =
  Pervasives.print_endline (gen_warning_close_children x `piq ?opts)
let prerr_warning_close_children ?opts x =
  Pervasives.prerr_endline (gen_warning_close_children x `piq ?opts)

let print_warning_big_age_between_spouses ?opts x =
  Pervasives.print_endline (gen_warning_big_age_between_spouses x `piq ?opts)
let prerr_warning_big_age_between_spouses ?opts x =
  Pervasives.prerr_endline (gen_warning_big_age_between_spouses x `piq ?opts)

let print_warning_dead_old ?opts x =
  Pervasives.print_endline (gen_warning_dead_old x `piq ?opts)
let prerr_warning_dead_old ?opts x =
  Pervasives.prerr_endline (gen_warning_dead_old x `piq ?opts)

let print_warning_old_individual ?opts x =
  Pervasives.print_endline (gen_warning_old_individual x `piq ?opts)
let prerr_warning_old_individual ?opts x =
  Pervasives.prerr_endline (gen_warning_old_individual x `piq ?opts)

let print_warning_witness_date_after_death ?opts x =
  Pervasives.print_endline (gen_warning_witness_date_after_death x `piq ?opts)
let prerr_warning_witness_date_after_death ?opts x =
  Pervasives.prerr_endline (gen_warning_witness_date_after_death x `piq ?opts)

let print_warning_witness_date_before_birth ?opts x =
  Pervasives.print_endline (gen_warning_witness_date_before_birth x `piq ?opts)
let prerr_warning_witness_date_before_birth ?opts x =
  Pervasives.prerr_endline (gen_warning_witness_date_before_birth x `piq ?opts)

let print_warning_changed_order_of_family_events ?opts x =
  Pervasives.print_endline (gen_warning_changed_order_of_family_events x `piq ?opts)
let prerr_warning_changed_order_of_family_events ?opts x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_family_events x `piq ?opts)

let print_warning_changed_order_of_person_events ?opts x =
  Pervasives.print_endline (gen_warning_changed_order_of_person_events x `piq ?opts)
let prerr_warning_changed_order_of_person_events ?opts x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_person_events x `piq ?opts)

let print_warning_fevent_order ?opts x =
  Pervasives.print_endline (gen_warning_fevent_order x `piq ?opts)
let prerr_warning_fevent_order ?opts x =
  Pervasives.prerr_endline (gen_warning_fevent_order x `piq ?opts)

let print_warning_fwitness_event_after_death ?opts x =
  Pervasives.print_endline (gen_warning_fwitness_event_after_death x `piq ?opts)
let prerr_warning_fwitness_event_after_death ?opts x =
  Pervasives.prerr_endline (gen_warning_fwitness_event_after_death x `piq ?opts)

let print_warning_fwitness_event_before_birth ?opts x =
  Pervasives.print_endline (gen_warning_fwitness_event_before_birth x `piq ?opts)
let prerr_warning_fwitness_event_before_birth ?opts x =
  Pervasives.prerr_endline (gen_warning_fwitness_event_before_birth x `piq ?opts)

let print_warning_pevent_order ?opts x =
  Pervasives.print_endline (gen_warning_pevent_order x `piq ?opts)
let prerr_warning_pevent_order ?opts x =
  Pervasives.prerr_endline (gen_warning_pevent_order x `piq ?opts)

let print_warning_pwitness_event_after_death ?opts x =
  Pervasives.print_endline (gen_warning_pwitness_event_after_death x `piq ?opts)
let prerr_warning_pwitness_event_after_death ?opts x =
  Pervasives.prerr_endline (gen_warning_pwitness_event_after_death x `piq ?opts)

let print_warning_pwitness_event_before_birth ?opts x =
  Pervasives.print_endline (gen_warning_pwitness_event_before_birth x `piq ?opts)
let prerr_warning_pwitness_event_before_birth ?opts x =
  Pervasives.prerr_endline (gen_warning_pwitness_event_before_birth x `piq ?opts)

let print_base_warnings ?opts x =
  Pervasives.print_endline (gen_base_warnings x `piq ?opts)
let prerr_base_warnings ?opts x =
  Pervasives.prerr_endline (gen_base_warnings x `piq ?opts)

let print_filter_date ?opts x =
  Pervasives.print_endline (gen_filter_date x `piq ?opts)
let prerr_filter_date ?opts x =
  Pervasives.prerr_endline (gen_filter_date x `piq ?opts)

let print_filter_date_range ?opts x =
  Pervasives.print_endline (gen_filter_date_range x `piq ?opts)
let prerr_filter_date_range ?opts x =
  Pervasives.prerr_endline (gen_filter_date_range x `piq ?opts)

let print_filters ?opts x =
  Pervasives.print_endline (gen_filters x `piq ?opts)
let prerr_filters ?opts x =
  Pervasives.prerr_endline (gen_filters x `piq ?opts)

let print_modification_status ?opts x =
  Pervasives.print_endline (gen_modification_status x `piq ?opts)
let prerr_modification_status ?opts x =
  Pervasives.prerr_endline (gen_modification_status x `piq ?opts)

let print_notification_birthday_params ?opts x =
  Pervasives.print_endline (gen_notification_birthday_params x `piq ?opts)
let prerr_notification_birthday_params ?opts x =
  Pervasives.prerr_endline (gen_notification_birthday_params x `piq ?opts)

let print_notification_birthday ?opts x =
  Pervasives.print_endline (gen_notification_birthday x `piq ?opts)
let prerr_notification_birthday ?opts x =
  Pervasives.prerr_endline (gen_notification_birthday x `piq ?opts)

let print_person_start ?opts x =
  Pervasives.print_endline (gen_person_start x `piq ?opts)
let prerr_person_start ?opts x =
  Pervasives.prerr_endline (gen_person_start x `piq ?opts)

let print_synchro_params ?opts x =
  Pervasives.print_endline (gen_synchro_params x `piq ?opts)
let prerr_synchro_params ?opts x =
  Pervasives.prerr_endline (gen_synchro_params x `piq ?opts)

let print_last_modifications ?opts x =
  Pervasives.print_endline (gen_last_modifications x `piq ?opts)
let prerr_last_modifications ?opts x =
  Pervasives.prerr_endline (gen_last_modifications x `piq ?opts)

let print_last_visits ?opts x =
  Pervasives.print_endline (gen_last_visits x `piq ?opts)
let prerr_last_visits ?opts x =
  Pervasives.prerr_endline (gen_last_visits x `piq ?opts)

let print_correspondance_family ?opts x =
  Pervasives.print_endline (gen_correspondance_family x `piq ?opts)
let prerr_correspondance_family ?opts x =
  Pervasives.prerr_endline (gen_correspondance_family x `piq ?opts)

let print_correspondance ?opts x =
  Pervasives.print_endline (gen_correspondance x `piq ?opts)
let prerr_correspondance ?opts x =
  Pervasives.prerr_endline (gen_correspondance x `piq ?opts)

let print_correspondance_list ?opts x =
  Pervasives.print_endline (gen_correspondance_list x `piq ?opts)
let prerr_correspondance_list ?opts x =
  Pervasives.prerr_endline (gen_correspondance_list x `piq ?opts)

let print_sex ?opts x =
  Pervasives.print_endline (gen_sex x `piq ?opts)
let prerr_sex ?opts x =
  Pervasives.prerr_endline (gen_sex x `piq ?opts)

let print_death_type ?opts x =
  Pervasives.print_endline (gen_death_type x `piq ?opts)
let prerr_death_type ?opts x =
  Pervasives.prerr_endline (gen_death_type x `piq ?opts)

let print_marriage_type ?opts x =
  Pervasives.print_endline (gen_marriage_type x `piq ?opts)
let prerr_marriage_type ?opts x =
  Pervasives.prerr_endline (gen_marriage_type x `piq ?opts)

let print_divorce_type ?opts x =
  Pervasives.print_endline (gen_divorce_type x `piq ?opts)
let prerr_divorce_type ?opts x =
  Pervasives.prerr_endline (gen_divorce_type x `piq ?opts)

let print_relation_parent_type ?opts x =
  Pervasives.print_endline (gen_relation_parent_type x `piq ?opts)
let prerr_relation_parent_type ?opts x =
  Pervasives.prerr_endline (gen_relation_parent_type x `piq ?opts)

let print_title_type ?opts x =
  Pervasives.print_endline (gen_title_type x `piq ?opts)
let prerr_title_type ?opts x =
  Pervasives.prerr_endline (gen_title_type x `piq ?opts)

let print_search_type ?opts x =
  Pervasives.print_endline (gen_search_type x `piq ?opts)
let prerr_search_type ?opts x =
  Pervasives.prerr_endline (gen_search_type x `piq ?opts)

let print_relation_type ?opts x =
  Pervasives.print_endline (gen_relation_type x `piq ?opts)
let prerr_relation_type ?opts x =
  Pervasives.prerr_endline (gen_relation_type x `piq ?opts)

let print_notif_birthday_params ?opts x =
  Pervasives.print_endline (gen_notif_birthday_params x `piq ?opts)
let prerr_notif_birthday_params ?opts x =
  Pervasives.prerr_endline (gen_notif_birthday_params x `piq ?opts)


