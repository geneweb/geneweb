let piqi = Api_piqi.piqi
  
let _ = Piqirun_ext.init_piqi piqi
  
let _int64_piqtype = Piqirun_ext.find_piqtype "int64"
  
let _int32_piqtype = Piqirun_ext.find_piqtype "int32"
  
let _protobuf_int64_piqtype = Piqirun_ext.find_piqtype "protobuf-int64"
  
let _string_piqtype = Piqirun_ext.find_piqtype "string"
  
let _protobuf_int32_piqtype = Piqirun_ext.find_piqtype "protobuf-int32"
  
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
  
let _infos_base_piqtype = Piqirun_ext.find_piqtype "api/infos-base"
  
let _reference_person_piqtype =
  Piqirun_ext.find_piqtype "api/reference-person"
  
let _list_reference_persons_piqtype =
  Piqirun_ext.find_piqtype "api/list-reference-persons"
  
let _relation_parent_piqtype = Piqirun_ext.find_piqtype "api/relation-parent"
  
let _title_piqtype = Piqirun_ext.find_piqtype "api/title"
  
let _spouse_piqtype = Piqirun_ext.find_piqtype "api/spouse"
  
let _person_piqtype = Piqirun_ext.find_piqtype "api/person"
  
let _full_person_piqtype = Piqirun_ext.find_piqtype "api/full-person"
  
let _full_family_piqtype = Piqirun_ext.find_piqtype "api/full-family"
  
let _internal_int32_piqtype = Piqirun_ext.find_piqtype "api/internal-int32"
  
let _list_persons_piqtype = Piqirun_ext.find_piqtype "api/list-persons"
  
let _list_full_persons_piqtype =
  Piqirun_ext.find_piqtype "api/list-full-persons"
  
let _list_full_families_piqtype =
  Piqirun_ext.find_piqtype "api/list-full-families"
  
let _search_params_piqtype = Piqirun_ext.find_piqtype "api/search-params"
  
let _image_piqtype = Piqirun_ext.find_piqtype "api/image"
  
let _full_image_piqtype = Piqirun_ext.find_piqtype "api/full-image"
  
let _list_images_piqtype = Piqirun_ext.find_piqtype "api/list-images"
  
let _list_full_images_piqtype =
  Piqirun_ext.find_piqtype "api/list-full-images"
  
let _pers_img_piqtype = Piqirun_ext.find_piqtype "api/pers-img"
  
let _list_pers_img_piqtype = Piqirun_ext.find_piqtype "api/list-pers-img"
  
let _index_piqtype = Piqirun_ext.find_piqtype "api/index"
  
let _image_address_piqtype = Piqirun_ext.find_piqtype "api/image-address"
  
let _close_persons_params_piqtype =
  Piqirun_ext.find_piqtype "api/close-persons-params"
  
let _person_relation_piqtype = Piqirun_ext.find_piqtype "api/person-relation"
  
let _full_person_relation_piqtype =
  Piqirun_ext.find_piqtype "api/full-person-relation"
  
let _list_person_relation_piqtype =
  Piqirun_ext.find_piqtype "api/list-person-relation"
  
let _list_full_person_relation_piqtype =
  Piqirun_ext.find_piqtype "api/list-full-person-relation"
  
let _anniversary_params_piqtype =
  Piqirun_ext.find_piqtype "api/anniversary-params"
  
let _graph_params_piqtype = Piqirun_ext.find_piqtype "api/graph-params"
  
let _graph_rel_params_piqtype =
  Piqirun_ext.find_piqtype "api/graph-rel-params"
  
let _cpl_rel_params_piqtype = Piqirun_ext.find_piqtype "api/cpl-rel-params"
  
let _node_piqtype = Piqirun_ext.find_piqtype "api/node"
  
let _full_node_piqtype = Piqirun_ext.find_piqtype "api/full-node"
  
let _edge_piqtype = Piqirun_ext.find_piqtype "api/edge"
  
let _graph_piqtype = Piqirun_ext.find_piqtype "api/graph"
  
let _full_graph_piqtype = Piqirun_ext.find_piqtype "api/full-graph"
  
let _all_persons_params_piqtype =
  Piqirun_ext.find_piqtype "api/all-persons-params"
  
let _all_families_params_piqtype =
  Piqirun_ext.find_piqtype "api/all-families-params"
  
let _warning_already_defined_piqtype =
  Piqirun_ext.find_piqtype "api/warning-already-defined"
  
let _warning_own_ancestor_piqtype =
  Piqirun_ext.find_piqtype "api/warning-own-ancestor"
  
let _warning_bad_sex_of_married_person_piqtype =
  Piqirun_ext.find_piqtype "api/warning-bad-sex-of-married-person"
  
let _warning_birth_after_death_piqtype =
  Piqirun_ext.find_piqtype "api/warning-birth-after-death"
  
let _warning_incoherent_sex_piqtype =
  Piqirun_ext.find_piqtype "api/warning-incoherent-sex"
  
let _warning_changed_order_of_children_piqtype =
  Piqirun_ext.find_piqtype "api/warning-changed-order-of-children"
  
let _warning_changed_order_of_marriages_piqtype =
  Piqirun_ext.find_piqtype "api/warning-changed-order-of-marriages"
  
let _warning_children_not_in_order_piqtype =
  Piqirun_ext.find_piqtype "api/warning-children-not-in-order"
  
let _warning_dead_too_early_to_be_father_piqtype =
  Piqirun_ext.find_piqtype "api/warning-dead-too-early-to-be-father"
  
let _warning_incoherent_ancestor_date_piqtype =
  Piqirun_ext.find_piqtype "api/warning-incoherent-ancestor-date"
  
let _warning_marriage_date_after_death_piqtype =
  Piqirun_ext.find_piqtype "api/warning-marriage-date-after-death"
  
let _warning_marriage_date_before_birth_piqtype =
  Piqirun_ext.find_piqtype "api/warning-marriage-date-before-birth"
  
let _warning_mother_dead_before_child_birth_piqtype =
  Piqirun_ext.find_piqtype "api/warning-mother-dead-before-child-birth"
  
let _warning_parent_born_after_child_piqtype =
  Piqirun_ext.find_piqtype "api/warning-parent-born-after-child"
  
let _warning_parent_too_young_piqtype =
  Piqirun_ext.find_piqtype "api/warning-parent-too-young"
  
let _warning_title_dates_error_piqtype =
  Piqirun_ext.find_piqtype "api/warning-title-dates-error"
  
let _warning_undefined_sex_piqtype =
  Piqirun_ext.find_piqtype "api/warning-undefined-sex"
  
let _warning_young_for_marriage_piqtype =
  Piqirun_ext.find_piqtype "api/warning-young-for-marriage"
  
let _warning_parent_too_old_piqtype =
  Piqirun_ext.find_piqtype "api/warning-parent-too-old"
  
let _warning_close_children_piqtype =
  Piqirun_ext.find_piqtype "api/warning-close-children"
  
let _warning_big_age_between_spouses_piqtype =
  Piqirun_ext.find_piqtype "api/warning-big-age-between-spouses"
  
let _warning_dead_old_piqtype =
  Piqirun_ext.find_piqtype "api/warning-dead-old"
  
let _warning_old_individual_piqtype =
  Piqirun_ext.find_piqtype "api/warning-old-individual"
  
let _warning_witness_date_after_death_piqtype =
  Piqirun_ext.find_piqtype "api/warning-witness-date-after-death"
  
let _warning_witness_date_before_birth_piqtype =
  Piqirun_ext.find_piqtype "api/warning-witness-date-before-birth"
  
let _warning_changed_order_of_family_events_piqtype =
  Piqirun_ext.find_piqtype "api/warning-changed-order-of-family-events"
  
let _warning_changed_order_of_person_events_piqtype =
  Piqirun_ext.find_piqtype "api/warning-changed-order-of-person-events"
  
let _warning_fevent_order_piqtype =
  Piqirun_ext.find_piqtype "api/warning-fevent-order"
  
let _warning_fwitness_event_after_death_piqtype =
  Piqirun_ext.find_piqtype "api/warning-fwitness-event-after-death"
  
let _warning_fwitness_event_before_birth_piqtype =
  Piqirun_ext.find_piqtype "api/warning-fwitness-event-before-birth"
  
let _warning_pevent_order_piqtype =
  Piqirun_ext.find_piqtype "api/warning-pevent-order"
  
let _warning_pwitness_event_after_death_piqtype =
  Piqirun_ext.find_piqtype "api/warning-pwitness-event-after-death"
  
let _warning_pwitness_event_before_birth_piqtype =
  Piqirun_ext.find_piqtype "api/warning-pwitness-event-before-birth"
  
let _base_warnings_piqtype = Piqirun_ext.find_piqtype "api/base-warnings"
  
let _filter_date_piqtype = Piqirun_ext.find_piqtype "api/filter-date"
  
let _filter_date_range_piqtype =
  Piqirun_ext.find_piqtype "api/filter-date-range"
  
let _filters_piqtype = Piqirun_ext.find_piqtype "api/filters"
  
let _modification_status_piqtype =
  Piqirun_ext.find_piqtype "api/modification-status"
  
let _notification_birthday_params_piqtype =
  Piqirun_ext.find_piqtype "api/notification-birthday-params"
  
let _notification_birthday_piqtype =
  Piqirun_ext.find_piqtype "api/notification-birthday"
  
let _person_start_piqtype = Piqirun_ext.find_piqtype "api/person-start"
  
let _synchro_params_piqtype = Piqirun_ext.find_piqtype "api/synchro-params"
  
let _correspondance_family_piqtype =
  Piqirun_ext.find_piqtype "api/correspondance-family"
  
let _correspondance_piqtype = Piqirun_ext.find_piqtype "api/correspondance"
  
let _correspondance_list_piqtype =
  Piqirun_ext.find_piqtype "api/correspondance-list"
  
let _sex_piqtype = Piqirun_ext.find_piqtype "api/sex"
  
let _death_type_piqtype = Piqirun_ext.find_piqtype "api/death-type"
  
let _marriage_type_piqtype = Piqirun_ext.find_piqtype "api/marriage-type"
  
let _divorce_type_piqtype = Piqirun_ext.find_piqtype "api/divorce-type"
  
let _relation_parent_type_piqtype =
  Piqirun_ext.find_piqtype "api/relation-parent-type"
  
let _title_type_piqtype = Piqirun_ext.find_piqtype "api/title-type"
  
let _search_type_piqtype = Piqirun_ext.find_piqtype "api/search-type"
  
let _relation_type_piqtype = Piqirun_ext.find_piqtype "api/relation-type"
  
let _notif_birthday_params_piqtype =
  Piqirun_ext.find_piqtype "api/notif-birthday-params"
  
let parse_int64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_int64 buf
  
let parse_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_int32 buf
  
let parse_protobuf_int64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_protobuf_int64 buf
  
let parse_string ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_string buf
  
let parse_protobuf_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_protobuf_int32 buf
  
let parse_bool ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_bool buf
  
let parse_infos_base ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _infos_base_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_infos_base buf
  
let parse_reference_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _reference_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_reference_person buf
  
let parse_list_reference_persons ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _list_reference_persons_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_reference_persons buf
  
let parse_relation_parent ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_relation_parent buf
  
let parse_title ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_title buf
  
let parse_spouse ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _spouse_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_spouse buf
  
let parse_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_person buf
  
let parse_full_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_full_person buf
  
let parse_full_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_full_family buf
  
let parse_internal_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _internal_int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_internal_int32 buf
  
let parse_list_persons ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_persons_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_list_persons buf
  
let parse_list_full_persons ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _list_full_persons_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_full_persons buf
  
let parse_list_full_families ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _list_full_families_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_full_families buf
  
let parse_search_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_search_params buf
  
let parse_image ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_image buf
  
let parse_full_image ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_image_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_full_image buf
  
let parse_list_images ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_images_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_list_images buf
  
let parse_list_full_images ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _list_full_images_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_full_images buf
  
let parse_pers_img ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pers_img_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_pers_img buf
  
let parse_list_pers_img ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _list_pers_img_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_list_pers_img buf
  
let parse_index ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_index buf
  
let parse_image_address ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _image_address_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_image_address buf
  
let parse_close_persons_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _close_persons_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_close_persons_params buf
  
let parse_person_relation ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _person_relation_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_person_relation buf
  
let parse_full_person_relation ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _full_person_relation_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_full_person_relation buf
  
let parse_list_person_relation ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _list_person_relation_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_person_relation buf
  
let parse_list_full_person_relation ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _list_full_person_relation_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_list_full_person_relation buf
  
let parse_anniversary_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _anniversary_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_anniversary_params buf
  
let parse_graph_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_graph_params buf
  
let parse_graph_rel_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _graph_rel_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_graph_rel_params buf
  
let parse_cpl_rel_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _cpl_rel_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_cpl_rel_params buf
  
let parse_node ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_node buf
  
let parse_full_node ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_node_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_full_node buf
  
let parse_edge ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_edge buf
  
let parse_graph ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_graph buf
  
let parse_full_graph ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _full_graph_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_full_graph buf
  
let parse_all_persons_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _all_persons_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_all_persons_params buf
  
let parse_all_families_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _all_families_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_all_families_params buf
  
let parse_warning_already_defined ?opts x (format : Piqirun_ext.input_format)
                                  =
  let x_pb =
    Piqirun_ext.convert _warning_already_defined_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_already_defined buf
  
let parse_warning_own_ancestor ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_own_ancestor_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_own_ancestor buf
  
let parse_warning_bad_sex_of_married_person ?opts x
                                            (format : Piqirun_ext.
                                             input_format)
                                            =
  let x_pb =
    Piqirun_ext.convert _warning_bad_sex_of_married_person_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_bad_sex_of_married_person buf
  
let parse_warning_birth_after_death ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_birth_after_death_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_birth_after_death buf
  
let parse_warning_incoherent_sex ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _warning_incoherent_sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_incoherent_sex buf
  
let parse_warning_changed_order_of_children ?opts x
                                            (format : Piqirun_ext.
                                             input_format)
                                            =
  let x_pb =
    Piqirun_ext.convert _warning_changed_order_of_children_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_changed_order_of_children buf
  
let parse_warning_changed_order_of_marriages ?opts x
                                             (format : Piqirun_ext.
                                              input_format)
                                             =
  let x_pb =
    Piqirun_ext.convert _warning_changed_order_of_marriages_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_changed_order_of_marriages buf
  
let parse_warning_children_not_in_order ?opts x
                                        (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_children_not_in_order_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_children_not_in_order buf
  
let parse_warning_dead_too_early_to_be_father ?opts x
                                              (format : Piqirun_ext.
                                               input_format)
                                              =
  let x_pb =
    Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_dead_too_early_to_be_father buf
  
let parse_warning_incoherent_ancestor_date ?opts x
                                           (format : Piqirun_ext.
                                            input_format)
                                           =
  let x_pb =
    Piqirun_ext.convert _warning_incoherent_ancestor_date_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_incoherent_ancestor_date buf
  
let parse_warning_marriage_date_after_death ?opts x
                                            (format : Piqirun_ext.
                                             input_format)
                                            =
  let x_pb =
    Piqirun_ext.convert _warning_marriage_date_after_death_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_marriage_date_after_death buf
  
let parse_warning_marriage_date_before_birth ?opts x
                                             (format : Piqirun_ext.
                                              input_format)
                                             =
  let x_pb =
    Piqirun_ext.convert _warning_marriage_date_before_birth_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_marriage_date_before_birth buf
  
let parse_warning_mother_dead_before_child_birth ?opts x
                                                 (format : Piqirun_ext.
                                                  input_format)
                                                 =
  let x_pb =
    Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqtype
      format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_mother_dead_before_child_birth buf
  
let parse_warning_parent_born_after_child ?opts x
                                          (format : Piqirun_ext.input_format)
                                          =
  let x_pb =
    Piqirun_ext.convert _warning_parent_born_after_child_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_parent_born_after_child buf
  
let parse_warning_parent_too_young ?opts x
                                   (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_parent_too_young_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_parent_too_young buf
  
let parse_warning_title_dates_error ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_title_dates_error_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_title_dates_error buf
  
let parse_warning_undefined_sex ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_undefined_sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_undefined_sex buf
  
let parse_warning_young_for_marriage ?opts x
                                     (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_young_for_marriage_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_young_for_marriage buf
  
let parse_warning_parent_too_old ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _warning_parent_too_old_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_parent_too_old buf
  
let parse_warning_close_children ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _warning_close_children_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_close_children buf
  
let parse_warning_big_age_between_spouses ?opts x
                                          (format : Piqirun_ext.input_format)
                                          =
  let x_pb =
    Piqirun_ext.convert _warning_big_age_between_spouses_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_big_age_between_spouses buf
  
let parse_warning_dead_old ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_dead_old_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_dead_old buf
  
let parse_warning_old_individual ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _warning_old_individual_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_old_individual buf
  
let parse_warning_witness_date_after_death ?opts x
                                           (format : Piqirun_ext.
                                            input_format)
                                           =
  let x_pb =
    Piqirun_ext.convert _warning_witness_date_after_death_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_witness_date_after_death buf
  
let parse_warning_witness_date_before_birth ?opts x
                                            (format : Piqirun_ext.
                                             input_format)
                                            =
  let x_pb =
    Piqirun_ext.convert _warning_witness_date_before_birth_piqtype format `pb
      x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_witness_date_before_birth buf
  
let parse_warning_changed_order_of_family_events ?opts x
                                                 (format : Piqirun_ext.
                                                  input_format)
                                                 =
  let x_pb =
    Piqirun_ext.convert _warning_changed_order_of_family_events_piqtype
      format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_changed_order_of_family_events buf
  
let parse_warning_changed_order_of_person_events ?opts x
                                                 (format : Piqirun_ext.
                                                  input_format)
                                                 =
  let x_pb =
    Piqirun_ext.convert _warning_changed_order_of_person_events_piqtype
      format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_changed_order_of_person_events buf
  
let parse_warning_fevent_order ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_fevent_order_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_fevent_order buf
  
let parse_warning_fwitness_event_after_death ?opts x
                                             (format : Piqirun_ext.
                                              input_format)
                                             =
  let x_pb =
    Piqirun_ext.convert _warning_fwitness_event_after_death_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_fwitness_event_after_death buf
  
let parse_warning_fwitness_event_before_birth ?opts x
                                              (format : Piqirun_ext.
                                               input_format)
                                              =
  let x_pb =
    Piqirun_ext.convert _warning_fwitness_event_before_birth_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_fwitness_event_before_birth buf
  
let parse_warning_pevent_order ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _warning_pevent_order_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_pevent_order buf
  
let parse_warning_pwitness_event_after_death ?opts x
                                             (format : Piqirun_ext.
                                              input_format)
                                             =
  let x_pb =
    Piqirun_ext.convert _warning_pwitness_event_after_death_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_pwitness_event_after_death buf
  
let parse_warning_pwitness_event_before_birth ?opts x
                                              (format : Piqirun_ext.
                                               input_format)
                                              =
  let x_pb =
    Piqirun_ext.convert _warning_pwitness_event_before_birth_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_warning_pwitness_event_before_birth buf
  
let parse_base_warnings ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _base_warnings_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_base_warnings buf
  
let parse_filter_date ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filter_date_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_filter_date buf
  
let parse_filter_date_range ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _filter_date_range_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_filter_date_range buf
  
let parse_filters ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _filters_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_filters buf
  
let parse_modification_status ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _modification_status_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_modification_status buf
  
let parse_notification_birthday_params ?opts x
                                       (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _notification_birthday_params_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_notification_birthday_params buf
  
let parse_notification_birthday ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _notification_birthday_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_notification_birthday buf
  
let parse_person_start ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_start_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_person_start buf
  
let parse_synchro_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _synchro_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_synchro_params buf
  
let parse_correspondance_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _correspondance_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_correspondance_family buf
  
let parse_correspondance ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _correspondance_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_correspondance buf
  
let parse_correspondance_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _correspondance_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_correspondance_list buf
  
let parse_sex ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_sex buf
  
let parse_death_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_death_type buf
  
let parse_marriage_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_marriage_type buf
  
let parse_divorce_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_divorce_type buf
  
let parse_relation_parent_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_relation_parent_type buf
  
let parse_title_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_title_type buf
  
let parse_search_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _search_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_search_type buf
  
let parse_relation_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_piqi.parse_relation_type buf
  
let parse_notif_birthday_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _notif_birthday_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_piqi.parse_notif_birthday_params buf
  
let gen_int64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_int64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int64_piqtype `pb format x_pb ?opts
  
let gen_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int32_piqtype `pb format x_pb ?opts
  
let gen_protobuf_int64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_protobuf_int64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int64_piqtype `pb format x_pb ?opts
  
let gen_string ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_string x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
  
let gen_protobuf_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_protobuf_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int32_piqtype `pb format x_pb ?opts
  
let gen_bool ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_bool x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
  
let gen_infos_base ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_infos_base x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _infos_base_piqtype `pb format x_pb ?opts
  
let gen_reference_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_reference_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _reference_person_piqtype `pb format x_pb ?opts
  
let gen_list_reference_persons ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_reference_persons x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _list_reference_persons_piqtype `pb format x_pb ?opts
  
let gen_relation_parent ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_piqtype `pb format x_pb ?opts
  
let gen_title ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _title_piqtype `pb format x_pb ?opts
  
let gen_spouse ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_spouse x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _spouse_piqtype `pb format x_pb ?opts
  
let gen_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_piqtype `pb format x_pb ?opts
  
let gen_full_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_person_piqtype `pb format x_pb ?opts
  
let gen_full_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_family_piqtype `pb format x_pb ?opts
  
let gen_internal_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_internal_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _internal_int32_piqtype `pb format x_pb ?opts
  
let gen_list_persons ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_persons x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_persons_piqtype `pb format x_pb ?opts
  
let gen_list_full_persons ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_persons x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_full_persons_piqtype `pb format x_pb ?opts
  
let gen_list_full_families ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_families x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_full_families_piqtype `pb format x_pb ?opts
  
let gen_search_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _search_params_piqtype `pb format x_pb ?opts
  
let gen_image ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _image_piqtype `pb format x_pb ?opts
  
let gen_full_image ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_image x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_image_piqtype `pb format x_pb ?opts
  
let gen_list_images ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_images x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_images_piqtype `pb format x_pb ?opts
  
let gen_list_full_images ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_images x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_full_images_piqtype `pb format x_pb ?opts
  
let gen_pers_img ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_pers_img x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _pers_img_piqtype `pb format x_pb ?opts
  
let gen_list_pers_img ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_pers_img x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_pers_img_piqtype `pb format x_pb ?opts
  
let gen_index ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_index x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _index_piqtype `pb format x_pb ?opts
  
let gen_image_address ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_image_address x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _image_address_piqtype `pb format x_pb ?opts
  
let gen_close_persons_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_close_persons_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _close_persons_params_piqtype `pb format x_pb ?opts
  
let gen_person_relation ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person_relation x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_relation_piqtype `pb format x_pb ?opts
  
let gen_full_person_relation ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_person_relation x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_person_relation_piqtype `pb format x_pb ?opts
  
let gen_list_person_relation ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_person_relation x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _list_person_relation_piqtype `pb format x_pb ?opts
  
let gen_list_full_person_relation ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_list_full_person_relation x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _list_full_person_relation_piqtype `pb format x_pb
      ?opts
  
let gen_anniversary_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_anniversary_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _anniversary_params_piqtype `pb format x_pb ?opts
  
let gen_graph_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _graph_params_piqtype `pb format x_pb ?opts
  
let gen_graph_rel_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph_rel_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _graph_rel_params_piqtype `pb format x_pb ?opts
  
let gen_cpl_rel_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_cpl_rel_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _cpl_rel_params_piqtype `pb format x_pb ?opts
  
let gen_node ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _node_piqtype `pb format x_pb ?opts
  
let gen_full_node ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_node x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_node_piqtype `pb format x_pb ?opts
  
let gen_edge ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _edge_piqtype `pb format x_pb ?opts
  
let gen_graph ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_graph x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _graph_piqtype `pb format x_pb ?opts
  
let gen_full_graph ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_full_graph x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _full_graph_piqtype `pb format x_pb ?opts
  
let gen_all_persons_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_persons_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _all_persons_params_piqtype `pb format x_pb ?opts
  
let gen_all_families_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_all_families_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _all_families_params_piqtype `pb format x_pb ?opts
  
let gen_warning_already_defined ?opts x (format : Piqirun_ext.output_format)
                                =
  let buf = Api_piqi.gen_warning_already_defined x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_already_defined_piqtype `pb format x_pb
      ?opts
  
let gen_warning_own_ancestor ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_own_ancestor x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _warning_own_ancestor_piqtype `pb format x_pb ?opts
  
let gen_warning_bad_sex_of_married_person ?opts x
                                          (format : Piqirun_ext.
                                           output_format)
                                          =
  let buf = Api_piqi.gen_warning_bad_sex_of_married_person x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_bad_sex_of_married_person_piqtype `pb format
      x_pb ?opts
  
let gen_warning_birth_after_death ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_birth_after_death x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_birth_after_death_piqtype `pb format x_pb
      ?opts
  
let gen_warning_incoherent_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_incoherent_sex x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_incoherent_sex_piqtype `pb format x_pb ?opts
  
let gen_warning_changed_order_of_children ?opts x
                                          (format : Piqirun_ext.
                                           output_format)
                                          =
  let buf = Api_piqi.gen_warning_changed_order_of_children x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_changed_order_of_children_piqtype `pb format
      x_pb ?opts
  
let gen_warning_changed_order_of_marriages ?opts x
                                           (format : Piqirun_ext.
                                            output_format)
                                           =
  let buf = Api_piqi.gen_warning_changed_order_of_marriages x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_changed_order_of_marriages_piqtype `pb
      format x_pb ?opts
  
let gen_warning_children_not_in_order ?opts x
                                      (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_children_not_in_order x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_children_not_in_order_piqtype `pb format
      x_pb ?opts
  
let gen_warning_dead_too_early_to_be_father ?opts x
                                            (format : Piqirun_ext.
                                             output_format)
                                            =
  let buf = Api_piqi.gen_warning_dead_too_early_to_be_father x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_dead_too_early_to_be_father_piqtype `pb
      format x_pb ?opts
  
let gen_warning_incoherent_ancestor_date ?opts x
                                         (format : Piqirun_ext.output_format)
                                         =
  let buf = Api_piqi.gen_warning_incoherent_ancestor_date x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_incoherent_ancestor_date_piqtype `pb format
      x_pb ?opts
  
let gen_warning_marriage_date_after_death ?opts x
                                          (format : Piqirun_ext.
                                           output_format)
                                          =
  let buf = Api_piqi.gen_warning_marriage_date_after_death x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_marriage_date_after_death_piqtype `pb format
      x_pb ?opts
  
let gen_warning_marriage_date_before_birth ?opts x
                                           (format : Piqirun_ext.
                                            output_format)
                                           =
  let buf = Api_piqi.gen_warning_marriage_date_before_birth x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_marriage_date_before_birth_piqtype `pb
      format x_pb ?opts
  
let gen_warning_mother_dead_before_child_birth ?opts x
                                               (format : Piqirun_ext.
                                                output_format)
                                               =
  let buf = Api_piqi.gen_warning_mother_dead_before_child_birth x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_mother_dead_before_child_birth_piqtype `pb
      format x_pb ?opts
  
let gen_warning_parent_born_after_child ?opts x
                                        (format : Piqirun_ext.output_format)
                                        =
  let buf = Api_piqi.gen_warning_parent_born_after_child x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_parent_born_after_child_piqtype `pb format
      x_pb ?opts
  
let gen_warning_parent_too_young ?opts x (format : Piqirun_ext.output_format)
                                 =
  let buf = Api_piqi.gen_warning_parent_too_young x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_parent_too_young_piqtype `pb format x_pb
      ?opts
  
let gen_warning_title_dates_error ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_title_dates_error x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_title_dates_error_piqtype `pb format x_pb
      ?opts
  
let gen_warning_undefined_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_undefined_sex x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _warning_undefined_sex_piqtype `pb format x_pb ?opts
  
let gen_warning_young_for_marriage ?opts x
                                   (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_young_for_marriage x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_young_for_marriage_piqtype `pb format x_pb
      ?opts
  
let gen_warning_parent_too_old ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_parent_too_old x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_parent_too_old_piqtype `pb format x_pb ?opts
  
let gen_warning_close_children ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_close_children x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_close_children_piqtype `pb format x_pb ?opts
  
let gen_warning_big_age_between_spouses ?opts x
                                        (format : Piqirun_ext.output_format)
                                        =
  let buf = Api_piqi.gen_warning_big_age_between_spouses x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_big_age_between_spouses_piqtype `pb format
      x_pb ?opts
  
let gen_warning_dead_old ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_dead_old x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _warning_dead_old_piqtype `pb format x_pb ?opts
  
let gen_warning_old_individual ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_old_individual x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_old_individual_piqtype `pb format x_pb ?opts
  
let gen_warning_witness_date_after_death ?opts x
                                         (format : Piqirun_ext.output_format)
                                         =
  let buf = Api_piqi.gen_warning_witness_date_after_death x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_witness_date_after_death_piqtype `pb format
      x_pb ?opts
  
let gen_warning_witness_date_before_birth ?opts x
                                          (format : Piqirun_ext.
                                           output_format)
                                          =
  let buf = Api_piqi.gen_warning_witness_date_before_birth x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_witness_date_before_birth_piqtype `pb format
      x_pb ?opts
  
let gen_warning_changed_order_of_family_events ?opts x
                                               (format : Piqirun_ext.
                                                output_format)
                                               =
  let buf = Api_piqi.gen_warning_changed_order_of_family_events x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_changed_order_of_family_events_piqtype `pb
      format x_pb ?opts
  
let gen_warning_changed_order_of_person_events ?opts x
                                               (format : Piqirun_ext.
                                                output_format)
                                               =
  let buf = Api_piqi.gen_warning_changed_order_of_person_events x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_changed_order_of_person_events_piqtype `pb
      format x_pb ?opts
  
let gen_warning_fevent_order ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_fevent_order x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _warning_fevent_order_piqtype `pb format x_pb ?opts
  
let gen_warning_fwitness_event_after_death ?opts x
                                           (format : Piqirun_ext.
                                            output_format)
                                           =
  let buf = Api_piqi.gen_warning_fwitness_event_after_death x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_fwitness_event_after_death_piqtype `pb
      format x_pb ?opts
  
let gen_warning_fwitness_event_before_birth ?opts x
                                            (format : Piqirun_ext.
                                             output_format)
                                            =
  let buf = Api_piqi.gen_warning_fwitness_event_before_birth x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_fwitness_event_before_birth_piqtype `pb
      format x_pb ?opts
  
let gen_warning_pevent_order ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_warning_pevent_order x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _warning_pevent_order_piqtype `pb format x_pb ?opts
  
let gen_warning_pwitness_event_after_death ?opts x
                                           (format : Piqirun_ext.
                                            output_format)
                                           =
  let buf = Api_piqi.gen_warning_pwitness_event_after_death x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_pwitness_event_after_death_piqtype `pb
      format x_pb ?opts
  
let gen_warning_pwitness_event_before_birth ?opts x
                                            (format : Piqirun_ext.
                                             output_format)
                                            =
  let buf = Api_piqi.gen_warning_pwitness_event_before_birth x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _warning_pwitness_event_before_birth_piqtype `pb
      format x_pb ?opts
  
let gen_base_warnings ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_base_warnings x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _base_warnings_piqtype `pb format x_pb ?opts
  
let gen_filter_date ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _filter_date_piqtype `pb format x_pb ?opts
  
let gen_filter_date_range ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filter_date_range x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _filter_date_range_piqtype `pb format x_pb ?opts
  
let gen_filters ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_filters x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _filters_piqtype `pb format x_pb ?opts
  
let gen_modification_status ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _modification_status_piqtype `pb format x_pb ?opts
  
let gen_notification_birthday_params ?opts x
                                     (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notification_birthday_params x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _notification_birthday_params_piqtype `pb format x_pb
      ?opts
  
let gen_notification_birthday ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notification_birthday x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _notification_birthday_piqtype `pb format x_pb ?opts
  
let gen_person_start ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_person_start x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_start_piqtype `pb format x_pb ?opts
  
let gen_synchro_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_synchro_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _synchro_params_piqtype `pb format x_pb ?opts
  
let gen_correspondance_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _correspondance_family_piqtype `pb format x_pb ?opts
  
let gen_correspondance ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _correspondance_piqtype `pb format x_pb ?opts
  
let gen_correspondance_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_correspondance_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _correspondance_list_piqtype `pb format x_pb ?opts
  
let gen_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sex_piqtype `pb format x_pb ?opts
  
let gen_death_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _death_type_piqtype `pb format x_pb ?opts
  
let gen_marriage_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _marriage_type_piqtype `pb format x_pb ?opts
  
let gen_divorce_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _divorce_type_piqtype `pb format x_pb ?opts
  
let gen_relation_parent_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_type_piqtype `pb format x_pb ?opts
  
let gen_title_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _title_type_piqtype `pb format x_pb ?opts
  
let gen_search_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_search_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _search_type_piqtype `pb format x_pb ?opts
  
let gen_relation_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_type_piqtype `pb format x_pb ?opts
  
let gen_notif_birthday_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_piqi.gen_notif_birthday_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _notif_birthday_params_piqtype `pb format x_pb ?opts
  
let print_int64 x = Pervasives.print_endline (gen_int64 x `piq)
  
let prerr_int64 x = Pervasives.prerr_endline (gen_int64 x `piq)
  
let print_int32 x = Pervasives.print_endline (gen_int32 x `piq)
  
let prerr_int32 x = Pervasives.prerr_endline (gen_int32 x `piq)
  
let print_protobuf_int64 x =
  Pervasives.print_endline (gen_protobuf_int64 x `piq)
  
let prerr_protobuf_int64 x =
  Pervasives.prerr_endline (gen_protobuf_int64 x `piq)
  
let print_string x = Pervasives.print_endline (gen_string x `piq)
  
let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
  
let print_protobuf_int32 x =
  Pervasives.print_endline (gen_protobuf_int32 x `piq)
  
let prerr_protobuf_int32 x =
  Pervasives.prerr_endline (gen_protobuf_int32 x `piq)
  
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
  
let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
  
let print_infos_base x = Pervasives.print_endline (gen_infos_base x `piq)
  
let prerr_infos_base x = Pervasives.prerr_endline (gen_infos_base x `piq)
  
let print_reference_person x =
  Pervasives.print_endline (gen_reference_person x `piq)
  
let prerr_reference_person x =
  Pervasives.prerr_endline (gen_reference_person x `piq)
  
let print_list_reference_persons x =
  Pervasives.print_endline (gen_list_reference_persons x `piq)
  
let prerr_list_reference_persons x =
  Pervasives.prerr_endline (gen_list_reference_persons x `piq)
  
let print_relation_parent x =
  Pervasives.print_endline (gen_relation_parent x `piq)
  
let prerr_relation_parent x =
  Pervasives.prerr_endline (gen_relation_parent x `piq)
  
let print_title x = Pervasives.print_endline (gen_title x `piq)
  
let prerr_title x = Pervasives.prerr_endline (gen_title x `piq)
  
let print_spouse x = Pervasives.print_endline (gen_spouse x `piq)
  
let prerr_spouse x = Pervasives.prerr_endline (gen_spouse x `piq)
  
let print_person x = Pervasives.print_endline (gen_person x `piq)
  
let prerr_person x = Pervasives.prerr_endline (gen_person x `piq)
  
let print_full_person x = Pervasives.print_endline (gen_full_person x `piq)
  
let prerr_full_person x = Pervasives.prerr_endline (gen_full_person x `piq)
  
let print_full_family x = Pervasives.print_endline (gen_full_family x `piq)
  
let prerr_full_family x = Pervasives.prerr_endline (gen_full_family x `piq)
  
let print_internal_int32 x =
  Pervasives.print_endline (gen_internal_int32 x `piq)
  
let prerr_internal_int32 x =
  Pervasives.prerr_endline (gen_internal_int32 x `piq)
  
let print_list_persons x = Pervasives.print_endline (gen_list_persons x `piq)
  
let prerr_list_persons x = Pervasives.prerr_endline (gen_list_persons x `piq)
  
let print_list_full_persons x =
  Pervasives.print_endline (gen_list_full_persons x `piq)
  
let prerr_list_full_persons x =
  Pervasives.prerr_endline (gen_list_full_persons x `piq)
  
let print_list_full_families x =
  Pervasives.print_endline (gen_list_full_families x `piq)
  
let prerr_list_full_families x =
  Pervasives.prerr_endline (gen_list_full_families x `piq)
  
let print_search_params x =
  Pervasives.print_endline (gen_search_params x `piq)
  
let prerr_search_params x =
  Pervasives.prerr_endline (gen_search_params x `piq)
  
let print_image x = Pervasives.print_endline (gen_image x `piq)
  
let prerr_image x = Pervasives.prerr_endline (gen_image x `piq)
  
let print_full_image x = Pervasives.print_endline (gen_full_image x `piq)
  
let prerr_full_image x = Pervasives.prerr_endline (gen_full_image x `piq)
  
let print_list_images x = Pervasives.print_endline (gen_list_images x `piq)
  
let prerr_list_images x = Pervasives.prerr_endline (gen_list_images x `piq)
  
let print_list_full_images x =
  Pervasives.print_endline (gen_list_full_images x `piq)
  
let prerr_list_full_images x =
  Pervasives.prerr_endline (gen_list_full_images x `piq)
  
let print_pers_img x = Pervasives.print_endline (gen_pers_img x `piq)
  
let prerr_pers_img x = Pervasives.prerr_endline (gen_pers_img x `piq)
  
let print_list_pers_img x =
  Pervasives.print_endline (gen_list_pers_img x `piq)
  
let prerr_list_pers_img x =
  Pervasives.prerr_endline (gen_list_pers_img x `piq)
  
let print_index x = Pervasives.print_endline (gen_index x `piq)
  
let prerr_index x = Pervasives.prerr_endline (gen_index x `piq)
  
let print_image_address x =
  Pervasives.print_endline (gen_image_address x `piq)
  
let prerr_image_address x =
  Pervasives.prerr_endline (gen_image_address x `piq)
  
let print_close_persons_params x =
  Pervasives.print_endline (gen_close_persons_params x `piq)
  
let prerr_close_persons_params x =
  Pervasives.prerr_endline (gen_close_persons_params x `piq)
  
let print_person_relation x =
  Pervasives.print_endline (gen_person_relation x `piq)
  
let prerr_person_relation x =
  Pervasives.prerr_endline (gen_person_relation x `piq)
  
let print_full_person_relation x =
  Pervasives.print_endline (gen_full_person_relation x `piq)
  
let prerr_full_person_relation x =
  Pervasives.prerr_endline (gen_full_person_relation x `piq)
  
let print_list_person_relation x =
  Pervasives.print_endline (gen_list_person_relation x `piq)
  
let prerr_list_person_relation x =
  Pervasives.prerr_endline (gen_list_person_relation x `piq)
  
let print_list_full_person_relation x =
  Pervasives.print_endline (gen_list_full_person_relation x `piq)
  
let prerr_list_full_person_relation x =
  Pervasives.prerr_endline (gen_list_full_person_relation x `piq)
  
let print_anniversary_params x =
  Pervasives.print_endline (gen_anniversary_params x `piq)
  
let prerr_anniversary_params x =
  Pervasives.prerr_endline (gen_anniversary_params x `piq)
  
let print_graph_params x = Pervasives.print_endline (gen_graph_params x `piq)
  
let prerr_graph_params x = Pervasives.prerr_endline (gen_graph_params x `piq)
  
let print_graph_rel_params x =
  Pervasives.print_endline (gen_graph_rel_params x `piq)
  
let prerr_graph_rel_params x =
  Pervasives.prerr_endline (gen_graph_rel_params x `piq)
  
let print_cpl_rel_params x =
  Pervasives.print_endline (gen_cpl_rel_params x `piq)
  
let prerr_cpl_rel_params x =
  Pervasives.prerr_endline (gen_cpl_rel_params x `piq)
  
let print_node x = Pervasives.print_endline (gen_node x `piq)
  
let prerr_node x = Pervasives.prerr_endline (gen_node x `piq)
  
let print_full_node x = Pervasives.print_endline (gen_full_node x `piq)
  
let prerr_full_node x = Pervasives.prerr_endline (gen_full_node x `piq)
  
let print_edge x = Pervasives.print_endline (gen_edge x `piq)
  
let prerr_edge x = Pervasives.prerr_endline (gen_edge x `piq)
  
let print_graph x = Pervasives.print_endline (gen_graph x `piq)
  
let prerr_graph x = Pervasives.prerr_endline (gen_graph x `piq)
  
let print_full_graph x = Pervasives.print_endline (gen_full_graph x `piq)
  
let prerr_full_graph x = Pervasives.prerr_endline (gen_full_graph x `piq)
  
let print_all_persons_params x =
  Pervasives.print_endline (gen_all_persons_params x `piq)
  
let prerr_all_persons_params x =
  Pervasives.prerr_endline (gen_all_persons_params x `piq)
  
let print_all_families_params x =
  Pervasives.print_endline (gen_all_families_params x `piq)
  
let prerr_all_families_params x =
  Pervasives.prerr_endline (gen_all_families_params x `piq)
  
let print_warning_already_defined x =
  Pervasives.print_endline (gen_warning_already_defined x `piq)
  
let prerr_warning_already_defined x =
  Pervasives.prerr_endline (gen_warning_already_defined x `piq)
  
let print_warning_own_ancestor x =
  Pervasives.print_endline (gen_warning_own_ancestor x `piq)
  
let prerr_warning_own_ancestor x =
  Pervasives.prerr_endline (gen_warning_own_ancestor x `piq)
  
let print_warning_bad_sex_of_married_person x =
  Pervasives.print_endline (gen_warning_bad_sex_of_married_person x `piq)
  
let prerr_warning_bad_sex_of_married_person x =
  Pervasives.prerr_endline (gen_warning_bad_sex_of_married_person x `piq)
  
let print_warning_birth_after_death x =
  Pervasives.print_endline (gen_warning_birth_after_death x `piq)
  
let prerr_warning_birth_after_death x =
  Pervasives.prerr_endline (gen_warning_birth_after_death x `piq)
  
let print_warning_incoherent_sex x =
  Pervasives.print_endline (gen_warning_incoherent_sex x `piq)
  
let prerr_warning_incoherent_sex x =
  Pervasives.prerr_endline (gen_warning_incoherent_sex x `piq)
  
let print_warning_changed_order_of_children x =
  Pervasives.print_endline (gen_warning_changed_order_of_children x `piq)
  
let prerr_warning_changed_order_of_children x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_children x `piq)
  
let print_warning_changed_order_of_marriages x =
  Pervasives.print_endline (gen_warning_changed_order_of_marriages x `piq)
  
let prerr_warning_changed_order_of_marriages x =
  Pervasives.prerr_endline (gen_warning_changed_order_of_marriages x `piq)
  
let print_warning_children_not_in_order x =
  Pervasives.print_endline (gen_warning_children_not_in_order x `piq)
  
let prerr_warning_children_not_in_order x =
  Pervasives.prerr_endline (gen_warning_children_not_in_order x `piq)
  
let print_warning_dead_too_early_to_be_father x =
  Pervasives.print_endline (gen_warning_dead_too_early_to_be_father x `piq)
  
let prerr_warning_dead_too_early_to_be_father x =
  Pervasives.prerr_endline (gen_warning_dead_too_early_to_be_father x `piq)
  
let print_warning_incoherent_ancestor_date x =
  Pervasives.print_endline (gen_warning_incoherent_ancestor_date x `piq)
  
let prerr_warning_incoherent_ancestor_date x =
  Pervasives.prerr_endline (gen_warning_incoherent_ancestor_date x `piq)
  
let print_warning_marriage_date_after_death x =
  Pervasives.print_endline (gen_warning_marriage_date_after_death x `piq)
  
let prerr_warning_marriage_date_after_death x =
  Pervasives.prerr_endline (gen_warning_marriage_date_after_death x `piq)
  
let print_warning_marriage_date_before_birth x =
  Pervasives.print_endline (gen_warning_marriage_date_before_birth x `piq)
  
let prerr_warning_marriage_date_before_birth x =
  Pervasives.prerr_endline (gen_warning_marriage_date_before_birth x `piq)
  
let print_warning_mother_dead_before_child_birth x =
  Pervasives.print_endline
    (gen_warning_mother_dead_before_child_birth x `piq)
  
let prerr_warning_mother_dead_before_child_birth x =
  Pervasives.prerr_endline
    (gen_warning_mother_dead_before_child_birth x `piq)
  
let print_warning_parent_born_after_child x =
  Pervasives.print_endline (gen_warning_parent_born_after_child x `piq)
  
let prerr_warning_parent_born_after_child x =
  Pervasives.prerr_endline (gen_warning_parent_born_after_child x `piq)
  
let print_warning_parent_too_young x =
  Pervasives.print_endline (gen_warning_parent_too_young x `piq)
  
let prerr_warning_parent_too_young x =
  Pervasives.prerr_endline (gen_warning_parent_too_young x `piq)
  
let print_warning_title_dates_error x =
  Pervasives.print_endline (gen_warning_title_dates_error x `piq)
  
let prerr_warning_title_dates_error x =
  Pervasives.prerr_endline (gen_warning_title_dates_error x `piq)
  
let print_warning_undefined_sex x =
  Pervasives.print_endline (gen_warning_undefined_sex x `piq)
  
let prerr_warning_undefined_sex x =
  Pervasives.prerr_endline (gen_warning_undefined_sex x `piq)
  
let print_warning_young_for_marriage x =
  Pervasives.print_endline (gen_warning_young_for_marriage x `piq)
  
let prerr_warning_young_for_marriage x =
  Pervasives.prerr_endline (gen_warning_young_for_marriage x `piq)
  
let print_warning_parent_too_old x =
  Pervasives.print_endline (gen_warning_parent_too_old x `piq)
  
let prerr_warning_parent_too_old x =
  Pervasives.prerr_endline (gen_warning_parent_too_old x `piq)
  
let print_warning_close_children x =
  Pervasives.print_endline (gen_warning_close_children x `piq)
  
let prerr_warning_close_children x =
  Pervasives.prerr_endline (gen_warning_close_children x `piq)
  
let print_warning_big_age_between_spouses x =
  Pervasives.print_endline (gen_warning_big_age_between_spouses x `piq)
  
let prerr_warning_big_age_between_spouses x =
  Pervasives.prerr_endline (gen_warning_big_age_between_spouses x `piq)
  
let print_warning_dead_old x =
  Pervasives.print_endline (gen_warning_dead_old x `piq)
  
let prerr_warning_dead_old x =
  Pervasives.prerr_endline (gen_warning_dead_old x `piq)
  
let print_warning_old_individual x =
  Pervasives.print_endline (gen_warning_old_individual x `piq)
  
let prerr_warning_old_individual x =
  Pervasives.prerr_endline (gen_warning_old_individual x `piq)
  
let print_warning_witness_date_after_death x =
  Pervasives.print_endline (gen_warning_witness_date_after_death x `piq)
  
let prerr_warning_witness_date_after_death x =
  Pervasives.prerr_endline (gen_warning_witness_date_after_death x `piq)
  
let print_warning_witness_date_before_birth x =
  Pervasives.print_endline (gen_warning_witness_date_before_birth x `piq)
  
let prerr_warning_witness_date_before_birth x =
  Pervasives.prerr_endline (gen_warning_witness_date_before_birth x `piq)
  
let print_warning_changed_order_of_family_events x =
  Pervasives.print_endline
    (gen_warning_changed_order_of_family_events x `piq)
  
let prerr_warning_changed_order_of_family_events x =
  Pervasives.prerr_endline
    (gen_warning_changed_order_of_family_events x `piq)
  
let print_warning_changed_order_of_person_events x =
  Pervasives.print_endline
    (gen_warning_changed_order_of_person_events x `piq)
  
let prerr_warning_changed_order_of_person_events x =
  Pervasives.prerr_endline
    (gen_warning_changed_order_of_person_events x `piq)
  
let print_warning_fevent_order x =
  Pervasives.print_endline (gen_warning_fevent_order x `piq)
  
let prerr_warning_fevent_order x =
  Pervasives.prerr_endline (gen_warning_fevent_order x `piq)
  
let print_warning_fwitness_event_after_death x =
  Pervasives.print_endline (gen_warning_fwitness_event_after_death x `piq)
  
let prerr_warning_fwitness_event_after_death x =
  Pervasives.prerr_endline (gen_warning_fwitness_event_after_death x `piq)
  
let print_warning_fwitness_event_before_birth x =
  Pervasives.print_endline (gen_warning_fwitness_event_before_birth x `piq)
  
let prerr_warning_fwitness_event_before_birth x =
  Pervasives.prerr_endline (gen_warning_fwitness_event_before_birth x `piq)
  
let print_warning_pevent_order x =
  Pervasives.print_endline (gen_warning_pevent_order x `piq)
  
let prerr_warning_pevent_order x =
  Pervasives.prerr_endline (gen_warning_pevent_order x `piq)
  
let print_warning_pwitness_event_after_death x =
  Pervasives.print_endline (gen_warning_pwitness_event_after_death x `piq)
  
let prerr_warning_pwitness_event_after_death x =
  Pervasives.prerr_endline (gen_warning_pwitness_event_after_death x `piq)
  
let print_warning_pwitness_event_before_birth x =
  Pervasives.print_endline (gen_warning_pwitness_event_before_birth x `piq)
  
let prerr_warning_pwitness_event_before_birth x =
  Pervasives.prerr_endline (gen_warning_pwitness_event_before_birth x `piq)
  
let print_base_warnings x =
  Pervasives.print_endline (gen_base_warnings x `piq)
  
let prerr_base_warnings x =
  Pervasives.prerr_endline (gen_base_warnings x `piq)
  
let print_filter_date x = Pervasives.print_endline (gen_filter_date x `piq)
  
let prerr_filter_date x = Pervasives.prerr_endline (gen_filter_date x `piq)
  
let print_filter_date_range x =
  Pervasives.print_endline (gen_filter_date_range x `piq)
  
let prerr_filter_date_range x =
  Pervasives.prerr_endline (gen_filter_date_range x `piq)
  
let print_filters x = Pervasives.print_endline (gen_filters x `piq)
  
let prerr_filters x = Pervasives.prerr_endline (gen_filters x `piq)
  
let print_modification_status x =
  Pervasives.print_endline (gen_modification_status x `piq)
  
let prerr_modification_status x =
  Pervasives.prerr_endline (gen_modification_status x `piq)
  
let print_notification_birthday_params x =
  Pervasives.print_endline (gen_notification_birthday_params x `piq)
  
let prerr_notification_birthday_params x =
  Pervasives.prerr_endline (gen_notification_birthday_params x `piq)
  
let print_notification_birthday x =
  Pervasives.print_endline (gen_notification_birthday x `piq)
  
let prerr_notification_birthday x =
  Pervasives.prerr_endline (gen_notification_birthday x `piq)
  
let print_person_start x = Pervasives.print_endline (gen_person_start x `piq)
  
let prerr_person_start x = Pervasives.prerr_endline (gen_person_start x `piq)
  
let print_synchro_params x =
  Pervasives.print_endline (gen_synchro_params x `piq)
  
let prerr_synchro_params x =
  Pervasives.prerr_endline (gen_synchro_params x `piq)
  
let print_correspondance_family x =
  Pervasives.print_endline (gen_correspondance_family x `piq)
  
let prerr_correspondance_family x =
  Pervasives.prerr_endline (gen_correspondance_family x `piq)
  
let print_correspondance x =
  Pervasives.print_endline (gen_correspondance x `piq)
  
let prerr_correspondance x =
  Pervasives.prerr_endline (gen_correspondance x `piq)
  
let print_correspondance_list x =
  Pervasives.print_endline (gen_correspondance_list x `piq)
  
let prerr_correspondance_list x =
  Pervasives.prerr_endline (gen_correspondance_list x `piq)
  
let print_sex x = Pervasives.print_endline (gen_sex x `piq)
  
let prerr_sex x = Pervasives.prerr_endline (gen_sex x `piq)
  
let print_death_type x = Pervasives.print_endline (gen_death_type x `piq)
  
let prerr_death_type x = Pervasives.prerr_endline (gen_death_type x `piq)
  
let print_marriage_type x =
  Pervasives.print_endline (gen_marriage_type x `piq)
  
let prerr_marriage_type x =
  Pervasives.prerr_endline (gen_marriage_type x `piq)
  
let print_divorce_type x = Pervasives.print_endline (gen_divorce_type x `piq)
  
let prerr_divorce_type x = Pervasives.prerr_endline (gen_divorce_type x `piq)
  
let print_relation_parent_type x =
  Pervasives.print_endline (gen_relation_parent_type x `piq)
  
let prerr_relation_parent_type x =
  Pervasives.prerr_endline (gen_relation_parent_type x `piq)
  
let print_title_type x = Pervasives.print_endline (gen_title_type x `piq)
  
let prerr_title_type x = Pervasives.prerr_endline (gen_title_type x `piq)
  
let print_search_type x = Pervasives.print_endline (gen_search_type x `piq)
  
let prerr_search_type x = Pervasives.prerr_endline (gen_search_type x `piq)
  
let print_relation_type x =
  Pervasives.print_endline (gen_relation_type x `piq)
  
let prerr_relation_type x =
  Pervasives.prerr_endline (gen_relation_type x `piq)
  
let print_notif_birthday_params x =
  Pervasives.print_endline (gen_notif_birthday_params x `piq)
  
let prerr_notif_birthday_params x =
  Pervasives.prerr_endline (gen_notif_birthday_params x `piq)
  

