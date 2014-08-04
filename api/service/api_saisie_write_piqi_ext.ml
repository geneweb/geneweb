let piqi = Api_saisie_write_piqi.piqi
  
let _ = Piqirun_ext.init_piqi piqi
  
let _protobuf_int32_piqtype = Piqirun_ext.find_piqtype "protobuf-int32"
  
let _int32_piqtype = Piqirun_ext.find_piqtype "int32"
  
let _string_piqtype = Piqirun_ext.find_piqtype "string"
  
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
  
let _dmy_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/dmy"
  
let _date_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/date"
  
let _person_search_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-search"
  
let _simple_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/simple-person"
  
let _witness_event_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/witness-event"
  
let _event_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/event"
  
let _relation_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/relation-person"
  
let _was_witness_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/was-witness"
  
let _person_search_info_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-search-info"
  
let _person_link_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-link"
  
let _witness_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/witness"
  
let _fevent_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/fevent"
  
let _relation_parent_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/relation-parent"
  
let _title_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/title"
  
let _pevent_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/pevent"
  
let _person_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/person"
  
let _family_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/family"
  
let _create_conflict_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/create-conflict"
  
let _modification_status_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/modification-status"
  
let _index_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/index-person"
  
let _index_family_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/index-family"
  
let _index_person_and_family_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/index-person-and-family"
  
let _family_spouse_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/family-spouse"
  
let _add_child_request_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-child-request"
  
let _add_child_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-child"
  
let _add_child_ok_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-child-ok"
  
let _add_parents_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-parents"
  
let _add_parents_ok_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-parents-ok"
  
let _add_family_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-family"
  
let _add_family_ok_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-family-ok"
  
let _edit_family_request_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/edit-family-request"
  
let _edit_family_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/edit-family"
  
let _edit_family_ok_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/edit-family-ok"
  
let _add_sibling_request_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-sibling-request"
  
let _add_sibling_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-sibling"
  
let _add_sibling_ok_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-sibling-ok"
  
let _add_first_fam_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/add-first-fam"
  
let _auto_complete_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/auto-complete"
  
let _auto_complete_result_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/auto-complete-result"
  
let _person_search_list_params_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-search-list-params"
  
let _person_search_list_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-search-list"
  
let _transl_calendar_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-calendar"
  
let _config_transl_calendar_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-calendar"
  
let _transl_witness_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-witness-type"
  
let _config_transl_witness_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-witness-type"
  
let _transl_precision_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-precision"
  
let _config_transl_precision_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-precision"
  
let _transl_death_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-death-type"
  
let _config_transl_death_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-death-type"
  
let _transl_relation_parent_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-relation-parent-type"
  
let _config_transl_relation_parent_type_piqtype =
  Piqirun_ext.find_piqtype
    "api_saisie_write/config-transl-relation-parent-type"
  
let _transl_fevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-fevent-name"
  
let _config_transl_fevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-fevent-name"
  
let _transl_pevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-pevent-name"
  
let _config_transl_pevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-pevent-name"
  
let _transl_access_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-access"
  
let _config_transl_access_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-access"
  
let _transl_update_warning_js_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-update-warning-js"
  
let _config_transl_update_warning_js_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-update-warning-js"
  
let _transl_short_greg_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-short-greg-month"
  
let _config_transl_short_greg_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-short-greg-month"
  
let _transl_french_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-french-month"
  
let _config_transl_french_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-french-month"
  
let _transl_hebrew_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/transl-hebrew-month"
  
let _config_transl_hebrew_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/config-transl-hebrew-month"
  
let _config_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/config"
  
let _sosa_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/sosa"
  
let _calendar_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/calendar"
  
let _witness_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/witness-type"
  
let _precision_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/precision"
  
let _sex_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/sex"
  
let _death_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/death-type"
  
let _relation_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/relation-type"
  
let _create_or_link_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/create-or-link"
  
let _fevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/fevent-name"
  
let _relation_parent_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/relation-parent-type"
  
let _pevent_name_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/pevent-name"
  
let _access_piqtype = Piqirun_ext.find_piqtype "api_saisie_write/access"
  
let _update_warning_js_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/update-warning-js"
  
let _person_or_family_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/person-or-family"
  
let _auto_complete_place_field_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/auto-complete-place-field"
  
let _auto_complete_field_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/auto-complete-field"
  
let _short_greg_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/short-greg-month"
  
let _french_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/french-month"
  
let _hebrew_month_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_write/hebrew-month"
  
let parse_protobuf_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_protobuf_int32 buf
  
let parse_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_int32 buf
  
let parse_string ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_string buf
  
let parse_bool ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_bool buf
  
let parse_dmy ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_dmy buf
  
let parse_date ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_date buf
  
let parse_person_search ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_search_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_search buf
  
let parse_simple_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _simple_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_simple_person buf
  
let parse_witness_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_witness_event buf
  
let parse_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_event buf
  
let parse_relation_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_relation_person buf
  
let parse_was_witness ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _was_witness_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_was_witness buf
  
let parse_person_search_info ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _person_search_info_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_search_info buf
  
let parse_person_link ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_link_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_link buf
  
let parse_witness ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_witness buf
  
let parse_fevent ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fevent_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_fevent buf
  
let parse_relation_parent ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_relation_parent buf
  
let parse_title ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_title buf
  
let parse_pevent ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pevent_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_pevent buf
  
let parse_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person buf
  
let parse_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_family buf
  
let parse_create_conflict ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _create_conflict_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_create_conflict buf
  
let parse_modification_status ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _modification_status_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_modification_status buf
  
let parse_index_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_index_person buf
  
let parse_index_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_index_family buf
  
let parse_index_person_and_family ?opts x (format : Piqirun_ext.input_format)
                                  =
  let x_pb =
    Piqirun_ext.convert _index_person_and_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_index_person_and_family buf
  
let parse_family_spouse ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_spouse_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_family_spouse buf
  
let parse_add_child_request ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _add_child_request_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_child_request buf
  
let parse_add_child ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_child_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_child buf
  
let parse_add_child_ok ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_child_ok_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_child_ok buf
  
let parse_add_parents ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_parents_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_parents buf
  
let parse_add_parents_ok ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _add_parents_ok_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_parents_ok buf
  
let parse_add_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_family buf
  
let parse_add_family_ok ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_family_ok_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_family_ok buf
  
let parse_edit_family_request ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _edit_family_request_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_edit_family_request buf
  
let parse_edit_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edit_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_edit_family buf
  
let parse_edit_family_ok ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _edit_family_ok_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_edit_family_ok buf
  
let parse_add_sibling_request ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _add_sibling_request_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_sibling_request buf
  
let parse_add_sibling ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_sibling_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_sibling buf
  
let parse_add_sibling_ok ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _add_sibling_ok_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_sibling_ok buf
  
let parse_add_first_fam ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_first_fam_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_add_first_fam buf
  
let parse_auto_complete ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _auto_complete_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_auto_complete buf
  
let parse_auto_complete_result ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _auto_complete_result_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_auto_complete_result buf
  
let parse_person_search_list_params ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _person_search_list_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_search_list_params buf
  
let parse_person_search_list ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _person_search_list_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_search_list buf
  
let parse_transl_calendar ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_calendar_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_calendar buf
  
let parse_config_transl_calendar ?opts x (format : Piqirun_ext.input_format)
                                 =
  let x_pb =
    Piqirun_ext.convert _config_transl_calendar_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_calendar buf
  
let parse_transl_witness_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_witness_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_witness_type buf
  
let parse_config_transl_witness_type ?opts x
                                     (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_witness_type_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_witness_type buf
  
let parse_transl_precision ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_precision_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_precision buf
  
let parse_config_transl_precision ?opts x (format : Piqirun_ext.input_format)
                                  =
  let x_pb =
    Piqirun_ext.convert _config_transl_precision_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_precision buf
  
let parse_transl_death_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_death_type buf
  
let parse_config_transl_death_type ?opts x
                                   (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_death_type buf
  
let parse_transl_relation_parent_type ?opts x
                                      (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_relation_parent_type_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_relation_parent_type buf
  
let parse_config_transl_relation_parent_type ?opts x
                                             (format : Piqirun_ext.
                                              input_format)
                                             =
  let x_pb =
    Piqirun_ext.convert _config_transl_relation_parent_type_piqtype format
      `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_relation_parent_type buf
  
let parse_transl_fevent_name ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_fevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_fevent_name buf
  
let parse_config_transl_fevent_name ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_fevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_fevent_name buf
  
let parse_transl_pevent_name ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_pevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_pevent_name buf
  
let parse_config_transl_pevent_name ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_pevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_pevent_name buf
  
let parse_transl_access ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_access_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_access buf
  
let parse_config_transl_access ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_access_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_access buf
  
let parse_transl_update_warning_js ?opts x
                                   (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_update_warning_js_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_update_warning_js buf
  
let parse_config_transl_update_warning_js ?opts x
                                          (format : Piqirun_ext.input_format)
                                          =
  let x_pb =
    Piqirun_ext.convert _config_transl_update_warning_js_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_update_warning_js buf
  
let parse_transl_short_greg_month ?opts x (format : Piqirun_ext.input_format)
                                  =
  let x_pb =
    Piqirun_ext.convert _transl_short_greg_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_short_greg_month buf
  
let parse_config_transl_short_greg_month ?opts x
                                         (format : Piqirun_ext.input_format)
                                         =
  let x_pb =
    Piqirun_ext.convert _config_transl_short_greg_month_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_short_greg_month buf
  
let parse_transl_french_month ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_french_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_french_month buf
  
let parse_config_transl_french_month ?opts x
                                     (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_french_month_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_french_month buf
  
let parse_transl_hebrew_month ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _transl_hebrew_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_transl_hebrew_month buf
  
let parse_config_transl_hebrew_month ?opts x
                                     (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _config_transl_hebrew_month_piqtype format `pb x
      ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config_transl_hebrew_month buf
  
let parse_config ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_config buf
  
let parse_sosa ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sosa_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_sosa buf
  
let parse_calendar ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_calendar buf
  
let parse_witness_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_witness_type buf
  
let parse_precision ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_precision buf
  
let parse_sex ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_sex buf
  
let parse_death_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_death_type buf
  
let parse_relation_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_relation_type buf
  
let parse_create_or_link ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _create_or_link_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_create_or_link buf
  
let parse_fevent_name ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_fevent_name buf
  
let parse_relation_parent_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_relation_parent_type buf
  
let parse_pevent_name ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pevent_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_pevent_name buf
  
let parse_access ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _access_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_access buf
  
let parse_update_warning_js ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _update_warning_js_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_update_warning_js buf
  
let parse_person_or_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _person_or_family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_person_or_family buf
  
let parse_auto_complete_place_field ?opts x
                                    (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _auto_complete_place_field_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_auto_complete_place_field buf
  
let parse_auto_complete_field ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _auto_complete_field_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_auto_complete_field buf
  
let parse_short_greg_month ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _short_greg_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_short_greg_month buf
  
let parse_french_month ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _french_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_french_month buf
  
let parse_hebrew_month ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _hebrew_month_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_write_piqi.parse_hebrew_month buf
  
let gen_protobuf_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_protobuf_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int32_piqtype `pb format x_pb ?opts
  
let gen_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int32_piqtype `pb format x_pb ?opts
  
let gen_string ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_string x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
  
let gen_bool ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_bool x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
  
let gen_dmy ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _dmy_piqtype `pb format x_pb ?opts
  
let gen_date ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _date_piqtype `pb format x_pb ?opts
  
let gen_person_search ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_search_piqtype `pb format x_pb ?opts
  
let gen_simple_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_simple_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _simple_person_piqtype `pb format x_pb ?opts
  
let gen_witness_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_event_piqtype `pb format x_pb ?opts
  
let gen_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _event_piqtype `pb format x_pb ?opts
  
let gen_relation_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_person_piqtype `pb format x_pb ?opts
  
let gen_was_witness ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_was_witness x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _was_witness_piqtype `pb format x_pb ?opts
  
let gen_person_search_info ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_info x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_search_info_piqtype `pb format x_pb ?opts
  
let gen_person_link ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_link x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_link_piqtype `pb format x_pb ?opts
  
let gen_witness ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_piqtype `pb format x_pb ?opts
  
let gen_fevent ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_fevent x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _fevent_piqtype `pb format x_pb ?opts
  
let gen_relation_parent ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_piqtype `pb format x_pb ?opts
  
let gen_title ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _title_piqtype `pb format x_pb ?opts
  
let gen_pevent ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_pevent x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _pevent_piqtype `pb format x_pb ?opts
  
let gen_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_piqtype `pb format x_pb ?opts
  
let gen_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _family_piqtype `pb format x_pb ?opts
  
let gen_create_conflict ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_create_conflict x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _create_conflict_piqtype `pb format x_pb ?opts
  
let gen_modification_status ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _modification_status_piqtype `pb format x_pb ?opts
  
let gen_index_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_index_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _index_person_piqtype `pb format x_pb ?opts
  
let gen_index_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_index_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _index_family_piqtype `pb format x_pb ?opts
  
let gen_index_person_and_family ?opts x (format : Piqirun_ext.output_format)
                                =
  let buf = Api_saisie_write_piqi.gen_index_person_and_family x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _index_person_and_family_piqtype `pb format x_pb
      ?opts
  
let gen_family_spouse ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_family_spouse x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _family_spouse_piqtype `pb format x_pb ?opts
  
let gen_add_child_request ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child_request x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_child_request_piqtype `pb format x_pb ?opts
  
let gen_add_child ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_child_piqtype `pb format x_pb ?opts
  
let gen_add_child_ok ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child_ok x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_child_ok_piqtype `pb format x_pb ?opts
  
let gen_add_parents ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_parents x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_parents_piqtype `pb format x_pb ?opts
  
let gen_add_parents_ok ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_parents_ok x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_parents_ok_piqtype `pb format x_pb ?opts
  
let gen_add_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_family_piqtype `pb format x_pb ?opts
  
let gen_add_family_ok ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_family_ok x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_family_ok_piqtype `pb format x_pb ?opts
  
let gen_edit_family_request ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family_request x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _edit_family_request_piqtype `pb format x_pb ?opts
  
let gen_edit_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _edit_family_piqtype `pb format x_pb ?opts
  
let gen_edit_family_ok ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family_ok x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _edit_family_ok_piqtype `pb format x_pb ?opts
  
let gen_add_sibling_request ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling_request x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_sibling_request_piqtype `pb format x_pb ?opts
  
let gen_add_sibling ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_sibling_piqtype `pb format x_pb ?opts
  
let gen_add_sibling_ok ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling_ok x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_sibling_ok_piqtype `pb format x_pb ?opts
  
let gen_add_first_fam ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_first_fam x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _add_first_fam_piqtype `pb format x_pb ?opts
  
let gen_auto_complete ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _auto_complete_piqtype `pb format x_pb ?opts
  
let gen_auto_complete_result ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_result x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _auto_complete_result_piqtype `pb format x_pb ?opts
  
let gen_person_search_list_params ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_list_params x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _person_search_list_params_piqtype `pb format x_pb
      ?opts
  
let gen_person_search_list ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_list x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_search_list_piqtype `pb format x_pb ?opts
  
let gen_transl_calendar ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_calendar x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_calendar_piqtype `pb format x_pb ?opts
  
let gen_config_transl_calendar ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_calendar x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_calendar_piqtype `pb format x_pb ?opts
  
let gen_transl_witness_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_witness_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_witness_type_piqtype `pb format x_pb ?opts
  
let gen_config_transl_witness_type ?opts x
                                   (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_witness_type x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_witness_type_piqtype `pb format x_pb
      ?opts
  
let gen_transl_precision ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_precision x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_precision_piqtype `pb format x_pb ?opts
  
let gen_config_transl_precision ?opts x (format : Piqirun_ext.output_format)
                                =
  let buf = Api_saisie_write_piqi.gen_config_transl_precision x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_precision_piqtype `pb format x_pb
      ?opts
  
let gen_transl_death_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_death_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_death_type_piqtype `pb format x_pb ?opts
  
let gen_config_transl_death_type ?opts x (format : Piqirun_ext.output_format)
                                 =
  let buf = Api_saisie_write_piqi.gen_config_transl_death_type x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_death_type_piqtype `pb format x_pb
      ?opts
  
let gen_transl_relation_parent_type ?opts x
                                    (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_relation_parent_type x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _transl_relation_parent_type_piqtype `pb format x_pb
      ?opts
  
let gen_config_transl_relation_parent_type ?opts x
                                           (format : Piqirun_ext.
                                            output_format)
                                           =
  let buf = Api_saisie_write_piqi.gen_config_transl_relation_parent_type x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_relation_parent_type_piqtype `pb
      format x_pb ?opts
  
let gen_transl_fevent_name ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_fevent_name x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_fevent_name_piqtype `pb format x_pb ?opts
  
let gen_config_transl_fevent_name ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_fevent_name x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_fevent_name_piqtype `pb format x_pb
      ?opts
  
let gen_transl_pevent_name ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_pevent_name x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_pevent_name_piqtype `pb format x_pb ?opts
  
let gen_config_transl_pevent_name ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_pevent_name x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_pevent_name_piqtype `pb format x_pb
      ?opts
  
let gen_transl_access ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_access x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_access_piqtype `pb format x_pb ?opts
  
let gen_config_transl_access ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_access x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _config_transl_access_piqtype `pb format x_pb ?opts
  
let gen_transl_update_warning_js ?opts x (format : Piqirun_ext.output_format)
                                 =
  let buf = Api_saisie_write_piqi.gen_transl_update_warning_js x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _transl_update_warning_js_piqtype `pb format x_pb
      ?opts
  
let gen_config_transl_update_warning_js ?opts x
                                        (format : Piqirun_ext.output_format)
                                        =
  let buf = Api_saisie_write_piqi.gen_config_transl_update_warning_js x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_update_warning_js_piqtype `pb format
      x_pb ?opts
  
let gen_transl_short_greg_month ?opts x (format : Piqirun_ext.output_format)
                                =
  let buf = Api_saisie_write_piqi.gen_transl_short_greg_month x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _transl_short_greg_month_piqtype `pb format x_pb
      ?opts
  
let gen_config_transl_short_greg_month ?opts x
                                       (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_short_greg_month x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_short_greg_month_piqtype `pb format
      x_pb ?opts
  
let gen_transl_french_month ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_french_month x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_french_month_piqtype `pb format x_pb ?opts
  
let gen_config_transl_french_month ?opts x
                                   (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_french_month x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_french_month_piqtype `pb format x_pb
      ?opts
  
let gen_transl_hebrew_month ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_hebrew_month x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _transl_hebrew_month_piqtype `pb format x_pb ?opts
  
let gen_config_transl_hebrew_month ?opts x
                                   (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_hebrew_month x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _config_transl_hebrew_month_piqtype `pb format x_pb
      ?opts
  
let gen_config ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _config_piqtype `pb format x_pb ?opts
  
let gen_sosa ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_sosa x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sosa_piqtype `pb format x_pb ?opts
  
let gen_calendar ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _calendar_piqtype `pb format x_pb ?opts
  
let gen_witness_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_type_piqtype `pb format x_pb ?opts
  
let gen_precision ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _precision_piqtype `pb format x_pb ?opts
  
let gen_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sex_piqtype `pb format x_pb ?opts
  
let gen_death_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _death_type_piqtype `pb format x_pb ?opts
  
let gen_relation_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_type_piqtype `pb format x_pb ?opts
  
let gen_create_or_link ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_create_or_link x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _create_or_link_piqtype `pb format x_pb ?opts
  
let gen_fevent_name ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_fevent_name x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _fevent_name_piqtype `pb format x_pb ?opts
  
let gen_relation_parent_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_type_piqtype `pb format x_pb ?opts
  
let gen_pevent_name ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_pevent_name x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _pevent_name_piqtype `pb format x_pb ?opts
  
let gen_access ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_access x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _access_piqtype `pb format x_pb ?opts
  
let gen_update_warning_js ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_update_warning_js x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _update_warning_js_piqtype `pb format x_pb ?opts
  
let gen_person_or_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_or_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_or_family_piqtype `pb format x_pb ?opts
  
let gen_auto_complete_place_field ?opts x
                                  (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_place_field x in
  let x_pb = Piqirun.to_string buf
  in
    Piqirun_ext.convert _auto_complete_place_field_piqtype `pb format x_pb
      ?opts
  
let gen_auto_complete_field ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_field x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _auto_complete_field_piqtype `pb format x_pb ?opts
  
let gen_short_greg_month ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_short_greg_month x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _short_greg_month_piqtype `pb format x_pb ?opts
  
let gen_french_month ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_french_month x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _french_month_piqtype `pb format x_pb ?opts
  
let gen_hebrew_month ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_hebrew_month x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _hebrew_month_piqtype `pb format x_pb ?opts
  
let print_protobuf_int32 x =
  Pervasives.print_endline (gen_protobuf_int32 x `piq)
  
let prerr_protobuf_int32 x =
  Pervasives.prerr_endline (gen_protobuf_int32 x `piq)
  
let print_int32 x = Pervasives.print_endline (gen_int32 x `piq)
  
let prerr_int32 x = Pervasives.prerr_endline (gen_int32 x `piq)
  
let print_string x = Pervasives.print_endline (gen_string x `piq)
  
let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
  
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
  
let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
  
let print_dmy x = Pervasives.print_endline (gen_dmy x `piq)
  
let prerr_dmy x = Pervasives.prerr_endline (gen_dmy x `piq)
  
let print_date x = Pervasives.print_endline (gen_date x `piq)
  
let prerr_date x = Pervasives.prerr_endline (gen_date x `piq)
  
let print_person_search x =
  Pervasives.print_endline (gen_person_search x `piq)
  
let prerr_person_search x =
  Pervasives.prerr_endline (gen_person_search x `piq)
  
let print_simple_person x =
  Pervasives.print_endline (gen_simple_person x `piq)
  
let prerr_simple_person x =
  Pervasives.prerr_endline (gen_simple_person x `piq)
  
let print_witness_event x =
  Pervasives.print_endline (gen_witness_event x `piq)
  
let prerr_witness_event x =
  Pervasives.prerr_endline (gen_witness_event x `piq)
  
let print_event x = Pervasives.print_endline (gen_event x `piq)
  
let prerr_event x = Pervasives.prerr_endline (gen_event x `piq)
  
let print_relation_person x =
  Pervasives.print_endline (gen_relation_person x `piq)
  
let prerr_relation_person x =
  Pervasives.prerr_endline (gen_relation_person x `piq)
  
let print_was_witness x = Pervasives.print_endline (gen_was_witness x `piq)
  
let prerr_was_witness x = Pervasives.prerr_endline (gen_was_witness x `piq)
  
let print_person_search_info x =
  Pervasives.print_endline (gen_person_search_info x `piq)
  
let prerr_person_search_info x =
  Pervasives.prerr_endline (gen_person_search_info x `piq)
  
let print_person_link x = Pervasives.print_endline (gen_person_link x `piq)
  
let prerr_person_link x = Pervasives.prerr_endline (gen_person_link x `piq)
  
let print_witness x = Pervasives.print_endline (gen_witness x `piq)
  
let prerr_witness x = Pervasives.prerr_endline (gen_witness x `piq)
  
let print_fevent x = Pervasives.print_endline (gen_fevent x `piq)
  
let prerr_fevent x = Pervasives.prerr_endline (gen_fevent x `piq)
  
let print_relation_parent x =
  Pervasives.print_endline (gen_relation_parent x `piq)
  
let prerr_relation_parent x =
  Pervasives.prerr_endline (gen_relation_parent x `piq)
  
let print_title x = Pervasives.print_endline (gen_title x `piq)
  
let prerr_title x = Pervasives.prerr_endline (gen_title x `piq)
  
let print_pevent x = Pervasives.print_endline (gen_pevent x `piq)
  
let prerr_pevent x = Pervasives.prerr_endline (gen_pevent x `piq)
  
let print_person x = Pervasives.print_endline (gen_person x `piq)
  
let prerr_person x = Pervasives.prerr_endline (gen_person x `piq)
  
let print_family x = Pervasives.print_endline (gen_family x `piq)
  
let prerr_family x = Pervasives.prerr_endline (gen_family x `piq)
  
let print_create_conflict x =
  Pervasives.print_endline (gen_create_conflict x `piq)
  
let prerr_create_conflict x =
  Pervasives.prerr_endline (gen_create_conflict x `piq)
  
let print_modification_status x =
  Pervasives.print_endline (gen_modification_status x `piq)
  
let prerr_modification_status x =
  Pervasives.prerr_endline (gen_modification_status x `piq)
  
let print_index_person x = Pervasives.print_endline (gen_index_person x `piq)
  
let prerr_index_person x = Pervasives.prerr_endline (gen_index_person x `piq)
  
let print_index_family x = Pervasives.print_endline (gen_index_family x `piq)
  
let prerr_index_family x = Pervasives.prerr_endline (gen_index_family x `piq)
  
let print_index_person_and_family x =
  Pervasives.print_endline (gen_index_person_and_family x `piq)
  
let prerr_index_person_and_family x =
  Pervasives.prerr_endline (gen_index_person_and_family x `piq)
  
let print_family_spouse x =
  Pervasives.print_endline (gen_family_spouse x `piq)
  
let prerr_family_spouse x =
  Pervasives.prerr_endline (gen_family_spouse x `piq)
  
let print_add_child_request x =
  Pervasives.print_endline (gen_add_child_request x `piq)
  
let prerr_add_child_request x =
  Pervasives.prerr_endline (gen_add_child_request x `piq)
  
let print_add_child x = Pervasives.print_endline (gen_add_child x `piq)
  
let prerr_add_child x = Pervasives.prerr_endline (gen_add_child x `piq)
  
let print_add_child_ok x = Pervasives.print_endline (gen_add_child_ok x `piq)
  
let prerr_add_child_ok x = Pervasives.prerr_endline (gen_add_child_ok x `piq)
  
let print_add_parents x = Pervasives.print_endline (gen_add_parents x `piq)
  
let prerr_add_parents x = Pervasives.prerr_endline (gen_add_parents x `piq)
  
let print_add_parents_ok x =
  Pervasives.print_endline (gen_add_parents_ok x `piq)
  
let prerr_add_parents_ok x =
  Pervasives.prerr_endline (gen_add_parents_ok x `piq)
  
let print_add_family x = Pervasives.print_endline (gen_add_family x `piq)
  
let prerr_add_family x = Pervasives.prerr_endline (gen_add_family x `piq)
  
let print_add_family_ok x =
  Pervasives.print_endline (gen_add_family_ok x `piq)
  
let prerr_add_family_ok x =
  Pervasives.prerr_endline (gen_add_family_ok x `piq)
  
let print_edit_family_request x =
  Pervasives.print_endline (gen_edit_family_request x `piq)
  
let prerr_edit_family_request x =
  Pervasives.prerr_endline (gen_edit_family_request x `piq)
  
let print_edit_family x = Pervasives.print_endline (gen_edit_family x `piq)
  
let prerr_edit_family x = Pervasives.prerr_endline (gen_edit_family x `piq)
  
let print_edit_family_ok x =
  Pervasives.print_endline (gen_edit_family_ok x `piq)
  
let prerr_edit_family_ok x =
  Pervasives.prerr_endline (gen_edit_family_ok x `piq)
  
let print_add_sibling_request x =
  Pervasives.print_endline (gen_add_sibling_request x `piq)
  
let prerr_add_sibling_request x =
  Pervasives.prerr_endline (gen_add_sibling_request x `piq)
  
let print_add_sibling x = Pervasives.print_endline (gen_add_sibling x `piq)
  
let prerr_add_sibling x = Pervasives.prerr_endline (gen_add_sibling x `piq)
  
let print_add_sibling_ok x =
  Pervasives.print_endline (gen_add_sibling_ok x `piq)
  
let prerr_add_sibling_ok x =
  Pervasives.prerr_endline (gen_add_sibling_ok x `piq)
  
let print_add_first_fam x =
  Pervasives.print_endline (gen_add_first_fam x `piq)
  
let prerr_add_first_fam x =
  Pervasives.prerr_endline (gen_add_first_fam x `piq)
  
let print_auto_complete x =
  Pervasives.print_endline (gen_auto_complete x `piq)
  
let prerr_auto_complete x =
  Pervasives.prerr_endline (gen_auto_complete x `piq)
  
let print_auto_complete_result x =
  Pervasives.print_endline (gen_auto_complete_result x `piq)
  
let prerr_auto_complete_result x =
  Pervasives.prerr_endline (gen_auto_complete_result x `piq)
  
let print_person_search_list_params x =
  Pervasives.print_endline (gen_person_search_list_params x `piq)
  
let prerr_person_search_list_params x =
  Pervasives.prerr_endline (gen_person_search_list_params x `piq)
  
let print_person_search_list x =
  Pervasives.print_endline (gen_person_search_list x `piq)
  
let prerr_person_search_list x =
  Pervasives.prerr_endline (gen_person_search_list x `piq)
  
let print_transl_calendar x =
  Pervasives.print_endline (gen_transl_calendar x `piq)
  
let prerr_transl_calendar x =
  Pervasives.prerr_endline (gen_transl_calendar x `piq)
  
let print_config_transl_calendar x =
  Pervasives.print_endline (gen_config_transl_calendar x `piq)
  
let prerr_config_transl_calendar x =
  Pervasives.prerr_endline (gen_config_transl_calendar x `piq)
  
let print_transl_witness_type x =
  Pervasives.print_endline (gen_transl_witness_type x `piq)
  
let prerr_transl_witness_type x =
  Pervasives.prerr_endline (gen_transl_witness_type x `piq)
  
let print_config_transl_witness_type x =
  Pervasives.print_endline (gen_config_transl_witness_type x `piq)
  
let prerr_config_transl_witness_type x =
  Pervasives.prerr_endline (gen_config_transl_witness_type x `piq)
  
let print_transl_precision x =
  Pervasives.print_endline (gen_transl_precision x `piq)
  
let prerr_transl_precision x =
  Pervasives.prerr_endline (gen_transl_precision x `piq)
  
let print_config_transl_precision x =
  Pervasives.print_endline (gen_config_transl_precision x `piq)
  
let prerr_config_transl_precision x =
  Pervasives.prerr_endline (gen_config_transl_precision x `piq)
  
let print_transl_death_type x =
  Pervasives.print_endline (gen_transl_death_type x `piq)
  
let prerr_transl_death_type x =
  Pervasives.prerr_endline (gen_transl_death_type x `piq)
  
let print_config_transl_death_type x =
  Pervasives.print_endline (gen_config_transl_death_type x `piq)
  
let prerr_config_transl_death_type x =
  Pervasives.prerr_endline (gen_config_transl_death_type x `piq)
  
let print_transl_relation_parent_type x =
  Pervasives.print_endline (gen_transl_relation_parent_type x `piq)
  
let prerr_transl_relation_parent_type x =
  Pervasives.prerr_endline (gen_transl_relation_parent_type x `piq)
  
let print_config_transl_relation_parent_type x =
  Pervasives.print_endline (gen_config_transl_relation_parent_type x `piq)
  
let prerr_config_transl_relation_parent_type x =
  Pervasives.prerr_endline (gen_config_transl_relation_parent_type x `piq)
  
let print_transl_fevent_name x =
  Pervasives.print_endline (gen_transl_fevent_name x `piq)
  
let prerr_transl_fevent_name x =
  Pervasives.prerr_endline (gen_transl_fevent_name x `piq)
  
let print_config_transl_fevent_name x =
  Pervasives.print_endline (gen_config_transl_fevent_name x `piq)
  
let prerr_config_transl_fevent_name x =
  Pervasives.prerr_endline (gen_config_transl_fevent_name x `piq)
  
let print_transl_pevent_name x =
  Pervasives.print_endline (gen_transl_pevent_name x `piq)
  
let prerr_transl_pevent_name x =
  Pervasives.prerr_endline (gen_transl_pevent_name x `piq)
  
let print_config_transl_pevent_name x =
  Pervasives.print_endline (gen_config_transl_pevent_name x `piq)
  
let prerr_config_transl_pevent_name x =
  Pervasives.prerr_endline (gen_config_transl_pevent_name x `piq)
  
let print_transl_access x =
  Pervasives.print_endline (gen_transl_access x `piq)
  
let prerr_transl_access x =
  Pervasives.prerr_endline (gen_transl_access x `piq)
  
let print_config_transl_access x =
  Pervasives.print_endline (gen_config_transl_access x `piq)
  
let prerr_config_transl_access x =
  Pervasives.prerr_endline (gen_config_transl_access x `piq)
  
let print_transl_update_warning_js x =
  Pervasives.print_endline (gen_transl_update_warning_js x `piq)
  
let prerr_transl_update_warning_js x =
  Pervasives.prerr_endline (gen_transl_update_warning_js x `piq)
  
let print_config_transl_update_warning_js x =
  Pervasives.print_endline (gen_config_transl_update_warning_js x `piq)
  
let prerr_config_transl_update_warning_js x =
  Pervasives.prerr_endline (gen_config_transl_update_warning_js x `piq)
  
let print_transl_short_greg_month x =
  Pervasives.print_endline (gen_transl_short_greg_month x `piq)
  
let prerr_transl_short_greg_month x =
  Pervasives.prerr_endline (gen_transl_short_greg_month x `piq)
  
let print_config_transl_short_greg_month x =
  Pervasives.print_endline (gen_config_transl_short_greg_month x `piq)
  
let prerr_config_transl_short_greg_month x =
  Pervasives.prerr_endline (gen_config_transl_short_greg_month x `piq)
  
let print_transl_french_month x =
  Pervasives.print_endline (gen_transl_french_month x `piq)
  
let prerr_transl_french_month x =
  Pervasives.prerr_endline (gen_transl_french_month x `piq)
  
let print_config_transl_french_month x =
  Pervasives.print_endline (gen_config_transl_french_month x `piq)
  
let prerr_config_transl_french_month x =
  Pervasives.prerr_endline (gen_config_transl_french_month x `piq)
  
let print_transl_hebrew_month x =
  Pervasives.print_endline (gen_transl_hebrew_month x `piq)
  
let prerr_transl_hebrew_month x =
  Pervasives.prerr_endline (gen_transl_hebrew_month x `piq)
  
let print_config_transl_hebrew_month x =
  Pervasives.print_endline (gen_config_transl_hebrew_month x `piq)
  
let prerr_config_transl_hebrew_month x =
  Pervasives.prerr_endline (gen_config_transl_hebrew_month x `piq)
  
let print_config x = Pervasives.print_endline (gen_config x `piq)
  
let prerr_config x = Pervasives.prerr_endline (gen_config x `piq)
  
let print_sosa x = Pervasives.print_endline (gen_sosa x `piq)
  
let prerr_sosa x = Pervasives.prerr_endline (gen_sosa x `piq)
  
let print_calendar x = Pervasives.print_endline (gen_calendar x `piq)
  
let prerr_calendar x = Pervasives.prerr_endline (gen_calendar x `piq)
  
let print_witness_type x = Pervasives.print_endline (gen_witness_type x `piq)
  
let prerr_witness_type x = Pervasives.prerr_endline (gen_witness_type x `piq)
  
let print_precision x = Pervasives.print_endline (gen_precision x `piq)
  
let prerr_precision x = Pervasives.prerr_endline (gen_precision x `piq)
  
let print_sex x = Pervasives.print_endline (gen_sex x `piq)
  
let prerr_sex x = Pervasives.prerr_endline (gen_sex x `piq)
  
let print_death_type x = Pervasives.print_endline (gen_death_type x `piq)
  
let prerr_death_type x = Pervasives.prerr_endline (gen_death_type x `piq)
  
let print_relation_type x =
  Pervasives.print_endline (gen_relation_type x `piq)
  
let prerr_relation_type x =
  Pervasives.prerr_endline (gen_relation_type x `piq)
  
let print_create_or_link x =
  Pervasives.print_endline (gen_create_or_link x `piq)
  
let prerr_create_or_link x =
  Pervasives.prerr_endline (gen_create_or_link x `piq)
  
let print_fevent_name x = Pervasives.print_endline (gen_fevent_name x `piq)
  
let prerr_fevent_name x = Pervasives.prerr_endline (gen_fevent_name x `piq)
  
let print_relation_parent_type x =
  Pervasives.print_endline (gen_relation_parent_type x `piq)
  
let prerr_relation_parent_type x =
  Pervasives.prerr_endline (gen_relation_parent_type x `piq)
  
let print_pevent_name x = Pervasives.print_endline (gen_pevent_name x `piq)
  
let prerr_pevent_name x = Pervasives.prerr_endline (gen_pevent_name x `piq)
  
let print_access x = Pervasives.print_endline (gen_access x `piq)
  
let prerr_access x = Pervasives.prerr_endline (gen_access x `piq)
  
let print_update_warning_js x =
  Pervasives.print_endline (gen_update_warning_js x `piq)
  
let prerr_update_warning_js x =
  Pervasives.prerr_endline (gen_update_warning_js x `piq)
  
let print_person_or_family x =
  Pervasives.print_endline (gen_person_or_family x `piq)
  
let prerr_person_or_family x =
  Pervasives.prerr_endline (gen_person_or_family x `piq)
  
let print_auto_complete_place_field x =
  Pervasives.print_endline (gen_auto_complete_place_field x `piq)
  
let prerr_auto_complete_place_field x =
  Pervasives.prerr_endline (gen_auto_complete_place_field x `piq)
  
let print_auto_complete_field x =
  Pervasives.print_endline (gen_auto_complete_field x `piq)
  
let prerr_auto_complete_field x =
  Pervasives.prerr_endline (gen_auto_complete_field x `piq)
  
let print_short_greg_month x =
  Pervasives.print_endline (gen_short_greg_month x `piq)
  
let prerr_short_greg_month x =
  Pervasives.prerr_endline (gen_short_greg_month x `piq)
  
let print_french_month x = Pervasives.print_endline (gen_french_month x `piq)
  
let prerr_french_month x = Pervasives.prerr_endline (gen_french_month x `piq)
  
let print_hebrew_month x = Pervasives.print_endline (gen_hebrew_month x `piq)
  
let prerr_hebrew_month x = Pervasives.prerr_endline (gen_hebrew_month x `piq)
  

