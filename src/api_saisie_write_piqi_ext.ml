let piqi = Api_saisie_write_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _dmy_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/dmy"
let _date_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/date"
let _person_search_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-search"
let _simple_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/simple-person"
let _witness_event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/witness-event"
let _event_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/event"
let _relation_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/relation-person"
let _was_witness_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/was-witness"
let _person_search_info_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-search-info"
let _person_link_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-link"
let _witness_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/witness"
let _fevent_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/fevent"
let _relation_parent_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/relation-parent"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/title"
let _pevent_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/pevent"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person"
let _family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/family"
let _create_conflict_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/create-conflict"
let _modification_status_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/modification-status"
let _index_person_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/index-person"
let _index_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/index-family"
let _index_person_and_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/index-person-and-family"
let _family_spouse_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/family-spouse"
let _add_child_request_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-child-request"
let _add_child_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-child"
let _add_child_ok_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-child-ok"
let _add_parents_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-parents"
let _add_parents_ok_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-parents-ok"
let _add_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-family"
let _add_family_ok_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-family-ok"
let _edit_family_request_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/edit-family-request"
let _edit_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/edit-family"
let _edit_family_ok_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/edit-family-ok"
let _add_sibling_request_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-sibling-request"
let _add_sibling_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-sibling"
let _add_sibling_ok_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-sibling-ok"
let _add_first_fam_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/add-first-fam"
let _auto_complete_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/auto-complete"
let _auto_complete_result_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/auto-complete-result"
let _person_search_list_params_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-search-list-params"
let _person_search_list_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-search-list"
let _transl_calendar_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-calendar"
let _config_transl_calendar_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-calendar"
let _transl_witness_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-witness-type"
let _config_transl_witness_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-witness-type"
let _transl_precision_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-precision"
let _config_transl_precision_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-precision"
let _transl_death_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-death-type"
let _config_transl_death_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-death-type"
let _transl_relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-relation-parent-type"
let _config_transl_relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-relation-parent-type"
let _transl_fevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-fevent-name"
let _config_transl_fevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-fevent-name"
let _transl_pevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-pevent-name"
let _config_transl_pevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-pevent-name"
let _transl_access_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-access"
let _config_transl_access_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-access"
let _transl_update_warning_js_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-update-warning-js"
let _config_transl_update_warning_js_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-update-warning-js"
let _transl_short_greg_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-short-greg-month"
let _config_transl_short_greg_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-short-greg-month"
let _transl_french_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-french-month"
let _config_transl_french_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-french-month"
let _transl_hebrew_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/transl-hebrew-month"
let _config_transl_hebrew_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config-transl-hebrew-month"
let _config_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/config"
let _sosa_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/sosa"
let _calendar_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/calendar"
let _witness_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/witness-type"
let _precision_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/precision"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/death-type"
let _relation_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/relation-type"
let _create_or_link_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/create-or-link"
let _fevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/fevent-name"
let _relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/relation-parent-type"
let _pevent_name_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/pevent-name"
let _access_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/access"
let _update_warning_js_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/update-warning-js"
let _person_or_family_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/person-or-family"
let _auto_complete_place_field_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/auto-complete-place-field"
let _auto_complete_field_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/auto-complete-field"
let _short_greg_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/short-greg-month"
let _french_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/french-month"
let _hebrew_month_piqi_type = Piqirun_ext.find_piqi_type "api_saisie_write/hebrew-month"


let parse_dmy ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_dmy buf

let parse_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_date buf

let parse_person_search ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_search_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_search buf

let parse_simple_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _simple_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_simple_person buf

let parse_witness_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_witness_event buf

let parse_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_event buf

let parse_relation_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_relation_person buf

let parse_was_witness ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _was_witness_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_was_witness buf

let parse_person_search_info ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_search_info_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_search_info buf

let parse_person_link ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_link_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_link buf

let parse_witness ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_witness buf

let parse_fevent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fevent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_fevent buf

let parse_relation_parent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_relation_parent buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_title buf

let parse_pevent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pevent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_pevent buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person buf

let parse_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_family buf

let parse_create_conflict ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _create_conflict_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_create_conflict buf

let parse_modification_status ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modification_status_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_modification_status buf

let parse_index_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_index_person buf

let parse_index_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_index_family buf

let parse_index_person_and_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_person_and_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_index_person_and_family buf

let parse_family_spouse ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_spouse_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_family_spouse buf

let parse_add_child_request ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_child_request_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_child_request buf

let parse_add_child ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_child_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_child buf

let parse_add_child_ok ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_child_ok_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_child_ok buf

let parse_add_parents ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_parents_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_parents buf

let parse_add_parents_ok ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_parents_ok_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_parents_ok buf

let parse_add_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_family buf

let parse_add_family_ok ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_family_ok_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_family_ok buf

let parse_edit_family_request ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edit_family_request_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_edit_family_request buf

let parse_edit_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edit_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_edit_family buf

let parse_edit_family_ok ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edit_family_ok_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_edit_family_ok buf

let parse_add_sibling_request ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_sibling_request_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_sibling_request buf

let parse_add_sibling ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_sibling_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_sibling buf

let parse_add_sibling_ok ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_sibling_ok_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_sibling_ok buf

let parse_add_first_fam ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _add_first_fam_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_add_first_fam buf

let parse_auto_complete ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _auto_complete_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_auto_complete buf

let parse_auto_complete_result ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _auto_complete_result_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_auto_complete_result buf

let parse_person_search_list_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_search_list_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_search_list_params buf

let parse_person_search_list ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_search_list_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_search_list buf

let parse_transl_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_calendar buf

let parse_config_transl_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_calendar buf

let parse_transl_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_witness_type buf

let parse_config_transl_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_witness_type buf

let parse_transl_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_precision buf

let parse_config_transl_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_precision buf

let parse_transl_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_death_type buf

let parse_config_transl_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_death_type buf

let parse_transl_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_relation_parent_type buf

let parse_config_transl_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_relation_parent_type buf

let parse_transl_fevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_fevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_fevent_name buf

let parse_config_transl_fevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_fevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_fevent_name buf

let parse_transl_pevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_pevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_pevent_name buf

let parse_config_transl_pevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_pevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_pevent_name buf

let parse_transl_access ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_access_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_access buf

let parse_config_transl_access ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_access_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_access buf

let parse_transl_update_warning_js ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_update_warning_js_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_update_warning_js buf

let parse_config_transl_update_warning_js ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_update_warning_js_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_update_warning_js buf

let parse_transl_short_greg_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_short_greg_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_short_greg_month buf

let parse_config_transl_short_greg_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_short_greg_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_short_greg_month buf

let parse_transl_french_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_french_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_french_month buf

let parse_config_transl_french_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_french_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_french_month buf

let parse_transl_hebrew_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _transl_hebrew_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_transl_hebrew_month buf

let parse_config_transl_hebrew_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_transl_hebrew_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config_transl_hebrew_month buf

let parse_config ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _config_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_config buf

let parse_sosa ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sosa_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_sosa buf

let parse_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_calendar buf

let parse_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_witness_type buf

let parse_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_precision buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_death_type buf

let parse_relation_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_relation_type buf

let parse_create_or_link ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _create_or_link_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_create_or_link buf

let parse_fevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _fevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_fevent_name buf

let parse_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_relation_parent_type buf

let parse_pevent_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _pevent_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_pevent_name buf

let parse_access ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _access_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_access buf

let parse_update_warning_js ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _update_warning_js_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_update_warning_js buf

let parse_person_or_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_or_family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_person_or_family buf

let parse_auto_complete_place_field ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _auto_complete_place_field_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_auto_complete_place_field buf

let parse_auto_complete_field ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _auto_complete_field_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_auto_complete_field buf

let parse_short_greg_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _short_greg_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_short_greg_month buf

let parse_french_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _french_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_french_month buf

let parse_hebrew_month ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _hebrew_month_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_saisie_write_piqi.parse_hebrew_month buf


let gen_dmy ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _dmy_piqi_type `pb format x_pb ?opts

let gen_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _date_piqi_type `pb format x_pb ?opts

let gen_person_search ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_search_piqi_type `pb format x_pb ?opts

let gen_simple_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_simple_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _simple_person_piqi_type `pb format x_pb ?opts

let gen_witness_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_event_piqi_type `pb format x_pb ?opts

let gen_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_piqi_type `pb format x_pb ?opts

let gen_relation_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_person_piqi_type `pb format x_pb ?opts

let gen_was_witness ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_was_witness x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _was_witness_piqi_type `pb format x_pb ?opts

let gen_person_search_info ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_info x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_search_info_piqi_type `pb format x_pb ?opts

let gen_person_link ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_link x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_link_piqi_type `pb format x_pb ?opts

let gen_witness ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_piqi_type `pb format x_pb ?opts

let gen_fevent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_fevent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fevent_piqi_type `pb format x_pb ?opts

let gen_relation_parent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_pevent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_pevent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _pevent_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_piqi_type `pb format x_pb ?opts

let gen_create_conflict ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_create_conflict x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _create_conflict_piqi_type `pb format x_pb ?opts

let gen_modification_status ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _modification_status_piqi_type `pb format x_pb ?opts

let gen_index_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_index_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_person_piqi_type `pb format x_pb ?opts

let gen_index_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_index_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_family_piqi_type `pb format x_pb ?opts

let gen_index_person_and_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_index_person_and_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _index_person_and_family_piqi_type `pb format x_pb ?opts

let gen_family_spouse ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_family_spouse x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_spouse_piqi_type `pb format x_pb ?opts

let gen_add_child_request ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child_request x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_child_request_piqi_type `pb format x_pb ?opts

let gen_add_child ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_child_piqi_type `pb format x_pb ?opts

let gen_add_child_ok ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_child_ok x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_child_ok_piqi_type `pb format x_pb ?opts

let gen_add_parents ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_parents x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_parents_piqi_type `pb format x_pb ?opts

let gen_add_parents_ok ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_parents_ok x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_parents_ok_piqi_type `pb format x_pb ?opts

let gen_add_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_family_piqi_type `pb format x_pb ?opts

let gen_add_family_ok ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_family_ok x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_family_ok_piqi_type `pb format x_pb ?opts

let gen_edit_family_request ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family_request x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edit_family_request_piqi_type `pb format x_pb ?opts

let gen_edit_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edit_family_piqi_type `pb format x_pb ?opts

let gen_edit_family_ok ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_edit_family_ok x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _edit_family_ok_piqi_type `pb format x_pb ?opts

let gen_add_sibling_request ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling_request x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_sibling_request_piqi_type `pb format x_pb ?opts

let gen_add_sibling ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_sibling_piqi_type `pb format x_pb ?opts

let gen_add_sibling_ok ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_sibling_ok x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_sibling_ok_piqi_type `pb format x_pb ?opts

let gen_add_first_fam ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_add_first_fam x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _add_first_fam_piqi_type `pb format x_pb ?opts

let gen_auto_complete ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _auto_complete_piqi_type `pb format x_pb ?opts

let gen_auto_complete_result ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_result x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _auto_complete_result_piqi_type `pb format x_pb ?opts

let gen_person_search_list_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_list_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_search_list_params_piqi_type `pb format x_pb ?opts

let gen_person_search_list ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_search_list x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_search_list_piqi_type `pb format x_pb ?opts

let gen_transl_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_calendar_piqi_type `pb format x_pb ?opts

let gen_config_transl_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_calendar_piqi_type `pb format x_pb ?opts

let gen_transl_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_witness_type_piqi_type `pb format x_pb ?opts

let gen_config_transl_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_witness_type_piqi_type `pb format x_pb ?opts

let gen_transl_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_precision_piqi_type `pb format x_pb ?opts

let gen_config_transl_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_precision_piqi_type `pb format x_pb ?opts

let gen_transl_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_death_type_piqi_type `pb format x_pb ?opts

let gen_config_transl_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_death_type_piqi_type `pb format x_pb ?opts

let gen_transl_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_config_transl_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_transl_fevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_fevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_fevent_name_piqi_type `pb format x_pb ?opts

let gen_config_transl_fevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_fevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_fevent_name_piqi_type `pb format x_pb ?opts

let gen_transl_pevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_pevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_pevent_name_piqi_type `pb format x_pb ?opts

let gen_config_transl_pevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_pevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_pevent_name_piqi_type `pb format x_pb ?opts

let gen_transl_access ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_access x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_access_piqi_type `pb format x_pb ?opts

let gen_config_transl_access ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_access x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_access_piqi_type `pb format x_pb ?opts

let gen_transl_update_warning_js ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_update_warning_js x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_update_warning_js_piqi_type `pb format x_pb ?opts

let gen_config_transl_update_warning_js ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_update_warning_js x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_update_warning_js_piqi_type `pb format x_pb ?opts

let gen_transl_short_greg_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_short_greg_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_short_greg_month_piqi_type `pb format x_pb ?opts

let gen_config_transl_short_greg_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_short_greg_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_short_greg_month_piqi_type `pb format x_pb ?opts

let gen_transl_french_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_french_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_french_month_piqi_type `pb format x_pb ?opts

let gen_config_transl_french_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_french_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_french_month_piqi_type `pb format x_pb ?opts

let gen_transl_hebrew_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_transl_hebrew_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _transl_hebrew_month_piqi_type `pb format x_pb ?opts

let gen_config_transl_hebrew_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config_transl_hebrew_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_transl_hebrew_month_piqi_type `pb format x_pb ?opts

let gen_config ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_config x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _config_piqi_type `pb format x_pb ?opts

let gen_sosa ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_sosa x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sosa_piqi_type `pb format x_pb ?opts

let gen_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _calendar_piqi_type `pb format x_pb ?opts

let gen_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_type_piqi_type `pb format x_pb ?opts

let gen_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _precision_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_relation_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_type_piqi_type `pb format x_pb ?opts

let gen_create_or_link ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_create_or_link x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _create_or_link_piqi_type `pb format x_pb ?opts

let gen_fevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_fevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _fevent_name_piqi_type `pb format x_pb ?opts

let gen_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_pevent_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_pevent_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _pevent_name_piqi_type `pb format x_pb ?opts

let gen_access ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_access x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _access_piqi_type `pb format x_pb ?opts

let gen_update_warning_js ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_update_warning_js x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _update_warning_js_piqi_type `pb format x_pb ?opts

let gen_person_or_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_person_or_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_or_family_piqi_type `pb format x_pb ?opts

let gen_auto_complete_place_field ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_place_field x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _auto_complete_place_field_piqi_type `pb format x_pb ?opts

let gen_auto_complete_field ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_auto_complete_field x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _auto_complete_field_piqi_type `pb format x_pb ?opts

let gen_short_greg_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_short_greg_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _short_greg_month_piqi_type `pb format x_pb ?opts

let gen_french_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_french_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _french_month_piqi_type `pb format x_pb ?opts

let gen_hebrew_month ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_saisie_write_piqi.gen_hebrew_month x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _hebrew_month_piqi_type `pb format x_pb ?opts


let print_dmy ?opts x =
  Pervasives.print_endline (gen_dmy x `piq ?opts)
let prerr_dmy ?opts x =
  Pervasives.prerr_endline (gen_dmy x `piq ?opts)

let print_date ?opts x =
  Pervasives.print_endline (gen_date x `piq ?opts)
let prerr_date ?opts x =
  Pervasives.prerr_endline (gen_date x `piq ?opts)

let print_person_search ?opts x =
  Pervasives.print_endline (gen_person_search x `piq ?opts)
let prerr_person_search ?opts x =
  Pervasives.prerr_endline (gen_person_search x `piq ?opts)

let print_simple_person ?opts x =
  Pervasives.print_endline (gen_simple_person x `piq ?opts)
let prerr_simple_person ?opts x =
  Pervasives.prerr_endline (gen_simple_person x `piq ?opts)

let print_witness_event ?opts x =
  Pervasives.print_endline (gen_witness_event x `piq ?opts)
let prerr_witness_event ?opts x =
  Pervasives.prerr_endline (gen_witness_event x `piq ?opts)

let print_event ?opts x =
  Pervasives.print_endline (gen_event x `piq ?opts)
let prerr_event ?opts x =
  Pervasives.prerr_endline (gen_event x `piq ?opts)

let print_relation_person ?opts x =
  Pervasives.print_endline (gen_relation_person x `piq ?opts)
let prerr_relation_person ?opts x =
  Pervasives.prerr_endline (gen_relation_person x `piq ?opts)

let print_was_witness ?opts x =
  Pervasives.print_endline (gen_was_witness x `piq ?opts)
let prerr_was_witness ?opts x =
  Pervasives.prerr_endline (gen_was_witness x `piq ?opts)

let print_person_search_info ?opts x =
  Pervasives.print_endline (gen_person_search_info x `piq ?opts)
let prerr_person_search_info ?opts x =
  Pervasives.prerr_endline (gen_person_search_info x `piq ?opts)

let print_person_link ?opts x =
  Pervasives.print_endline (gen_person_link x `piq ?opts)
let prerr_person_link ?opts x =
  Pervasives.prerr_endline (gen_person_link x `piq ?opts)

let print_witness ?opts x =
  Pervasives.print_endline (gen_witness x `piq ?opts)
let prerr_witness ?opts x =
  Pervasives.prerr_endline (gen_witness x `piq ?opts)

let print_fevent ?opts x =
  Pervasives.print_endline (gen_fevent x `piq ?opts)
let prerr_fevent ?opts x =
  Pervasives.prerr_endline (gen_fevent x `piq ?opts)

let print_relation_parent ?opts x =
  Pervasives.print_endline (gen_relation_parent x `piq ?opts)
let prerr_relation_parent ?opts x =
  Pervasives.prerr_endline (gen_relation_parent x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_pevent ?opts x =
  Pervasives.print_endline (gen_pevent x `piq ?opts)
let prerr_pevent ?opts x =
  Pervasives.prerr_endline (gen_pevent x `piq ?opts)

let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_family ?opts x =
  Pervasives.print_endline (gen_family x `piq ?opts)
let prerr_family ?opts x =
  Pervasives.prerr_endline (gen_family x `piq ?opts)

let print_create_conflict ?opts x =
  Pervasives.print_endline (gen_create_conflict x `piq ?opts)
let prerr_create_conflict ?opts x =
  Pervasives.prerr_endline (gen_create_conflict x `piq ?opts)

let print_modification_status ?opts x =
  Pervasives.print_endline (gen_modification_status x `piq ?opts)
let prerr_modification_status ?opts x =
  Pervasives.prerr_endline (gen_modification_status x `piq ?opts)

let print_index_person ?opts x =
  Pervasives.print_endline (gen_index_person x `piq ?opts)
let prerr_index_person ?opts x =
  Pervasives.prerr_endline (gen_index_person x `piq ?opts)

let print_index_family ?opts x =
  Pervasives.print_endline (gen_index_family x `piq ?opts)
let prerr_index_family ?opts x =
  Pervasives.prerr_endline (gen_index_family x `piq ?opts)

let print_index_person_and_family ?opts x =
  Pervasives.print_endline (gen_index_person_and_family x `piq ?opts)
let prerr_index_person_and_family ?opts x =
  Pervasives.prerr_endline (gen_index_person_and_family x `piq ?opts)

let print_family_spouse ?opts x =
  Pervasives.print_endline (gen_family_spouse x `piq ?opts)
let prerr_family_spouse ?opts x =
  Pervasives.prerr_endline (gen_family_spouse x `piq ?opts)

let print_add_child_request ?opts x =
  Pervasives.print_endline (gen_add_child_request x `piq ?opts)
let prerr_add_child_request ?opts x =
  Pervasives.prerr_endline (gen_add_child_request x `piq ?opts)

let print_add_child ?opts x =
  Pervasives.print_endline (gen_add_child x `piq ?opts)
let prerr_add_child ?opts x =
  Pervasives.prerr_endline (gen_add_child x `piq ?opts)

let print_add_child_ok ?opts x =
  Pervasives.print_endline (gen_add_child_ok x `piq ?opts)
let prerr_add_child_ok ?opts x =
  Pervasives.prerr_endline (gen_add_child_ok x `piq ?opts)

let print_add_parents ?opts x =
  Pervasives.print_endline (gen_add_parents x `piq ?opts)
let prerr_add_parents ?opts x =
  Pervasives.prerr_endline (gen_add_parents x `piq ?opts)

let print_add_parents_ok ?opts x =
  Pervasives.print_endline (gen_add_parents_ok x `piq ?opts)
let prerr_add_parents_ok ?opts x =
  Pervasives.prerr_endline (gen_add_parents_ok x `piq ?opts)

let print_add_family ?opts x =
  Pervasives.print_endline (gen_add_family x `piq ?opts)
let prerr_add_family ?opts x =
  Pervasives.prerr_endline (gen_add_family x `piq ?opts)

let print_add_family_ok ?opts x =
  Pervasives.print_endline (gen_add_family_ok x `piq ?opts)
let prerr_add_family_ok ?opts x =
  Pervasives.prerr_endline (gen_add_family_ok x `piq ?opts)

let print_edit_family_request ?opts x =
  Pervasives.print_endline (gen_edit_family_request x `piq ?opts)
let prerr_edit_family_request ?opts x =
  Pervasives.prerr_endline (gen_edit_family_request x `piq ?opts)

let print_edit_family ?opts x =
  Pervasives.print_endline (gen_edit_family x `piq ?opts)
let prerr_edit_family ?opts x =
  Pervasives.prerr_endline (gen_edit_family x `piq ?opts)

let print_edit_family_ok ?opts x =
  Pervasives.print_endline (gen_edit_family_ok x `piq ?opts)
let prerr_edit_family_ok ?opts x =
  Pervasives.prerr_endline (gen_edit_family_ok x `piq ?opts)

let print_add_sibling_request ?opts x =
  Pervasives.print_endline (gen_add_sibling_request x `piq ?opts)
let prerr_add_sibling_request ?opts x =
  Pervasives.prerr_endline (gen_add_sibling_request x `piq ?opts)

let print_add_sibling ?opts x =
  Pervasives.print_endline (gen_add_sibling x `piq ?opts)
let prerr_add_sibling ?opts x =
  Pervasives.prerr_endline (gen_add_sibling x `piq ?opts)

let print_add_sibling_ok ?opts x =
  Pervasives.print_endline (gen_add_sibling_ok x `piq ?opts)
let prerr_add_sibling_ok ?opts x =
  Pervasives.prerr_endline (gen_add_sibling_ok x `piq ?opts)

let print_add_first_fam ?opts x =
  Pervasives.print_endline (gen_add_first_fam x `piq ?opts)
let prerr_add_first_fam ?opts x =
  Pervasives.prerr_endline (gen_add_first_fam x `piq ?opts)

let print_auto_complete ?opts x =
  Pervasives.print_endline (gen_auto_complete x `piq ?opts)
let prerr_auto_complete ?opts x =
  Pervasives.prerr_endline (gen_auto_complete x `piq ?opts)

let print_auto_complete_result ?opts x =
  Pervasives.print_endline (gen_auto_complete_result x `piq ?opts)
let prerr_auto_complete_result ?opts x =
  Pervasives.prerr_endline (gen_auto_complete_result x `piq ?opts)

let print_person_search_list_params ?opts x =
  Pervasives.print_endline (gen_person_search_list_params x `piq ?opts)
let prerr_person_search_list_params ?opts x =
  Pervasives.prerr_endline (gen_person_search_list_params x `piq ?opts)

let print_person_search_list ?opts x =
  Pervasives.print_endline (gen_person_search_list x `piq ?opts)
let prerr_person_search_list ?opts x =
  Pervasives.prerr_endline (gen_person_search_list x `piq ?opts)

let print_transl_calendar ?opts x =
  Pervasives.print_endline (gen_transl_calendar x `piq ?opts)
let prerr_transl_calendar ?opts x =
  Pervasives.prerr_endline (gen_transl_calendar x `piq ?opts)

let print_config_transl_calendar ?opts x =
  Pervasives.print_endline (gen_config_transl_calendar x `piq ?opts)
let prerr_config_transl_calendar ?opts x =
  Pervasives.prerr_endline (gen_config_transl_calendar x `piq ?opts)

let print_transl_witness_type ?opts x =
  Pervasives.print_endline (gen_transl_witness_type x `piq ?opts)
let prerr_transl_witness_type ?opts x =
  Pervasives.prerr_endline (gen_transl_witness_type x `piq ?opts)

let print_config_transl_witness_type ?opts x =
  Pervasives.print_endline (gen_config_transl_witness_type x `piq ?opts)
let prerr_config_transl_witness_type ?opts x =
  Pervasives.prerr_endline (gen_config_transl_witness_type x `piq ?opts)

let print_transl_precision ?opts x =
  Pervasives.print_endline (gen_transl_precision x `piq ?opts)
let prerr_transl_precision ?opts x =
  Pervasives.prerr_endline (gen_transl_precision x `piq ?opts)

let print_config_transl_precision ?opts x =
  Pervasives.print_endline (gen_config_transl_precision x `piq ?opts)
let prerr_config_transl_precision ?opts x =
  Pervasives.prerr_endline (gen_config_transl_precision x `piq ?opts)

let print_transl_death_type ?opts x =
  Pervasives.print_endline (gen_transl_death_type x `piq ?opts)
let prerr_transl_death_type ?opts x =
  Pervasives.prerr_endline (gen_transl_death_type x `piq ?opts)

let print_config_transl_death_type ?opts x =
  Pervasives.print_endline (gen_config_transl_death_type x `piq ?opts)
let prerr_config_transl_death_type ?opts x =
  Pervasives.prerr_endline (gen_config_transl_death_type x `piq ?opts)

let print_transl_relation_parent_type ?opts x =
  Pervasives.print_endline (gen_transl_relation_parent_type x `piq ?opts)
let prerr_transl_relation_parent_type ?opts x =
  Pervasives.prerr_endline (gen_transl_relation_parent_type x `piq ?opts)

let print_config_transl_relation_parent_type ?opts x =
  Pervasives.print_endline (gen_config_transl_relation_parent_type x `piq ?opts)
let prerr_config_transl_relation_parent_type ?opts x =
  Pervasives.prerr_endline (gen_config_transl_relation_parent_type x `piq ?opts)

let print_transl_fevent_name ?opts x =
  Pervasives.print_endline (gen_transl_fevent_name x `piq ?opts)
let prerr_transl_fevent_name ?opts x =
  Pervasives.prerr_endline (gen_transl_fevent_name x `piq ?opts)

let print_config_transl_fevent_name ?opts x =
  Pervasives.print_endline (gen_config_transl_fevent_name x `piq ?opts)
let prerr_config_transl_fevent_name ?opts x =
  Pervasives.prerr_endline (gen_config_transl_fevent_name x `piq ?opts)

let print_transl_pevent_name ?opts x =
  Pervasives.print_endline (gen_transl_pevent_name x `piq ?opts)
let prerr_transl_pevent_name ?opts x =
  Pervasives.prerr_endline (gen_transl_pevent_name x `piq ?opts)

let print_config_transl_pevent_name ?opts x =
  Pervasives.print_endline (gen_config_transl_pevent_name x `piq ?opts)
let prerr_config_transl_pevent_name ?opts x =
  Pervasives.prerr_endline (gen_config_transl_pevent_name x `piq ?opts)

let print_transl_access ?opts x =
  Pervasives.print_endline (gen_transl_access x `piq ?opts)
let prerr_transl_access ?opts x =
  Pervasives.prerr_endline (gen_transl_access x `piq ?opts)

let print_config_transl_access ?opts x =
  Pervasives.print_endline (gen_config_transl_access x `piq ?opts)
let prerr_config_transl_access ?opts x =
  Pervasives.prerr_endline (gen_config_transl_access x `piq ?opts)

let print_transl_update_warning_js ?opts x =
  Pervasives.print_endline (gen_transl_update_warning_js x `piq ?opts)
let prerr_transl_update_warning_js ?opts x =
  Pervasives.prerr_endline (gen_transl_update_warning_js x `piq ?opts)

let print_config_transl_update_warning_js ?opts x =
  Pervasives.print_endline (gen_config_transl_update_warning_js x `piq ?opts)
let prerr_config_transl_update_warning_js ?opts x =
  Pervasives.prerr_endline (gen_config_transl_update_warning_js x `piq ?opts)

let print_transl_short_greg_month ?opts x =
  Pervasives.print_endline (gen_transl_short_greg_month x `piq ?opts)
let prerr_transl_short_greg_month ?opts x =
  Pervasives.prerr_endline (gen_transl_short_greg_month x `piq ?opts)

let print_config_transl_short_greg_month ?opts x =
  Pervasives.print_endline (gen_config_transl_short_greg_month x `piq ?opts)
let prerr_config_transl_short_greg_month ?opts x =
  Pervasives.prerr_endline (gen_config_transl_short_greg_month x `piq ?opts)

let print_transl_french_month ?opts x =
  Pervasives.print_endline (gen_transl_french_month x `piq ?opts)
let prerr_transl_french_month ?opts x =
  Pervasives.prerr_endline (gen_transl_french_month x `piq ?opts)

let print_config_transl_french_month ?opts x =
  Pervasives.print_endline (gen_config_transl_french_month x `piq ?opts)
let prerr_config_transl_french_month ?opts x =
  Pervasives.prerr_endline (gen_config_transl_french_month x `piq ?opts)

let print_transl_hebrew_month ?opts x =
  Pervasives.print_endline (gen_transl_hebrew_month x `piq ?opts)
let prerr_transl_hebrew_month ?opts x =
  Pervasives.prerr_endline (gen_transl_hebrew_month x `piq ?opts)

let print_config_transl_hebrew_month ?opts x =
  Pervasives.print_endline (gen_config_transl_hebrew_month x `piq ?opts)
let prerr_config_transl_hebrew_month ?opts x =
  Pervasives.prerr_endline (gen_config_transl_hebrew_month x `piq ?opts)

let print_config ?opts x =
  Pervasives.print_endline (gen_config x `piq ?opts)
let prerr_config ?opts x =
  Pervasives.prerr_endline (gen_config x `piq ?opts)

let print_sosa ?opts x =
  Pervasives.print_endline (gen_sosa x `piq ?opts)
let prerr_sosa ?opts x =
  Pervasives.prerr_endline (gen_sosa x `piq ?opts)

let print_calendar ?opts x =
  Pervasives.print_endline (gen_calendar x `piq ?opts)
let prerr_calendar ?opts x =
  Pervasives.prerr_endline (gen_calendar x `piq ?opts)

let print_witness_type ?opts x =
  Pervasives.print_endline (gen_witness_type x `piq ?opts)
let prerr_witness_type ?opts x =
  Pervasives.prerr_endline (gen_witness_type x `piq ?opts)

let print_precision ?opts x =
  Pervasives.print_endline (gen_precision x `piq ?opts)
let prerr_precision ?opts x =
  Pervasives.prerr_endline (gen_precision x `piq ?opts)

let print_sex ?opts x =
  Pervasives.print_endline (gen_sex x `piq ?opts)
let prerr_sex ?opts x =
  Pervasives.prerr_endline (gen_sex x `piq ?opts)

let print_death_type ?opts x =
  Pervasives.print_endline (gen_death_type x `piq ?opts)
let prerr_death_type ?opts x =
  Pervasives.prerr_endline (gen_death_type x `piq ?opts)

let print_relation_type ?opts x =
  Pervasives.print_endline (gen_relation_type x `piq ?opts)
let prerr_relation_type ?opts x =
  Pervasives.prerr_endline (gen_relation_type x `piq ?opts)

let print_create_or_link ?opts x =
  Pervasives.print_endline (gen_create_or_link x `piq ?opts)
let prerr_create_or_link ?opts x =
  Pervasives.prerr_endline (gen_create_or_link x `piq ?opts)

let print_fevent_name ?opts x =
  Pervasives.print_endline (gen_fevent_name x `piq ?opts)
let prerr_fevent_name ?opts x =
  Pervasives.prerr_endline (gen_fevent_name x `piq ?opts)

let print_relation_parent_type ?opts x =
  Pervasives.print_endline (gen_relation_parent_type x `piq ?opts)
let prerr_relation_parent_type ?opts x =
  Pervasives.prerr_endline (gen_relation_parent_type x `piq ?opts)

let print_pevent_name ?opts x =
  Pervasives.print_endline (gen_pevent_name x `piq ?opts)
let prerr_pevent_name ?opts x =
  Pervasives.prerr_endline (gen_pevent_name x `piq ?opts)

let print_access ?opts x =
  Pervasives.print_endline (gen_access x `piq ?opts)
let prerr_access ?opts x =
  Pervasives.prerr_endline (gen_access x `piq ?opts)

let print_update_warning_js ?opts x =
  Pervasives.print_endline (gen_update_warning_js x `piq ?opts)
let prerr_update_warning_js ?opts x =
  Pervasives.prerr_endline (gen_update_warning_js x `piq ?opts)

let print_person_or_family ?opts x =
  Pervasives.print_endline (gen_person_or_family x `piq ?opts)
let prerr_person_or_family ?opts x =
  Pervasives.prerr_endline (gen_person_or_family x `piq ?opts)

let print_auto_complete_place_field ?opts x =
  Pervasives.print_endline (gen_auto_complete_place_field x `piq ?opts)
let prerr_auto_complete_place_field ?opts x =
  Pervasives.prerr_endline (gen_auto_complete_place_field x `piq ?opts)

let print_auto_complete_field ?opts x =
  Pervasives.print_endline (gen_auto_complete_field x `piq ?opts)
let prerr_auto_complete_field ?opts x =
  Pervasives.prerr_endline (gen_auto_complete_field x `piq ?opts)

let print_short_greg_month ?opts x =
  Pervasives.print_endline (gen_short_greg_month x `piq ?opts)
let prerr_short_greg_month ?opts x =
  Pervasives.prerr_endline (gen_short_greg_month x `piq ?opts)

let print_french_month ?opts x =
  Pervasives.print_endline (gen_french_month x `piq ?opts)
let prerr_french_month ?opts x =
  Pervasives.prerr_endline (gen_french_month x `piq ?opts)

let print_hebrew_month ?opts x =
  Pervasives.print_endline (gen_hebrew_month x `piq ?opts)
let prerr_hebrew_month ?opts x =
  Pervasives.prerr_endline (gen_hebrew_month x `piq ?opts)


