let piqi = Api_app_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _dmy_piqi_type = Piqirun_ext.find_piqi_type "api_app/dmy"
let _date_piqi_type = Piqirun_ext.find_piqi_type "api_app/date"
let _relation_parent_piqi_type = Piqirun_ext.find_piqi_type "api_app/relation-parent"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api_app/title"
let _witness_event_piqi_type = Piqirun_ext.find_piqi_type "api_app/witness-event"
let _event_piqi_type = Piqirun_ext.find_piqi_type "api_app/event"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api_app/person"
let _family_piqi_type = Piqirun_ext.find_piqi_type "api_app/family"
let _base_warnings_piqi_type = Piqirun_ext.find_piqi_type "api_app/base-warnings"
let _modification_status_piqi_type = Piqirun_ext.find_piqi_type "api_app/modification-status"
let _calendar_piqi_type = Piqirun_ext.find_piqi_type "api_app/calendar"
let _precision_piqi_type = Piqirun_ext.find_piqi_type "api_app/precision"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api_app/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/death-type"
let _marriage_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/marriage-type"
let _divorce_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/divorce-type"
let _relation_parent_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/relation-parent-type"
let _title_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/title-type"
let _access_piqi_type = Piqirun_ext.find_piqi_type "api_app/access"
let _event_name_piqi_type = Piqirun_ext.find_piqi_type "api_app/event-name"
let _witness_type_piqi_type = Piqirun_ext.find_piqi_type "api_app/witness-type"


let parse_dmy ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_dmy buf

let parse_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_date buf

let parse_relation_parent ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_relation_parent buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_title buf

let parse_witness_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_witness_event buf

let parse_event ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_event buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_person buf

let parse_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_family buf

let parse_base_warnings ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _base_warnings_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_base_warnings buf

let parse_modification_status ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _modification_status_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_modification_status buf

let parse_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_calendar buf

let parse_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_precision buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_death_type buf

let parse_marriage_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_marriage_type buf

let parse_divorce_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_divorce_type buf

let parse_relation_parent_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_parent_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_relation_parent_type buf

let parse_title_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_title_type buf

let parse_access ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _access_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_access buf

let parse_event_name ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_name_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_event_name buf

let parse_witness_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_app_piqi.parse_witness_type buf


let gen_dmy ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _dmy_piqi_type `pb format x_pb ?opts

let gen_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _date_piqi_type `pb format x_pb ?opts

let gen_relation_parent ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_witness_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_event_piqi_type `pb format x_pb ?opts

let gen_event ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_piqi_type `pb format x_pb ?opts

let gen_base_warnings ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_base_warnings x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _base_warnings_piqi_type `pb format x_pb ?opts

let gen_modification_status ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _modification_status_piqi_type `pb format x_pb ?opts

let gen_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _calendar_piqi_type `pb format x_pb ?opts

let gen_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _precision_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_marriage_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _marriage_type_piqi_type `pb format x_pb ?opts

let gen_divorce_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _divorce_type_piqi_type `pb format x_pb ?opts

let gen_relation_parent_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _relation_parent_type_piqi_type `pb format x_pb ?opts

let gen_title_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_type_piqi_type `pb format x_pb ?opts

let gen_access ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_access x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _access_piqi_type `pb format x_pb ?opts

let gen_event_name ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_event_name x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _event_name_piqi_type `pb format x_pb ?opts

let gen_witness_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _witness_type_piqi_type `pb format x_pb ?opts


let print_dmy ?opts x =
  Pervasives.print_endline (gen_dmy x `piq ?opts)
let prerr_dmy ?opts x =
  Pervasives.prerr_endline (gen_dmy x `piq ?opts)

let print_date ?opts x =
  Pervasives.print_endline (gen_date x `piq ?opts)
let prerr_date ?opts x =
  Pervasives.prerr_endline (gen_date x `piq ?opts)

let print_relation_parent ?opts x =
  Pervasives.print_endline (gen_relation_parent x `piq ?opts)
let prerr_relation_parent ?opts x =
  Pervasives.prerr_endline (gen_relation_parent x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_witness_event ?opts x =
  Pervasives.print_endline (gen_witness_event x `piq ?opts)
let prerr_witness_event ?opts x =
  Pervasives.prerr_endline (gen_witness_event x `piq ?opts)

let print_event ?opts x =
  Pervasives.print_endline (gen_event x `piq ?opts)
let prerr_event ?opts x =
  Pervasives.prerr_endline (gen_event x `piq ?opts)

let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_family ?opts x =
  Pervasives.print_endline (gen_family x `piq ?opts)
let prerr_family ?opts x =
  Pervasives.prerr_endline (gen_family x `piq ?opts)

let print_base_warnings ?opts x =
  Pervasives.print_endline (gen_base_warnings x `piq ?opts)
let prerr_base_warnings ?opts x =
  Pervasives.prerr_endline (gen_base_warnings x `piq ?opts)

let print_modification_status ?opts x =
  Pervasives.print_endline (gen_modification_status x `piq ?opts)
let prerr_modification_status ?opts x =
  Pervasives.prerr_endline (gen_modification_status x `piq ?opts)

let print_calendar ?opts x =
  Pervasives.print_endline (gen_calendar x `piq ?opts)
let prerr_calendar ?opts x =
  Pervasives.prerr_endline (gen_calendar x `piq ?opts)

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

let print_access ?opts x =
  Pervasives.print_endline (gen_access x `piq ?opts)
let prerr_access ?opts x =
  Pervasives.prerr_endline (gen_access x `piq ?opts)

let print_event_name ?opts x =
  Pervasives.print_endline (gen_event_name x `piq ?opts)
let prerr_event_name ?opts x =
  Pervasives.prerr_endline (gen_event_name x `piq ?opts)

let print_witness_type ?opts x =
  Pervasives.print_endline (gen_witness_type x `piq ?opts)
let prerr_witness_type ?opts x =
  Pervasives.prerr_endline (gen_witness_type x `piq ?opts)


