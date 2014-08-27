let piqi = Api_app_piqi.piqi
  
let _ = Piqirun_ext.init_piqi piqi
  
let _int32_piqtype = Piqirun_ext.find_piqtype "int32"
  
let _protobuf_int32_piqtype = Piqirun_ext.find_piqtype "protobuf-int32"
  
let _string_piqtype = Piqirun_ext.find_piqtype "string"
  
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
  
let _dmy_piqtype = Piqirun_ext.find_piqtype "api_app/dmy"
  
let _date_piqtype = Piqirun_ext.find_piqtype "api_app/date"
  
let _relation_parent_piqtype =
  Piqirun_ext.find_piqtype "api_app/relation-parent"
  
let _title_piqtype = Piqirun_ext.find_piqtype "api_app/title"
  
let _witness_event_piqtype = Piqirun_ext.find_piqtype "api_app/witness-event"
  
let _event_piqtype = Piqirun_ext.find_piqtype "api_app/event"
  
let _person_piqtype = Piqirun_ext.find_piqtype "api_app/person"
  
let _family_piqtype = Piqirun_ext.find_piqtype "api_app/family"
  
let _base_warnings_piqtype = Piqirun_ext.find_piqtype "api_app/base-warnings"
  
let _modification_status_piqtype =
  Piqirun_ext.find_piqtype "api_app/modification-status"
  
let _calendar_piqtype = Piqirun_ext.find_piqtype "api_app/calendar"
  
let _precision_piqtype = Piqirun_ext.find_piqtype "api_app/precision"
  
let _sex_piqtype = Piqirun_ext.find_piqtype "api_app/sex"
  
let _death_type_piqtype = Piqirun_ext.find_piqtype "api_app/death-type"
  
let _marriage_type_piqtype = Piqirun_ext.find_piqtype "api_app/marriage-type"
  
let _divorce_type_piqtype = Piqirun_ext.find_piqtype "api_app/divorce-type"
  
let _relation_parent_type_piqtype =
  Piqirun_ext.find_piqtype "api_app/relation-parent-type"
  
let _title_type_piqtype = Piqirun_ext.find_piqtype "api_app/title-type"
  
let _access_piqtype = Piqirun_ext.find_piqtype "api_app/access"
  
let _event_name_piqtype = Piqirun_ext.find_piqtype "api_app/event-name"
  
let _witness_type_piqtype = Piqirun_ext.find_piqtype "api_app/witness-type"
  
let parse_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_int32 buf
  
let parse_protobuf_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_protobuf_int32 buf
  
let parse_string ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_string buf
  
let parse_bool ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_bool buf
  
let parse_dmy ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_dmy buf
  
let parse_date ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_date buf
  
let parse_relation_parent ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_relation_parent buf
  
let parse_title ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_title buf
  
let parse_witness_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_witness_event buf
  
let parse_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_event buf
  
let parse_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_person buf
  
let parse_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_family buf
  
let parse_base_warnings ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _base_warnings_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_base_warnings buf
  
let parse_modification_status ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _modification_status_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_modification_status buf
  
let parse_calendar ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_calendar buf
  
let parse_precision ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_precision buf
  
let parse_sex ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_sex buf
  
let parse_death_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_death_type buf
  
let parse_marriage_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_marriage_type buf
  
let parse_divorce_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_divorce_type buf
  
let parse_relation_parent_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_parent_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_relation_parent_type buf
  
let parse_title_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_title_type buf
  
let parse_access ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _access_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in Api_app_piqi.parse_access buf
  
let parse_event_name ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_name_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_event_name buf
  
let parse_witness_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_app_piqi.parse_witness_type buf
  
let gen_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int32_piqtype `pb format x_pb ?opts
  
let gen_protobuf_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_protobuf_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int32_piqtype `pb format x_pb ?opts
  
let gen_string ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_string x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
  
let gen_bool ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_bool x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
  
let gen_dmy ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _dmy_piqtype `pb format x_pb ?opts
  
let gen_date ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _date_piqtype `pb format x_pb ?opts
  
let gen_relation_parent ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_relation_parent x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_piqtype `pb format x_pb ?opts
  
let gen_title ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _title_piqtype `pb format x_pb ?opts
  
let gen_witness_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_event_piqtype `pb format x_pb ?opts
  
let gen_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _event_piqtype `pb format x_pb ?opts
  
let gen_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_piqtype `pb format x_pb ?opts
  
let gen_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _family_piqtype `pb format x_pb ?opts
  
let gen_base_warnings ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_base_warnings x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _base_warnings_piqtype `pb format x_pb ?opts
  
let gen_modification_status ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_modification_status x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _modification_status_piqtype `pb format x_pb ?opts
  
let gen_calendar ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _calendar_piqtype `pb format x_pb ?opts
  
let gen_precision ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _precision_piqtype `pb format x_pb ?opts
  
let gen_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sex_piqtype `pb format x_pb ?opts
  
let gen_death_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _death_type_piqtype `pb format x_pb ?opts
  
let gen_marriage_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _marriage_type_piqtype `pb format x_pb ?opts
  
let gen_divorce_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _divorce_type_piqtype `pb format x_pb ?opts
  
let gen_relation_parent_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_relation_parent_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_parent_type_piqtype `pb format x_pb ?opts
  
let gen_title_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _title_type_piqtype `pb format x_pb ?opts
  
let gen_access ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_access x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _access_piqtype `pb format x_pb ?opts
  
let gen_event_name ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_event_name x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _event_name_piqtype `pb format x_pb ?opts
  
let gen_witness_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_app_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_type_piqtype `pb format x_pb ?opts
  
let print_int32 x = Pervasives.print_endline (gen_int32 x `piq)
  
let prerr_int32 x = Pervasives.prerr_endline (gen_int32 x `piq)
  
let print_protobuf_int32 x =
  Pervasives.print_endline (gen_protobuf_int32 x `piq)
  
let prerr_protobuf_int32 x =
  Pervasives.prerr_endline (gen_protobuf_int32 x `piq)
  
let print_string x = Pervasives.print_endline (gen_string x `piq)
  
let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
  
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
  
let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
  
let print_dmy x = Pervasives.print_endline (gen_dmy x `piq)
  
let prerr_dmy x = Pervasives.prerr_endline (gen_dmy x `piq)
  
let print_date x = Pervasives.print_endline (gen_date x `piq)
  
let prerr_date x = Pervasives.prerr_endline (gen_date x `piq)
  
let print_relation_parent x =
  Pervasives.print_endline (gen_relation_parent x `piq)
  
let prerr_relation_parent x =
  Pervasives.prerr_endline (gen_relation_parent x `piq)
  
let print_title x = Pervasives.print_endline (gen_title x `piq)
  
let prerr_title x = Pervasives.prerr_endline (gen_title x `piq)
  
let print_witness_event x =
  Pervasives.print_endline (gen_witness_event x `piq)
  
let prerr_witness_event x =
  Pervasives.prerr_endline (gen_witness_event x `piq)
  
let print_event x = Pervasives.print_endline (gen_event x `piq)
  
let prerr_event x = Pervasives.prerr_endline (gen_event x `piq)
  
let print_person x = Pervasives.print_endline (gen_person x `piq)
  
let prerr_person x = Pervasives.prerr_endline (gen_person x `piq)
  
let print_family x = Pervasives.print_endline (gen_family x `piq)
  
let prerr_family x = Pervasives.prerr_endline (gen_family x `piq)
  
let print_base_warnings x =
  Pervasives.print_endline (gen_base_warnings x `piq)
  
let prerr_base_warnings x =
  Pervasives.prerr_endline (gen_base_warnings x `piq)
  
let print_modification_status x =
  Pervasives.print_endline (gen_modification_status x `piq)
  
let prerr_modification_status x =
  Pervasives.prerr_endline (gen_modification_status x `piq)
  
let print_calendar x = Pervasives.print_endline (gen_calendar x `piq)
  
let prerr_calendar x = Pervasives.prerr_endline (gen_calendar x `piq)
  
let print_precision x = Pervasives.print_endline (gen_precision x `piq)
  
let prerr_precision x = Pervasives.prerr_endline (gen_precision x `piq)
  
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
  
let print_access x = Pervasives.print_endline (gen_access x `piq)
  
let prerr_access x = Pervasives.prerr_endline (gen_access x `piq)
  
let print_event_name x = Pervasives.print_endline (gen_event_name x `piq)
  
let prerr_event_name x = Pervasives.prerr_endline (gen_event_name x `piq)
  
let print_witness_type x = Pervasives.print_endline (gen_witness_type x `piq)
  
let prerr_witness_type x = Pervasives.prerr_endline (gen_witness_type x `piq)
  

