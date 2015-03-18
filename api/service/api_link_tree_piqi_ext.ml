let piqi = Api_link_tree_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _dmy_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/dmy"
let _date_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/date"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/title"
let _connection_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/connection"
let _family_link_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/family-link"
let _person_link_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/person-link"
let _person_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/person"
let _family_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/family"
let _link_tree_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/link-tree"
let _link_tree_params_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/link-tree-params"
let _calendar_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/calendar"
let _precision_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/precision"
let _sex_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/sex"
let _death_type_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/death-type"
let _title_type_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/title-type"
let _marriage_type_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/marriage-type"
let _divorce_type_piqi_type = Piqirun_ext.find_piqi_type "api_link_tree/divorce-type"


let parse_dmy ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_dmy buf

let parse_date ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_date buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_title buf

let parse_connection ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _connection_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_connection buf

let parse_family_link ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_link_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_family_link buf

let parse_person_link ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_link_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_person_link buf

let parse_person ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_person buf

let parse_family ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_family buf

let parse_link_tree ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _link_tree_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_link_tree buf

let parse_link_tree_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _link_tree_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_link_tree_params buf

let parse_calendar ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_calendar buf

let parse_precision ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_precision buf

let parse_sex ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_sex buf

let parse_death_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_death_type buf

let parse_title_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_title_type buf

let parse_marriage_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_marriage_type buf

let parse_divorce_type ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_link_tree_piqi.parse_divorce_type buf


let gen_dmy ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _dmy_piqi_type `pb format x_pb ?opts

let gen_date ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _date_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_connection ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_connection x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _connection_piqi_type `pb format x_pb ?opts

let gen_family_link ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_family_link x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_link_piqi_type `pb format x_pb ?opts

let gen_person_link ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_person_link x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_link_piqi_type `pb format x_pb ?opts

let gen_person ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _person_piqi_type `pb format x_pb ?opts

let gen_family ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _family_piqi_type `pb format x_pb ?opts

let gen_link_tree ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_link_tree x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _link_tree_piqi_type `pb format x_pb ?opts

let gen_link_tree_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_link_tree_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _link_tree_params_piqi_type `pb format x_pb ?opts

let gen_calendar ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _calendar_piqi_type `pb format x_pb ?opts

let gen_precision ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _precision_piqi_type `pb format x_pb ?opts

let gen_sex ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _sex_piqi_type `pb format x_pb ?opts

let gen_death_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _death_type_piqi_type `pb format x_pb ?opts

let gen_title_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_title_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_type_piqi_type `pb format x_pb ?opts

let gen_marriage_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _marriage_type_piqi_type `pb format x_pb ?opts

let gen_divorce_type ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_link_tree_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _divorce_type_piqi_type `pb format x_pb ?opts


let print_dmy ?opts x =
  Pervasives.print_endline (gen_dmy x `piq ?opts)
let prerr_dmy ?opts x =
  Pervasives.prerr_endline (gen_dmy x `piq ?opts)

let print_date ?opts x =
  Pervasives.print_endline (gen_date x `piq ?opts)
let prerr_date ?opts x =
  Pervasives.prerr_endline (gen_date x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_connection ?opts x =
  Pervasives.print_endline (gen_connection x `piq ?opts)
let prerr_connection ?opts x =
  Pervasives.prerr_endline (gen_connection x `piq ?opts)

let print_family_link ?opts x =
  Pervasives.print_endline (gen_family_link x `piq ?opts)
let prerr_family_link ?opts x =
  Pervasives.prerr_endline (gen_family_link x `piq ?opts)

let print_person_link ?opts x =
  Pervasives.print_endline (gen_person_link x `piq ?opts)
let prerr_person_link ?opts x =
  Pervasives.prerr_endline (gen_person_link x `piq ?opts)

let print_person ?opts x =
  Pervasives.print_endline (gen_person x `piq ?opts)
let prerr_person ?opts x =
  Pervasives.prerr_endline (gen_person x `piq ?opts)

let print_family ?opts x =
  Pervasives.print_endline (gen_family x `piq ?opts)
let prerr_family ?opts x =
  Pervasives.prerr_endline (gen_family x `piq ?opts)

let print_link_tree ?opts x =
  Pervasives.print_endline (gen_link_tree x `piq ?opts)
let prerr_link_tree ?opts x =
  Pervasives.prerr_endline (gen_link_tree x `piq ?opts)

let print_link_tree_params ?opts x =
  Pervasives.print_endline (gen_link_tree_params x `piq ?opts)
let prerr_link_tree_params ?opts x =
  Pervasives.prerr_endline (gen_link_tree_params x `piq ?opts)

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

let print_title_type ?opts x =
  Pervasives.print_endline (gen_title_type x `piq ?opts)
let prerr_title_type ?opts x =
  Pervasives.prerr_endline (gen_title_type x `piq ?opts)

let print_marriage_type ?opts x =
  Pervasives.print_endline (gen_marriage_type x `piq ?opts)
let prerr_marriage_type ?opts x =
  Pervasives.prerr_endline (gen_marriage_type x `piq ?opts)

let print_divorce_type ?opts x =
  Pervasives.print_endline (gen_divorce_type x `piq ?opts)
let prerr_divorce_type ?opts x =
  Pervasives.prerr_endline (gen_divorce_type x `piq ?opts)


