let piqi = Api_saisie_read_piqi.piqi
  
let _ = Piqirun_ext.init_piqi piqi
  
let _int32_piqtype = Piqirun_ext.find_piqtype "int32"
  
let _int64_piqtype = Piqirun_ext.find_piqtype "int64"
  
let _protobuf_int32_piqtype = Piqirun_ext.find_piqtype "protobuf-int32"
  
let _string_piqtype = Piqirun_ext.find_piqtype "string"
  
let _bool_piqtype = Piqirun_ext.find_piqtype "bool"
  
let _protobuf_int64_piqtype = Piqirun_ext.find_piqtype "protobuf-int64"
  
let _dmy_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/dmy"
  
let _date_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/date"
  
let _witness_event_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/witness-event"
  
let _event_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/event"
  
let _person_tree_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/person-tree"
  
let _simple_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/simple-person"
  
let _relation_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/relation-person"
  
let _event_witness_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/event-witness"
  
let _person_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/person"
  
let _family_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/family"
  
let _index_person_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/index-person"
  
let _node_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/node"
  
let _edge_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/edge"
  
let _graph_tree_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/graph-tree"
  
let _graph_tree_params_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/graph-tree-params"
  
let _sosa_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/sosa"
  
let _calendar_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/calendar"
  
let _precision_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/precision"
  
let _sex_piqtype = Piqirun_ext.find_piqtype "api_saisie_read/sex"
  
let _death_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/death-type"
  
let _marriage_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/marriage-type"
  
let _divorce_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/divorce-type"
  
let _relation_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/relation-type"
  
let _witness_type_piqtype =
  Piqirun_ext.find_piqtype "api_saisie_read/witness-type"
  
let parse_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_int32 buf
  
let parse_int64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _int64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_int64 buf
  
let parse_protobuf_int32 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int32_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_protobuf_int32 buf
  
let parse_string ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _string_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_string buf
  
let parse_bool ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _bool_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_bool buf
  
let parse_protobuf_int64 ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _protobuf_int64_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_protobuf_int64 buf
  
let parse_dmy ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _dmy_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_dmy buf
  
let parse_date ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _date_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_date buf
  
let parse_witness_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_witness_event buf
  
let parse_event ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_event buf
  
let parse_person_tree ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_tree_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_person_tree buf
  
let parse_simple_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _simple_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_simple_person buf
  
let parse_relation_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _relation_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_relation_person buf
  
let parse_event_witness ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _event_witness_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_event_witness buf
  
let parse_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_person buf
  
let parse_family ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _family_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_family buf
  
let parse_index_person ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _index_person_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_index_person buf
  
let parse_node ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _node_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_node buf
  
let parse_edge ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _edge_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_edge buf
  
let parse_graph_tree ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _graph_tree_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_graph_tree buf
  
let parse_graph_tree_params ?opts x (format : Piqirun_ext.input_format) =
  let x_pb =
    Piqirun_ext.convert _graph_tree_params_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_graph_tree_params buf
  
let parse_sosa ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sosa_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_sosa buf
  
let parse_calendar ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _calendar_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_calendar buf
  
let parse_precision ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _precision_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_precision buf
  
let parse_sex ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _sex_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_sex buf
  
let parse_death_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _death_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_death_type buf
  
let parse_marriage_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _marriage_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_marriage_type buf
  
let parse_divorce_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _divorce_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_divorce_type buf
  
let parse_relation_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _relation_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_relation_type buf
  
let parse_witness_type ?opts x (format : Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _witness_type_piqtype format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb
  in Api_saisie_read_piqi.parse_witness_type buf
  
let gen_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int32_piqtype `pb format x_pb ?opts
  
let gen_int64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_int64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _int64_piqtype `pb format x_pb ?opts
  
let gen_protobuf_int32 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_protobuf_int32 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int32_piqtype `pb format x_pb ?opts
  
let gen_string ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_string x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _string_piqtype `pb format x_pb ?opts
  
let gen_bool ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_bool x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _bool_piqtype `pb format x_pb ?opts
  
let gen_protobuf_int64 ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_protobuf_int64 x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _protobuf_int64_piqtype `pb format x_pb ?opts
  
let gen_dmy ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_dmy x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _dmy_piqtype `pb format x_pb ?opts
  
let gen_date ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_date x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _date_piqtype `pb format x_pb ?opts
  
let gen_witness_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_witness_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_event_piqtype `pb format x_pb ?opts
  
let gen_event ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _event_piqtype `pb format x_pb ?opts
  
let gen_person_tree ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person_tree x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_tree_piqtype `pb format x_pb ?opts
  
let gen_simple_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_simple_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _simple_person_piqtype `pb format x_pb ?opts
  
let gen_relation_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_relation_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_person_piqtype `pb format x_pb ?opts
  
let gen_event_witness ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_event_witness x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _event_witness_piqtype `pb format x_pb ?opts
  
let gen_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _person_piqtype `pb format x_pb ?opts
  
let gen_family ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_family x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _family_piqtype `pb format x_pb ?opts
  
let gen_index_person ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_index_person x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _index_person_piqtype `pb format x_pb ?opts
  
let gen_node ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_node x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _node_piqtype `pb format x_pb ?opts
  
let gen_edge ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_edge x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _edge_piqtype `pb format x_pb ?opts
  
let gen_graph_tree ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _graph_tree_piqtype `pb format x_pb ?opts
  
let gen_graph_tree_params ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_graph_tree_params x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _graph_tree_params_piqtype `pb format x_pb ?opts
  
let gen_sosa ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_sosa x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sosa_piqtype `pb format x_pb ?opts
  
let gen_calendar ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_calendar x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _calendar_piqtype `pb format x_pb ?opts
  
let gen_precision ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_precision x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _precision_piqtype `pb format x_pb ?opts
  
let gen_sex ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_sex x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _sex_piqtype `pb format x_pb ?opts
  
let gen_death_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_death_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _death_type_piqtype `pb format x_pb ?opts
  
let gen_marriage_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_marriage_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _marriage_type_piqtype `pb format x_pb ?opts
  
let gen_divorce_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_divorce_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _divorce_type_piqtype `pb format x_pb ?opts
  
let gen_relation_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_relation_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _relation_type_piqtype `pb format x_pb ?opts
  
let gen_witness_type ?opts x (format : Piqirun_ext.output_format) =
  let buf = Api_saisie_read_piqi.gen_witness_type x in
  let x_pb = Piqirun.to_string buf
  in Piqirun_ext.convert _witness_type_piqtype `pb format x_pb ?opts
  
let print_int32 x = Pervasives.print_endline (gen_int32 x `piq)
  
let prerr_int32 x = Pervasives.prerr_endline (gen_int32 x `piq)
  
let print_int64 x = Pervasives.print_endline (gen_int64 x `piq)
  
let prerr_int64 x = Pervasives.prerr_endline (gen_int64 x `piq)
  
let print_protobuf_int32 x =
  Pervasives.print_endline (gen_protobuf_int32 x `piq)
  
let prerr_protobuf_int32 x =
  Pervasives.prerr_endline (gen_protobuf_int32 x `piq)
  
let print_string x = Pervasives.print_endline (gen_string x `piq)
  
let prerr_string x = Pervasives.prerr_endline (gen_string x `piq)
  
let print_bool x = Pervasives.print_endline (gen_bool x `piq)
  
let prerr_bool x = Pervasives.prerr_endline (gen_bool x `piq)
  
let print_protobuf_int64 x =
  Pervasives.print_endline (gen_protobuf_int64 x `piq)
  
let prerr_protobuf_int64 x =
  Pervasives.prerr_endline (gen_protobuf_int64 x `piq)
  
let print_dmy x = Pervasives.print_endline (gen_dmy x `piq)
  
let prerr_dmy x = Pervasives.prerr_endline (gen_dmy x `piq)
  
let print_date x = Pervasives.print_endline (gen_date x `piq)
  
let prerr_date x = Pervasives.prerr_endline (gen_date x `piq)
  
let print_witness_event x =
  Pervasives.print_endline (gen_witness_event x `piq)
  
let prerr_witness_event x =
  Pervasives.prerr_endline (gen_witness_event x `piq)
  
let print_event x = Pervasives.print_endline (gen_event x `piq)
  
let prerr_event x = Pervasives.prerr_endline (gen_event x `piq)
  
let print_person_tree x = Pervasives.print_endline (gen_person_tree x `piq)
  
let prerr_person_tree x = Pervasives.prerr_endline (gen_person_tree x `piq)
  
let print_simple_person x =
  Pervasives.print_endline (gen_simple_person x `piq)
  
let prerr_simple_person x =
  Pervasives.prerr_endline (gen_simple_person x `piq)
  
let print_relation_person x =
  Pervasives.print_endline (gen_relation_person x `piq)
  
let prerr_relation_person x =
  Pervasives.prerr_endline (gen_relation_person x `piq)
  
let print_event_witness x =
  Pervasives.print_endline (gen_event_witness x `piq)
  
let prerr_event_witness x =
  Pervasives.prerr_endline (gen_event_witness x `piq)
  
let print_person x = Pervasives.print_endline (gen_person x `piq)
  
let prerr_person x = Pervasives.prerr_endline (gen_person x `piq)
  
let print_family x = Pervasives.print_endline (gen_family x `piq)
  
let prerr_family x = Pervasives.prerr_endline (gen_family x `piq)
  
let print_index_person x = Pervasives.print_endline (gen_index_person x `piq)
  
let prerr_index_person x = Pervasives.prerr_endline (gen_index_person x `piq)
  
let print_node x = Pervasives.print_endline (gen_node x `piq)
  
let prerr_node x = Pervasives.prerr_endline (gen_node x `piq)
  
let print_edge x = Pervasives.print_endline (gen_edge x `piq)
  
let prerr_edge x = Pervasives.prerr_endline (gen_edge x `piq)
  
let print_graph_tree x = Pervasives.print_endline (gen_graph_tree x `piq)
  
let prerr_graph_tree x = Pervasives.prerr_endline (gen_graph_tree x `piq)
  
let print_graph_tree_params x =
  Pervasives.print_endline (gen_graph_tree_params x `piq)
  
let prerr_graph_tree_params x =
  Pervasives.prerr_endline (gen_graph_tree_params x `piq)
  
let print_sosa x = Pervasives.print_endline (gen_sosa x `piq)
  
let prerr_sosa x = Pervasives.prerr_endline (gen_sosa x `piq)
  
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
  
let print_relation_type x =
  Pervasives.print_endline (gen_relation_type x `piq)
  
let prerr_relation_type x =
  Pervasives.prerr_endline (gen_relation_type x `piq)
  
let print_witness_type x = Pervasives.print_endline (gen_witness_type x `piq)
  
let prerr_witness_type x = Pervasives.prerr_endline (gen_witness_type x `piq)
  

