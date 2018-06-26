let piqi = Api_stats_piqi.piqi


let _ = Piqirun_ext.init_piqi piqi


let _data_piqi_type = Piqirun_ext.find_piqi_type "api_stats/data"
let _data_l_piqi_type = Piqirun_ext.find_piqi_type "api_stats/data-l"
let _stat_piqi_type = Piqirun_ext.find_piqi_type "api_stats/stat"
let _stats_piqi_type = Piqirun_ext.find_piqi_type "api_stats/stats"
let _stats_params_piqi_type = Piqirun_ext.find_piqi_type "api_stats/stats-params"
let _title_piqi_type = Piqirun_ext.find_piqi_type "api_stats/title"
let _serie_piqi_type = Piqirun_ext.find_piqi_type "api_stats/serie"


let parse_data ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _data_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_data buf

let parse_data_l ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _data_l_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_data_l buf

let parse_stat ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _stat_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_stat buf

let parse_stats ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _stats_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_stats buf

let parse_stats_params ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _stats_params_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_stats_params buf

let parse_title ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _title_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_title buf

let parse_serie ?opts x (format :Piqirun_ext.input_format) =
  let x_pb = Piqirun_ext.convert _serie_piqi_type format `pb x ?opts in
  let buf = Piqirun.init_from_string x_pb in
  Api_stats_piqi.parse_serie buf


let gen_data ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_data x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _data_piqi_type `pb format x_pb ?opts

let gen_data_l ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_data_l x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _data_l_piqi_type `pb format x_pb ?opts

let gen_stat ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_stat x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _stat_piqi_type `pb format x_pb ?opts

let gen_stats ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_stats x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _stats_piqi_type `pb format x_pb ?opts

let gen_stats_params ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_stats_params x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _stats_params_piqi_type `pb format x_pb ?opts

let gen_title ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_title x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _title_piqi_type `pb format x_pb ?opts

let gen_serie ?opts x (format :Piqirun_ext.output_format) =
  let buf = Api_stats_piqi.gen_serie x in
  let x_pb = Piqirun.to_string buf in
  Piqirun_ext.convert _serie_piqi_type `pb format x_pb ?opts


let print_data ?opts x =
  Pervasives.print_endline (gen_data x `piq ?opts)
let prerr_data ?opts x =
  Pervasives.prerr_endline (gen_data x `piq ?opts)

let print_data_l ?opts x =
  Pervasives.print_endline (gen_data_l x `piq ?opts)
let prerr_data_l ?opts x =
  Pervasives.prerr_endline (gen_data_l x `piq ?opts)

let print_stat ?opts x =
  Pervasives.print_endline (gen_stat x `piq ?opts)
let prerr_stat ?opts x =
  Pervasives.prerr_endline (gen_stat x `piq ?opts)

let print_stats ?opts x =
  Pervasives.print_endline (gen_stats x `piq ?opts)
let prerr_stats ?opts x =
  Pervasives.prerr_endline (gen_stats x `piq ?opts)

let print_stats_params ?opts x =
  Pervasives.print_endline (gen_stats_params x `piq ?opts)
let prerr_stats_params ?opts x =
  Pervasives.prerr_endline (gen_stats_params x `piq ?opts)

let print_title ?opts x =
  Pervasives.print_endline (gen_title x `piq ?opts)
let prerr_title ?opts x =
  Pervasives.prerr_endline (gen_title x `piq ?opts)

let print_serie ?opts x =
  Pervasives.print_endline (gen_serie x `piq ?opts)
let prerr_serie ?opts x =
  Pervasives.prerr_endline (gen_serie x `piq ?opts)


