module rec Api_stats_piqi:
  sig
    type protobuf_int32 = int32
    type title =
      [
        | `st_ind_longevity
        | `st_ind_birth_month
        | `st_ind_parent_age
        | `st_ind_lastname
        | `st_ind_firstname
        | `st_ind_occupation
        | `st_ind_youngest_parent
        | `st_ind_oldest
        | `st_ind_astro
        | `st_ind_moon
        | `st_fam_first_marr_parent_age
        | `st_fam_marr_day
        | `st_fam_marr_month
        | `st_fam_avg_marr_nb
        | `st_fam_avg_marr_duration
        | `st_fam_avg_nb_children
        | `st_fam_int_btw_children
        | `st_fam_diff_age_btw_children
        | `st_fam_diff_age_btw_cpl
        | `st_fam_longuest_marr
        | `st_fam_shortest_marr
        | `st_asc
        | `st_desc
        | `st_desc_man_woman
        | `st_asc_lastname
        | `st_asc_firstname
        | `st_asc_occupation
        | `st_desc_lastname
        | `st_desc_firstname
        | `st_desc_occupation
      ]
    type serie =
      [
        | `serie_male
        | `serie_female
        | `serie_month_1
        | `serie_month_2
        | `serie_month_3
        | `serie_month_4
        | `serie_month_5
        | `serie_month_6
        | `serie_month_7
        | `serie_month_8
        | `serie_month_9
        | `serie_month_10
        | `serie_month_11
        | `serie_month_12
        | `serie_day_1
        | `serie_day_2
        | `serie_day_3
        | `serie_day_4
        | `serie_day_5
        | `serie_day_6
        | `serie_day_7
        | `serie_male_age_first_child
        | `serie_male_age_last_child
        | `serie_female_age_first_child
        | `serie_female_age_last_child
        | `serie_all
        | `serie_ind_found
        | `serie_ind_uniq
        | `serie_top_10_1
        | `serie_top_10_2
        | `serie_top_10_3
        | `serie_top_10_4
        | `serie_top_10_5
        | `serie_top_10_6
        | `serie_top_10_7
        | `serie_top_10_8
        | `serie_top_10_9
        | `serie_top_10_10
        | `serie_aries
        | `serie_taurus
        | `serie_gemini
        | `serie_cancer
        | `serie_leo
        | `serie_virgo
        | `serie_libra
        | `serie_scorpio
        | `serie_sagittarius
        | `serie_capricorn
        | `serie_aquarius
        | `serie_pisces
        | `serie_moon_new
        | `serie_moon_first_quarter
        | `serie_moon_full
        | `serie_moon_last_quarter
        | `serie_moon_other
      ]
    type data = Data.t
    type data_l = Data_l.t
    type stat = Stat.t
    type stats = Stats.t
    type stats_params = Stats_params.t
  end = Api_stats_piqi
and Data:
  sig
    type t = {
      mutable nb: Api_stats_piqi.protobuf_int32;
      mutable value: Api_stats_piqi.protobuf_int32;
    }
  end = Data
and Data_l:
  sig
    type t = {
      mutable data: Api_stats_piqi.data list;
    }
  end = Data_l
and Stat:
  sig
    type t = {
      mutable title: Api_stats_piqi.title;
      mutable labels: Api_stats_piqi.protobuf_int32 list;
      mutable series: Api_stats_piqi.serie list;
      mutable series_string: string list;
      mutable datas: Api_stats_piqi.data_l list;
    }
  end = Stat
and Stats:
  sig
    type t = {
      mutable stats: Api_stats_piqi.stat list;
    }
  end = Stats
and Stats_params:
  sig
    type t = {
      mutable i: Api_stats_piqi.protobuf_int32 option;
    }
  end = Stats_params


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_data x =
  let x = Piqirun.parse_record x in
  let _nb, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _value, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Data.nb = _nb;
    Data.value = _value;
  }

and parse_data_l x =
  let x = Piqirun.parse_record x in
  let _data, x = Piqirun.parse_repeated_field 1 parse_data x in
  Piqirun.check_unparsed_fields x;
  {
    Data_l.data = _data;
  }

and parse_stat x =
  let x = Piqirun.parse_record x in
  let _title, x = Piqirun.parse_required_field 1 parse_title x in
  let _labels, x = Piqirun.parse_repeated_field 2 parse_protobuf_int32 x in
  let _series, x = Piqirun.parse_repeated_field 3 parse_serie x in
  let _series_string, x = Piqirun.parse_repeated_field 4 parse_string x in
  let _datas, x = Piqirun.parse_repeated_field 5 parse_data_l x in
  Piqirun.check_unparsed_fields x;
  {
    Stat.title = _title;
    Stat.labels = _labels;
    Stat.series = _series;
    Stat.series_string = _series_string;
    Stat.datas = _datas;
  }

and parse_stats x =
  let x = Piqirun.parse_record x in
  let _stats, x = Piqirun.parse_repeated_field 1 parse_stat x in
  Piqirun.check_unparsed_fields x;
  {
    Stats.stats = _stats;
  }

and parse_stats_params x =
  let x = Piqirun.parse_record x in
  let _i, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Stats_params.i = _i;
  }

and parse_title x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `st_ind_longevity
    | 1l -> `st_ind_birth_month
    | 2l -> `st_ind_parent_age
    | 3l -> `st_ind_lastname
    | 4l -> `st_ind_firstname
    | 5l -> `st_ind_occupation
    | 6l -> `st_ind_youngest_parent
    | 7l -> `st_ind_oldest
    | 8l -> `st_ind_astro
    | 9l -> `st_ind_moon
    | 10l -> `st_fam_first_marr_parent_age
    | 11l -> `st_fam_marr_day
    | 12l -> `st_fam_marr_month
    | 13l -> `st_fam_avg_marr_nb
    | 14l -> `st_fam_avg_marr_duration
    | 15l -> `st_fam_avg_nb_children
    | 16l -> `st_fam_int_btw_children
    | 17l -> `st_fam_diff_age_btw_children
    | 18l -> `st_fam_diff_age_btw_cpl
    | 19l -> `st_fam_longuest_marr
    | 20l -> `st_fam_shortest_marr
    | 21l -> `st_asc
    | 22l -> `st_desc
    | 23l -> `st_desc_man_woman
    | 24l -> `st_asc_lastname
    | 25l -> `st_asc_firstname
    | 26l -> `st_asc_occupation
    | 27l -> `st_desc_lastname
    | 28l -> `st_desc_firstname
    | 29l -> `st_desc_occupation
    | x -> Piqirun.error_enum_const x
and packed_parse_title x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `st_ind_longevity
    | 1l -> `st_ind_birth_month
    | 2l -> `st_ind_parent_age
    | 3l -> `st_ind_lastname
    | 4l -> `st_ind_firstname
    | 5l -> `st_ind_occupation
    | 6l -> `st_ind_youngest_parent
    | 7l -> `st_ind_oldest
    | 8l -> `st_ind_astro
    | 9l -> `st_ind_moon
    | 10l -> `st_fam_first_marr_parent_age
    | 11l -> `st_fam_marr_day
    | 12l -> `st_fam_marr_month
    | 13l -> `st_fam_avg_marr_nb
    | 14l -> `st_fam_avg_marr_duration
    | 15l -> `st_fam_avg_nb_children
    | 16l -> `st_fam_int_btw_children
    | 17l -> `st_fam_diff_age_btw_children
    | 18l -> `st_fam_diff_age_btw_cpl
    | 19l -> `st_fam_longuest_marr
    | 20l -> `st_fam_shortest_marr
    | 21l -> `st_asc
    | 22l -> `st_desc
    | 23l -> `st_desc_man_woman
    | 24l -> `st_asc_lastname
    | 25l -> `st_asc_firstname
    | 26l -> `st_asc_occupation
    | 27l -> `st_desc_lastname
    | 28l -> `st_desc_firstname
    | 29l -> `st_desc_occupation
    | x -> Piqirun.error_enum_const x

and parse_serie x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `serie_male
    | 1l -> `serie_female
    | 2l -> `serie_month_1
    | 3l -> `serie_month_2
    | 4l -> `serie_month_3
    | 5l -> `serie_month_4
    | 6l -> `serie_month_5
    | 7l -> `serie_month_6
    | 8l -> `serie_month_7
    | 9l -> `serie_month_8
    | 10l -> `serie_month_9
    | 11l -> `serie_month_10
    | 12l -> `serie_month_11
    | 13l -> `serie_month_12
    | 14l -> `serie_day_1
    | 15l -> `serie_day_2
    | 16l -> `serie_day_3
    | 17l -> `serie_day_4
    | 18l -> `serie_day_5
    | 19l -> `serie_day_6
    | 20l -> `serie_day_7
    | 21l -> `serie_male_age_first_child
    | 22l -> `serie_male_age_last_child
    | 23l -> `serie_female_age_first_child
    | 24l -> `serie_female_age_last_child
    | 25l -> `serie_all
    | 26l -> `serie_ind_found
    | 27l -> `serie_ind_uniq
    | 28l -> `serie_top_10_1
    | 29l -> `serie_top_10_2
    | 30l -> `serie_top_10_3
    | 31l -> `serie_top_10_4
    | 32l -> `serie_top_10_5
    | 33l -> `serie_top_10_6
    | 34l -> `serie_top_10_7
    | 35l -> `serie_top_10_8
    | 36l -> `serie_top_10_9
    | 37l -> `serie_top_10_10
    | 38l -> `serie_aries
    | 39l -> `serie_taurus
    | 40l -> `serie_gemini
    | 41l -> `serie_cancer
    | 42l -> `serie_leo
    | 43l -> `serie_virgo
    | 44l -> `serie_libra
    | 45l -> `serie_scorpio
    | 46l -> `serie_sagittarius
    | 47l -> `serie_capricorn
    | 48l -> `serie_aquarius
    | 49l -> `serie_pisces
    | 50l -> `serie_moon_new
    | 51l -> `serie_moon_first_quarter
    | 52l -> `serie_moon_full
    | 53l -> `serie_moon_last_quarter
    | 54l -> `serie_moon_other
    | x -> Piqirun.error_enum_const x
and packed_parse_serie x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `serie_male
    | 1l -> `serie_female
    | 2l -> `serie_month_1
    | 3l -> `serie_month_2
    | 4l -> `serie_month_3
    | 5l -> `serie_month_4
    | 6l -> `serie_month_5
    | 7l -> `serie_month_6
    | 8l -> `serie_month_7
    | 9l -> `serie_month_8
    | 10l -> `serie_month_9
    | 11l -> `serie_month_10
    | 12l -> `serie_month_11
    | 13l -> `serie_month_12
    | 14l -> `serie_day_1
    | 15l -> `serie_day_2
    | 16l -> `serie_day_3
    | 17l -> `serie_day_4
    | 18l -> `serie_day_5
    | 19l -> `serie_day_6
    | 20l -> `serie_day_7
    | 21l -> `serie_male_age_first_child
    | 22l -> `serie_male_age_last_child
    | 23l -> `serie_female_age_first_child
    | 24l -> `serie_female_age_last_child
    | 25l -> `serie_all
    | 26l -> `serie_ind_found
    | 27l -> `serie_ind_uniq
    | 28l -> `serie_top_10_1
    | 29l -> `serie_top_10_2
    | 30l -> `serie_top_10_3
    | 31l -> `serie_top_10_4
    | 32l -> `serie_top_10_5
    | 33l -> `serie_top_10_6
    | 34l -> `serie_top_10_7
    | 35l -> `serie_top_10_8
    | 36l -> `serie_top_10_9
    | 37l -> `serie_top_10_10
    | 38l -> `serie_aries
    | 39l -> `serie_taurus
    | 40l -> `serie_gemini
    | 41l -> `serie_cancer
    | 42l -> `serie_leo
    | 43l -> `serie_virgo
    | 44l -> `serie_libra
    | 45l -> `serie_scorpio
    | 46l -> `serie_sagittarius
    | 47l -> `serie_capricorn
    | 48l -> `serie_aquarius
    | 49l -> `serie_pisces
    | 50l -> `serie_moon_new
    | 51l -> `serie_moon_first_quarter
    | 52l -> `serie_moon_full
    | 53l -> `serie_moon_last_quarter
    | 54l -> `serie_moon_other
    | x -> Piqirun.error_enum_const x


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__data code x =
  let _nb = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Data.nb in
  let _value = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Data.value in
  Piqirun.gen_record code (_nb :: _value :: [])

and gen__data_l code x =
  let _data = Piqirun.gen_repeated_field 1 gen__data x.Data_l.data in
  Piqirun.gen_record code (_data :: [])

and gen__stat code x =
  let _title = Piqirun.gen_required_field 1 gen__title x.Stat.title in
  let _labels = Piqirun.gen_repeated_field 2 gen__protobuf_int32 x.Stat.labels in
  let _series = Piqirun.gen_repeated_field 3 gen__serie x.Stat.series in
  let _series_string = Piqirun.gen_repeated_field 4 gen__string x.Stat.series_string in
  let _datas = Piqirun.gen_repeated_field 5 gen__data_l x.Stat.datas in
  Piqirun.gen_record code (_title :: _labels :: _series :: _series_string :: _datas :: [])

and gen__stats code x =
  let _stats = Piqirun.gen_repeated_field 1 gen__stat x.Stats.stats in
  Piqirun.gen_record code (_stats :: [])

and gen__stats_params code x =
  let _i = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Stats_params.i in
  Piqirun.gen_record code (_i :: [])

and gen__title code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `st_ind_longevity -> 0l
    | `st_ind_birth_month -> 1l
    | `st_ind_parent_age -> 2l
    | `st_ind_lastname -> 3l
    | `st_ind_firstname -> 4l
    | `st_ind_occupation -> 5l
    | `st_ind_youngest_parent -> 6l
    | `st_ind_oldest -> 7l
    | `st_ind_astro -> 8l
    | `st_ind_moon -> 9l
    | `st_fam_first_marr_parent_age -> 10l
    | `st_fam_marr_day -> 11l
    | `st_fam_marr_month -> 12l
    | `st_fam_avg_marr_nb -> 13l
    | `st_fam_avg_marr_duration -> 14l
    | `st_fam_avg_nb_children -> 15l
    | `st_fam_int_btw_children -> 16l
    | `st_fam_diff_age_btw_children -> 17l
    | `st_fam_diff_age_btw_cpl -> 18l
    | `st_fam_longuest_marr -> 19l
    | `st_fam_shortest_marr -> 20l
    | `st_asc -> 21l
    | `st_desc -> 22l
    | `st_desc_man_woman -> 23l
    | `st_asc_lastname -> 24l
    | `st_asc_firstname -> 25l
    | `st_asc_occupation -> 26l
    | `st_desc_lastname -> 27l
    | `st_desc_firstname -> 28l
    | `st_desc_occupation -> 29l
  )
and packed_gen__title x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `st_ind_longevity -> 0l
    | `st_ind_birth_month -> 1l
    | `st_ind_parent_age -> 2l
    | `st_ind_lastname -> 3l
    | `st_ind_firstname -> 4l
    | `st_ind_occupation -> 5l
    | `st_ind_youngest_parent -> 6l
    | `st_ind_oldest -> 7l
    | `st_ind_astro -> 8l
    | `st_ind_moon -> 9l
    | `st_fam_first_marr_parent_age -> 10l
    | `st_fam_marr_day -> 11l
    | `st_fam_marr_month -> 12l
    | `st_fam_avg_marr_nb -> 13l
    | `st_fam_avg_marr_duration -> 14l
    | `st_fam_avg_nb_children -> 15l
    | `st_fam_int_btw_children -> 16l
    | `st_fam_diff_age_btw_children -> 17l
    | `st_fam_diff_age_btw_cpl -> 18l
    | `st_fam_longuest_marr -> 19l
    | `st_fam_shortest_marr -> 20l
    | `st_asc -> 21l
    | `st_desc -> 22l
    | `st_desc_man_woman -> 23l
    | `st_asc_lastname -> 24l
    | `st_asc_firstname -> 25l
    | `st_asc_occupation -> 26l
    | `st_desc_lastname -> 27l
    | `st_desc_firstname -> 28l
    | `st_desc_occupation -> 29l
  )

and gen__serie code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `serie_male -> 0l
    | `serie_female -> 1l
    | `serie_month_1 -> 2l
    | `serie_month_2 -> 3l
    | `serie_month_3 -> 4l
    | `serie_month_4 -> 5l
    | `serie_month_5 -> 6l
    | `serie_month_6 -> 7l
    | `serie_month_7 -> 8l
    | `serie_month_8 -> 9l
    | `serie_month_9 -> 10l
    | `serie_month_10 -> 11l
    | `serie_month_11 -> 12l
    | `serie_month_12 -> 13l
    | `serie_day_1 -> 14l
    | `serie_day_2 -> 15l
    | `serie_day_3 -> 16l
    | `serie_day_4 -> 17l
    | `serie_day_5 -> 18l
    | `serie_day_6 -> 19l
    | `serie_day_7 -> 20l
    | `serie_male_age_first_child -> 21l
    | `serie_male_age_last_child -> 22l
    | `serie_female_age_first_child -> 23l
    | `serie_female_age_last_child -> 24l
    | `serie_all -> 25l
    | `serie_ind_found -> 26l
    | `serie_ind_uniq -> 27l
    | `serie_top_10_1 -> 28l
    | `serie_top_10_2 -> 29l
    | `serie_top_10_3 -> 30l
    | `serie_top_10_4 -> 31l
    | `serie_top_10_5 -> 32l
    | `serie_top_10_6 -> 33l
    | `serie_top_10_7 -> 34l
    | `serie_top_10_8 -> 35l
    | `serie_top_10_9 -> 36l
    | `serie_top_10_10 -> 37l
    | `serie_aries -> 38l
    | `serie_taurus -> 39l
    | `serie_gemini -> 40l
    | `serie_cancer -> 41l
    | `serie_leo -> 42l
    | `serie_virgo -> 43l
    | `serie_libra -> 44l
    | `serie_scorpio -> 45l
    | `serie_sagittarius -> 46l
    | `serie_capricorn -> 47l
    | `serie_aquarius -> 48l
    | `serie_pisces -> 49l
    | `serie_moon_new -> 50l
    | `serie_moon_first_quarter -> 51l
    | `serie_moon_full -> 52l
    | `serie_moon_last_quarter -> 53l
    | `serie_moon_other -> 54l
  )
and packed_gen__serie x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `serie_male -> 0l
    | `serie_female -> 1l
    | `serie_month_1 -> 2l
    | `serie_month_2 -> 3l
    | `serie_month_3 -> 4l
    | `serie_month_4 -> 5l
    | `serie_month_5 -> 6l
    | `serie_month_6 -> 7l
    | `serie_month_7 -> 8l
    | `serie_month_8 -> 9l
    | `serie_month_9 -> 10l
    | `serie_month_10 -> 11l
    | `serie_month_11 -> 12l
    | `serie_month_12 -> 13l
    | `serie_day_1 -> 14l
    | `serie_day_2 -> 15l
    | `serie_day_3 -> 16l
    | `serie_day_4 -> 17l
    | `serie_day_5 -> 18l
    | `serie_day_6 -> 19l
    | `serie_day_7 -> 20l
    | `serie_male_age_first_child -> 21l
    | `serie_male_age_last_child -> 22l
    | `serie_female_age_first_child -> 23l
    | `serie_female_age_last_child -> 24l
    | `serie_all -> 25l
    | `serie_ind_found -> 26l
    | `serie_ind_uniq -> 27l
    | `serie_top_10_1 -> 28l
    | `serie_top_10_2 -> 29l
    | `serie_top_10_3 -> 30l
    | `serie_top_10_4 -> 31l
    | `serie_top_10_5 -> 32l
    | `serie_top_10_6 -> 33l
    | `serie_top_10_7 -> 34l
    | `serie_top_10_8 -> 35l
    | `serie_top_10_9 -> 36l
    | `serie_top_10_10 -> 37l
    | `serie_aries -> 38l
    | `serie_taurus -> 39l
    | `serie_gemini -> 40l
    | `serie_cancer -> 41l
    | `serie_leo -> 42l
    | `serie_virgo -> 43l
    | `serie_libra -> 44l
    | `serie_scorpio -> 45l
    | `serie_sagittarius -> 46l
    | `serie_capricorn -> 47l
    | `serie_aquarius -> 48l
    | `serie_pisces -> 49l
    | `serie_moon_new -> 50l
    | `serie_moon_first_quarter -> 51l
    | `serie_moon_full -> 52l
    | `serie_moon_last_quarter -> 53l
    | `serie_moon_other -> 54l
  )


let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_data x = gen__data (-1) x
let gen_data_l x = gen__data_l (-1) x
let gen_stat x = gen__stat (-1) x
let gen_stats x = gen__stats (-1) x
let gen_stats_params x = gen__stats_params (-1) x
let gen_title x = gen__title (-1) x
let gen_serie x = gen__serie (-1) x


let rec default_int32 () = 0l
and default_protobuf_int32 () = default_int32 ()
and default_string () = ""
and default_data () =
  {
    Data.nb = default_protobuf_int32 ();
    Data.value = default_protobuf_int32 ();
  }
and default_data_l () =
  {
    Data_l.data = [];
  }
and default_stat () =
  {
    Stat.title = default_title ();
    Stat.labels = [];
    Stat.series = [];
    Stat.series_string = [];
    Stat.datas = [];
  }
and default_stats () =
  {
    Stats.stats = [];
  }
and default_stats_params () =
  {
    Stats_params.i = None;
  }
and default_title () = `st_ind_longevity
and default_serie () = `serie_male


let piqi = "\226\202\2304\tapi_stats\226\231\249\238\001\020api_stats.proto.piqi\162\244\146\155\011\024geneweb.api.stats.object\218\244\134\182\012s\138\233\142\251\014m\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002nb\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005value\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\004data\218\244\134\182\012:\138\233\142\251\0144\210\203\242$#\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\004data\210\171\158\194\006\004data\218\164\238\191\004\006data-l\218\244\134\182\012\248\001\138\233\142\251\014\241\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005title\210\171\158\194\006\005title\210\203\242$/\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006labels\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006series\210\171\158\194\006\005serie\210\203\242$.\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\rseries-string\210\171\158\194\006\006string\210\203\242$&\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005datas\210\171\158\194\006\006data-l\218\164\238\191\004\004stat\218\244\134\182\012:\138\233\142\251\0144\210\203\242$$\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\005stats\210\171\158\194\006\004stat\218\164\238\191\004\005stats\218\244\134\182\012G\138\233\142\251\014A\210\203\242$*\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\001i\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012stats-params\218\244\134\182\012\156\b\138\176\205\197\001\149\b\218\164\238\191\004\005title\170\183\218\222\005\027\232\146\150q\000\218\164\238\191\004\016st-ind-longevity\170\183\218\222\005\029\232\146\150q\002\218\164\238\191\004\018st-ind-birth-month\170\183\218\222\005\028\232\146\150q\004\218\164\238\191\004\017st-ind-parent-age\170\183\218\222\005\026\232\146\150q\006\218\164\238\191\004\015st-ind-lastname\170\183\218\222\005\027\232\146\150q\b\218\164\238\191\004\016st-ind-firstname\170\183\218\222\005\028\232\146\150q\n\218\164\238\191\004\017st-ind-occupation\170\183\218\222\005!\232\146\150q\012\218\164\238\191\004\022st-ind-youngest-parent\170\183\218\222\005\024\232\146\150q\014\218\164\238\191\004\rst-ind-oldest\170\183\218\222\005\023\232\146\150q\016\218\164\238\191\004\012st-ind-astro\170\183\218\222\005\022\232\146\150q\018\218\164\238\191\004\011st-ind-moon\170\183\218\222\005'\232\146\150q\020\218\164\238\191\004\028st-fam-first-marr-parent-age\170\183\218\222\005\026\232\146\150q\022\218\164\238\191\004\015st-fam-marr-day\170\183\218\222\005\028\232\146\150q\024\218\164\238\191\004\017st-fam-marr-month\170\183\218\222\005\029\232\146\150q\026\218\164\238\191\004\018st-fam-avg-marr-nb\170\183\218\222\005#\232\146\150q\028\218\164\238\191\004\024st-fam-avg-marr-duration\170\183\218\222\005!\232\146\150q\030\218\164\238\191\004\022st-fam-avg-nb-children\170\183\218\222\005\"\232\146\150q \218\164\238\191\004\023st-fam-int-btw-children\170\183\218\222\005'\232\146\150q\"\218\164\238\191\004\028st-fam-diff-age-btw-children\170\183\218\222\005\"\232\146\150q$\218\164\238\191\004\023st-fam-diff-age-btw-cpl\170\183\218\222\005\031\232\146\150q&\218\164\238\191\004\020st-fam-longuest-marr\170\183\218\222\005\031\232\146\150q(\218\164\238\191\004\020st-fam-shortest-marr\170\183\218\222\005\017\232\146\150q*\218\164\238\191\004\006st-asc\170\183\218\222\005\018\232\146\150q,\218\164\238\191\004\007st-desc\170\183\218\222\005\028\232\146\150q.\218\164\238\191\004\017st-desc-man-woman\170\183\218\222\005\026\232\146\150q0\218\164\238\191\004\015st-asc-lastname\170\183\218\222\005\027\232\146\150q2\218\164\238\191\004\016st-asc-firstname\170\183\218\222\005\028\232\146\150q4\218\164\238\191\004\017st-asc-occupation\170\183\218\222\005\027\232\146\150q6\218\164\238\191\004\016st-desc-lastname\170\183\218\222\005\028\232\146\150q8\218\164\238\191\004\017st-desc-firstname\170\183\218\222\005\029\232\146\150q:\218\164\238\191\004\018st-desc-occupation\218\244\134\182\012\201\r\138\176\205\197\001\194\r\218\164\238\191\004\005serie\170\183\218\222\005\021\232\146\150q\000\218\164\238\191\004\nserie-male\170\183\218\222\005\023\232\146\150q\002\218\164\238\191\004\012serie-female\170\183\218\222\005\024\232\146\150q\004\218\164\238\191\004\rserie-month-1\170\183\218\222\005\024\232\146\150q\006\218\164\238\191\004\rserie-month-2\170\183\218\222\005\024\232\146\150q\b\218\164\238\191\004\rserie-month-3\170\183\218\222\005\024\232\146\150q\n\218\164\238\191\004\rserie-month-4\170\183\218\222\005\024\232\146\150q\012\218\164\238\191\004\rserie-month-5\170\183\218\222\005\024\232\146\150q\014\218\164\238\191\004\rserie-month-6\170\183\218\222\005\024\232\146\150q\016\218\164\238\191\004\rserie-month-7\170\183\218\222\005\024\232\146\150q\018\218\164\238\191\004\rserie-month-8\170\183\218\222\005\024\232\146\150q\020\218\164\238\191\004\rserie-month-9\170\183\218\222\005\025\232\146\150q\022\218\164\238\191\004\014serie-month-10\170\183\218\222\005\025\232\146\150q\024\218\164\238\191\004\014serie-month-11\170\183\218\222\005\025\232\146\150q\026\218\164\238\191\004\014serie-month-12\170\183\218\222\005\022\232\146\150q\028\218\164\238\191\004\011serie-day-1\170\183\218\222\005\022\232\146\150q\030\218\164\238\191\004\011serie-day-2\170\183\218\222\005\022\232\146\150q \218\164\238\191\004\011serie-day-3\170\183\218\222\005\022\232\146\150q\"\218\164\238\191\004\011serie-day-4\170\183\218\222\005\022\232\146\150q$\218\164\238\191\004\011serie-day-5\170\183\218\222\005\022\232\146\150q&\218\164\238\191\004\011serie-day-6\170\183\218\222\005\022\232\146\150q(\218\164\238\191\004\011serie-day-7\170\183\218\222\005%\232\146\150q*\218\164\238\191\004\026serie-male-age-first-child\170\183\218\222\005$\232\146\150q,\218\164\238\191\004\025serie-male-age-last-child\170\183\218\222\005'\232\146\150q.\218\164\238\191\004\028serie-female-age-first-child\170\183\218\222\005&\232\146\150q0\218\164\238\191\004\027serie-female-age-last-child\170\183\218\222\005\020\232\146\150q2\218\164\238\191\004\tserie-all\170\183\218\222\005\026\232\146\150q4\218\164\238\191\004\015serie-ind-found\170\183\218\222\005\025\232\146\150q6\218\164\238\191\004\014serie-ind-uniq\170\183\218\222\005\025\232\146\150q8\218\164\238\191\004\014serie-top-10-1\170\183\218\222\005\025\232\146\150q:\218\164\238\191\004\014serie-top-10-2\170\183\218\222\005\025\232\146\150q<\218\164\238\191\004\014serie-top-10-3\170\183\218\222\005\025\232\146\150q>\218\164\238\191\004\014serie-top-10-4\170\183\218\222\005\025\232\146\150q@\218\164\238\191\004\014serie-top-10-5\170\183\218\222\005\025\232\146\150qB\218\164\238\191\004\014serie-top-10-6\170\183\218\222\005\025\232\146\150qD\218\164\238\191\004\014serie-top-10-7\170\183\218\222\005\025\232\146\150qF\218\164\238\191\004\014serie-top-10-8\170\183\218\222\005\025\232\146\150qH\218\164\238\191\004\014serie-top-10-9\170\183\218\222\005\026\232\146\150qJ\218\164\238\191\004\015serie-top-10-10\170\183\218\222\005\022\232\146\150qL\218\164\238\191\004\011serie-aries\170\183\218\222\005\023\232\146\150qN\218\164\238\191\004\012serie-taurus\170\183\218\222\005\023\232\146\150qP\218\164\238\191\004\012serie-gemini\170\183\218\222\005\023\232\146\150qR\218\164\238\191\004\012serie-cancer\170\183\218\222\005\020\232\146\150qT\218\164\238\191\004\tserie-leo\170\183\218\222\005\022\232\146\150qV\218\164\238\191\004\011serie-virgo\170\183\218\222\005\022\232\146\150qX\218\164\238\191\004\011serie-libra\170\183\218\222\005\024\232\146\150qZ\218\164\238\191\004\rserie-scorpio\170\183\218\222\005\028\232\146\150q\\\218\164\238\191\004\017serie-sagittarius\170\183\218\222\005\026\232\146\150q^\218\164\238\191\004\015serie-capricorn\170\183\218\222\005\025\232\146\150q`\218\164\238\191\004\014serie-aquarius\170\183\218\222\005\023\232\146\150qb\218\164\238\191\004\012serie-pisces\170\183\218\222\005\025\232\146\150qd\218\164\238\191\004\014serie-moon-new\170\183\218\222\005#\232\146\150qf\218\164\238\191\004\024serie-moon-first-quarter\170\183\218\222\005\026\232\146\150qh\218\164\238\191\004\015serie-moon-full\170\183\218\222\005\"\232\146\150qj\218\164\238\191\004\023serie-moon-last-quarter\170\183\218\222\005\027\232\146\150ql\218\164\238\191\004\016serie-moon-other"
include Api_stats_piqi
