let use_plus_for_space ~(to_string : ?pct_encoder:_ -> _) value =
  let space = " " in
  Str.global_replace (Str.regexp_string space) "+"
    (to_string
       ~pct_encoder:
         (Uri.pct_encoder
            ~query_value:(`Custom (`Query_value, space, String.empty))
            ())
       value)

let to_string url = use_plus_for_space ~to_string:Uri.to_string url

let encoded_of_query params =
  use_plus_for_space
    ~to_string:(fun ?pct_encoder params ->
      Uri.encoded_of_query ?pct_encoder params)
    params
