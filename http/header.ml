let extract_params name stop_char request =
  let ln = String.length name in
  let name = String.lowercase_ascii name in
  List.filter_map
    (fun x ->
      if
        String.length x >= ln
        && String.lowercase_ascii (String.sub x 0 ln) = name
      then
        let i =
          Option.value ~default:(String.length x)
            (String.index_from_opt x ln stop_char)
        in
        Some (String.sub x ln (i - ln))
      else None)
    request

let extract_param name stop_char request =
  match extract_params name stop_char request with x :: _ -> x | [] -> ""
