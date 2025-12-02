let rec extract_param name stop_char =
  let case_unsensitive_eq s1 s2 =
    String.lowercase_ascii s1 = String.lowercase_ascii s2
  in
  function
  | x :: l ->
      if
        String.length x >= String.length name
        && case_unsensitive_eq (String.sub x 0 (String.length name)) name
      then
        let i =
          match String.index_from_opt x (String.length name) stop_char with
          | Some i -> i
          | None -> String.length x
        in
        String.sub x (String.length name) (i - String.length name)
      else extract_param name stop_char l
  | [] -> ""
