(** [extract_param name stopc request] can be used to extract some parameter
    from a browser [request] (list of strings); [name] is a string which should
    match the beginning of a request line, [stopc] is a character ending the
    request line. For example, the string request has been obtained by:
    [extract_param "GET /" ' ']. Answers the empty string if the parameter is
    not found. *)
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

(** Like [extract_param], but returns the values of all the lines starting with
    [name], in request order. A header field may be repeated; for list-valued
    fields (X-Forwarded-For...) the occurrences form one list. *)
let extract_params name stop_char request =
  let name_lc = String.lowercase_ascii name in
  let ln = String.length name in
  List.filter_map
    (fun x ->
      if
        String.length x >= ln
        && String.lowercase_ascii (String.sub x 0 ln) = name_lc
      then
        let i =
          match String.index_from_opt x ln stop_char with
          | Some i -> i
          | None -> String.length x
        in
        Some (String.sub x ln (i - ln))
      else None)
    request
