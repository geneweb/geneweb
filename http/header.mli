val extract_param : string -> char -> string list -> string
(** [extract_param name stopc request] can be used to extract some parameter
    from a browser [request] (list of strings); [name] is a string which should
    match the beginning of a request line, [stopc] is a character ending the
    request line. For example, the string request has been obtained by:
    [extract_param "GET /" ' ']. Answers the empty string if the parameter is
    not found. *)
