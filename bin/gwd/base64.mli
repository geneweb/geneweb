val decode : string -> string
(** Decode a {i Base64} string as used in HTTP basic authorization. Input is
    expected padded (length multiple of 4); returns the empty string on empty or
    malformed-length input. *)
