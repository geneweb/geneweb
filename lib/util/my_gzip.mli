val gunzip_file : string -> Bigstringaf.t
(** [gunzip_file path] decompresses gzip file and returns full content. *)

val gzip_string : ?level:int -> string -> string
(** [gzip_string ?level input] compresses [input] using gzip format.
    @param level compression level 1-9, default 6
    @return gzip compressed data *)
