type mode = Generic | Text | Font

val compress : ?quality:int -> ?mode:mode -> string -> string
(** [compress ?quality ?mode input] compresses [input] using Brotli.
    @param quality 0-11, default 4 (fast, good ratio for dynamic content)
    @param mode Text for HTML/UTF-8, Generic otherwise
    @raise Failure if compression fails *)
