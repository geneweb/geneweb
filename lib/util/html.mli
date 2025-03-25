val escape : string -> string
(** [escape str] replaces '&', '"', '<' and '>'
    with their corresponding character entities (using entity number) *)

val map : (Markup.signal -> Markup.signal) -> string -> string
