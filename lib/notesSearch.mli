(** Text search helpers for misc notes. Matching follows [Util.in_text]: exact
    when [case_sens] is [true], otherwise case- and accent-insensitive. *)

val plain_text : string -> string
(** [plain_text m] is the readable text of the note source [m]: HTML tags
    dropped, wiki links replaced by their displayed text, [''] and [=] markup
    removed, common HTML entities decoded and white space collapsed. *)

val count_occurrences : bool -> string -> string -> int
(** [count_occurrences case_sens s p] counts non-overlapping occurrences of [s]
    in the plain text [p]. *)

val snippet : ?context:int -> bool -> string -> string -> string
(** [snippet ?context case_sens s p] is an HTML-escaped excerpt of the plain
    text [p] around the first occurrence of [s] (about [context] bytes on each
    side, default 80), with occurrences wrapped in [<span class="found">] and an
    ellipsis on truncated sides. Without an occurrence, the beginning of [p] is
    returned. *)

val highlight : bool -> string -> string -> string
(** [highlight case_sens s p] is the whole plain text [p], HTML-escaped, with
    occurrences of [s] wrapped in [<span class="found">]. *)
