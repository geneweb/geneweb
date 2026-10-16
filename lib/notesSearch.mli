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

val highlight_html : bool -> string -> string -> string
(** [highlight_html case_sens s html] wraps every occurrence of [s] in the text
    of the rendered HTML [html] in [<span class="found">], the first one with
    [id="first-found"] so that a [#first-found] URL fragment scrolls to it.
    Tags, attribute values, comments and the contents of [script], [style],
    [textarea] and [title] are left untouched. Matching is done on the decoded
    text: an entity counts as the character it stands for and a white space
    run as one space. Occurrences that span a tag ([mou<i>lin</i>]) are not
    highlighted. *)

val highlight : bool -> string -> string -> string
(** [highlight case_sens s p] is the whole plain text [p], HTML-escaped, with
    occurrences of [s] wrapped in [<span class="found">]. *)
