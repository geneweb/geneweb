(** Text search helpers for misc notes. Matching follows [Util.in_text]: exact
    when [case_sens] is [true], otherwise case- and accent-insensitive.

    A query is a list of [terms] (see {!terms}); the functions below find, count
    and highlight occurrences of any of them. With [~full_word:true], an
    occurrence counts only if it has no letter, digit or combining mark right
    before or right after it: [l'a] does not match in [l'aube], but [aube] does.
    In that mode a leading or trailing ['*'] lifts the check on that side:
    [henri*] matches [Henriette], [*ette] matches [Juliette], and the whole word
    is highlighted. Without it the ['*'] are ignored, as any match already
    counts. A ['*'] inside a term is a plain character.

    In both modes, a ['?'] in a term matches any one character, white space
    included: [Dupon?] matches [Dupont] and [Dupond], [Jean?Pierre] matches
    [Jean-Pierre] and [Jean Pierre]. *)

val core : string -> string
(** [core t] is the term [t] without its leading and trailing ['*']. *)

val query : bool -> string -> string list * string list
(** [query case_sens q] splits the query [q] into terms: words separated by
    white space, or phrases between double quotes (["Saint Pair"]), and returns
    [(required, excluded)]. A term written with a leading ['-'] ([-word],
    [-"some phrase"]) is excluded; a ['-'] inside a word ([Saint-Pair]) is not
    special and a lone ['-'] is ignored. Empty terms (or made only of ['*']) and
    duplicates (equal as compared by [Util.in_text]) are dropped. *)

val terms : bool -> string -> string list
(** [terms case_sens q] is the list of required terms of [query case_sens q],
    the ones to highlight. *)

val mem_all : ?full_word:bool -> bool -> string list -> string -> bool
(** [mem_all case_sens terms p] is [true] if every term occurs in [p]. *)

val matches :
  ?full_word:bool -> bool -> string list * string list -> string -> bool
(** [matches case_sens (terms, excluded) p] is [true] if every term of [terms]
    and none of [excluded] occurs in [p], as returned by {!query}. *)

val plain_text : string -> string
(** [plain_text m] is the readable text of the note source [m]: HTML tags
    dropped, wiki links replaced by their displayed text, [''] and [=] markup
    removed, common HTML entities decoded and white space collapsed. *)

val count_occurrences : ?full_word:bool -> bool -> string list -> string -> int
(** [count_occurrences case_sens terms p] counts non-overlapping occurrences of
    any of [terms] in the plain text [p]. *)

val snippet :
  ?context:int -> ?full_word:bool -> bool -> string list -> string -> string
(** [snippet ?context case_sens terms p] is an HTML-escaped excerpt of the plain
    text [p] around the first occurrence of any of [terms] (about [context]
    bytes on each side, default 80), with occurrences wrapped in
    [<span class="found">] and an ellipsis on truncated sides. Without an
    occurrence, the beginning of [p] is returned. *)

val highlight_html : ?full_word:bool -> bool -> string list -> string -> string
(** [highlight_html case_sens terms html] wraps every occurrence of any of
    [terms] in the text of the rendered HTML [html] in [<span class="found">],
    the first one with [id="first-found"] so that a [#first-found] URL fragment
    scrolls to it. Tags, attribute values, comments and the contents of
    [script], [style], [textarea] and [title] are left untouched. Matching is
    done on the decoded text: an entity counts as the character it stands for
    and a white space run as one space. Occurrences that span a tag
    ([mou<i>lin</i>]) are not highlighted. *)

val highlight : ?full_word:bool -> bool -> string list -> string -> string
(** [highlight case_sens terms p] is the whole plain text [p], HTML-escaped,
    with occurrences of [terms] wrapped in [<span class="found">]. *)

val fold : string -> string
(** [fold s] is [s] with each character replaced by its lowercase unaccented
    form (as compared by the case-insensitive matcher) and the typographic
    apostrophes by ['], for the notes corpus of [m=NOTES_SEARCH]. *)

val candidate : string list * string list -> string -> bool
(** [candidate query] is a fast test on {!fold}ed texts: [candidate query f] is
    [true] for every [f] whose original text could {!matches} [query], in any
    mode (case sensitive or not, word match or not), and possibly for a few
    more. Its result must be confirmed with {!matches} on the original text. *)
