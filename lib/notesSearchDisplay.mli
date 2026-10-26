(** [m=NOTES_SEARCH]: one search in every kind of notes of a base. *)

type query
(** A query read from the URL: text [s], options [c=on] (case sensitive) and
    [word=on] (word match), the scopes, [k], the maximum number of corpus hits,
    and [near], the maximum distance in words between the required terms (see
    {!NotesSearch.within}). *)

val query_of_env : Config.config -> query
(** [query_of_env conf] reads the query of [conf.env]. The scopes are the ticked
    boxes among those the user may search: [sc_m] (misc notes, for everybody),
    [sc_w] (wizard notes, for friends and wizards, when the base has them),
    [sc_p], [sc_f], [sc_pe] and [sc_fe] (person, family, person event and family
    event notes, for friends and wizards); all of them when none is ticked. [k]
    defaults to the base parameter [notes_search_max_results], else 500. *)

val print_form : Config.config -> query -> unit
(** [print_form conf q] prints the search form filled with [q], with the query
    syntax help. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] renders [m=NOTES_SEARCH]. The query follows
    {!NotesSearch.query}: AND-ed words, ["phrases"], [-excluded], [*] and [?]; a
    query without a required term searches nothing. Required words of 2 or 3
    letters outside a phrase get a warning (see {!NotesSearch.short_words}).

    Misc and wizard notes are read live, galleries on their displayed texts
    (title, chronicle, descriptions, labels). Person, family and event notes
    come from the {!NotesCorpus} built by [cache_files -notes]; persons and
    families changed since are read live. Each candidate is read from the base,
    checked for access ([RESTRICT] for misc notes, [authorized_age] of the
    person or of both spouses), and matched on its current text. At most [k]
    corpus hits, see {!query_of_env}.

    Results are grouped by scope, with links between the groups; persons show
    their dates, couples their marriage year, and for wizards a link to
    [m=MOD_IND] / [m=MOD_FAM]. The links carry the query ([hl]), see
    {!NotesSearch.highlight_note}. *)
