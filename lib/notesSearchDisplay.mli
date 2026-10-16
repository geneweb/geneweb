(** [m=NOTES_SEARCH]: one search in every kind of notes of a base. *)

type query
(** A query read from the URL: text [s], options [c=on] (case sensitive) and
    [word=on] (word match), and the scopes. *)

val query_of_env : Config.config -> query
(** [query_of_env conf] reads the query of [conf.env]. The scopes are the
    ticked boxes among those the user may search: [sc_m] (misc notes, for
    everybody), [sc_w] (wizard notes, for friends and wizards, when the base
    has them), [sc_p], [sc_f], [sc_pe] and [sc_fe] (person, family, person
    event and family event notes, for friends and wizards); all of them when
    none is ticked. *)

val print_form : ?suffix:string -> ?help:bool -> Config.config -> query -> unit
(** [print_form conf q] prints the search form filled with [q], with the
    query syntax help ([?help], default [true]). [?suffix] keeps the element
    ids unique when the form is printed twice on a page. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] renders [m=NOTES_SEARCH]. The query follows
    {!NotesSearch.query}: AND-ed words, ["phrases"], [-excluded], [*] and [?].

    Misc and wizard notes are read live. Person, family and event notes come
    from the {!NotesCorpus} built by [cache_files -notes]; persons and
    families changed since are read live. Each candidate is read from the
    base, checked for access ([RESTRICT] for misc notes, [authorized_age] of
    the person or of both spouses), and matched on its current text. At most
    [notes_search_max_results] corpus hits (base parameter, default 500).

    Results are grouped by scope; persons show their birth and death years,
    couples their marriage year, and for wizards a link to [m=MOD_IND] /
    [m=MOD_FAM]. The person links carry the query ([hl]), see
    {!highlight_note}. The form is repeated below the results. *)

val highlight_note : Config.config -> string -> string
(** [highlight_note conf html] highlights in the rendered note [html] the terms
    of the [hl] URL parameter (options [hl_c=on], case sensitive, and
    [hl_w=on], word match), which the links of the results carry; [html] is
    returned unchanged without [hl]. Used by {!Perso} for the notes of person
    pages. *)
