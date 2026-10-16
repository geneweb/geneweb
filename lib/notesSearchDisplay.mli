val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] renders [m=NOTES_SEARCH]: a search in the person, family
    and event notes of the base, for friends and wizards only: other users get a
    "notes search reserved to friends or wizards" page (HTTP 401).

    The query [s] follows the misc notes search ({!NotesSearch.query}: AND-ed
    words, ["phrases"], [-excluded], [*] and [?]), with [c=on] (case sensitive)
    and [word=on] (word match). The scopes are chosen with [sc_p] (person
    notes), [sc_f] (family notes), [sc_pe] (person event notes) and [sc_fe]
    (family event notes), all of them when none is given.

    Candidates come from the {!NotesCorpus} built by [cache_files -notes];
    persons and families changed since are read live. Each candidate is read
    from the base, checked for access ([authorized_age] of the person, or of
    both spouses), and matched on its current text. At most
    [notes_search_max_results] hits (base parameter, default 500) are shown. *)
