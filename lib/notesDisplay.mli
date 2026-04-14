module Driver = Geneweb_db.Driver

val print_linked_list :
  Config.config ->
  Driver.base ->
  ?person:Driver.person ->
  (Driver.iper, Driver.ifam) Def.NLDB.page list ->
  unit
(** [print_linked_list conf base ?person pgl] renders [pgl], a list of pages
    (individuals, families, wiki notes, wizard notes, gallery notes) that link
    to some reference. The rendering layout is selected from the URL parameter
    [type]:
    - [Some "gallery"] → a thumbnail wall ({!print_linked_list_gallery}): for
      each gallery note, the thumbnails of images where [person] appears are
      shown (if [person] is given; otherwise a single default thumbnail per
      gallery note).
    - any other value → a standard table ({!print_linked_list_standard}) with
      one row per linked page.

    When [person] is provided, links to gallery notes carry that person's
    identifier as a URL fragment (via {!Util.acces}, honouring [access_by_key])
    so that downstream pages can restrict to galleries where the person appears.
*)

val print_what_links : Config.config -> Driver.base -> string -> unit
(** [print_what_links conf base fnotes] renders the page backlinks for a
    miscellaneous note identified by [fnotes] (its filename). Outputs the
    standard HTML header/trailer, a title [linked pages: fnotes], and delegates
    the list rendering to {!print_linked_list} with no [person] scope. *)

val print_what_links_p : Config.config -> Driver.base -> Driver.person -> unit
(** [print_what_links_p conf base p] renders the linked-pages view for person
    [p] (URL [m=LINKED&p=...&n=...] or [m=LINKED&i=N]). Checks
    {!Util.authorized_age}; on denial, outputs an "incorrect request" page.
    Otherwise, looks up the NLDB entries for [p]'s canonical key [(fn, sn, oc)]
    via {!Notes.links_to_ind}, then renders the result with {!print_linked_list}
    scoped on [p]. The page title is either [linked pages] (default) or
    [linked galleries] (when [type=gallery] is set in the URL environment). *)

val print : Config.config -> Driver.base -> unit
(** [print conf base] renders the base-wide notes page (URL [m=NOTES] without
    [f], or with [f] pointing to a specific miscellaneous note). Parses the
    note's [TYPE] header: if [TYPE=gallery] (or the deprecated [TYPE=album]),
    hands off to the gallery viewer template [notes_gallery]; otherwise renders
    a standard wiki-formatted note page. *)

val print_json : Config.config -> Driver.base -> unit
(** [print_json conf base] outputs the raw note content (no HTML wrapping) for
    JavaScript consumers (gallery viewer, editor). For gallery notes, the
    payload is first run through {!Notes.safe_gallery} to sanitize embedded HTML
    in [alt], [desc], [title], [chronicle] fields. *)

val print_mod : Config.config -> Driver.base -> unit
(** [print_mod conf base] renders the edit form for a note. Reads the target
    note via [f] in the URL environment. For gallery notes without the [raw=on]
    query flag, serves the dedicated [notes_upd_gallery] template (reactive
    image-map editor). Otherwise serves the plain textarea form. If the note has
    a [RESTRICT] header listing non-authorized keys, refuses the edit with an
    explanatory page. *)

val print_mod_json : Config.config -> Driver.base -> unit
(** [print_mod_json conf base] returns the note content formatted for AJAX
    consumption by the gallery editor. Unlike {!print_json}, this variant
    preserves edit-mode structure (including restricted fields) and is intended
    for authenticated editors only. *)

val print_mod_ok : Config.config -> Driver.base -> unit
(** [print_mod_ok conf base] processes the POST submission of an edited note.
    Writes the new content, updates the notes-links database ([nldb]) and the
    per-person [cache_linked_pages] counters accordingly, handles key renames
    and merges if the note was a gallery (via {!Notes.update_cache_linked_pages}
    and {!Notes.rewrite_key}), and records a history entry. Redirects to the
    view page on success. *)

val print_misc_notes : Config.config -> Driver.base -> unit
(** [print_misc_notes conf base] renders the directory listing of miscellaneous
    notes. Navigates the notes filesystem tree, showing subdirectories and notes
    at the current level, plus a search box. The current subdirectory is taken
    from the [d] URL parameter (empty = root). Also appends any loose files
    present under the notes directory that are not (yet) referenced in the
    database, under a "files not in db" heading. *)

val print_misc_notes_search : Config.config -> Driver.base -> unit
(** [print_misc_notes_search conf base] dispatches a text search request: reads
    [s] (the search string) from the URL and [c=on] for case sensitivity, walks
    the notes database, and renders the first matching note (via
    {!print_whole_notes} with search highlighting) or falls back to
    {!print_misc_notes} if no match is found. *)
