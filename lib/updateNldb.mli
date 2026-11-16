val compute :
  ?verbose:bool ->
  ?debug:bool ->
  ?progress:bool ->
  Geneweb_db.Driver.base ->
  string ->
  unit
(** [compute base bdir] rebuilds from scratch the notes links database
    ([notes_links]) and the linked pages cache of [base], whose directory is
    [bdir] (the [.gwb] suffix is optional). Every place where wiki links may
    appear is scanned: base notes, wizard notes, misc notes ([notes_d]), and the
    notes, sources and witness notes of every person, family and event (as
    gathered by {!Notes.notes_bearing_text_of_person} and
    {!Notes.notes_bearing_text_of_family}).

    - [verbose] (default [true]): print section headers and page names on
      stderr.
    - [debug] (default [false]): print every person/family visited.
    - [progress] (default [true]): show progress bars (only on a tty).

    For best performance, the caller should have loaded the strings array
    ({!Geneweb_db.Driver.load_strings_array}) beforehand. *)
