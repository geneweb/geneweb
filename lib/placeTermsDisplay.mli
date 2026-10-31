val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [m=PLACE_TERMS]: spellings to group.
    - no parameter: the clusters of spellings ({!PlaceTerms.clusters});
    - [c=<cluster key>]: the spellings of a cluster, the spelling to keep and
      the occurrences to merge;
    - [t=<term>]: every context and full place where [term] occurs, with a form
      to rename the checked occurrences;
    - [groups=on]: the places to complete ({!PlaceGroups});
    - [g=<istr>]: a group of places to complete, the complete place to keep and
      the places to replace by it; with [gt=<istr>&p<istr>=on...]: the preview
      of that change;
    - [s=<prefix>]: index of the terms whose lowered form starts with [prefix]
      (not linked from the pages);
    - [t=<term>] (or [t1], [t2]... for several)
      [&nt=<new term>&o<istr>.<depth>=on...]: preview of that renaming, with the
      form leading to [m=PLACE_TERMS_OK]. *)

val print_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** [m=PLACE_TERMS_OK]: renames the checked occurrences (see {!PlaceRename}) and
    commits. *)

val print_sources : Config.config -> Geneweb_db.Driver.base -> unit
(** [m=SOURCE_TERMS]: sources to group, same layout as [m=PLACE_TERMS].
    - [part=w] (default) or [part=s]: the clusters of spellings of the wizards
      or of the sources ({!SourceTerms.clusters}), the sources chosen by the
      beginning of their key ([s=]);
    - [c=<cluster key>]: the spelling to keep and the spellings to replace;
    - [t1=...&nt=...]: preview, with the form leading to [m=SOURCE_TERMS_OK]. *)

val print_sources_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** [m=SOURCE_TERMS_OK]: replaces the spellings in the source strings (see
    {!PlaceRename.apply}) and commits. *)
