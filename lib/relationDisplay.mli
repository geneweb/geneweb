open Config

val print :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person option ->
  unit
(** [print conf base p p1] entry point for the [m=R] route. Renders the
    relationship between [p1] and [p] when [p1] is provided; otherwise falls
    back to the [relmenu] template. Honours the [et=S] (shortest path), [et=M]
    (by-marriage) and [long=on] URL parameters. Reports a base loop via
    {!Hutil.rheader} when [Consang.TopologicalSortError] is raised during
    computation. *)

val print_multi : config -> Geneweb_db.Driver.base -> unit
(** [print_multi conf base] entry point for the [m=RLM] route. Collects persons
    from the [i1], [i2], ... URL parameters and renders the multi-person
    relationship DAG. Performs an HTTP redirect to a canonical URL when
    person-pool parameters (PNOC) are detected. *)
