val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] draws the persons selected like [m=DAG] ([i1], [s1]…) as a
    layered graph, each person once, families as union points, and reports the
    layout metrics with a link to the equivalent [m=DAG]. Prototype.

    Person layers are spread over equal slots (see {!Layered.layout});
    [spread=0] restores the first trial, centred on the neighbours. Each union
    has its own colour, shared by its edges, and every edge is drawn over a halo
    of the page background, so that at a crossing the later edge passes over the
    earlier one. *)
