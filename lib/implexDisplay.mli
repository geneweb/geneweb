val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** [print conf base p] displays the pedigree collapse and inbreeding analysis
    of the ancestry of [p]: summary, generation profile, junctions with their
    loops, clusters and most repeated ancestors, with links to the DAG and
    relationship graphs. Parameter [v] limits the displayed generations and
    junctions, [n] the number of listed ancestors (50 by default). *)
