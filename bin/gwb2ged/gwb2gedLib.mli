val gwb2ged :
  Geneweb_db.Driver.base ->
  bool ->
  Gwexport.gwexport_opts ->
  (Geneweb_db.Driver.iper -> bool) * (Geneweb_db.Driver.ifam -> bool) ->
  unit
(** [gwb2ged base with_indexes opts sel] Converts a Geneweb database [base] to a
    GEDCOM file. * `with_indexes` specifies if indexes are printed or not; *
    `opts` are the export options * `sel` is a pair of selectors returned by the
    database export *)
