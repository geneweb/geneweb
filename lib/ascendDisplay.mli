(* Copyright (c) 1998-2007 INRIA *)
(** Ancestor display entry points. *)

val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** [print conf base p] renders the ancestor view selected by the current
    request parameters for person [p].

    Depending on the URL parameters, this may display the ancestor menu,
    ancestor lists, ancestor trees, fan charts, Sosa views, or ancestor DAGs. *)
