(* Copyright (c) 1998-2007 INRIA *)

(** Resolution and HTML rendering of the kinship link (route [m=RL]). Provides
    the per-path back-end used by [RelationDisplay] when it delegates the
    rendering of an individual relation path. *)

open Config
open Def

type info = {
  ip : Geneweb_db.Driver.iper;
      (** Common ancestor from which the two branches descend. *)
  sp : sex;  (** Sex of the common ancestor [ip]. *)
  ip1 : Geneweb_db.Driver.iper;  (** First endpoint of the relation. *)
  ip2 : Geneweb_db.Driver.iper;  (** Second endpoint of the relation. *)
  b1 : (Geneweb_db.Driver.iper * sex) list;
      (** Descending branch from [ip] down to [ip1], as a sequence of
          [(iper, sex)] pairs. *)
  b2 : (Geneweb_db.Driver.iper * sex) list;
      (** Descending branch from [ip] down to [ip2]. *)
  c1 : int;
      (** Index of [b1] among the alternative branches reaching [ip1], used for
          prev/next navigation (1-based; [0] disables navigation on this side).
      *)
  c2 : int;  (** Index of [b2] among the branches reaching [ip2]. *)
  pb1 : (Geneweb_db.Driver.iper * sex) list option;
      (** Previous branch of same length on side 1, if any. *)
  pb2 : (Geneweb_db.Driver.iper * sex) list option;
      (** Previous branch of same length on side 2, if any. *)
  nb1 : (Geneweb_db.Driver.iper * sex) list option;
      (** Next branch of same length on side 1, if any. *)
  nb2 : (Geneweb_db.Driver.iper * sex) list option;
      (** Next branch of same length on side 2, if any. *)
  sp1 : Geneweb_db.Driver.person option;
      (** Optional terminal spouse displayed at the [ip1] endpoint (URL
          parameter [3]). *)
  sp2 : Geneweb_db.Driver.person option;
      (** Optional terminal spouse displayed at the [ip2] endpoint (URL
          parameter [4]). *)
  bd : int;  (** Border thickness in pixels (URL parameter [bd]). *)
  td_prop : Adef.safe_string;
      (** CSS [class="..."] attribute applied to cells when the [color] URL
          parameter is set; empty otherwise. *)
}
(** Parameters describing a single rendered relation path, populated from URL
    parameters by [print] and consumed by [print_relation_path]. *)

val make_dist_tab :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  int ->
  (Geneweb_db.Driver.iper -> int) * (Geneweb_db.Driver.iper -> int)
(** [make_dist_tab conf base ia maxlev] builds two functions [(dmin, dmax)] over
    the ancestors of [ia] up to depth [maxlev]. [dmin ip] (resp. [dmax ip])
    returns the minimum (resp. maximum) number of generations on an ascending
    path from [ip] to [ia], among paths of length at most [maxlev]. Used as
    input to [find_first_branch] to prune the search.

    Returns a phony table when [maxlev] is at or below the depth threshold read
    from the URL parameter [rel_threshold] (default [10]); this spares the
    topological sort on short paths. *)

val find_first_branch :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper -> int) * (Geneweb_db.Driver.iper -> int) ->
  Geneweb_db.Driver.iper ->
  int ->
  Geneweb_db.Driver.iper ->
  sex ->
  (Geneweb_db.Driver.iper * sex) list option
(** [find_first_branch conf base dist ia len ip sex] looks for the first
    ascending path from [ip] (of sex [sex]) to [ia] of length exactly [len],
    trying the paternal side before the maternal one and pruning with the [dist]
    table from [make_dist_tab]. Returns the path as a sequence of [(iper, sex)]
    pairs, or [None] when no path of the requested length exists. *)

val print_relation_path : config -> Geneweb_db.Driver.base -> info -> unit
(** [print_relation_path conf base info] renders the relation path described by
    [info] as HTML. Switches between a single-branch display (when one of
    [info.b1], [info.b2] is empty) and a two-branch display, and between a plain
    layout and a bordered [<table>] layout depending on whether [info.bd] is
    non-zero or [info.td_prop] is non-empty. *)

val print : config -> Geneweb_db.Driver.base -> unit
(** Entry point for the [m=RL] route. Parses URL parameters [1], [2]
    (endpoints), [dag] (rendering mode), [l1], [l2] (branch lengths), [b1], [b2]
    (branch Sosa numbers), [c1], [c2] (branch indices for prev/next), [3], [4]
    (terminal spouses), [bd], [color] (border and color decoration), [sib]
    (siblings flag), then dispatches to the DAG renderer when [dag=on] or to the
    branch renderer otherwise. *)
