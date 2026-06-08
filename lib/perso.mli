(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

type generation_person =
  | GP_person of
      Geneweb_sosa.t * Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam option
  | GP_same of Geneweb_sosa.t * Geneweb_sosa.t * Geneweb_db.Driver.iper
  | GP_interv of
      (Geneweb_sosa.t
      * Geneweb_sosa.t
      * (Geneweb_sosa.t * Geneweb_sosa.t) option)
      option
  | GP_missing of Geneweb_sosa.t * Geneweb_db.Driver.iper

val interp_templ :
  ?no_headers:bool ->
  string ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  unit

val interp_templ_with_menu :
  (bool -> unit) ->
  string ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  unit

val print :
  ?no_headers:bool ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  unit
(** Displays the HTML page of a Geneweb_db.Driver.person *)

val linked_page_text :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  'a ->
  Adef.safe_string ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page
  * ('b * ('a * Def.NLDB.ind) list) ->
  Adef.safe_string

(**)

val limit_desc : config -> int

val make_desc_level_table :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper, int) Geneweb_db.Collection.Marker.t
  * (Geneweb_db.Driver.ifam, int) Geneweb_db.Collection.Marker.t

type dup =
  | DupFam of Geneweb_db.Driver.ifam * Geneweb_db.Driver.ifam
  | DupInd of Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
  | NoDup

type excl_dup =
  (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper) list
  * (Geneweb_db.Driver.ifam * Geneweb_db.Driver.ifam) list

val excluded_possible_duplications : config -> excl_dup

val first_possible_duplication :
  Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> excl_dup -> dup

(* Ajout pour l'API *)
val nobility_titles_list :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (int
  * Geneweb_db.Driver.istr Def.gen_title_name
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr list
  * (Adef.date option * Adef.date option) list)
  list

val print_isolated : config -> Geneweb_db.Driver.base -> unit
(** Display persons with no parents and no families, grouped into truly
    isolated, linked by relation (rparents), and referenced by other persons
    (related/witnesses). *)
