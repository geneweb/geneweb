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

val string_of_marriage_text :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.family ->
  Adef.safe_string

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

val interp_notempl_with_menu :
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

val get_linked_page :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  Adef.safe_string

val get_birth_text :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val get_baptism_text :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val get_death_text :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val get_burial_text :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val get_cremation_text :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val get_marriage_date_text :
  config -> Geneweb_db.Driver.family -> bool -> Adef.safe_string

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

val string_of_died :
  config -> Geneweb_db.Driver.person -> bool -> Adef.safe_string

val string_of_parent_age :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person * bool ->
  (Geneweb_db.Driver.family -> Geneweb_db.Driver.iper) ->
  Adef.safe_string

val string_of_image_url :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person * bool ->
  bool ->
  bool ->
  Adef.escaped_string

val round_2_dec : float -> float

(* TODO put in lib/image.ml *)
val string_of_image_size :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person * bool -> string

val string_of_image_medium_size :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person * bool -> string

val string_of_image_small_size :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person * bool -> string

val get_link :
  generation_person list -> Geneweb_db.Driver.iper -> generation_person option

(**)

val infinite : int
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

val has_history :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool -> bool

val has_possible_duplications :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool

val string_of_title :
  ?safe:bool ->
  ?link:bool ->
  config ->
  Geneweb_db.Driver.base ->
  Adef.safe_string ->
  Geneweb_db.Driver.person ->
  int
  * Geneweb_db.Driver.istr Def.gen_title_name
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr list
  * (Def.date option * Def.date option) list ->
  Adef.safe_string
(** Optionnal [link] argument is passed to {!val:DateDisplay.string_of_ondate}
*)
