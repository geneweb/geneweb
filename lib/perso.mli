(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type generation_person =
  | GP_person of Sosa.t * Gwdb.iper * Gwdb.ifam option
  | GP_same of Sosa.t * Sosa.t * Gwdb.iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * Gwdb.iper

val interp_templ :
  ?no_headers:bool ->
  string ->
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  unit

val interp_templ_with_menu :
  (bool -> unit) -> string -> Config.config -> Gwdb.base -> Gwdb.person -> unit

val interp_notempl_with_menu :
  (bool -> unit) -> string -> Config.config -> Gwdb.base -> Gwdb.person -> unit

val print :
  ?no_headers:bool -> Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays the HTML page of a person *)

val print_ascend : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays the ascendants of the selected person *)

val print_what_links : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays links to pages associated to the person *)

val links_to_ind :
  Config.config ->
  Gwdb.base ->
  ((Gwdb.iper, Gwdb.ifam) Def.NLDB.page
  * ('a * ((string * string * int) * 'b) list))
  list ->
  string * string * int ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page list

val get_linked_page :
  Config.config -> Gwdb.base -> Gwdb.person -> string -> Adef.safe_string

val get_birth_text : Config.config -> Gwdb.person -> bool -> Adef.safe_string
val get_baptism_text : Config.config -> Gwdb.person -> bool -> Adef.safe_string
val get_death_text : Config.config -> Gwdb.person -> bool -> Adef.safe_string
val get_burial_text : Config.config -> Gwdb.person -> bool -> Adef.safe_string

val get_cremation_text :
  Config.config -> Gwdb.person -> bool -> Adef.safe_string

val get_marriage_date_text :
  Config.config -> Gwdb.family -> bool -> Adef.safe_string

val get_marriage_witnesses_and_notes :
  Gwdb.family -> (Gwdb.iper * Def.witness_kind * Gwdb.istr) array

val linked_page_text :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  string ->
  'a ->
  Adef.safe_string ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page * ('b * ('a * Def.NLDB.ind) list) ->
  Adef.safe_string

(**)

val limit_desc : Config.config -> int

val make_desc_level_table :
  Config.config ->
  Gwdb.base ->
  int ->
  Gwdb.person ->
  (Gwdb.IperSet.elt, int) Gwdb.Marker.t * (Gwdb.ifam, int) Gwdb.Marker.t

type dup =
  | DupFam of Gwdb.ifam * Gwdb.ifam
  | DupInd of Gwdb.iper * Gwdb.iper
  | NoDup

type excl_dup = (Gwdb.iper * Gwdb.iper) list * (Gwdb.ifam * Gwdb.ifam) list

val excluded_possible_duplications : Config.config -> excl_dup
val first_possible_duplication : Gwdb.base -> Gwdb.iper -> excl_dup -> dup

(* Ajout pour l'API *)
val nobility_titles_list :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (int
  * Gwdb.istr Def.gen_title_name
  * Gwdb.istr
  * Gwdb.istr list
  * (Date.date option * Date.date option) list)
  list

val has_history : Config.config -> Gwdb.base -> Gwdb.person -> bool -> bool

val has_possible_duplications :
  Config.config -> Gwdb.base -> Gwdb.person -> bool

val string_of_title :
  ?safe:bool ->
  ?link:bool ->
  Config.config ->
  Gwdb.base ->
  Adef.safe_string ->
  Gwdb.person ->
  int
  * Gwdb.istr Def.gen_title_name
  * Gwdb.istr
  * Gwdb.istr list
  * (Date.date option * Date.date option) list ->
  Adef.safe_string
(** Optionnal [link] argument is passed to {!val:DateDisplay.string_of_ondate}
*)
