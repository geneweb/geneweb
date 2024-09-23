(* Copyright (c) 1998-2007 INRIA *)

val search_key_aux :
  (Config.config -> Gwdb.base -> Gwdb.person list -> string -> Gwdb.person list) ->
  Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.person list
(** [search_key_aux aux conf base str] search persons by misc name [str] (could be one of the mix). If result is empty tries to
    it search names with roman number instead of numerals (if they are present in the name). Applies [aux] on the result and removes
    all dublicates. Empty persons, persons with private names or persons to which there are no rights to access are not listed. *)

val search_by_name : Config.config -> Gwdb.base -> string -> Gwdb.person list
(** Search persons by name that has format {i "firstname surname"}. Dublicates are possible. Empty persons, persons with private
    names or persons to which there are no rights to access are not listed. *)

val search_partial_key :
  Config.config -> Gwdb.base -> string -> Gwdb.person list
(** Calls [search_key_aux] with [aux] fonction that makes calls to [search_by_name] if result is empty. *)

val search_by_sosa :
  conf:Config.config -> base:Gwdb.base -> sosa:Sosa.t -> Gwdb.person option

val search_by_key : Config.config -> Gwdb.base -> string -> Gwdb.person option
(** Same as [search_by_name] but search by key that has format {i "firstname.occ surname"}. *)

val search_approx_key : Config.config -> Gwdb.base -> string -> Gwdb.person list
(** Calls [search_key_aux] with [aux] fonction that filter result list and only persons whose
    [fname^sname] or one of their misc names are equal to key. *)

val print :
  Config.config ->
  Gwdb.base ->
  (Config.config -> Gwdb.base -> string -> Gwdb.person list -> unit) ->
  (Config.config -> string -> unit) ->
  unit
(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
      trouver la meilleure combinaison de résultat pour afficher la réponse
      la plus probable.                                                     *)

val persons_starting_with :
  conf:Config.config ->
  base:Gwdb.base ->
  filter:(Gwdb.person -> bool) ->
  first_name_prefix:string ->
  surname_prefix:string ->
  limit:int ->
  Gwdb.person list
