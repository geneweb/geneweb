(* Copyright (c) 1998-2007 INRIA *)

(** Place-string manipulation for the Places-Persons-Surnames (PPS) index.

    A GeneWeb place string has the shape

    {[
    [ suburb ] - town subdivision, region, country
    ]}

    where the bracketed suburb and the parenthesised subdivision are optional.
    This module splits such strings into their components, normalises them, and
    scans the base to build the raw-place -> persons/surnames array consumed by
    {!PlaceDisplay}. All rendering lives in {!PlaceDisplay}; this module is pure
    apart from {!get_all}, which reads the base. *)

open Config
open Geneweb_db.Driver

val split_suburb : string -> string * string
(** [split_suburb "[foo-bar] - boobar (baz)"] is [("foo-bar", "boobar (baz)")].
    A string with no bracketed suburb yields [("", s)]. *)

val only_suburb : string -> string
(** [only_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar"];
    [only_suburb "boobar (baz)"] is [""]. *)

val without_suburb : string -> string
(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"]; a string
    with no suburb is returned unchanged. *)

val normalize : string -> string
(** [normalize s] rewrites a bracketed suburb into a leading comma-separated
    component: ["[foo] - bar"] becomes ["foo, bar"]. Strings with no suburb are
    returned unchanged. *)

val compare_places : string -> string -> int
(** Total order on raw place strings: compares the comma-separated components of
    the suburb-stripped part first, then the suburbs. *)

val fold_place_long : bool -> string -> string list * string
(** [fold_place_long inverted s] parses [s] into [(components, suburb)]. The
    suburb is dropped from [components]; the remainder is split on commas and on
    a trailing parenthesised subdivision, so ["town (sub)"] yields
    [[sub; town]]. When [inverted] is [true] the component list is reversed (for
    bases that record places country-first, [places_inverted=yes] in the
    [.gwf]). *)

val places_to_string : bool -> string list -> string
(** [places_to_string inverse pl] renders a component list back to a
    comma-separated string. When [inverse] is [true] the list is reversed before
    rendering. *)

val normalize_place : bool -> string -> string
(** [normalize_place inverted s] runs the raw place [s] through the exact same
    pipeline as the m=PPS place key ({!fold_place_long} then rendered
    child-first), so the result is byte-comparable with the key emitted in the
    m=L marker URLs. Example: ["Paris (75)"] -> ["Paris, 75"]. *)

val max_rlm_nbr : config -> int
(** Maximum number of persons for which an m=RLM relationship graph is computed.
    Read from the ["max_rlm_nbr"] request parameter, then the base [.gwf], then
    a built-in default. *)

exception List_too_long
(** Raised by {!get_all} in long-display mode once the number of distinct places
    exceeds the [max_length] threshold, letting the caller fall back to short
    display. *)

val get_all :
  config ->
  base ->
  add_birth:bool ->
  add_baptism:bool ->
  add_death:bool ->
  add_burial:bool ->
  add_marriage:bool ->
  'key ->
  'value ->
  (string -> 'key) ->
  ('key -> bool) ->
  ('acc option -> person -> 'acc) ->
  ('acc -> 'value) ->
  int ->
  ('key * 'value) array
(** [get_all conf base ~add_birth .. dummy_key dummy_value fold_place filter
     mk_value fn max_length] scans [base] once and returns an array mapping each
    selected place key to its accumulated value.

    - [fold_place] turns a raw place string into the key (typically
      {!fold_place_long}); [filter] keeps only the keys of interest.
    - [mk_value] folds each matching person into an accumulator ([None] the
      first time a key is seen), and [fn] finalises the accumulator into the
      stored value.
    - [dummy_key] / [dummy_value] seed the result array before it is filled.
    - Only persons and families passing {!Util.authorized_age} are counted, and
      the [add_*] flags select which events contribute.

    Raises {!List_too_long} in long-display mode once more than [max_length]
    distinct keys are seen. *)

val find_in : config -> string list -> string -> bool
(** [find_in conf components ini] tests whether the query [ini] matches the
    place [components]. Honours the ["word"] (whole word), ["case"] (case
    sensitive) and ["any"] (match any component) request parameters, and splits
    [ini] on commas or a parenthesised tail so a multi-part query can match
    across components. *)
