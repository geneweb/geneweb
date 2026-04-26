(* Copyright (c) 1998-2007 INRIA *)
open Config
(** Name search engine for the [m=S] route.

    Parses the search request URL ([p], [n], [pn], [oc] parameters) into a
    {!name_components} record, dispatches to a series of search strategies
    (Sosa, Key, FullName, ApproxKey, PartialKey, Surname, FirstName) chosen
    according to which components were provided, and delegates rendering to
    {!Some} via the [specify] callback or the direct display helpers.

    Apostrophe variants of the query are handled internally by every search
    strategy; callers do not need to pre-expand them.

    A per-request {!Some.AliasCache.t} is created at the top of {!print} and
    threaded through the call chain so that persons matched via one of their
    aliases can be rendered with the matched alias suffix " [alias]" by the
    {!Some} display functions. *)

type search_case =
  | NoInput  (** No useful query parameter was provided. *)
  | PersonName of string
      (** Combined name string (the [pn] URL parameter) with no separator
          recognised by the parser; will be tried via the full name_order
          dispatch. *)
  | SurnameOnly of string
      (** Only a surname was provided (the [n] URL parameter, or [pn] parsed as
          surname-only). *)
  | FirstNameOnly of string
      (** Only a first name was provided (the [p] URL parameter, or [pn] parsed
          as first-name-only). *)
  | FirstNameSurname of string * string
      (** Both [p] and [n] URL parameters were provided. *)
  | ParsedName of {
      first_name : string option;
      surname : string option;
      oc : string option;
      original : string;
      format :
        [ `Space | `Slash | `Dot | `SlashSurname | `SlashFirstName | `DotOc ];
    }
      (** [pn] URL parameter parsed by {!parse_person_name}. The [format] field
          records which separator was recognised, driving the subsequent
          search-method ordering. *)
  | InvalidFormat of string
      (** [pn] could not be parsed; the offending string is preserved for
          diagnostic display. *)

type name_components = {
  first_name : string option;
  surname : string option;
  oc : string option;
  person_name : string option;
  case : search_case;
}
(** Parsed search request. Fields are populated from the [p], [n], [pn] and [oc]
    URL parameters; [case] is the dispatch-driving summary used by {!print}. *)

val extract_name_components :
  config -> Geneweb_db.Driver.base -> name_components
(** [extract_name_components conf base] parses the search request URL parameters
    [p] (first name), [n] (surname), and [pn] (combined person-name string) into
    a {!name_components} record describing what was provided and how to dispatch
    the search.

    The [base] argument is required to access the configured surname particles
    ({!Geneweb_db.Driver.base_particles}) so that a query like
    ["henri de foresta"] is recognised as fn = "henri", sn = "de foresta" — see
    {!parse_person_name} for the parsing rules.

    The seven cases:
    - [p], [n], [pn] all empty → [NoInput]
    - [pn] alone → parsed by {!parse_person_name}
    - [n] alone → [SurnameOnly]
    - [p] alone → [FirstNameOnly]
    - [p] and [n] → [FirstNameSurname]
    - [n] and [pn] → [SurnameOnly] (pn ignored, n wins)
    - [p] and [pn] → [PersonName] (pn merged with p)

    @param conf Search request configuration (env, base_env)
    @param base Genealogical database
    @return The parsed name components and dispatch case *)

val search_key_aux :
  (config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person list ->
  string ->
  Geneweb_db.Driver.person list) ->
  config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.person list
(** [search_key_aux aux conf base str] searches persons by misc name [str]
    (which could be one of the aliases or misc names). If result is empty, tries
    to search names with roman numerals converted from arabic numbers (if
    present in the name). Applies [aux] on the result and removes all
    duplicates.

    Empty persons, persons with private names, or persons to which there are no
    access rights are filtered out.

    @param aux Function to filter and process the intermediate result list
    @param conf Base configuration
    @param base Genealogical database
    @param str Name string to search
    @return List of matching persons (deduplicated) *)

val search_by_name :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person list
(** [search_by_name conf base name] searches persons by name in the format
    ["firstname surname"]. The first space splits the input: tokens before the
    first space form the first name, everything after is the surname. The search
    uses the surname index to find bearers of the surname, then filters by first
    name matching. Duplicates are possible (different occurrences with same
    name). Empty persons, persons with private names, or persons to which there
    are no access rights are filtered out.
    @param conf Base configuration
    @param base Genealogical database
    @param name Name string in format "firstname surname"
    @return List of matching persons *)

val search_by_key :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person option
(** [search_by_key conf base key] searches by unique person key in the format
    ["firstname.occ surname"] where [occ] is the occurrence number.

    Only one person may match such a format (keys are unique identifiers).

    Example: ["Jean.2 DUPONT"] searches for the second occurrence of Jean
    DUPONT.

    @param conf Base configuration
    @param base Genealogical database
    @param key Key string in format "firstname.occ surname"
    @return [Some person] if exactly one person matches, [None] otherwise *)

val print :
  config ->
  Geneweb_db.Driver.base ->
  (config ->
  Geneweb_db.Driver.base ->
  Some.AliasCache.t ->
  string ->
  Geneweb_db.Driver.person list ->
  Geneweb_db.Driver.person list ->
  Geneweb_db.Driver.person list ->
  unit) ->
  unit
(** [print conf base specify] is the main search entry point.

    Analyzes request parameters ([&p], [&n], [&pn]) and dispatches to
    appropriate search strategies.

    {b Search parameter formats:}
    - [&p=firstname]: First name only search
    - [&n=surname]: Surname only search
    - [&p=fn&n=sn]: Combined first name and surname search
    - [&pn=name]: Parsed name with multiple formats:
    - ["fn sn"]: Space-separated (last space = separator)
    - ["fn/sn"]: Slash-separated
    - ["fn.sn"]: Dot-separated
    - ["fn.oc sn"]: Dot-separated with occurrence number
    - ["/sn"]: Surname only (slash prefix)
    - ["fn/"]: First name only (slash suffix)

    {b Search methods executed in order:}
    + Sosa: Sosa-Stradonitz ancestor number
    + Key: Unique person key (FirstName.Occ SURNAME)
    + FullName: Combined first name + surname (exact then fuzzy)
    + ApproxKey: Approximate key matching (handles aliases)
    + PartialKey: Partial name matching
    + Surname: Surname search (exact then phonetic)
    + FirstName: First name search with options

    {b Search options:}

    Options [&p_order], [&p_exact] only apply to first-name searches ([&p] and
    [&pn] with first-name component). See the search engine internals for the
    full option set.

    {b Result handling:}
    - Single exact match: Display person directly (calls [Perso.print])
    - Multiple exact matches: Call [specify] callback to disambiguate
    - No matches: Display welcome page
    - Mixed results: Separate by category (exact/partial/spouse)

    @param conf Base configuration and request parameters
    @param base Genealogical database
    @param specify
      Callback for displaying multiple match disambiguation, receives: conf,
      base, query, exact_list, partial_list, spouse_list *)
