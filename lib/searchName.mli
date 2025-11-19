(* Copyright (c) 1998-2007 INRIA *)

open Config

type search_case =
  | NoInput
  | PersonName of string
  | SurnameOnly of string
  | FirstNameOnly of string
  | FirstNameSurname of string * string
  | ParsedName of {
      first_name : string option;
      surname : string option;
      oc : string option;
      original : string;
      format :
        [ `Space | `Slash | `Dot | `SlashSurname | `SlashFirstName | `DotOc ];
    }
  | InvalidFormat of string

type name_components = {
  first_name : string option;
  surname : string option;
  oc : string option;
  person_name : string option;
  case : search_case;
}

val extract_name_components : config -> name_components

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
    ["firstname surname"] (space-separated, last space is the separator).

    The search uses the surname index to find bearers of the surname, then
    filters by first name matching.

    Duplicates are possible (different occurrences with same name). Empty
    persons, persons with private names, or persons to which there are no access
    rights are filtered out.

    @param conf Base configuration
    @param base Genealogical database
    @param name Name string in format "firstname surname"
    @return List of matching persons *)

val search_by_sosa :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person option
(** [search_by_sosa conf base sosa_str] searches a person using their
    Sosa-Stradonitz ancestor number.

    This requires that a Sosa reference person (Sosa 1) has been defined in the
    base configuration.

    @param conf Base configuration (must have sosa_ref defined)
    @param base Genealogical database
    @param sosa_str String representation of the Sosa number
    @return [Some person] if found and Sosa reference exists, [None] otherwise
*)

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

val search_approx_key :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person list
(** [search_approx_key conf base key] searches by approximate key matching.

    Calls {!search_key_aux} with a filter function that only keeps persons whose
    concatenated first name and surname ([firstname ^ surname]) or one of their
    misc names/aliases equals the search key.

    This is more flexible than {!search_by_key} as it:
    - Doesn't require the occurrence number
    - Matches against aliases
    - Returns multiple matches if they exist

    @param conf Base configuration
    @param base Genealogical database
    @param key Approximate key string
    @return List of matching persons *)

val print :
  config ->
  Geneweb_db.Driver.base ->
  (config ->
  Geneweb_db.Driver.base ->
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

    Options [&p_order], [&p_exact] only apply to first name searches ([&p] and
    [&pn] with first name component). See {!type:opts} for details.

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
