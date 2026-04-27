(** Display module for the search-name pipeline.

    Renders the result pages produced by {!SearchName.print}: the "please
    specify" page when a search is ambiguous, the alphabetic surname/first-name
    listings, the surname-by-branch view, and the related navigation chrome.

    Most entry points take an {!AliasCache.t} threaded from the search engine so
    that persons matched via one of their aliases can be rendered with the
    matched alias suffix " [alias]". *)

module AliasCache : sig
  type t

  val create : unit -> t
  (** Per-request cache mapping iper to an optional alias string. Used to
      display [alias] tags next to person names in result lists when the person
      matched a query via one of their aliases. *)

  val add_alias : t -> Geneweb_db.Driver.Iper.t -> string -> unit
  (** Register that this iper matched via the given alias string. *)

  val add_direct : t -> Geneweb_db.Driver.Iper.t -> unit
  (** Register that this iper matched directly (not via an alias). *)

  val get_alias : t -> Geneweb_db.Driver.Iper.t -> string option
  (** [Some alias] if the iper matched via that alias, [None] otherwise
      (including for ipers never registered). *)
end

val surname_not_found : Config.config -> string -> unit
(** [surname_not_found conf x] renders a "surname not found" page for the given
    query [x]. *)

val persons_of_fsname :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.istr list) ->
  (Geneweb_db.Driver.istr -> Geneweb_db.Driver.iper list) ->
  (Geneweb_db.Driver.person -> Geneweb_db.Driver.istr) ->
  string ->
  (string * Geneweb_db.Driver.istr * Geneweb_db.Driver.iper list) list
  * (string -> string)
(** [persons_of_fsname conf base base_strings_of_fsname find proj x] function
    where:
    - [base_strings_of_fsname base x] is a function that returns the list of
      first/surnames (as istr) being equal to [x]
    - [find istr] is a function that returns the list of persons having [istr]
      as a first/surname id
    - [proj iper] is a function that returns first/surname id from the giving
      person id.
    - [x] is a first/surname or its substring. Returns [(l,inj)] where [l] is a
      list of [(str,istr,iperl)] where [istr] is id of [str] and [iperl] is a
      list of persons found that has [istr] as a first/surname such that
      [str = inj x] *)

val specify :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  string ->
  Geneweb_db.Driver.person list ->
  Geneweb_db.Driver.person list ->
  Geneweb_db.Driver.person list ->
  unit
(** [specify conf base alias_cache query pl1 pl2 pl3] renders the "please
    specify" page when a search returns multiple matching persons.
    - [pl1] contains direct or alias matches on the first name.
    - [pl2] contains additional surname-only matches.
    - [pl3] contains persons matched via their spouse's name. Each pile is
      sorted (by birth date by default, by first name when [sort_fn=on] is in
      [conf.env]) and rendered as a separate section. Persons that matched via
      an alias display the matched alias next to their name when [alias_cache]
      has been populated upstream. *)

val search_surname_print :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  (Config.config -> string -> unit) ->
  string ->
  unit
(** [search_surname_print conf base alias_cache not_found_fun x] renders the
    surname listing page for the query [x]. If no surname matches,
    [not_found_fun conf x] is invoked to produce the not-found page — callers
    pass {!surname_not_found} for the standard message or [(fun _ _ -> ())] to
    suppress it (e.g. when the empty result is expected and another section will
    be displayed).

    Honours the alternate query parameters [v1], [v2], ... that allow several
    candidate spellings to be searched in one request. *)

val first_name_print_list_multi :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  string ->
  (int * (string * Geneweb_db.Driver.person list) list * bool * Mutil.StrSet.t)
  list ->
  unit
(** [first_name_print_list_multi conf base alias_cache query sections_groups]
    renders the first-name listing page split into sections.

    Each entry of [sections_groups] is a tuple [(kind, sections, rev, variants)]
    where:
    - [kind] is the section identifier (0 = exact, 1 = aliases, 2 = included, 3
      = phonetic, 4 = permuted), driving the section header, anchor id, and
      ordering;
    - [sections] is the per-letter grouping of persons to render;
    - [rev] indicates whether to display surname before first name;
    - [variants] is the set of orthographic variants captured for that section,
      displayed under the header as clickable links. *)

val print_several_possible_surnames :
  string ->
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  'a * (string * Geneweb_db.Driver.person list) list ->
  unit
(** [print_several_possible_surnames query conf base alias_cache (_, groups)]
    renders the disambiguation page when a query matches several distinct
    surnames. Each group is rendered as a clickable surname entry leading to its
    dedicated listing.

    The first tuple component is currently unused; it is kept for historical
    compatibility with callers that may pass alternate-query metadata. *)

val print_surname_details :
  Config.config ->
  Geneweb_db.Driver.base ->
  AliasCache.t ->
  string ->
  (string * Geneweb_db.Driver.person list) list ->
  unit
(** [print_surname_details conf base alias_cache query surnames_groups] renders
    the [m=SN] page: a detailed view of each candidate surname found by the
    query, with all matching persons listed under their surname header rather
    than via the multi-page navigation used by
    {!print_several_possible_surnames}. *)

val name_unaccent : string -> string
(** calls Name.unaccent_utf8 false (no lower) on all characters of the string *)
