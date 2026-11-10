(** Dictionaries of a base: the distinct strings of a kind of field (first
    names, places, sources...) listed by m=MOD_DATA and checked by chk_data, the
    fields of persons and families holding them, and their sort key. *)

type t =
  | First_name
  | Surname
  | First_name_alias
  | Surname_alias
  | Public_name
  | Qualifier
  | Alias
  | Occupation
  | Place
  | Source
  | Title
  | Domain
  | Place_last
      (** the places again, sorted from their last term (see {!last_key}); not
          listed among the books, the order "from the last term" of the book of
          places *)

val all : t list
(** Every dictionary, first names first. *)

val to_string : t -> string
(** Name of the dictionary in URLs ([data] parameter): ["fn"], ["sn"], ["fna"],
    ["sna"], ["pubn"], ["qual"], ["alias"], ["occu"], ["place"], ["src"],
    ["title"], ["domain"], ["place_last"]. *)

val of_string : string -> t option
(** Inverse of {!to_string} for the dictionaries that are books of [m=MOD_DATA]:
    [Place_last], only another order of the book of places, has no [data] value.
*)

val person_strings :
  t -> ('iper, 'person, 'string) Def.gen_person -> 'string list
(** Strings of the fields of a person belonging to the dictionary, those of its
    events included. The list may hold duplicates and empty strings. *)

val family_strings :
  t -> ('person, 'ifam, 'string) Def.gen_family -> 'string list
(** Same as {!person_strings} for a family. Only places and sources have family
    fields. *)

val records_dict : t -> t
(** The dictionary holding the same strings in the same records: [Place] for
    [Place_last], which only sorts them otherwise; the dictionary itself for the
    others. The index writes the records of such dictionaries once. *)

val has_family_fields : t -> bool
(** Whether families hold strings of the dictionary: places and sources. *)

val suburb : string -> (int * int) option
(** [suburb s] is [Some (i, j)] when [s] is a place with a suburb, written
    ["[suburb] - place"] (hyphen, en dash or em dash): [i] is the index of the
    closing bracket and [j] the start of the place. [None] otherwise. *)

val without_suburb : string -> string
(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"]; a string
    with no suburb is returned unchanged. *)

val paren_form : string -> (string * string) option
(** [paren_form "aaaa (bbbb)"] is [Some ("aaaa", "bbbb")]: a term ending with a
    parenthesised part, read from its last opening parenthesis, both parts
    trimmed and non empty; [None] otherwise. The one definition of this form,
    for the order of the book of places and for {!Geneweb.PlaceRename}. *)

val last_terms : string -> string list
(** [last_terms s] lists the terms of the place [s] read from its end, the
    suburb dropped; a term ["aaaa (bbbb)"], wherever it sits, counts as
    ["aaaa, bbbb"]. Example: ["[Boileau] - Granville (Manche), France"] gives
    [["France"; "Manche"; "Granville"]]. *)

val last_terms_sep : string
(** [", "], the separator joining the terms of {!last_key}. *)

val last_key : string -> string
(** [last_key s] is [String.concat last_terms_sep (last_terms s)]: the place as
    displayed in the order from its last term. *)

val key : Re.re -> t -> string -> string
(** [key particles dict s] is the sort key of [s] in [dict], compared byte by
    byte, so that the entries whose displayed text starts with a prefix form an
    interval of keys starting with that prefix:

    - surnames and domains: the name without its particle ([particles], see
      {!Mutil.get_particle}), a null byte, then the particle. Their order is the
      one of {!Mutil.compare_after_particle}.
    - places: the place without its suburb, a null byte, then the whole string
      if it has a suburb. A place comes before its suburbs.
    - places from their last term: {!last_key}, a null byte, then the whole
      string.
    - other dictionaries: the string itself, in the order of [String.compare].

    Keys are distinct for distinct strings. *)
