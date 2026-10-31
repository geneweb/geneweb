(** Analysis of the source strings: signatures, sources and dates.

    A source string is usually a list of signatures separated by commas, each
    signature being

    {[
    wizard source date
    ]}

    where the wizard is written [[[w:ident]]] or as plain text, the source may
    be between parentheses, and the date takes many forms ([24 ii 2021],
    [28 VIII 2020], [08.04.2022], [25/11/04], [4xii2018], [28 avril 07]...).
    Commas also occur inside a source ([AD Vosges, AC Gérardmer]): a string is
    only cut at a comma followed by a wizard. A plain-text wizard is recognized
    when it is a known wizard: the ident of a [[[w:ident]]], or a text often
    found before a [(] at the start of a source string. Every part keeps its
    span in the raw string, so it can be rewritten in place.

    The wizards and the sources are then grouped in clusters of spellings:
    ["Gilles Francez"] and ["[[w:G.Francez]]"], ["AD 75"] and ["AD75"]. *)

type span = { text : string; pos : int; len : int }
(** A part of a raw string: [text] is [String.sub raw pos len]. *)

type segment = {
  wizard : span option;  (** ["[[w:ident]]"] as a whole, or the plain name *)
  source : span option;  (** without its parentheses *)
  date : span option;
}

type known
(** The known wizards. *)

val known_of_strings : ?threshold:int -> string list -> known
(** The idents of the [[[w:ident]]] of the strings, and the texts found at least
    [threshold] times (default 20) before a [(] at their start. *)

val known_names : known -> string list
val parse : known -> string -> segment list

type part = Wizard | Source

val part_of_string : string -> part option
(** ["w"] or ["s"] *)

val string_of_part : part -> string
val spans : part -> segment list -> span list

val wizard_key : string -> string
(** Cluster key of a wizard: case, accents and punctuation ignored, the first
    word reduced to its initial: ["Gilles Francez"], ["G.Francez"],
    ["[[w:G.Francez]]"] give ["gfrancez"]. *)

val source_key : string -> string
(** Cluster key of a source: letters and digits only, lowered and without
    accents: ["AD 75"], ["AD75"], ["ad-75"] give ["ad75"]. *)

val key : part -> string -> string

type spelling = {
  text : string;
  strings : int;  (** number of source strings containing it *)
  uses : int;  (** persons and families using these strings *)
}

type cluster = {
  part : part;
  ckey : string;
  spellings : spelling list;  (** most used first *)
  total : int;  (** uses of the cluster *)
}

type data = {
  known : known;
  strings : (Geneweb_db.Driver.istr * string * int) list;
      (** the source strings used, with their uses *)
}

val load : Config.config -> Geneweb_db.Driver.base -> data
(** One pass over the persons and families, then the analysis of the strings. *)

val of_strings : (Geneweb_db.Driver.istr * string * int) list -> data

val clusters : data -> part -> cluster list
(** The clusters of two spellings or more, by decreasing uses. *)

val samples :
  data ->
  part ->
  string list ->
  int ->
  (string * (string * span list) list) list
(** [samples d part spellings n]: for each spelling, up to [n] strings
    containing it, with its spans. *)

val plan :
  Geneweb_db.Driver.base ->
  data ->
  part ->
  terms:string list ->
  new_term:string ->
  (PlaceRename.item * span list) list
(** Replacing the spellings [terms] by [new_term] in every source string, with
    the replaced spans of each old string. *)
