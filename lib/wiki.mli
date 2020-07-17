(* Copyright (c) 1998-2007 INRIA *)

open Config

type document

type notelink =
  | WLpage of (string list * string) * string * string * string
  | WLperson of int * Def.NLDB.key * string * string option
  | WLwizard of string * string

val doc_of_string : string -> document

val empty : document

(** [add_h1 s doc]
    Added [s] as a first level header at the beginning of [doc].
*)
val add_h1 : string -> document -> document

(** Extract GeneWeb links from a document  *)
val notelinks : document -> notelink list

(** Character use in GeneWeb wiki syntax as directory separator in internal links. *)
val char_dir_sep : char

(** [check_file_name s]
    checks if [s] only contains allowed characters
    and decompose the file in two parts: path and filename. *)
val check_file_name : string -> (string list * string) option

(** [extract_sub_part s n]
    Extract the [n]-th subpart of wikitext string [s].
    Each title define a new subpart in the document.
    Subparts include other subparts of lower level.
    Subpart count starts from 1.

    e.g. For this document:

    == H2 ==
    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    === H3 ===
    Nulla eleifend libero vitae nisi imperdiet auctor.
    ==== H4 ====
    Aenean luctus est interdum eleifend sollicitudin.
    === H3 ===
    Pellentesque quis tortor cursus risus tincidunt auctor.
    == H2 ==
    Nullam nec nisl eget est ornare condimentum.

    Subpart 2 is:
    == H3 ==
    Nulla eleifend libero vitae nisi imperdiet auctor.
    == H4 ==
    Aenean luctus est interdum eleifend sollicitudin.

    Subpart 3 is:
    == H4 ==
    Aenean luctus est interdum eleifend sollicitudin.

    Subpart 4 is:
    == H3 ==
    Pellentesque quis tortor cursus risus tincidunt auctor.

*)
val extract_sub_part : string -> int -> string

val split_title_and_text : string -> (string * string) list * string

(** [html_of_wiki conf base s]
    Convert [s], which is wikitext into its HTML representation.
    If edit options are provided, a link to modify/view each subpart is displayed
    according to the user rigths: [(can_edit, mode, source_filename)]
*)
val html_of_wiki : ?edit:(bool * string * string) -> config -> Gwdb.base -> string -> string

(** [interp conf base ?env ?unsafe s]
    Util.safe_html ∘ html_of_wiki ∘ Util.string_with_macros
*)
val interp
  : ?edit:(bool * string * string)
  -> config
  -> Gwdb.base
  -> ?env:(char * (unit -> string)) list
  -> ?unsafe:bool
  -> string
  -> string

(** [interp_inline conf base ?env ?unsafe s]
    Same as {!val:interp}, but for inline wiki instead of block.
*)
val interp_inline
  : config
  -> Gwdb.base
  -> ?env:(char * (unit -> string)) list
  -> ?unsafe:bool
  -> string
  -> string

val notes_aliases : config -> (string * string) list

val map_notes : (string * string) list -> string -> string

val file_path : Config.config -> Gwdb.base -> string -> string

(**/**)
(** Do not use. Expose for internal usage only. *)
val first_cnt : int
val of_wktxt : ?cnt:int ref -> Wikitext.Type.document -> document
val replace_toc : string -> document -> document
val to_wktxt : Config.config -> Gwdb.base -> document -> Wikitext.Type.document
(**/**)
