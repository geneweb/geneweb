val commit_notes :
  Config.config -> Geneweb_db.Driver.base -> string -> string -> unit

val notes_links_db :
  Config.config ->
  Geneweb_db.Driver.base ->
  bool ->
  (Mutil.StrSet.elt
  * (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list)
  list

val update_notes_links_db :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string ->
  unit
(** Re-scans [who]'s note-bearing text into nldb and adjusts the linked-pages
    cache by the resulting difference in referenced keys (added and removed), so
    the cache stays exact between two runs of update_nldb instead of drifting on
    every edit. *)

val has_links : string -> bool
(** Whether a text contains a double opening bracket, i.e. may hold a link
    indexed in nldb. *)

val notes_bearing_text_of_person :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, _, Geneweb_db.Driver.istr) Def.gen_person ->
  string
(** Concatenation of the person's note-bearing fields, as scanned for links. *)

val notes_bearing_text_of_family :
  Geneweb_db.Driver.base ->
  (_, Geneweb_db.Driver.ifam, Geneweb_db.Driver.istr) Def.gen_family ->
  string
(** Concatenation of the family's note-bearing fields, as scanned for links. *)

val update_notes_links_person :
  Config.config ->
  ?old_text:string ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, _, Geneweb_db.Driver.istr) Def.gen_person ->
  unit
(** Re-scans a person's note-bearing fields into nldb. [?old_text] is the text
    [notes_bearing_text_of_person] returned before the edit ([""] for a person
    that did not exist); when given, the full nldb rewrite is skipped if the
    text is unchanged or neither version contains a link. Omit it to force a
    rescan. *)

val update_notes_links_family :
  Config.config ->
  ?old_text:string ->
  Geneweb_db.Driver.base ->
  (_, Geneweb_db.Driver.ifam, Geneweb_db.Driver.istr) Def.gen_family ->
  unit
(** Same as [update_notes_links_person] for a family. *)

val file_path : Config.config -> Geneweb_db.Driver.base -> string -> string

val read_notes :
  Geneweb_db.Driver.base -> string -> (string * string) list * string

val merge_possible_aliases :
  Config.config ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list

val update_ind_key :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  Def.NLDB.key ->
  string * string * int ->
  unit
(** [update_ind_key conf base pgl oldk newk] rewrites the links to [oldk] in
    every page of [pgl] so they designate [newk]. [oldk] is lower-cased (as
    built by [Util.make_key]); [newk] carries the case-preserved first name and
    surname, since it is written back into the notes. *)

val source :
  Config.config -> Geneweb_db.Driver.base -> string -> Adef.safe_string
(** [source conf base str] Interprets wiki syntax in a "source" context:
    - supposed to be one line
    - no <p> surrounding tag *)

val note :
  Config.config ->
  Geneweb_db.Driver.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [note conf base env str] Interprets wiki syntax in a "note" context:
    - [env] is available during [str] interpretation *)

val person_note :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  Adef.safe_string
(** [person_note conf base person str] Interprets wiki syntax in a "note"
    context:
    - env is available during [str] interpretation with [i] variable bound to
      person image *)

val source_note_with_env :
  Config.config ->
  Geneweb_db.Driver.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [source_note_with_env conf base env str] Interprets wiki syntax in a
    "source" context with a predefined env. *)

val wiki_of_source :
  Config.config ->
  Geneweb_db.Driver.base ->
  always_show_link:bool ->
  Geneweb_db.Driver.person ->
  string ->
  string
(** [wiki_of_source conf base ~always_show_link p s] renders source string [s]
    to HTML in the "NOTES" wiki context, marking links to non-public persons;
    [p] provides the [%i]/[%k] macro env and the person-existence check. *)

type mode = Delete | Rename | Merge

val links_to_ind :
  Config.config ->
  Geneweb_db.Driver.base ->
  ((Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page
  * (string list * (Def.NLDB.key * Def.NLDB.ind) list))
  list ->
  Def.NLDB.key ->
  string option ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list

val links_to_cache_entries :
  Config.config ->
  Geneweb_db.Driver.base ->
  ((Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page
  * (string list * (Def.NLDB.key * Def.NLDB.ind) list))
  list ->
  Def.NLDB.key ->
  (Def.NLDB.key * Def.NLDB.ind) list

val linked_pages_nbr :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int

val count_linked_pages : Geneweb_db.Driver.base -> Def.NLDB.key -> int
(** [count_linked_pages base key] is the number of nldb pages holding at least
    one link to [key], without alias merging nor access filtering, i.e. the
    value [update_nldb] stores in the linked-pages cache. *)

val cache_linked_pages_name : string

val update_cache_linked_pages :
  Config.config -> mode -> Def.NLDB.key -> Def.NLDB.key -> int -> unit
(** [update_cache_linked_pages conf mode old_key new_key nbr] updates the
    linked-pages cache if it exists: [Delete] removes [old_key] ([new_key] and
    [nbr] are ignored); [Rename] and [Merge] remove [old_key] and set [new_key]
    to [nbr]. The file is rewritten only when an entry changes. *)

val on_person_saved :
  Config.config ->
  Geneweb_db.Driver.base ->
  old_key:Def.NLDB.key ->
  ?old_text:string ->
  pgl:
    (unit ->
    (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list) ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  Def.gen_person ->
  unit
(** [on_person_saved conf base ~old_key ?old_text ~pgl p], called right after
    [Driver.patch_person] for an existing person, re-indexes [p]'s note-bearing
    fields (see [update_notes_links_person] for [?old_text]); if [p]'s key
    differs from [old_key], rewrites the pages returned by [pgl ()] and updates
    the linked-pages cache. [pgl] is only forced on a rename. *)

val json_extract_img : Config.config -> string -> string * string

val json_gallery_items_for_key :
  Config.config ->
  string ->
  Def.NLDB.key ->
  (int * string * string * string) list
(** [json_gallery_items_for_key conf s key] returns
    [(img_index_1based, img_url, img_file, desc)] for each gallery image whose
    map contains person [key]. *)

val safe_gallery : Config.config -> Geneweb_db.Driver.base -> string -> string

val get_linked_page_family :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  string ->
  Adef.safe_string
(** [get_linked_page_family conf base ifam tag] returns backlink HTML for family
    [ifam] and header tag [tag] (e.g. "MARIAGE"). *)

val mark_pnocs_validity : Geneweb_db.Driver.base -> string -> string
(** [mark_pnocs_validity base s] augments the gallery JSON [s]: every map entry
    that designates a person ([t] empty or ["p"]) and has both [fn] and [sn]
    non-empty receives an additional ["valid"] boolean field indicating whether
    the [(fn, sn, oc)] triple resolves to a person via [Driver.person_of_key]
    (lowercased keys, default [oc=0]). Entries for GeneWeb internal ([t="g"]) or
    external ([t="e"]) links, and entries with incomplete keys, are returned
    unchanged. Used by both the gallery viewer and the gallery editor to seed
    client-side validity state without requiring per-row API round-trips at page
    load. *)
