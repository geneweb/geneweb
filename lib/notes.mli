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
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string ->
  unit

val update_notes_links_person :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, _, Geneweb_db.Driver.istr) Def.gen_person ->
  unit

val update_notes_links_family :
  Geneweb_db.Driver.base ->
  (_, Geneweb_db.Driver.ifam, Geneweb_db.Driver.istr) Def.gen_family ->
  unit

val file_path : Config.config -> Geneweb_db.Driver.base -> string -> string

val read_notes :
  Geneweb_db.Driver.base -> string -> (string * string) list * string

val merge_possible_aliases :
  Config.config ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list

type display_name = { df_first_name : string; df_surname : string }
(** The real, case-preserved (first name, surname) of a person, for building the
    text written back into a note. Deliberately distinct from [Def.NLDB.key],
    which is always lower-cased (comparison/cache key) and must never be used
    for that purpose. *)

val update_ind_key :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  Def.NLDB.key ->
  string * string * int ->
  display_name ->
  unit

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

val cache_linked_pages_name : string

val update_cache_linked_pages :
  Config.config -> mode -> Def.NLDB.key -> Def.NLDB.key -> int -> unit

val on_person_saved :
  Config.config ->
  Geneweb_db.Driver.base ->
  old_key:Def.NLDB.key ->
  pgl:(Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  Def.gen_person ->
  unit
(** [on_person_saved conf base ~old_key ~pgl p], called right after
    [Driver.patch_person] for a person that already existed before this save -
    an ordinary edit (updateIndOk.ml, updateField.ml), NOT a brand-new person
    (call [update_notes_links_person] directly for that) and NOT mergeIndOk.ml's
    merge (which collapses two old keys into one new one and keeps its own
    direct sequence for that reason): unconditionally re-indexes [p]'s own
    note-bearing fields into nldb, then, if [p]'s key differs from [old_key],
    rewrites every page in [pgl] (the pages that referenced [old_key], as
    computed by the caller via [links_to_ind] for its own "linked pages"
    display) to the new key/name and refreshes the linked-pages count cache.
    This is the single place callers should go through for this sequence instead
    of reimplementing it by hand. *)

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
