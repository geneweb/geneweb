(** Renaming a term of the place strings.

    A place is one string id ([istr]) shared by every record that uses it. A
    term is renamed by computing the new string of each place containing it,
    keeping the rest of the string byte for byte (see {!Place.components}), then
    by doing what m=MOD_DATA_OK does for that place: the records using the old
    id are moved to the id of the new string and recorded in the history. The
    old strings are left as they are (they may be used elsewhere, e.g. as a
    title domain). Each place is planned as:

    - [New_place]: the new string does not exist yet; it is created.
    - [Merge_existing j]: the new string already exists as [j] (possibly a place
      this batch renames too: it keeps its text, the substitution is
      simultaneous).
    - [Merge_batch]: several places of the batch get the same new string; they
      share the id created for the first one.

    All the places are done in a single pass over persons and families. *)

type occurrence = Geneweb_db.Driver.istr * int option
(** A place string and the term to rename in it: [Some d] is the component at
    depth [d] (root first, as {!Place.components}), [None] is the suburb. *)

val occurrence_key : occurrence -> string
(** Form field name of an occurrence, e.g. ["o123.2"] or ["o123.s"]. *)

val occurrence_of_key : string -> occurrence option

val place_key : Geneweb_db.Driver.istr -> string
(** Form field name of a place kept in the preview, e.g. ["p123"]. *)

val occurrences_of_env : Config.config -> occurrence list
(** The occurrences checked in the submitted form. *)

val valid_new_term : string -> bool
(** A new term must not be empty nor contain brackets or parentheses, which
    would change how the place strings are split. Commas are allowed (to split a
    term in two). *)

val terms_of_env : Config.config -> string list
(** The terms to rename, from the form fields [t] and [t1], [t2]... *)

val rewrite :
  bool ->
  terms:string list ->
  new_term:string ->
  string ->
  int option list ->
  string option
(** [rewrite inverted ~terms ~new_term raw depths] replaces in [raw] the
    components at [depths] whose text is one of [terms]. [None] if none of them
    is one of [terms] any more. *)

val rewrite_with :
  bool ->
  terms:string list ->
  (string -> string) ->
  string ->
  int option list ->
  string option
(** Like {!rewrite}, each matching component [c] being replaced by [f c]. *)

type usage
(** For every string id used as a place: the number of persons and families
    using it. *)

val usage : Geneweb_db.Driver.base -> usage
(** One pass over all persons and families, regardless of access rights. *)

val uses : usage -> Geneweb_db.Driver.istr -> int

type kind = New_place | Merge_existing of Geneweb_db.Driver.istr | Merge_batch

type item = {
  istr : Geneweb_db.Driver.istr;
  old_s : string;
  new_s : string;
  depths : int option list;
  kind : kind;
}

val plan :
  Config.config ->
  Geneweb_db.Driver.base ->
  terms:string list ->
  new_term:string ->
  occurrence list ->
  item list
(** What renaming [terms] into [new_term] at the given occurrences would do: one
    spelling or several (a cluster of spellings). Unchanged places are left out.
*)

val plan_places :
  Geneweb_db.Driver.base ->
  target:Geneweb_db.Driver.istr ->
  Geneweb_db.Driver.istr list ->
  item list
(** [plan_places base ~target places]: every place of [places] replaced as a
    whole by the existing place [target] ([Merge_existing target]). *)

val places_of_env : Config.config -> Geneweb_db.Driver.istr list
(** The places checked in the submitted form (fields [p<istr>=on]). *)

type result = {
  places : int;  (** places renamed *)
  merged : int;
      (** of which merged into an existing or another renamed place *)
  persons : int;  (** persons modified *)
  families : int;  (** families modified *)
  complete : bool;
      (** [false] when the [max_nb_update] limit stopped the update: applying
          the same plan again goes on with the remaining records *)
}

val apply : Config.config -> Geneweb_db.Driver.base -> item list -> result
(** Carries out a plan like m=MOD_DATA_OK (history "mq" for each modified person
    and the spouses of each modified family) and commits the patches. *)
