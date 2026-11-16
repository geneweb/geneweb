(** Concurrent edits of a person or a family.

    The person and family forms carry a snapshot of the record as stored when
    the form was displayed: the state that the user edits, which tells the
    changes of the user from those saved meanwhile by other forms. *)

open Def
module Driver = Geneweb_db.Driver

val person_snapshot :
  Config.config -> (Driver.iper, Update.key, string) gen_person -> string
(** [person_snapshot conf sp] seals the person [sp], as stored in the base
    ({!UpdateInd.string_person_of}), for the hidden field [snapshot] of the
    person form. The value is marshalled and authenticated by an HMAC keyed by
    the secret salt of the server and bound to the running executable: only this
    executable reads it back, never a forged or a foreign value. *)

val person_of_snapshot :
  Config.config ->
  Driver.iper ->
  string ->
  (Driver.iper, Update.key, string) gen_person option
(** [person_of_snapshot conf ip s] is the person sealed in [s] by
    {!person_snapshot}; [None] when [s] is empty or altered, was sealed by
    another executable or with another salt, or holds another person than [ip].
*)

val family_snapshot :
  Config.config ->
  (Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend ->
  string
(** Same as {!person_snapshot} for a family as stored in the base
    ({!UpdateFam.string_family_of}). *)

val family_of_snapshot :
  Config.config ->
  Driver.ifam ->
  string ->
  ((Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend)
  option
(** Same as {!person_of_snapshot} for a family sealed by {!family_snapshot}. *)

val merge_person :
  Config.config ->
  snapshot:string ->
  (Driver.iper, Update.key, string) gen_person ->
  (Driver.iper, Update.key, string) gen_person ->
  (Driver.iper, Update.key, string) gen_person * string
(** [merge_person conf ~snapshot a b] merges the person [b], posted by a form
    displayed on the state sealed in [snapshot], with the person [a] as stored
    now, field by field and event by event ({!Event.align_pevents}). A field
    changed on one side only takes the value of that side; a field changed on
    both sides to different values is a conflict, which keeps the value of [a].
    Without a readable snapshot, any difference is a conflict. The notes of the
    person and of its events are merged line by line, as by {!merge_note}. The
    fields that the form derives from the events (birth, baptism, death and
    burial dates, places, notes and sources) are merged without conflict, the
    form deriving them again.

    Returns the merged person and its review in JSON:
    [{"kind", "snapshot", "changes", "conflicts"}], where [kind] is ["merge"],
    [snapshot] tells whether the snapshot was read, [changes] lists the fields
    changed since the snapshot ([what], [before], [after]) and [conflicts] the
    differences to check ([what], [base], [mine], the [selector] of the inputs
    of the field in the form, and the pairs of input name and value giving back
    the value of [b], [restore], or of [a], [saved]). *)

val merge_family :
  Config.config ->
  snapshot:string ->
  (Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend ->
  (Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend ->
  ((Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend)
  * string
(** Same as {!merge_person} for a family: its events are aligned by
    {!Event.align_fevents}, its comment is merged line by line, and its children
    are merged as a set, keeping the children added and dropping those removed
    on either side. *)

val added_families :
  Config.config -> Driver.base -> Driver.iper -> int -> string
(** [added_families conf base ip seen] is the review, in the JSON of
    {!merge_person} with the [kind] ["families"], of the families of [ip] beyond
    its [seen] first ones: those added since a form creating a family of [ip]
    was displayed. *)

val note_snapshot : Config.config -> string -> string -> string
(** [note_snapshot conf fname s] seals the text [s] of the note [fname] as
    stored, its header lines included, like {!person_snapshot}. *)

val note_of_snapshot : Config.config -> string -> string -> string option
(** [note_of_snapshot conf fname s] is the text sealed in [s] by
    {!note_snapshot}; [None] when [s] is not readable or holds another note than
    [fname]. *)

val merge_note :
  Config.config -> string option -> string -> string -> string * string
(** [merge_note conf o a b] merges line by line the text [b] of a note, posted
    by a form displayed on the text [o] ({!note_of_snapshot}), with its text [a]
    as stored now: between the lines kept on both sides, a block of lines
    changed on one side only takes that side, a block changed on both sides to
    different lines keeps the lines of [a] and is a difference to check. Without
    [o], any difference is to be checked. Returns the merged text and its review
    in the JSON of {!merge_person}, whose differences carry [text]: the name of
    the text area, and the lines of [a] and of [b] with the unchanged line
    before and after them, to swap them in the text area. *)
