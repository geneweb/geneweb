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
