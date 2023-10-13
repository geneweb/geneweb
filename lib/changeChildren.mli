val digest_children : Gwdb.base -> Gwdb.iper list -> string
(** Returns digest (using md5 algorithm) of concatenated for every children first name, surname and occurence number *)

val check_digest : Config.config -> string -> unit
(** Checks if children digest in environement [conf.env] corresponds to specified digest. Other print error page. *)

exception ChangeChildrenConflict of Gwdb.person * Gwdb.person
(** Exception raised when children change defines a new children information (new first name, new surname and new occurence number) are in
    conflict with another person already existing in the base *)

exception FirstNameMissing of Gwdb.iper
(** Exception raised when children change removes it first name *)

val change_children :
  Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.iper list ->
  ((string * string * int * Gwdb.iper) * (string * string * int * Gwdb.iper))
  list
(** Change all person's children by looking up information to update inside [conf.env] that was send by the form.
    Changes also children's personal image name. Could raise [ChangeChildrenConflict] if new children's key
    is in conflict with another and [FirstNameMissing] if new children's first name is empty. If surname modification is
    requested but absent then children takes parent's surname. Returns informations used by [Update] module to record
    children's update operation. *)
