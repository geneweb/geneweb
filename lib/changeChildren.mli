
(** Returns digest (using md5 algorithm) of concatenated for every childran first name, surname and occurence number *)
val digest_children : Gwdb.base -> Gwdb.iper list -> string

(** Checks if children digest in environement [conf.env] corresponds to specified digest. Other print error page. *)
val check_digest : Config.config -> string -> unit

(** Exception raised when childran change defines a new childran information (new first name, new surname and new occurence number) are in
    conflict with another person already existing in the base *)
exception ChangeChildrenConflict of Gwdb.person * Gwdb.person

(** Exception raised when childran change removes it first name *)
exception FirstNameMissing of Gwdb.iper

(** Change all person's children by looking up information to update inside [conf.env] that was send by the form.
    Changes also childran's personal image name. Could raise [ChangeChildrenConflict] if new childran's key 
    is in conflict with another and [FirstNameMissing] if new childran's first name is empty. If surname modification is 
    requested but absent then childran takes parent's surname. Returns informations used by [Update] module to record 
    children's update operation. *)
val change_children :
  Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.iper list ->
  ((string * string * int * Gwdb.iper) * (string * string * int * Gwdb.iper))
  list