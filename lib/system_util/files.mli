
(** [ls_r dirs]
    List directories (and subdirectories) contents of [dirs], including [dirs] themselves.
*)
val ls_r : string list -> string list

(** [rm fname]
    Remove [fname]. If [fname] does not exists, do nothing.
 *)
val rm : string -> unit

(** [rm_rf dir]
    Remove directory [dir] and everything inside [dir].
*)
val rm_rf : string -> unit

(** [mv src dst]
    Move [src] to [dst]. If [src] does not exists, do nothing.
 *)
val mv : string -> string -> unit

(** [mkdir_p ?perm dir]
    Create the directory [dir].
    No error if existing, make parent directories as needed.
*)
val mkdir_p : ?perm:int -> string -> unit

(** Remove every file in the directory and then remove the directory itself *)
val remove_dir : string -> unit

(** [search_file directories file]
    Search for a [file] in different [directories] and return
    then first result or [None] if not found
  *)
val search_file_opt : string list -> string -> string option

(** [search_asset fname]
    Searches for a file in assets directories.
    i.e. directories previously registered with [Secure.add_assets] *)
val search_asset_opt : string -> string option

(** Returns the name of a lock file (with extension .lck). Result is generally used as an
    argument for [Lock.control] function. *)
val lock_file : string -> string

(** [check_magic magic ic]
    Read (and consume) the [magic] string at the beggining of [ic]
    and return [true].
    If [ic] does not start with [magic], reset the reading position
    of [ic] to where is was before you call [check_magic] and return [false].
 *)
val check_magic : string -> in_channel -> bool


(** [check_magics magics ic]
    Given a list of magic strings [magics] returns true if for one of the strings s
    [check_magic s ic] returns [true], [false] otherwise
 *)  
val check_magics : string list -> in_channel -> bool
  
(** [read_or_create_channel ?magic fname read write]

    If [fname] exists (and starts with [magic] if this one is provided),
    [read] function is used on the file.
    If it does not, or does not start with [magic], or if [read] raise an exception,
    [write] function is used on the file.

    This function takes care of locking and closing files so you must not take care of
    that in [read]/[write].
    It also takes care of writing [magic] at the beginning of the file before calling
    [write]

    On Windows, file is not locked.
*)
val read_or_create_channel
  :  ?magic:string
  -> ?wait:bool
  -> string
  -> (in_channel -> 'a)
  -> (out_channel -> 'a)
  -> 'a

(** [read_or_create_value ?magic fname create]

    If [fname] exists (and starts and ends with [magic] if this one is provided),
    return the unmarshalled value.
    If it does not, or does not start with [magic], or if unmarshalling raise an exception,
    [create] function is used to produce the value to be marshalled.

    On Windows, file is not locked.
*)
val read_or_create_value
  :  ?magic:string
  -> ?wait:bool
  -> string
  -> (unit -> 'a)
  -> 'a
