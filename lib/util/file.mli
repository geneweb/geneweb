exception File_error of string

val create_file : ?required_perm:int -> string -> unit
(** [create_file ?required_perm ~kind path] creates a file at [path] if the
    target does not exist.

    The default permissions are 0o644. In absence of required permissions,
    we do not check the permissions of the target.

    @raise File_error if the target exists but it is not a regular file or
                      the target file does not fulfill the required
                      permissions. *)

val create_dir : ?parent:bool -> ?required_perm:int -> string -> unit
(** [create_dir ?parent ?required_perm ~kind path] creates a directory at
    [path] if the target does not exist.

    The default permissions are 0o755. In absence of required permissions,
    we do not check the permissions of the target.

    If [parent] is [true], the function also creates all the intermediate
    entries of the path that do not exist with the same permissions.
    Notice that in this case the function does not behave exactly as the Unix
    command `mkdir -p`. In particular it does not support relative paths which
    involve dot-dot entries.

    @raise File_error if one of the entry in the path exists but it is not
                      a directory or the target directory does not fulfill the
                      required permissions.

    @raise Invalid_argument on empty path. *)

val walk_folder :
  ?recursive:bool ->
  ([ `File of string | `Dir of string ] -> 'a -> 'a) ->
  string ->
  'a ->
  'a
(** [walk_folder ~recursive f dir] accumulates [f] on all the regular files or
    directories of [dir].

    The argument of [f] is the relative path of the file or subdirectory in
    [dir].

    If [recursive] is [true], the iterator also explores subdirectories. [false]
    is the default.

    @raise Unix.Unix_error if the function cannot open a file or a
                           subdirectory in [dir]. *)
