exception File_error of string

val create_file : ?required_perm:int -> string -> unit
(** [create_file ?required_perm ~kind path] creates a file at [path] if the
    target does not exist.

    The default permissions are 0o644. In absence of required permissions, we do
    not check the permissions of the target.

    @raise File_error
      if the target exists but it is not a regular file or the target file does
      not fulfill the required permissions. *)

val create_dir : ?parent:bool -> ?required_perm:int -> string -> unit
(** [create_dir ?parent ?required_perm ~kind path] creates a directory at [path]
    if the target does not exist.

    The default permissions are 0o755. In absence of required permissions, we do
    not check the permissions of the target.

    If [parent] is [true], the function also creates all the intermediate
    entries of the path that do not exist with the same permissions. Notice that
    in this case the function does not behave exactly as the Unix command `mkdir
    -p`. In particular it does not support relative paths which involve dot-dot
    entries.

    @raise File_error
      if one of the entry in the path exists but it is not a directory or the
      target directory does not fulfill the required permissions.

    @raise Invalid_argument on empty path. *)

type entry =
  | File of string
  | Dir of string
  | Exn of { path : string; exn : exn; bt : Printexc.raw_backtrace }

val walk_folder : ?recursive:bool -> (entry -> 'a -> 'a) -> string -> 'a -> 'a
(** [walk_folder ~recursive f dir] applies [f] to each regular file or directory
    within [dir].

    The iterator yields:
    - [File path] for each regular file,
    - [Dir path] for each subdirectory, where [path] is the relative path to the
      item within [dir].

    If an exception is raised while accessing an entry, the iterator yields
    [Exn { path; exn; bt }] where:
    - [path] is the relative path of the entry,
    - [exn] is the raised exception,
    - [bt] is the raw backtrace associated with this exception,

    If [recursive] is [true], subdirectories are explored recursively. The
    default is [false]. *)

val copy_file : ?perm:int -> ?overwrite:bool -> string -> string -> unit
(** [copy_file ?perm ?overwrite src dst] copies the file [src] into the
    destination [dst]. The file [dst] is created with permissions [perm]. The
    default permission value is [0o640].

    If [overwrite] is [true] (default), existing destination files are
    overwritten. If [overwrite] is [false], the function fails if the
    destination already exists.

    This function can be used to copy binary files. *)
