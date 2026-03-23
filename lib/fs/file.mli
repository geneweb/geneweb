exception File_error of string

val file_exists : Fpath.t -> bool
(** [file_exists path] returns [true] if [path] exists in the filesystem. *)

val readdir : Fpath.t -> string array
(** [readdir dir] returns the names of all files present in [dir]. *)

val remove : ?force:bool -> ?recursive:bool -> Fpath.t -> unit
(** [remove ?force ?recursive path] removes the file or directory at [path]. If
    [force] is [true], no error is raised if the path doesn't exist. If [force]
    is [false] (default), the function raises an error if the path doesn't
    exist. If [recursive] is [true], directories are removed recursively with
    all their contents (like [rm -rf]). If [recursive] is [false] (default),
    only files can be removed. *)

val rename : Fpath.t -> Fpath.t -> unit
(** [rename src dst] renames [src] to [dst]. *)

val stat : Fpath.t -> Unix.stats
(** [stat path] returns information about the file at [path]. *)

val openfile :
  Fpath.t -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr
(** [openfile path flags perm] opens the file at [path] with the given [flags]
    and permissions [perm]. *)

val check_suffix : Fpath.t -> string -> bool
(** [check_suffix path suffix] returns [true] if [path] ends with [suffix]. *)

val chop_suffix : Fpath.t -> string -> string
(** [chop_suffix path suffix] removes [suffix] from the end of [path] and
    returns the result as a string. *)

val chmod : Fpath.t -> int -> unit
(** [chmod path perm] changes the permissions of [path] to [perm]. *)

val opendir : Fpath.t -> Unix.dir_handle
(** [opendir path] opens the directory at [path] and returns a directory handle.
*)

val is_directory : Fpath.t -> bool
(** [is_directory path] returns [true] if [path] is a directory. *)

val create_file : ?required_perm:int -> Fpath.t -> unit
(** [create_file ?required_perm ~kind path] creates a file at [path] if the
    target does not exist.

    The default permissions are 0o644. In absence of required permissions, we do
    not check the permissions of the target.

    @raise File_error
      if the target exists but it is not a regular file or the target file does
      not fulfill the required permissions. *)

val create_dir : ?parent:bool -> ?required_perm:int -> Fpath.t -> unit
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

val mkdir_p : ?perm:int -> Fpath.t -> unit
(** [mkdir_p ?perm dir] Create the directory [dir]. No error if existing, make
    parent directories as needed. *)

type entry =
  | File of Fpath.t
  | Dir of Fpath.t
  | Exn of { path : Fpath.t; exn : exn; bt : Printexc.raw_backtrace }

val walk_folder : ?recursive:bool -> (entry -> 'a -> 'a) -> Fpath.t -> 'a -> 'a
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

val copy_file : ?perm:int -> ?overwrite:bool -> Fpath.t -> Fpath.t -> unit
(** [copy_file ?perm ?overwrite src dst] copies the file [src] into the
    destination [dst]. The file [dst] is created with permissions [perm]. The
    default permission value is [0o640].

    If [overwrite] is [true] (default), existing destination files are
    overwritten. If [overwrite] is [false], the function fails if the
    destination already exists.

    This function can be used to copy binary files. *)

val open_in_text : Fpath.t -> in_channel
val open_out_text : Fpath.t -> out_channel
val open_in_bin : Fpath.t -> in_channel
val open_out_bin : Fpath.t -> out_channel
val with_open_out_text : Fpath.t -> (out_channel -> 'a) -> 'a
val with_open_out_bin : Fpath.t -> (out_channel -> 'a) -> 'a
val with_open_in_text : Fpath.t -> (in_channel -> 'a) -> 'a
val with_open_in_bin : Fpath.t -> (in_channel -> 'a) -> 'a
