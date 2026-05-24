(** Persistent cache for the Places-Persons-Surnames (PPS) index.

    The cache stores, for each normalised place string, the list of persons and
    families associated with that place, together with the event that produced
    the association.

    The cache is unfiltered: all events and all persons/families are stored
    regardless of the current request flags. Filtering by event type or by
    initial letter is performed at query time by the caller.

    Serialisation uses Marshal. The magic number {!magic} must be bumped
    whenever the types [t], [gen_pers_event_name] or [gen_fam_event_name] change
    their representation, as this forces a full rebuild. *)

open Def
open Geneweb_db.Driver

type t = {
  persons : (string, (istr gen_pers_event_name * iper) list) Hashtbl.t;
  families : (string, (istr gen_fam_event_name * ifam) list) Hashtbl.t;
}
(** In-memory cache. *)

val magic : string
(** Magic number embedded at the start of every cache file. Changing this string
    invalidates all existing cache files. *)

val cache_path : string -> string
(** [cache_path bdir] returns the path of the cache file for base directory
    [bdir]. Convention: [<bdir>/caches/place_pps/all.cache]. *)

val cache_is_valid : string -> base -> bool
(** [cache_is_valid bdir base] returns [true] if a cache file exists and is more
    recent than the patches file of [base]. *)

val build : Config.config -> base -> t
(** [build conf base] performs a single pass over [base] and returns a fully
    populated cache. Empty place strings are excluded. This is the only function
    that reads [conf] and [base] directly; all other functions operate on the
    in-memory [t]. *)

val write : string -> t -> unit
(** [write bdir cache] atomically writes [cache] to disk under
    [cache_path bdir]. Writes to a [.tmp] file then renames. *)

val read : string -> t
(** [read bdir] reads and returns the cache from disk. Raises [Failure] if the
    magic number does not match. *)

val get_or_build : string -> Config.config -> base -> t
(** [get_or_build bdir conf base] returns the cache, reading it from disk if
    valid, rebuilding and persisting it otherwise. *)
