(* Copyright (c) 1998-2007 INRIA *)

val with_database : ?read_only:bool -> string -> (Dbdisk.dsk_base -> 'a) -> 'a
(** [with_database ?read_only dbname k] initializes a [dsk_base] structure from
    the database located in the specified directory [dbname].

    Both data and functionality part are initialized. The continuation [k] is
    called with the [dsk_base] structure.

    If ~read_only:true, then the database will be loaded in memory, and kept in
    a cache. All next uses of [with_database] on the same database will use the
    memory-loaded database. This constraints operations on the base, and attempt
    to mutate its values will result in failure. *)

val make :
  string ->
  string list ->
  ((int, int, int) Def.gen_person array
  * int Def.gen_ascend array
  * int Def.gen_union array)
  * ((int, int, int) Def.gen_family array
    * int Adef.gen_couple array
    * int Def.gen_descend array)
  * string array
  * Def.base_notes ->
  (Dbdisk.dsk_base -> 'a) ->
  'a
(** [make bname particles ((persons, ascendants, unions) (families, couples,
     descendants) strings base_notes) k] initializes a [dsk_base] structure with
    giving data. The continuation [k] is called with the [dsk_base] structure.

    This function should be called for database creating purpose only. In
    particular, the functionality part of the [dsk_base] structure is not
    initalized. *)

val magic_of_version : Dbdisk.base_version -> string
(** Header written in the {i base} file of this version. *)

val base_format_of_dir : string -> string
(** [base_format_of_dir bname] is the header of the {i base} file of database
    [bname] (with or without its [.gwb] suffix), without opening the database;
    [""] if it cannot be read. *)

type base_format_status =
  | Current  (** Written by this version. *)
  | Older of string
      (** Header of an earlier format, read with conversion on every access. *)
  | Unreadable of string
      (** Header of a {i base} or {i patches} format this version cannot read,
          or [""] if the {i base} file cannot be read at all. *)

val base_format_status_of_dir : string -> base_format_status
(** [base_format_status_of_dir bname] tells whether the database [bname] (with
    or without its [.gwb] suffix) can be read by this version, from the headers
    of its {i base} and {i patches} files only. *)

(* Ajout pour l'API *)

type synchro_patch = {
  mutable synch_list : (string * int list * int list) list;
}
(** List of commited modifications inside the database. First element is a
    timestamp of a commit, second - changed/added by considered commit person
    ids, third - changed/added by considered commit families ids. *)

val input_synchro : string -> synchro_patch
(** Get [synchro_patch] from the giving database directory. *)
