(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk

type name_index_data = int array array
(** Index for all kind of mix between person's names (first index inside
    {i names.inx}) *)

type strings_of_fsname = int array array
(** Index for sub-strings of person's surame and first name (second and third
    index respectively inside {i names.inx}) *)

val magic_GnWb0020 : string
(** Header for the {i base} file (version 0020) *)

val magic_GnWb0021 : string
(** Header for the {i base} file (version 0021) *)

val magic_GnWb0022 : string
(** Header for the {i base} file (version 0022) *)

val magic_GnWb0023 : string
(** Header for the {i base} file (version 0023) *)

val magic_GnWb0024 : string
(** Header for the latest version of {i base} file *)

val table_size : int
(** Maximal size of hash table for name indexation (inside {i names.inx}) *)

val compare_fnames_i : Dbdisk.base_data -> int -> int -> int
(** [compare_fnames_i base i1 i2] compare two first names that have indexes [i1]
    and [i2] inside the [base]. *)

val compare_fnames : string -> string -> int
(** [compare_fnames] compare two first names. *)

val compare_snames_i : Dbdisk.base_data -> int -> int -> int
(** [compare_snames_i base i1 i2] compare two surnames that have indexes [i1]
    and [i2] inside the [base]. *)

val compare_snames : Dbdisk.base_data -> string -> string -> int
(** [compare_snames_i base s1 s2] compare two surnames according to the principe
    specified by [Mutil.compare_after_particle]. *)

val dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> dsk_title list) -> string list
(** [dsk_person_misc_names base p nobtit] computes various mix between all kind
    of names of a person's entry [p] from the database [base]. [nobtit] is used
    to return a title entries for passed in argument person. *)

val poi : dsk_base -> int -> dsk_person
(** [poi base i] returns person's entry with index [i] from [base]. *)

val sou : dsk_base -> int -> string
(** [poi base i] returns string with index [i] from [base]. *)

val p_first_name : dsk_base -> dsk_person -> string
(** Returns person's first name from the given person's entry. *)

val p_surname : dsk_base -> dsk_person -> string
(** Returns person's surname from the given person's entry. *)

val output_value_no_sharing : out_channel -> _ -> unit
(** Output given value to the channel. Uses [Marshall.to_channel] with
    [No_sharing] flag. *)

val int_size : int
(** Size of integer value inside the Geneweb's binary files *)

(** Hastable that has unhashed int as a key. *)
module IntHT : sig
  include module type of Hashtbl.Make (struct
    type t = int

    let equal = ( = )
    let hash x = x
  end)
end

val name_index : string -> int
(** [name_index s] Compute the index of crush_lowered version of s in an array
    of size [table_size]. *)
