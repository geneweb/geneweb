(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk

type name_index_data = int array array
type strings_of_fsname = int array array

val magic_GnWb0020 : string
val magic_GnWb0021 : string
val magic_GnWb0022 : string
val magic_GnWb0023 : string
val table_size : int

val compare_istr_fun : Dbdisk.base_data -> int -> int -> int
val compare_names : Dbdisk.base_data -> string -> string -> int

val dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> dsk_title list) -> string list

val poi : dsk_base -> int -> dsk_person
val sou : dsk_base -> int -> string
val p_first_name : dsk_base -> dsk_person -> string
val p_surname : dsk_base -> dsk_person -> string

val output_value_no_sharing : out_channel -> _ -> unit
val output_array_no_sharing : out_channel -> (int -> _) -> int -> unit
val int_size : int

module IntHT : sig
  include module type of Hashtbl.Make (struct
      type t = int
      let equal = (=)
      let hash x = x
    end)
end

(** [name_index s]
    Compute the index of crush_lowered version of s
    in an array of size {!val:table_size}.
*)
val name_index : string -> int
