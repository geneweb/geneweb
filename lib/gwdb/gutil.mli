(* Copyright (c) 1998-2007 INRIA *)

val spouse : Gwdb.iper -> Gwdb.family -> Gwdb.iper
(** [spouse p f] returns spouse of giving person inside the family. *)

val person_not_a_key_find_all : Gwdb.base -> string -> Gwdb.iper list
(** Returns list of persons having the giving name as one of the misc names. *)

val person_ht_find_all : Gwdb.base -> string -> Gwdb.iper list
(** Returns list of persons from the giving key. If key has form
    {i "firstname.occ surname"} then returns list of one corresponding person.
    Otherwise calls [person_not_a_key_find_all] *)

val person_of_string_key : Gwdb.base -> string -> Gwdb.iper option
(** [person_of_string_key base key] Finds a key inside [key] string of the form
    {i "firstname.occ surname"} and returns a corresponding person. The first
    occurence of an int preceded by a dot is used as occurence number.

    If person doesn't exists or key isn't found then returns [None] *)

val person_of_string_dot_key : Gwdb.base -> string -> Gwdb.iper option
(** [person_of_string_dot_key base key] Same as {!val:person_of_string_key}, but
    use the last occurence of an int preceded by a dot as occurence number. *)

val find_same_name : Gwdb.base -> Gwdb.person -> Gwdb.person list
(** Returns list of persons having the same first name and surname as the
    specified person *)

val designation : Gwdb.base -> Gwdb.person -> string
(** Returns person's key that has form {i "firstname.occ surname"} *)

val arg_list_of_string : string -> string list
(** Parse line and extract separated arguments ("" and '' are used to indlude
    spaces inside the argument) *)

val sort_person_list : Gwdb.base -> Gwdb.person list -> Gwdb.person list
(** Sort list of persons by comparison with following order:
    - Compare by birth and death date
    - Compare by surname
    - Compare by first name
    - Compare by occurence number
    - Compare by id *)

val sort_uniq_person_list : Gwdb.base -> Gwdb.person list -> Gwdb.person list
(** Same as [sort_person_list] but also remove duplicates *)

val homonyms :
  base:Gwdb.base -> first_name:string -> surname:string -> Gwdb.iper list

val get_all_occurrence_numbers :
  base:Gwdb.base -> first_name:string -> surname:string -> Ext_int.Set.t

val find_free_occ : Gwdb.base -> string -> string -> int
(** Find first free occurence number for the person with specified first name
    and surname. *)

val get_birth_death_date :
  Gwdb.person -> Date.date option * Date.date option * bool
(** [get_birth_death p] Return [(birth, death, approx)]. If birth/death date can
    not be found, baptism/burial date is used and [approx] is set to [true] (it
    is [false] if both birth and death dates are found). *)

val split_key : string -> (string * int * string) option
(** [split_key key] returns the tuple (first name, occurence, surname) from a
    key of the form first name.occurence surname *)
