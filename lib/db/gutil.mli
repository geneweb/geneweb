(* Copyright (c) 1998-2007 INRIA *)

open Def

val spouse : Driver.iper -> Driver.family -> Driver.iper
(** [spouse p f] returns spouse of giving person inside the family. *)

val person_not_a_key_find_all : Driver.base -> string -> Driver.iper list
(** Returns list of persons having the giving name as one of the misc names. *)

val person_ht_find_all : Driver.base -> string -> Driver.iper list
(** Returns list of persons from the giving key. If key has form
    {i "firstname.occ surname"} then returns list of one corresponding person.
    Otherwise calls [person_not_a_key_find_all] *)

val person_of_string_key : Driver.base -> string -> Driver.iper option
(** [person_of_string_key base key] Finds a key inside [key] string of the form
    {i "firstname.occ surname"} and returns a corresponding person. The first
    occurence of an int preceded by a dot is used as occurence number.

    If person doesn't exists or key isn't found then returns [None] *)

val person_of_string_dot_key : Driver.base -> string -> Driver.iper option
(** [person_of_string_dot_key base key] Same as {!val:person_of_string_key}, but
    use the last occurence of an int preceded by a dot as occurence number. *)

val find_same_name : Driver.base -> Driver.person -> Driver.person list
(** Returns list of persons having the same first name and surname as the
    specified person *)

val designation : Driver.base -> Driver.person -> string
(** Returns person's key that has form {i "firstname.occ surname"} *)

val trim_trailing_spaces : string -> string
(** Trim at the end of string *)

(* TODO: This function is very slow and should be rewritten completely. *)
val alphabetic_utf_8 : string -> string -> int
(** Compare two UTF-8 encoded strings by alphabetic order *)

val alphabetic : string -> string -> int
(** Compare two ISO-8859-1 encoded strings by alphabetic order *)

val alphabetic_order : string -> string -> int
(** Same as [alphabetic_utf_8] *)

val arg_list_of_string : string -> string list
(** Parse line and extract separated arguments ("" and '' are used to indlude
    spaces inside the argument) *)

val sort_person_list : Driver.base -> Driver.person list -> Driver.person list
(** Sort list of persons by comparison with following order:
    - Compare by birth and death date
    - Compare by surname
    - Compare by first name
    - Compare by occurence number
    - Compare by id *)

val sort_uniq_person_list :
  Driver.base -> Driver.person list -> Driver.person list
(** Same as [sort_person_list] but also remove duplicates *)

val father : 'a gen_couple -> 'a
(** Same as [Adef.father] *)

val mother : 'a gen_couple -> 'a
(** Same as [Adef.mother] *)

val couple : bool -> 'a -> 'a -> 'a gen_couple
(** [couple multi f m] creates a couple from father [f] and mother [m]. If
    [multi] true uses multiparent functionality *)

val parent_array : 'a gen_couple -> 'a array
(** Same as [Adef.parent_array] *)

val find_free_occ : Driver.base -> string -> string -> int
(** Find first free occurence number for the person with specified first name
    and surname. *)

val get_birth_death_date : Driver.person -> date option * date option * bool
(** [get_birth_death p] Return [(birth, death, approx)]. If birth/death date can
    not be found, baptism/burial date is used and [approx] is set to [true] (it
    is [false] if both birth and death dates are found). *)
