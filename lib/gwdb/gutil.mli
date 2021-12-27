(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

(** [spouse p f] returns spouse of giving person inside the family. *)
val spouse : iper -> family -> iper

(**  Returns list of persons having the giving name as one of the misc names. *)
val person_not_a_key_find_all : base -> string -> iper list

(** Returns list of persons from the giving key. If key has form {i "firstname.occ surname"}
    then returns list of one corresponding person. Otherwise calls [person_not_a_key_find_all]  *)
val person_ht_find_all : base -> string -> iper list

(** [person_of_string_key base key] try to find a key inside [key] string of
    the form {i "firstname.occ surname"} and returns a corresponding person. 
    If person doesn't exists or key isn't found then returns [None] *)
val person_of_string_key : base -> string -> iper option

(** Returns list of persons having the same first name and surname 
    as the specified person *)
val find_same_name : base -> person -> person list

(** Returns person's key that has form {i "firstname.occ surname"} *)
val designation : base -> person -> string

(** Trim at the end of string *)
val trim_trailing_spaces : string -> string

(** Compare two UTF-8 encoded strings by alphabetic order *)
val alphabetic_utf_8 : string -> string -> int

(** Compare two ISO-8859-1 encoded strings by alphabetic order *)
val alphabetic : string -> string -> int

(** Same as [alphabetic_utf_8] *)
val alphabetic_order : string -> string -> int

(** Parse line and extract separated arguments ("" and '' are used to indlude spaces 
    inside the argument) *)
val arg_list_of_string : string -> string list

(** Sort list of persons by comparison with following order:
    - Compare by birth and death date
    - Compare by surname
    - Compare by first name
    - Compare by occurence number
    - Compare by id *)
val sort_person_list : base -> person list -> person list

(** Same as [sort_person_list] but also remove duplicates *)
val sort_uniq_person_list : base -> person list -> person list

(** Same as [Adef.father] *)
val father : 'a gen_couple -> 'a

(** Same as [Adef.mother] *)
val mother : 'a gen_couple -> 'a

(** [couple multi f m] creates a couple from father [f] and mother [m]. If
    [multi] true uses multiparent functionality *)
val couple : bool -> 'a -> 'a -> 'a gen_couple

(** Same as [Adef.parent_array] *)
val parent_array : 'a gen_couple -> 'a array

(** Find first free occurence number for the person with specified first name
    and surname. *)
val find_free_occ : base -> string -> string -> int


(** [get_birth_death p]
    Return [(birth, death, approx)]. If birth/death date can not be found,
    baptism/burial date is used and [approx] is set to [true] (it is [false]
    if both birth and death dates are found).
*)
val get_birth_death_date : person -> date option * date option * bool
