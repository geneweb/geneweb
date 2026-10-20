(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Driver = Geneweb_db.Driver

val person_key : Driver.base -> Driver.iper -> Update.key
(** [person_key base ip] is the key linking person [ip] in a family form. A
    person named "?" gets its index as occurrence number, as expected by
    {!Update.insert_person}. *)

val string_family_of :
  Driver.base ->
  Driver.ifam ->
  (Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend
(** [string_family_of base ifam] is family [ifam] as the family form handles it:
    persons as {!person_key} keys, strings decoded. *)

val print_update_fam :
  ?grandparents:(string * Update.key) list ->
  config ->
  Driver.base ->
  (Update.key, Driver.ifam, string) gen_family
  * Update.key Adef.gen_couple
  * Update.key gen_descend ->
  string ->
  unit
(** [print_update_fam ?grandparents conf base (fam, cpl, des) digest] renders
    the family form [updfam.txt] filled with [fam], its couple [cpl] and its
    children [des], for [m] among [ADD_FAM], [ADD_PAR], [MOD_FAM], their [_OK]
    redisplays and the family merge modes; any other [m] yields the
    incorrect-request page. [digest] is posted back for the concurrent update
    check. [grandparents] binds the input prefix ["pa<k><j>"] of parent [j] of
    the family parent [k] to its key, to redisplay these blocks. *)

val print_add : config -> Driver.base -> unit
(** [print_add conf base] handles [m=ADD_FAM]: empty family form, or with
    [ip=<iper>] that person as father or mother according to its sex; a person
    of unknown sex is placed only with [sex=M] or [sex=F]. *)

val print_add_parents : config -> Driver.base -> unit
(** [print_add_parents conf base] handles [m=ADD_PAR]: family form with the
    person [ip] (or [pp], [np], [ocp]) as only child and the father's surname
    preset to the child's. *)

val print_mod : config -> Driver.base -> unit
(** [print_mod conf base] handles [m=MOD_FAM]: form of family [i]. *)

val print_del : config -> Driver.base -> unit
(** [print_del conf base] handles [m=DEL_FAM]: confirmation page for the
    deletion of family [i], posting [m=DEL_FAM_OK]. *)

val print_inv : config -> Driver.base -> unit
(** [print_inv conf base] handles [m=INV_FAM]: confirmation page for swapping
    family [f] of person [i] with the preceding one, posting [m=INV_FAM_OK]. *)

val change_order : Driver.person -> Driver.ifam -> int -> Driver.ifam list
(** [change_order p ifam n] is the list of families of [p] with [ifam] moved to
    position [n], from 1 to the number of families; out of that range, [ifam] is
    dropped. *)

val print_change_order : config -> Driver.base -> unit
(** [print_change_order conf base] handles [m=CHG_FAM_ORD]: shows the families
    of person [i] before and after moving family [f] to position [n], posting
    [m=CHG_FAM_ORD_OK]. *)

val print_change_event_order : config -> Driver.base -> unit
(** [print_change_event_order conf base] handles [m=CHG_EVT_FAM_ORD]: form
    [updfamevt.txt] for reordering the events of family [i]. *)
