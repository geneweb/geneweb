(* Copyright (c) 2007-2008 INRIA *)

type key = State.Person_reference.t = {
  pk_first_name : string;
  pk_surname : string;
  pk_occ : int;
}
(** Key to refer a person's definition *)

(** Represents a person in .gw file. It could be either reference to a person
    (only key elements provided) or definition (all information provided). *)
type somebody =
  | Undefined of key  (** Reference to person *)
  | Defined of (Gwdb.iper, Gwdb.iper, string) Def.gen_person
      (** Person's definition *)

type 'a assumption = Weak of 'a | Strong of 'a

(** Blocks that could appear in .gw file. *)
type gw_syntax =
  | Family of
      somebody Def.gen_couple
      * Def.sex assumption
      * Def.sex assumption
      * (somebody * Def.sex assumption) list
      * (string Def.gen_fam_event_name
        * Def.cdate
        * string
        * string
        * string
        * string
        * (somebody * Def.sex assumption * Def.witness_kind * string) list)
        list
      * ( (Gwdb.iper, Gwdb.iper, string) Def.gen_person,
          Gwdb.ifam,
          string )
        Def.gen_family
      * (Gwdb.iper, Gwdb.iper, string) Def.gen_person Def.gen_descend
      (** Family definition block. Contains:
      - Family couple (father's and mother's definition/reference)
      - Father's sex
      - Mother's sex
      - List of witnesses definition/reference with their sex.
      - List of information about every family event (name, date,
        place, reason, source, notes and witnesses)
      - Family definition
      - Children (descendants) *)
  | Notes of key * string
      (** Block that defines personal notes. First element represents
      reference to person. Second is note's content. *)
  | Relations of
      somebody * Def.sex assumption * (somebody, string) Def.gen_relation list
      (** Block that defines relations of a person with someone outisde of
      family block. Contains:
      - Concerned person definition/reference
      - Sex of person
      - List of his relations. *)
  | Pevent of
      somebody
      * Def.sex assumption
      * (string Def.gen_pers_event_name
        * Def.cdate
        * string
        * string
        * string
        * string
        * (somebody * Def.sex assumption * Def.witness_kind * string) list)
        list
      (** Block that defines events of a person. Specific to gwplus format. Contains:
      - Concerned person definition/reference
      - Sex of person
      - List of information about every personal event (name, date,
      place, reason, source, notes and witnesses)*)
  | Bnotes of string * string
      (** Block that defines database notes and extended pages.
      First string represents name of extended page ("" for
      database notes, only one for file).
      Second is note's or page's content. *)
  | Wnotes of string * string
      (** Block that defines wizard notes.
      First string represents wizard's id.
      Second is note's content. *)

val check_magic : string -> in_channel -> unit
(** Checks a .gwo header and prints fails if header is absent or not compatible. *)

val comp_families : State.t -> string -> unit
(** Compile .gw file and save result to corresponding .gwo *)

val make_strong_assumption : 'a -> 'a assumption
val make_weak_assumption : 'a -> 'a assumption
val value_of_assumption : 'a assumption -> 'a
