(* Copyright (c) 2007-2008 INRIA *)

open Def

val rgpd_dir : string ref
(** Fonctionnement RGPD *)

val rgpd : bool ref
(** activate RGPD features (SemiPublic otehrwise ignore them *)

val semi_pub_cnt : int ref
(** count the number of person registered as SemiPublic *)

type key = { pk_first_name : string; pk_surname : string; pk_occ : int }
(** Key to refer a person's definition *)

(** Represents a person in .gw file. It could be either reference to a person
    (only key elements provided) or definition (all information provided). *)
type somebody =
  | Undefined of key  (** Reference to person *)
  | Defined of
      (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) gen_person
      (** Person's definition *)

(** Blocks that could appear in .gw file. *)
type gw_syntax =
  | Family of
      somebody gen_couple
      * sex
      * sex
      * (somebody * sex) list
      * (string gen_fam_event_name
        * cdate
        * string
        * string
        * string
        * string
        * (somebody * sex * witness_kind) list)
        list
      * ( (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) gen_person,
          Geneweb_db.Driver.ifam,
          string )
        gen_family
      * (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) gen_person
        gen_descend
      (** Family definition block. Contains:
          - Family couple (father's and mother's definition/reference)
          - Father's sex
          - Mother's sex
          - List of witnesses definition/reference with their sex.
          - List of information about every family event (name, date, place,
            reason, source, notes and witnesses)
          - Family definition
          - Children (descendants) *)
  | Notes of key * string
      (** Block that defines personal notes. First element represents reference
          to person. Second is note's content. *)
  | Relations of somebody * sex * (somebody, string) gen_relation list
      (** Block that defines relations of a person with someone outisde of
          family block. Contains:
          - Concerned person definition/reference
          - Sex of person
          - List of his relations. *)
  | Pevent of
      somebody
      * sex
      * (string gen_pers_event_name
        * cdate
        * string
        * string
        * string
        * string
        * (somebody * sex * witness_kind) list)
        list
      (** Block that defines events of a person. Specific to gwplus format.
          Contains:
          - Concerned person definition/reference
          - Sex of person
          - List of information about every personal event (name, date, place,
            reason, source, notes and witnesses)*)
  | Bnotes of string * string
      (** Block that defines database notes and extended pages. First string
          represents name of extended page ("" for database notes, only one for
          file). Second is note's or page's content. *)
  | Wnotes of string * string
      (** Block that defines wizard notes. First string represents wizard's id.
          Second is note's content. *)

val magic_gwo : string
(** .gwo file header *)

val line_cnt : int ref
(** Line counter while reading .gw file *)

val no_fail : bool ref
(** Do not raise exception if syntax error occured. Instead print error
    information on stdout *)

val no_picture : bool ref
(** Save path to the images *)

val create_all_keys : bool ref
(** Forces to create all the keys for every persons (even for ? ?). Enabled for
    gwplus format. *)

val verbose : bool ref
val out_file : string ref

val comp_families : string -> unit
(** Compile .gw file and save result to corresponding .gwo *)

(* Ajout pour l'API *)

val date_of_string : string -> int -> date option
(** Parses [Def.date] from string that starts at pos [i] inside [s] *)
