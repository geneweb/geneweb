module Person_reference : sig
  type t = { pk_first_name : string; pk_surname : string; pk_occ : int }
end

module Person_reference_set : sig
  type t
end

module Person_reference_map : sig
  type +'a t
end

type person_references = private {
  valid_references : Person_reference_set.t;
  fixed_occurrence_numbers : int option Person_reference_map.t;
}

type t = {
  just_comp : bool;
  out_file : string;
  force : bool;
  separate : bool;
  bnotes : string;
  shift : int;
  no_fail : bool;
      (** Do not raise exception if syntax error occured. Instead print error
          information on stdout *)
  no_picture : bool;  (** Save path to the images *)
  no_public : bool;  (** Ignore public access in source files **)
  mutable create_all_keys : bool;
      (** Forces to create all the keys for every persons (even for ? ?).
          Enabled for gwplus format. *)
  mutable files : (string * bool * string * int) list;
  mutable line_cnt : int;  (** Line counter while reading .gw file *)
  mutable person_references : person_references;
  default_source : string;
      (** Default source field for persons and families without source data *)
  do_check : bool;  (** Base consistency check *)
  do_consang : bool;  (** Compute consanguinity *)
  pr_stats : bool;  (** Print base's statistics *)
  particules_file : string;  (** File containing the particles to use *)
}

val default : t
(** Default state *)

val add_person_reference : t -> Person_reference.t -> unit
val get_fixed_person_reference : t -> Person_reference.t -> Person_reference.t
val all_person_references_are_valid : t -> bool
