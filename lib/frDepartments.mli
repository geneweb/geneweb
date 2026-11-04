(** The departments of France, to read and write their codes in the places of a
    French base (Roglo): ["Nantes (44)"] is ["Nantes (Loire-Atlantique)"]. *)

val name_of_code : string -> string option
(** [name_of_code "44"] is [Some "Loire-Atlantique"]: two figures, ["2A"],
    ["2B"] or ["971"] to ["976"]; the current name of the department. *)

val is_name : string -> bool
(** The name of a department, with any spelling ({!PlaceTerms.cluster_key}), its
    former names included (["Loire-Inférieure"]). *)
