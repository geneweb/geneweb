type stats = {
  (* Number of men *)
  mutable men : int;
  (* Number of women *)
  mutable women : int;
  (* Number of persons with neuter sex *)
  mutable neutre : int;
  (* Number of persons with unknown first name and surname *)
  mutable noname : int;
  (* Oldest father with age when he became father *)
  mutable oldest_father : int * Gwdb.person;
  (* Oldest mother with age when she became mother *)
  mutable oldest_mother : int * Gwdb.person;
  (* Youngest father with age when he became father *)
  mutable youngest_father : int * Gwdb.person;
  (* Youngest mother with age when he became mother *)
  mutable youngest_mother : int * Gwdb.person;
  (* Oldest dead person with his age when he died *)
  mutable oldest_dead : int * Gwdb.person;
  (* Oldest person that is still alive with his age *)
  mutable oldest_still_alive : int * Gwdb.person;
}
(** Statistic about persons in database *)

val stat_base : Gwdb.base -> stats
(** Compute [stats] from the database's persons *)

val print_stats : Gwdb.base -> stats -> unit
(** Prints statistic [stats] on stdout *)
