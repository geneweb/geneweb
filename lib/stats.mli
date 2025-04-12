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
  mutable oldest_father : int * Geneweb_db.Driver.person;
  (* Oldest mother with age when she became mother *)
  mutable oldest_mother : int * Geneweb_db.Driver.person;
  (* Youngest father with age when he became father *)
  mutable youngest_father : int * Geneweb_db.Driver.person;
  (* Youngest mother with age when he became mother *)
  mutable youngest_mother : int * Geneweb_db.Driver.person;
  (* Oldest dead person with his age when he died *)
  mutable oldest_dead : int * Geneweb_db.Driver.person;
  (* Oldest person that is still alive with his age *)
  mutable oldest_still_alive : int * Geneweb_db.Driver.person;
}
(** Statistic about persons in database *)

val stat_base : Geneweb_db.Driver.base -> stats
(** Compute [stats] from the database's persons *)

val print_stats : Geneweb_db.Driver.base -> stats -> unit
(** Prints statistic [stats] on stdout *)
