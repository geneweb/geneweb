(** Unified database synchronization and maintenance library. *)

module Privacy : sig
  val apply_cutoff_date :
    base:Gwdb.base -> cutoff_year:int -> verbosity:int -> bool
  (** Estimate dates and apply privacy access rules (Public / Private)
      based on a cutoff birth year. Returns [true] if any modifications were made. *)

  val apply_list :
    base:Gwdb.base -> access:Def.access -> file:string -> verbosity:int -> bool
  (** Apply explicit access override for persons listed in [file].
      Returns [true] if any person's access level changed. *)
end

module Integrity : sig
  val perform_fixes : base:Gwdb.base -> fast:bool -> verbosity:int -> int
  (** Perform database integrity checks and repairs. Returns the count of fixes performed. *)
end

module CycleCheck : sig
  val detect_loop : base:Gwdb.base -> bool
  (** Check for ancestor-descendant cycle loops in the database.
      Returns [true] if a loop is detected. *)
end

module Consanguinity : sig
  val compute : base:Gwdb.base -> scratch:bool -> verbosity:int -> bool
  (** Calculate consanguinity values across the database. *)
end

module NotesIndexing : sig
  val compute : base:Gwdb.base -> bdir:string -> verbosity:int -> unit
  (** Index cross-references and links in database notes. *)
end

val main : unit -> unit
(** CLI entry point for the gwresync command-line utility. *)
