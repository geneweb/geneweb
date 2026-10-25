(** Verified anomalies, recorded by several wizards at once.

    A wizard who has checked an anomaly and found the data right (a woman
    really married at 13, a man really dead at 104) marks it verified, and the
    report stops counting it. A verification covers that anomaly only: its
    kind, its persons and events ({!Anomaly.identity}). Another anomaly of the
    same person, found later, is listed again. A verification can be
    cancelled.

    Files, in the base config directory, all JSON Lines with one record per
    line: [{"a": anomaly, "by": wizard, "at": "YYYY-MM-DD HH:MM:SS",
    "ok": true}] ([false] for a cancellation).
    - [verified.jsonl]: the verified anomalies, merged.
    - [verified_d/<wizard>.jsonl]: what each wizard did since the last merge.
      Each wizard appends to their own file, so simultaneous verifications
      need no lock, and they count as soon as written ({!load}).
    - [verified_d/archive/<timestamp>/]: the merged fragments. *)

val can_verify : Anomaly.kind -> bool
(** Kinds that can be true facts: ages at marriage, at a child's birth and at
    death, age gaps, children spacing and order, events order, possible
    duplicates, undefined sex. The other kinds are always errors to correct. *)

type entry = { anomaly : Anomaly.t; wizard : string; date : string }

type t
(** The verified anomalies: the merged file, then the pending records in
    chronological order. *)

val load : string -> t
(** [load config_dir]. Missing files count as empty. *)

val find : t -> Anomaly.t -> entry option
val mem : t -> Anomaly.t -> bool
val size : t -> int

val entries : t -> entry list
(** Most recent first. *)

val pending : string -> (string * string) list
(** [pending config_dir] lists the fragments not merged yet, as
    [(wizard, path)], sorted by wizard. *)

val record :
  string -> wizard:string -> verified:bool -> Anomaly.t -> (unit, string) result
(** [record config_dir ~wizard ~verified a] appends a verification of [a]
    (or its cancellation, when [verified] is [false]) to the wizard's
    fragment. [Error] for a kind that {!can_verify} refuses, an anomaly about
    an unnamed person, or a write failure. *)

type fold_result = {
  entries : int;  (** verified anomalies after the merge *)
  fragments : int;  (** fragments merged and archived *)
  rewritten : bool;  (** [verified.jsonl] changed (the old one kept as .bak) *)
}

val fold : string -> fold_result
(** [fold config_dir] merges the pending fragments into [verified.jsonl] under
    a lock. Each fragment is claimed by a rename before being read, so a
    record written meanwhile lands in a new fragment, merged next time. *)
