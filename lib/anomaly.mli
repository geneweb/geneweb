(** Anomalies of a base: the warnings of {!Check.check_base}, kept as typed
    values that survive the base being rebuilt.

    A {!Def.warning} refers to persons and families by [iper] and [ifam], which
    change at every gwu/gwc cycle. An anomaly refers to them by key (first name,
    occurrence number, surname) instead, so that a report written by gwc can be
    read later by gwd, and a verified anomaly can be recorded in the whitelist
    ({!AnomalyWhitelist}) and still match after a rebuild.

    The report is produced by gwc and ged2gwb during the check they already run
    while building a base, or by the [anoma] tool on an existing base. It is
    stored as JSON Lines in [<base>.gwb/config/anomalies.jsonl] and rendered by
    gwd (m=ANOMA, {!AnomalyDisplay}) in the reader's language. *)

type person = { first_name : string; surname : string; occ : int }
(** A person, by key. *)

type couple = { father : person; mother : person }
(** A family, by the keys of its two parents. *)

type pevent = string Def.gen_pers_event_name
type fevent = string Def.gen_fam_event_name

(** One anomaly. Mirrors {!Def.warning}, keeping what is needed to render the
    message; comments name the persons in constructor order. *)
type t =
  | BigAgeBetweenSpouses of person * person * int
      (** spouse, spouse, age difference in years *)
  | BirthAfterDeath of person
  | ChangedOrderOfChildren of couple
  | ChangedOrderOfFamilyEvents of couple
  | ChangedOrderOfMarriages of person
  | ChangedOrderOfPersonEvents of person
  | ChildrenNotInOrder of couple * person * person  (** parents, elder, child *)
  | CloseChildren of couple * person * person
  | DeadOld of person * int  (** age at death in years *)
  | DeadTooEarlyToBeFather of person * person  (** father, child *)
  | DistantChildren of couple * person * person
  | FEventOrder of person * fevent * fevent
  | FWitnessEventAfterDeath of person * fevent * couple
      (** witness, event, family of the event *)
  | FWitnessEventBeforeBirth of person * fevent * couple
  | IncoherentAncestorDate of person * person  (** ancestor, person *)
  | IncoherentSex of person
  | MarriageDateAfterDeath of person
  | MarriageDateBeforeBirth of person
  | MotherDeadBeforeChildBirth of person * person  (** mother, child *)
  | OldForMarriage of person * int * couple  (** age at marriage *)
  | ParentBornAfterChild of person * person  (** parent, child *)
  | ParentTooOld of person * int * person  (** parent, age, child *)
  | ParentTooYoung of person * int * person
  | PEventOrder of person * pevent * pevent
  | PossibleDuplicateFam of couple  (** a couple with several unions *)
  | PossibleDuplicateFamHomonymous of couple * person
      (** a couple, and the spouse having unions with several homonyms *)
  | PWitnessEventAfterDeath of person * pevent * person
      (** witness, event, person of the event *)
  | PWitnessEventBeforeBirth of person * pevent * person
  | TitleDatesError of person * string  (** title, as "ident place" *)
  | UndefinedSex of person
  | YoungForMarriage of person * int * couple

(** The kind of an anomaly, one per constructor of {!t}. *)
type kind =
  | K_BigAgeBetweenSpouses
  | K_BirthAfterDeath
  | K_ChangedOrderOfChildren
  | K_ChangedOrderOfFamilyEvents
  | K_ChangedOrderOfMarriages
  | K_ChangedOrderOfPersonEvents
  | K_ChildrenNotInOrder
  | K_CloseChildren
  | K_DeadOld
  | K_DeadTooEarlyToBeFather
  | K_DistantChildren
  | K_FEventOrder
  | K_FWitnessEventAfterDeath
  | K_FWitnessEventBeforeBirth
  | K_IncoherentAncestorDate
  | K_IncoherentSex
  | K_MarriageDateAfterDeath
  | K_MarriageDateBeforeBirth
  | K_MotherDeadBeforeChildBirth
  | K_OldForMarriage
  | K_ParentBornAfterChild
  | K_ParentTooOld
  | K_ParentTooYoung
  | K_PEventOrder
  | K_PossibleDuplicateFam
  | K_PossibleDuplicateFamHomonymous
  | K_PWitnessEventAfterDeath
  | K_PWitnessEventBeforeBirth
  | K_TitleDatesError
  | K_UndefinedSex
  | K_YoungForMarriage

val kind : t -> kind

val all_kinds : kind list
(** Every kind, in display order. *)

val kind_name : kind -> string
(** Stable name of a kind ([DeadOld] …), used in the JSON file, in URLs and in
    the [anomalies_hidden] base parameter. *)

val kind_of_name : string -> kind option

val age : t -> int option
(** The age carried by the anomaly, if any. *)

(** What an anomaly is about: the person or family to look at and correct. *)
type subject = Person of person | Couple of couple

val subjects : t -> subject list
(** Main subjects, first one first. Related persons (a child, a witnessed
    person's relative) are not subjects. *)

val persons : t -> person list
(** Every person the anomaly names, subjects first. *)

val person_key : person -> string
(** ["first_name.occ surname"], as in the base history. *)

val is_unnamed : person -> bool
(** The person is "? ?": its key does not identify it. *)

val identity : t -> string
(** What identifies an anomaly across analyses: its kind, the keys of its
    persons, its events. Ages are left out, so that a verification survives a
    change of date that keeps the anomaly. *)

val to_json : t -> Yojson.Safe.t
(** One anomaly, as in the report:
    [{"k": kind, "p": persons, "c": couples, "n": integers, "e": events, "s":
     strings}]. *)

val of_json : Yojson.Safe.t -> t option

(** {1 Collecting} *)

val of_warning : Geneweb_db.Driver.base -> CheckItem.base_warning -> t

val collector :
  Geneweb_db.Driver.base -> (CheckItem.base_warning -> unit) * (unit -> t list)
(** [collector base] is [(add, result)]: pass [add] the warnings of a check run
    on [base] (gwc and ged2gwb chain it with their own warning printer), then
    [result ()] gives the anomalies in the order found, without duplicates. *)

val collect : Geneweb_db.Driver.base -> t list
(** Run {!Check.check_base} and keep its warnings, in the order found. Errors
    are ignored: gwc reports them itself, and a base with errors is not written.
*)

(** {1 Report} *)

type change = { time : string; wizard : string }
(** Last change of a person in the base history: ["YYYY-MM-DD HH:MM:SS"] and the
    wizard's login. *)

type report = {
  date : string;  (** analysis time, ["YYYY-MM-DD HH:MM:SS"] local *)
  producer : string;  (** ["gwc"], ["ged2gwb"] or ["anoma"] *)
  history_days : int;  (** window of [changes] *)
  anomalies : t list;
  changes : (string * change) list;
      (** last change of the persons of [anomalies] within the last
          [history_days] days, by {!person_key} *)
}

val default_history_days : int

val make :
  ?history_days:int -> bdir:string -> producer:string -> t list -> report
(** [make ~bdir ~producer anomalies] dates the report and reads the recent
    changes of its persons from [<bdir>/history]. *)

val write :
  ?history_days:int -> bdir:string -> producer:string -> t list -> unit
(** [write ~bdir ~producer anomalies] saves [make ~bdir ~producer anomalies] in
    [<bdir>/config] ({!GWPARAM.base_config_dir}), silently: gwc and ged2gwb
    output is unchanged. A failure is reported on stderr and otherwise ignored:
    the report must never make a build fail. *)

val file : string -> string
(** [file config_dir] is the report path in the base config directory
    ({!GWPARAM.base_config_dir}): [anomalies.jsonl]. *)

val save : string -> report -> unit
(** [save config_dir report] writes the report atomically, creating the
    directory if needed. The file is JSON Lines: the header (date, producer,
    changes) on the first line, then one anomaly per line. *)

val fold :
  ?kind:kind -> string -> ('a -> t -> 'a) -> 'a -> (report * 'a, string) result
(** [fold ?kind config_dir f init] reads the report line by line, folding [f]
    over its anomalies (only those of [kind] when given: the other lines are
    skipped unparsed). Returns the header, as a report with no anomalies, and
    the result. [Error] for a missing or unreadable file, or a format this
    version does not know; a damaged line is skipped. *)

val load : string -> (report, string) result
(** [load config_dir] reads the whole report. *)

val exists : string -> bool
