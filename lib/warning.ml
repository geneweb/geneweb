(*TODO why polymorphic *)

(** Database warnings attached to the specification of the person, family, relation, etc. *)
type ('iper, 'person, 'family, 'descend, 'title, 'pevent, 'fevent) warning =
  | BigAgeBetweenSpouses of 'person * 'person * Duration.t
    (* Age differece between couples is greater then 50 years *)
  | BirthAfterDeath of 'person  (** Person is born after his death *)
  | IncoherentSex of 'person * int * int  (** Incoherent sex of person *)
  | ChangedOrderOfChildren of 'family * 'descend * 'iper array * 'iper array
      (** Children order has been modified *)
  | ChangedOrderOfMarriages of 'person * 'family array * 'family array
      (** Person's marriages order has been modified *)
  | ChangedOrderOfFamilyEvents of 'family * 'fevent list * 'fevent list
      (** Family's events order has been modified *)
  | ChangedOrderOfPersonEvents of 'person * 'pevent list * 'pevent list
      (** Person's events order has been modified *)
  | ChildrenNotInOrder of 'family * 'descend * 'person * 'person
      (** Children aren't ordered *)
  | CloseChildren of 'family * 'person * 'person
      (** Age difference between two child is less then 7 month (except for twins) *)
  | DeadOld of 'person * Duration.t
      (** Dead old (at the age older then 109 after 1900 year and older then 100 before) *)
  | DeadTooEarlyToBeFather of 'person * 'person
      (** Children is born in more then 1 year after his father's death *)
  | DistantChildren of 'family * 'person * 'person
      (** Age gap between two of siblings greater then 50 years *)
  | FEventOrder of 'person * 'fevent * 'fevent
      (** Familial events haven't been ordered correctly *)
  | FWitnessEventAfterDeath of 'person * 'fevent * 'family
      (** Witness is dead before familial event date *)
  | FWitnessEventBeforeBirth of 'person * 'fevent * 'family
      (** Witness is born after familial event date *)
  | IncoherentAncestorDate of 'person * 'person
      (** Ancestor is born after person's birth *)
  | MarriageDateAfterDeath of 'person  (** Person is married after his death *)
  | MarriageDateBeforeBirth of 'person
      (** Person is married before his birth *)
  | MotherDeadBeforeChildBirth of 'person * 'person
      (** Children is born after his mother's death *)
  | ParentBornAfterChild of 'person * 'person
      (** Parent is born after one of his children *)
  | ParentTooOld of 'person * Duration.t * 'person
      (** Person became a parent at age older then 55 years for mother and 70 for father *)
  | ParentTooYoung of 'person * Duration.t * 'person
      (** Person became a parent at age younger then 11 years old *)
  | PEventOrder of 'person * 'pevent * 'pevent
      (** Personal events haven't been ordered correctly *)
  | PossibleDuplicateFam of 'family * 'family
      (** There is a possibility that two families are a duplicate of each other *)
  | PossibleDuplicateFamHomonymous of 'family * 'family * 'person
      (** There is a possibility that two families are a duplicate of each other (Homonymous spouse) *)
  | PWitnessEventAfterDeath of 'person * 'pevent * 'person
      (** Witness is dead before personal event date *)
  | PWitnessEventBeforeBirth of 'person * 'pevent * 'person
      (** Witness is born after personal event date *)
  | TitleDatesError of 'person * 'title
      (** Title's start date is after end date or person is born after title dates *)
  | UndefinedSex of 'person  (** Person has undefined sex (Neuter) *)
  | YoungForMarriage of 'person * Duration.t * 'family
      (** Person is married before he was 12 years old *)
  | OldForMarriage of 'person * Duration.t * 'family
      (** Person is married after he was 100 years old *)

(** Missing sources warning *)
type ('person, 'descend, 'title) misc = MissingSources
