(** Grammatical forms of event names.

    An event name such as "baptism" is translated by a lexicon key. Sentences
    that embed it often need it with a preposition and an article that agree
    with the noun: French "au baptême", "aux funérailles", "du mariage"; German
    "bei der Taufe", "des Todes"; Russian "при крещении". Such phrases are
    stored as optional lexicon entries named after the key of the event,
    followed by a form suffix: "baptism (at)", "baptism (of)".

    A language that has no entry for an event (or a custom event, whose name is
    free text) gets [None]; callers then fall back to the bare name and to the
    legacy sentence. *)

type form =
  | At  (** "at the X", used after a witness kind *)
  | Of  (** "of the X", used after a noun such as "notes" *)

val pevent_key : 'a Def.gen_pers_event_name -> string option
(** Lexicon key of a personal event name, [None] for a custom event. *)

val fevent_key : 'a Def.gen_fam_event_name -> string option
(** Lexicon key of a family event name, [None] for a custom event. *)

val form_key : form -> string -> string
(** [form_key form key] is the lexicon key holding [form] of the event named by
    [key]: [form_key At "baptism"] is ["baptism (at)"]. A stacked key keeps its
    first variant: [form_key Of "occupation/occupations"] is
    ["occupation (of)"]. *)

val pevent :
  Config.config -> form -> 'a Def.gen_pers_event_name -> string option
(** [pevent conf form name] is the translation of [form] of [name] in the
    language of [conf], if the lexicon has one. *)

val fevent : Config.config -> form -> 'a Def.gen_fam_event_name -> string option
(** Same as [pevent] for a family event. *)
