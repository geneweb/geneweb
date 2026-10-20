open Config
open Def
module Driver = Geneweb_db.Driver

val reconstitute_from_fevents :
  bool ->
  'string ->
  ('person, 'string) gen_fam_event list ->
  (relation_kind * Adef.cdate * 'string * 'string * 'string)
  * divorce
  * ('person * witness_kind) array
(** [reconstitute_from_fevents nsck empty fevents] derives the summary fields of
    a family from its events [fevents], whatever the representation of persons
    and strings ([empty] being the empty string):

    - the main union event as (relation, date, place, note, source): the
      earliest marriage, otherwise the latest other union event, [NoMention] and
      [Residence] coming last; union events dated before an annulation are
      ignored; without any, [NoMention] with no date and [empty] strings;
    - the divorce: the latest divorce or separation, [NotDivorced] without any;
    - the witnesses of the main union event.

    With [nsck] (no sex check, same-sex couple), [Married] and [NotMarried]
    become their [NoSexesCheck] variants. *)

val effective_mod :
  config ->
  Driver.base ->
  bool ->
  (Update.key, Driver.ifam, string) gen_family ->
  Update.key Adef.gen_couple ->
  Update.key gen_descend ->
  Driver.ifam
  * (Driver.iper, Driver.ifam, Driver.istr) gen_family
  * Driver.iper Adef.gen_couple
  * Driver.iper gen_descend
(** [effective_mod conf base nsck sfam scpl sdes] patches family
    [sfam.fam_index] as submitted, without committing: creates or links its
    persons ({!Update.insert_person}), derives its summary fields from its
    events ({!reconstitute_from_fevents}, the relation following the [nsck]
    input of [conf.env]), infers its origin file when empty, and updates the
    unions of old and new parents, the ascendants of old and new children and
    the related pointers of witnesses. Unless [nsck], parents of unknown sex
    become father and mother, and a parent of the other sex is an error. Returns
    the family as stored: index, family, couple, children.

    Errors (name conflict, unknown linked person, sex mismatch, same person as
    both parents, new child already having parents) print their page and raise
    {!Update.ModErr}. *)

val effective_del : config -> Driver.base -> Driver.family -> unit
(** [effective_del conf base fam] deletes family [fam]
    ({!Driver.delete_family_rec}, which also removes the empty persons it
    leaves), updates the notes links database and records a [df] history entry
    for each parent and child. Patches are left uncommitted. *)

val all_checks_family :
  config ->
  Driver.base ->
  Driver.ifam ->
  (Driver.iper, Driver.ifam, Driver.istr) gen_family ->
  Driver.iper Adef.gen_couple ->
  Driver.iper gen_descend ->
  Update.key Adef.gen_couple
  * Update.key gen_descend
  * ((Driver.iper array * Driver.iper array)
    * (Driver.iper array * Driver.iper array))
    option ->
  CheckItem.base_warning list * CheckItem.base_misc list
(** [all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)] checks
    family [ifam] once patched as [fam], [cpl], [des]. The ancestry loop check
    runs when the submitted couple [scpl] or children [sdes] link an existing
    person, unless [onfs], the (parents, children) before and after a
    modification, shows that every linked person was already there. Warnings
    come from {!CheckItem.family} and miscellanea from
    {!CheckItem.check_other_fields}; the reordering of the marriages of a parent
    or of the family events they report is applied. A loop prints its page and
    raises {!Update.ModErr}. Returns the warnings and miscellanea, sorted and
    deduplicated. *)

val patch_parent_with_pevents :
  Driver.base -> Driver.iper Adef.gen_couple -> unit
(** [patch_parent_with_pevents base cpl] aligns the events of both parents with
    their main fields: when birth, baptism or death has a date or a place, the
    first event of that kind takes its date, place, note and source, keeping its
    witnesses, or is added if missing. Other events are kept. *)

val patch_children_with_pevents : Driver.base -> Driver.iper gen_descend -> unit
(** [patch_children_with_pevents base des] is {!patch_parent_with_pevents} for
    the children [des]. *)

val print_family :
  ?with_parents:Driver.iper list ->
  config ->
  Driver.base ->
  CheckItem.base_warning list * CheckItem.base_misc list ->
  Driver.iper Adef.gen_couple ->
  Driver.iper gen_descend ->
  unit
(** [print_family ?with_parents conf base (wl, ml) cpl des] prints the body of
    the result page of a family update: the parents [cpl], each one listed in
    [with_parents] followed by its own parents, the children [des], then the
    warnings [wl] and miscellanea [ml]. It keeps the default source [dsrc] in
    [conf.henv] for the next forms, set from the family source [src] when
    [rdsrc=on] and [dsrc] is empty. *)

val print_mod_aux :
  config ->
  Driver.base ->
  ((Update.key, Driver.ifam, string) gen_family ->
  Update.key Adef.gen_couple ->
  Update.key gen_descend ->
  unit) ->
  unit
(** [print_mod_aux conf base callback] is the submission step shared by
    [m=MOD_FAM_OK] and [m=MRG_MOD_FAM_OK]: rejects a stale [digest], redisplays
    the form on an insertion request or a [return], reports missing names,
    otherwise passes the submitted family, couple and children, stripped of
    empty entries, to [callback]. *)

val print_add : config -> Driver.base -> unit
(** [print_add conf base] handles [m=ADD_FAM_OK], and [m=ADD_PAR_OK] through
    {!print_add_parents}: rejects a stale [digest] (number of families of [ip]),
    redisplays the form on an insertion request or a [return], refuses a family
    linked to no existing person when [propose_add_family=no] and reports
    missing names; then creates the family and the parents' families of the
    [pa<k><j>] blocks, commits once, records history and prints the result. *)

val print_add_parents : config -> Driver.base -> unit
(** [print_add_parents conf base] handles [m=ADD_PAR_OK]: when the form only
    links two parents already in a union and the child, adds the child to that
    union, with the parents' families of the [pa<k><j>] blocks; otherwise
    {!print_add}. *)

val print_mod : config -> Driver.base -> unit
(** [print_mod conf base] handles [m=MOD_FAM_OK]: updates family [i] and creates
    the parents' families of the [pa<k><j>] blocks, commits once, records
    history and prints the result; an unchanged family without such blocks
    redirects to the person page with [nomod=1]. *)

val print_del : config -> Driver.base -> unit
(** [print_del conf base] handles [m=DEL_FAM_OK]: deletes family [i], commits
    and prints the confirmation. *)

val print_inv : config -> Driver.base -> unit
(** [print_inv conf base] handles [m=INV_FAM_OK]: swaps family [f] of person [i]
    with the preceding one. *)

val print_change_order_ok : config -> Driver.base -> unit
(** [print_change_order_ok conf base] handles [m=CHG_FAM_ORD_OK]: moves family
    [f] of person [i] to position [n] ({!UpdateFam.change_order}). *)

val print_change_event_order : config -> Driver.base -> unit
(** [print_change_event_order conf base] handles [m=CHG_EVT_FAM_ORD_OK]:
    reorders the events of family [i] by the submitted [e_id<k>] and [e_pos<k>],
    derives the summary fields again and prints the result; an unchanged order
    redirects to the person page. *)
