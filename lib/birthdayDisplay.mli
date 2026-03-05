val gen_print :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  (unit ->
  Geneweb_db.Driver.person
  * (Config.config ->
    Geneweb_db.Driver.base ->
    Geneweb_db.Driver.person ->
    Adef.safe_string)) ->
  ?max_d:int ->
  ?mode:(unit -> unit) ->
  bool ->
  unit
(** Display anniversaries for a given month, one section per day. [f_scan]
    returns the next person and a function producing the display text/link for
    that person; raises [Not_found] when exhausted. When [dead_people] is true,
    lists birth and death anniversaries with death reason; otherwise lists
    birthdays of living people only. [~max_d] is the maximum total relationship
    degree reachable for the target person (passed to the month/degree form).
    [~mode] emits hidden inputs specific to the calling context; when provided,
    a month selector (and degree stepper in cousins context) is rendered after
    the listing. *)

val print_birth : Config.config -> Geneweb_db.Driver.base -> int -> unit
(** Displays birthdays for alive people for a given month *)

val print_dead : Config.config -> Geneweb_db.Driver.base -> int -> unit
(** Displays anniversaries for dead people for a given month *)

val print_marriage : Config.config -> Geneweb_db.Driver.base -> int -> unit
(** Displays marriage anniversaries for a given month *)

val gen_print_menu_birth :
  Config.config ->
  Geneweb_db.Driver.base ->
  (unit ->
  Geneweb_db.Driver.person
  * (Config.config ->
    Geneweb_db.Driver.base ->
    Geneweb_db.Driver.person ->
    Adef.safe_string)) ->
  (unit -> unit) ->
  unit
(** [gen_print_menu_birth conf base (next,txt_of) mode] displays the main
    birthdays menu for alive people that contains:
    - Persons that has their birthdays today
    - Persons that has their birthdays tomorrow
    - Persons that has their birthdays after today
    - Form to select the month of birthdays we want to see. [next] is function
      that returns next person from iterator, [txt_of] text/link that describes
      person's information and [mode] that add some additional hidden inputs in
      the month form *)

val print_menu_birth : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the main birthdays menu considering all alive people *)

val gen_print_menu_dead :
  Config.config ->
  Geneweb_db.Driver.base ->
  (unit ->
  Geneweb_db.Driver.person
  * (Config.config ->
    Geneweb_db.Driver.base ->
    Geneweb_db.Driver.person ->
    Adef.safe_string)) ->
  (unit -> unit) ->
  unit
(** [gen_print_menu_dead conf base (next,txt_of) mode] displays the main
    anniversaries menu for dead people that contains:
    - Persons that has their anniversaries today
    - Persons that has their anniversaries tomorrow
    - Persons that has their anniversaries after today
    - Form to select the month of anniversaries we want to see. [next] is
      function that returns next person from iterator, [txt_of] text/link that
      describes person's information and [mode] that add some additional hidden
      inputs in the month form *)

val print_menu_dead : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the main anniversaries menu considering all dead people *)

val print_menu_marriage : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the main wedding anniversaries menu *)

val print_anniversaries : Config.config -> unit
(** Displays the menu of anniversaries selection *)
