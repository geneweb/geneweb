
(** [gen_print conf base month (next,txt_of) dead_people] displays anniversaries
    for a given month separated by day.
    If [dead_people] is true then displays birth/death anniversaries for dead people with death reason.
    Otherwise displays birthdays for alive people.
    [next] is function that returns next person from iterator
    and [txt_of] text/link that describes person's information *)
val gen_print
  :  Config.config
  -> Gwdb.base
  -> int
  -> (unit -> Gwdb.person * (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string))
  -> bool
  -> unit

(** Displays birthdays for alive people for a given month *)
val print_birth : Config.config -> Gwdb.base -> int -> unit

(** Displays anniversaries for dead people for a given month *)
val print_dead : Config.config -> Gwdb.base -> int -> unit

(** Displays marriage anniversaries for a given month *)
val print_marriage : Config.config -> Gwdb.base -> int -> unit

(** [gen_print_menu_birth conf base (next,txt_of) mode] displays the main birthdays menu for alive people
    that contains:
    - Persons that has their birthdays today
    - Persons that has their birthdays tomorrow
    - Persons that has their birthdays after today
    - Form to select the month of birthdays we want to see.
    [next] is function that returns next person from iterator, [txt_of] text/link that
    describes person's information and [mode] that add some additional hidden inputs in the month form *)
val gen_print_menu_birth
  :  Config.config
  -> Gwdb.base
  -> (unit -> Gwdb.person * (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string))
  -> (unit -> unit)
  -> unit

(** Displays the main birthdays menu considering all alive people *)
val print_menu_birth : Config.config -> Gwdb.base -> unit

(** [gen_print_menu_dead conf base (next,txt_of) mode] displays the main anniversaries menu for dead people
    that contains:
    - Persons that has their anniversaries today
    - Persons that has their anniversaries tomorrow
    - Persons that has their anniversaries after today
    - Form to select the month of anniversaries we want to see.
    [next] is function that returns next person from iterator, [txt_of] text/link that
    describes person's information and [mode] that add some additional hidden inputs in the month form *)
val gen_print_menu_dead
  :  Config.config
  -> Gwdb.base
  -> (unit -> Gwdb.person * (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string))
  -> (unit -> unit)
  -> unit

(** Displays the main anniversaries menu considering all dead people *)
val print_menu_dead : Config.config -> Gwdb.base -> unit

(** Displays the main wedding anniversaries menu *)
val print_menu_marriage : Config.config -> Gwdb.base -> unit

(** Displays the menu of anniversaries selection *)
val print_anniversaries : Config.config -> unit
