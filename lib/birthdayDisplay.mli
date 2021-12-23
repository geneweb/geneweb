
val gen_print :
  Config.config ->
  Gwdb.base ->
  int ->
  (unit ->
   Gwdb.person *
   (Config.config -> Gwdb.base -> Gwdb.person -> string)) ->
  bool -> unit

(** Displays birthdays for a given month *)
val print_birth : Config.config -> Gwdb.base -> int -> unit

(** Displays death anniversaries for a given month *)
val print_dead : Config.config -> Gwdb.base -> int -> unit

(** Displays marriage anniversaries for a given month *)
val print_marriage : Config.config -> Gwdb.base -> int -> unit

val gen_print_menu_birth :
  Config.config ->
  Gwdb.base ->
  (unit ->
   Gwdb.person *
   (Config.config -> Gwdb.base -> Gwdb.person -> string)) ->
  (unit -> 'a) -> unit

(** Displays the main anniversaries menu *)
val print_menu_birth : Config.config -> Gwdb.base -> unit

val gen_print_menu_dead :
  Config.config ->
  Gwdb.base ->
  (unit ->
   Gwdb.person *
   (Config.config -> Gwdb.base -> Gwdb.person -> string)) ->
  (unit -> 'a) -> unit

(** Displays the main death anniversaries menu *)
val print_menu_dead : Config.config -> Gwdb.base -> unit

(** Displays the main marriage anniversaries menu *)
val print_menu_marriage : Config.config -> Gwdb.base -> unit

(** Displays the menu of anniversaries modification *)
val print_anniversaries : Config.config -> unit
