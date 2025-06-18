val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Displays the menu that lists all person's relatives depending on ancestor
    and his descandant levels specified by [conf.env] variables {i v1} (for
    ancestor) and {i v2} for his descandant. For exemple :

    "v1" = 1, "v2" = 1 - Displays all person's siblings (mount to the person's
    parent (ancestor of level 1) and lists all his children (descandant of level
    1)); "v1" = 2, "v2" = 2 - Displays all cousins; "v1" = 2, "v2" = 1 -
    Displays all uncles/aunts; "v1" = 1, "v2" = 2 - Displays all nieces/nephews;
    etc.

    Variable "t" is used to display anniversaries for relatives like
    [BirthdayDisplay.gen_print]. If nor of those variables are defined, prints
    menu that allows to access the most common relatives (except for direct
    relatives) like cousins, siblings, uncles/aunts, etc. *)
