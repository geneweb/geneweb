type lists = {
  to_delete : Geneweb_db.Driver.iper list;
  to_review : Geneweb_db.Driver.iper list;
  couples_to_delete : Geneweb_db.Driver.ifam list;
  couples_to_review : Geneweb_db.Driver.ifam list;
}
(** Persons without data, persons with data, couples without data and other
    couples, as listed by {!val:print}. *)

val sort : Config.config -> Geneweb_db.Driver.base -> string list -> lists
(** [sort conf base names] sorts the persons whose first name and surname are
    both among [names] (compared with Name.lower) or have no letter or digit
    into the lists displayed by {!val:print}. *)

val blank_names :
  Config.config ->
  Geneweb_db.Driver.base ->
  (string * Geneweb_db.Driver.istr * string) list
(** [blank_names conf base] returns the first names (["fn"]) and surnames
    (["sn"]) other than ["?"] without any letter or digit (Unicode) or that
    Name.lower reduces to nothing, with their string index and value. They are
    read from the checkdata caches of first names and surnames when they exist,
    so that a name of an outdated cache may have no bearer left, and from the
    persons otherwise. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] displays the form of the names that stand for an unknown
    person (parameter [ph], comma-separated, the default list when empty,
    compared with Name.lower; names that Name.lower reduces to nothing, such as
    ["?"], ["??"] or ["-"], always are) and of the number of entries shown per
    list (parameter [max], 0 to 500, default 100). Once submitted, it displays
    links to the non-empty lists below with their sizes, then the first entries
    of each list. Two first lists give the first names and surnames without any
    letter or digit (Unicode) other than ["?"]: those that Name.lower reduces to
    nothing, then the others, to check. Each entry links to its dictionary
    entry, which counts its bearers, with its invisible characters highlighted,
    to rename it or to add it to [ph]. The persons whose first name and surname
    are both such names are found through the first name and surname indexes,
    and sorted into:
    - persons without data or parents, replaced by ["? ?"] in their families or
      deleted when isolated, with a button to delete them all;
    - persons with data, or with parents unless named ["? ?"], to process by
      hand;
    - couples of two such persons with at most one child and no family data,
      whose spouses have no data, parents or other family, with a button to
      delete all their spouses, which removes the families;
    - the other couples of two such persons, to process by hand.

    A person in a couple of two such persons is listed through that couple only.
    A personal source equal to the source of one of the person's families is not
    counted as data; portraits, coats of arms and carrousel images, saved
    versions included, are. *)

val print_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** [print_ok conf base] sorts the persons again for the names of [ph] and
    deletes all the persons of the list given by [del]: ["persons"] for the
    persons without data, ["couples"] for the spouses of the couples without
    data. It commits once and calls [notify_change] once, then archives the
    history file of each deleted person ({!HistoryDiff.archive_deleted}) and
    logs it at info level, without recording anything in the history. *)
