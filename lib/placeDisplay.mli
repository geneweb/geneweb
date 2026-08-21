(* Copyright (c) 1998-2007 INRIA *)

(** Rendering of the Places-Persons-Surnames (m=PPS) index: the HTML page that
    lists every place of the base together with the surnames and persons
    attached to it, in either the nested "long" or the flat "short" display.

    The scan and place-string manipulation live in {!Place}; this module only
    turns the resulting array into HTML. *)

val print_all_places_surnames : Config.config -> Geneweb_db.Driver.base -> unit
(** [print_all_places_surnames conf base] renders the full m=PPS page. Event
    selection ([bi] / [ba] / [de] / [bu] / [ma]), the initial-letter filter [k],
    the display mode ([display=long|short]), the sort order ([a_sort] / [f_sort]
    / [up]) and the [keep] grouping depth are all read from the request
    parameters. Automatically falls back from long to short display when the
    base holds too many distinct places. *)
