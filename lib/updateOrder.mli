(** Order of a list of a person or a family (its families, its events), changed
    in one request from a window of the menubar (updateOrder.txt,
    updateOrder.js).

    The request names the person or the family by index [i] or by keys, and
    carries [o], the keys of the items in their new order, and [prev], their
    keys in the order read when the window was opened, both separated by commas.
    It is refused when the stored order differs from [prev], when [o] is not a
    permutation of the items, or when [o] places an item before another one that
    the rules of {!CheckItem} order before it ({!CheckItem.misplaced}). It
    answers [{success, message}] in JSON, like {!UpdateField.print}, [message]
    being the translated notice to display; an unchanged order answers
    [{success:false, message:""}]. A missing or hidden person or family yields
    the standard incorrect-request page. *)

val print_families : Config.config -> Geneweb_db.Driver.base -> unit
(** [print_families conf base] handles [m=CHG_FAM_ORD_OK]: the items are the
    families of the person, keyed by index and ordered by their union dates
    ({!CheckItem.compare_unions}). The union is written at once and the history
    records the inversion (["if"]) of the first moved family. *)
