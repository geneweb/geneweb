(* Public functions for API (plugin v7_descend) *)

val print : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays the descendants of the selected in [conv.env] person. Descendants
    could be displayed by different ways depending on variable {i t} in
    [conv.env] environement:

    - "L" dispalying descendants in unordered list
    - "F" same as "L" but displays only female line
    - "M" same as "L" but displays only female line
    - "H" table dispalying
    - "I" table dispalying with spouses information
    - "A" numerated list (d'Aboville)
    - "V" displaying a tree of descendants

    Previous dispalyings are done by template evaluation. Next ones are done by
    functions inside this module:

    - "B" for [print_aboville]
    - "S" for [display_descendants_level]
    - "K" for [display_descendant_with_table]
    - "N" for [display_descendants_with_numbers]
    - "G" for [display_descendant_index]
    - "C" for [display_spouse_index]
    - "T" for [print_tree]

    Variable {i v} is used to select maximal level to descend for descendant
    displaying (1 for children, 2 for grandchildren, etc). If {i t} variable
    isn't defined, then displays the form that allows customising of display. *)
