val main_page : Config.config -> Gwdb.base -> unit
(** Displays a menu for merging possible duplications of persons *)

val answ_ind_y_n : Config.config -> Gwdb.base -> unit
(** Either displays the merge dupliate menu if `answer_y` is not a key of the
    request, or a form for merging two persons *)

val answ_fam_y_n : Config.config -> Gwdb.base -> unit
(** Same than `answ_ind_y_n` but for families *)
