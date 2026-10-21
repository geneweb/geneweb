val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a form where all person's children with their first names, surnames
    and occurence numbers are listed and could be modified on submit. Id of
    person should be mentionned in environement [conf.env] with binding
    {i "ip"=id} otherwise displays Bad request page. *)

val print_ok : Config.config -> Geneweb_db.Driver.base -> unit
(** Performs and displays results of children modification requested by form
    submiting. If changes of one of children raises an error displays
    corresponding error page that either just informs user about error source
    either propose to fix up solution. *)
