val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Entry point for [m=C] (Cousins). Dispatches on [conf.env]:

    - [json_level=N] → JSON endpoint: writes the [N]-th level slice with
      [Content-type: application/json], consumed by [cousmenu.js] to pull levels
      incrementally.
    - [v1=1] and [v2=1], or [v1=0], or [v2=0] → renders the [cousins] template
      (close relatives: siblings, direct ancestors, direct descendants).
    - [v1] set together with any [v2] → classic HTML matrix rendered by
      [print_cousins], iterating Sosa branches up to [v1] and listing
      descendants down to [v2], capped by [max_cousins].
    - [t=AN] or [t=AD], wizard or friend only → anniversaries page over the
      relatives set via [BirthdayDisplay.gen_print].
    - otherwise → renders the [cousmenu] template (menu view) and inlines the
      level-0 cousins JSON in a [<script id="cousins-data">] block for
      [cousmenu.js] to bootstrap from. *)
