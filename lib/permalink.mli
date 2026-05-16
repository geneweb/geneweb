open Config

val query : config -> Geneweb_db.Driver.base -> Adef.encoded_string
(** [query conf base] returns the current request query string in which every
    index-bearing parameter is replaced by its stable key form. Neither the
    scheme/host/port nor the base name are included: the client rebuilds the
    absolute URL from window.location and the wizard/friend base-name suffix.

    Rewritten parameters (audited list, to be revisited as tools evolve):
    - [i] -> [p] & [n] [& oc]
    - [ei] -> [ep] & [en] [& eoc]
    - [i?] (2 chars) -> [p?] & [n?] [& oc?] (e.g. i1, i2)
    - [ef?] -> father's key of the family [& f?] Dropped parameters: [dsrc],
      [escache], [templ]. *)

val query_aux : config -> Adef.encoded_string
(** Like {!query} but without database access: index parameters are kept
    verbatim (no key rewrite), only volatile parameters are dropped. Used where
    no base is in scope (generic page header) and for index-only tools. *)

val script : config -> Adef.encoded_string -> Adef.safe_string
(** [script conf q] emits the common permalink head block: the inline
    [<script>window.GWPERMA={"q":...,"r"?:1|2,"t":{...}};</script>] data island
    plus a deferred, content-hashed [<script>] loading [copylink.min.js]. [r] is
    the server-known role (1 friend, 2 wizard, omitted if anonymous); [t] holds
    the role-restricted localized labels (the external script cannot reach the
    lexicon). The bare base name and cgi flag are not embedded: the client
    derives them from window.location. JSON values are escaped so the block is
    safe in [<head>]. *)
