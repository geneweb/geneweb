val assets : string ref
(** When dynamically loading a plugin, this variable contains the path of the
    assets directory associated to the plugin being currently loaded. *)

val register :
  ns:string ->
  (string * (string -> Geneweb.Config.config -> string option -> bool)) list ->
  unit
(** [register ~ns handlers] register modes handlers of a plugin.

    [ns] is the namespace of the plugin (i.e. its name)

    [handler] is a associative list of handler. The key is the mode (the `m`
    GET/POST parameter). The value is the handler itself. The difference between
    a plugin handler and default gwd's handlers (the ones in request.ml) is that
    a plugin handler takes an extra (first) argument being the path of the asset
    directory associated to this plugin and returns a boolean.

    If the handler returns [true], it means that it actually processed the
    request. If is is [false], [gwd] must try another plugin handler to treat
    the request. If no plugin is suitable, gwd's default handler must be used,
    or fail if it does not exists.

    Handlers can overwrite pre-existing modes or create new ones. *)

val ht :
  (string, string * (Geneweb.Config.config -> string option -> bool)) Hashtbl.t
(** Table of handlers registered by plugins. *)

val register_se :
  ns:string ->
  (string -> Geneweb.Config.config -> string option -> unit) ->
  unit
(** [register_se ~ns hook] register a plugin hook (side effect function).

    If enabled, hooks are executed before the request handlers, in the order of
    registration (first registred = first executed).

    For exemple, a plugin could be to change the [conf] output to print
    everything in a buffer and apply a transformation to the resulting document
    before actually sending it to the client. *)

val se : (string * (Geneweb.Config.config -> string option -> unit)) list ref
(** Table of hooks registered by plugins. *)
