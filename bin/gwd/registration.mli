type hook = Geneweb.Config.config -> string option -> unit
type handler = Geneweb.Config.config -> string option -> bool

val register : name:string -> hook list -> (string * handler) list -> unit
(** [register ~name hooks handlers] registers the plugin [name] with [hooks] and
    [handlers]. *)

val call_hooks : (name:string -> hook -> unit) -> unit
(** [call_hooks f] iterates on all the registered hooks in order. *)

val try_handlers : meth:string -> (name:string -> handler -> bool) -> bool
(** [try_handlers ~meth f] tries handlers for the method [meth] in order until
    one succeed. *)

val all_registered : unit -> string list
(** [all_registered ()] returns the list of all the registered plugins. *)
