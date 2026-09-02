(* FIXME: this type is an instance of `Request.handler`, that is
   `(string option, unit) Request.handler.

   We cannot define it as an instance because of a cyclic dependency between
   this module and `Request`. *)
type hook =
  Geneweb_http.Connection.t -> Geneweb.Config.config -> string option -> unit

(* FIXME: this type is an instance of `Request.handler`, that is
   `(string option, bool) Request.handler.

   We cannot define it as an instance because of a cyclic dependency between
   this module and `Request`. *)
type handler =
  Geneweb_http.Connection.t -> Geneweb.Config.config -> string option -> bool

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
