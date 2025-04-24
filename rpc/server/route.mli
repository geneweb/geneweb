type t
(** Type of a route. *)

val path : string -> Geneweb_rpc.Service.t -> t
(** [path n srv] attaches the service [srv] to the path [n]. *)

val route : t list -> Server.handler
(** [route l] generates the handler for a server corresponding to the routes
    [l]. *)
