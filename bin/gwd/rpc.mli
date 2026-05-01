val is_available : bool

val make_backend : Unix.file_descr -> Gwd_lib.Reporter.backend
(** [make_backend fd] creates a backend from the file descriptor [fd]. This
    backend is adapted to be used in a concurrent Lwt sofware. *)

val start :
  ?interface:string ->
  port:int ->
  max_requests:int ->
  timeout:float ->
  task_timeout:float ->
  ?tls:bool ->
  ?certfile:string ->
  ?keyfile:string ->
  index_fuel:int ->
  index_dir:string ->
  unit
(** [start ?interface ~port ~max_requests ~timeout ?task_timeout ~tls
     ~index_fuel ~index_dir] starts the RPC server on the interface. See the
    documentation of [Geneweb_rpc.Server.start] for more details. *)
