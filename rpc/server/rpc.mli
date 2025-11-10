type handler =
  Unix.sockaddr ->
  string ->
  Geneweb_rpc.Json_rpc.Request.t ->
  Geneweb_rpc.Json_rpc.Response.t Lwt.t
(** Type of user handler. This first argument is client socket address and the
    second argument is the request object. *)

val start :
  interface:string ->
  port:int ->
  ?max_connection:int ->
  ?idle_timeout:float ->
  ?task_timeout:float ->
  ?tls:bool ->
  ?certfile:string ->
  ?keyfile:string ->
  handler ->
  unit
(** [start ~interface ~port hdl] initializes an asynchronous RPC server based on
    a HTTP/1.1 server with WebSocket capability.

    The server accepts only HTTP requests that upgrade to the WebSocket
    protocol; all other requests are rejected. The content of WebSocket frames
    is expected to be a valid Request object of the JSON-RPC protocol.

    The server listens on the specified [interface] and [port].

    Optional parameters:
    - [max_connection]: Limits the number of concurrent connections per client
      if specified; otherwise, no limit is enforced.
    - [idle_timeout]: Specifies the duration (in seconds) after which idle
      connections are closed if specified; otherwise no limit is enforced.
    - [task_timeout]: Specifies the duration (in seconds) after which tasks are
      canceled. Note that this timeout may not cancel non-cooperative tasks.
    - [tls]: Enables TLS support if set to [true] and valid certificates and
      private key files are provided. If [tls] is [true], connections are
      accepted only through TLS.

    @raise Invalid_argument
      if [tls] is [true] but [certfile] and [keyfile] is missing. *)
