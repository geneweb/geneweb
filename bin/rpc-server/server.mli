type handler =
  Unix.sockaddr ->
  string ->
  Geneweb_rpc.Json_rpc.Request.t ->
  Geneweb_rpc.Json_rpc.Response.t Lwt.t
(** Type of user handler. This first argument is client socket address and
    the second argument is the request object. *)

val start :
  interface:string ->
  port:int ->
  ?max_connection:int ->
  ?idle:float ->
  ?tls:bool ->
  ?certfile:string ->
  ?keyfile:string ->
  handler ->
  unit
(** [start ~interface ~port hdl] initializes an asynchronous RPC server based
    on a HTTP/1.1 server with WebSocket capability.

    The server only accepts HTTP requests that upgrade to the WebSocket
    protocol. All other requests are rejected. The content of the WebSocket
    frame are expected to be Request object of the JSON-RPC protocol.

    If the option [max_connection] is given, the number of connection per
    client is limited. Otherwise, there is no limit.

    If the option [idle] is given, idle connections are closed.

    If the option [tls] is true and both certificate files and private key
    are provided, the server only accepts connection through TLS.

    The server listens on the specified [interface] and [port].

    @raise Invalid_argument if [tls] is [true] but [certfile] and [keyfile]
                            is missing. *)
