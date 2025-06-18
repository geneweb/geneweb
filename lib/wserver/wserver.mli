(* Copyright (c) 1998-2007 INRIA *)

(* module [Wserver]: elementary web service *)

type handler =
  Unix.sockaddr * string list -> string -> Adef.encoded_string -> unit

val start :
  ?addr:string ->
  port:int ->
  ?timeout:int ->
  max_pending_requests:int ->
  n_workers:int ->
  handler ->
  unit
(** [Wserver.start ~secret_salt ?addr ~port ?timeout ~n_workers callback] starts
    a HTTP 1.1 server that listens on the address [addr] and port [port].

    On Unix, worker jobs managed by [n_workers] workers have a time limit of
    [timeout]. If [timeout] is [0], there is no limit. This is the default.

    The [max_pending_requests] argument specified the maximum number of pending
    requests that the server can store. If the queue is full, new requests are
    ignored until space becomes available.

    When a client connects, [callback] is invoked with the arguments
    [(addr, request) path query] where:
    - [addr] is the client address,
    - [request] is the client request,
    - [path] is the path of the request,
    - [query] is the query content.

    Listening on ports < 1024 may require root privileges. *)

val close_connection : unit -> unit
(** Closes the current socket *)

val printf : ('a, out_channel, unit) format -> 'a
(** Formatter printing in the out channel associated to the connected socket *)
(* To be called to print page contents. *)

val print_string : string -> unit
(** Prints a string in the out channel associated to the socket *)
(* To be called to print page contents. *)

val header : string -> unit
(** Prints a header; cannot be called if part of content part already has been
    sent *)
(* To print an http header line *)

val wflush : unit -> unit
(** Flushes the content of the current socket *)
(* To flush page contents print. *)

val http : Def.httpStatus -> unit
(** [Output.status conf answer] sends the http header where [answer] represents
    the answer status. *)

val http_redirect_temporarily : string -> unit
(** [Output.status conf_redirect url] sends the http header where [url]
    represents the Location where the request needs to be redirected. *)

val get_request_and_content : char Stream.t -> string list * Adef.encoded_string
(** Returns the request from a stream read from a socket. *)

val wsocket : unit -> Unix.file_descr
(** Returns the last used socket *)

val woc : unit -> out_channel
(** Return the out_channel associated to the socket *)

val sock_in : string ref
(** Names of the files used in windows implementation to communicate http
    requests and html answers. Default "wserver.sin" and "wserver.sou". Can have
    relative or absolute paths. *)

val sock_out : string ref

val stop_server : string ref
(** Name of the file whose presence tells the server to stop (at least one
    request is necessary to unfreeze the server to make it check that this file
    exits. Default "STOP_SERVER". Can have relative or absolute path. *)

val cgi : bool ref
(** CGI (Common Gateway Interface) mode (default false). *)

(* Example:

    - Source program "foo.ml":
         Wserver.f
           (fun _ -> prerr_endline)
           None 2371 60 None
           (fun _ s _ ->
              Output.status conf Wserver.OK;
              Output.print_sstring conf "You said: %s...\n" s);;
    - Compilation:
         ocamlc -custom unix.cma -cclib -lunix wserver.cmo foo.ml
    - Run:
         ./a.out
    - Launch a Web browser and open the location:
         http://localhost:2368/hello   (but see the remark below)
    - You should see a new page displaying the text:
         You said: hello...

   Possible problem: if the browser says that it cannot connect to
       "localhost:2368",
   try:
       "localhost.domain:2368" (the domain where your machine is)
       "127.0.0.1:2368"
       "machine:2368"          (your machine name)
       "machine.domain:2368"   (your machine name)
       "addr:2368"             (your machine internet address)
*)
