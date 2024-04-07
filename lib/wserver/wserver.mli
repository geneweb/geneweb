(* Copyright (c) 1998-2007 INRIA *)

(* module [Wserver]: elementary web service *)

val f :
  ([ `LOG_EMERG
   | `LOG_ALERT
   | `LOG_CRIT
   | `LOG_ERR
   | `LOG_WARNING
   | `LOG_NOTICE
   | `LOG_INFO
   | `LOG_DEBUG ] ->
  string ->
  unit) ->
  string option ->
  int ->
  int ->
  int option ->
  (Unix.sockaddr * string list -> string -> Adef.encoded_string -> unit) ->
  unit
(** [ Wserver.f syslog addr port tmout maxc g ]
    Starts an elementary httpd server at port [port] in the current
    machine. The variable [addr] is [Some the-address-to-use] or
    [None] for any of the available addresses of the present machine.
    The port number is any number greater than 1024 (to create a
    client < 1024, you must be root). At each connection, the function
    [g] is called: [g (addr, request) path query] where [addr] is the
    client identification socket, [request] the browser request, [path]
    the part of the [request] before the query part and [query] the query content.
    The function [g] has [tmout] seconds to answer some
    text on standard output. If [maxc] is [Some n], maximum [n]
    clients can be treated at the same time; [None] means no limit.
    [syslog] is the function used to log errors or debug info. It is
    called syslog because it is used with the same gravity levels, but
    it can be anything.

    See the example below.
*)

val close_connection : unit -> unit
(** Closes the current socket *)

val printf : ('a, out_channel, unit) format -> 'a
(** Formatter printing in the out channel associated to the connected socket *)
(* To be called to print page contents. *)

val print_string : string -> unit
(** Prints a string in the out channel associated to the socket *)
(* To be called to print page contents. *)

val header : string -> unit
(** Prints a header; cannot be called if part of content part already has been sent *)
(* To print an http header line *)

val wflush : unit -> unit
(** Flushes the content of the current socket *)
(* To flush page contents print. *)

val http : Def.httpStatus -> unit
(** [Output.status conf answer] sends the http header where [answer]
    represents the answer status. *)

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
(** Names of the files used in windows implementation to communicate
    http requests and html answers. Default "wserver.sin" and
    "wserver.sou". Can have relative or absolute paths. *)

val sock_out : string ref

val stop_server : string ref
(** Name of the file whose presence tells the server to stop (at least
    one request is necessary to unfreeze the server to make it check
    that this file exits. Default "STOP_SERVER". Can have relative
    or absolute path. *)

val cgi : bool ref
(** CGI (Common Gateway Interface) mode (default false). *)

val no_fork : bool ref
(** Do not fork processes at every request (default: false) *)

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
