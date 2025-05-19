(* Copyright (c) 1998-2007 INRIA *)

(* module [Wserver]: elementary web service *)

val f :
  syslog:
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
  addr:string option ->
  port:int ->
  timeout:int ->
  max_clients:int option ->
  handler:(Unix.sockaddr * string list -> string -> Adef.encoded_string -> unit) ->
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
(** [http status] sends the http header where [status]
    represents the answer status. *)

val http_redirect_temporarily : string -> unit
(** [http_redirect_temporarily url] sends the http header where [url]
    represents the Location where the request needs to be redirected. *)

val http_redirect_permanently : string -> unit
(** [http_redirect_permanently url] sends the http header where [url]
    represents the Location where the request needs to be redirected. *)

val stop_server : string ref
(** Name of the file whose presence tells the server to stop (at least
    one request is necessary to unfreeze the server to make it check
    that this file exits. Default "STOP_SERVER". Can have relative
    or absolute path. *)

val request_timeout : unit -> unit
val set_on_timeout : (int -> unit) -> unit
val set_timeout : int -> unit

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
