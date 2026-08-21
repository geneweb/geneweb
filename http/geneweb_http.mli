(* Copyright (c) 1998-2007 INRIA *)

module Code : sig
  type status =
    | OK (* 200 *)
    | Moved_Temporarily (* 302 *)
    | Bad_Request (* 400 *)
    | Unauthorized (* 401 *)
    | Forbidden (* 403 *)
    | Not_Found (* 404 *)
    | Service_Unavailable (* 503 *)

  val status_code : status -> int
  val pp : status Fmt.t
  val to_string : status -> string
end

module Header : sig
  val extract_param : string -> char -> string list -> string
  (** [extract_param name stopc request] can be used to extract some parameter
      from a browser [request] (list of strings); [name] is a string which
      should match the beginning of a request line, [stopc] is a character
      ending the request line. For example, the string request has been obtained
      by: [extract_param "GET /" ' ']. Answers the empty string if the parameter
      is not found. *)
end

module Connection : sig
  type t

  val of_out_channel : cgi:bool -> Out_channel.t -> t

  val close : t -> unit
  (** Closes the current socket *)

  val woc : t -> out_channel
  (** Return the out_channel associated to the socket *)

  val wsocket : t -> Unix.file_descr
  (** Returns the last used socket *)

  val pp_sockaddr : Format.formatter -> Unix.sockaddr -> unit
  (** Formats an [ADDR_INET] as [ip:port], bracketing the address when it is
      IPv6. Must not be called with [ADDR_UNIX]. *)

  val is_lan_candidate : Unix.sockaddr -> bool
  (** [true] for a non-loopback, non-wildcard IPv4 [ADDR_INET]; [false] for
      IPv6, [ADDR_UNIX], the loopback, and the wildcard address. *)

  val wflush : t -> unit
  (** Flushes the content of the current socket *)
  (* To flush page contents print. *)

  val header : t -> string -> unit
  (** Prints a header; cannot be called if part of content part already has been
      sent *)

  val http : t -> Code.status -> unit
  (** [Output.status conf answer] sends the http header where [answer]
      represents the answer status. *)

  val http_redirect_temporarily : t -> string -> unit
  (** [Output.status conf_redirect url] sends the http header where [url]
      represents the Location where the request needs to be redirected. *)

  val printf : t -> ('a, out_channel, unit) format -> 'a
  (** Formatter printing in the out channel associated to the connected socket
  *)
  (* To be called to print page contents. *)

  val print_string : t -> string -> unit
  (** Prints a string in the out channel associated to the socket *)
  (* To be called to print page contents. *)
end

module Server : sig
  type handler =
    Connection.t -> Unix.sockaddr * string list -> string -> string -> unit

  val timestamp_tag : unit Logs.Tag.def

  val start :
    ?addr:string ->
    port:int ->
    ?timeout:int ->
    max_pending_requests:int ->
    n_workers:int ->
    handler ->
    unit
  (** [Wserver.start ~secret_salt ?addr ~port ?timeout ~n_workers callback]
      starts a HTTP 1.1 server that listens on the address [addr] and port
      [port].

      On Unix, worker jobs managed by [n_workers] workers have a time limit of
      [timeout]. If [timeout] is [0], there is no limit. This is the default.

      The [max_pending_requests] argument specified the maximum number of
      pending requests that the server can store. If the queue is full, new
      requests are ignored until space becomes available.

      When a client connects, [callback] is invoked with the arguments
      [(addr, request) path query] where:
      - [addr] is the client address,
      - [request] is the client request,
      - [path] is the path of the request,
      - [query] is the query content.

      Listening on ports < 1024 may require root privileges. *)

  (* To print an http header line *)

  val get_request_and_content : char Stream.t -> string list * string
  (** Returns the request from a stream read from a socket. *)

  val sock_in : string ref
  (** Names of the files used in windows implementation to communicate http
      requests and html answers. Default "wserver.sin" and "wserver.sou". Can
      have relative or absolute paths. *)

  val sock_out : string ref

  val stop_server : string ref
  (** Name of the file whose presence tells the server to stop (at least one
      request is necessary to unfreeze the server to make it check that this
      file exits. Default "STOP_SERVER". Can have relative or absolute path. *)

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
end
