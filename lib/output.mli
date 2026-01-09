(** This module uses `conf.output_conf` in order to print an answer to te
    request. e.g. send HTML page to the client.

    By default:
    - GeneWeb's web server set HTTP status to OK if a printing function is used
      without a previous call to [status].
    - The output channel is a socket given by [gwd] *)

val status : Config.config -> Def.httpStatus -> unit
(** [status conf answer] print HTTP status line to the output channel where
    [answer] is a HTTP status. *)

val header : Config.config -> ('a, unit, string, unit) format4 -> 'a
(** Formatter printing of the HTTP header (header line) to the output channel.
*)

val print_string :
  Config.config -> [< `encoded | `escaped | `safe ] Adef.astring -> unit
(** Printing the part of HTTP response body on the output channel. *)

val print_sstring : Config.config -> string -> unit
(** Printing the part of HTTP response body on the output channel. Use it to
    print strings considered safe. *)

val printf : Config.config -> ('a, unit, string, unit) format4 -> 'a
(** Formatter printing of the part of HTTP response body on the output channel.
*)

val flush : Config.config -> unit
(** Making sure that previously printed content of the buffer is sent on the
    output channel. MUST be called only once at the end of the page because
    plugins can define a custom output configuration which needs the whole
    document or close the channel on [flush]. *)
