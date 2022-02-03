(** By default:
    - GeneWeb's web server set HTTP status to OK if a printing function is used
    without a previous call to [status].
    - The output channel is a socket given by [gwd]
*)

(** [status conf answer] print HTTP status line to the output channel
    where [answer] is a HTTP status. *)
val status : Config.config -> Def.httpStatus -> unit

(** Formatter printing of the HTTP header (header line) to the output channel. *)
val header : Config.config -> ('a, unit, string, unit) format4 -> 'a

(** Printing the part of HTTP response body on the output channel. *)
val print_string : Config.config -> string -> unit

(** Formatter printing of the part of HTTP response body on the output channel. *)
val printf : Config.config -> ('a, unit, string, unit) format4 -> 'a

(** Making sure that previously printed content of the buffer is sent on the output channel.
    MUST be called only once at the end of the page because plugins can define a custom
    output configuration which needs the whole document or close the channel on [flush].
 *)
val flush : Config.config -> unit
