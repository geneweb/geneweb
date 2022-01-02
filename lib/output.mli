
(** [status conf answer] print HTTP status line to the socket where [answer] is a HTTP status. *)
val status : Config.config -> Def.httpStatus -> unit

(** Formatter printing of the HTTP header (header line) to the socket. If used without seting HTTP status with [status],
    it is set OK. *)
val header : Config.config -> ('a, unit, string, unit) format4 -> 'a

(** Printing the part of HTTP response body on the socket. If used without seting HTTP status with [status], it is set OK. *) 
val print_string : Config.config -> string -> unit

(** Formatter printing of the part of HTTP response body on the socket. If used without seting HTTP status with [status],
    it is set OK. *)
val printf : Config.config -> ('a, unit, string, unit) format4 -> 'a

(** Flushes (send) printed previously content of the buffer on the socket. *)
val flush : Config.config -> unit