type t
(** Type of locations. *)

type source = [ `File of string | `In_channel of in_channel | `Raw of string ]
(** Type of sources. *)

val equal_source : source -> source -> bool
(** [equal_source s1 s2] checks if the sources [s1] and [s2] are equal. *)

val pp_source : source Fmt.t
(** [pp_source ppf s] prints a string representation of the source on the
    formatter [ppf]. *)

val dummy : t
(** Dummy location. *)

val of_lexbuf : Lexing.lexbuf -> t
(** [of_lexbuf lexbuf] creates a location from the current state of the lexing
    buffer [lexbuf]. *)

val equal : t -> t -> bool
(** [equal t1 t2] checks if the locations [t1] and [t2] are equal. *)

val pp : t Fmt.t
(** [pp ppf t] prints a string representation of the location on the formatter
    [ppf]. *)

val pp_with_input : t Fmt.t
(** [pp_with_input ppf t] prints the lines referenced by [t] on the formatter
    [ppf] using Pp_loc library. For channel sources, this assumes that
    unrestricted seeking operations on channels. *)
