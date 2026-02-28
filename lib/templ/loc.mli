type t
(** Type of locations. *)

type source = [ `File of string | `Raw of string ]
(** Type of sources. *)

val equal_source : source -> source -> bool
(** [equal_source s1 s2] checks if the sources [s1] and [s2] are equal. *)

val pp_source : source Fmt.t
(** [pp_source ppf s] prints a string representation of the source on the
    formatter [ppf]. *)

val dummy : t
(** Dummy location. *)

val is_dummy : t -> bool
(** [is_dummy t] checks if [t] is the dummy location. *)

val of_offsets : source -> int -> int -> t
(** [of_offsets src start stop] creates a location for the source [src] starting
    at [start] and ending at [stop]. *)

val equal : t -> t -> bool
(** [equal t1 t2] checks if the locations [t1] and [t2] are equal. *)

val pp : t Fmt.t
(** [pp ppf t] prints a string representation of the location on the formatter
    [ppf]. *)

val pp_with_source : t Fmt.t
(** [pp_with_source ppf t] prints the lines referenced by [t] on the formatter
    [ppf] using Pp_loc library. For channel sources, this assumes that
    unrestricted seeking operations on channels. *)
