val default_max_cnt : int
(** Default number of names that could be printed simultaneously on the page *)

(** Type that represents result of name selection *)
type t =
  | Result of (string * string * int) list
      (** Exhaustive result with the list of names
                                                 (key and printable name) with number of
                                                 persons that have giving name*)
  | Specify of string list
      (** Not exhaustive result that specifies all existing names
                                 prefixes (their length depends on initial searched prefix) *)

val select_names :
  at_least:int option ->
  Config.Trimmed.t ->
  Gwdb.base ->
  bool ->
  string ->
  int ->
  t * int
(** [select_names conf base is_surnames ini limit]
    Select up to [limit] first names/surnames starting with [ini].
    If more values are available, return [Specify] with different
    possible prefixes with the length at most equal to the length of [ini]+1
    (for empty [ini] specifies all first letters of existing names).
    Otherwise, return the list of values [Result] with all first names
    and number of persons that have giving name. *)

val ini : int -> string -> string
(** Returns prefix of length [len] of UTF8 encoded name *)

val groupby_ini :
  int -> (string * 'a * 'b) list -> (string * ('a * 'b) list) list
(** [groupby_ini len results] returns alphabeticaly ordered list of grouped by name prefix (with length [len])
    results. *)

val groupby_count : t -> (int * string list) list
(** Returns ordered (from bigest to smallest) list of grouped by name frequency (number of persons
    having the name) results. Shouldn't be used when results are represented with [Specify]. *)
