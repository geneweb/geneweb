(** Default number of names that could be printed simultaneously on the page *)
val default_max_cnt : int

(** Type that represents result of name selection *)
type t = 
    | Result of (string * string * int) list (** Exhaustive result with the list of names 
                                                 (key and printable name) with number of 
                                                 persons that have giving name*)
    | Specify of string list (** Not exhaustive result that specifies all existing names 
                                 prefixes (their length depends on initial searched prefix) *)

(** Returns list of all first name's first letter present in the base (UTF8 encoded). 
    Used for fast access for base's names *)
val first_letters : Gwdb.base -> bool -> string list

(** [select_names conf base is_surnames ini limit]
    Select up to [limit] first names/surnames starting with [ini].
    If more values are available, return [Specify] with different
    possible prefixes with the length at most equal to the length of [ini]+1
    (for empty [ini] specifies all first letters of existing names).
    Otherwise, return the list of values [Result] with all first names 
    and number of persons that have giving name. *)
val select_names :
  Geneweb.Config.config -> Gwdb.base -> bool -> string -> int -> t * int

(** Returns prefix of length [len] of UTF8 encoded name *)
val ini : int -> string -> string

(** [groupby_ini len results] returns alphabeticaly ordered list of grouped by name prefix (with length [len]) 
    results. *)
val groupby_ini :
  int -> (string * 'a * 'b) list -> (string * ('a * 'b) list) list

(** Returns ordered (from bigest to smallest) list of grouped by name frequency (number of persons 
    having the name) results. Shouldn't be used when results are represented with [Specify]. *)
val groupby_count : t -> (int * string list) list