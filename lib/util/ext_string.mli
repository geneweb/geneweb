val hexa_string : string -> string

val only_printable : string -> string
(** Trims and remplaces all non-printable characters by spaces in the given
    string. *)

val only_printable_or_nl : string -> string
(** Same as [only_printable] but also accepts '\n'. *)

val nb_char_occ : char -> string -> int
(** [nb_char_occ c s] return the number of times [c] appears in [s]. *)

val split_on_char : char -> string -> string list
(** [split_on_char separator str] Same output as
    [String.split_on_char separator s |> List.map String.trim |> List.filter
     ((<>) "")] *)

val strip_all_trailing_spaces : string -> string
(** Remove all trailing spaces in string *)

module Set : Set.S with type elt = string
(** Set of strings *)

module Map : Map.S with type key = string

val tr : char -> char -> string -> string
(** [tr c1 c2 str] Return a new string which is the same as [str] with all
    occurences of [c1] replaced by [c2]. If [str] does not contain [c1] [str] is
    returned untouched. *)

val unsafe_tr : char -> char -> string -> string
(** [unsafe_tr c1 c2 str] Update [str] in place. Replace all occurences of [c1]
    by [c2]. *)

val start_with : string -> int -> string -> bool
(** [start_with prefix off str] Test if [str] starts with [prefix] (at offset
    [off]).

    Raise [Invalid_argument] if [off] is not a valid index in [str]. *)

val contains : string -> string -> bool
(** [contains str sub] Test [sub] is contained in [str]. *)

val digest : string -> string
(** [digest s] Returns the (128 bits long, using MD5 algorithm) digest of [s].
*)

val trim_trailing_spaces : string -> string
(** Trim at the end of string *)

val end_with : string -> string -> bool
(** [end_with s x] returns [true] iff [s] is ending with [x]. *)
