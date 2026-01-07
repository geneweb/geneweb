val nbc : char -> int
(** Return the number of bytes composing the UTF8 character starting with [c] *)

val next : string -> int -> int
(** [Utf8.next s i] returns the index of the character comming after the one
    which starts at [i]. *)

val get : string -> int -> int
(** [Utf8.get s n] returns the index where the [n]-th character starts in string
    [s]. *)

val length : string -> int
(** Return the length (number of characters, not bytes) of the given string. *)

val sub : ?pad:char -> string -> int -> int -> string
(** [sub ?pad s start len] Return a fresh UTF8-friendly substring of [len]
    characters, padded if needed. Be careful [start] is the index of the byte
    where to start in [s], not the [start-th] UTF8-character. *)

val cmap_utf_8 :
  (Uchar.t -> [< `Self | `Uchars of Uchar.t list ]) -> string -> string
(** [cmap_utf_8 cmap s] returns the UTF-8 encoded string resulting from applying
    the character map [cmap] to every character of the UTF-8 encoded string [s].
*)

val lowercase : string -> string
(** Returns UTF-8 encoded string with all uppercase letters translated to
    lowercase *)

val uppercase : string -> string
(** Returns UTF-8 encoded string with all lowercase letters translated to
    uppercase *)

val capitalize_fst : string -> string
(** Returns UTF-8 encoded string where the first letter is capitalised *)

val capitalize : string -> string
(** Returns UTF-8 encoded string where the first letter is capitalised and
    others minimalised *)

val initial : string -> int option
(** Returns position of first capital letter in the string. *)

module C : sig
  (** Utf8 char type. *)
  type t = Str of string | Chr of char | Empty

  val unaccent_next : bool -> string -> int -> int -> t * int * int
  (** [unaccent_next trimmed s i0 len] Returns [(t, start, next)]: next UTF-8
      character in string [s] starting at position [i0]. The diacritic marks are
      removed, character is also case lowered, and any character returning
      [Empty] (unsupported or reported as empty) is ignored: the next character
      in [s] will be picked except if you reach [len]. In that case, [Empty] is
      returned.

      [start] is the byte offset in [s] where the resulting character [t]
      starts. [next] is the offset of the byte after [t]. *)

  val cp : string -> int -> Uchar.t
  (** [cp s i] returns the Unicode code point of the character starting at
      [i]-th byte. *)
end

val compare : string -> string -> int
(** [compare a b] compare normalized version of [a] and [b] It is case
    insensitive. It starts with unaccented comparison of [a] and [b], and refine
    the result with accents comparison.

    Here is an exemple of how letters would be sorted:
    [A À Á Â B C Ç Č D E É L Ł Ô Ö Ø Œ P Q R * . ?] *)

val utf_8_of_iso_8859_1 : string -> string
(** Convert encoded string with ISO 8859-1 to UTF 8 *)

val iso_8859_1_of_utf_8 : string -> string
(** Convert encoded string with UTF 8 to ISO 8859-1 *)

val start_with_wildcard : ?ignore_case:bool -> string -> int -> string -> bool
(** [start_with_wildcard ~ignore_case prefix off str] Test if [str] starts with
    [prefix] (at offset [off]) ignoring the case according to the value of
    [ignore_case] (default [false]). Occurrences of ['_'] in [prefix] will match
    both ['_'] and [' '] in [str] and trailing ['_'] of [prefix] is treated as
    an optional ['_'] [' '].

    Raise [Invalid_argument] if [off] is not a valid index in [str]. *)

val normalize : string -> string
(** [normalize s] Return [s] normalized using
    {{:http://www.unicode.org/glossary/#normalization_form_c}NFC} with all
    malformed UTF-8 character replaced by
    {{:http://unicode.org/glossary/#replacement_character}the replacement
     character} *)

val unaccent_next : bool -> string -> int -> string * int
(** [unaccent_next lower s i] checks UTF-8 characher that starts at position [i]
    inside [s] and returns couple (cs,np) where [cs] is ASCII representation of
    this character (characters between 0x00 and 0x7F) and [np] it's a position
    of next utf8 character inside [s]. If [lower] is true then [cs] will contain
    only lowercase letters. Example : unaccent_next "aÈa" 1 -> ("e",3) *)

val alphabetic_order : string -> string -> int
(** Compare two UTF-8 encoded strings by alphabetic order *)

val filter_map :
  ([ `Uchar of Uchar.t | `Malformed of string ] -> Uchar.t option) ->
  string ->
  string

val unaccent : string -> string
val uchar_to_string : Uchar.t -> string
