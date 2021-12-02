
(** Return the number of bytes composing the UTF8 character starting with [c] *)
val nbc : char -> int

(** [Utf8.next s i] returns the index of the character comming after
    the one which starts at [i]. *)
val next : string -> int -> int

(** [Utf8.get s n] returns the index where the [n]-th character
    starts in string [s]. *)
val get : string -> int -> int

(** Return the length (number of characters, not bytes)
    of the given string. *)
val length : string -> int

(** [sub ?pad s start len]
    Return a fresh UTF8-friendly substring of [len] characters, padded if needed.
    Be careful [start] is the index of the byte where to start in [s],
    not the [start-th] UTF8-character. *)
val sub : ?pad:char -> string -> int -> int -> string

(** [cmap_utf_8 cmap s] returns the UTF-8 encoded string
    resulting from applying the character map [cmap] to every character
    of the UTF-8 encoded string [s]. *)
val cmap_utf_8 :
  (Uchar.t -> [< `Self | `Uchars of Uchar.t list ]) -> string -> string

(** Returns UTF-8 encoded string with all uppercase letters translated to lowercase *)
val lowercase : string -> string

(** Returns UTF-8 encoded string with all lowercase letters translated to uppercase *)
val uppercase : string -> string

(** Returns UTF-8 encoded string where the first letter is capitalised *)
val capitalize_fst : string -> string

(** Returns UTF-8 encoded string where the first letter is capitalised and others minimalised *)
val capitalize : string -> string

(** [compare a b] compare normalized version of [a] and [b]
    It is case insensitive.
    It starts with unaccented comparison of [a] and [b],
    and refine the result with accents comparison.

    Here is an exemple of how letters would be sorted:
    [A À Á Â B C Ç Č D E É L Ł Ô Ö Ø Œ P Q R * . ?]
 *)
val compare : string -> string -> int