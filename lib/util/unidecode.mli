(** Note: in this module, decoding a character means translating an UTF8
    character to an ASCII representation *)

val nbc : char -> int
(** For a given byte, returns how many bytes compose the UTF8 character. *)

val decode :
  (int -> string -> 'a) ->
  (int -> char -> 'a) ->
  (int -> 'a) ->
  string ->
  int ->
  int ->
  'a
(** [decode fns fnc unsupported s i len]

    [s] is the source string, [len] its length, and [i] the offset of the byte
    where the character you want to decode starts.

    If its representation is a char, e.g. "À" -> 'A', [fnc] will be called with
    that char.

    If its representation is a string, e.g. "Ч" -> "TCH", [fns] will be called
    with that string.

    In case of un supported char, [unsupported] will be called.

    In all of these 3 cases, the first argument is the offset of the next byte
    starting a new character in the original string. *)

val decode_string :
  ?unsupported:(Bytes.t -> int -> int -> int ref -> unit) -> string -> string
(** [decode_string str] Return a version of [str] where all characters has been
    decoded using [decode].

    If [str] already is an ASCII string, [str] itself is returned. *)
