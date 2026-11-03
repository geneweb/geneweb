type 'a loc = { content : 'a; offset : int; len : int }
(** Location of a token in a string. *)

val tokenize : string -> string loc list
(** [tokenize s] transforms [s] into a list of tokens with locations. *)

val normalize : string -> string
(** [normalize s] produces a normal form of string: letters are lowercased and
    non-ASCII characters are transliterated to ASCII (e.g. ["Hélène"] becomes
    ["helene"]). *)

val preprocess : string -> string loc list
(** [preprocess s] transforms [s] into a list of normalized tokens. Locations
    refer to [normalize s], not to [s]. *)
