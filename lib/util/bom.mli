(** BOM (Byte Order Mark) detection for file imports. *)

type t = Utf8 | Utf16_le | Utf16_be | Utf32_le | Utf32_be | None

val check : in_channel -> t
(** Detect BOM at current position. UTF-8 BOM is consumed, others rewind. *)

val to_string : t -> string

val is_unsupported : t -> bool
(** True for UTF-16 and UTF-32 variants. *)
