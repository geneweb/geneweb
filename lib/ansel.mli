
(** Convert ISO-8859-1 encoded string to ANSEL encoding used inside gedcom files *)
val of_iso_8859_1 : string -> string

(** Convert ANSEL used inside gedcom files to ISO-8859-1 encoding *)
val to_iso_8859_1 : string -> string