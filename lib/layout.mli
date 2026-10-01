type dir = string
type file = string
type t
type mode = Reorg | Legacy | Detect

val reorg_layout : string -> t
val legacy_layout : string -> t
val of_bname : mode:mode -> string -> t
val gwf : t -> string
val cnt : t -> string
val portraits : t -> string
val src : t -> string
val etc : t -> string
val config : t -> string
val lang : t -> string
val images : t -> string
val albums : t -> string
