(** Country of a client address, looked up in an IP-to-country CSV file such as
    DB-IP's "IP to Country Lite" ([dbip-country-lite-YYYY-MM.csv],
    uncompressed).

    Each line of the file is [first,last,CC]: the first and last addresses of a
    range and its ISO 3166-1 alpha-2 country code. The lines must be sorted by
    first address, IPv4 ranges before IPv6 ones, as DB-IP provides them. The
    file is searched by dichotomy on disk, without being loaded: a lookup reads
    a few dozen lines. *)

val file : unit -> string
(** [file ()] is [etc/ip_country.csv] in the bases directory, shared by all the
    bases. *)

val unknown : string
(** [unknown] is ["ZZ"], the code of an address that is not an IP address (a
    host name, when gwd resolves the client addresses) or that is in no range of
    the file. *)

val country : in_channel -> string -> string
(** [country ic addr] is the country code of the address [addr], an IPv4 or IPv6
    address in text form, in the file open on [ic]; {!unknown} if there is none.
*)

val countries : string -> string list -> (string * int) list option
(** [countries fname addrs] counts the addresses [addrs] by country, in the file
    [fname]: the country codes with their number of addresses, most frequent
    first, then by code. [None] if the file cannot be opened. *)
