open Config

val print_image_file : config -> string -> (unit, string) result
(** [print_image_file conf fname] send HTTP respose with content of an image file at the path [fname].
    MIME type of an image is deducted from [fname] extension. Returns [false] if image
    wasn't found or couldn't be send. *)

val print_source : config -> string -> unit

val print : config -> Gwdb.base -> unit
(** Searhes image's filename in the environement [conf.env] and sends HTTP respose with its content on the socket. If filename isn't presented, looks up
    personal image for person's mentionned in [conf.env] *)

val print_html : config -> unit
(** Sends HTTP respose with HTML page containg just image specified in arguments. *)

val print_placeholder_gendered_portrait : config -> Gwdb.person -> int -> unit
(** prints html `<img>` tag of the default gendered portrait with square size [size] *)
