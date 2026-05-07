open Config

val print_image_file : config -> string -> (unit, string) result
(** [print_image_file conf fname] send HTTP respose with content of an image
    file at the path [fname]. MIME type of an image is deducted from [fname]
    extension. Returns [false] if image wasn't found or couldn't be send. *)

val print_source : config -> string -> unit
(** Display an image of given filename in images folder Filename may contain
    sub-folders, but cannot point outside images *)

val print : config -> Geneweb_db.Driver.base -> unit
(** Searches image's filename in the environement [conf.env] and sends HTTP
    response with its content on the socket. If filename isn't presented, looks
    up personal image (portrait) for person mentionned in [conf.env] *)

val print_blason : config -> Geneweb_db.Driver.base -> unit
(** Searches blason filename in the environement [conf.env] and sends HTTP
    response with its content on the socket. If filename isn't presented, looks
    up family image (blason) for person mentionned in [conf.env] *)

val print_html : config -> unit
(** Sends HTTP response with HTML page containg just image specified in
    arguments. *)

val print_placeholder_gendered_portrait :
  config -> Geneweb_db.Driver.person -> int -> unit
(** prints html `<img>` tag of the default gendered portrait with square size
    [size] *)

val print_folder_images_json : config -> string option -> unit
(** [print_folder_images_json conf folder] sends a JSON array of image paths
    found in [albums_d/folder], as `albums/folder/file.jpg`… Only regular files
    whose extension is in [Image.ext_list_1] are included, sorted
    alphabetically. If [folder] is [None] or contains unsafe path components, an
    empty array is returned. Response [Content-type: application/json]. *)

val print_album_image : config -> unit
(** [print_album_image conf] serves the album image whose path is given by the
    [s] environment parameter, resolved under [!GWPARAM.albums_d]. The path may
    include subdirectories (e.g. `vacances/foo.jpg`). Paths containing “..” or
    “\\” components are rejected. Access is granted to wizards, friends, or for
    non-private images. *)
