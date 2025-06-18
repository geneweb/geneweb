open Config

val portrait_folder : config -> string
val carrousel_folder : config -> string
val ext_list_1 : string array
val ext_list_2 : string array
val find_file_without_ext : string -> string
val is_url : string -> bool

val src_of_string :
  config ->
  string ->
  [ `Src_with_size_info of string | `Path of string | `Url of string | `Empty ]

val scale_to_fit : max_w:int -> max_h:int -> w:int -> h:int -> int * int
(** [scale_to_fit ~max_w ~max_h ~w ~h] is the [(width, height)] of a
    proportionally scaled [(w, h)] rectangle so it can fit in a [(max_w, max_h)]
    rectangle *)

val parse_src_with_size_info :
  config ->
  [< `Src_with_size_info of string ] ->
  ([> `Path of string | `Url of string ] * (int * int), string) result

(* TODO this should be removed *)
val default_image_filename :
  string -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string
(** [default_image_filename mode base p] is the default filename of [p]'s
    portrait or blason. Without it's file extension. e.g: default_image_filename
    "Jean Claude" "DUPOND" 3 is "jean_claude.3.dupond" or
    "jean_claude.3.dupond.blason" *)

val size_from_path : string -> (int * int, unit) result
(** [size_from_path path]
    - Error () if failed to read or parse file
    - Ok (width, height) of the file. It works by opening the file and reading
      magic numbers *)

val path_of_filename : config -> string -> string
(** [path_of_filename fname] search for image {i images/fname} inside the base
    and assets directories. Return the path to found file or [fname] if file
    isn't found. *)

val rename_portrait_and_blason :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string * string * int ->
  unit
(** Rename portrait to match updated name *)

val src_to_string : [< `Path of string | `Url of string ] -> string
(** [src_to_string src] is [src] as a string *)

(* TODO this should be removed *)
val get_portrait_path :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  [> `Path of string | `Url of string ] option
(** [get_portrait_path conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some path] with [path] the full path with extension of [p]'s portrait. *)

val get_portrait_with_size :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  ([> `Path of string | `Url of string ] * (int * int) option) option
(** [get_portrait_with_size conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some (src, size_opt)] with [src] the url or path of [p]'s portrait.
      [size_opt] is the (width,height) of the portrait if we could recover them
*)

val get_blason_with_size :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  ([> `Path of string | `Url of string ] * (int * int) option) option
(** [get_blason_with_size conf base p] is
    - [None] if we don't have access to [p]'s family portrait or it doesn't
      exist.
    - [Some (src, size_opt)] with [src] the url or path of [p]'s family
      portrait. [size_opt] is the (width,height) of the family portrait if we
      could recover them *)

val has_blason :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool -> bool
(** [has_blason conf base p self] is
    - [true] if [p] has coat of arms. If Self is true it checks only for its own
      coat of arms
    - [false] if [p] has not *)

val has_blason_stop :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** [has_blason_stop conf base p] is
    - [true] if [p] stops to use coat of arms of his ancestors
    - [false] if [p] did not stop or has no coat of arms *)

val get_portrait :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  [> `Path of string | `Url of string ] option
(** [get_portrait conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some src] with [src] the url or path of [p]'s portrait. *)

val get_old_portrait :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  [> `Path of string | `Url of string ] option

val get_blason_aux :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  bool ->
  [> `Path of string | `Url of string ] option
(** [get_blason conf base p self saved] is
    - [None] if we don't have access to [p]'s blason or it doesn't exist.
    - [Some src] with [src] the url or path of [p]'s blason. *)

val get_blason :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  [> `Path of string | `Url of string ] option

val get_old_blason :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  [> `Path of string | `Url of string ] option

val get_portrait_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string

val get_old_portrait_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string

val get_blason_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string

val get_old_blason_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string

val get_old_portrait_or_blason :
  config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.person ->
  [> `Path of string | `Url of string ] option
(** [get_portrait conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some src] with [src] the url or path of [p]'s portrait. *)

val get_blason_owner :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.iper option
(** [get_blason_owner conf base p] is
    - [None] if we do not find blason owner.
    - [Some fa_iper] with [fa_iper] the owner [p]'s blason. *)

(* -- Carrousel -- *)

val get_carrousel_imgs :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (string * string * string * string) list

val get_carrousel_old_imgs :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (string * string * string * string) list

val is_not_private_img : config -> string -> bool
(** determines if image is private (pathname contains "private/") *)
