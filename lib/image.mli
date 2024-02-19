open Config
open Gwdb

val portrait_folder : config -> string
val carrousel_folder : config -> string
val authorized_image_file_extension : string array

val scale_to_fit : max_w:int -> max_h:int -> w:int -> h:int -> int * int
(** [scale_to_fit ~max_w ~max_h ~w ~h] is the {(width, height)} of a proportionally scaled {(w, h)} rectangle so it can fit in a {(max_w, max_h)} rectangle *)

val source_filename : config -> string -> string
(** Returns path to the image file with the giving name in directory {i src/}. *)

(* TODO this should be removed *)
val default_portrait_filename : base -> person -> string
(** [default_portrait_filename base p] is the default filename of [p]'s portrait. Without it's file extension.
 e.g: default_portrait_filename_of_key "Jean Claude" "DUPOND" 3 is "jean_claude.3.dupond"
 *)

val size_from_path : [ `Path of string ] -> (int * int, unit) result
(** [size_from_path path]
    - Error () if failed to read or parse file
    - Ok (width, height) of the file.
It works by opening the file and reading magic numbers *)

val path_of_filename : string -> [> `Path of string ]
(** [path_of_filename fname] search for image {i images/fname} inside the base and assets directories.
    Return the path to found file or [fname] if file isn't found.  *)

val rename_portrait : config -> base -> person -> string * string * int -> unit
(** Rename portrait to match updated name *)

val src_to_string : [< `Path of string | `Url of string ] -> string
(** [src_to_string src] is [src] as a string *)

(* TODO this should be removed *)
val get_portrait_path : config -> base -> person -> [> `Path of string ] option
(** [get_portrait_path conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some path] with [path] the full path with extension of [p]'s portrait.
*)

val get_portrait_with_size :
  config ->
  base ->
  person ->
  ([> `Path of string | `Url of string ] * (int * int) option) option
(** [get_portrait_with_size conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some (src, size_opt)] with [src] the url or path of [p]'s portrait. [size_opt] is the (width,height) of the portrait if we could recover them *)

val get_portrait :
  config -> base -> person -> [> `Path of string | `Url of string ] option
(** [get_portrait conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some src] with [src] the url or path of [p]'s portrait.
*)

val get_old_portrait :
  config -> base -> person -> [> `Path of string | `Url of string ] option
(** [get_portrait conf base p] is
    - [None] if we don't have access to [p]'s portrait or it doesn't exist.
    - [Some src] with [src] the url or path of [p]'s portrait.
*)

(* -- Carrousel -- *)

val get_carrousel_imgs :
  config -> base -> person -> (string * string * string * string) list

val get_carrousel_old_imgs :
  config -> base -> person -> (string * string * string * string) list

val is_not_private_img : config -> string -> bool
(** determines if image is private (pathname contains "private/") *)
