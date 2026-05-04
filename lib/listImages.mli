val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] handles [m=LIST_IMAGES]. Required URL parameter [src]
    selects the source root:
    - [src=key]: lists images under {!GWPARAM.images_d}/[<key>] where [<key>] is
      built from URL parameters [p], [n] and [oc] (default [oc=0]). Multi-key
      mode supported via numeric suffixes: [p1]/[n1]/[oc1], [p2]/[n2]/[oc2],
      etc. — collection stops at the first missing pair. With an extra
      [dir=<subdir>] parameter, lists images of that subdirectory only.
    - [src=albums]: lists albums under {!GWPARAM.albums_d}. Without [dir],
      returns the list of album subdirectory names. With [dir=<album>], lists
      images of that album.

    Output is a JSON array, one entry per requested key (for [src=key] batch
    mode) or a single entry (other modes):

    {[
      [
        { "src": "key",
          "key": "henri.0.gouraud",
          "files": ["scan.jpg"],
          "subdirs": [ { "name": "lettres",
                         "files": ["1914.jpg"] } ] },
        ...
      ]
    ]}

    For [src=key] without [dir]: [files] are root-level images and [subdirs] is
    one level deep (with their own files). For [src=key] with [dir]: [files] are
    the images of that subdir, [subdirs] is omitted. For [src=albums] without
    [dir]: [files] is empty, [subdirs] lists albums (without their files for
    this top-level case). For [src=albums] with [dir]: [files] contains the
    album's images, [subdirs] is empty.

    Files are filtered by {!Image.ext_list_1} (case-insensitive). Hidden entries
    (leading dot or tilde) are skipped. Path-traversal attempts (segments
    containing slashes, backslashes or [..]) are rejected.

    Requires {!Config.config.wizard} access. *)

val list_images : string -> string list
(** Internal helper exposed for testing. Returns image filenames in [dir],
    filtered by {!Image.ext_list_1} (case-insensitive), sorted. *)

val list_subdirs : string -> string list
(** Internal helper exposed for testing. Returns subdirectory names in [dir],
    excluding hidden entries, sorted. *)

val image_extension : string -> bool
(** Internal helper exposed for testing. Case-insensitive match against
    {!Image.ext_list_1}. *)
