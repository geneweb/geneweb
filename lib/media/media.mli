(** Requests of wizards on the media files of a base, answering in JSON.

    The field [dest] of a request gives the place of the files:
    - [portrait], [blason]: the portrait or the coat of arms of the person of
      the request ([i] or [p]/[n]/[oc]), a single file replaced as a whole, the
      previous one saved in [saved/];
    - [person]: the directory of the images of that person, without
      subdirectory;
    - [images]: the images directory of the base, without subdirectory;
    - [albums]: the albums directory of the base, the album being the field
      [subdir] (levels separated by [/], checked by [Image.safe_subpath]).

    A file replaced or deleted moves to the {!MediaDir.saved} directory of its
    place: [saved/] in the directory of a person (with its note and source) and
    in the images directory, and in [saved/] at the root of the albums under the
    same path ([albums/saved/a/b/] for [albums/a/b/]).

    The bvar [can_send_image=no] forbids every request ([403]). The errors
    answer [{"error": translated message}]: [400] invalid request, name or
    subdirectory, [403] forbidden, [409] name taken (with ["conflict": true]),
    [413] file too big, [415] type not accepted for the place, [500] file not
    written. *)

val print_send : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_send conf base] stores the file sent in the field [file] of a
    [multipart/form-data] request ([m=MEDIA_SEND_OK]) and answers
    [{"name": file name, "dest": dest, "url": URL of the file}].

    The type of the file is read from its first bytes, and from the extension of
    its client name for DOCX, ODT and TXT. It gives the extension of the stored
    file: JPEG, PNG and GIF are accepted everywhere, PDF, DOCX and ODT in
    [person] and [images], TXT in [images]. The field [name] is the name of the
    file without extension, checked by [Image.safe_name]; it is not used for
    [portrait] and [blason]. A file of [albums] goes in an album: [subdir] is
    required. A file is limited to the bvar [max_images_size] (20 000 000 bytes
    by default) for an image and [max_documents_size] (100 000 000 bytes by
    default) for a document.

    A name is taken when a file of the same name exists or, in the directory of
    a person, an entry of the same name whatever its extension. The request is
    then refused unless the field [replace] is [on].

    The history of the base records the files of a person ([si], [sb], [sf]);
    the others are logged. *)

val print_delete : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_delete conf base] moves the file [name] of [images] or of [albums]
    ([subdir], empty for a file at the root of the albums) to its saved
    directory ([m=MEDIA_DEL_OK]), logs it and answers [{"name": name}]. An album
    [name] without any entry is removed. The files of a person are deleted by
    the carrousel ([m=DEL_IMAGE_C_OK]). *)

val print_mkdir : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_mkdir conf base] creates the album [subdir] and its parents
    ([m=MEDIA_MKDIR_OK]), logs it and answers [{"subdir": subdir}]. The root of
    the albums is refused. *)

val print_list : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_list conf base] answers [m=MEDIA_LIST] with the files and the
    subdirectories of the place given by [dest] and, as for [print_send], the
    person or [subdir] (empty for the root of the albums):
    [{"files": [{"name", "size" (bytes)}], "dirs": [{"name", "empty"}]}], both
    sorted by name, as listed by {!MediaDir.read}. In the directory of a person,
    the notes and sources of the images ([.txt], [.src]) are left out. [dirs] is
    empty but for [albums], where [empty] tells an album without any entry. *)
