val print_upload : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_upload conf base] stores the file sent in the field [file] of a
    [multipart/form-data] request ([m=UPLOAD_OK]) and answers in JSON.

    The field [dest] gives the destination:
    - [portrait], [blason]: the portrait or the coat of arms of the person of
      the request ([i] or [p]/[n]/[oc]), which replaces the previous one, saved
      in [saved/];
    - [person]: the directory of the images of that person;
    - [images], [albums]: the images or the albums directory of the base, or its
      subdirectory [subdir] (levels separated by [/]), created if needed.

    The type of the file is read from its first bytes, and from the extension of
    its client name for DOCX, ODT and TXT. It gives the extension of the stored
    file: JPEG, PNG and GIF are accepted everywhere, PDF, DOCX and ODT in
    [person] and [images], TXT in [images]. The field [name] is the name of the
    file without extension, checked by [Image.safe_name]; it is not used for
    [portrait] and [blason]. A file is limited to the bvar [max_images_size] (20
    000 000 bytes by default) for an image and [max_documents_size] (100 000 000
    bytes by default) for a document.

    A name is taken when a file of the same name exists or, in the directory of
    a person, an entry of the same name whatever its extension. The request is
    then refused unless the field [replace] is [on]: the existing file moves to
    [saved/] with its note and source.

    The history of the base records the files of a person ([si], [sb], [sf]);
    the others are logged.

    Answers:
    - [200] [{"name": file name, "dest": dest, "url": URL of the file}]
    - [400] invalid request, name or subdirectory, [403] sending forbidden by
      the bvar [can_send_image=no], [409] name taken (with ["conflict": true]),
      [413] file too big, [415] type not accepted for the destination, [500]
      file not written: [{"error": translated message}] *)

val print_list : Geneweb.Config.config -> Geneweb_db.Driver.base -> unit
(** [print_list conf base] answers [m=UPLOAD_LIST] with the JSON
    [{"files": [...], "dirs": [...]}] of the destination given by the fields
    [dest] and, as for [print_upload], the person or [subdir].

    [files] are the names in that directory of the files an upload may collide
    with: hidden files, subdirectories and, for a person, the notes and sources
    of the images are left out. [dirs] are, for [images] and [albums], the paths
    of all the subdirectories of the destination, levels separated by [/],
    without the [saved] ones nor, for [images], the directories of the persons.
*)
