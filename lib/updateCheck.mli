val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] handles [m=CHK_DIGEST], asked by an open person, family or
    note form (js/updateConflict.js) to learn early that its record was saved
    meanwhile by another form. The request names the person by index [i], the
    family by index [i] with [t=fam], or the note by file name [f] (none for the
    notes of the base) with [t=note], and carries the [digest] of the form. It
    answers [{"changed"}] in JSON: [true] when the digest of the record as
    stored differs from [digest] ({!Update.digest_person},
    {!Update.digest_family}, {!Mutil.digest} of the text of the note), or when
    the record cannot be found. *)
