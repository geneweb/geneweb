val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] handles [m=CHK_DIGEST], asked by an open person or family
    form (js/updateConflict.js) to learn early that its record was saved
    meanwhile by another form. The request names the person by index [i], or the
    family by index [i] with [t=fam], and carries the [digest] of the form. It
    answers [{"changed"}] in JSON: [true] when the digest of the record as
    stored differs from [digest] ({!Update.digest_person},
    {!Update.digest_family}), or when the record cannot be found. *)
