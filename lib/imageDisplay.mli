(** [print_image_file conf fname] send HTTP respose with content of an image file at the path [fname].
    MIME type of an image is deducted from [fname] extension. Returns [false] if image 
    wasn't found or couldn't be send. *)
val print_image_file : Config.config -> string -> bool 

(** [Description] : Traite une requête image.
    [Args] :
      - config : configuration de la requête
      - base : base de donnée sélectionnée                                  *)
val print : Config.config -> Gwdb.base -> unit

(** [Description] : Affiche une image seule dans une page HTML.
    [Args] :
      - conf : configuration de la requête
      - base : argument non utilisé                                          *)
val print_html : Config.config -> unit
