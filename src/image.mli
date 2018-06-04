(** [Description] : Affiche une image (avec ses en-têtes) en réponse HTTP en
                    utilisant Wserver. Le type MIME de l'image est deviné à
                    partir de l'extension contenu dans le nom du fichier.
    [Args] :
      - fname : le nom du fichier image
    [Retour] : True si l'image a pu être affichée                           *)
val print_image_file : string -> bool

(** [Description] : Traite une requête image.
    [Args] :
      - config : configuration de la requête
      - base : base de donnée sélectionnée                                  *)
val print : Config.config -> Gwdb.base -> unit

(** [Description] : Affiche une image seule dans une page HTML.
    [Args] :
      - conf : configuration de la requête
      - base : argument non utilisé                                          *)
val print_html : Config.config -> 'a -> unit
