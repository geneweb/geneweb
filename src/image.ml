(* camlp5r *)
(* $Id: image.ml,v 5.8 2009-03-11 09:22:39 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;

(* ************************************************************************** *)
(*  [Fonc] content : bool -> string -> int -> string -> unit                  *)
(** [Description] : Envoie les en-têtes de contenu et de cache pour une image
                    sur le flux HTTP sortant de Wserver.
    [Args] :
      - cgi : True en mode CGI, False en serveur autonome
      - t : le type MIME de l'image, par exemple "png", "jpeg" ou "gif"
      - len : la taille en octet de l'image
      - fname : le nom du fichier image
    [Retour] : aucun
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
value content cgi t len fname =
  do {
    if not cgi then Wserver.http "" else ();
    Wserver.wprint "Content-type: image/%s" t;
    Util.nl ();
    Wserver.wprint "Content-length: %d" len;
    Util.nl ();
    Wserver.wprint "Content-disposition: inline; filename=%s"
      (Filename.basename fname);
    Util.nl ();
    (* TODO: Utiliser un cache public pour les images non personelles. *)
    Wserver.wprint "Cache-control: private, max-age=%d" (60 * 60 * 24 * 30);
    Util.nl ();
    Util.nl ();
    Wserver.wflush ();
  }
;

(* ************************************************************************** *)
(*  [Fonc] print_image_type : bool -> string -> string -> bool                *)
(** [Description] : Affiche une image (avec ses en-têtes) en réponse HTTP en
                    utilisant Wserver.
    [Args] :
      - cgi : True en mode CGI, False en serveur autonome
      - fname : le chemin vers le fichier image
      - itype : le type MIME de l'image, par exemple "png", "jpeg" ou "gif"
    [Retour] : True si le fichier image existe et qu'il a été servi en réponse
               HTTP.
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
value print_image_type cgi fname itype =
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let buf = Bytes.create 1024 in
      let len = in_channel_length ic in
      do {
        content cgi itype len fname;
        let rec loop len =
          if len = 0 then ()
          else do {
            let olen = min (String.length buf) len in
            really_input ic buf 0 olen;
            Wserver.wprint "%s" (String.sub buf 0 olen);
            loop (len - olen)
          }
        in
        loop len;
        close_in ic;
        True
      }
  | None -> False ]
;

(* ************************************************************************** *)
(*  [Fonc] print_image_file : bool -> string -> bool                          *)
(* ************************************************************************** *)
value print_image_file cgi fname =
  List.exists
    (fun (suff, itype) ->
       if Filename.check_suffix fname suff ||
          Filename.check_suffix fname (String.uppercase suff) then
         print_image_type cgi fname itype
       else False)
    [(".png", "png"); (".jpg", "jpeg"); (".jpeg", "jpeg"); (".gif", "gif")]
;

(* ************************************************************************** *)
(*  [Fonc] print_personal_image : Config.config -> Gwdb.base -> Gwdb.person -> unit *)
(** [Description] : Affiche l'image d'une personne en réponse HTTP.
    [Args] :
      - conf : configuration de la requête
      - base : base de donnée sélectionnée
      - p : personne dans la base dont il faut afficher l'image
    [Retour] : aucun
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
value print_personal_image conf base p =
  match Util.image_and_size conf base p (fun x y -> Some (1, 1)) with
  [ Some (True, f, _) ->
      if print_image_file conf.cgi f then () else Hutil.incorrect_request conf
  | _ -> Hutil.incorrect_request conf ]
;

(* ************************************************************************** *)
(*  [Fonc] print_source_image : Config.config -> string -> unit               *)
(** [Description] : Affiche une image à partir de son basename uniquement en
                    la cherchant dans les dossiers d'images.
    [Args] :
      - config : configuration de la requête
      - f : basename de l'image
    [Retour] : aucun
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
value print_source_image conf f =
  let fname =
    if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f
  in
  if fname = Filename.basename fname then
    let fname = Util.source_image_file_name conf.bname fname in
    if print_image_file conf.cgi fname then () else Hutil.incorrect_request conf
  else Hutil.incorrect_request conf
;

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Gwdb.base -> unit                         *)
(* ************************************************************************** *)
value print conf base =
  match Util.p_getenv conf.env "s" with
  [ Some f -> print_source_image conf f
  | None ->
      match Util.find_person_in_env conf base "" with
      [ Some p -> print_personal_image conf base p
      | _ -> Hutil.incorrect_request conf ] ]
;

(* ************************************************************************** *)
(*  [Fonc] print_html : config -> 'a -> unit                                  *)
(* ************************************************************************** *)
value print_html conf base =
  do {
    Util.html conf;
    Util.nl ();
    Wserver.wprint "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n";
    Wserver.wprint "<head>\n";
    Wserver.wprint "  <title>%s</title>\n"
      (Util.transl_nth conf "image/images" 0);
    Wserver.wprint "</head>\n<body>\n";
    Wserver.wprint "<img src=\"%s" (Util.commd conf);
    Mutil.list_iter_first
      (fun first (k, v) ->
         let v = if k = "m" then "IM" else v in
         Wserver.wprint "%s%s=%s" (if first then "" else ";") k v)
      conf.env;
    Wserver.wprint "\"%s>\n</body>\n</html>" conf.xhs;
  }
;
