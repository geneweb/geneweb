(* $Id: image.ml,v 5.8 2009-03-11 09:22:39 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

(* ************************************************************************** *)
(*  [Fonc] content : string -> int -> string -> unit                          *)
(** [Description] : Envoie les en-têtes de contenu et de cache pour un fichier
                    image, pdf ou html sur le flux HTTP sortant de Wserver.
    [Args] :
      - ct : le content_type MIME du fichier, par exemple "image/png",
             "image/jpeg" ou "application/pdf"
      - len : la taille en octet du fichier
      - fname : le nom du fichier
    [Retour] : aucun
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
let content ct len fname =
  Wserver.http HttpStatus.OK;
  Wserver.header "Content-type: %s" ct;
  Wserver.header "Content-length: %d" len;
  Wserver.header "Content-disposition: inline; filename=%s"
    (Filename.basename fname);
  (* TODO: Utiliser un cache public pour les images non personelles. *)
  Wserver.header "Cache-control: private, max-age=%d" (60 * 60 * 24 * 30);
  Wserver.wflush ()

(* ************************************************************************** *)
(*  [Fonc] print_image_type : config -> string -> string -> bool                        *)
(** [Description] : Affiche une image (avec ses en-têtes) en réponse HTTP en
                    utilisant Wserver.
    [Args] :
      - fname : le chemin vers le fichier image
      - ctype : le content_type MIME du fichier, par exemple "image/png",
                "image/jpeg" ou "application/pdf"
    [Retour] : True si le fichier image existe et qu'il a été servi en réponse
               HTTP.
    [Rem] : Ne pas utiliser en dehors de ce module.                           *)
(* ************************************************************************** *)
let print_image_type fname ctype =
  match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
    Some ic ->
      let buf = Bytes.create 1024 in
      let len = in_channel_length ic in
      content ctype len fname;
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          Wserver.printf "%s" (Bytes.sub_string buf 0 olen);
          loop (len - olen)
      in
      loop len; close_in ic; true
  | None -> false

let print_image_type_2 conf fname ctype =
  if Sys.file_exists fname then
    match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
      Some ic ->
        let buf = Bytes.create 1024 in
        let len = in_channel_length ic in
        content ctype len fname;
        let rec loop len =
          if len = 0 then ()
          else
            let olen = min (Bytes.length buf) len in
            really_input ic buf 0 olen;
            Wserver.printf "%s" (Bytes.sub_string buf 0 olen);
            loop (len - olen)
        in
        loop len; close_in ic; true
    | None -> false
  else
    begin
    let title _ = Wserver.printf "Error" in
    Hutil.header conf title;
    Wserver.printf "<ul>\n";
    Wserver.printf "<li>\n";
    Wserver.printf "Cannot access file \"%s\"\n" fname;
    Wserver.printf "</li>\n";
    Wserver.printf "</ul>\n";
    Hutil.gen_trailer true conf;
    raise Exit
    end


(* ************************************************************************** *)
(*  [Fonc] print_image_file : string -> bool                                  *)
(* ************************************************************************** *)
let print_image_file fname =
  List.exists
    (fun (suff, ctype) ->
       if Filename.check_suffix fname suff ||
          Filename.check_suffix fname (String.uppercase_ascii suff)
       then
         print_image_type fname ctype
       else false)
    [(".png", "image/png"); (".jpg", "image/jpeg");
     (".jpeg", "image/jpeg"); (".pjpeg", "image/jpeg");
     (".gif", "image/gif"); (".pdf", "application/pdf");
     (".htm", "text/html"); (".html", "text/html")]

let print_image_file_2 conf fname =
  List.exists
    (fun (suff, ctype) ->
       if Filename.check_suffix fname suff ||
          Filename.check_suffix fname (String.uppercase_ascii suff)
       then
         print_image_type_2 conf fname ctype
       else false)
    [(".png", "image/png"); (".jpg", "image/jpeg");
     (".jpeg", "image/jpeg"); (".pjpeg", "image/jpeg");
     (".gif", "image/gif"); (".pdf", "application/pdf");
     (".htm", "text/html"); (".html", "text/html")]

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
let print_personal_image conf base p =
  match Util.image_and_size conf base p (fun _ _ -> Some (1, 1)) with
    Some (true, f, _) ->
      if print_image_file_2 conf f then () else Hutil.incorrect_request conf
  | _ -> Hutil.incorrect_request conf

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
let print_source_image conf f =
  let fname =
    if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f
  in
  (*if fname = Filename.basename fname then TODO clean if ok*)
    let fname = Util.source_image_file_name conf fname in
    if print_image_file_2 conf fname then () else Hutil.incorrect_request conf
  (*else Hutil.incorrect_request conf*)

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Gwdb.base -> unit                         *)
(* ************************************************************************** *)
let print conf base =
  match Util.p_getenv conf.env "s" with
    Some f -> print_source_image conf f
  | None ->
      match Util.find_person_in_env conf base "" with
        Some p -> print_personal_image conf base p
      | _ -> Hutil.incorrect_request conf

(* ************************************************************************** *)
(*  [Fonc] print_html : config -> 'a -> unit                                  *)
(* ************************************************************************** *)
let print_html conf =
  Util.html conf;
  Wserver.printf "<head>\n";
  Wserver.printf "  <title>%s</title>\n"
    (Util.transl_nth conf "image/images" 0);
  Wserver.printf "</head>\n<body>\n";
  Wserver.printf "<img src=\"%s" (Util.commd conf);
  Mutil.list_iter_first
    (fun first (k, v) ->
       let v = if k = "m" then "DOC" else v in
       Wserver.printf "%s%s=%s" (if first then "" else "&") k v)
    conf.env;
  Wserver.printf "\"%s>\n</body>\n</html>" conf.xhs
