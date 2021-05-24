(* $Id: image.ml,v 5.8 2009-03-11 09:22:39 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

(* ************************************************************************** *)
(*  [Fonc] print_image_file : string -> bool                                  *)
(* ************************************************************************** *)
let print_image_file conf fname =
  List.exists
    (fun (suff, ctype) ->
       if Filename.check_suffix fname suff ||
          Filename.check_suffix fname (String.uppercase_ascii suff)
       then
        Output.print_file conf fname ctype true
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
      if print_image_file conf f then () else Hutil.incorrect_request conf
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
  if fname = Filename.basename fname then
    let fname = Util.source_image_file_name conf.bname fname in
    if print_image_file conf fname then () else Hutil.incorrect_request conf
  else Hutil.incorrect_request conf

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
  Output.print_string conf "<head>\n";
  Output.printf conf "  <title>%s</title>\n"
    (Util.transl_nth conf "image/images" 0);
  Output.print_string conf "</head>\n<body>\n";
  Output.printf conf "<img src=\"%s" (Util.commd conf);
  Mutil.list_iter_first
    (fun first (k, v) ->
       let v = if k = "m" then "IM" else v in
       Output.printf conf "%s%s=%s" (if first then "" else "&") k v)
    conf.env;
  Output.print_string conf "\">\n</body>\n</html>"
