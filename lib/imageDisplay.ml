(* $Id: image.ml,v 5.8 2009-03-11 09:22:39 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

let print_placeholder_gendered_portrait conf p size =
  let image, alt =
    match Driver.get_sex p with
    | Male -> ("male.png", "M")
    | Female -> ("female.png", "F")
    | Neuter -> ("sexunknown.png", "?")
  in
  Output.printf conf
    {|<img src="%s/%s" alt="%s" title="sex" width="%d" height="%d">|}
    (Util.images_prefix conf) image alt size size

(* ************************************************************************** *)
(*  [Fonc] content : string -> int -> string -> unit                          *)
(* ************************************************************************** *)

(** [Description] : Envoie les en-têtes de contenu et de cache pour un fichier
    image, pdf ou html sur le flux HTTP sortant de Wserver. [Args] :
    - ct : le content_type MIME du fichier, par exemple "image/png",
      "image/jpeg" ou "application/pdf"
    - len : la taille en octet du fichier
    - fname : le nom du fichier [Retour] : aucun [Rem] : Ne pas utiliser en
      dehors de ce module. *)
let content conf ct len fname =
  Output.status conf Def.OK;
  Output.header conf "Content-type: %s" ct;
  Output.header conf "Content-length: %d" len;
  Output.header conf "Content-disposition: inline; filename=%s"
    (Filename.basename fname);
  (* TODO: Utiliser un cache public pour les images non personelles. *)
  Output.header conf "Cache-control: private, max-age=%d" (60 * 60 * 24 * 365);
  Output.flush conf

let print_image_file conf fname =
  let res =
    List.find_opt
      (fun (suff, _ctype) ->
        if
          Filename.check_suffix fname suff
          || Filename.check_suffix fname (String.uppercase_ascii suff)
        then true
        else false)
      [
        (".png", "image/png");
        (".jpg", "image/jpeg");
        (".jpeg", "image/jpeg");
        (".pjpeg", "image/jpeg");
        (".gif", "image/gif");
        (".pdf", "application/pdf");
        (".htm", "text/html");
        (".html", "text/html");
      ]
  in
  match res with
  | None ->
      Error
        (Format.sprintf "Could not find mime type from extension for file: %s"
           fname)
  | Some (_suff, ctype) -> (
      try
        let ic = Secure.open_in_bin fname in
        let buf = Bytes.create 1024 in
        let len = in_channel_length ic in
        content conf ctype len fname;
        let rec loop len =
          if len = 0 then ()
          else
            let olen = min (Bytes.length buf) len in
            really_input ic buf 0 olen;
            Output.print_sstring conf (Bytes.sub_string buf 0 olen);
            loop (len - olen)
        in
        loop len;
        close_in ic;
        Ok ()
      with Sys_error e ->
        Logs.syslog `LOG_ERR
          (Format.sprintf "Error printing image file content for %s : %s" fname
             e);
        Error e)

(* ************************************************************************** *)
(*  [Fonc] print_portrait : Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit *)
(* ************************************************************************** *)

(** [Description] : Affiche l'image d'une personne en réponse HTTP. [Args] :
    - conf : configuration de la requête
    - base : base de donnée sélectionnée
    - p : personne dans la base dont il faut afficher l'image [Retour] : aucun
      [Rem] : Ne pas utiliser en dehors de ce module. *)
let print_portrait conf base p =
  match Image.get_portrait conf base p with
  | Some (`Path path) ->
      Result.fold ~ok:ignore
        ~error:(fun _ ->
          Hutil.incorrect_request conf
            ~comment:"print_image_file failed (portrait)")
        (print_image_file conf path)
  | Some (`Url url) ->
      Util.html conf;
      Output.print_sstring conf "<head><title>";
      Output.print_sstring conf (Util.transl_nth conf "image/images" 0);
      Output.print_sstring conf "</title></head><body>";
      Output.print_sstring conf (Printf.sprintf {|<img src=%s>|} url);
      Output.print_sstring conf "</body></html>"
  | None -> Hutil.incorrect_request conf

(* ********************************************************************************* *)
(*  [Fonc] print_blason : Config.config -> Gwdb.base -> Gwdb.person -> unit *)
(* ********************************************************************************* *)

(** [Description] : Affiche l'image de la famille d'une personne en réponse
    HTTP. [Args] :
    - conf : configuration de la requête
    - base : base de donnée sélectionnée
    - p : personne dans la base dont il faut afficher l'image de la famille
      [Retour] : aucun [Rem] : Ne pas utiliser en dehors de ce module. *)
let print_blason_aux conf base p =
  match Image.get_blason conf base p false with
  | Some (`Path path) ->
      Result.fold ~ok:ignore
        ~error:(fun _ -> Hutil.incorrect_request conf)
        (print_image_file conf path)
  | Some (`Url url) ->
      Util.html conf;
      Output.print_sstring conf "<head><title>";
      Output.print_sstring conf (Util.transl_nth conf "image/images" 0);
      Output.print_sstring conf "</title></head><body>";
      Output.print_sstring conf (Printf.sprintf {|<img src=%s>|} url);
      Output.print_sstring conf "</body></html>"
  | None -> Hutil.incorrect_request conf

let print_source conf f =
  let fname = if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f in
  let fname = Filename.concat (!GWPARAM.images_d conf.bname) fname in
  if (conf.wizard || conf.friend) || Image.is_not_private_img conf fname then
    Result.fold ~ok:ignore
      ~error:(fun _ -> Hutil.incorrect_request conf)
      (print_image_file conf fname)
  else Hutil.incorrect_request conf

let print conf base =
  match Util.p_getenv conf.env "s" with
  | Some f -> print_source conf f
  | None -> (
      match Util.find_person_in_env conf base "" with
      | Some p -> print_portrait conf base p
      | None -> Hutil.incorrect_request conf)

let print_blason conf base =
  match Util.p_getenv conf.env "s" with
  | Some f -> print_source conf f
  | None -> (
      match Util.find_person_in_env conf base "" with
      | Some p -> print_blason_aux conf base p
      | None -> Hutil.incorrect_request conf)

let print_html conf =
  let ext =
    match Util.p_getenv conf.env "s" with
    | Some f -> Filename.extension f
    | _ -> ""
  in
  match ext with
  | ".htm" | ".html" | ".pdf" ->
      let title _ = Output.print_sstring conf "Error" in
      Hutil.header conf title;
      Output.print_sstring conf
        "<body><ul><li>DOCH not available for html and pdf.";
      Hutil.trailer conf
  | _ ->
      Util.html conf;
      Output.print_sstring conf "<head><title>";
      Output.print_sstring conf (Util.transl_nth conf "image/images" 0);
      Output.print_sstring conf "</title></head><body><img src=\"";
      Output.print_string conf (Util.commd conf);
      Mutil.list_iter_first
        (fun first (k, v) ->
          let v = if k = "m" then Adef.encoded "IM" else v in
          if not first then Output.print_sstring conf "&";
          Output.print_sstring conf k;
          Output.print_sstring conf "=";
          Output.print_string conf v)
        conf.env;
      Output.print_sstring conf "\"></body></html>"
