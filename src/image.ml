(* camlp5r *)
(* $Id: image.ml,v 5.8 2009-03-11 09:22:39 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Hutil;
open Util;

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
    Util.nl ();
    Wserver.wflush ();
  }
;

value print_image_type cgi fname itype =
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let buf = String.create 1024 in
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

value print_image_file cgi fname =
  List.exists
    (fun (suff, itype) ->
       if Filename.check_suffix fname suff ||
          Filename.check_suffix fname (String.uppercase suff) then
         print_image_type cgi fname itype
       else False)
    [(".png", "png"); (".jpg", "jpeg"); (".jpeg", "jpeg"); (".gif", "gif")]
;

value print_personal_image conf base p =
  match image_and_size conf base p (fun x y -> Some (1, 1)) with
  [ Some (True, f, _) ->
      if print_image_file conf.cgi f then () else incorrect_request conf
  | _ -> incorrect_request conf ]
;

value print_source_image conf f =
  let fname =
    if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f
  in
  if fname = Filename.basename fname then
    let fname = source_image_file_name conf.bname fname in
    if print_image_file conf.cgi fname then () else incorrect_request conf
  else incorrect_request conf
;

value print conf base =
  match p_getenv conf.env "s" with
  [ Some f -> print_source_image conf f
  | None ->
      match find_person_in_env conf base "" with
      [ Some p -> print_personal_image conf base p
      | _ -> incorrect_request conf ] ]
;

value print_html conf base =
  do {
    Util.html conf;
    nl ();
    Wserver.wprint "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n";
    Wserver.wprint "<head>\n";
    Wserver.wprint "  <title>%s</title>\n"
      (Util.transl_nth conf "image/images" 0);
    Wserver.wprint "</head>\n<body>\n";
    Wserver.wprint "<img src=\"%s" (commd conf);
    Mutil.list_iter_first
      (fun first (k, v) ->
         let v = if k = "m" then "IM" else v in
         Wserver.wprint "%s%s=%s" (if first then "" else ";") k v)
      conf.env;
    Wserver.wprint "\"%s>\n</body>\n</html>" conf.xhs;
  }
;
