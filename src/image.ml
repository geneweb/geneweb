(* camlp4r *)
(* $Id: image.ml,v 3.4 2000-05-03 17:54:34 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Util;
open Config;

value content cgi t len =
  do if not cgi then
       do Wserver.wprint "HTTP/1.0 200 OK"; Util.nl (); return ()
     else ();
     Wserver.wprint "Content-type: image/%s" t; Util.nl ();
     Wserver.wprint "Content-length: %d" len; Util.nl ();
     Util.nl ();
     Wserver.wflush ();
  return ()
;

value print_image_type cgi fname itype =
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let buf = String.create 1024 in
      let len = in_channel_length ic in
      do content cgi itype len;
         loop len where rec loop len =
           if len == 0 then ()
           else
             let olen = min (String.length buf) len in
             do really_input ic buf 0 olen;
                Wserver.wprint "%s" (String.sub buf 0 olen);
             return loop (len - olen);
         close_in ic;
      return True
 | None -> False ]
;

value print_image_file cgi fname =
  List.exists
    (fun (suff, itype) ->
       if Filename.check_suffix fname suff
       || Filename.check_suffix fname (String.uppercase suff) then
         print_image_type cgi fname itype
       else False)
    [(".png", "png"); (".jpg", "jpeg"); (".jpeg", "jpeg");
     (".gif", "gif")]
;

value print_personal_image conf base p =
  match image_and_size conf base p (fun x -> Some (1, 1)) with
  [ Some (f, Some _) ->
      if print_image_file conf.cgi f then () else incorrect_request conf
  | _ -> incorrect_request conf ]
;

value print conf base =
  match find_person_in_env conf base "" with
  [ Some p -> print_personal_image conf base p
  | _ -> incorrect_request conf ]
;
