(* camlp4r *)
(* $Id: image.ml,v 3.2 2000-05-02 17:15:45 doligez Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Util;
open Config;

value nl () = Wserver.wprint "\013\010";

value content cgi t len =
  do if not cgi then
       do Wserver.wprint "HTTP/1.0 200 OK"; nl (); return ()
     else ();
     Wserver.wprint "Content-type: image/%s" t; nl ();
     Wserver.wprint "Content-length: %d" len; nl ();
     nl ();
     Wserver.wflush ();
  return ()
;

value print_image_type cgi fname itype =
  match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do try
           do content cgi itype (in_channel_length ic);
              try
                let b = " " in
                while True do
                  b.[0] := input_char ic;
                  Wserver.wprint "%c" b.[0];
                done
              with [ End_of_file -> () ];
           return ()
         with e -> do close_in ic; return raise e;
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
