(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: sendImage.ml,v 4.2 2001-04-21 13:50:56 ddr Exp $ *)

open Gutil;
open Util;
open Config;
open Def;

value incorrect conf = do { incorrect_request conf; raise Update.ModErr };

value incorrect_content_type conf base p s =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<p>\n<font size=-1><em>";
    Wserver.wprint "Error: incorrect image content type: %s" s;
    Wserver.wprint "</em></font>\n<p>\n";
    Wserver.wprint "<ul><li>%s</ul>\n"
      (referenced_person_title_text conf base p);
    trailer conf;
    raise Update.ModErr
  }
;

value raw_get conf key =
  try List.assoc key conf.env with [ Not_found -> incorrect conf ]
;

(* Send image form *)

value print_send_image conf base p =
  let title h =
    do {
      Wserver.wprint "%s"
        (capitale
           (transl_decline conf "send" (transl_nth conf "image/images" 0)));
      if h then ()
      else do {
        let fn = p_first_name base p in
        let sn = p_surname base p in
        Wserver.wprint ": ";
        Wserver.wprint "%s.%d %s" fn p.occ sn;
      };
    }
  in
  let digest = Update.digest_person p in
  do {
    header conf title;
    tag "form" "method=POST action=\"%s\" enctype=\"multipart/form-data\""
        conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=SND_IMAGE_OK>\n";
      Wserver.wprint "<input type=hidden name=i value=%d>\n"
        (Adef.int_of_iper p.cle_index);
      Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
      Wserver.wprint "\n";
      Wserver.wprint "%s:\n" (capitale (transl conf "file"));
      Wserver.wprint "\
<input type=file name=file size=50 maxlength=250 accept=\"image/*\">
";
      Wserver.wprint "<p>\n";
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    trailer conf;
  }
;

value print conf base =
  match p_getint conf.env "i" with
  [ Some ip ->
      let p = base.data.persons.get ip in
      let fn = p_first_name base p in
      let sn = p_surname base p in
      if sou base p.image <> "" || fn = "?" || sn = "?" then
        incorrect_request conf
      else print_send_image conf base p
  | _ -> incorrect_request conf ]
;

(* Delete image form *)

value print_delete_image conf base p =
  let title h =
    do {
      Wserver.wprint "%s"
        (capitale
           (transl_decline conf "delete" (transl_nth conf "image/images" 0)));
      if h then ()
      else do {
        let fn = p_first_name base p in
        let sn = p_surname base p in
        let occ =
          if fn = "?" || sn = "?" then Adef.int_of_iper p.cle_index else p.occ
        in
        Wserver.wprint ": ";
        Wserver.wprint "%s.%d %s" fn occ sn;
      };
    }
  in
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=DEL_IMAGE_OK>\n";
      Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
        (Adef.int_of_iper p.cle_index);
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some ip ->
      let p = base.data.persons.get ip in
      if sou base p.image <> "" then incorrect_request conf
      else
        match auto_image_file conf base p with
        [ Some _ -> print_delete_image conf base p
        | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;

(* Send image form validated *)

value print_sent conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "image received"))
  in
  do {
    header conf title;
    tag "ul" begin
      html_li conf;
      afficher_personne_referencee conf base p;
      Wserver.wprint "\n";
    end;
    trailer conf;
  }
;

value write_file fname content =
  let oc = open_out_bin fname in
  do { output_string oc content; flush oc; close_out oc; }
;

value move_file_to_old conf typ fname bfname =
  let old_dir =
    Filename.concat (Util.base_path ["images"] conf.bname) "old"
  in
  if Sys.file_exists (Filename.concat old_dir bfname ^ ".gif") ||
     Sys.file_exists (Filename.concat old_dir bfname ^ ".jpg") ||
     Sys.file_exists (Filename.concat old_dir bfname ^ ".png") then
    try Sys.remove (fname ^ typ) with [ Sys_error _ -> () ]
  else do {
    try Unix.mkdir old_dir 0o777 with [ Unix.Unix_error _ _ _ -> () ];
    try Unix.rename (fname ^ typ) (Filename.concat old_dir bfname ^ typ) with
    [ Unix.Unix_error _ _ _ -> () ];
  }
;

value normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff &&
     Char.code s.[1] = 0xd8 && String.sub s 6 4 = "JFIF" then
    ".jpg"
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then ".png"
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then ".gif"
  else ""
;

value string_search s v =
  loop 0 0 where rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
;

(* get the image type, possibly removing spurious header *)

value image_type s =
  let r = normal_image_type s in
  if r <> "" then (r, s)
  else
    match string_search s "JFIF" with
    [ Some i when i > 6 ->
        let s = String.sub s (i - 6) (String.length s - i + 6) in
        (normal_image_type s, s)
    | _ ->
        match string_search s "\137PNG" with
        [ Some i ->
            let s = String.sub s i (String.length s - i) in
            (normal_image_type s, s)
        | _ ->
            match string_search s "GIF8" with
            [ Some i ->
                let s = String.sub s i (String.length s - i) in
                (normal_image_type s, s)
            | None -> ("", s) ] ] ]
;

value dump_bad_image conf s =
  match p_getenv conf.base_env "dump_bad_images" with
  [ Some "yes" ->
      try
        let oc = open_out_bin "bad-image" in
        do { output_string oc s; flush oc; close_out oc }
      with
      [ Sys_error _ -> () ]
  | _ -> () ]
;

value effective_send_ok conf base p file =
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    let s =
      loop 0 strm where rec loop len =
        parser
        [ [: `x :] -> loop (Buff.store len x) strm
        | [: :] -> Buff.get len ]
    in
    content ^ s
  in
  let (typ, content) =
    let (x, c) = image_type content in
    if x = "" then do {
      let ct = Wserver.extract_param "content-type: " '\n' request in
      dump_bad_image conf content;
      incorrect_content_type conf base p ct
    }
    else (x, c)
  in
  let bfname = default_image_name base p in
  let bfdir =
    let bfdir = Util.base_path ["images"] conf.bname in
    if Sys.file_exists bfdir then bfdir
    else do {
      let d = Filename.concat Util.base_dir.val "images" in
      let d1 = Filename.concat d conf.bname in
      try Unix.mkdir d 0o777 with [ Unix.Unix_error _ _ _ -> () ];
      try Unix.mkdir d1 0o777 with [ Unix.Unix_error _ _ _ -> () ];
      d1
    }
  in
  let fname = Filename.concat bfdir bfname in
  do {
    if Sys.file_exists (fname ^ ".gif") then
      move_file_to_old conf ".gif" fname bfname
    else if Sys.file_exists (fname ^ ".jpg") then
      move_file_to_old conf ".jpg" fname bfname
    else if Sys.file_exists (fname ^ ".png") then
      move_file_to_old conf ".png" fname bfname
    else ();
    write_file (fname ^ typ) content;
    let key = (sou base p.first_name, sou base p.surname, p.occ) in
    History.record conf base key "si";
    print_sent conf base p;
  }
;

value print_send_ok conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let ip =
          let s = raw_get conf "i" in
          try int_of_string s with [ Failure _ -> incorrect conf ]
        in
        let p = base.data.persons.get ip in
        let digest = Update.digest_person p in
        if digest = raw_get conf "digest" then
          let file = raw_get conf "file" in effective_send_ok conf base p file
        else Update.error_digest conf base
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

(* Delete image form validated *)

value print_deleted conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "image deleted"))
  in
  do {
    header conf title;
    tag "ul" begin
      html_li conf;
      afficher_personne_referencee conf base p;
      Wserver.wprint "\n";
    end;
    trailer conf;
  }
;

value effective_delete_ok conf base p =
  let bfname = default_image_name base p in
  let fname = Filename.concat (Util.base_path ["images"] conf.bname) bfname in
  do {
    if Sys.file_exists (fname ^ ".gif") then
      move_file_to_old conf ".gif" fname bfname
    else if Sys.file_exists (fname ^ ".jpg") then
      move_file_to_old conf ".jpg" fname bfname
    else if Sys.file_exists (fname ^ ".png") then
      move_file_to_old conf ".png" fname bfname
    else incorrect conf;
    let key = (sou base p.first_name, sou base p.surname, p.occ) in
    History.record conf base key "di";
    print_deleted conf base p;
  }
;

value print_del_ok conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        match p_getint conf.env "i" with
        [ Some ip ->
            let p = base.data.persons.get ip in
            effective_delete_ok conf base p
        | None -> incorrect conf ]
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;
