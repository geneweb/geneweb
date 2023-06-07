open Geneweb
open Config
open Def
open Gwdb
open Util

let rn fname s =
  try if Sys.file_exists fname then Sys.rename fname s
  with Failure _ ->
    Printf.eprintf "Rn failed: %s to %s\n" fname s;
    flush stderr

type image_type = JPEG | GIF | PNG

let extension_of_type = function
  | JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let image_types = [ JPEG; GIF; PNG ]
let raise_modErr s = raise @@ Update.ModErr (Update.UERR s)

let incorrect conf str =
  Hutil.incorrect_request conf ~comment:str;
  failwith (__FILE__ ^ " (" ^ str ^ ")" :> string)

let incorrect_content_type conf base p s =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (Util.transl conf "error"))
  in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<p>\n<em style=\"font-size:smaller\">";
  Output.printf conf "Error: incorrect image content type: %s" s;
  Output.printf conf "</em>\n</p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (Util.referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  failwith (__FILE__ ^ " " ^ string_of_int __LINE__ :> string)

let error_too_big_image conf base p len max_len =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (Util.transl conf "error"))
  in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<p><em style=\"font-size:smaller\">";
  Output.printf conf "Error: this image is too big: %d bytes<br>\n" len;
  Output.printf conf "Maximum authorized in this database: %d bytes<br>\n"
    max_len;
  Output.printf conf "</em></p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (Util.referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  failwith (__FILE__ ^ " " ^ string_of_int __LINE__ :> string)

let raw_get conf key =
  try List.assoc key conf.env
  with Not_found -> incorrect conf ("raw_get" ^ key)

let insert_saved fname =
  let l = String.split_on_char Filename.dir_sep.[0] fname |> List.rev in
  let l =
    (if List.length l >= 2 then List.hd l :: "saved" :: List.tl l
    else if List.length l = 1 then List.hd l :: [ "saved" ]
    else l)
    |> List.rev
  in
  String.concat Filename.dir_sep l

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content;
  flush oc;
  close_out oc

(* Move fname to saved_dir if it exists with some extension.
   Returns the number of moved files *)
let move_file_to_saved conf fname bfname =
  List.fold_left
    (fun cnt typ ->
      let ext = extension_of_type typ in
      let new_file = fname ^ ext in
      if Sys.file_exists new_file then (
        let old_dir =
          Filename.concat (Util.base_path [ "images" ] conf.bname) "saved"
        in
        let old_file = Filename.concat old_dir bfname ^ ext in
        Mutil.rm old_file;
        Mutil.mkdir_p old_dir;
        (try Unix.rename new_file old_file
         with Unix.Unix_error (_, _, _) -> ());
        cnt + 1)
      else cnt)
    0 image_types

let move_file_to_save dir file =
  try
    let save_dir = Filename.concat dir "saved" in
    let fname = Filename.basename file in
    if not (Sys.file_exists save_dir) then Mutil.mkdir_p save_dir;
    let orig_file = Filename.concat dir fname in
    let saved_file = Filename.concat save_dir fname in
    (* TODO handle rn errors *)
    rn orig_file saved_file;
    let orig_file_t = Filename.remove_extension orig_file ^ ".txt" in
    let saved_file_t = Filename.remove_extension saved_file ^ ".txt" in
    if Sys.file_exists orig_file_t then rn orig_file_t saved_file_t;
    1
  with _ -> 0

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then Some JPEG
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then Some PNG
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then Some GIF
  else None

let string_search s v =
  let rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

(* get the image type, possibly removing spurious header *)

let image_type s =
  match normal_image_type s with
  | Some t -> Some (t, s)
  | None -> (
      match string_search s "JFIF" with
      | Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ -> (
          match string_search s "\137PNG" with
          | Some i ->
              let s = String.sub s i (String.length s - i) in
              Some (PNG, s)
          | _ -> (
              match string_search s "GIF8" with
              | Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None)))

let dump_bad_image conf s =
  match List.assoc_opt "dump_bad_images" conf.base_env with
  | Some "yes" -> (
      try
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s;
        flush oc;
        close_out oc
      with Sys_error _ -> ())
  | _ -> ()

let swap_files conf file1 file2 txt =
  let tmp_file =
    String.concat Filename.dir_sep
      [ Util.base_path [ "images" ] conf.bname; "tempfile.tmp" ]
  in
  let ext_1 = Filename.extension file1 in
  let ext_2 = Filename.extension file2 in
  rn file1 tmp_file;
  rn file2 (Filename.remove_extension file1 ^ ext_2);
  rn tmp_file (Filename.remove_extension file2 ^ ext_1);
  if txt then (
    let tmp_file_t =
      String.concat Filename.dir_sep
        [ Util.base_path [ "images" ] conf.bname; "tempfile.tmp" ]
    in
    let file1_t = Filename.remove_extension file1 ^ ".txt" in
    let file2_t = Filename.remove_extension file2 ^ ".txt" in
    rn file1_t tmp_file_t;
    rn file2_t file1_t;
    rn tmp_file_t file2_t)

let rename_files file1 file2 txt =
  rn file1 file2;
  if txt then
    let file1_t = Filename.remove_extension file1 ^ ".txt" in
    let file2_t = Filename.remove_extension file2 ^ ".txt" in
    rn file1_t file2_t

let clean_saved_portrait dir bfname =
  let file =
    Filename.remove_extension (String.concat Filename.dir_sep [ dir; bfname ])
  in
  Mutil.rm (file ^ ".jpg");
  Mutil.rm (file ^ ".jpeg");
  Mutil.rm (file ^ ".png");
  Mutil.rm (file ^ ".gif")

let get_extension conf saved keydir =
  let f =
    if saved then
      String.concat Filename.dir_sep
        [ Util.base_path [ "images" ] conf.bname; "saved"; keydir ]
    else
      String.concat Filename.dir_sep
        [ Util.base_path [ "images" ] conf.bname; keydir ]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".jpeg") then ".jpeg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let print_confirm_c conf base save_m report =
  match Util.p_getint conf.env "i" with
  | Some ip ->
      let p = poi base (Gwdb.iper_of_string (string_of_int ip)) in
      let digest = Image.default_portrait_filename base p in
      let new_env =
        List.fold_left
          (fun accu (k, v) ->
            if k = "m" then ("m", Adef.encoded "REFRESH") :: accu
            else if k = "idigest" || k = "" || k = "file" then accu
            else (k, v) :: accu)
          [] conf.env
      in
      let new_env =
        if save_m = "REFRESH" then new_env
        else ("em", Adef.encoded save_m) :: new_env
      in
      let new_env = ("idigest", Adef.encoded digest) :: new_env in
      let new_env = ("report", Adef.encoded report) :: new_env in
      let conf = { conf with env = new_env } in
      Perso.interp_templ "carrousel" conf base p
  | None -> Hutil.incorrect_request conf

(* ************************************************************************ *)
(*  send, delete, reset and print functions                                        *)
(*                                                                          *)
(* ************************************************************************ *)

(* we need print_link_delete_image in the send function *)
let print_link_delete_image conf base p =
  if Option.is_some @@ Image.get_portrait conf base p then (
    Output.print_sstring conf {|<p><a class="btn btn-primary" href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=DEL_IMAGE&i=";
    Output.print_string conf (get_iper p |> string_of_iper |> Mutil.encode);
    Output.print_sstring conf {|">|};
    transl conf "delete" |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf {| |};
    transl_nth conf "image/images" 0 |> Output.print_sstring conf;
    Output.print_sstring conf "</a></p>")

let print_send_image conf base p =
  let title h =
    if Option.is_some @@ Image.get_portrait conf base p then
      transl_nth conf "image/images" 0
      |> transl_decline conf "modify"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    else
      transl_nth conf "image/images" 0
      |> transl_decline conf "add" |> Utf8.capitalize_fst
      |> Output.print_sstring conf;
    if not h then (
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html (p_first_name base p));
      Output.printf conf ".%d " (get_occ p);
      Output.print_string conf (Util.escape_html (p_surname base p));)
  in
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<h2>\n";
  title false;
  Output.print_sstring conf "</h2>\n";
  Output.printf conf
    "<form method=\"post\" action=\"%s\" enctype=\"multipart/form-data\">\n"
    conf.command;
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "SND_IMAGE_OK");
  Util.hidden_input conf "i" (get_iper p |> string_of_iper |> Mutil.encode);
  Util.hidden_input conf "digest" (Mutil.encode digest);
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "file"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf
    {| <input type="file" class="form-control-file" name="file" accept="image/*"></p>|};
  (match
     Option.map int_of_string @@ List.assoc_opt "max_images_size" conf.base_env
   with
  | Some len ->
      Output.print_sstring conf "<p>(maximum authorized size = ";
      Output.print_sstring conf (string_of_int len);
      Output.print_sstring conf " bytes)</p>"
  | None -> ());
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-primary mt-2">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></form>";
  print_link_delete_image conf base p;
  Hutil.trailer conf

let print_sent conf base p =
  let title _ =
    transl conf "image received"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let effective_send_ok conf base p file =
  let strm = Stream.of_string file in
  let request, content = Wserver.get_request_and_content strm in
  let content =
    let s =
      let rec loop len (strm__ : _ Stream.t) =
        match Stream.peek strm__ with
        | Some x ->
            Stream.junk strm__;
            loop (Buff.store len x) strm
        | _ -> Buff.get len
      in
      loop 0 strm
    in
    (content :> string) ^ s
  in
  let typ, content =
    match image_type content with
    | None ->
        dump_bad_image conf content;
        Mutil.extract_param "content-type: " '\n' request
        |> incorrect_content_type conf base p
    | Some (typ, content) -> (
        match
          Option.map int_of_string
          @@ List.assoc_opt "max_images_size" conf.base_env
        with
        | Some len when String.length content > len ->
            error_too_big_image conf base p (String.length content) len
        | _ -> (typ, content))
  in
  let bfname = Image.default_portrait_filename base p in
  let bfdir =
    let bfdir = Util.base_path [ "images" ] conf.bname in
    if Sys.file_exists bfdir then bfdir
    else
      let d = Filename.concat (Secure.base_dir ()) "images" in
      let d1 = Filename.concat d conf.bname in
      (try Unix.mkdir d 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d1 0o777 with Unix.Unix_error (_, _, _) -> ());
      d1
  in
  let fname = Filename.concat bfdir bfname in
  let _moved = move_file_to_saved conf fname bfname in
  write_file (fname ^ extension_of_type typ) content;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "si";
  print_sent conf base p

let print_send_ok conf base =
  let ip =
    try raw_get conf "i" |> Mutil.decode |> iper_of_string
    with Failure _ -> incorrect conf "print send ok"
  in
  let p = poi base ip in
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  if (digest :> string) = Mutil.decode (raw_get conf "digest") then
    raw_get conf "file" |> Adef.as_string |> effective_send_ok conf base p
  else Update.error_digest conf

(* carrousel *)
let effective_send_c_ok conf base p file file_name mode =
  let notes =
    match Util.p_getenv conf.env "notes" with
    | Some v ->
        Util.safe_html
          (Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> Adef.safe ""
  in
  let strm = Stream.of_string file in
  let request, content = Wserver.get_request_and_content strm in
  let content =
    if mode = "comment" then ""
    else
      let s =
        let rec loop len (strm__ : _ Stream.t) =
          match Stream.peek strm__ with
          | Some x ->
              Stream.junk strm__;
              loop (Buff.store len x) strm
          | _ -> Buff.get len
        in
        loop 0 strm
      in
      (content :> string) ^ s
  in
  let typ, content =
    if content <> "" then
      match image_type content with
      | None ->
          let ct = Mutil.extract_param "Content-Type: " '\n' request in
          dump_bad_image conf content;
          incorrect_content_type conf base p ct
      | Some (typ, content) -> (
          match List.assoc_opt "max_images_size" conf.base_env with
          | Some len when String.length content > int_of_string len ->
              error_too_big_image conf base p (String.length content)
                (int_of_string len)
          | _ -> (typ, content))
    else (GIF, content (* we dont care which type, content = "" *))
  in
  let keydir = Image.default_portrait_filename base p in
  let full_dir =
    if mode = "portraits" then
      String.concat Filename.dir_sep [ Util.base_path [ "images" ] conf.bname ]
    else
      String.concat Filename.dir_sep
        [ Util.base_path [ "src" ] conf.bname; "images"; keydir ]
  in
  (* TODO is this necessary ?? *)
  let _ =
    if not (Sys.file_exists full_dir) then (
      let d1 =
        String.concat Filename.dir_sep
          [ Util.base_path [ "images" ] conf.bname ]
      in
      let d2 =
        String.concat Filename.dir_sep
          [ Util.base_path [ "src" ] conf.bname; "images"; keydir ]
      in
      Mutil.mkdir_p d1;
      Mutil.mkdir_p d2)
  in
  let full_name =
    Filename.concat full_dir
      (if mode = "portraits" then keydir ^ extension_of_type typ else file_name)
  in
  let save_dir = Filename.concat full_dir "saved" in
  if mode = "portraits" then
    (* if saved portrait exists, move it to images *)
    match Image.get_old_portrait conf base p with
    | Some (`Path old_portrait) ->
        let fname = Filename.basename old_portrait in
        let img_dir =
          String.concat Filename.dir_sep
            [ Util.base_path [ "src" ] conf.bname; "images"; keydir ]
        in
        rn old_portrait
          (Filename.concat img_dir
             ((if Sys.file_exists (Filename.concat img_dir fname) then "~"
              else "")
             ^ fname))
    | Some (`Url _url) -> () (* remember url in text file ?? *)
    | _ -> (
        ();
        (* move portrait to saved portraits *)
        match Image.get_portrait conf base p with
        | Some (`Path portrait) ->
            rn portrait (Filename.concat save_dir (Filename.basename portrait))
        | Some (`Url _url) -> () (* ??? *)
        | _ -> ())
  else if content <> "" then (
    if Sys.file_exists full_name then
      rn full_name (Filename.concat save_dir (Filename.basename full_name));
    if Sys.file_exists (Filename.remove_extension full_name ^ ".txt") then
      rn
        (Filename.remove_extension full_name ^ ".txt")
        (Filename.concat save_dir
           (Filename.remove_extension (Filename.basename full_name) ^ ".txt")));

  if content <> "" then write_file full_name content;
  if notes <> Adef.safe "" then
    write_file (Filename.remove_extension full_name ^ ".txt") (notes :> string);

  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "si"
    else if file_name <> "" && notes <> Adef.safe "" then "sb"
    else if file_name <> "" then "so"
    else if notes <> Adef.safe "" then "sc"
    else "sn");
  file_name

(* Delete *)
let print_delete_image conf base p =
  let title h =
    transl_nth conf "image/images" 0
    |> transl_decline conf "delete"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    if not h then (
      let fn = p_first_name base p in
      let sn = p_surname base p in
      let occ = get_occ p in
      Output.print_sstring conf (Util.transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html fn);
      Output.print_sstring conf ".";
      Output.print_sstring conf (string_of_int occ);
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html sn))
  in
  Hutil.header conf title;
  Output.printf conf "<form method=\"post\" action=\"%s\">" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "DEL_IMAGE_OK");
  Util.hidden_input conf "i" (get_iper p |> string_of_iper |> Mutil.encode);
  Output.print_sstring conf
    {|<p><button type="submit" class="btn btn-primary">|};
  transl_nth conf "validate/delete" 1
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></p></form>|};
  Hutil.trailer conf

let print_deleted conf base p =
  let title _ =
    transl conf "image deleted"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let effective_delete_ok conf base p =
  let bfname = Image.default_portrait_filename base p in
  let fname = Filename.concat (Util.base_path [ "images" ] conf.bname) bfname in
  if move_file_to_saved conf fname bfname = 0 then
    incorrect conf "effective delete";
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "di";
  print_deleted conf base p

let print_del_ok conf base =
  match p_getenv conf.env "i" with
  | Some ip ->
      let p = poi base (iper_of_string ip) in
      effective_delete_ok conf base p
  | None -> incorrect conf "print del ok"

let print_del conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip -> (
      let p = poi base (iper_of_string ip) in
      match Image.get_portrait conf base p with
      | Some _src -> print_delete_image conf base p
      | None -> Hutil.incorrect_request conf)

(*carrousel *)
(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_c_ok conf base p =
  let keydir = Image.default_portrait_filename base p in
  let file_name =
    try (List.assoc "file_name" conf.env :> string) with Not_found -> ""
  in
  (*let file_name = Mutil.decode file_name in *)
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let saved =
    try List.assoc "delete" conf.env = Adef.encoded "on"
    with Not_found -> false
  in
  let ext = get_extension conf saved keydir in
  let file = if file_name = "" then keydir ^ ext else file_name in
  let full_dir =
    if mode = "portraits" then Util.base_path [ "images" ] conf.bname
    else
      String.concat Filename.dir_sep
        [ Util.base_path [ "src" ] conf.bname; "images"; keydir ]
  in
  (* TODO verify we dont destroy a saved image
      having the same name as portrait! *)
  if saved then
    Mutil.rm (String.concat Filename.dir_sep [ full_dir; "saved"; file ])
  else if move_file_to_save full_dir file = 0 then
    incorrect conf "effective delete";
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed (if mode = "portraits" then "di" else "do");
  file_name

(* carrousel *)
(* reset portrait or image from old folder to portrait or others *)

let effective_reset_c_ok conf base p =
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let keydir = Image.default_portrait_filename base p in
  let file_name =
    try (List.assoc "file_name" conf.env :> string) with Not_found -> ""
  in
  (if mode = "portraits" then
   let file_name = keydir in
   let ext_saved = get_extension conf true keydir in
   let ext = get_extension conf false keydir in
   let ext = if ext = "." then ext_saved else ext in
   let file_in_old =
     String.concat Filename.dir_sep
       [
         Util.base_path [ "images" ] conf.bname; "saved"; file_name ^ ext_saved;
       ]
   in
   let file_in_portraits =
     String.concat Filename.dir_sep
       [ Util.base_path [ "images" ] conf.bname; file_name ^ ext ]
   in
   if Sys.file_exists file_in_portraits then
     swap_files conf file_in_old file_in_portraits false
   else rename_files file_in_old file_in_portraits false
  else
    let file_name = file_name in
    let file_in_old =
      String.concat Filename.dir_sep
        [
          Util.base_path [ "src" ] conf.bname;
          "images";
          keydir;
          "saved";
          file_name;
        ]
    in
    let file_in_src =
      String.concat Filename.dir_sep
        [ Util.base_path [ "src" ] conf.bname; "images"; keydir; file_name ]
    in
    if Sys.file_exists file_in_src then
      swap_files conf file_in_old file_in_src true
    else rename_files file_in_old file_in_src true);
  file_name

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Gwdb.base -> unit                         *)
(* ************************************************************************** *)

(* most functions in GeneWeb end with a COMMAND_OK confirmation step *)
(* for carrousel, we have chosen to ignore this step and refresh *)
(* the updated page directly *)
(* if em="" this is the first pass, do it *)

let print_main_c conf base =
  match Util.p_getenv conf.env "em" with
  | None -> (
      match Util.p_getenv conf.env "m" with
      | Some m -> (
          let save_m = m in
          match Util.p_getenv conf.env "i" with
          | Some ip -> (
              let p = poi base (Gwdb.iper_of_string ip) in
              let digest = Image.default_portrait_filename base p in
              let conf, report =
                match Util.p_getenv conf.env "m" with
                | Some "SND_IMAGE_C_OK" ->
                    let mode =
                      try (List.assoc "mode" conf.env :> string)
                      with Not_found -> "portraits"
                    in
                    let file_name =
                      try (List.assoc "file_name" conf.env :> string)
                      with Not_found -> ""
                    in
                    let file_name =
                      if file_name = "" then
                        try (List.assoc "file_name_2" conf.env :> string)
                        with Not_found -> ""
                      else file_name
                    in
                    let file_name_2 = Filename.remove_extension file_name in
                    let new_env =
                      List.fold_left
                        (fun accu (k, v) ->
                          if k = "file_name_2" then
                            (k, Adef.encoded file_name_2) :: accu
                          else (k, v) :: accu)
                        [] conf.env
                    in
                    let conf = { conf with env = new_env } in
                    let file =
                      if mode <> "comment" then (raw_get conf "file" :> string)
                      else "file_name"
                    in
                    let idigest =
                      try (List.assoc "idigest" conf.env :> string)
                      with Not_found -> ""
                    in
                    if digest = idigest then
                      (conf, effective_send_c_ok conf base p file file_name mode)
                    else (conf, "idigest error")
                | Some "DEL_IMAGE_C_OK" ->
                    let idigest =
                      try (List.assoc "idigest" conf.env :> string)
                      with Not_found -> ""
                    in
                    if digest = idigest then
                      (conf, effective_delete_c_ok conf base p)
                    else (conf, "idigest error")
                | Some "RESET_IMAGE_C_OK" ->
                    let idigest =
                      try (List.assoc "idigest" conf.env :> string)
                      with Not_found -> ""
                    in
                    Printf.eprintf "Idigest: |%s|\n" idigest;
                    if digest = idigest then
                      (conf, effective_reset_c_ok conf base p)
                    else (conf, "idigest error")
                | Some "IMAGE_C" -> (conf, "image")
                | _ -> (conf, "incorrect request")
              in
              match report with
              | "idigest error" ->
                  failwith
                    (__FILE__ ^ " idigest error, line " ^ string_of_int __LINE__
                      :> string)
              | "incorrect request" -> Hutil.incorrect_request conf
              | _ -> print_confirm_c conf base save_m report)
          | None -> Hutil.incorrect_request conf)
      | None -> Hutil.incorrect_request conf)
  (* em!="" second pass, ignore *)
  | Some _ -> print_confirm_c conf base "REFRESH" ""

let print conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = poi base (iper_of_string ip) in
      let fn = p_first_name base p in
      let sn = p_surname base p in
      if fn = "?" || sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base p

(* carrousel *)
let print_c ?(saved = false) conf base =
  match (Util.p_getenv conf.env "s", Util.find_person_in_env conf base "") with
  | Some f, Some p ->
      let k = Image.default_portrait_filename base p in
      let f = Filename.concat k f in
      ImageDisplay.print_source conf (if saved then insert_saved f else f)
  | Some f, _ -> ImageDisplay.print_source conf f
  | _, Some p -> (
      match
        (if saved then Image.get_old_portrait else Image.get_portrait)
          conf base p
      with
      | Some (`Path f) ->
          Result.fold ~ok:ignore
            ~error:(fun _ -> Hutil.incorrect_request conf)
            (ImageDisplay.print_image_file conf f)
      | _ -> Hutil.incorrect_request conf)
  | _, _ -> Hutil.incorrect_request conf
