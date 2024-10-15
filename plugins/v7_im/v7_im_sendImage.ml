open Geneweb
open Config
open Def
open Gwdb
open Util

type image_type = JPEG | GIF | PNG

let image_types = [ JPEG; GIF; PNG ]

let extension_of_type = function
  | JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let raise_modErr s = raise @@ Update.ModErr (Update.UERR s)

let incorrect conf =
  Hutil.incorrect_request conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe)

let incorrect_content_type conf base p s =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<p>\n";
  Output.print_sstring conf "<em style=\"font-size:smaller\">";
  Output.printf conf "Error: incorrect image content type: %s" s;
  Output.print_sstring conf "</em>\n";
  Output.print_sstring conf "</p>\n";
  Output.print_sstring conf "<ul>\n";
  Output.print_sstring conf "<li>\n";
  Output.print_string conf
    (NameDisplay.referenced_person_title_text conf base p);
  Output.print_sstring conf "</li>\n";
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe)

let error_too_big_image conf base p len max_len =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<p><em style=\"font-size:smaller\">";
  Output.printf conf "Error: this image is too big: %d bytes<br>\n" len;
  Output.printf conf "Maximum authorized in this database: %d bytes<br>\n"
    max_len;
  Output.print_sstring conf "</em></p>\n";
  Output.print_sstring conf "<ul>\n";
  Output.print_sstring conf "<li>\n";
  Output.print_string conf
    (NameDisplay.referenced_person_title_text conf base p);
  Output.print_sstring conf "</li>\n";
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe)

let raw_get conf key =
  try List.assoc key conf.env with Not_found -> incorrect conf

(* print delete image link *)
let print_link_delete_image conf base p =
  if Option.is_some @@ Image.get_portrait conf base p then (
    Output.print_sstring conf {|<p><a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=DEL_IMAGE&i=";
    Output.print_string conf (get_iper p |> string_of_iper |> Mutil.encode);
    Output.print_sstring conf {|">|};
    transl conf "delete" |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf {| |};
    transl_nth conf "image/images" 0 |> Output.print_sstring conf;
    Output.print_sstring conf "</a></p>")

(* Send image form *)

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
      let fn = p_first_name base p in
      let sn = p_surname base p in
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html fn);
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html sn);
      Util.print_reference conf fn (get_occ p) sn)
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
    {| <input type="file" class="form-control" name="file" size="50" maxlength="250" accept="image/*"></p>|};
  (match
     Option.map int_of_string @@ List.assoc_opt "max_images_size" conf.base_env
   with
  | Some len ->
      Output.print_sstring conf "<p>(maximum authorized size = ";
      Output.print_sstring conf (string_of_int len);
      Output.print_sstring conf " bytes)</p>"
  | None -> ());
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-secondary btn-lg mt-2">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></form>";
  print_link_delete_image conf base p;
  Hutil.trailer conf

let print conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = poi base (iper_of_string ip) in
      let fn = p_first_name base p in
      let sn = p_surname base p in
      if fn = "?" || sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base p

(* Delete image form *)

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
    {|<p><button type="submit" class="btn btn-secondary btn-lg">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></p></form>|};
  Hutil.trailer conf

let print_del conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip -> (
      let p = poi base (iper_of_string ip) in
      match Image.get_portrait conf base p with
      | Some _src -> print_delete_image conf base p
      | None -> Hutil.incorrect_request conf)

(* Send image form validated *)

let print_sent conf base p =
  let title _ =
    transl conf "image received"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (NameDisplay.referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content;
  flush oc;
  close_out oc

(* Move fname to old_dir if it exists with some extension.
   Returns the number of moved files *)
let move_file_to_old conf fname bfname =
  List.fold_left
    (fun cnt typ ->
      let ext = extension_of_type typ in
      let new_file = fname ^ ext in
      if Sys.file_exists new_file then (
        let old_dir =
          Filename.concat (Util.base_path [ "images" ] conf.bname) "old"
        in
        let old_file = Filename.concat old_dir bfname ^ ext in
        Files.rm old_file;
        Files.mkdir_p old_dir;
        (try Unix.rename new_file old_file
         with Unix.Unix_error (_, _, _) -> ());
        cnt + 1)
      else cnt)
    0 image_types

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
  if List.assoc_opt "dump_bad_images" conf.base_env = Some "yes" then
    try
      let oc = Secure.open_out_bin "bad-image" in
      output_string oc s;
      flush oc;
      close_out oc
    with Sys_error _ -> ()

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
  let _moved = move_file_to_old conf fname bfname in
  write_file (fname ^ extension_of_type typ) content;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "si";
  print_sent conf base p

let print_send_ok conf base =
  let ip =
    try raw_get conf "i" |> Mutil.decode |> iper_of_string
    with Failure _ -> incorrect conf
  in
  let p = poi base ip in
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  if (digest :> string) = Mutil.decode (raw_get conf "digest") then
    raw_get conf "file" |> Adef.as_string |> effective_send_ok conf base p
  else Update.error_digest conf

(* Delete image form validated *)

let print_deleted conf base p =
  let title _ =
    transl conf "image deleted"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (NameDisplay.referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let effective_delete_ok conf base p =
  let bfname = Image.default_portrait_filename base p in
  let fname = Filename.concat (Util.base_path [ "images" ] conf.bname) bfname in
  if move_file_to_old conf fname bfname = 0 then incorrect conf;
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
  | None -> incorrect conf
