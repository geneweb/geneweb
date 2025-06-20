open Config
open Def
open Util
module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

let cp = Filesystem.copy_file ~perm:0o666

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
  Hutil.header conf title;
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
  Hutil.header ~error:true conf title;
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
  let l = List.rev @@ match l with h :: t -> h :: "saved" :: t | _ -> l in
  String.concat Filename.dir_sep l

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content;
  flush oc;
  close_out oc

let move_file_to_save dir file =
  try
    let save_dir = Filename.concat dir "saved" in
    Filesystem.create_dir ~parent:true save_dir;
    let fname = Filename.basename file in
    let orig_file = Filename.concat dir fname in
    if not (Sys.file_exists orig_file) then
      raise (Sys_error (Printf.sprintf "File not found: %s" orig_file));
    let saved_file = Filename.concat save_dir fname in
    if Sys.file_exists saved_file then Mutil.rm saved_file;
    rn orig_file saved_file;
    let extensions = [ ".txt"; ".src" ] in
    List.iter
      (fun ext ->
        let orig_ext = Filename.remove_extension orig_file ^ ext in
        let saved_ext = Filename.remove_extension saved_file ^ ext in
        if Sys.file_exists orig_ext then rn orig_ext saved_ext)
      extensions;
    1
  with
  | Sys_error e ->
      Logs.syslog `LOG_ERR (Printf.sprintf "Error moving file to saved: %s" e);
      0
  | _ -> 0

let create_blason_stop conf base p =
  let blason_dir = !GWPARAM.portraits_d conf.bname in
  let blason_stop =
    String.concat Filename.dir_sep
      [ blason_dir; Image.default_image_filename "blasons" base p ^ ".stop" ]
  in
  let oc = open_out blason_stop in
  close_out oc;
  blason_stop

let move_blason_file conf base src dst =
  let blason_dir = !GWPARAM.portraits_d conf.bname in
  let blason_src =
    String.concat Filename.dir_sep
      [ blason_dir; Image.get_blason_name conf base src ]
  in
  if
    Image.has_blason conf base src true
    && Sys.file_exists blason_src
    && not (Image.has_blason conf base dst true)
  then (
    let blason_dst =
      String.concat Filename.dir_sep
        [
          blason_dir;
          Image.default_image_filename "blasons" base dst
          ^ Filename.extension blason_src;
        ]
    in
    rn blason_src blason_dst;
    blason_dst)
  else ""

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

let extract_filename_from_url url =
  try
    let without_protocol =
      match string_search url "://" with
      | Some pos -> String.sub url (pos + 3) (String.length url - pos - 3)
      | None -> url
    in
    let without_params =
      match String.index_opt without_protocol '?' with
      | Some pos -> String.sub without_protocol 0 pos
      | None -> without_protocol
    in
    let filename = Filename.basename without_params in
    if filename = "" || filename = "/" || not (String.contains filename '.')
    then "image_" ^ string_of_int (int_of_float (Unix.time ()))
    else filename
  with _ -> "image_" ^ string_of_int (int_of_float (Unix.time ()))

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
        (* Where will "bad-image"end up? *)
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s;
        flush oc;
        close_out oc
      with Sys_error _ -> ())
  | _ -> ()

(* swap files between new and old folder *)
(* [| ".jpg"; ".jpeg"; ".png"; ".gif" |] *)

let swap_files_aux dir file old_file =
  let ext = Filename.extension file in
  let old_ext = Filename.extension old_file in
  let tmp_file = String.concat Filename.dir_sep [ dir; "tempfile.tmp" ] in
  if ext <> old_ext then (
    rn file (Filename.remove_extension old_file ^ ext);
    rn old_file (Filename.remove_extension file ^ old_ext))
  else (
    rn file tmp_file;
    rn old_file file;
    rn tmp_file old_file)

let swap_files file old_file =
  let dir = Filename.dirname file in
  swap_files_aux dir file old_file;
  let txt_file = Filename.remove_extension file ^ ".txt" in
  let old_file = Filename.remove_extension old_file ^ ".txt" in
  swap_files_aux dir txt_file old_file;
  let src_file = Filename.remove_extension file ^ ".src" in
  let old_file = Filename.remove_extension old_file ^ ".src" in
  swap_files_aux dir src_file old_file

let clean_saved_portrait file =
  let file = Filename.remove_extension file in
  Array.iter (fun ext -> Mutil.rm (file ^ ext)) Image.ext_list_1

(* TODO merge with Image.file_without_extension *)
let get_extension conf keydir mode saved fname =
  let t_ext = Filename.extension fname in
  let fname =
    if Array.mem t_ext Image.ext_list_2 then Filename.remove_extension fname
    else fname
  in
  let dir =
    match mode with
    | "portraits" | "blasons" -> !GWPARAM.portraits_d conf.bname
    | _ -> Filename.concat (!GWPARAM.images_d conf.bname) keydir
  in
  let f =
    if saved then String.concat Filename.dir_sep [ dir; "saved"; fname ]
    else Filename.concat dir fname
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".jpeg") then ".jpeg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else if Sys.file_exists (f ^ ".url") then ".url"
  else if Sys.file_exists (f ^ ".stop") then ".stop"
  else "."

(* ************************************************************************ *)
(*  send, delete, reset and print functions                                 *)
(*                                                                          *)
(* ************************************************************************ *)

(* we need print_link_delete_image in the send function *)
let print_link_delete_image conf base p =
  if Option.is_some @@ Image.get_portrait conf base p then (
    Output.print_sstring conf {|<div><a class="btn btn-danger mt-3" href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=DEL_IMAGE&i=";
    Output.print_string conf
      (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
    Output.print_sstring conf {|">|};
    transl conf "delete" |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf {| |};
    transl_nth conf "image/images" 0 |> Output.print_sstring conf;
    Output.print_sstring conf "</a></div>")

let print_send_image conf base mode p =
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
      Output.print_string conf (Util.escape_html (Driver.p_first_name base p));
      Output.print_sstring conf (Format.sprintf ".%d " (Driver.get_occ p));
      Output.print_string conf (Util.escape_html (Driver.p_surname base p)))
  in
  Hutil.header conf title;
  Output.printf conf
    "<form method=\"post\" action=\"%s\" enctype=\"multipart/form-data\">\n"
    conf.command;
  Output.print_sstring conf
    "<div class=\"d-inline-flex align-items-center mt-2\">\n";
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "SND_IMAGE_C_OK");
  Util.hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  Util.hidden_input conf "mode" (Adef.encoded mode);
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "file"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf
    {|<input type="file" class="form-control-file ml-1" name="file" accept="image/*">|};
  (match
     Option.map int_of_string @@ List.assoc_opt "max_images_size" conf.base_env
   with
  | Some len ->
      Output.print_sstring conf "<p>(maximum authorized size = ";
      Output.print_sstring conf (string_of_int len);
      Output.print_sstring conf " bytes)</p>"
  | None -> ());
  Output.print_sstring conf
    {|<span>></span><button type="submit" class="btn btn-primary ml-3">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></div></form>";
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
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
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
  let fname = Image.default_image_filename mode base p in
  let dir =
    if mode = "portraits" || mode = "blasons" then
      !GWPARAM.portraits_d conf.bname
    else !GWPARAM.images_d conf.bname
  in
  Filesystem.create_dir ~parent:true dir;
  let fname =
    Filename.concat dir
      (if mode = "portraits" || mode = "blasons" then
         fname ^ extension_of_type typ
       else fname)
  in
  let _moved = move_file_to_save dir fname in
  write_file fname content;
  let changed =
    U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed "si";
  print_sent conf base p

let print_send_ok conf base =
  let ip =
    try raw_get conf "i" |> Mutil.decode |> Driver.Iper.of_string
    with Failure _ -> incorrect conf "print send ok"
  in
  let p = Driver.poi base ip in
  raw_get conf "file" |> Adef.as_string |> effective_send_ok conf base p

(* carrousel *)
let effective_send_c_ok conf base p file file_name =
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let image_url =
    try (List.assoc "image_url" conf.env :> string) with Not_found -> ""
  in
  let image_name =
    let user_provided_name =
      try (List.assoc "image_name" conf.env :> string) with Not_found -> ""
    in
    if user_provided_name = "" && image_url <> "" then
      extract_filename_from_url image_url
    else if user_provided_name = "" then ""
    else user_provided_name
  in
  let note =
    match Util.p_getenv conf.env "note" with
    | Some v ->
        Util.safe_html
          (Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> Adef.safe ""
  in
  let source =
    match Util.p_getenv conf.env "source" with
    | Some v ->
        Util.safe_html
          (Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> Adef.safe ""
  in
  let strm = Stream.of_string file in
  let request, content = Wserver.get_request_and_content strm in
  let content =
    if mode = "note" || mode = "source" || image_url <> "" then ""
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
  let keydir = Image.default_image_filename mode base p in
  let dir =
    if mode = "portraits" || mode = "blasons" then
      !GWPARAM.portraits_d conf.bname
    else Filename.concat (!GWPARAM.images_d conf.bname) keydir
  in
  Filesystem.create_dir ~parent:true dir;
  let fname =
    Filename.concat dir
      (if mode = "portraits" || mode = "blasons" then
         keydir ^ extension_of_type typ
       else file_name)
  in
  (* move pre-existing file to saved *)
  if mode = "portraits" || mode = "blasons" then
    match
      if mode = "portraits" then Image.get_portrait conf base p
      else Image.get_blason conf base p true
    with
    | Some (`Path portrait) ->
        if move_file_to_save dir portrait = 0 then
          incorrect conf "effective send (portrait/blason)"
    | Some (`Url url) -> (
        let fname =
          if mode = "portraits" then
            Image.default_image_filename "portraits" base p
          else Image.default_image_filename "blasons" base p
        in
        let dir = Filename.concat dir "saved" in
        Filesystem.create_dir ~parent:true dir;
        let fname = Filename.concat dir fname ^ ".url" in
        try write_file fname url
        with _ ->
          incorrect conf
            (Printf.sprintf "effective send (effective send url portrait %s)"
               fname)
        (* TODO update person to supress url image *))
    | _ -> ()
  else if content <> "" then
    if Sys.file_exists fname then
      if move_file_to_save dir fname = 0 then
        incorrect conf "effective send (image)";
  let fname =
    if image_url <> "" then Filename.concat dir image_name ^ ".url" else fname
  in
  if content <> "" then
    try write_file fname content
    with _ ->
      incorrect conf
        (Printf.sprintf "effective send (writing content file %s)" fname)
  else if image_url <> "" then
    try write_file fname image_url
    with _ ->
      incorrect conf
        (Printf.sprintf "effective send (writing .url file %s)" fname)
  else ();
  if note <> Adef.safe "" then
    let fname = Filename.remove_extension fname ^ ".txt" in
    try write_file fname (note :> string)
    with _ ->
      incorrect conf
        (Printf.sprintf "effective send (writing .txt file %s)" fname)
  else ();
  if source <> Adef.safe "" then
    let fname = Filename.remove_extension fname ^ ".src" in
    try write_file fname (source :> string)
    with _ ->
      incorrect conf
        (Printf.sprintf "effective send (writing .txt file %s)" fname)
  else ();
  let changed =
    U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed
    (match mode with
    | "portraits" -> "sp"
    | "blasons" -> "sb"
    | "carrousel" ->
        if file_name <> "" && note <> Adef.safe "" && source <> Adef.safe ""
        then "s3"
        else if file_name <> "" then "sf"
        else if note <> Adef.safe "" then "so"
        else if source <> Adef.safe "" then "ss"
        else "sx"
    | "note" -> "so"
    | "source" -> "ss"
    | _ -> "s?");
  file_name

(* Delete *)
let print_delete_image conf base p =
  let title h =
    transl_nth conf "image/images" 0
    |> transl_decline conf "delete"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    if not h then (
      let fn = Driver.p_first_name base p in
      let sn = Driver.p_surname base p in
      let occ = Driver.get_occ p in
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
  Util.hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  Output.print_sstring conf
    {|<div class="mt-3"><button type="submit" class="btn btn-danger">|};
  transl_nth conf "validate/delete" 1
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></div></form>|};
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
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let fname = Image.default_image_filename "portraits" base p in
  let ext = get_extension conf fname mode false fname in
  let dir = !GWPARAM.portraits_d conf.bname in
  if move_file_to_save (fname ^ ext) dir = 0 then
    incorrect conf "effective delete (ok)";
  let changed =
    U_Delete_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed "di";
  print_deleted conf base p

let print_del_ok conf base =
  match p_getenv conf.env "i" with
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      effective_delete_ok conf base p
  | None -> incorrect conf "print del ok"

let print_del conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip -> (
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      match Image.get_portrait conf base p with
      | Some _ -> print_delete_image conf base p
      | None -> Hutil.incorrect_request conf)

(*carrousel *)
(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_c_ok conf base ?(f_name = "") p =
  let keydir = Image.default_image_filename "portraits" base p in
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let delete =
    try List.assoc "delete" conf.env = Adef.encoded "on"
    with Not_found -> false
  in
  let fname =
    if f_name = "" then
      try List.assoc "file_name" conf.env |> Mutil.decode with Not_found -> ""
    else f_name
  in
  let dir =
    if mode = "portraits" || mode = "blasons" then
      !GWPARAM.portraits_d conf.bname
    else Filename.concat (!GWPARAM.images_d conf.bname) keydir
  in
  Filesystem.create_dir ~parent:true dir;
  (* TODO verify we dont destroy a saved image
      having the same name as portrait! *)
  let orig_file =
    if delete then String.concat Filename.dir_sep [ dir; "saved"; fname ]
    else Filename.concat dir fname
  in
  let orig_file =
    if Filename.extension orig_file = ".url" && Sys.file_exists orig_file then
      orig_file
    else Filename.remove_extension orig_file |> Image.find_file_without_ext
  in
  (* FIXME basename *)
  let file = Filename.basename orig_file in
  if orig_file = "" then incorrect conf "empty file name"
    (* if delete is on, we are talking about saved files *)
  else if delete then Mutil.rm orig_file (* is it needed ?, move should do it *)
  else if move_file_to_save dir file = 0 then
    incorrect conf "effective delete (c_ok)";
  let changed =
    U_Delete_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed
    (match mode with
    | "portraits" -> "dq"
    | "blasons" -> "db"
    | "carrousel" -> "dc"
    | _ -> "d?");
  fname

let effective_copy_portrait_to_blason conf base p =
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let keydir = Image.default_image_filename "portraits" base p in
  let create_url_file keydir url =
    let fname =
      Filename.concat (!GWPARAM.images_d conf.bname) (keydir ^ ".url")
    in
    let oc = Secure.open_out_bin fname in
    output_string oc url;
    flush oc;
    close_out oc;
    fname
  in
  let dir = !GWPARAM.portraits_d conf.bname in
  let fname, url =
    match Image.src_of_string conf (Driver.sou base (Driver.get_image p)) with
    | `Url u -> (create_url_file keydir u, true)
    | _ -> (Image.default_image_filename "portraits" base p, false)
  in
  let ext =
    if url then ".url" else get_extension conf keydir mode false fname
  in
  let portrait_filename =
    String.concat Filename.dir_sep [ dir; keydir ^ ext ]
  in
  let blason_filename =
    String.concat Filename.dir_sep
      [ dir; Image.default_image_filename "blasons" base p ^ ext ]
  in
  let has_blason_self = Image.has_blason conf base p true in
  (* attention, xxx.url has to be removed too *)
  let _deleted =
    (if has_blason_self then
       effective_delete_c_ok conf base
         ~f_name:(Filename.basename blason_filename)
         p
     else "OK")
    <> ""
  in
  cp portrait_filename blason_filename;
  History.record conf base
    (U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p)))
    "cb";
  blason_filename

let effective_copy_image_to_blason conf base p =
  let fname =
    try List.assoc "file_name" conf.env |> Mutil.decode with Not_found -> ""
  in
  let fname = Mutil.decode (Adef.encoded fname) in
  let keydir = Image.default_image_filename "portraits" base p in
  let ext = Filename.extension fname in
  let blason_filename =
    String.concat Filename.dir_sep
      [
        Image.portrait_folder conf;
        Image.default_image_filename "blasons" base p ^ ext;
      ]
  in
  let has_blason_self = Image.has_blason conf base p true in
  let _deleted =
    (if has_blason_self then
       effective_delete_c_ok conf base
         ~f_name:(Filename.basename blason_filename)
         p
     else "OK")
    <> ""
  in
  let fname =
    String.concat Filename.dir_sep
      [ Image.carrousel_folder conf; keydir; fname ]
  in
  cp fname blason_filename;
  History.record conf base
    (U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p)))
    "cd";
  blason_filename

(* reset portrait or image from old folder to portrait or others *)

let effective_reset_c_ok conf base p =
  (* WARNING: when saved portrait is an url, we should update the person record
     when doing a reset  idem for delete *)
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let keydir = Image.default_image_filename "portraits" base p in
  let file_name =
    match mode with
    | "portraits" -> Image.default_image_filename "portraits" base p
    | "blasons" -> Image.default_image_filename "blasons" base p
    | "carrousel" -> (
        try List.assoc "file_name" conf.env |> Mutil.decode
        with Not_found -> "")
    | _ -> ""
  in
  let ext = get_extension conf keydir mode false file_name in
  let old_ext = get_extension conf keydir mode true file_name in
  let t_ext = Filename.extension file_name in
  let file_name_no_ext =
    if Array.mem t_ext Image.ext_list_2 then Filename.remove_extension file_name
    else file_name
  in
  let ext =
    match Image.get_portrait conf base p with
    | Some src -> if Image.is_url (Image.src_to_string src) then ".url" else ext
    | _ -> ext
  in
  let dir =
    if mode = "portraits" || mode = "blasons" then
      !GWPARAM.portraits_d conf.bname
    else Filename.concat (!GWPARAM.images_d conf.bname) keydir
  in
  let file_in_new =
    if ext <> "." then Filename.concat dir (file_name_no_ext ^ ext)
    else Filename.concat dir (file_name_no_ext ^ old_ext)
  in
  let file_in_old =
    if old_ext <> "." then
      String.concat Filename.dir_sep
        [ dir; "saved"; file_name_no_ext ^ old_ext ]
    else String.concat Filename.dir_sep [ dir; "saved"; file_name_no_ext ^ ext ]
  in
  swap_files file_in_new file_in_old;
  let changed =
    U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed
    (match mode with
    | "portraits" -> "rp"
    | "blasons" -> "rb"
    | "carrousel" -> "rc"
    | _ -> "r?");
  file_name

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Geneweb_db.Driver.base -> unit                         *)
(* ************************************************************************** *)

(* Carrousel image management with direct HTTP redirects:
   - Process POST action immediately
   - Redirect to main page with success message in URL parameters
   - Single, clean request cycle *)

let print_main_c conf base =
  match Util.p_getenv conf.env "em" with
  | None -> (
      (* Process action and redirect with success message *)
      match Util.p_getenv conf.env "m" with
      | Some m -> (
          match Util.p_getenv conf.env "i" with
          | Some ip -> (
              let p = Driver.poi base (Driver.Iper.of_string ip) in
              let processed_filename =
                match m with
                | "SND_IMAGE_C_OK" ->
                    let mode =
                      try (List.assoc "mode" conf.env :> string)
                      with Not_found -> "portraits"
                    in
                    let file_name =
                      try List.assoc "file_name" conf.env |> Mutil.decode
                      with Not_found -> ""
                    in
                    let file_name =
                      if file_name = "" then
                        try List.assoc "file_name_2" conf.env |> Mutil.decode
                        with Not_found -> ""
                      else file_name
                    in
                    let file_name =
                      (Mutil.decode (Adef.encoded file_name) :> string)
                    in
                    let file =
                      if mode = "note" || mode = "source" then "file_name"
                      else
                        match Util.p_getenv conf.env "image_url" with
                        | Some url when url <> "" -> ""
                        | _ -> (
                            try (raw_get conf "file" :> string) with _ -> "")
                    in
                    effective_send_c_ok conf base p file file_name
                | "DEL_IMAGE_C_OK" -> effective_delete_c_ok conf base p
                | "RESET_IMAGE_C_OK" -> effective_reset_c_ok conf base p
                | "BLASON_MOVE_TO_ANC" ->
                    if Image.has_blason conf base p true then
                      match Util.p_getenv conf.env "ia" with
                      | Some ia ->
                          let fa = Driver.poi base (Driver.Iper.of_string ia) in
                          Filename.basename (move_blason_file conf base p fa)
                      | None -> ""
                    else ""
                | "PORTRAIT_TO_BLASON" ->
                    Filename.basename
                      (effective_copy_portrait_to_blason conf base p)
                | "IMAGE_TO_BLASON" ->
                    Filename.basename
                      (effective_copy_image_to_blason conf base p)
                | "BLASON_STOP" ->
                    let has_blason_self = Image.has_blason conf base p true in
                    let has_blason = Image.has_blason conf base p false in
                    if has_blason && not has_blason_self then
                      Filename.basename (create_blason_stop conf base p)
                    else "error"
                | _ -> "incorrect request"
              in

              (* HTTP redirect to main page with success message *)
              match processed_filename with
              | "error" -> Hutil.incorrect_request conf
              | "incorrect request" ->
                  Hutil.incorrect_request conf ~comment:"incorrect request"
              | _ ->
                  let mode =
                    try (List.assoc "mode" conf.env :> string)
                    with Not_found -> "portraits"
                  in
                  let display_filename =
                    if Filename.extension processed_filename = "" then
                      (* Pas d'extension, essayer de la récupérer *)
                      let keydir =
                        Image.default_image_filename "portraits" base p
                      in
                      let ext =
                        get_extension conf keydir mode false processed_filename
                      in
                      if ext <> "." then processed_filename ^ ext
                      else processed_filename
                    else processed_filename
                  in
                  let base_url =
                    Printf.sprintf
                      "%sm=SND_IMAGE_C&i=%s&em=%s&file_name=%s&mode=%s"
                      (commd conf :> string)
                      ip
                      (Mutil.encode m :> string)
                      (Mutil.encode display_filename :> string)
                      (Mutil.encode mode :> string)
                  in
                  let url_params = ref [] in
                  (try
                     if List.assoc "delete" conf.env = Adef.encoded "on" then
                       url_params := ("delete", "on") :: !url_params
                   with Not_found -> ());
                  (try
                     let fn2 =
                       List.assoc "file_name_2" conf.env |> Mutil.decode
                     in
                     if fn2 <> "" then
                       url_params := ("file_name_2", fn2) :: !url_params
                   with Not_found -> ());
                  (if m = "IMAGE_TO_BLASON" then
                     let ext = Filename.extension processed_filename in
                     if ext <> "" then url_params := ("ext", ext) :: !url_params);
                  let params_string =
                    List.fold_left
                      (fun acc (key, value) ->
                        acc ^ "&" ^ key ^ "=" ^ (Mutil.encode value :> string))
                      "" !url_params
                  in
                  let redirect_url = base_url ^ params_string in
                  Output.status conf Def.Moved_Temporarily;
                  Output.header conf "Location: %s" redirect_url;
                  Output.flush conf)
          | None -> Hutil.incorrect_request conf ~comment:"missing person index"
          )
      | None -> Hutil.incorrect_request conf ~comment:"missing action")
  (* Display main page with success message from URL params *)
  | Some _ ->
      let p =
        match Util.p_getint conf.env "i" with
        | Some ip -> Driver.poi base (Driver.Iper.of_string (string_of_int ip))
        | None -> failwith "No person index in success display"
      in
      Perso.interp_templ "carrousel" conf base p

let print conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      let fn = Driver.p_first_name base p in
      let sn = Driver.p_surname base p in
      if fn = "?" || sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base "portraist" p

let print_family conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      let sn = Driver.p_surname base p in
      if sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base "blasons" p

(* carrousel *)
let print_c ?(saved = false) ?(portrait = true) conf base =
  let mode = if portrait then "portraits" else "blasons" in
  match (Util.p_getenv conf.env "s", Util.find_person_in_env conf base "") with
  | Some f, Some p ->
      let k = Image.default_image_filename "portraits" base p in
      let f = Filename.concat k f in
      ImageDisplay.print_source conf (if saved then insert_saved f else f)
  | Some f, _ -> ImageDisplay.print_source conf f
  | _, Some p -> (
      match
        if saved then Image.get_old_portrait_or_blason conf base "portraits" p
        else if mode = "portraits" then Image.get_portrait conf base p
        else Image.get_blason conf base p false
      with
      | Some (`Path f) ->
          Result.fold ~ok:ignore
            ~error:(fun _ -> Hutil.incorrect_request conf)
            (ImageDisplay.print_image_file conf f)
      | _ -> Hutil.incorrect_request conf)
  | _, _ -> Hutil.incorrect_request conf
