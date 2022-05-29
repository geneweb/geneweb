open Geneweb
open Config
open Def
open Gwdb
open Util
open TemplAst

(* Mutil ************************************ *)
let rn fname s =
  try if Sys.file_exists fname then Sys.rename fname s
  with Failure _ ->
    begin
      Printf.eprintf "Rn failed: %s to %s\n" fname s; flush stderr
    end
(* ********************************** *)

type image_type = JPEG | GIF | PNG

let extension_of_type =
  function
    JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let raise_modErr s =
  raise @@ Update.ModErr (Update.UERR s)

let incorrect conf =
  Hutil.incorrect_request conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__)

let incorrect_content_type conf base p s =
  let title _ = Output.print_string conf (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_string conf "<p>\n<em style=\"font-size:smaller\">";
  Output.printf conf "Error: incorrect image content type: %s" s;
  Output.printf conf "</em>\n</p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p);
  Hutil.trailer conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__)

let error_too_big_image conf base p len max_len =
  let title _ = Output.print_string conf (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_string conf "<p><em style=\"font-size:smaller\">";
  Output.printf conf "Error: this image is too big: %d bytes<br>\n" len;
  Output.printf conf "Maximum authorized in this database: %d bytes<br>\n" max_len;
  Output.printf conf "</em></p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p);
  Hutil.trailer conf;
  raise_modErr (__FILE__ ^ " " ^ string_of_int __LINE__)

let raw_get conf key =
  try List.assoc key conf.env with Not_found -> incorrect conf

(* Send image form validated *)

let print_sent conf base p =
  let title _ =
    Output.print_string conf (Utf8.capitalize (transl conf "image received"))
  in
  Hutil.header conf title;
  Output.printf conf "<ul><li>\n%s\n</li></ul>\n"
    (referenced_person_text conf base p);
  Hutil.trailer conf

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content; flush oc; close_out oc

let move_file_to_save dir file  =
  try
    begin
      let save_dir = Filename.concat dir "saved" in
      let fname = Filename.basename file in
      if not (Sys.file_exists save_dir) then Mutil.mkdir_p save_dir;
      let orig_file = Filename.concat dir fname in
      let saved_file = Filename.concat save_dir fname in
      (* TODO handle rn errors *)
      rn orig_file saved_file;
      let orig_file_t = (Filename.remove_extension orig_file) ^ ".txt" in
      let saved_file_t = (Filename.remove_extension saved_file) ^ ".txt" in
      if Sys.file_exists orig_file_t then
        rn orig_file_t saved_file_t;
      1
    end
  with _ -> 0

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then
    Some JPEG
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
    Some t -> Some (t, s)
  | None ->
      match string_search s "JFIF" with
        Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ ->
          match string_search s "\137PNG" with
            Some i ->
              let s = String.sub s i (String.length s - i) in Some (PNG, s)
          | _ ->
              match string_search s "GIF8" with
                Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None

let dump_bad_image conf s =
  match p_getenv conf.base_env "dump_bad_images" with
    Some "yes" ->
      begin try
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s; flush oc; close_out oc
      with Sys_error _ -> ()
      end
  | _ -> ()


(* ************************************************************************ *)
(* Code for carousel                                                        *)

let clean_saved_portrait dir bfname =
  let file = Filename.remove_extension
    (String.concat Filename.dir_sep [dir; bfname])
  in
  Mutil.rm (file ^ ".jpg") ;
  Mutil.rm (file ^ ".png") ;
  Mutil.rm (file ^ ".gif")

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_extension conf saved keydir =
  let f =
    if saved then
      String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); "saved"; keydir]
    else String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let print_confirm_c conf base save_m report =
  match p_getint conf.env "i" with
  | Some ip ->
      let p = poi base (Gwdb.iper_of_string (string_of_int ip)) in
      let digest = default_image_name base p in
      let new_env =
        List.fold_left
          (fun accu (k, v) ->
             if k = "m" then ("m", "REFRESH") :: accu
             else if k = "idigest" || k = "" then accu
             else (k, v) :: accu)
          [] conf.env
      in
      let new_env =
        if save_m = "REFRESH" then new_env
        else ("em", save_m) :: new_env
      in
      let new_env = ("idigest", digest) :: new_env in
      let new_env = ("report", report) :: new_env in
      let conf = { (conf) with env = new_env } in
      Perso.interp_templ "carrousel" conf base p
  | None ->
      Hutil.incorrect_request conf

(* ************************************************************************ *)
(*  send, delete and reset functions                                        *)
(*                                                                          *)
(* ************************************************************************ *)

let effective_send_c_ok conf base p file file_name mode =
  let notes = match Util.p_getenv conf.env "notes" with
    Some v ->
      Util.safe_html
        (only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> ""
  in
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    if mode = "comment" then ""
    else
      let s =
        let rec loop len (strm__ : _ Stream.t) =
          match Stream.peek strm__ with
          | Some x -> Stream.junk strm__; loop (Buff.store len x) strm
          | _ -> Buff.get len
        in
        loop 0 strm
      in
      content ^ s
  in
  let (typ, content) =
    if content <> "" then
      match image_type content with
      | None ->
          let ct = Mutil.extract_param "Content-Type: " '\n' request in
          dump_bad_image conf content; incorrect_content_type conf base p ct
      | Some (typ, content) ->
          match p_getint conf.base_env "max_images_size" with
          | Some len when String.length content > len ->
              error_too_big_image conf base p (String.length content) len
          | _ -> typ, content
    else GIF, content (* we dont care which type, content = "" *)
  in
  let keydir = default_image_name base p in
  let full_dir =
    if mode = "portraits" then
      String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname);]
    else
      String.concat Filename.dir_sep
        [(Util.base_path ["src"] conf.bname); "images"; keydir]
  in
  let _ =
    if not (Sys.file_exists full_dir) then
      let d1 = String.concat
        Filename.dir_sep [(Util.base_path ["images"] conf.bname);]
      in
      let d2 = String.concat
        Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images"; keydir]
      in
      Mutil.mkdir_p d1;
      Mutil.mkdir_p d2;
  in
  let full_name = Filename.concat full_dir
     (if mode = "portraits" then
      keydir ^ (extension_of_type typ)
     else file_name)
  in
  if mode = "portraits" then
    begin match Util.auto_image_file conf base p with
    | Some f ->
        let dir = Util.base_path ["images"] conf.bname in
        let save_dir = Filename.concat dir "saved" in
        let fname = Filename.basename f in
        clean_saved_portrait save_dir fname;
        if (move_file_to_save dir fname) = 0 then incorrect conf;
    | None -> ()
    end
  else
    begin
    if (Sys.file_exists full_name) && content <> "" then
      let dir = String.concat Filename.dir_sep
        [(Util.base_path ["src"] conf.bname); "images"; keydir]
      in (* attention au full name *)
      if (move_file_to_save dir full_name) = 0 then incorrect conf
    end;
  if content <> "" then write_file full_name content;
  if notes <> "" then
    write_file ((Filename.remove_extension full_name) ^ ".txt") notes;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "si" else
      if file_name <> "" && notes <> "" then "sb"
      else if file_name <> "" then "so"
      else if notes <> "" then "sc" else "sn");
  file_name

(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_c_ok conf base p =
  let keydir = default_image_name base p in
  let file_name = try List.assoc "file_name" conf.env with Not_found -> "" in
  let file_name = Mutil.decode file_name in
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let saved =
    try (List.assoc "delete" conf.env = "on") with Not_found -> false
  in
  let ext = get_extension conf saved keydir in
  let file = if file_name = "" then keydir ^ ext else file_name in
  let full_dir =
    if mode = "portraits" then (Util.base_path ["images"] conf.bname)
    else String.concat Filename.dir_sep
      [(Util.base_path ["src"] conf.bname); "images"; keydir]
  in
    (* TODO verify we dont destroy a saved image
        having the same name as portrait! *)
  if saved then Mutil.rm
    (String.concat Filename.dir_sep [full_dir; "saved"; file])
  else
    if (move_file_to_save full_dir file) = 0 then incorrect conf;
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed (if mode = "portraits" then "di" else "do");
  file_name

(* reset portrait or image from old folder to portrait or others *)

let swap_files conf file1 file2 txt =
  let tmp_file =
    String.concat Filename.dir_sep
      [(Util.base_path ["images"] conf.bname); "tempfile.tmp"]
  in
  let ext_1 = Filename.extension file1 in
  let ext_2 = Filename.extension file2 in
  rn file1 tmp_file;
  rn file2 ((Filename.remove_extension file1) ^ ext_2);
  rn tmp_file ((Filename.remove_extension file2) ^ ext_1);
  if txt then
  begin
    let tmp_file_t =
      String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); "tempfile.tmp"]
    in
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    rn file1_t tmp_file_t;
    rn file2_t file1_t;
    rn tmp_file_t file2_t
   end

let rename_files file1 file2 txt =
  rn file1 file2;
  if txt then
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    rn file1_t file2_t

let effective_reset_c_ok conf base p =
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let keydir = default_image_name base p in
  let file_name =
    try List.assoc "file_name" conf.env with Not_found -> ""
  in
  if mode = "portraits" then
    begin
      let file_name = keydir in
      let ext_saved = get_extension conf true keydir in
      let ext = get_extension conf false keydir in
      let ext = if ext = "." then ext_saved else ext in
      let file_in_old =
        String.concat Filename.dir_sep
          [(Util.base_path ["images"] conf.bname); "saved";
            (file_name ^ ext_saved)]
      in
      let file_in_portraits =
        String.concat Filename.dir_sep
          [(Util.base_path ["images"] conf.bname);
            (file_name ^ ext)]
      in
      if Sys.file_exists file_in_portraits then
        swap_files conf file_in_old file_in_portraits false
      else
        rename_files file_in_old file_in_portraits false
    end
  else
    begin
      let file_name = Mutil.decode file_name in
      let file_in_old =
        String.concat Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images";
            keydir; "saved"; file_name]
      in
      let file_in_src =
        String.concat Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images"; keydir; file_name]
      in
      if Sys.file_exists file_in_src then
        swap_files conf file_in_old file_in_src true
      else
        rename_files file_in_old file_in_src true
    end;
  file_name

let print_c conf base =
  (* most functions in GeneWeb end with a COMMAND_OK confirmation step *)
  (* here we have chosen to ignore this step and refresh the updated page directly *)
  (* if em="" this is the first pass, do it *)
  begin match p_getenv conf.env "em" with
  | None ->
    begin match p_getenv conf.env "m" with
    | Some m ->
      let save_m = m in
      begin match p_getenv conf.env "i" with
      | Some ip ->
          let p = poi base (Gwdb.iper_of_string ip) in
          let digest = default_image_name base p in
          let (conf, report) =
            begin match p_getenv conf.env "m" with
            | Some "SND_IMAGE_C_OK" ->
                let mode =
                  try List.assoc "mode" conf.env with Not_found -> "portraits"
                in
                let file_name =
                    (try List.assoc "file_name" conf.env with Not_found -> "")
                in
                let file_name =
                  if file_name = "" then
                    (try List.assoc "file_name_2" conf.env with Not_found -> "")
                  else file_name
                in
                let file_name_2 = Filename.remove_extension file_name in
                let new_env =
                  List.fold_left
                    (fun accu (k, v) ->
                       if k = "file_name_2" then (k, file_name_2) :: accu
                       else (k, v) :: accu)
                    [] conf.env
                in
                let conf = { (conf) with env = new_env } in
                let file_name = Mutil.decode file_name in
                let file =
                  if mode <> "comment" then raw_get conf "file"
                  else "file_name"
                in
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_send_c_ok conf base p file file_name mode
                else
                  conf, "digest error"
            | Some "DEL_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_delete_c_ok conf base p
                else conf, "digest error"
            | Some "RESET_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_reset_c_ok conf base p
                else conf, "digest error"
            | Some "IMAGE_C" -> conf, "image"
            | _ -> conf, "incorrect request"
            end
          in
          begin match report with
          | "digest error" -> Update.error_digest conf
          | "incorrect request" -> Hutil.incorrect_request conf
          | _ -> print_confirm_c conf base save_m report
          end
      | None -> Hutil.incorrect_request conf
      end
    | None -> Hutil.incorrect_request conf
    end
  (* em!="" second pass, ignore *)
  | Some _ -> print_confirm_c conf base "REFRESH" ""
  end

(* from ImageDisplay.ml. Labeled as "do not use outside module"!*)
let print_personal_image conf base p =
  match Util.image_and_size conf base p (fun _ _ -> Some (1, 1)) with
    Some (true, f, _) ->
      if ImageDisplay.print_image_file conf f then () else Hutil.incorrect_request conf
  | _ -> Hutil.incorrect_request conf

(* from ImageDisplay.ml. Labeled as "do not use outside module"!*)
(* remove Filename.basename *)
let print_source_image conf f =
  let fname =
    if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f
  in
  let fname = Util.source_image_file_name conf.bname fname in
  if ImageDisplay.print_image_file conf fname then () else Hutil.incorrect_request conf

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Gwdb.base -> unit                         *)
(* ************************************************************************** *)
let print ?(saved=false) conf base =
  match (Util.p_getenv conf.env "s", Util.find_person_in_env conf base "") with
  | (Some f, Some p) ->
      let k = Util.default_image_name base p in
      let f =
          if saved then String.concat Filename.dir_sep [k; "saved"; f]
          else String.concat Filename.dir_sep [k; f]
      in
      print_source_image conf f
  | (Some f, _) ->
      print_source_image conf f
  | (_, Some p) ->
      begin match Util.auto_image_file ~saved:saved conf base p with
      | Some f ->
          if ImageDisplay.print_image_file conf f then () else Hutil.incorrect_request conf
      | _ -> Hutil.incorrect_request conf
      end
  | (_, _) -> Hutil.incorrect_request conf
