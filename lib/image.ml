open Config
open Gwdb

let portrait_folder conf = Util.base_path [ "images" ] conf.bname

let carrousel_folder conf =
  Filename.concat (Util.base_path [ "src" ] conf.bname) "images"

(** [default_portrait_filename_of_key fn sn occ] is the default filename
 of the corresponding person's portrait. WITHOUT its file extenssion.
 e.g: default_portrait_filename_of_key "Jean Claude" "DUPOND" 3 is "jean_claude.3.dupond"
 *)
let default_portrait_filename_of_key first_name surname occ =
  let space_to_unders = Mutil.tr ' ' '_' in
  let f = space_to_unders (Name.lower first_name) in
  let s = space_to_unders (Name.lower surname) in
  Format.sprintf "%s.%d.%s" f occ s

let default_portrait_filename base p =
  default_portrait_filename_of_key (p_first_name base p) (p_surname base p)
    (get_occ p)

let authorized_image_file_extension = [| ".jpg"; ".jpeg"; ".png"; ".gif" |]

let find_img_opt f =
  let exists ext =
    let fname = f ^ ext in
    if Sys.file_exists fname then Some fname else None
  in
  match exists ".url" with
  | Some f ->
      let ic = open_in f in
      let url = input_line ic in
      close_in ic;
      Some (`Url url)
  | None -> (
      match Mutil.array_find_map exists authorized_image_file_extension with
      | None -> None
      | Some f -> Some (`Path f))

(** [full_portrait_path conf base p] is [Some path] if [p] has a portrait.
    [path] is a the full path of the file with file extension. *)
let full_portrait_path conf base p =
  (* TODO why is extension not in filename..? *)
  let s = default_portrait_filename base p in
  let f = Filename.concat (portrait_folder conf) s in
  match find_img_opt f with
  | Some (`Path _) as full_path -> full_path
  | Some (`Url _)
  (* should not happen, there is only ".url" file in carrousel folder *)
  | None ->
      None

let source_filename conf src =
  let fname1 = Filename.concat (carrousel_folder conf) src in
  if Sys.file_exists fname1 then fname1
  else
    List.fold_right Filename.concat [ Secure.base_dir (); "src"; "images" ] src

let path_of_filename src =
  let fname1 =
    List.fold_right Filename.concat [ Secure.base_dir (); "images" ] src
  in
  if Sys.file_exists fname1 then `Path fname1
  else `Path (Util.search_in_assets (Filename.concat "images" src))

let png_size ic =
  let magic = really_input_string ic 4 in
  if magic = "\137PNG" then (
    seek_in ic 16;
    let wid = input_binary_int ic in
    let hei = input_binary_int ic in
    Ok (wid, hei))
  else Error ()

let gif_size ic =
  let magic = really_input_string ic 4 in
  if magic = "GIF8" then (
    seek_in ic 6;
    let wid =
      let x = input_byte ic in
      (input_byte ic * 256) + x
    in
    let hei =
      let x = input_byte ic in
      (input_byte ic * 256) + x
    in
    Ok (wid, hei))
  else Error ()

let jpeg_size ic =
  let magic = really_input_string ic 10 in
  if
    Char.code magic.[0] = 0xff
    && Char.code magic.[1] = 0xd8
    &&
    let m = String.sub magic 6 4 in
    m = "JFIF" || m = "Exif"
  then
    let exif_type = String.sub magic 6 4 = "Exif" in
    let rec loop found =
      while Char.code (input_char ic) <> 0xFF do
        ()
      done;
      let ch =
        let rec loop ch =
          if Char.code ch = 0xFF then loop (input_char ic) else ch
        in
        loop (input_char ic)
      in
      if Char.code ch = 0xC0 || Char.code ch = 0xC3 then
        if exif_type && not found then loop true
        else (
          for _i = 1 to 3 do
            ignore @@ input_char ic
          done;
          let a = input_char ic in
          let b = input_char ic in
          let c = input_char ic in
          let d = input_char ic in
          let wid = (Char.code c lsl 8) lor Char.code d in
          let hei = (Char.code a lsl 8) lor Char.code b in
          Ok (wid, hei))
      else
        let a = input_char ic in
        let b = input_char ic in
        let len = (Char.code a lsl 8) lor Char.code b in
        let len = if len >= 32768 then 0 else len in
        for _i = 1 to len - 2 do
          ignore @@ input_char ic
        done;
        if Char.code ch <> 0xDA then loop found else Error ()
    in
    loop false
  else Error ()

let size_from_path fname =
  (* TODO: size and mime type should be in db *)
  let (`Path fname) = fname in
  let res =
    if fname = "" then Error ()
    else
      try
        let ic = Secure.open_in_bin fname in
        let r =
          try
            (* TODO: should match on mime type here *)
            match String.lowercase_ascii @@ Filename.extension fname with
            | ".jpeg" | ".jpg" -> jpeg_size ic
            | ".png" -> png_size ic
            | ".gif" -> gif_size ic
            | _s -> Error ()
          with End_of_file -> Error ()
        in
        close_in ic;
        r
      with Sys_error _e -> Error ()
  in
  res

let src_to_string = function `Url s | `Path s -> s

let scale_to_fit ~max_w ~max_h ~w ~h =
  let w, h =
    if h > max_h then
      let w = w * max_h / h in
      let h = max_h in
      (w, h)
    else (w, h)
  in
  let w, h =
    if w > max_w then
      let h = h * max_w / w in
      let w = max_w in
      (w, h)
    else (w, h)
  in
  (w, h)

let is_not_private_img _conf fname =
  not (Mutil.contains fname ("private" ^ Filename.dir_sep))

(** [has_access_to_portrait conf base p] is true iif we can see [p]'s portrait. *)
let has_access_to_portrait conf base p =
  let img = get_image p in
  (conf.wizard || conf.friend)
  || (not conf.no_image)
     && Util.authorized_age conf base p
     && ((not (is_empty_string img)) || full_portrait_path conf base p <> None)
     && is_not_private_img conf (sou base img)
(* TODO: privacy settings should be in db not in url *)

(** [has_access_to_carrousel conf base p] is true iif ???. *)
let has_access_to_carrousel conf base p =
  (conf.wizard || conf.friend)
  || (not conf.no_image)
     && Util.authorized_age conf base p
     && not (Util.is_hide_names conf p)

let get_portrait_path conf base p =
  if has_access_to_portrait conf base p then full_portrait_path conf base p
  else None

(* parse a string to an `Url or a `Path *)
let urlorpath_of_string conf s =
  let http = "http://" in
  let https = "https://" in
  if Mutil.start_with http 0 s || Mutil.start_with https 0 s then `Url s
  else if Filename.is_implicit s then
    match List.assoc_opt "images_path" conf.base_env with
    | Some p when p <> "" -> `Path (Filename.concat p s)
    | Some _ | None ->
        let fname = Filename.concat (portrait_folder conf) s in
        `Path fname
  else `Path s

let src_of_string conf s =
  if s = "" then `Empty
  else
    let l = String.length s - 1 in
    if s.[l] = ')' then `Src_with_size_info s else urlorpath_of_string conf s

let parse_src_with_size_info conf s =
  let (`Src_with_size_info s) = s in
  let l = String.length s - 1 in
  try
    let pos1 = String.index s '(' in
    let pos2 = String.index_from s pos1 'x' in
    let w = String.sub s (pos1 + 1) (pos2 - pos1 - 1) |> int_of_string in
    let h = String.sub s (pos2 + 1) (l - pos2 - 1) |> int_of_string in
    let s = String.sub s 0 pos1 in
    Ok (urlorpath_of_string conf s, (w, h))
  with Not_found | Failure _ ->
    !GWPARAM.syslog `LOG_ERR
      (Format.sprintf "Error parsing portrait source with size info %s" s);
    Error "Failed to parse url with size info"

let get_portrait conf base p =
  if has_access_to_portrait conf base p then
    match src_of_string conf (sou base (get_image p)) with
    | `Src_with_size_info _s as s_info -> (
        match parse_src_with_size_info conf s_info with
        | Error _e -> None
        | Ok (s, _size) -> Some s)
    | `Url _s as url -> Some url
    | `Path p as path -> if Sys.file_exists p then Some path else None
    | `Empty -> full_portrait_path conf base p
  else None

(* In images/carrousel we store either
   - the image as the original image.jpg/png/tif image
   - the url to the image as content of a image.url text file
*)
let get_old_portrait conf base p =
  if has_access_to_portrait conf base p then
    let key = default_portrait_filename base p in
    let f =
      Filename.concat (Filename.concat (portrait_folder conf) "old") key
    in
    find_img_opt f
  else None

let rename_portrait conf base p (nfn, nsn, noc) =
  match get_portrait conf base p with
  | Some (`Path old_f) -> (
      let new_s = default_portrait_filename_of_key nfn nsn noc in
      let old_s = default_portrait_filename base p in
      let f = Filename.concat (portrait_folder conf) new_s in
      let old_ext = Filename.extension old_f in
      let new_f = f ^ old_ext in
      (try Sys.rename old_f new_f
       with Sys_error e ->
         !GWPARAM.syslog `LOG_ERR
           (Format.sprintf
              "Error renaming portrait: old_path=%s new_path=%s : %s" old_f
              new_f e));
      let new_s_f =
        String.concat Filename.dir_sep [ portrait_folder conf; "old"; new_s ]
      in
      let old_s_f =
        String.concat Filename.dir_sep [ portrait_folder conf; "old"; old_s ]
      in
      (if Sys.file_exists (old_s_f ^ old_ext) then
       try Sys.rename (old_s_f ^ old_ext) (new_s_f ^ old_ext)
       with Sys_error e ->
         !GWPARAM.syslog `LOG_ERR
           (Format.sprintf
              "Error renaming old portrait: old_path=%s new_path=%s : %s" old_f
              new_f e));
      let new_s_f =
        String.concat Filename.dir_sep [ carrousel_folder conf; new_s ]
      in
      let old_s_f =
        String.concat Filename.dir_sep [ carrousel_folder conf; old_s ]
      in
      if Sys.file_exists old_s_f then
        try Sys.rename old_s_f new_s_f
        with Sys_error e ->
          !GWPARAM.syslog `LOG_ERR
            (Format.sprintf
               "Error renaming carrousel store: old_path=%s new_path=%s : %s"
               old_f new_f e))
  | Some (`Url _url) -> () (* old url still applies *)
  | None -> ()

let get_portrait_with_size conf base p =
  if has_access_to_portrait conf base p then
    match src_of_string conf (sou base (get_image p)) with
    | `Src_with_size_info _s as s_info -> (
        match parse_src_with_size_info conf s_info with
        | Error _e -> None
        | Ok (s, size) -> Some (s, Some size))
    | `Url _s as url -> Some (url, None)
    | `Path p as path ->
        if Sys.file_exists p then
          Some (path, size_from_path path |> Result.to_option)
        else None
    | `Empty -> (
        match full_portrait_path conf base p with
        | None -> None
        | Some path -> Some (path, size_from_path path |> Result.to_option))
  else None

(* For carrousel ************************************ *)

let carrousel_file_path conf base p fname old =
  let dir =
    let dir = default_portrait_filename base p in
    if old then Filename.concat dir "old" else dir
  in
  String.concat Filename.dir_sep
    ([ carrousel_folder conf; dir ] @ if fname = "" then [] else [ fname ])

let get_carrousel_file_content conf base p fname kind old =
  let fname =
    Filename.chop_extension (carrousel_file_path conf base p fname old) ^ kind
  in
  if Sys.file_exists fname then (
    let ic = Secure.open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    if s = "" then None else Some s)
  else None

(* get list of files in carrousel *)
let get_carrousel_img_aux conf base p old =
  let get_carrousel_img_note fname =
    Option.value ~default:""
      (get_carrousel_file_content conf base p fname ".txt" false)
  in
  let get_carrousel_img_src fname =
    Option.value ~default:""
      (get_carrousel_file_content conf base p fname ".src" false)
  in
  let get_carrousel_img fname =
    let path = carrousel_file_path conf base p fname old in
    find_img_opt (Filename.chop_extension path)
  in
  if not (has_access_to_carrousel conf base p) then []
  else
    let f = carrousel_file_path conf base p "" old in
    try
      if Sys.file_exists f && Sys.is_directory f then
        Array.fold_left
          (fun acc f1 ->
            let ext = Filename.extension f1 in
            if
              f1 <> ""
              && f1.[0] <> '.'
              && (Array.mem ext authorized_image_file_extension || ext = ".url")
            then
              match get_carrousel_img f1 with
              | None -> acc
              | Some (`Path path) ->
                  (path, "", get_carrousel_img_src f1, get_carrousel_img_note f1)
                  :: acc
              | Some (`Url url) ->
                  ( Filename.chop_extension (Filename.basename f1) ^ ".url",
                    url,
                    get_carrousel_img_src f1,
                    get_carrousel_img_note f1 )
                  :: acc
            else acc)
          [] (Sys.readdir f)
      else []
    with Sys_error e ->
      !GWPARAM.syslog `LOG_ERR (Format.sprintf "carrousel error: %s, %s" f e);
      []

let get_carrousel_imgs conf base p = get_carrousel_img_aux conf base p false
let get_carrousel_old_imgs conf base p = get_carrousel_img_aux conf base p true

(* end carrousel ************************************ *)
