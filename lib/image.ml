open Config
module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

let path_str path =
  match path with Some (`Path pa) -> pa | Some (`Url u) -> u | None -> ""

let portrait_folder conf = !GWPARAM.portraits_d conf.bname
let carrousel_folder conf = !GWPARAM.images_d conf.bname

(** [default_portrait_filename_of_key fn sn occ] is the default filename of the
    corresponding person's portrait. WITHOUT its file extenssion. e.g:
    default_portrait_filename_of_key "Jean Claude" "DUPOND" 3 is
    "jean_claude.3.dupond" *)
let default_image_filename_of_key mode first_name surname occ =
  let sp2_ = Mutil.tr ' ' '_' in
  let f = sp2_ (Name.lower first_name) in
  let s = sp2_ (Name.lower surname) in
  if mode = "blasons" then Format.sprintf "%s.%d.%s.blason" f occ s
  else Format.sprintf "%s.%d.%s" f occ s

let default_image_filename_aux mode base p saved =
  let name =
    default_image_filename_of_key mode
      (Driver.p_first_name base p)
      (Driver.p_surname base p) (Driver.get_occ p)
  in
  if saved then Filename.concat "saved" name else name

let default_image_filename mode base p =
  default_image_filename_aux mode base p false

let ext_list_1 = [| ".jpg"; ".jpeg"; ".png"; ".gif" |]
let ext_list_2 = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".url"; ".stop" |]

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
      match exists ".stop" with
      | Some f -> Some (`Path f)
      | None -> (
          match Mutil.array_find_map exists ext_list_1 with
          | None -> None
          | Some f -> Some (`Path f)))

let find_file_without_ext f =
  let exists ext =
    let fname = f ^ ext in
    if Sys.file_exists fname then Some fname else None
  in
  let ext = Filename.extension f in
  (* file f happens to have correct extension *)
  if Array.mem ext ext_list_2 then f
  else
    match Mutil.array_find_map exists ext_list_2 with None -> "" | Some f -> f

(** [full_image_path mode conf base p] is [Some path] if [p] has a portrait or a
    blason. [path] is a the full path of the file with file extension. *)
let full_image_path mode conf base p saved =
  (* TODO why is extension not in filename..? *)
  let s = default_image_filename_aux mode base p saved in
  let f = Filename.concat (portrait_folder conf) s in
  match find_img_opt f with
  | Some (`Path _) as full_path -> full_path
  | Some (`Url _) as full_url -> full_url
  | None -> None

let path_of_filename conf fname =
  let fname1 = Filename.concat (!GWPARAM.images_d conf.bname) fname in
  if Sys.file_exists fname1 then fname1
  else Util.search_in_assets (Filename.concat "images" fname)

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

(** [has_access_to_portrait conf base p] is true iif we can see [p]'s portrait.
*)
let has_access_to_image mode conf base p =
  let img = Driver.get_image p in
  (conf.wizard || conf.friend)
  || (not conf.no_image)
     && Util.authorized_age conf base p
     && ((not (Driver.Istr.is_empty img))
        || full_image_path mode conf base p false <> None)
     && is_not_private_img conf (Driver.sou base img)

(* TODO: privacy settings should be in db not in url *)

(** [has_access_to_carrousel conf base p] is true iif ???. *)
let has_access_to_carrousel conf base p =
  (conf.wizard || conf.friend)
  || (not conf.no_image)
     && Util.authorized_age conf base p
     && not (Util.is_hide_names conf p)

let get_portrait_path conf base p =
  if has_access_to_image "portraits" conf base p then
    full_image_path "portraits" conf base p false
  else None

let is_url str =
  if
    Mutil.start_with "http" 0 str
    || Mutil.start_with "https" 0 str
    || Mutil.start_with "file" 0 str
  then true
  else false

(* parse a string to an `Url or a `Path *)
let urlorpath_of_string conf s =
  if is_url s then `Url s
  else if Filename.is_implicit s then
    (* FIXME basename does not work with sub fodlers *)
    let s = Filename.basename s in
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
    Logs.syslog `LOG_ERR
      (Format.sprintf "Error parsing portrait source with size info %s" s);
    Error "Failed to parse url with size info"

(* In images/carrousel we store either
   - the image as the original image.jpg/png/tif image
   - the url to the image as content of a image.url text file
*)
let get_old_portrait_or_blason conf base mode p =
  if has_access_to_image mode conf base p then
    let f =
      Filename.concat (portrait_folder conf)
        (default_image_filename_aux mode base p true)
    in
    find_img_opt f
  else None

let get_portrait_aux conf base p saved =
  let f =
    Filename.concat (portrait_folder conf)
      (default_image_filename_aux "portraits" base p saved)
  in
  if has_access_to_image "portraits" conf base p then
    if not saved then
      match src_of_string conf (Driver.sou base (Driver.get_image p)) with
      | `Src_with_size_info _s as s_info -> (
          match parse_src_with_size_info conf s_info with
          | Error _e -> None
          | Ok (s, _size) -> Some s)
      | `Url _s as url -> Some url
      | `Path p as path -> if Sys.file_exists p then Some path else None
      | `Empty -> full_image_path "portraits" conf base p false
    else find_img_opt f
  else None

let get_portrait conf base p = get_portrait_aux conf base p false
let get_old_portrait conf base p = get_portrait_aux conf base p true

let get_portrait_name_aux conf base p saved =
  let name = default_image_filename_aux "portraits" base p saved in
  let f = Filename.concat (portrait_folder conf) name in
  Printf.eprintf "Get_portrait_name: %s, %s\n" name f;
  match find_img_opt f with
  | Some (`Path p) ->
      Printf.eprintf "Path %s\n" p;
      Filename.basename p
  | Some (`Url u) ->
      Printf.eprintf "Url %s\n" u;
      Filename.basename name ^ ".url"
  | None -> ""

let get_portrait_name conf base p = get_portrait_name_aux conf base p false
let get_old_portrait_name conf base p = get_portrait_name_aux conf base p true

(* if self = true, then do not loop for fathers *)
let get_blason_aux conf base p self saved =
  if has_access_to_image "blasons" conf base p then
    let rec loop p =
      match
        src_of_string conf
          (path_str (full_image_path "blasons" conf base p saved))
      with
      | `Src_with_size_info s when Filename.extension s = ".stop" -> None
      | `Src_with_size_info _s as s_info -> (
          match parse_src_with_size_info conf s_info with
          | Error _e -> None
          | Ok (s, _size) -> Some s)
      | `Path p when Filename.extension p = ".stop" -> None
      | `Path p -> Some (`Path p)
      | `Url u -> Some (`Url u)
      | `Empty -> (
          match Driver.get_parents p with
          | Some ifam when not self ->
              let cpl = Driver.foi base ifam in
              let fa = Driver.poi base (Driver.get_father cpl) in
              loop fa
          | _ -> None)
    in
    loop p
  else None

let get_blason conf base p self = get_blason_aux conf base p self false
let get_old_blason conf base p self = get_blason_aux conf base p self true

let get_blason_name_aux conf base p saved =
  let name = default_image_filename_aux "blasons" base p saved in
  let f = Filename.concat (portrait_folder conf) name in
  match find_img_opt f with
  | Some (`Path p) -> Filename.basename p
  | Some (`Url _u) -> name ^ ".url"
  | None -> ""

let get_blason_name conf base p = get_blason_name_aux conf base p false
let get_old_blason_name conf base p = get_blason_name_aux conf base p true

let has_blason conf base p self =
  match get_blason conf base p self with
  | None -> false
  | Some (`Path p) when Filename.extension p = ".stop" -> false
  | Some (`Path _p) -> true
  | Some (`Url _u) -> true

let has_blason_stop conf base p =
  match
    src_of_string conf (path_str (full_image_path "blasons" conf base p false))
  with
  | `Path p when Filename.extension p = ".stop" -> true
  | _ -> false

let get_blason_owner conf base p =
  if has_access_to_image "blasons" conf base p then
    let rec loop p =
      match Driver.get_parents p with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          let fa_iper = Driver.get_father cpl in
          let fa = Driver.poi base fa_iper in
          if get_blason conf base fa true <> None then Some fa_iper else loop fa
      | _ -> None
    in
    loop p
  else None

let rename_portrait_and_blason conf base p (nfn, nsn, noc) =
  let sp2_ = Mutil.tr ' ' '_' in
  let old_key =
    Format.sprintf "%s.%d.%s"
      (Driver.get_first_name p |> Driver.sou base |> Name.lower |> sp2_)
      (Driver.get_occ p)
      (Driver.get_surname p |> Driver.sou base |> Name.lower |> sp2_)
  in
  let new_key =
    Format.sprintf "%s.%d.%s"
      (nfn |> Name.lower |> sp2_)
      noc
      (nsn |> Name.lower |> sp2_)
  in
  if old_key = new_key then ()
  else
    let p_dir = !GWPARAM.portraits_d conf.bname in
    let i_dir = !GWPARAM.images_d conf.bname in
    let old_carrousel = Filename.concat i_dir old_key in
    let new_carrousel = Filename.concat i_dir new_key in
    (if Sys.file_exists old_carrousel && Sys.is_directory old_carrousel then
       try Sys.rename old_carrousel new_carrousel
       with Sys_error e ->
         Logs.syslog `LOG_ERR
           (Format.sprintf "Error renaming carrousel directory %s to %s: %s"
              old_carrousel new_carrousel e));
    let rename_files_with_extensions dir base_name =
      Array.iter
        (fun ext ->
          let blason =
            if Filename.check_suffix base_name ".blason" then ".blason" else ""
          in
          let old_file = Filename.concat dir (base_name ^ ext) in
          if Sys.file_exists old_file then
            let new_file = Filename.concat dir (new_key ^ blason ^ ext) in
            try Sys.rename old_file new_file
            with Sys_error e ->
              Logs.syslog `LOG_ERR
                (Format.sprintf "Error renaming %s to %s: %s" old_file new_file
                   e))
        [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".stop" |]
    in
    rename_files_with_extensions p_dir old_key;
    rename_files_with_extensions p_dir (old_key ^ ".blason");
    let saved_dir = Filename.concat p_dir "saved" in
    if Sys.file_exists saved_dir then (
      rename_files_with_extensions saved_dir old_key;
      rename_files_with_extensions saved_dir (old_key ^ ".blason"))

let get_portrait_with_size conf base p =
  if has_access_to_image "portraits" conf base p then
    match src_of_string conf (Driver.sou base (Driver.get_image p)) with
    | `Src_with_size_info _s as s_info -> (
        match parse_src_with_size_info conf s_info with
        | Error _e -> None
        | Ok (s, size) -> Some (s, Some size))
    | `Url _s as url -> Some (url, None)
    | `Path p as path ->
        if Sys.file_exists p then
          Some (path, size_from_path p |> Result.to_option)
        else None
    | `Empty -> (
        match full_image_path "portraits" conf base p false with
        | Some (`Path p) -> Some (`Path p, size_from_path p |> Result.to_option)
        | Some (`Url u) -> Some (`Url u, None)
        | None -> None)
  else None

let get_blason_with_size conf base p self =
  if has_access_to_image "blasons" conf base p then
    let rec loop p =
      match
        src_of_string conf
          (path_str (full_image_path "blasons" conf base p false))
      with
      | `Src_with_size_info _s as s_info -> (
          match parse_src_with_size_info conf s_info with
          | Error _e -> None
          | Ok (s, size) -> Some (s, Some size))
      | `Url _s as url -> Some (url, None)
      | `Path p as path ->
          if Sys.file_exists p then
            Some (path, size_from_path p |> Result.to_option)
          else None
      | `Empty -> (
          match Driver.get_parents p with
          | Some ifam when not self ->
              let cpl = Driver.foi base ifam in
              let fa = Driver.poi base (Driver.get_father cpl) in
              loop fa
          | _ -> (
              match full_image_path "blasons" conf base p false with
              | Some (`Path p) ->
                  Some (`Path p, size_from_path p |> Result.to_option)
              | Some (`Url u) -> Some (`Url u, None)
              | None -> None))
    in
    loop p
  else None

(* For carrousel ************************************ *)

let carrousel_file_path conf base p fname old =
  let dir =
    let dir = default_image_filename "portraits" base p in
    if old then Filename.concat dir "saved" else dir
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
              && (Array.mem ext ext_list_1 || ext = ".url")
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
      Logs.syslog `LOG_ERR (Format.sprintf "carrousel error: %s, %s" f e);
      []

let get_carrousel_imgs conf base p = get_carrousel_img_aux conf base p false
let get_carrousel_old_imgs conf base p = get_carrousel_img_aux conf base p true

(* end carrousel ************************************ *)
