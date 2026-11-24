open Geneweb
open Config

let src = Logs.Src.create ~doc:"Media" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Code = Geneweb_http.Code

type dest = Portrait | Blason | Person | Images | Albums
type kind = Jpeg | Png | Gif | Pdf | Docx | Odt | Txt

let ( let* ) = Result.bind

let dest_of_string s =
  match s with
  | "portrait" -> Some Portrait
  | "blason" -> Some Blason
  | "person" -> Some Person
  | "images" -> Some Images
  | "albums" -> Some Albums
  | _ -> None

let extension kind =
  match kind with
  | Jpeg -> ".jpg"
  | Png -> ".png"
  | Gif -> ".gif"
  | Pdf -> ".pdf"
  | Docx -> ".docx"
  | Odt -> ".odt"
  | Txt -> ".txt"

let is_image kind =
  match kind with Jpeg | Png | Gif -> true | Pdf | Docx | Odt | Txt -> false

let accepted dest kind =
  match dest with
  | Portrait | Blason | Albums -> is_image kind
  | Person -> kind <> Txt
  | Images -> true

let detect filename content =
  let starts prefix = String.starts_with ~prefix content in
  let ext = String.lowercase_ascii (Filename.extension filename) in
  if starts "\xFF\xD8\xFF" then Some Jpeg
  else if starts "\x89PNG\r\n\x1A\n" then Some Png
  else if starts "GIF87a" || starts "GIF89a" then Some Gif
  else if starts "%PDF-" then Some Pdf
  else if starts "PK\003\004" && ext = ".docx" then Some Docx
  else if starts "PK\003\004" && ext = ".odt" then Some Odt
  else if
    ext = ".txt"
    && String.is_valid_utf_8 content
    && not (String.contains content '\000')
  then Some Txt
  else None

let max_size conf kind =
  let bvar, default =
    if is_image kind then ("max_images_size", 20_000_000)
    else ("max_documents_size", 100_000_000)
  in
  Option.value ~default
    (Option.bind (List.assoc_opt bvar conf.base_env) int_of_string_opt)

let stem_extensions =
  [
    ".jpg";
    ".jpeg";
    ".pjpeg";
    ".png";
    ".gif";
    ".pdf";
    ".docx";
    ".odt";
    ".htm";
    ".html";
    ".url";
  ]

let typed_extensions =
  [ ".jpg"; ".jpeg"; ".png"; ".gif"; ".pdf"; ".docx"; ".odt"; ".txt" ]

let root conf dest =
  match dest with
  | Albums -> Layout.albums conf.layout
  | Portrait | Blason | Person | Images -> Layout.images conf.layout

let person_dir conf base p =
  Filename.concat (root conf Person)
    (Image.default_image_filename "portraits" base p)

let error conf status key = Error (status, Util.transl conf key)

let failure conf e =
  Log.err (fun k -> k "%s" e);
  error conf Code.Internal_Server_Error "error"

let find_person conf base =
  match Util.find_person_in_env conf base "" with
  | Some p -> Ok p
  | None -> error conf Code.Bad_Request "incorrect request"

let find_dest conf =
  match Option.bind (Util.p_getenv conf.env "dest") dest_of_string with
  | Some dest -> Ok dest
  | None -> error conf Code.Bad_Request "incorrect request"

type place = { dir : string; saved : string; segments : string list }

let find_segments conf =
  match Util.p_getenv conf.env "subdir" with
  | None | Some "" -> Ok []
  | Some s -> (
      match Image.safe_subpath s with
      | Some segments -> Ok segments
      | None -> error conf Code.Bad_Request "invalid subdirectory")

let flat_place conf dir =
  let* segments = find_segments conf in
  if segments = [] then
    Ok { dir; saved = Filename.concat dir MediaDir.saved; segments }
  else error conf Code.Bad_Request "invalid subdirectory"

let find_place conf base dest =
  match dest with
  | Person ->
      let* p = find_person conf base in
      flat_place conf (person_dir conf base p)
  | Images -> flat_place conf (root conf Images)
  | Albums ->
      let* segments = find_segments conf in
      let root = root conf Albums in
      let concat = List.fold_left Filename.concat in
      Ok
        {
          dir = concat root segments;
          saved = concat (Filename.concat root MediaDir.saved) segments;
          segments;
        }
  | Portrait | Blason -> error conf Code.Bad_Request "incorrect request"

let find_stem conf =
  let stem name =
    let ext = String.lowercase_ascii (Filename.extension name) in
    if List.mem ext typed_extensions then Filename.remove_extension name
    else name
  in
  match
    Option.bind
      (Option.map stem (Util.p_getenv conf.env "name"))
      Image.safe_name
  with
  | Some stem -> Ok stem
  | None -> error conf Code.Bad_Request "invalid file name"

let store conf place ~sidecars name existing content =
  if existing <> [] && Util.p_getenv conf.env "replace" <> Some "on" then
    error conf Code.Conflict "upload conflict"
  else
    try
      Filesystem.create_dir ~parent:true place.dir;
      List.iter
        (fun fname ->
          if ImageCarrousel.move_file_to_saved ~sidecars place.saved fname = 0
          then failwith "cannot save the previous file")
        existing;
      ImageCarrousel.write_file (Filename.concat place.dir name) content;
      Ok ()
    with (Sys_error _ | Failure _ | Unix.Unix_error _) as e ->
      failure conf (Printexc.to_string e)

let path place name =
  String.concat "/" (List.rev (name :: List.rev place.segments))

let url conf query = (Util.commd conf :> string) ^ query

let file_url conf dest place name =
  url conf
    ((if dest = Albums then "m=IMA&s=" else "m=IM&s=")
    ^ (Mutil.encode (path place name) :> string))

let check_sending conf =
  if List.assoc_opt "can_send_image" conf.base_env = Some "no" then
    error conf Code.Forbidden "sending images forbidden"
  else Ok ()

let send conf base =
  let* () = check_sending conf in
  let* dest = find_dest conf in
  let* content =
    match List.assoc_opt "file" conf.env with
    | Some content -> Ok (content : Adef.encoded_string :> string)
    | None -> error conf Code.Bad_Request "incorrect request"
  in
  let filename =
    Option.value ~default:"" (Util.p_getenv conf.env "file_name")
  in
  let* kind =
    match detect filename content with
    | Some kind when accepted dest kind -> Ok kind
    | Some _ | None ->
        error conf Code.Unsupported_Media_Type "file type not accepted here"
  in
  let limit = max_size conf kind in
  let* () =
    if String.length content > limit then
      let size =
        Printf.sprintf (Util.ftransl conf "%d MB") (max 1 (limit / 1_000_000))
      in
      Error
        ( Code.Payload_Too_Large,
          Printf.sprintf (Util.ftransl conf "file too big (max %s)") size )
    else Ok ()
  in
  let ext = extension kind in
  match dest with
  | Portrait | Blason -> (
      let* p = find_person conf base in
      let mode, m =
        if dest = Blason then ("blasons", "FIM") else ("portraits", "IM")
      in
      match ImageCarrousel.send_portrait conf base p mode ext content with
      | Ok () ->
          Ok
            ( Image.default_image_filename mode base p ^ ext,
              url conf ("m=" ^ m ^ "&" ^ (Util.acces conf base p :> string)) )
      | Error e -> failure conf e)
  | Person ->
      let* p = find_person conf base in
      let* place = flat_place conf (person_dir conf base p) in
      let* stem = find_stem conf in
      let name = stem ^ ext in
      let existing =
        List.filter Sys.file_exists
          (List.map
             (fun ext -> Filename.concat place.dir (stem ^ ext))
             stem_extensions)
      in
      let* () = store conf place ~sidecars:true name existing content in
      History.record conf base
        (Def.U_Send_image
           (Util.string_gen_person base (Driver.gen_person_of_person p)))
        "sf";
      Ok
        ( name,
          url conf
            ("m=IM_C&"
            ^ (Util.acces conf base p :> string)
            ^ "&s="
            ^ (Mutil.encode name :> string)) )
  | Images | Albums ->
      let* place = find_place conf base dest in
      let* () =
        if dest = Albums && place.segments = [] then
          error conf Code.Bad_Request "invalid subdirectory"
        else Ok ()
      in
      let* stem = find_stem conf in
      let name = stem ^ ext in
      let fname = Filename.concat place.dir name in
      let existing = if Sys.file_exists fname then [ fname ] else [] in
      let* () = store conf place ~sidecars:false name existing content in
      Log.info (fun k -> k "%s: %s uploaded %s" conf.bname conf.user fname);
      Ok (name, file_url conf dest place name)

let is_empty dir =
  try
    let d = Unix.opendir dir in
    Fun.protect
      ~finally:(fun () -> Unix.closedir d)
      (fun () ->
        let rec loop () =
          match Unix.readdir d with
          | "." | ".." -> loop ()
          | _ -> false
          | exception End_of_file -> true
        in
        loop ())
  with Unix.Unix_error _ -> false

let make_dir conf base =
  let* () = check_sending conf in
  let* place = find_place conf base Albums in
  if place.segments = [] then error conf Code.Bad_Request "invalid subdirectory"
  else
    try
      Filesystem.create_dir ~parent:true place.dir;
      Log.info (fun k -> k "%s: %s created %s" conf.bname conf.user place.dir);
      Ok (String.concat "/" place.segments)
    with (Sys_error _ | Unix.Unix_error _ | Filesystem.File_error _) as e ->
      failure conf (Printexc.to_string e)

let delete conf base =
  let* () = check_sending conf in
  let* dest = find_dest conf in
  let* place =
    match dest with
    | Images | Albums -> find_place conf base dest
    | Portrait | Blason | Person ->
        error conf Code.Bad_Request "incorrect request"
  in
  let name = Option.value ~default:"" (Util.p_getenv conf.env "name") in
  let fname = Filename.concat place.dir name in
  let* () =
    if Image.safe_name name = None || not (Sys.file_exists fname) then
      error conf Code.Bad_Request "incorrect request"
    else if not (Sys.is_directory fname) then
      if ImageCarrousel.move_file_to_saved ~sidecars:false place.saved fname = 0
      then failure conf ("cannot save " ^ fname)
      else Ok ()
    else if dest = Albums && is_empty fname then
      try Ok (Unix.rmdir fname)
      with Unix.Unix_error (e, _, _) -> failure conf (Unix.error_message e)
    else error conf Code.Bad_Request "incorrect request"
  in
  Log.info (fun k -> k "%s: %s deleted %s" conf.bname conf.user fname);
  Ok name

let print_json conf status json =
  Output.status conf status;
  Output.header conf "Content-type: application/json; charset=utf-8";
  Output.header conf "Connection: close";
  Output.print_sstring conf (Yojson.Basic.to_string json);
  Output.flush conf

let print_error conf (status, msg) =
  let conflict =
    if status = Code.Conflict then [ ("conflict", `Bool true) ] else []
  in
  print_json conf status
    (`Assoc (("error", `String (Utf8.capitalize_fst msg)) :: conflict))

let print_send conf base =
  match send conf base with
  | Ok (name, url) ->
      let dest = Option.value ~default:"" (Util.p_getenv conf.env "dest") in
      print_json conf Code.OK
        (`Assoc
           [
             ("name", `String name); ("dest", `String dest); ("url", `String url);
           ])
  | Error e -> print_error conf e

let print_delete conf base =
  match delete conf base with
  | Ok name -> print_json conf Code.OK (`Assoc [ ("name", `String name) ])
  | Error e -> print_error conf e

let print_mkdir conf base =
  match make_dir conf base with
  | Ok subdir -> print_json conf Code.OK (`Assoc [ ("subdir", `String subdir) ])
  | Error e -> print_error conf e

let print_list conf base =
  let listing =
    let* dest = find_dest conf in
    match dest with
    | Portrait | Blason -> Ok ([], [])
    | Person | Images | Albums ->
        let* place = find_place conf base dest in
        let t = MediaDir.read place.dir in
        let sidecar (f : MediaDir.file) =
          let ext = String.lowercase_ascii (Filename.extension f.name) in
          ext = ".txt" || ext = ".src"
        in
        let dir name =
          let empty = is_empty (Filename.concat place.dir name) in
          `Assoc [ ("name", `String name); ("empty", `Bool empty) ]
        in
        Ok
          (match dest with
          | Person -> (List.filter (fun f -> not (sidecar f)) t.files, [])
          | Images -> (t.files, [])
          | Albums | Portrait | Blason -> (t.files, List.map dir t.dirs))
  in
  match listing with
  | Ok (files, dirs) ->
      let file (f : MediaDir.file) =
        `Assoc [ ("name", `String f.name); ("size", `Int f.size) ]
      in
      print_json conf Code.OK
        (`Assoc [ ("files", `List (List.map file files)); ("dirs", `List dirs) ])
  | Error e -> print_error conf e
