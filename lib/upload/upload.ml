open Geneweb
open Config

let src = Logs.Src.create ~doc:"Upload" __MODULE__

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

let is_key_dir name =
  let len = String.length name in
  let rec digits i =
    if i < len && name.[i] >= '0' && name.[i] <= '9' then digits (i + 1) else i
  in
  let rec loop i =
    match String.index_from_opt name i '.' with
    | Some i ->
        let j = digits (i + 1) in
        (j > i + 1 && j < len && name.[j] = '.') || loop (i + 1)
    | None -> false
  in
  loop 0

let is_dir path = try Sys.is_directory path with Sys_error _ -> false

let root conf dest =
  match dest with
  | Albums -> !GWPARAM.albums_d conf.bname
  | Portrait | Blason | Person | Images -> !GWPARAM.images_d conf.bname

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

let find_subdir conf dest =
  match Util.p_getenv conf.env "subdir" with
  | None | Some "" -> Ok []
  | Some s -> (
      match Image.safe_subpath s with
      | Some (first :: _ as segments)
        when not (dest = Images && is_key_dir first) ->
          Ok segments
      | Some _ | None -> error conf Code.Bad_Request "invalid subdirectory")

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

let store conf dir name existing content =
  if existing <> [] && Util.p_getenv conf.env "replace" <> Some "on" then
    error conf Code.Conflict "upload conflict"
  else
    try
      Filesystem.create_dir ~parent:true dir;
      List.iter
        (fun fname ->
          if ImageCarrousel.move_file_to_save dir fname = 0 then
            failwith "cannot save the previous file")
        existing;
      ImageCarrousel.write_file (Filename.concat dir name) content;
      Ok ()
    with (Sys_error _ | Failure _ | Unix.Unix_error _) as e ->
      failure conf (Printexc.to_string e)

let url conf query = (Util.commd conf :> string) ^ query

let upload conf base =
  let* () =
    if List.assoc_opt "can_send_image" conf.base_env = Some "no" then
      error conf Code.Forbidden "sending images forbidden"
    else Ok ()
  in
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
      let* stem = find_stem conf in
      let dir = person_dir conf base p in
      let name = stem ^ ext in
      let existing =
        List.filter Sys.file_exists
          (List.map
             (fun ext -> Filename.concat dir (stem ^ ext))
             stem_extensions)
      in
      let* () = store conf dir name existing content in
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
      let* segments = find_subdir conf dest in
      let* stem = find_stem conf in
      let dir = List.fold_left Filename.concat (root conf dest) segments in
      let name = stem ^ ext in
      let fname = Filename.concat dir name in
      let existing = if Sys.file_exists fname then [ fname ] else [] in
      let* () = store conf dir name existing content in
      Log.info (fun k -> k "%s: %s uploaded %s" conf.bname conf.user fname);
      let path = String.concat "/" (List.rev (name :: List.rev segments)) in
      Ok
        ( name,
          url conf
            ((if dest = Albums then "m=IMA&s=" else "m=IM&s=")
            ^ (Mutil.encode path :> string)) )

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

let print_upload conf base =
  match upload conf base with
  | Ok (name, url) ->
      let dest = Option.value ~default:"" (Util.p_getenv conf.env "dest") in
      print_json conf Code.OK
        (`Assoc
           [
             ("name", `String name); ("dest", `String dest); ("url", `String url);
           ])
  | Error e -> print_error conf e

let list_files dest dir =
  let listed name =
    let ext = String.lowercase_ascii (Filename.extension name) in
    name.[0] <> '.'
    && (List.mem ext stem_extensions || (ext = ".txt" && dest <> Person))
    && not (is_dir (Filename.concat dir name))
  in
  match Sys.readdir dir with
  | names -> List.sort String.compare (List.filter listed (Array.to_list names))
  | exception Sys_error _ -> []

let list_dirs dest root =
  let rec loop acc path =
    let dir = if path = "" then root else Filename.concat root path in
    match Sys.readdir dir with
    | names ->
        Array.fold_left
          (fun acc name ->
            if
              name.[0] = '.'
              || name = "saved"
              || (path = "" && dest = Images && is_key_dir name)
              || not (is_dir (Filename.concat dir name))
            then acc
            else
              let path = if path = "" then name else path ^ "/" ^ name in
              loop (path :: acc) path)
          acc names
    | exception Sys_error _ -> acc
  in
  List.sort String.compare (loop [] "")

let print_list conf base =
  let listing =
    let* dest = find_dest conf in
    match dest with
    | Portrait | Blason -> Ok ([], [])
    | Person ->
        let* p = find_person conf base in
        Ok (list_files dest (person_dir conf base p), [])
    | Images | Albums ->
        let* segments = find_subdir conf dest in
        let root = root conf dest in
        let dir = List.fold_left Filename.concat root segments in
        Ok (list_files dest dir, list_dirs dest root)
  in
  match listing with
  | Ok (files, dirs) ->
      let strings l = `List (List.map (fun s -> `String s) l) in
      print_json conf Code.OK
        (`Assoc [ ("files", strings files); ("dirs", strings dirs) ])
  | Error e -> print_error conf e
