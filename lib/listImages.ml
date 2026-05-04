open Config
open Util
module Driver = Geneweb_db.Driver

let image_extension f =
  let ext = String.lowercase_ascii (Filename.extension f) in
  Array.exists (fun e -> String.lowercase_ascii e = ext) Image.ext_list_1

let is_visible f = String.length f > 0 && f.[0] <> '.' && f.[0] <> '~'

let list_images dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f ->
        let full = Filename.concat dir f in
        try
          (Unix.stat full).st_kind = Unix.S_REG
          && is_visible f && image_extension f
        with Unix.Unix_error _ -> false)
    |> List.sort compare
  with Sys_error _ | Unix.Unix_error _ -> []

let list_subdirs dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f ->
        let full = Filename.concat dir f in
        try (Unix.stat full).st_kind = Unix.S_DIR && is_visible f
        with Unix.Unix_error _ -> false)
    |> List.sort compare
  with Sys_error _ | Unix.Unix_error _ -> []

let safe_segment s =
  s <> "" && s <> ".."
  && (not (String.contains s '/'))
  && not (String.contains s '\\')

let json_string s = `String s
let json_strings l = `List (List.map json_string l)

let json_subdir base name =
  let sub = Filename.concat base name in
  `Assoc [ ("name", `String name); ("files", json_strings (list_images sub)) ]

let json_key_full conf key =
  let base = Filename.concat (!GWPARAM.images_d conf.bname) key in
  `Assoc
    [
      ("src", `String "key");
      ("key", `String key);
      ("files", json_strings (list_images base));
      ("subdirs", `List (List.map (json_subdir base) (list_subdirs base)));
    ]

let json_key_dir conf key dir =
  let base = Filename.concat (!GWPARAM.images_d conf.bname) key in
  let target = Filename.concat base dir in
  `Assoc
    [
      ("src", `String "key");
      ("key", `String key);
      ("dir", `String dir);
      ("files", json_strings (list_images target));
    ]

let json_albums_root conf =
  let base = !GWPARAM.albums_d conf.bname in
  `Assoc
    [
      ("src", `String "albums");
      ("files", `List []);
      ( "subdirs",
        `List
          (List.map
             (fun n -> `Assoc [ ("name", `String n) ])
             (list_subdirs base)) );
    ]

let json_albums_dir conf dir =
  let base = !GWPARAM.albums_d conf.bname in
  let target = Filename.concat base dir in
  `Assoc
    [
      ("src", `String "albums");
      ("dir", `String dir);
      ("files", json_strings (list_images target));
    ]

let parse_keys env =
  let mk fn sn oc =
    if fn = "" || sn = "" then None
    else
      let oc_int = try int_of_string oc with _ -> 0 in
      Some (Image.key_dir_basename fn sn oc_int)
  in
  let get suffix k =
    match p_getenv env (k ^ suffix) with Some v -> v | None -> ""
  in
  let key_at suffix =
    let oc = match get suffix "oc" with "" -> "0" | s -> s in
    mk (get suffix "p") (get suffix "n") oc
  in
  let rec collect i acc =
    match key_at (string_of_int i) with
    | Some k -> collect (i + 1) (k :: acc)
    | None -> List.rev acc
  in
  let head = match key_at "" with Some k -> [ k ] | None -> [] in
  head @ collect 1 []

let print conf _base =
  let charset = if conf.charset = "" then "utf-8" else conf.charset in
  Output.header conf "Content-type: application/json; charset=%s" charset;
  let src = match p_getenv conf.env "src" with Some s -> s | None -> "key" in
  let dir = p_getenv conf.env "dir" in
  let json =
    match (src, dir) with
    | "albums", None -> `List [ json_albums_root conf ]
    | "albums", Some d when safe_segment d -> `List [ json_albums_dir conf d ]
    | "key", None ->
        let keys = parse_keys conf.env |> List.filter safe_segment in
        `List (List.map (json_key_full conf) keys)
    | "key", Some d when safe_segment d ->
        let keys = parse_keys conf.env |> List.filter safe_segment in
        `List (List.map (fun k -> json_key_dir conf k d) keys)
    | _ -> `List []
  in
  Output.print_sstring conf (Yojson.Basic.to_string json)
