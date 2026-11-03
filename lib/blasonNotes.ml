(* Redirect references to a person's portrait (m=IM) towards his blason in
   the misc notes of a dedicated folder (e.g. Roglo's Blasons:...), once the
   portrait has been turned into the blason. The blason pages reference a
   blason file with m=FIM and an url blason with the url itself. *)

module Driver = Geneweb_db.Driver

let src = Logs.Src.create ~doc:"BlasonNotes" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)

type target = Fim | Url of string

let default_notes_dir = "Blasons"

(* Notes folder scanned for portrait references; [blason_notes_dir=] in the
   .gwf file disables the scan. *)
let notes_dir conf =
  match List.assoc_opt "blason_notes_dir" conf.Config.base_env with
  | Some d -> String.trim d
  | None -> default_notes_dir

(* Misc notes names of the page [dir] ("Blasons") and of all the .txt files
   below the folder [dir] ("Blasons:pages:A:m"). *)
let fnotes_below root dir =
  let rec walk rel acc =
    let path = List.fold_left Filename.concat root (List.rev rel) in
    let entries = try Sys.readdir path with Sys_error _ -> [||] in
    Array.fold_left
      (fun acc e ->
        let full = Filename.concat path e in
        if Sys.is_directory full then walk (e :: rel) acc
        else if Filename.check_suffix e ".txt" then
          let name = Filename.chop_suffix e ".txt" in
          let fnotes =
            String.concat
              (String.make 1 NotesLinks.char_dir_sep)
              (List.rev (name :: rel))
          in
          match NotesLinks.check_file_name fnotes with
          | Some _ -> fnotes :: acc
          | None -> acc
        else acc)
      acc entries
  in
  if dir = "" then []
  else
    let below =
      if Sys.file_exists (Filename.concat root dir) then walk [ dir ] [] else []
    in
    (* the folder may come with a page of its own: Blasons.txt *)
    if Sys.file_exists (Filename.concat root (dir ^ ".txt")) then dir :: below
    else below

(* An URL query is split on ';' and '&'; separators are kept so that the
   rewritten URL differs from the original only on the modified tokens. *)
let split_query s =
  let len = String.length s in
  let rec loop i start acc =
    if i = len then List.rev (String.sub s start (i - start) :: acc)
    else
      match s.[i] with
      | ';' | '&' ->
          loop (i + 1) (i + 1)
            (String.make 1 s.[i] :: String.sub s start (i - start) :: acc)
      | _ -> loop (i + 1) start acc
  in
  loop 0 0 []

(* Query part of a token: the first one may carry the url prefix
   ("%sm=IM", "http://host/roglo?m=IM"). *)
let query_part t =
  if String.starts_with ~prefix:"%s" t then String.sub t 2 (String.length t - 2)
  else
    match String.rindex_opt t '?' with
    | Some i -> String.sub t (i + 1) (String.length t - i - 1)
    | None -> t

let param tokens key =
  let prefix = key ^ "=" in
  List.find_map
    (fun t ->
      let t = query_part t in
      if String.starts_with ~prefix t then
        let n = String.length prefix in
        let v = String.sub t n (String.length t - n) in
        Some (Mutil.decode (Adef.encoded v))
      else None)
    tokens

let is_im t = query_part t = "m=IM"

(* "%sm=IM" -> "%sm=FIM" *)
let fim t = String.sub t 0 (String.length t - 2) ^ "FIM"

(* [tokens] is an m=IM url designating the person [p]. *)
let designates base p tokens =
  match param tokens "i" with
  | Some i -> String.trim i = Driver.Iper.to_string (Driver.get_iper p)
  | None -> (
      match (param tokens "p", param tokens "n") with
      | Some fn, Some sn ->
          let oc =
            match param tokens "oc" with
            | Some oc ->
                Option.value ~default:0 (int_of_string_opt (String.trim oc))
            | None -> 0
          in
          Name.lower fn = Name.lower (Driver.p_first_name base p)
          && Name.lower sn = Name.lower (Driver.p_surname base p)
          && oc = Driver.get_occ p
      | Some _, None | None, Some _ | None, None -> false)

let rtrim_index s =
  let rec loop i =
    if i > 0 && (s.[i - 1] = ' ' || s.[i - 1] = '\t') then loop (i - 1) else i
  in
  loop (String.length s)

(* "k/jacques.0.d'Antoine " -> "k/jacques.0.d'Antoine.blason " *)
let blason_key t =
  let i = rtrim_index t in
  let k = String.sub t 0 i in
  if Filename.check_suffix k ".blason" then t
  else k ^ ".blason" ^ String.sub t i (String.length t - i)

(* Rewrites an attribute value if it is an m=IM url of [p]. *)
let rewrite_value base p target v =
  let tokens = split_query v in
  if List.exists is_im tokens && designates base p tokens then
    match target with
    | Url u -> Some u
    | Fim ->
        Some
          (String.concat ""
             (List.map
                (fun t ->
                  if is_im t then fim t
                  else if String.starts_with ~prefix:"k/" t then blason_key t
                  else t)
                tokens))
  else None

(* Scans the quoted attribute values (="..." or ='...') of [s]. *)
let rewrite_text_with rewrite s =
  let len = String.length s in
  let b = Buffer.create (len + 64) in
  let rec loop i changed =
    if i >= len then changed
    else
      match String.index_from_opt s i '=' with
      | None ->
          Buffer.add_substring b s i (len - i);
          changed
      | Some j when j + 1 < len && (s.[j + 1] = '"' || s.[j + 1] = '\'') -> (
          let q = s.[j + 1] in
          Buffer.add_substring b s i (j + 2 - i);
          match String.index_from_opt s (j + 2) q with
          | None ->
              Buffer.add_substring b s (j + 2) (len - j - 2);
              changed
          | Some k -> (
              let v = String.sub s (j + 2) (k - j - 2) in
              match rewrite v with
              | Some v' ->
                  Buffer.add_string b v';
                  loop k true
              | None ->
                  Buffer.add_string b v;
                  loop k changed))
      | Some j ->
          Buffer.add_substring b s i (j + 1 - i);
          loop (j + 1) changed
  in
  if loop 0 false then Some (Buffer.contents b) else None

(* Returns the names of the updated notes. *)
let rewrite_text base p target s =
  rewrite_text_with (rewrite_value base p target) s

let notes_path fnotes =
  match NotesLinks.check_file_name fnotes with
  | Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> fnotes

(* An url replaces the whole attribute value: it must not close it. *)
let usable_target target =
  match target with
  | Fim -> true
  | Url u -> not (String.contains u '"' || String.contains u '\'')

let redirect_portrait_refs conf base p target =
  let root =
    Filename.concat (Util.bpath conf.Config.bname) (Driver.base_notes_dir base)
  in
  let dir = notes_dir conf in
  let files = if usable_target target then fnotes_below root dir else [] in
  Log.debug (fun k ->
      k "%s.%d %s: %d notes in %s"
        (Driver.p_first_name base p)
        (Driver.get_occ p) (Driver.p_surname base p) (List.length files)
        (Filename.concat root dir));
  List.fold_left
    (fun acc fnotes ->
      let s = Driver.base_notes_read base (notes_path fnotes) in
      if not (Mutil.contains s "m=IM") then acc
      else
        match rewrite_text base p target s with
        | Some s' ->
            Log.info (fun k -> k "portrait -> blason in note %s" fnotes);
            Notes.commit_notes conf base fnotes s';
            fnotes :: acc
        | None -> acc)
    [] files
  |> List.rev
