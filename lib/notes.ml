(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module StrSet = Mutil.StrSet
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let file_path conf base fname =
  String.concat Filename.dir_sep
    [ Util.bpath conf.bname; Driver.base_notes_dir base; fname ^ ".txt" ]

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  | Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = Driver.base_notes_read base fnotes in
  Wiki.split_title_and_text s

let merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
        let pg =
          match pg with
          | Def.NLDB.PgMisc f -> Def.NLDB.PgMisc (Wiki.map_notes aliases f)
          | x -> x
        in
        let sl = List.map (Wiki.map_notes aliases) sl in
        (pg, (sl, il)))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
      let sl, _il1, list =
        let list1, list2 =
          match list with
          | ((pg1, _) as x) :: l -> if pg = pg1 then ([ x ], l) else ([], list)
          | [] -> ([], list)
        in
        match list1 with
        | [ (_, (sl1, il1)) ] ->
            let sl =
              List.fold_left
                (fun sl s -> if List.mem s sl then sl else s :: sl)
                sl sl1
            in
            let il =
              List.fold_left
                (fun il i -> if List.mem i il then il else i :: il)
                il il1
            in
            (sl, il, list2)
        | _ -> (sl, il, list)
      in
      (pg, (sl, il)) :: list)
    [] db

let notes_links_db conf base eliminate_unlinked =
  let db = Driver.read_nldb base in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
        let record_it =
          let open Def.NLDB in
          match pg with
          | PgInd ip -> pget conf base ip |> authorized_age conf base
          | PgFam ifam ->
              Driver.(
                foi base ifam |> get_father |> pget conf base
                |> authorized_age conf base)
          | PgNotes | PgMisc _ | PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun db2 s ->
              try
                let list = List.assoc s db2 in
                (s, pg :: list) :: List.remove_assoc s db2
              with Not_found -> (s, [ pg ]) :: db2)
            db2 sl
        else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, _il)) ->
        let open Def.NLDB in
        match pg with
        | PgInd _ | PgFam _ | PgNotes | PgWizard _ ->
            List.fold_left (fun set s -> StrSet.add s set) set sl
        | PgMisc s ->
            Hashtbl.add misc s sl;
            set)
      StrSet.empty db
  in
  let mark = Hashtbl.create 1 in
  (let rec loop = function
     | s :: sl ->
         if Hashtbl.mem mark s then loop sl
         else (
           Hashtbl.add mark s ();
           let sl1 = try Hashtbl.find misc s with Not_found -> [] in
           loop (List.rev_append sl1 sl))
     | [] -> ()
   in
   loop (StrSet.elements set));
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 -> if is_referenced s then (s, list) :: db2 else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) ->
      Gutil.alphabetic_order (Name.lower s1) (Name.lower s2))
    db2

let image_url_from_path conf path =
  if path = "" then ""
  else
    let prefix = (Util.commd conf :> string) in
    let album = "albums/" in
    if String.starts_with ~prefix:album path then
      let n = String.length album in
      prefix ^ "m=IMA&s=" ^ String.sub path n (String.length path - n)
    else prefix ^ "m=DOC&s=" ^ path

let json_extract_img conf s =
  let extract l =
    List.fold_left
      (fun state e ->
        match (state, e) with
        | (None, img), ("path", `String s) -> (Some s, img)
        | (path, None), ("img", `String s) -> (path, Some s)
        | (path, None), ("images", `List images) ->
            (* Extract the first image if available *)
            let img =
              match images with
              | `Assoc img_obj :: _ -> (
                  try
                    match List.assoc "img" img_obj with
                    | `String s -> Some s
                    | _ -> None
                  with Not_found -> None)
              | _ -> None
            in
            (path, img)
        | state, _ -> state)
      (None, None) l
  in
  let json = try Yojson.Basic.from_string s with _ -> `Null in
  let _, img = match json with `Assoc l -> extract l | _ -> (None, None) in
  match img with
  | Some img -> (image_url_from_path conf img, img)
  | None -> ("", "")

let extract_pnoc json =
  let fn =
    try
      json
      |> Yojson.Basic.Util.member "fn"
      |> Yojson.Basic.Util.to_string_option |> Option.value ~default:""
    with _ -> ""
  in
  let sn =
    try
      json
      |> Yojson.Basic.Util.member "sn"
      |> Yojson.Basic.Util.to_string_option |> Option.value ~default:""
    with _ -> ""
  in
  let oc =
    try
      match json |> Yojson.Basic.Util.member "oc" with
      | `String oc_str -> ( try int_of_string oc_str with _ -> 0)
      | `Int oc_int -> oc_int
      | _ -> 0
    with _ -> 0
  in
  (fn, sn, oc)

let mark_pnocs_validity base s =
  let mark_one e =
    match e with
    | `Assoc l ->
        let t =
          try match List.assoc "t" l with `String s -> s | _ -> ""
          with Not_found -> ""
        in
        if t = "" || t = "p" then
          let fn, sn, oc = extract_pnoc (`Assoc l) in
          if fn = "" || sn = "" then `Assoc l
          else
            let ok =
              Driver.person_of_key base (Name.lower fn) (Name.lower sn) oc
              <> None
            in
            `Assoc (l @ [ ("valid", `Bool ok) ])
        else `Assoc l
    | other -> other
  in
  let walk_image_fields l =
    List.map
      (function
        | "map", `List lmap -> ("map", `List (List.map mark_one lmap))
        | other -> other)
      l
  in
  let walk_top_fields l =
    List.map
      (function
        | "map", `List lmap -> ("map", `List (List.map mark_one lmap))
        | "images", `List imgs ->
            ( "images",
              `List
                (List.map
                   (function
                     | `Assoc img_l -> `Assoc (walk_image_fields img_l)
                     | other -> other)
                   imgs) )
        | other -> other)
      l
  in
  let json = try Yojson.Basic.from_string s with _ -> `Assoc [] in
  let json =
    match json with `Assoc l -> `Assoc (walk_top_fields l) | _ -> `Assoc []
  in
  Yojson.Basic.to_string json

let safe_gallery conf base s =
  let html s =
    let s =
      let wi =
        {
          Wiki.wi_mode = "NOTES";
          Wiki.wi_file_path = file_path conf base;
          Wiki.wi_person_exists = person_exists conf base;
          Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
          Wiki.wi_always_show_link = conf.wizard || conf.friend;
        }
      in
      Wiki.syntax_links conf wi s
    in
    Util.string_with_macros conf [] s
  in
  let safe_map e =
    match e with
    | `Assoc l ->
        `Assoc
          (List.map
             (function
               | key, `String s when key = "alt" -> (key, `String (html s))
               | e -> e)
             l)
    | _ -> `Assoc []
  in
  let safe_json l =
    List.map
      (function
        | key, `String s when key = "title" -> (key, `String (html s))
        | key, `String s when key = "chronicle" -> (key, `String (html s))
        | key, `String s when key = "desc" -> (key, `String (html s))
        | "map", `List lmap -> ("map", `List (List.map safe_map lmap))
        | "images", `List images_l ->
            ( "images",
              `List
                (List.map
                   (function
                     | `Assoc img_l ->
                         `Assoc
                           (List.map
                              (function
                                | "map", `List lmap ->
                                    ("map", `List (List.map safe_map lmap))
                                | "img", `String s -> ("img", `String s)
                                | "desc", `String s -> ("desc", `String (html s))
                                | e -> e)
                              img_l)
                     | e -> e)
                   images_l) )
        | e -> e)
      l
  in
  let json = try Yojson.Basic.from_string s with _ -> `Assoc [] in
  let json =
    match json with `Assoc l -> `Assoc (safe_json l) | _ -> `Assoc []
  in
  Yojson.Basic.to_string json

let update_notes_links_db base fnotes s =
  let list_nt, list_ind =
    NotesLinks.fold_links
      (fun ~pos ~i:_ ~j:_ link (list_nt, list_ind) ->
        match link with
        | NotesLinks.WLpage (_, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            (list_nt, list_ind)
        | NotesLinks.WLperson (_, key, _, txt, fam_marker) ->
            let link =
              {
                Def.NLDB.lnTxt = txt;
                Def.NLDB.lnPos = pos;
                Def.NLDB.lnFamMarker = fam_marker;
              }
            in
            (list_nt, (key, link) :: list_ind)
        | NotesLinks.WLwizard _ | NotesLinks.WLimage _ | NotesLinks.WLnone _ ->
            (list_nt, list_ind))
      ([], []) s 0
  in
  NotesLinks.update_db base fnotes (list_nt, list_ind)

(* The concatenation of every note-bearing field of a person - the exact
   text that gets scanned for outgoing [[fn/sn/oc/text]] links. Exposed
   so callers can compute it BEFORE patching a person (the "old" text)
   and compare it against the text after patching, to decide whether a
   full nldb read-modify-write ([update_notes_links_db], and therefore
   [update_notes_links_person] below) is actually needed - see there. *)
let notes_bearing_text_of_person base (p : _ Def.gen_person) =
  let sl =
    [
      p.notes;
      p.occupation;
      p.birth_note;
      p.birth_src;
      p.baptism_note;
      p.baptism_src;
      p.death_note;
      p.death_src;
      p.burial_note;
      p.burial_src;
      p.psources;
    ]
  in
  let sl =
    let rec loop l accu =
      match l with
      | [] -> accu
      | evt :: l -> loop l (evt.Def.epers_note :: evt.Def.epers_src :: accu)
    in
    loop p.pevents sl
  in
  String.concat " " (List.map (Driver.sou base) sl)

(* Likewise for a family's note-bearing fields. *)
let notes_bearing_text_of_family base (f : _ Def.gen_family) =
  let sl = [ f.marriage_note; f.marriage_src; f.comment; f.fsources ] in
  let sl =
    let rec loop l accu =
      match l with
      | [] -> accu
      | evt :: l -> loop l (evt.Def.efam_note :: evt.Def.efam_src :: accu)
    in
    loop f.fevents sl
  in
  String.concat " " (List.map (Driver.sou base) sl)

(* [update_notes_links_db] always does a full read + linear scan + full
   rewrite of the nldb file, however large it is (see [NotesLinks.read]/
   [write] and [add_in_db]'s [List.remove_assoc]) - there is no partial
   update. On a very large base, paying this on every single save (as
   [on_person_saved] now does unconditionally) is a real cost even when
   the edit had nothing to do with notes at all (a birth date, a
   source...). [?old_text], when given, is compared against the freshly
   computed text: if they're equal, nothing note-relevant changed, and
   the expensive read-modify-write is skipped entirely. Callers that
   don't have an old snapshot handy (a brand-new person, or a merge
   where "old" doesn't cleanly apply to the combined result) simply
   omit it and always reindex, exactly as before this optimization. *)
let update_notes_links_person ?old_text base (p : _ Def.gen_person) =
  let s = notes_bearing_text_of_person base p in
  if old_text <> Some s then
    update_notes_links_db base (Def.NLDB.PgInd p.Def.key_index) s

let update_notes_links_family ?old_text base (f : _ Def.gen_family) =
  let s = notes_bearing_text_of_family base f in
  if old_text <> Some s then
    update_notes_links_db base (Def.NLDB.PgFam f.Def.fam_index) s

let commit_notes conf base fnotes s =
  let pg = if fnotes = "" then Def.NLDB.PgNotes else Def.NLDB.PgMisc fnotes in
  let fname = path_of_fnotes fnotes in
  let fpath =
    String.concat Filename.dir_sep
      [ Util.bpath conf.bname; Driver.base_notes_dir base; fname ]
  in
  Filesystem.create_dir ~parent:true (Filename.dirname fpath);
  (try Driver.commit_notes base fname s
   with Sys_error m ->
     Hutil.incorrect_request conf ~comment:("explication todo: " ^ m));
  History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
  update_notes_links_db base pg s

let commit_wiznotes conf base fnotes s =
  let pg = Def.NLDB.PgWizard fnotes in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat
      (Util.bpath (conf.bname ^ ".gwb"))
      [ Driver.base_wiznotes_dir base; fname ]
  in
  Filesystem.create_dir ~parent:true (Filename.dirname fpath);
  Driver.commit_wiznotes base fname s;
  History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
  update_notes_links_db base pg s

(* TODO Henri -> Henri-xx -> Henri fails to remove the -xx !! *)
(* TODO adjust replacement to news capital variants *)
let replace olds news str =
  let olds_l = Name.lower olds in
  let olds_u1 = Utf8.capitalize_fst olds_l in
  let olds_u2 = Utf8.uppercase olds_l in
  let regexp =
    Str.regexp (olds ^ "\\|" ^ olds_l ^ "\\|" ^ olds_u1 ^ "\\|" ^ olds_u2)
  in
  Str.global_replace regexp news str

(*
TITLE=Test imap
TYPE=gallery
{"title":"Test imap","desc":"","path":"doc","img":"famille-ph-gouraud.jpg",
 "map":
 [{"shape":"rect","coords":"104,100.7,145,152.7",
   "fn":"henri",
   "sn":"gouraud",
   "gw":"[[Henri/Gouraud/0/Henri Gouraud]]",
   "oc":"0",
   "alt":"Henri Gouraud",
   "group":"1"},{...}],
 "groups":[]}
*)

let _print_key label (fn, sn, oc) =
  Printf.eprintf "Key: %s: %s.%d %s\n" label fn oc sn

let lower_key (fn, sn, oc) = (Name.lower fn, Name.lower sn, oc)

let json_gallery_items_for_key conf s key =
  let json = try Yojson.Basic.from_string s with _ -> `Null in
  let lkey = lower_key key in
  let has_key ml =
    let l = match ml with `List l -> l | _ -> [] in
    List.exists
      (fun e -> Def.NLDB.equal_key (lower_key (extract_pnoc e)) lkey)
      l
  in
  let url f = image_url_from_path conf f in
  let str k l =
    try match List.assoc k l with `String v -> v | _ -> ""
    with Not_found -> ""
  in
  let process images =
    let _, r =
      List.fold_left
        (fun (i, acc) img ->
          let i = i + 1 in
          match img with
          | `Assoc il
            when has_key (try List.assoc "map" il with Not_found -> `Null) ->
              let f = str "img" il in
              (i, (i, url f, f, str "desc" il) :: acc)
          | _ -> (i, acc))
        (0, []) images
    in
    List.rev r
  in
  match json with
  | `Assoc l -> (
      match List.assoc_opt "images" l with
      | Some (`List imgs) -> process imgs
      | _ -> process [ `Assoc l ])
  | _ -> []

let replace_person person_json (new_fn, new_sn, new_oc) =
  `Assoc
    (List.map
       (function
         | "fn", _ -> ("fn", `String new_fn)
         | "sn", _ -> ("sn", `String new_sn)
         | "oc", _ -> ("oc", `String (string_of_int new_oc))
         | key, value -> (* Preserve any other fields *) (key, value))
       (Yojson.Basic.Util.to_assoc person_json))

(* Processes the map to replace target person
   with new values if the condition is met *)
let update_map json oldk newk =
  let update_map_list lmap =
    List.map
      (fun person_json ->
        let current_person = extract_pnoc person_json |> lower_key in
        if current_person = lower_key oldk then replace_person person_json newk
        else person_json)
      lmap
  in
  let update_fields l =
    List.map
      (function
        | "map", `List lmap -> ("map", `List (update_map_list lmap))
        | "images", `List imgs ->
            ( "images",
              `List
                (List.map
                   (function
                     | `Assoc il ->
                         `Assoc
                           (List.map
                              (function
                                | "map", `List lmap ->
                                    ("map", `List (update_map_list lmap))
                                | e -> e)
                              il)
                     | e -> e)
                   imgs) )
        | field -> field)
      l
  in
  `Assoc (update_fields (Yojson.Basic.Util.to_assoc json))

let update_gallery s oldk newk =
  (* assumes the json part starts at the first { *)
  let title_part, json_part =
    try
      let json_start = String.index s '{' in
      let json_end = String.rindex s '}' in
      ( String.sub s 0 json_start,
        String.sub s json_start (json_end - json_start + 1) )
    with Not_found -> ("", "{}")
  in
  let json = Yojson.Basic.from_string json_part in
  match json with
  | `Assoc [] -> s
  | _ ->
      let updated_json = update_map json oldk newk in
      title_part ^ Yojson.Basic.pretty_to_string updated_json ^ "\n"

(* [oldk]/[newk] are [Def.NLDB.key] triples: they are always lower-cased
   (see [Util.make_key]), because they double as Hashtbl keys
   (cache_linked_pages) and are compared against the lower-cased key that
   [NotesLinks.misc_notes_link] parses out of [[fn/sn/oc/text]] links.
   [display_name] is deliberately a different type: the real,
   case-preserved (first name, surname) of a person, for building the
   text written back into a note. Never use a [Def.NLDB.key]'s fn/sn for
   that - doing so is what previously turned the surname (and first
   name) lowercase after a rename. *)
type display_name = { df_first_name : string; df_surname : string }

let rewrite_key s oldk newk new_name _file =
  let s =
    if Mutil.contains s "TYPE=gallery" || Mutil.contains s "TYPE=album" then
      update_gallery s oldk newk
    else s
  in
  let slen = String.length s in
  let rec rebuild rs i =
    if i >= slen then rs
    else
      match NotesLinks.misc_notes_link s i with
      | WLpage (j, _, _, _, _)
      | WLwizard (j, _, _)
      | WLimage (j, _, _, _)
      | WLnone (j, _) ->
          let ss = String.sub s i (j - i) in
          rebuild (rs ^ ss) j
      | WLperson (j, k, name, text, fam_marker) ->
          if Def.NLDB.equal_key k oldk then
            let _, _, oc = newk in
            let { df_first_name = fn; df_surname = sn } = new_name in
            let ofn, osn, _ooc = oldk in
            let name =
              match name with
              | Some str -> Some (replace ofn fn str |> replace osn sn)
              | None -> None
            in
            let fam_suffix =
              match fam_marker with
              | Some n -> "&" ^ string_of_int n
              | None -> ""
            in
            let ss =
              Printf.sprintf "[[%s/%s/%d/%s%s]]%s" fn sn oc
                (Option.fold
                   ~none:(Printf.sprintf "%s %s" fn sn)
                   ~some:(fun txt -> txt)
                   name)
                (Option.fold ~none:"" ~some:(fun txt -> ";" ^ txt) text)
                fam_suffix
            in
            rebuild (rs ^ ss) j
          else
            let ss = String.sub s i (j - i) in
            rebuild (rs ^ ss) j
  in
  rebuild "" 0

let replace_ind_key_in_str base is oldk newk new_name p =
  let s = Driver.sou base is in
  let design = Gutil.designation base p in
  let s' = rewrite_key s oldk newk new_name design in
  Driver.insert_string base s'

let update_ind_key_pgind base p oldk newk new_name =
  let oldp = Driver.gen_person_of_person @@ Driver.poi base p in
  let replace is =
    replace_ind_key_in_str base is oldk newk new_name (Driver.poi base p)
  in
  let notes = replace oldp.notes in
  let occupation = replace oldp.occupation in
  let birth_note = replace oldp.birth_note in
  let birth_src = replace oldp.birth_src in
  let baptism_note = replace oldp.baptism_note in
  let baptism_src = replace oldp.baptism_src in
  let death_note = replace oldp.death_note in
  let death_src = replace oldp.death_src in
  let burial_note = replace oldp.burial_note in
  let burial_src = replace oldp.burial_src in
  let psources = replace oldp.psources in
  let pevents =
    List.map
      (fun (ev : _ Def.gen_pers_event) ->
        {
          ev with
          epers_note = replace ev.epers_note;
          epers_src = replace ev.epers_src;
        })
      oldp.pevents
  in
  let newp =
    {
      oldp with
      notes;
      occupation;
      birth_note;
      birth_src;
      baptism_note;
      baptism_src;
      death_note;
      death_src;
      burial_note;
      burial_src;
      psources;
      pevents;
    }
  in
  Driver.patch_person base p newp;
  update_notes_links_person base newp

let update_ind_key_pgfam base f oldk newk new_name =
  let oldf = Driver.gen_family_of_family @@ Driver.foi base f in
  let cpl = Driver.foi base f in
  let fath = Driver.poi base (Driver.get_father cpl) in
  let moth = Driver.poi base (Driver.get_mother cpl) in
  let _family =
    Gutil.designation base fath ^ " x " ^ Gutil.designation base moth
  in
  let replace is = replace_ind_key_in_str base is oldk newk new_name fath in
  let marriage_note = replace oldf.marriage_note in
  let marriage_src = replace oldf.marriage_src in
  let comment = replace oldf.comment in
  let fsources = replace oldf.fsources in
  let fevents =
    List.map
      (fun (ev : _ Def.gen_fam_event) ->
        {
          ev with
          efam_note = replace ev.efam_note;
          efam_src = replace ev.efam_src;
        })
      oldf.fevents
  in
  let newf =
    { oldf with marriage_note; marriage_src; comment; fsources; fevents }
  in
  Driver.patch_family base f newf;
  update_notes_links_family base newf

let update_ind_key_pgmisc conf base f oldk newk new_name =
  let fname = path_of_fnotes f in
  let oldn = Driver.base_notes_read base fname in
  let newn = rewrite_key oldn oldk newk new_name f in
  commit_notes conf base f newn

let update_ind_key_pgwiz conf base f oldk newk new_name =
  let fname = path_of_fnotes f in
  let oldn = Driver.base_wiznotes_read base fname in
  let newn = rewrite_key oldn oldk newk new_name f in
  commit_wiznotes conf base f newn

let update_ind_key conf base link_pages oldk newk new_name =
  Printf.eprintf "updating %d note pages...\n%!" (List.length link_pages);
  List.iter
    (function
      | Def.NLDB.PgInd p -> update_ind_key_pgind base p oldk newk new_name
      | PgFam f -> update_ind_key_pgfam base f oldk newk new_name
      | PgNotes -> update_ind_key_pgmisc conf base "" oldk newk new_name
      | PgMisc f -> update_ind_key_pgmisc conf base f oldk newk new_name
      | PgWizard f -> update_ind_key_pgwiz conf base f oldk newk new_name)
    link_pages

let wiki_aux pp conf base env str =
  let s = Util.string_with_macros conf env str in
  let lines = pp (Wiki.html_of_tlsw conf s) in
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  String.concat "\n" lines |> Wiki.syntax_links conf wi |> Util.safe_html

let source conf base str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base [] str

let note conf base env str = wiki_aux (fun x -> x) conf base env str

let person_note conf base p str =
  let env =
    [
      ('i', fun () -> Driver.Iper.to_string (Driver.get_iper p));
      ('k', fun () -> Image.default_image_filename "portraits" base p);
    ]
  in
  note conf base env str

let source_note_with_env conf base env str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let wiki_of_source conf base ~always_show_link p s =
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_mark_if_not_public = Util.mark_if_not_public conf base;
      Wiki.wi_always_show_link = always_show_link;
    }
  in
  let env =
    [
      ( 'i',
        fun () ->
          Geneweb_db.Driver.Iper.to_string (Geneweb_db.Driver.get_iper p) );
      ('k', fun () -> Image.default_image_filename "portraits" base p);
    ]
  in
  Util.string_with_macros conf env (Wiki.syntax_links conf wi s)

let fold_linked_pages conf base db key type_filter transform =
  List.fold_left
    (fun acc (pg, (_, il)) ->
      let record_it =
        match (pg, type_filter) with
        | Def.NLDB.PgMisc n, Some typ -> (
            let nenv = read_notes base n |> fst in
            try
              let t = List.assoc "TYPE" nenv in
              t = typ || (t = "album" && typ = "gallery")
            with Not_found -> false)
        | Def.NLDB.PgInd ip, None -> (
            authorized_age conf base (pget conf base ip)
            && match type_filter with Some "gallery" -> false | _ -> true)
        | Def.NLDB.PgFam ifam, None -> (
            authorized_age conf base
              (pget conf base (Driver.get_father @@ Driver.foi base ifam))
            && match type_filter with Some "gallery" -> false | _ -> true)
        | _, _ -> (
            match type_filter with Some "gallery" -> false | _ -> true)
      in
      if record_it then
        List.fold_left
          (fun acc (k, ind) ->
            if Def.NLDB.equal_key k key then transform pg k ind acc else acc)
          acc il
      else acc)
    [] db
  |> List.sort_uniq compare

let links_to_cache_entries conf base db key =
  fold_linked_pages conf base db key None (fun _pg k ind acc -> (k, ind) :: acc)

let links_to_ind conf base db key typ =
  fold_linked_pages conf base db key typ (fun pg _k _ind acc -> pg :: acc)

type mode = Delete | Rename | Merge
type cache_linked_pages_t = (Def.NLDB.key, int) Hashtbl.t

let cache_linked_pages_name = "cache_linked_pages"

let get_linked_pages_fname conf =
  Filename.concat (!GWPARAM.bpath conf.bname) cache_linked_pages_name

let read_cache_linked_pages conf =
  let fname = get_linked_pages_fname conf in
  match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
  | Some ic ->
      let ht : cache_linked_pages_t = input_value ic in
      close_in ic;
      ht
  | None ->
      Printf.eprintf "%s not exist. Run update_nldb\n" fname;
      let ht : cache_linked_pages_t = Hashtbl.create 10 in
      ht

(* sync with update_nldb.ml if this changes *)
let write_cache_linked_pages conf cache_linked_pages =
  let fname = get_linked_pages_fname conf in
  let oc = open_out_bin fname in
  output_value oc cache_linked_pages;
  close_out oc

let update_cache_linked_pages conf mode old_key new_key nbr =
  let ht = read_cache_linked_pages conf in
  (match mode with
  | Delete -> Hashtbl.remove ht old_key
  | Merge ->
      (* [nbr] here is trusted: every current caller (see mergeInd.ml)
         computes it fresh from the just-updated nldb before calling
         this. Drop any stale entry under [old_key] (when it differs
         from [new_key]) and set the correct, current count. *)
      if old_key <> new_key then Hashtbl.remove ht old_key;
      Hashtbl.replace ht new_key nbr
  | Rename -> (
      (* The number of pages linking to this person doesn't change on a
         pure rename - only the key does - so reuse whatever was already
         cached under [old_key] rather than trusting the caller's [nbr]:
         every current caller (updateField.ml, updateIndOk.ml) just
         passes 0 here, not knowing the real count. *)
      match Hashtbl.find_opt ht old_key with
      | Some n ->
          Hashtbl.remove ht old_key;
          Hashtbl.replace ht new_key n
      | None -> ()));
  (* Every mode must persist: a mutation that's only applied to [ht] in
     memory and never written is silently lost (this used to be true
     only for [Rename], leaving [Delete] and [Merge] permanently stale
     until the next full [update_nldb] rebuild). *)
  write_cache_linked_pages conf ht

(* Call once, right after [Driver.patch_person], for a person that
   already existed before this operation - an ordinary edit (see
   updateIndOk.ml, updateField.ml) - NOT for a brand-new person, which
   has no prior key to fix up elsewhere and should just call
   [update_notes_links_person] directly. mergeIndOk.ml does NOT go
   through this either: a merge collapses two old keys into one new
   one, which doesn't fit this single-[old_key] shape, so it keeps its
   own (already correct) direct sequence.

   This is the single place that knows the full note-links contract for
   an ordinary person save, in the correct order - the two call sites
   above used to each reimplement this by hand, and one of them had
   forgotten a step:
   - always re-scan [p]'s own note-bearing fields into nldb, so a
     [[fn/sn/oc/text]] link just added or edited is tracked (this must
     happen before the next step, in case of self-reference)
   - if [p]'s key actually changed relative to [old_key], rewrite every
     page in [pgl] that referenced [old_key] to the new key/name, and
     refresh the linked-pages count cache accordingly

   [pgl] (the pages that referenced [old_key]) is taken as a parameter
   rather than recomputed here because every current caller already
   computes it via [links_to_ind] for its own "linked pages" display,
   before this function runs any rewrite. *)
let on_person_saved conf base ~old_key ?old_text
    ~(pgl : (Driver.iper, Driver.ifam) Def.NLDB.page list) p =
  update_notes_links_person ?old_text base p;
  let new_key = Util.make_key base p in
  if old_key <> new_key then (
    let new_name =
      {
        df_first_name = Driver.sou base p.first_name;
        df_surname = Driver.sou base p.surname;
      }
    in
    update_ind_key conf base pgl old_key new_key new_name;
    update_cache_linked_pages conf Rename old_key new_key 0)

let linked_pages_nbr conf base ip =
  let key =
    Util.make_key base (Driver.gen_person_of_person (Driver.poi base ip))
  in
  let ht = read_cache_linked_pages conf in
  let entry = try Some (Hashtbl.find ht key) with Not_found -> None in
  match entry with Some nbr -> nbr | None -> 0

let linked_page_text_family conf base ifam s (str : Adef.safe_string)
    (pg, (_, il)) : Adef.safe_string =
  match pg with
  | Def.NLDB.PgMisc pg -> (
      let fam = Driver.foi base ifam in
      let father_key =
        let p = Driver.poi base (Driver.get_father fam) in
        let fn = Name.lower (Driver.sou base (Driver.get_first_name p)) in
        let sn = Name.lower (Driver.sou base (Driver.get_surname p)) in
        (fn, sn, Driver.get_occ p)
      in
      let mother_key =
        let p = Driver.poi base (Driver.get_mother fam) in
        let fn = Name.lower (Driver.sou base (Driver.get_first_name p)) in
        let sn = Name.lower (Driver.sou base (Driver.get_surname p)) in
        (fn, sn, Driver.get_occ p)
      in
      let dominated_by_marker =
        let markers =
          List.filter_map (fun (_, ind) -> ind.Def.NLDB.lnFamMarker) il
        in
        List.sort_uniq compare markers
      in
      let matching_marker =
        List.find_opt
          (fun marker ->
            let dominated_keys =
              List.filter_map
                (fun (key, ind) ->
                  if ind.Def.NLDB.lnFamMarker = Some marker then Some key
                  else None)
                il
            in
            List.mem father_key dominated_keys
            && List.mem mother_key dominated_keys)
          dominated_by_marker
      in
      match matching_marker with
      | None -> str
      | Some marker -> (
          try
            let nenv, _ = read_notes base pg in
            let v = List.assoc s nenv in
            if v = "" then raise Not_found;
            let persons_with_marker =
              List.filter
                (fun (_, ind) -> ind.Def.NLDB.lnFamMarker = Some marker)
                il
            in
            let father_annot =
              List.find_map
                (fun (key, ind) ->
                  if key = father_key then ind.Def.NLDB.lnTxt else None)
                persons_with_marker
            in
            let mother_annot =
              List.find_map
                (fun (key, ind) ->
                  if key = mother_key then ind.Def.NLDB.lnTxt else None)
                persons_with_marker
            in
            let annot_suffix =
              match (father_annot, mother_annot) with
              | Some fa, Some ma when fa <> "" && ma <> "" ->
                  Printf.sprintf " (%s/%s)" fa ma
              | Some fa, _ when fa <> "" -> Printf.sprintf " (%s)" fa
              | _, Some ma when ma <> "" -> Printf.sprintf " (%s)" ma
              | _ -> ""
            in
            let lnPos =
              match persons_with_marker with
              | (_, ind) :: _ -> ind.Def.NLDB.lnPos
              | [] -> 0
            in
            let a, b, c =
              try
                let i = String.index v '{' in
                let j = String.index v '}' in
                ( String.sub v 0 i,
                  String.sub v (i + 1) (j - i - 1),
                  String.sub v (j + 1) (String.length v - j - 1) )
              with Not_found -> ("", v, "")
            in
            let str1 =
              Printf.sprintf "%s<a href=\"%sm=NOTES;f=%s#p_%d\">%s</a>%s%s" a
                (Util.commd conf :> string)
                (Mutil.encode pg :> string)
                lnPos b c annot_suffix
              |> Util.safe_html
            in
            if (str :> string) = "" then str1
            else if Util.start_with (str1 :> string) 0 "<li>" then
              Adef.safe ((str :> string) ^ (str1 :> string))
            else Adef.safe ((str :> string) ^ ", " ^ (str1 :> string))
          with Not_found -> str))
  | _ -> str

let get_linked_page_family conf base ifam s =
  let db = Driver.read_nldb base in
  let db = merge_possible_aliases conf db in
  List.fold_left (linked_page_text_family conf base ifam s) (Adef.safe "") db
