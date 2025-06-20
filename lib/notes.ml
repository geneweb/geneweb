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
  | Some img -> ((Util.commd conf :> string) ^ "m=DOC&s=" ^ img, img)
  | None -> ("", "")

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
  let slen = String.length s in
  let list_nt, list_ind =
    let rec loop list_nt list_ind pos i =
      if i = slen then (list_nt, list_ind)
      else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
        | NotesLinks.WLpage (j, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson (j, key, _, txt) ->
            let list_ind =
              let link = { Def.NLDB.lnTxt = txt; Def.NLDB.lnPos = pos } in
              (key, link) :: list_ind
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind pos j
        | NotesLinks.WLnone (j, _) -> loop list_nt list_ind pos j
    in
    loop [] [] 1 0
  in
  NotesLinks.update_db base fnotes (list_nt, list_ind)

let update_notes_links_person base (p : _ Def.gen_person) =
  let s =
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
  in
  update_notes_links_db base (Def.NLDB.PgInd p.Def.key_index) s

let update_notes_links_family base (f : _ Def.gen_family) =
  let s =
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
  in
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

let _print_key label (fn, sn, oc) =
  Printf.eprintf "Key: %s: %s.%d %s\n" label fn oc sn

let lower_key (fn, sn, oc) = (Name.lower fn, Name.lower sn, oc)

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
  let map_data =
    json |> Yojson.Basic.Util.member "map" |> Yojson.Basic.Util.to_list
  in
  let updated_map =
    List.map
      (fun person_json ->
        let current_person = extract_pnoc person_json |> lower_key in
        if current_person = lower_key oldk then replace_person person_json newk
        else person_json)
      map_data
  in
  `Assoc
    (List.map
       (function
         | "map", _ -> ("map", `List updated_map)
         | field -> field (* Preserve all other top-level fields *))
       (Yojson.Basic.Util.to_assoc json))

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

let rewrite_key s oldk newk _file =
  let s =
    if Mutil.contains s "TYPE=gallery" then update_gallery s oldk newk else s
  in
  let slen = String.length s in
  let rec rebuild rs i =
    if i >= slen then rs
    else
      match NotesLinks.misc_notes_link s i with
      | WLpage (j, _, _, _, _) | WLwizard (j, _, _) | WLnone (j, _) ->
          let ss = String.sub s i (j - i) in
          rebuild (rs ^ ss) j
      | WLperson (j, k, name, text) ->
          if Def.NLDB.equal_key k oldk then
            let fn, sn, oc = newk in
            let ofn, osn, _ooc = oldk in
            let name =
              match name with
              | Some str -> Some (replace ofn fn str |> replace osn sn)
              | None -> None
            in
            let ss =
              Printf.sprintf "[[%s/%s/%d/%s%s]]" fn sn oc
                (Option.fold
                   ~none:(Printf.sprintf "%s %s" fn sn)
                   ~some:(fun txt -> txt)
                   name)
                (Option.fold ~none:"" ~some:(fun txt -> ";" ^ txt) text)
            in
            rebuild (rs ^ ss) j
          else
            let ss = String.sub s i (j - i) in
            rebuild (rs ^ ss) j
  in
  rebuild "" 0

let replace_ind_key_in_str base is oldk newk p =
  let s = Driver.sou base is in
  let design = Gutil.designation base p in
  let s' = rewrite_key s oldk newk design in
  Driver.insert_string base s'

let update_ind_key_pgind base p oldk newk =
  let oldp = Driver.gen_person_of_person @@ Driver.poi base p in
  let replace is =
    replace_ind_key_in_str base is oldk newk (Driver.poi base p)
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

let update_ind_key_pgfam base f oldk newk =
  let oldf = Driver.gen_family_of_family @@ Driver.foi base f in
  let cpl = Driver.foi base f in
  let fath = Driver.poi base (Driver.get_father cpl) in
  let moth = Driver.poi base (Driver.get_mother cpl) in
  let _family =
    Gutil.designation base fath ^ " x " ^ Gutil.designation base moth
  in
  let replace is = replace_ind_key_in_str base is oldk newk fath in
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

let update_ind_key_pgmisc conf base f oldk newk =
  let fname = path_of_fnotes f in
  let oldn = Driver.base_notes_read base fname in
  let newn = rewrite_key oldn oldk newk f in
  commit_notes conf base f newn

let update_ind_key_pgwiz conf base f oldk newk =
  let fname = path_of_fnotes f in
  let oldn = Driver.base_wiznotes_read base fname in
  let newn = rewrite_key oldn oldk newk f in
  commit_wiznotes conf base f newn

let update_ind_key conf base link_pages oldk newk =
  Printf.eprintf "updating %d note pages...\n%!" (List.length link_pages);
  List.iter
    (function
      | Def.NLDB.PgInd p -> update_ind_key_pgind base p oldk newk
      | PgFam f -> update_ind_key_pgfam base f oldk newk
      | PgNotes -> update_ind_key_pgmisc conf base "" oldk newk
      | PgMisc f -> update_ind_key_pgmisc conf base f oldk newk
      | PgWizard f -> update_ind_key_pgwiz conf base f oldk newk)
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

let source_note conf base p str =
  let env =
    [
      ('i', fun () -> Driver.Iper.to_string (Driver.get_iper p));
      ('k', fun () -> Image.default_image_filename "portraits" base p);
    ]
  in
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let source_note_with_env conf base env str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let fold_linked_pages conf base db key type_filter transform =
  List.fold_left
    (fun acc (pg, (_, il)) ->
      let record_it =
        match (pg, type_filter) with
        | Def.NLDB.PgMisc n, Some typ ->
            let nenv = read_notes base n |> fst in
            let gallery =
              try List.assoc "TYPE" nenv = typ with Not_found -> false
            in
            gallery
        | Def.NLDB.PgInd ip, None -> (
            authorized_age conf base (pget conf base ip)
            &&
            match type_filter with
            | Some "gallery" | Some "album" -> false
            | _ -> true)
        | Def.NLDB.PgFam ifam, None -> (
            authorized_age conf base
              (pget conf base (Driver.get_father @@ Driver.foi base ifam))
            &&
            match type_filter with
            | Some "gallery" | Some "album" -> false
            | _ -> true)
        | _, _ -> (
            true
            &&
            match type_filter with
            | Some "gallery" | Some "album" -> false
            | _ -> true)
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
  match mode with
  | Delete -> Hashtbl.remove ht old_key
  | Merge -> (
      let entry = try Some (Hashtbl.find ht old_key) with Not_found -> None in
      match entry with
      | Some _ -> Hashtbl.remove ht old_key
      | None ->
          ();
          Hashtbl.add ht new_key nbr)
  | Rename ->
      (let entry =
         try Some (Hashtbl.find ht old_key) with Not_found -> None
       in
       match entry with
       | Some _ ->
           Hashtbl.remove ht old_key;
           Hashtbl.add ht new_key nbr
       | None -> ());
      write_cache_linked_pages conf ht

let linked_pages_nbr conf base ip =
  let key =
    Util.make_key base (Driver.gen_person_of_person (Driver.poi base ip))
  in
  let ht = read_cache_linked_pages conf in
  let entry = try Some (Hashtbl.find ht key) with Not_found -> None in
  match entry with Some nbr -> nbr | None -> 0

let has_linked_pages conf base ip = linked_pages_nbr conf base ip <> 0
