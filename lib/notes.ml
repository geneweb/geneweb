(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
module StrSet = Mutil.StrSet

let file_path conf base fname =
  String.concat Filename.dir_sep
    [ Util.bpath conf.bname; base_notes_dir base; fname ^ ".txt" ]

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  | Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base_notes_read base fnotes in
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
  let db = Gwdb.read_nldb base in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
        let record_it =
          let open Def.NLDB in
          match pg with
          | PgInd ip -> pget conf base ip |> authorized_age conf base
          | PgFam ifam ->
              foi base ifam |> get_father |> pget conf base
              |> authorized_age conf base
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
        | state, _ -> state)
      (None, None) l
  in
  let json = try Yojson.Basic.from_string s with _ -> `Null in
  let path, img = match json with `Assoc l -> extract l | _ -> (None, None) in
  match (path, img) with
  | Some path, Some img ->
      let full_path =
        match path with
        | "doc" -> (Util.commd conf :> string) ^ "m=DOC&s=" ^ img
        | "private" -> (
            match List.assoc_opt "gallery_path_private" conf.base_env with
            | Some s -> s ^ img
            | None -> "")
        | "public" -> (
            match List.assoc_opt "gallery_path" conf.base_env with
            | Some s -> s ^ img
            | None -> "")
        | path -> path ^ img
      in
      (full_path, img)
  | _ -> ("", "")

let safe_gallery conf s =
  let html s = Util.string_with_macros conf [] s in
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
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
    in
    loop [] [] 1 0
  in
  NotesLinks.update_db base fnotes (list_nt, list_ind)

let commit_notes conf base fnotes s =
  let pg = if fnotes = "" then Def.NLDB.PgNotes else Def.NLDB.PgMisc fnotes in
  let fname = path_of_fnotes fnotes in
  let fpath =
    String.concat Filename.dir_sep
      [ Util.bpath conf.bname; base_notes_dir base; fname ]
  in
  File.create_dir ~parent:true (Filename.dirname fpath);
  try Gwdb.commit_notes base fname s
  with Sys_error m ->
    Hutil.incorrect_request conf ~comment:("explication todo: " ^ m);
    History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
    update_notes_links_db base pg s

let wiki_aux pp conf base env str =
  let s = Util.string_with_macros conf env str in
  let lines = pp (Wiki.html_of_tlsw conf s) in
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  String.concat "\n" lines |> Wiki.syntax_links conf wi |> Util.safe_html

let source conf base str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base [] str

let note conf base env str = wiki_aux (fun x -> x) conf base env str

let person_note conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  note conf base env str

let source_note conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let source_note_with_env conf base env str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

(**/**)

(* ((Gwdb.iper, Gwdb.ifam) Def.NLDB.page * ('a * (Def.NLDB.key * 'b) list)) list -> *)
let links_to_ind conf base db key =
  let l =
    List.fold_left
      (fun pgl (pg, (_, il)) ->
        let record_it =
          match pg with
          | Def.NLDB.PgInd ip -> authorized_age conf base (pget conf base ip)
          | Def.NLDB.PgFam ifam ->
              authorized_age conf base
                (pget conf base (get_father @@ foi base ifam))
          | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun pgl (k, l) -> if k = key then (k, l) :: pgl else pgl)
            pgl il
        else pgl)
      [] db
  in
  List.sort_uniq compare l

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
       | Some pgl ->
           Hashtbl.remove ht old_key;
           Hashtbl.add ht new_key nbr
       | None -> ());
      write_cache_linked_pages conf ht

let linked_pages_nbr conf base ip =
  let key = Util.make_key base (Gwdb.gen_person_of_person (poi base ip)) in
  let ht = read_cache_linked_pages conf in
  let entry = try Some (Hashtbl.find ht key) with Not_found -> None in
  match entry with Some nbr -> nbr | None -> 0

let has_linked_pages conf base ip = linked_pages_nbr conf base ip <> 0
