(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
module StrSet = Mutil.StrSet

let file_path conf base fname =
  Util.bpath
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [ base_notes_dir base; fname ^ ".txt" ])

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
    List.fold_left Filename.concat
      (Util.bpath (conf.bname ^ ".gwb"))
      [ base_notes_dir base; fname ]
  in
  Mutil.mkdir_p (Filename.dirname fpath);
  Gwdb.commit_notes base fname s;
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
