(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

module StrSet = Mutil.StrSet

let file_path conf base fname =
  Util.base_path []
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [base_notes_dir base; fname ^ ".txt"])

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
    Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base_notes_read base fnotes in Wiki.split_title_and_text s

let merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
         let pg =
           match pg with
             NotesLinks.PgMisc f ->
               NotesLinks.PgMisc (Wiki.map_notes aliases f)
           | x -> x
         in
         let sl = List.map (Wiki.map_notes aliases) sl in pg, (sl, il))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
       let (sl, _il1, list) =
         let (list1, list2) =
           match list with
             (pg1, _ as x) :: l -> if pg = pg1 then [x], l else [], list
           | [] -> [], list
         in
         match list1 with
           [_, (sl1, il1)] ->
             let sl =
               List.fold_left
                 (fun sl s -> if List.mem s sl then sl else s :: sl) sl sl1
             in
             let il =
               List.fold_left
                 (fun il i -> if List.mem i il then il else i :: il) il il1
             in
             sl, il, list2
         | _ -> sl, il, list
       in
       (pg, (sl, il)) :: list)
    [] db

let notes_links_db conf base eliminate_unlinked =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
         let record_it =
           match pg with
             NotesLinks.PgInd ip ->
               authorized_age conf base (pget conf base ip)
           | NotesLinks.PgFam ifam ->
               authorized_age conf base (pget conf base (get_father @@ foi base ifam))
           | NotesLinks.PgNotes | NotesLinks.PgMisc _ |
             NotesLinks.PgWizard _ ->
               true
         in
         if record_it then
           List.fold_left
             (fun db2 s ->
                try
                  let list = List.assoc s db2 in
                  (s, pg :: list) :: List.remove_assoc s db2
                with Not_found -> (s, [pg]) :: db2)
             db2 sl
         else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, _il)) ->
         match pg with
           NotesLinks.PgInd _ | NotesLinks.PgFam _ | NotesLinks.PgNotes |
           NotesLinks.PgWizard _ ->
             List.fold_left (fun set s -> StrSet.add s set) set sl
         | NotesLinks.PgMisc s -> Hashtbl.add misc s sl; set)
      StrSet.empty db
  in
  let mark = Hashtbl.create 1 in
  begin let rec loop =
    function
      s :: sl ->
        if Hashtbl.mem mark s then loop sl
        else
          begin
            Hashtbl.add mark s ();
            let sl1 = try Hashtbl.find misc s with Not_found -> [] in
            loop (List.rev_append sl1 sl)
          end
    | [] -> ()
  in
    loop (StrSet.elements set)
  end;
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 ->
           if is_referenced s then (s, list) :: db2 else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) -> Gutil.alphabetic_order (Name.lower s1) (Name.lower s2))
    db2

let update_notes_links_db conf fnotes s =
  let slen = String.length s in
  let (list_nt, list_ind) =
    let rec loop list_nt list_ind pos i =
      if i = slen then list_nt, list_ind
      else if i + 1 < slen && s.[i] = '%' then
        loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
          NotesLinks.WLpage (j, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson (j, key, _, txt) ->
            let list_ind =
              let link = {NotesLinks.lnTxt = txt; NotesLinks.lnPos = pos} in
              (key, link) :: list_ind
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind pos j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
    in
    loop [] [] 1 0
  in
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  NotesLinks.update_db bdir fnotes (list_nt, list_ind)

let commit_notes conf base fnotes s =
  let pg =
    if fnotes = "" then NotesLinks.PgNotes else NotesLinks.PgMisc fnotes
  in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat (Util.base_path [] (conf.bname ^ ".gwb"))
      [base_notes_dir base; fname]
  in
  Mutil.mkdir_p (Filename.dirname fpath);
  Gwdb.commit_notes base fname s ;
  History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
  update_notes_links_db conf pg s
