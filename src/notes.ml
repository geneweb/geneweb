(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.128 2005-09-21 05:42:31 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

value file_path conf fname =
  List.fold_right Filename.concat
    [Util.base_path [] (conf.bname ^ ".gwb"); "notes_d"]
    (fname ^ ".txt")
;

value path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  [ Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> "" ]
;

value read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base.data.bnotes.nread fnotes RnAll in
  Wiki.split_title_and_text s
;

value print_whole_notes conf fnotes title s =
  do {
    header_no_page_title conf
      (fun _ -> Wserver.wprint "%s" (if title = "" then fnotes else title));
    let what_links_page () =
      if fnotes <> "" then
        stagn "a" "href=\"%sm=NOTES;f=%s;ref=on\"" (commd conf) fnotes begin
          Wserver.wprint "(%s)" (transl conf "linked pages");
        end
      else ()
    in      
    gen_print_link_to_welcome what_links_page conf True;
    xtag "br";
    xtag "br";
    if title <> "" then
      Wserver.wprint "<h1 style=\"text-align:center\">%s</h1>\n" title
    else ();
    match Util.open_etc_file "summary" with
    [ Some ic -> Templ.copy_from_templ conf [] ic
    | None -> () ];
    let file_path = file_path conf in
    let s = string_with_macros conf [] s in
    let edit_opt = if conf.wizard then Some ("NOTES", fnotes) else None in
    let s = Wiki.html_with_summary_of_tlsw conf "NOTES" file_path edit_opt s in
    Wserver.wprint "%s\n" s;
    trailer conf;
  }
;

value print_notes_part conf fnotes title s cnt0 =
  do {
    header_no_page_title conf
      (fun _ -> Wserver.wprint "%s" (if title = "" then fnotes else title));
    print_link_to_welcome conf True;
    match Util.open_etc_file "summary" with
    [ Some ic -> Templ.copy_from_templ conf [] ic
    | None -> () ];
    if cnt0 = 0 && title <> "" then do {
      xtag "br";
      xtag "br";
      Wserver.wprint "<h1 style=\"text-align:center\">%s</h1>\n" title
    }
    else ();
    let s = string_with_macros conf [] s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let mode = "NOTES" in
    let file_path = file_path conf in
    Wiki.print_sub_part conf conf.wizard file_path mode mode fnotes cnt0 lines;
    trailer conf;
  }
;

value notes_links_db conf base eliminate_unlinked =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db =
    let aliases = Wiki.notes_aliases conf in
    List.fold_left
      (fun list (pg, (sl, il)) ->
         let pg =
           match pg with
           [ NotesLinks.PgMisc f ->
               NotesLinks.PgMisc (Wiki.map_notes aliases f)
           | x -> x ]
         in
         let sl = List.map (Wiki.map_notes aliases) sl in
         let (sl, il1, list) =
           let (list1, list2) =
             List.partition (fun (pg1, _) -> pg = pg1) list
           in
           match list1 with
           [ [(_, (sl1, il1))] ->
               let sl =
                 List.fold_left
                   (fun sl s -> if List.mem s sl then sl else [s :: sl]) sl sl1
               in
               let il =
                 List.fold_left
                   (fun il i -> if List.mem i il then il else [i :: il]) il il1
               in
               (sl, il, list2)
           | _ -> (sl, il, list) ]
         in
         [(pg, (sl, il)) :: list])
      [] db
  in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, il)) ->
         let record_it =
           match pg with
           [ NotesLinks.PgInd ip -> authorized_age conf base (poi base ip)
           | NotesLinks.PgNotes | NotesLinks.PgMisc _
           | NotesLinks.PgWizard _ -> True ]
         in
         if record_it then
           List.fold_left
             (fun db2 s ->
                try
                  let list = List.assoc s db2 in
                  [(s, [pg :: list]) :: List.remove_assoc s db2]
                with
                [ Not_found -> [(s, [pg]) :: db2] ])
              db2 sl
         else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let rec is_referenced in_test s =
    fun
    [ [(NotesLinks.PgInd _ | NotesLinks.PgNotes | NotesLinks.PgWizard _,
        (sl, il)) ::
       pgsll] ->
        if List.mem s sl then True
        else is_referenced in_test s pgsll
    | [(NotesLinks.PgMisc s1, (sl, il)) :: pgsll] ->
        if is_referenced in_test s pgsll then True
        else if List.mem s sl then
          if List.mem s1 in_test then False
          else is_referenced [s :: in_test] s1 db
        else False
    | [] -> False ]
  in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 ->
           if is_referenced [] s db then [(s, list) :: db2]
           else db2)
        db2 []
    else db2
  in
  List.sort (fun (s1, _) (s2, _) -> compare s1 s2) db2
;

value print_what_links conf base fnotes =
  let title h =
    do {
      Wserver.wprint "%s " (capitale (transl conf "linked pages"));
      if h then Wserver.wprint "[%s]" fnotes
      else
        stag "tt" begin
          Wserver.wprint "[";
          stag "a" "href=\"%sm=NOTES;f=%s\"" (commd conf) fnotes begin
            Wserver.wprint "%s" fnotes;
          end;
          Wserver.wprint "]";
        end
    }
  in
  let db = notes_links_db conf base False in
  do {
    Util.header conf title;
    Util.print_link_to_welcome conf True;
    try
      let pl = List.assoc fnotes db in
      tag "ul" begin
        List.iter
          (fun pg ->
             stagn "li" begin
               match pg with
               [ NotesLinks.PgInd ip ->
                   let p = poi base ip in
                   Wserver.wprint "%s%s"
                     (Util.referenced_person_title_text conf base p)
                     (Date.short_dates_text conf base p)
               | NotesLinks.PgNotes ->
                   stagn "a" "href=\"%sm=NOTES\"" (commd conf) begin
                     Wserver.wprint "%s" (transl_nth conf "note/notes" 1);
                   end
               | NotesLinks.PgMisc fnotes ->
                   stagn "tt" begin
                     Wserver.wprint "[";
                     stag "a" "href=\"%sm=NOTES;f=%s\"" (commd conf) fnotes
                     begin
                       Wserver.wprint "%s" fnotes;
                     end;
                     Wserver.wprint "]";
                   end
               | NotesLinks.PgWizard wizname ->
                   stagn "tt" begin
                     stag "i" begin
                       Wserver.wprint "%s"
                         (transl_nth conf "wizard/wizards/friend/friends" 0);
                     end;
		     Wserver.wprint " ";
                     stag "a" "href=\"%sm=WIZNOTES;v=%s\"" (commd conf)
                       (code_varenv wizname)
                     begin
                       Wserver.wprint "%s" wizname;
                     end;
                   end ];
             end)
          pl;
      end
    with
    [ Not_found -> () ];
    Util.trailer conf;
  }
;

value print conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  match p_getenv conf.env "ref" with
  [ Some "on" -> print_what_links conf base fnotes
  | _ ->
      let (nenv, s) = read_notes base fnotes in
      let title = try List.assoc "TITLE" nenv with [ Not_found -> "" ] in
      match p_getint conf.env "v" with
      [ Some cnt0 -> print_notes_part conf fnotes title s cnt0
      | None -> print_whole_notes conf fnotes title s ] ]
;

value print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s%s" (capitale (transl_decline conf "modify" s))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let (env, s) = read_notes base fnotes in
  Wiki.print_mod_page conf "NOTES" fnotes title env s
;

value update_notes_links_db conf fnotes s force =
  let slen = String.length s in
  let (list_nt, list_ind) =
    loop [] [] 1 0 where rec loop list_nt list_ind pos i =
      if i = slen then (list_nt, list_ind)
      else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
        [ NotesLinks.WLpage j _ lfname _ _ ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else [lfname :: list_nt]
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson j key _ txt ->
            let list_ind =
              if List.mem_assoc key list_ind then list_ind
              else
                let link = {NotesLinks.lnTxt = txt; NotesLinks.lnPos = pos} in
                [(key, link) :: list_ind]
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1) ]
  in
  if not force && list_nt = [] && list_ind = [] then ()
  else
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    NotesLinks.update_db bdir fnotes (list_nt, list_ind)
;

value commit_notes conf base fnotes s =
  let pg =
    if fnotes = "" then NotesLinks.PgNotes
    else NotesLinks.PgMisc fnotes
  in
  let fname = path_of_fnotes fnotes in
  do {
    Gutil.mkdir_p (Filename.dirname (file_path conf fname));
    try base.func.commit_notes fname s with
    [ Sys_error _ -> do { incorrect_request conf; raise Update.ModErr } ];
    History.record_notes conf base (p_getint conf.env "v", fnotes) "mn";
    update_notes_links_db conf pg s True;
  }
;

value print_mod_ok conf base =
  let fname =
    fun
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  let edit_mode _ = if conf.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = read_notes base in
  let commit conf = commit_notes conf base in
  let string_filter = string_with_macros conf [] in
  let file_path = file_path conf in
  Wiki.print_mod_ok conf edit_mode mode fname read_string commit string_filter
    file_path True
;

value begin_text_without_html_tags lim s =
  loop 0 0 0 where rec loop i size len =
    if i >= String.length s then Buff.get len
    else if size > lim && String.length s > i + 3 then Buff.get len ^ "..."
    else if s.[i] = '<' then
      let i =
        loop (i + 1) where rec loop i =
          if i = String.length s then i
          else if s.[i] = '>' then i + 1
          else loop (i + 1)
      in
      loop i size len
    else if s.[i] = '=' then loop (i + 1) size len
    else
      let nbc = if utf_8_db.val then Gutil.nbc s.[i] else i + 1 in
      loop (i + nbc) (size + 1) (Buff.mstore len (String.sub s i nbc))
;

value print_misc_notes conf base =
  let d =
    match p_getenv conf.env "d" with
    [ Some d -> d
    | None -> "" ]
  in
  let title h =
    Wserver.wprint "%s"
      (if d = "" then capitale (nominative (transl conf "miscellaneous notes"))
       else if h then "- " ^ d ^ " -"
       else "<tt>- " ^ d ^ " -</tt>")
  in
  let db = notes_links_db conf base True in
  let db =
    List.fold_right
      (fun (f, _) list ->
         if String.length f >= String.length d then
           if String.sub f 0 (String.length d) = d then
             let r =
               String.sub f (String.length d) (String.length f - String.length d)
             in
             if d = "" || r <> "" && r.[0] = NotesLinks.char_dir_sep then
               let r =
                 if d = "" then r else String.sub r 1 (String.length r - 1)
               in
               try
                 let i = String.index r NotesLinks.char_dir_sep in
                 let r = String.sub r 0 i in
                 match list with
                 [ [(r', None) :: _] when r = r' -> list
                 | _ -> [(r, None) :: list] ]
               with
               [ Not_found -> [(r, Some f) :: list] ]
             else list
           else list
         else list)
      db []
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    if db <> [] then
      tag "ul" begin
        if d <> "" then
          tag "li" begin
            stag "a" "href=\"%sm=MISC_NOTES%s\"" (commd conf)
              (try
                 let i = String.rindex d NotesLinks.char_dir_sep in
                 let d = String.sub d 0 i in
                 ";d=" ^ d
               with
               [ Not_found -> "" ])
            begin
              Wserver.wprint "<tt>&lt;--</tt>";
            end;
          end
        else ();
        List.iter
          (fun (r, f) ->
             match f with
             [ Some f ->
                 let txt =
                   let (n, s) = read_notes base f in
                   let t = try List.assoc "TITLE" n with [ Not_found -> "" ] in
                   if t <> "" then t
                   else if s = "" then ""
                   else "<em>" ^ begin_text_without_html_tags 50 s ^ "</em>"
                 in
                 let c =
                   let f = file_path conf (path_of_fnotes f) in
                   if Sys.file_exists f then "" else " style=\"color:red\""
                 in
                 tag "li" "style=\"list-style-type:circle\"" begin
                   Wserver.wprint "<tt>[";
                   stag "a" "href=\"%sm=NOTES;f=%s\"%s" (commd conf) f c begin
                     Wserver.wprint "%s" r;
                   end;
                   Wserver.wprint "]</tt>%s\n"
                     (if txt = "" then "" else " : " ^ txt);
                 end
             | None ->
                 tag "li" begin
                   stag "tt" begin
                     stag "a" "href=\"%sm=MISC_NOTES;d=%s\"" (commd conf)
                       (if d = "" then r else
                        d ^ String.make 1 NotesLinks.char_dir_sep ^ r)
                     begin
                       Wserver.wprint "%s " r;
                       Wserver.wprint "--&gt;";
                     end;
                   end;
                 end ])
          db;
      end
    else ();
    trailer conf;
  }
;
