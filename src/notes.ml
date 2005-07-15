(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.108 2005-07-15 05:48:56 ddr Exp $ *)
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

value print_notes_sub_part conf sub_fname cnt0 lines =
  let mode = "NOTES" in
  let file_path = file_path conf in
  Wiki.print_sub_part conf conf.wizard file_path mode mode sub_fname cnt0 lines
;

value read_notes base fnotes =
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
          Wserver.wprint "(%s)" (transl conf "what links page");
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
    print_notes_sub_part conf fnotes cnt0 lines;
    trailer conf;
  }
;

value notes_links_db conf base eliminate_unlinked =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db2 =
    List.fold_left
      (fun db2 (pg, sl) ->
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
    [ [(NotesLinks.PgInd _ | NotesLinks.PgNotes | NotesLinks.PgWizard _, sl) ::
       pgsll] ->
        if List.mem s sl then True
        else is_referenced in_test s pgsll
    | [(NotesLinks.PgMisc s1, sl) :: pgsll] ->
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
      Wserver.wprint "%s " (capitale (transl conf "what links page"));
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
                     stag "a" "href=\"%sm=NOTES;f=%s\"" (commd conf) fnotes begin
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
    [ Some f -> if NotesLinks.check_file_name f then f else ""
    | None -> "" ]
  in
  match p_getenv conf.env "ref" with
  [ Some "on" -> print_what_links conf base fnotes
  | _ ->
      let (title, s) = read_notes base fnotes in
      match p_getint conf.env "v" with
      [ Some cnt0 -> print_notes_part conf fnotes title s cnt0
      | None -> print_whole_notes conf fnotes title s ] ]
;

value print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if NotesLinks.check_file_name f then f else ""
    | None -> "" ]
  in
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s%s" (capitale (transl_decline conf "modify" s))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let (ntitle, s) = read_notes base fnotes in
  Wiki.print_mod_page conf "NOTES" fnotes title ntitle s
;

value update_notes_links_db conf fnotes s force =
  let slen = String.length s in
  let list =
    loop [] 0 where rec loop list i =
      if i = slen then list
      else
        match NotesLinks.misc_notes_link s i with
        [ Some (j, lfname, _, _) ->
            let list = if List.mem lfname list then list else [lfname :: list] in
            loop list j
        | None -> loop list (i + 1) ]
  in
  if not force && list = [] then ()
  else
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    NotesLinks.update_db bdir fnotes list
;

value commit_notes conf base fnotes s =
  let pg =
    if fnotes = "" then NotesLinks.PgNotes
    else NotesLinks.PgMisc fnotes
  in
  do {
    base.func.commit_notes fnotes s;
    History.record_notes conf base (p_getint conf.env "v", fnotes) "mn";
    update_notes_links_db conf pg s True;
  }
;

value print_mod_ok conf base =
  let fname =
    fun
    [ Some f -> if NotesLinks.check_file_name f then f else ""
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
  let title _ =
    Wserver.wprint "%s"
      (capitale (nominative (transl conf "miscellaneous notes")))
  in
  let db2 = notes_links_db conf base True in
  do {
    header conf title;
    print_link_to_welcome conf True;
    if db2 <> [] then
      tag "ul" begin
        List.iter
          (fun (f, pl) ->
             let txt =
               let (t, s) = read_notes base f in
               if t <> "" then t
               else if String.length s < String.length f then f
               else "<em>" ^ begin_text_without_html_tags 50 s ^ "</em>"
             in
             let c =
               let f = file_path conf f in
               if Sys.file_exists f then "" else " style=\"color:red\""
             in
             tag "li" begin
               Wserver.wprint "<tt>[";
               stag "a" "href=\"%sm=NOTES;f=%s\"%s" (commd conf) f c begin
                 Wserver.wprint "%s" f;
               end;
               Wserver.wprint "]</tt> : %s\n" txt;
             end)
          db2;
      end
    else ();
    trailer conf;
  }
;
