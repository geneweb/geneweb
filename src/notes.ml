(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.95 2005-07-07 12:39:46 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;
open Printf;

(* TLSW: Text Language Stolen to Wikipedia
   = title level 1 =
   == title level 2 ==
   ...
   ====== title level 6 ======
   * list ul/li item
   * list ul/li item
   ** list ul/li item 2nd level
   ** list ul/li item 2nd level
   ...
   [[first_name/surname/oc/text]] link; 'text' displayed
   [[first_name/surname/text]] link (oc = 0); 'text' displayed
   [[first_name/surname]] link (oc = 0); 'first_name surname' displayed
   [[[notes_subfile/text]]] link to a sub-file; 'text' displayed
   [[[notes_subfile]]] link to a sub-file; 'notes_subfile' displayed
   __TOC__ : summary (unnumbered)
   __SHORT_TOC__ : short summary (unnumbered)
   __NOTOC__ : no (automatic) numbered summary *)

value file_path conf fname =
  List.fold_right Filename.concat
    [Util.base_path [] (conf.bname ^ ".gwb"); "notes_d"]
    (fname ^ ".txt")
;

value insert_sub_part s v sub_part =
  let (lines, _) = Wiki.lines_list_of_string s in
  let (lines, sl) =
    loop False [] 0 Wiki.first_cnt lines
    where rec loop sub_part_added lines lev cnt =
      fun
      [ [s :: sl] ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            if v = Wiki.first_cnt - 1 then
              (if sub_part = "" then [] else [""; sub_part], [s :: sl])
            else
              let nlev = Wiki.section_level s len in
              if cnt = v then
                let lines =
                  if sub_part = "" then lines else [""; sub_part :: lines]
                in
                loop True lines nlev (cnt + 1) sl
              else if cnt > v then
                if nlev > lev then loop sub_part_added lines lev (cnt + 1) sl
                else (lines, [s :: sl])
              else loop sub_part_added [s :: lines] lev (cnt + 1) sl
            else if cnt <= v then loop sub_part_added [s :: lines] lev cnt sl
            else loop sub_part_added lines lev cnt sl
      | [] ->
          let lines =
            if sub_part_added then lines
            else if sub_part = "" then lines
            else [""; sub_part :: lines]
          in
          (lines, []) ]
  in
  String.concat "\n" (List.rev_append lines sl)
;

value rev_extract_sub_part s v =
  let (lines, _) = Wiki.lines_list_of_string s in
  loop [] 0 Wiki.first_cnt lines where rec loop lines lev cnt =
    fun
    [ [s :: sl] ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          if v = Wiki.first_cnt - 1 then lines
          else
            let nlev = Wiki.section_level s len in
            if cnt = v then loop [s :: lines] nlev (cnt + 1) sl
            else if cnt > v then
              if nlev > lev then loop [s :: lines] lev (cnt + 1) sl
              else lines
            else loop lines lev (cnt + 1) sl
        else if cnt <= v then loop lines lev cnt sl
        else loop [s :: lines] lev cnt sl
    | [] -> lines ]
;

value extract_sub_part s v =
  let rev_lines = rev_extract_sub_part s v in
  String.concat "\n" (List.rev rev_lines)
;

value print_sub_part conf mode sub_fname cnt0 s =
  let sfn = if sub_fname = "" then "" else ";f=" ^ sub_fname in
  let s =
    if cnt0 < Wiki.first_cnt && conf.wizard then
      Wiki.string_of_modify_link conf mode 0 sfn (s = "") ^ s
    else s
  in
  do {
    tag "p" begin
      if cnt0 >= Wiki.first_cnt then do {
        stag "a" "href=\"%sm=%s;v=%d%s\"" (commd conf) mode (cnt0 - 1) sfn begin
          Wserver.wprint "&lt;&lt;";
        end;
        Wserver.wprint "\n";
      }
      else ();
      if cnt0 >= Wiki.first_cnt - 1 then do {
        stag "a" "href=\"%sm=%s%s\"" (commd conf) mode sfn begin
          Wserver.wprint "^^";
        end;
        Wserver.wprint "\n";
      }
      else ();
      if s <> "" then do {
        stag "a" "href=\"%sm=%s;v=%d%s\"" (commd conf) mode (cnt0 + 1) sfn begin
          Wserver.wprint "&gt;&gt;";
        end;
        Wserver.wprint "\n";
      }
      else ();
    end;
    Wserver.wprint "%s\n" s
  }
;

value print_notes_sub_part conf sub_fname cnt0 lines =
  let mode = "NOTES" in
  let lines = Wiki.html_of_tlsw_lines  conf mode sub_fname cnt0 True lines [] in
  let file_path = file_path conf in
  let lines =
    List.map
      (fun
       [ "__TOC__" | "__SHORT_TOC__" ->
           sprintf "<p>...%s...</p>"
             (transl_nth conf "visualize/show/hide/summary" 3)
       | "__NOTOC__" -> ""
       | s -> s ])
      lines
  in
  let s = String.concat "\n" lines in
  let s = Wiki.syntax_links conf mode file_path s in
  let s = string_with_macros conf [] s in
  print_sub_part conf mode sub_fname cnt0 s
;

value split_title_and_text s =
  let (tit, txt) =
    try
      let i = String.index s '\n' in
      let tit = String.sub s 0 i in
      let txt = String.sub s (i + 1) (String.length s - i - 1) in
      (tit, txt)
    with
    [ Not_found -> (s, "") ]
  in
  if String.length tit > 0 && tit.[0] = '=' || String.contains tit '<'
  || String.contains tit '['
  then
    ("", s)
  else (tit, txt)

;

value read_notes base fnotes =
  let s = base.data.bnotes.nread fnotes RnAll in
  split_title_and_text s
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
    let s = Wiki.html_with_summary_of_tlsw conf "NOTES" file_path fnotes s in
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
    let lines = List.rev (rev_extract_sub_part s cnt0) in
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
           | NotesLinks.PgNotes | NotesLinks.PgMisc _ -> True ]
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
    [ [(NotesLinks.PgInd _ | NotesLinks.PgNotes, sl) :: pgsll] ->
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
    [ Some f -> if Gutil.check_file_name f then f else ""
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

value print_mod_page conf mode fname title ntitle s =
  let s = if ntitle = "" then s else ntitle ^ "\n" ^ s in
  let (has_v, v) =
    match p_getint conf.env "v" with
    [ Some v -> (True, v)
    | None -> (False, 0) ]
  in
  let sub_part = if not has_v then s else extract_sub_part s v in
  let sfn = if fname = "" then "" else ";f=" ^ fname in
  do {
    header conf title;
    tag "div" "style=\"float:%s;margin-%s:5px\"" conf.right conf.left begin
      stag "a" "href=\"%sm=%s%s%s\"" (commd conf) mode
        (if has_v then ";v=" ^ string_of_int v else "") sfn
      begin
        Wserver.wprint "(%s)\n"
          (transl_nth conf "visualize/show/hide/summary" 0);
      end;
    end;
    print_link_to_welcome conf False;
    if has_v then
      tag "p" begin
        if v >= Wiki.first_cnt then do {
          stag "a" "href=\"%sm=MOD_%s;v=%d%s\"" (commd conf) mode (v - 1) sfn
          begin
            Wserver.wprint "&lt;&lt;";
          end;
          Wserver.wprint "\n";
        }
        else ();
        if sub_part <> "" then do {
          stag "a" "href=\"%sm=MOD_%s;v=%d%s\"" (commd conf) mode (v + 1) sfn
          begin
            Wserver.wprint "&gt;&gt;";
          end;
          Wserver.wprint "\n";
        }
        else ();
      end
    else ();
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_%s_OK\"" mode;
        if has_v then
          xtag "input" "type=\"hidden\" name=\"v\" value=\"%d\"" v
        else ();
        if fname <> "" then
          xtag "input" "type=\"hidden\" name=\"f\" value=\"%s\"" fname
        else ();
        let digest = Iovalue.digest s in
        xtag "input" "type=\"hidden\" name=\"digest\" value=\"%s\"" digest;
        stagn "textarea" "name=\"notes\" rows=\"30\" cols=\"110\"" begin
          if sub_part <> "" then Wserver.wprint "%s" (quote_escaped sub_part)
          else ();
        end;
      end;
      tag "p" begin
        xtag "input" "type=\"submit\" value=\"Ok\"";
      end;
    end;
    trailer conf;
  }
;

value print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if Gutil.check_file_name f then f else ""
    | None -> "" ]
  in
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s%s" (capitale (transl_decline conf "modify" s))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let (ntitle, s) = read_notes base fnotes in
  print_mod_page conf "NOTES" fnotes title ntitle s
;

value print_ok conf base fnotes s =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "notes modified"))
  in
  do {
    header_no_page_title conf title;
    tag "div" "style=\"text-align:center\"" begin
      Wserver.wprint "--- ";
      title ();
      Wserver.wprint " ---\n";
    end;
    print_link_to_welcome conf True;
    let get_v = p_getint conf.env "v" in
    let v =
      match get_v with
      [ Some v -> v
      | None -> 0 ]
    in
    History.record_notes conf base (get_v, fnotes) "mn";
    let (title, s) = if v = 0 then split_title_and_text s else ("", s) in
    if v = 0 && title <> "" then do {
      xtag "br";
      xtag "br";
      Wserver.wprint "<h1 style=\"text-align:center\">%s</h1>\n" title
    }
    else ();
    let (lines, _) = Wiki.lines_list_of_string s in
    print_notes_sub_part conf fnotes v lines;
    trailer conf
  }
;

value update_notes_links_db conf fnotes s force =
  let slen = String.length s in
  let list =
    loop [] 0 where rec loop list i =
      if i = slen then list
      else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
        match Gutil.ext_file_link s i with
        [ Some (j, lfname, _, _) -> loop [lfname :: list] j
        | None -> loop list (i + 3) ]
      else loop list (i + 1)
  in
  if not force && list = [] then ()
  else
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    NotesLinks.update_db bdir fnotes list
;

value print_mod_ok conf base =
  let sub_part =
    match p_getenv conf.env "notes" with
    [ Some v -> strip_all_trailing_spaces v
    | None -> failwith "notes unbound" ]
  in
  let digest =
    match p_getenv conf.env "digest" with
    [ Some s -> s
    | None -> "" ]
  in
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if Gutil.check_file_name f then f else ""
    | None -> "" ]
  in
  let old_notes =
    let (t, s) = read_notes base fnotes in
    if t = "" then s else t ^ "\n" ^ s
  in
  try
    if digest <> Iovalue.digest old_notes then Update.error_digest conf
    else
      let s =
        match p_getint conf.env "v" with
        [ Some v -> insert_sub_part old_notes v sub_part
        | None -> sub_part ]
      in
      let pg =
        if fnotes = "" then NotesLinks.PgNotes
        else NotesLinks.PgMisc fnotes
      in
      do {
        base.func.commit_notes fnotes s;
        update_notes_links_db conf pg s True;
        print_ok conf base fnotes sub_part
      }
  with
  [ Update.ModErr -> () ]
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
             tag "li" begin
               Wserver.wprint "<tt>[";
               stag "a" "href=\"%sm=NOTES;f=%s\"" (commd conf) f begin
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
