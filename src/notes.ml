(* camlp5r ./pa_html.cmo *)
(* $Id: notes.ml,v 5.33 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value file_path conf base fname =
  Util.base_path []
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [base_notes_dir base; fname ^ ".txt"])
;

value path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  [ Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> "" ]
;

value read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base_notes_read base fnotes in
  Wiki.split_title_and_text s
;

value print_search_form conf from_note =
  tag "table" begin
    tag "tr" begin
      tag "td" "align=\"%s\"" conf.right begin
        tag "form" "method=\"get\" action=\"%s\"" conf.command begin
          tag "p" begin
            hidden_env conf;
            xtag "input"
              "type=\"hidden\" name=\"m\" value=\"MISC_NOTES_SEARCH\"";
            xtag "input"
              "name=\"s\" size=\"30\" maxlength=\"40\" value=\"%s\""
              (match p_getenv conf.env "s" with
               [ Some s -> quote_escaped s
               | None -> "" ]);
            match from_note with
            [ Some n ->
                xtag "input" "type=\"hidden\" name=\"z\" value=\"%s\"" n
            | None -> () ];
            xtag "br";
            tag "label" begin
              xtag "input" "type=\"checkbox\" name=\"c\" value=\"on\"%s"
                (match p_getenv conf.env "c" with
                 [ Some "on" -> " checked=\"checked\""
                 | Some _ | None -> "" ]);
              Wserver.wprint "%s\n"
                (transl_nth conf "search/case sensitive" 1);
            end;
            xtag "input" "type=\"submit\" value=\"%s\""
              (capitale (transl_nth conf "search/case sensitive" 0));
          end;
        end;
      end;
    end;
  end
;

value print_whole_notes conf base fnotes title s ho = do {
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
  tag "p" begin
    xtag "br";
    xtag "br";
  end;
  if title <> "" then
    let title =
      match ho with
      [ Some (case_sens, h) -> html_highlight case_sens h title
      | None -> title ]
    in
    Wserver.wprint "<h1>%s</h1>\n" title
  else ();
  match Util.open_etc_file "summary" with
  [ Some ic -> Templ.copy_from_templ conf [] ic
  | None -> () ];
  let file_path = file_path conf base in
  let s = string_with_macros conf [] s in
  let edit_opt = Some (conf.wizard, "NOTES", fnotes) in
  let s =
    let wi =
      {Wiki.wi_mode = "NOTES"; Wiki.wi_file_path = file_path;
       Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.html_with_summary_of_tlsw conf wi edit_opt s
  in
  let s =
    match ho with
    [ Some (case_sens, h) -> html_highlight case_sens h s
    | None -> s ]
  in
  Wserver.wprint "%s\n" s;
  match ho with
  [ Some _ -> print_search_form conf (Some fnotes)
  | None -> () ];
  trailer conf;
};

value print_notes_part conf base fnotes title s cnt0 =
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
      Wserver.wprint "<h1>%s</h1>\n" title
    }
    else ();
    let s = string_with_macros conf [] s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let mode = "NOTES" in
    let wi =
      {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = file_path conf base;
       Wiki.wi_person_exists = person_exists conf base;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.print_sub_part conf wi conf.wizard mode fnotes cnt0 lines;
    trailer conf;
  }
;

value merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
         let pg =
           match pg with
           [ NotesLinks.PgMisc f ->
               NotesLinks.PgMisc (Wiki.map_notes aliases f)
           | x -> x ]
         in
         let sl = List.map (Wiki.map_notes aliases) sl in
         (pg, (sl, il)))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
       let (sl, il1, list) =
         let (list1, list2) =
           match list with
           [ [((pg1, _) as x) :: l] ->
               if pg = pg1 then ([x], l) else ([], list)
           | [] -> ([], list) ]
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
;

value notes_links_db conf base eliminate_unlinked = do {
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, il)) ->
         let record_it =
           match pg with
           [ NotesLinks.PgInd ip ->
               authorized_age conf base (pget conf base ip)
           | NotesLinks.PgFam ifam ->
               let fam = foi base ifam in
               if is_deleted_family fam then False
               else authorized_age conf base (pget conf base (get_father fam))
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
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, il)) ->
         match pg with
         [ NotesLinks.PgInd _ | NotesLinks.PgFam _ | NotesLinks.PgNotes |
           NotesLinks.PgWizard _ ->
             List.fold_left (fun set s -> StrSet.add s set) set sl
         | NotesLinks.PgMisc s ->
             do { Hashtbl.add misc s sl; set } ])
      StrSet.empty db
  in
  let mark = Hashtbl.create 1 in
  loop (StrSet.elements set) where rec loop =
    fun
    [ [s :: sl] ->
        if Hashtbl.mem mark s then loop sl
        else do {
          Hashtbl.add mark s ();
          let sl1 = try Hashtbl.find misc s with [ Not_found -> [] ] in
          loop (List.rev_append sl1 sl)
        }
    | [] -> () ];
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 ->
           if is_referenced s then [(s, list) :: db2]
           else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) -> alphabetic_order (Name.lower s1) (Name.lower s2))
    db2
};

value print_linked_list conf base pgl =
  tag "ul" begin
    List.iter
      (fun pg ->
         stagn "li" begin
           match pg with
           [ NotesLinks.PgInd ip ->
               let p = pget conf base ip in
               Wserver.wprint "%s%s"
                 (Util.referenced_person_title_text conf base p)
                 (Date.short_dates_text conf base p)
           | NotesLinks.PgFam ifam ->
               let fam = foi base ifam in
               let fath = pget conf base (get_father fam) in
               let moth = pget conf base (get_mother fam) in
               Wserver.wprint "%s%s &amp; %s %s"
                 (Util.referenced_person_title_text conf base fath)
                 (Date.short_dates_text conf base fath)
                 (Util.referenced_person_title_text conf base moth)
                 (Date.short_dates_text conf base moth)
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
                     (transl_nth conf
                        "wizard/wizards/friend/friends/exterior" 0);
                 end;
                 Wserver.wprint " ";
                 stag "a" "href=\"%sm=WIZNOTES;v=%s\"" (commd conf)
                   (code_varenv wizname)
                 begin
                   Wserver.wprint "%s" wizname;
                 end;
               end ];
         end)
      pgl;
  end
;

value print_what_links conf base fnotes =  do {
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
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf True;
  match try Some (List.assoc fnotes db) with [ Not_found -> None ] with
  [ Some pgl -> print_linked_list conf base pgl
  | None -> () ];
   Hutil.trailer conf;
};

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
      [ Some cnt0 -> print_notes_part conf base fnotes title s cnt0
      | None -> print_whole_notes conf base fnotes title s None ] ]
;

value print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  let title _ =
    Wserver.wprint "%s - %s%s" (capitale (transl conf "base notes"))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let (env, s) = read_notes base fnotes in
  Wiki.print_mod_view_page conf True "NOTES" fnotes title env s
;

value update_notes_links_db conf fnotes s =
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
              let link = {NotesLinks.lnTxt = txt; NotesLinks.lnPos = pos} in
              [(key, link) :: list_ind]
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard j _ _ -> loop list_nt list_ind pos j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1) ]
  in
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  NotesLinks.update_db bdir fnotes (list_nt, list_ind)
;

value commit_notes conf base fnotes s =  do {
  let pg =
    if fnotes = "" then NotesLinks.PgNotes
    else NotesLinks.PgMisc fnotes
  in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat
      (Util.base_path [] (conf.bname ^ ".gwb"))
      [base_notes_dir base; fname]
  in
  Mutil.mkdir_p (Filename.dirname fpath);
  try Gwdb.commit_notes base fname s with
  [ Sys_error _ -> do { incorrect_request conf; raise Update.ModErr } ];
  History.record conf base (Def.U_Notes (p_getint conf.env "v") fnotes) "mn";
  update_notes_links_db conf pg s;
};

value print_mod_ok conf base =
  let fname =
    fun
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  let edit_mode _ = if conf.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = read_notes base in
  let commit = commit_notes conf base in
  let string_filter = string_with_macros conf [] in
  let file_path = file_path conf base in
  let wi =
    {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = file_path;
     Wiki.wi_person_exists = person_exists conf base;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
    True
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
      let nbc = if utf_8_db.val then Name.nbc s.[i] else i + 1 in
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
      (if d = "" then
         capitale (Util.translate_eval (transl conf "miscellaneous notes"))
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
               String.sub f (String.length d)
                 (String.length f - String.length d)
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
          tag "li" "class=\"parent\"" begin
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
                   let f = file_path conf base (path_of_fnotes f) in
                   if Sys.file_exists f then "" else " style=\"color:red\""
                 in
                 tag "li" "class=\"file\"" begin
                   Wserver.wprint "<tt>[";
                   stag "a" "href=\"%sm=NOTES;f=%s\"%s" (commd conf) f c begin
                     Wserver.wprint "%s" r;
                   end;
                   Wserver.wprint "]</tt>%s\n"
                     (if txt = "" then "" else " : " ^ txt);
                 end
             | None ->
                 tag "li" "class=\"folder\"" begin
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
    if d = "" then print_search_form conf None else ();
    trailer conf;
  }
;

(* searching *)

value search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let db =
    let db = notes_links_db conf base True in
    let db = ["" :: List.map fst db] in
    match p_getenv conf.env "z" with
    [ None -> db
    | Some f ->
        loop db where rec loop =
          fun
          [ [fnotes :: list] -> if f = fnotes then list else loop list
          | [] -> [] ] ]
  in
  let noteo =
    loop db where rec loop =
      fun
      [ [fnotes :: list] ->
          let (nenv, nt) = read_notes base fnotes in
          let tit = try List.assoc "TITLE" nenv with [ Not_found -> "" ] in
          if in_text case_sens s tit || in_text case_sens s nt then
            Some (fnotes, tit, nt)
          else loop list
      | [] -> None ]
  in
  match noteo with
  [ Some (fnotes, tit, nt) ->
      print_whole_notes conf base fnotes tit nt (Some (case_sens, s))
  | None -> print_misc_notes conf base ]
;

value print_misc_notes_search conf base =
  match try Some (List.assoc "s" conf.env) with [ Not_found -> None ] with
  [ Some s -> search_text conf base (Wserver.gen_decode False s)
  | None -> print_misc_notes conf base ]
;
