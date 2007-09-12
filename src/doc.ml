(* camlp5r ./pa_html.cmo *)
(* $Id: doc.ml,v 5.9 2007-09-12 09:58:44 ddr Exp $ *)

open Config;

value notify_change_wdoc = ref "";

value start_with s i p =
  i + String.length p < String.length s &&
  String.lowercase (String.sub s i (String.length p)) = p
;

value last_is s i p =
  loop i (String.length p - 1) where rec loop i k =
    if i <= 0 then False
    else if k < 0 then True
    else
      let c = Char.lowercase s.[i] in
      let c = if c = '\n' || c = '\r' then ' ' else c in
      if c = ' ' && p.[k] = ' ' then
        let rec loop1 i =
          if i <= 0 then False
          else
            match s.[i] with
            [ '\n' | '\r' | ' ' -> loop1 (i - 1)
            | _ -> loop i (k - 1) ]
        in
        loop1 (i - 1)
      else if c = p.[k] then loop (i - 1) (k - 1)
      else False
;

value http = "http://";

value url_basename name =
  try
    let p = String.rindex name '/' + 1 in
    String.sub name p (String.length name - p)
  with
  [ Not_found -> name ]
;

value url_dirname name =
  try
    match String.rindex name '/' with
    [ 0 -> "/"
    | n -> String.sub name 0 n ]
  with
  [ Not_found -> "." ]
;

value string_contains s ss =
  let sslen = String.length ss in
  let mlen = String.length s - sslen in
  loop 0 where rec loop i =
    if i >= mlen then False
    else if String.sub s i sslen = ss then True
    else loop (i + 1)
;

value url_is_relative n = String.length n < 1 || n.[0] <> '/';

value url_is_implicit n =
  url_is_relative n && not (string_contains n "./") &&
  not (string_contains n "../") && not (string_contains n ".\\") &&
  not (string_contains n "..\\") && not (string_contains n ":") &&
  not (string_contains n "::")
;

value copy conf pref_doc pref_img s =
  loop 0 where rec loop i =
    if i = String.length s then ()
    else if last_is s i "<a href=" then do {
      let i = do { Wserver.wprint "="; i + 1 } in
      let i = if s.[i] = '"' then do { Wserver.wprint "\""; i + 1 } else i in
      if s.[i] = '#' || start_with s i http || start_with s i "mailto:" then
        ()
      else Wserver.wprint "%s" pref_doc;
      loop i
    }
    else if last_is s i " src=" || last_is s i " background=" then do {
      let i = do { Wserver.wprint "="; i + 1 } in
      let (img, i) =
        if s.[i] = '"' then
          let rec loop i len =
            if i = String.length s then (Buff.get len, i)
            else if s.[i] = '"' then (Buff.get len, i + 1)
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop (i + 1) 0
        else
          let rec loop i len =
            if i = String.length s then (Buff.get len, i)
            else if s.[i] = '>' then (Buff.get len, i)
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop (i + 1) 0
      in
      let img = url_basename img in
      Wserver.wprint "\"%s%s\"" pref_img img;
      loop i
    }
    else if last_is s i "<body>" then do {
      Wserver.wprint "%s>" (Util.body_prop conf); loop (i + 1)
    }
    else do { Wserver.wprint "%c" s.[i]; loop (i + 1) }
;

value mac_name_of_url_name s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = '/' then loop (Buff.store len ':') (i + 1)
    else loop (Buff.store len s.[i]) (i + 1)
;

value print conf =
  let v =
    match Util.p_getenv conf.env "v" with
    [ Some f -> f
    | None -> "" ]
  in
  let v = if v = "" then "index.htm" else v in
  if url_is_implicit v then
    let fname = if Sys.os_type = "MacOS" then mac_name_of_url_name v else v in
    let fname =
      if Filename.check_suffix fname ".htm" then fname else fname ^ ".htm"
    in
    let fname = Util.search_in_doc_path fname in
    match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
    [ Some ic ->
        do {
          Util.html conf;
          Util.nl ();
          let s =
            let len = ref 0 in
            do {
              try
                let rec loop () =
                  do {
                    len.val := Buff.store len.val (input_char ic); loop ()
                  }
                in
                loop ()
              with
              [ End_of_file -> close_in ic ];
              Buff.get len.val
            }
          in
          let pref_doc =
            let dir = url_dirname v ^ "/" in
            let dir = if dir = "./" then "" else dir in
            conf.indep_command ^ "m=DOC;v=" ^ dir
          in
          let pref_img =
            if Util.images_url.val <> "" then Util.images_url.val ^ "/"
            else conf.indep_command ^ "m=IM;v=/"
          in
          copy conf pref_doc pref_img s
        }
    | None -> Hutil.incorrect_request conf ]
  else Hutil.incorrect_request conf
;

(* Writable (ou Wiki) Doc *)

open TemplAst;

value wdoc_file_path lang fname =
  let dir = Util.search_in_doc_path "wdoc" in
  if lang = "" then Filename.concat dir (fname ^ ".txt")
  else List.fold_right Filename.concat [dir; lang] (fname ^ ".txt")
;

value read_wdoc lang fname =
  let fname = wdoc_file_path lang fname in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let s =
        let len = ref 0 in
        do {
          try
            let rec loop () =
              do {
                len.val := Buff.store len.val (input_char ic); loop ()
              }
            in
            loop ()
          with
          [ End_of_file -> close_in ic ];
          Buff.get len.val
        }
      in
      Wiki.split_title_and_text s
  | None -> ([], "") ]
;

value print_whole_wdoc conf fdoc title s =
  let s = Util.filter_html_tags s in
  let s = "<br /><br />\n" ^ s in
  let s =
    let edit_opt = Some (conf.wizard, "WDOC", fdoc) in
    let wi =
      {Wiki.wi_mode = "WDOC"; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = wdoc_file_path conf.lang;
       Wiki.wi_person_exists _ = True;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.html_with_summary_of_tlsw conf wi edit_opt s
  in
  let fname =
    let f = Filename.concat "wdoc" "wdoc.txt" in
    Util.search_in_doc_path f
  in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        Util.html conf;
        Util.nl ();
        let env = [("title", title); ("doc", s); ("page", fdoc)] in
        Templ.copy_from_templ conf env ic;
      }
  | None ->
      let title _ = Wserver.wprint "Error" in
      do {
        Hutil.header conf title;
        Wserver.wprint "<ul>\n<li>\n";
        Wserver.wprint "Cannot access file \"wdoc.txt\".\n";
        Wserver.wprint "</li>\n</ul>\n";
        Hutil.trailer conf;
        raise Exit
      } ]
;

value print_part_wdoc conf fdoc title s cnt0 =
  do {
    Hutil.header_no_page_title conf (fun _ -> Wserver.wprint "%s" title);
    let s = Util.filter_html_tags s in
    let lines = Wiki.extract_sub_part s cnt0 in
    let lines = if cnt0 = 0 then [title; "<br /><br />" :: lines] else lines in
    let mode = "WDOC" in
    let file_path = wdoc_file_path conf.lang in
    let wi =
      {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
       Wiki.wi_file_path = file_path;
       Wiki.wi_person_exists _ = True;
       Wiki.wi_always_show_link = conf.wizard || conf.friend}
    in
    Wiki.print_sub_part conf wi conf.wizard mode fdoc cnt0 lines;
    Hutil.trailer conf;
  }
;

value print_wdoc_dir conf files =
  let () = Array.sort compare files in
  let s =
    loop 0 0 where rec loop len i =
      if i = Array.length files then Buff.get len
      else
        let f = files.(i) in
        if Filename.check_suffix f ".txt" then
          let f = Filename.chop_suffix f ".txt" in
          let s =
            Printf.sprintf "<li><a href=\"%sm=WDOC;f=%s\">%s</a></li>\n"
              (Util.commd conf) f f
          in
          loop (Buff.mstore len s) (i + 1)
        else loop len (i + 1)
  in
  let s = if s = "" then s else "<ul>\n" ^ s ^ "</ul>" in
  print_whole_wdoc conf "" conf.lang s
;

value print_wdoc_not_this_lang conf =
  print_whole_wdoc conf "" (Printf.sprintf "(%s)" conf.lang) ""
;

value print_wdoc_main conf =
  let dname =
    let dir = Util.search_in_doc_path "wdoc" in
    Filename.concat dir conf.lang
  in
  try print_wdoc_dir conf (Sys.readdir dname) with
  [ Sys_error _ -> print_wdoc_not_this_lang conf ]
;

value print_wdoc conf =
  let conf = {(conf) with cancel_links = True} in
  let fdocp =
    match Util.p_getenv conf.env "f" with
    [ Some f -> f
    | None -> "" ]
  in
  let fdoc = if fdocp = "" then "index" else fdocp in
  let (env, s) = read_wdoc conf.lang fdoc in
  let title = try List.assoc "TITLE" env with [ Not_found -> "" ] in
  if s = "" && fdocp = "" then
    print_wdoc_main conf
  else
    match Util.p_getint conf.env "v" with
    [ Some cnt0 -> print_part_wdoc conf fdoc title s cnt0
    | None -> print_whole_wdoc conf fdocp title s ]
;

value print_mod_wdoc conf =
  let conf = {(conf) with cancel_links = True} in
  let fname =
    match Util.p_getenv conf.env "f" with
    [ Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> "" ]
  in
  let fname = if fname = "" then "index" else fname in
  let cnt =
    match Util.p_getenv conf.env "v" with
    [ Some cnt -> cnt
    | None -> "" ]
  in
  let title _ =
    Wserver.wprint "%s - (%s)"
      (Util.capitale (Util.transl_decline conf "modify" ""))
      (fname ^ (if cnt = "" then "" else " #" ^ cnt))
  in
  let (env, s) = read_wdoc conf.lang fname in
  Wiki.print_mod_view_page conf True "WDOC" fname title env s
;

value commit_wdoc conf file_path fdoc s =
  let fname = wdoc_file_path conf.lang fdoc in
  do {
    try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
    try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
    if s = "" then ()
    else do {
      let dir = Util.search_in_doc_path "wdoc" in
      try Unix.mkdir (Filename.concat dir conf.lang) 0o755 with _ -> ();
      let oc = Secure.open_out fname in
      output_string oc s;
      output_char oc '\n';
      close_out oc;
      IFDEF UNIX THEN
        if notify_change_wdoc.val <> "" then
          let comm = notify_change_wdoc.val in
          let args = [| comm; fname; conf.lang; fdoc |] in
          match Unix.fork () with
          [ 0 ->
              if Unix.fork () <> 0 then exit 0
              else do {
                try Unix.execvp comm args with _ -> ();
                exit 0
              }
          | id -> ignore (Unix.waitpid [] id) ]
        else ()
      ELSE () END;
    }
  }
;

value print_mod_wdoc_ok conf base =
  let fname =
    fun
    [ Some f -> if NotesLinks.check_file_name f <> None then f else "index"
    | None -> "index" ]
  in
  let edit_mode _ = if conf.wizard then Some "WDOC" else None in
  let mode = "WDOC" in
  let read_string = read_wdoc conf.lang in
  let commit = commit_wdoc conf base in
  let string_filter = Util.filter_html_tags in
  let file_path = wdoc_file_path conf.lang in
  let wi =
    {Wiki.wi_mode = mode; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = file_path; Wiki.wi_person_exists _ = True;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
    True
;
