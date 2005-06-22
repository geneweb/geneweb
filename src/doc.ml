(* camlp4r ./pa_html.cmo *)
(* $Id: doc.ml,v 4.16 2005-06-22 11:37:44 ddr Exp $ *)

open Config;

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
    if i == String.length s then ()
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
    if i == String.length s then Buff.get len
    else if s.[i] == '/' then loop (Buff.store len ':') (i + 1)
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
    | None -> Util.incorrect_request conf ]
  else Util.incorrect_request conf
;

(* Writable (ou Wiki) Doc *)

open TemplAst;

value wdoc_file_path conf fname =
  let dir = Util.search_in_doc_path "wdoc" in
  List.fold_right Filename.concat [dir; conf.lang] (fname ^ ".txt")
;

value read_wdoc conf fname =
  let fname = wdoc_file_path conf fname in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let title = try input_line ic with [ End_of_file -> "" ] in
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
      (title, s)
  | None -> ("", "") ]
;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  ">Doc." ^ func ^ ": not impl " ^ desc ^ "<p>\n"
;

value no_lang conf = {(conf) with henv = List.remove_assoc "lang" conf.henv};

value eval_var conf env loc =
  fun
  [ ["doctype"] -> Util.doctype conf ^ "\n"
  | ["prefix_no_lang"] -> Util.commd (no_lang conf)
  | ["/"] -> conf.xhs
  | [s] -> List.assoc s env
  | _ -> raise Not_found ]
;

value eval_var_handled conf env loc sl =
  try eval_var conf env loc sl with
  [ Not_found -> Printf.sprintf " %%%s?" (String.concat "." sl) ]
;

value eval_bool_ast conf env a =
  let eval_var loc sl  = VVstring (eval_var conf env loc sl) in
  let eval_apply _ = raise Not_found in
  Templ.eval_bool_expr conf (eval_var, eval_apply) a
;

value eval_ast conf env =
  fun
  [ Atext s -> s
  | Avar loc s sl -> eval_var_handled conf env loc [s :: sl]
  | ast -> not_impl "eval_ast" ast ]
;

value print_var conf env loc =
  fun
  [ ["copyright"] -> Util.print_copyright conf
  | sl -> Wserver.wprint "%s" (eval_var_handled conf env loc sl) ]
;

value rec print_ast conf env =
  fun
  [ Avar loc s sl -> print_var conf env loc [s :: sl]
  | Aif a1 a2 a3 -> print_if conf env a1 a2 a3
  | ast -> Wserver.wprint "%s" (eval_ast conf env ast) ]
and print_ast_list conf env al =
  List.iter (print_ast conf env) al
and print_if conf env a1 a2 a3 =
  let test =
    try eval_bool_ast conf env a1 with
    [ Failure x -> do { Wserver.wprint "%s" x; False } ]
  in
  print_ast_list conf env (if test then a2 else a3)
;

value print_whole_wdoc conf fdoc title s =
  let s = Util.filter_html_tags True s in
  let s = "<br /><br />\n" ^ s in
  let s = Notes.html_with_summary_of_tlsw conf "WDOC" wdoc_file_path fdoc s in
  let fname =
    let f = Filename.concat "wdoc" "wdoc.txt" in
    Util.search_in_doc_path f
  in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let astl = Templ.parse_templ conf (Stream.of_channel ic) in
      do {
        close_in ic;
        Util.html conf;
        Util.nl ();
        let env = [("title", title); ("doc", s); ("page", fdoc)] in
        List.iter (print_ast conf env) astl;
      }
  | None ->
      let title _ = Wserver.wprint "Error" in
      do {
        Util.header conf title;
        Wserver.wprint "<ul>\n<li>\n";
        Wserver.wprint "Cannot access file \"wdoc.txt\".\n";
        Wserver.wprint "</li>\n</ul>\n";
        Util.trailer conf;
        raise Exit
      } ]
;

value print_wdoc_sub_part conf sub_fname cnt0 lines =
  let mode = "WDOC" in
  let file_path = wdoc_file_path in
  let lines = Notes.html_of_tlsw_lines conf mode sub_fname cnt0 True lines in
  let s = Notes.syntax_links conf mode file_path (String.concat "\n" lines) in
  let s = Util.filter_html_tags True s in
  Notes.print_sub_part conf mode sub_fname cnt0 s
;

value print_part_wdoc conf fdoc title s cnt0 =
  do {
    Util.header_no_page_title conf (fun _ -> Wserver.wprint "%s" title);
    let lines = List.rev (Notes.rev_extract_sub_part s cnt0) in
    let lines =
      if cnt0 = 0 then [title; "<br /><br />" :: lines] else lines
    in
    print_wdoc_sub_part conf fdoc cnt0 lines;
    Util.trailer conf;
  }
;

value print_wdoc_dir conf =
  let dname =
    let dir = Util.search_in_doc_path "wdoc" in
    Filename.concat dir conf.lang
  in
  let files = try Sys.readdir dname with [ Sys_error _ -> [| |] ] in
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

value print_wdoc conf =
  let conf = {(conf) with cancel_links = True} in
  let fdocp =
    match Util.p_getenv conf.env "f" with
    [ Some f -> f
    | None -> "" ]
  in
  let fdoc = if fdocp = "" then "index" else fdocp in
  let (title, s) = read_wdoc conf fdoc in
  if s = "" && fdocp = "" then
    print_wdoc_dir conf
  else
    match Util.p_getint conf.env "v" with
    [ Some cnt0 -> print_part_wdoc conf fdoc title s cnt0
    | None -> print_whole_wdoc conf fdoc title s ]
;

value print_mod_wdoc conf =
  let conf = {(conf) with cancel_links = True} in
  let fname =
    match Util.p_getenv conf.env "f" with
    [ Some f -> if Notes.check_file_name f then f else ""
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
  let (ntitle, s) = read_wdoc conf fname in
  Notes.print_mod_page conf "WDOC" fname title (ntitle ^ "\n" ^ s)
;

value print_ok conf fdoc s =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "notes modified"))
  in
  do {
    Util.header_no_page_title conf title;
    tag "div" "style=\"text-align:center\"" begin
      Wserver.wprint "--- ";
      title ();
      Wserver.wprint " ---\n";
    end;
    let get_v = Util.p_getint conf.env "v" in
    let (has_v, v) =
      match get_v with
      [ Some v -> (True, v)
      | None -> (False, 0) ]
    in
    if has_v then
      let lines = Notes.lines_list_of_string s in
      let lines =
        match lines with
        [ [title :: lines] when v = 0 -> [title; "<br /><br />" :: lines]
        | l -> l ]
      in
      print_wdoc_sub_part conf fdoc v lines
    else
      let sfn = if fdoc = "" then "" else ";f=" ^ fdoc in
      Wserver.wprint "<a href=\"%sm=WDOC%s\">%s</a>\n" (Util.commd conf) sfn
        (Util.capitale (Util.transl_nth conf "note/notes" 1));
    Util.trailer conf
  }
;

value commit_wdoc conf fdoc s =
  let fname = wdoc_file_path conf fdoc in
  do {
    try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
    try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
    if s = "" then ()
    else do {
      let oc = Secure.open_out fname in
      output_string oc s;
      output_char oc '\n';
      close_out oc;
    }
  }
;

value print_mod_wdoc_ok conf =
   let sub_part =
    match Util.p_getenv conf.env "notes" with
    [ Some v -> Gutil.strip_all_trailing_spaces v
    | None -> failwith "notes unbound" ]
  in
  let digest =
    match Util.p_getenv conf.env "digest" with
    [ Some s -> s
    | None -> "" ]
  in
  let fdoc =
    match Util.p_getenv conf.env "f" with
    [ Some f -> if Notes.check_file_name f then f else "index"
    | None -> "index" ]
  in
  let old_doc =
    let (t, s) = read_wdoc conf fdoc in
    t ^ "\n" ^ s
  in
  try
    if digest <> Iovalue.digest old_doc then Update.error_digest conf
    else
      let s =
        match Util.p_getint conf.env "v" with
        [ Some v -> Notes.insert_sub_part old_doc v sub_part
        | None -> sub_part ]
      in
      do {
        commit_wdoc conf fdoc s;
        print_ok conf fdoc sub_part
      }
  with
  [ Update.ModErr -> () ]
;
