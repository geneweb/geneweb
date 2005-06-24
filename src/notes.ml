(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.70 2005-06-24 21:00:27 ddr Exp $ *)

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
   [[[notes_subfile]]] link to a sub-file; 'notes_subfile' displayed *)

module Buff = Buff.Make (struct value buff = ref (String.create 80); end);

value first_cnt = 1;

value tab lev s = String.make (2 * lev) ' ' ^ s;

value rec syntax_lists conf list =
  fun
  [ [s :: sl] ->
      if String.length s > 0 && s.[0] = '*' then
        let (sl, rest) = select_list_lines conf [] [s :: sl] in
        let list = syntax_ul 0 list sl in
        syntax_lists conf list rest
      else syntax_lists conf [s :: list] sl
  | [] -> List.rev list ]
and select_list_lines conf list =
  fun
  [ [s :: sl] ->
      let len = String.length s in
      if len > 0 && s.[0] = '*' then
        let s = String.sub s 1 (len - 1) in
        let (s, sl) =
          loop s sl where rec loop s1 =
            fun
            [ [""; s :: sl]
              when String.length s > 1 && s.[0] = '*' && s.[1] = '*' ->
                let br = "<br" ^ conf.xhs ^ ">" in
                loop (s1 ^ br ^ br) [s :: sl]
            | [s :: sl] ->
                if String.length s > 0 && s.[0] <> '*' then
                  loop (s1 ^ "\n" ^ s) sl
                else (s1, [s :: sl])
            | [] -> (s1, []) ]
        in
        select_list_lines conf [s :: list] sl
      else (List.rev list, [s :: sl])
  | [] -> (List.rev list, []) ]
and syntax_ul lev list sl =
  let list = [tab lev "<ul>" :: list] in
  let list =
    loop list sl where rec loop list =
      fun
      [ [s1; s2 :: sl] ->
          if String.length s2 > 0 && s2.[0] = '*' then
            let list = [tab lev "<li>" ^ s1 :: list] in
            let (list2, sl) =
              loop [] [s2 :: sl] where rec loop list =
                fun
                [ [s :: sl] ->
                    if String.length s > 0 && s.[0] = '*' then
                      let s = String.sub s 1 (String.length s - 1) in
                      loop [s :: list] sl
                    else (list, [s :: sl])
                | [] -> (list, []) ]
            in
            let list = syntax_ul (lev + 1) list (List.rev list2) in
            loop [tab lev "</li>" :: list] sl
          else
            loop [tab lev "<li>" ^ s1 ^ "</li>" :: list] [s2 :: sl]
      | [s] -> [tab lev "<li>" ^ s ^ "</li>" :: list]
      | [] -> list ]
  in
  [tab lev "</ul>" :: list]
;

value rev_syntax_lists conf list rev_list =
  syntax_lists conf list (List.rev rev_list)
;

value check_file_name s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '_' -> loop (i + 1)
      | _ -> False ]
;

value ext_file_link s i =
  let slen = String.length s in
  let j =
    loop (i + 3) where rec loop j =
      if j = slen then j
      else if
        j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
      then j + 3
      else loop (j + 1)
  in
  if j > i + 6 then
    let b = String.sub s (i + 3) (j - i - 6) in
    let (fname, sname, text) =
      try
        let k = String.index b '/' in
        let j = try String.index b '#' with [ Not_found -> k ] in
        (String.sub b 0 j, String.sub b j (k - j),
         String.sub b (k + 1) (String.length b - k - 1))
      with
      [ Not_found -> (b, "", b) ]
    in
    if check_file_name fname then Some (j, fname, sname, text)
    else None
  else None
;

value file_path conf fname =
  List.fold_right Filename.concat
    [Util.base_path [] (conf.bname ^ ".gwb"); "notes_d"]
    (fname ^ ".txt")
;

value syntax_links conf mode file_path s =
  let slen = String.length s in
  loop 0 0 where rec loop i len =
    if i = slen then Buff.get len
    else if s.[i] = '{' then
      let j =
        loop (i + 1) where rec loop j =
          if j = slen then j
          else if s.[j] = '}' then j + 1
          else loop (j + 1)
      in
      let b = String.sub s (i + 1) (j - i - 2) in
      loop j (Buff.mstore len (sprintf "<span class=\"highlight\">%s</span>" b))
    else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
      match ext_file_link s i with
      [ Some (j, fname, sname, text) ->
          let c =
            let f = file_path conf fname in
            if Sys.file_exists f then "" else " style=\"color:red\""
          in
          let t =
            sprintf "<a href=\"%sm=%s;f=%s%s\"%s>%s</a>"
              (commd conf) mode fname sname c text
          in
          loop j (Buff.mstore len t)
      | None -> loop (i + 3) (Buff.mstore len "[[[") ]
    else if i < slen - 1 && s.[i] = '[' && s.[i+1] = '[' then
      let j =
        loop (i + 2) where rec loop j =
          if j = slen then j
          else if j < slen - 1 && s.[j] = ']' && s.[j+1] = ']' then j + 2
          else loop (j + 1)
      in
      let t =
        let b = String.sub s (i + 2) (j - i - 4) in
        try
          let k = 0 in
          let l = String.index_from b k '/' in
          let fn = String.sub b k (l - k) in
          let k = l + 1 in
          let (fn, sn, oc, name) =
            try
              let l = String.index_from b k '/' in
              let sn = String.sub b k (l - k) in
              let (oc, name) =
                try
                  let k = l + 1 in
                  let l = String.index_from b k '/' in
                  let x = String.sub b k (l - k) in
                  (x, String.sub b (l + 1) (String.length b - l - 1))
                with
                [ Not_found ->
                    ("", String.sub b (l + 1) (String.length b - l - 1)) ]
              in
              (fn, sn, oc, name)
            with
            [ Not_found ->
                let sn = String.sub b k (String.length b - k) in
                let name = fn ^ " " ^ sn in
                (fn, sn, "", name) ]
          in
          sprintf "<a href=\"%sp=%s;n=%s%s\">%s</a>" (commd conf)
            (code_varenv (Name.lower fn)) (code_varenv (Name.lower sn))
            (if oc = "" then "" else ";oc=" ^ oc) name
        with
        [ Not_found -> "[[" ^ b ^ "]]" ]
      in
      loop j (Buff.mstore len t)
    else loop (i + 1) (Buff.store len s.[i])
;

value section_level s len =
  loop 1 (len - 2) 4 where rec loop i j k =
    if i > 5 then i
    else if len > k && s.[i] = '=' && s.[j] = '=' then
      loop (i + 1) (j - 1) (k + 2)
    else i
;

value lines_list_of_string s =
  loop [] 0 0 where rec loop lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else [Buff.get len :: lines])
    else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
    else loop lines (Buff.store len s.[i]) (i + 1)
;

value insert_sub_part s v sub_part =
  let lines = lines_list_of_string s in
  let (lines, sl) =
    loop False [] 0 first_cnt lines
    where rec loop sub_part_added lines lev cnt =
      fun
      [ [s :: sl] ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            if v = first_cnt - 1 then
              (if sub_part = "" then [] else [""; sub_part], [s :: sl])
            else
              let nlev = section_level s len in
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
  let lines = lines_list_of_string s in
  loop [] 0 first_cnt lines where rec loop lines lev cnt =
    fun
    [ [s :: sl] ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          if v = first_cnt - 1 then lines
          else
            let nlev = section_level s len in
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

value summary_of_tlsw_lines conf lines =
  let (rev_summary, lev, _, cnt, rev_sections_nums) =
    List.fold_left
      (fun (summary, lev, indent_stack, cnt, sections_nums) s ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          let slev = section_level s len in
          let (lev, stack) =
            loop lev indent_stack where rec loop lev stack =
              match stack with
              [ [(prev_num, prev_slev) :: rest_stack] ->
                  if slev < prev_slev then
                    match rest_stack with
                    [ [(_, prev_prev_slev) :: _] ->
                        if slev > prev_prev_slev then
                          let stack = [(prev_num, slev) :: rest_stack] in
                          loop lev stack
                        else
                          loop (lev - 1) rest_stack
                    | [] ->
                        let stack = [(prev_num + 1, slev) :: rest_stack] in
                        (lev - 1, stack) ]
                  else if slev = prev_slev then
                    let stack = [(prev_num + 1, slev) :: rest_stack] in
                    (lev - 1, stack)
                  else
                    let stack = [(1, slev) :: stack] in
                    (lev, stack)
              | [] ->
                  let stack = [(1, slev) :: stack] in
                  (lev, stack) ]
          in
          let section_num =
            let nums = List.map fst stack in
            String.concat "." (List.rev_map string_of_int nums)
          in
          let summary =
            let s =
              sprintf "<a href=\"#a_%d\">%s - %s</a>" cnt section_num
                (String.sub s slev (len - 2 * slev))
            in
            let line =
              sprintf "<div style=\"margin-left:%.3fem\">%s</div>"
                (float lev *. 1.618) s
            in
            [line :: summary]
          in
          (summary, lev + 1, stack, cnt + 1, [section_num :: sections_nums])
        else (summary, lev, indent_stack, cnt, sections_nums))
      ([], 0, [], first_cnt, []) lines
  in
  if cnt <= first_cnt + 1 then
    (* less that 2 paragraphs : summary abandonned *)
    ([], [])
  else
    let lines =
      ["<dl><dd>";
       "<table id=\"toc\" border=\"1\" cellpadding=\"10\">";
       "<tr><td align=\"" ^ conf.left ^ "\">";
       "<div style=\"text-align:center\"><b>" ^
          capitale (transl_nth conf "visualize/show/hide/summary" 3) ^ "</b>";
"<script type=\"text/javascript\">
//<![CDATA[
showTocToggle()
//]]>
</script>";
"</div>";
"<div id=\"tocinside\">" ::
       List.rev_append rev_summary
         ["</div>";
          "</td></tr></table>";
          "</dd></dl>"]]
    in
    (lines, List.rev rev_sections_nums)
;

value html_of_tlsw_lines conf mode sub_fname cnt0 with_mod_parag lines
    sections_nums =
  let sfn = if sub_fname = "" then "" else ";f=" ^ sub_fname in
  let (rev_lines, _, _) =
    List.fold_left
      (fun (lines, cnt, sections_nums) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let lev = section_level s len in
           let (section_num, sections_nums) =
             match sections_nums with
             [ [a :: l] -> (a ^ " - ", l)
             | [] -> ("", []) ]
           in
           let s =
             sprintf "<h%d>%s%s%s</h%d>" lev section_num
               (String.sub s lev (len-2*lev))
               (if lev <= 3 then "<hr" ^ conf.xhs ^ ">" else "") lev
           in
           if with_mod_parag then
             let n1 =
               if conf.wizard then
                 sprintf
                   "<div style=\"float:%s;margin-%s:5px\">\
                    (<a href=\"%sm=MOD_%s;v=%d%s\">%s</a>)</div>"
                   conf.right conf.left (commd conf) mode cnt sfn
                     (transl_decline conf "modify" "")
               else ""
             in
             let n2 =
               sprintf "<p><a name=\"a_%d\" id=\"a_%d\"></a></p>" cnt cnt
             in
             ([s; n1; n2 :: lines], cnt + 1, sections_nums)
           else ([s :: lines], cnt + 1, sections_nums)
         else ([s :: lines], cnt, sections_nums))
      ([], max cnt0 first_cnt, sections_nums) lines
  in
  rev_syntax_lists conf [] rev_lines
;

value html_with_summary_of_tlsw conf mode file_path sub_fname s =
  let lines = lines_list_of_string s in
  let (summary, sections_nums) = summary_of_tlsw_lines conf lines in
  let (rev_lines_before_summary, lines) =
    loop [] lines where rec loop lines_bef =
      fun
      [ [s :: sl] ->
          if String.length s > 1 && s.[0] = '=' then (lines_bef, [s :: sl])
          else loop [s :: lines_bef] sl
      | [] -> (lines_bef, []) ]
  in
  let lines_before_summary =
    rev_syntax_lists conf [] rev_lines_before_summary
  in
  let lines_after_summary =
    html_of_tlsw_lines conf mode sub_fname first_cnt True lines sections_nums
  in
  let s =
    syntax_links conf mode file_path
      (String.concat "\n"
        (lines_before_summary @ summary @ lines_after_summary))
  in
  if conf.wizard && (lines_before_summary <> [] || lines = []) then
    sprintf "%s(<a href=\"%sm=MOD_%s;v=0%s\">%s</a>)%s\n"
      (if s = "" then "<p>" else "<div style=\"float:right;margin-left:15px\">")
      (commd conf) mode (if sub_fname = "" then "" else ";f=" ^ sub_fname)
      (transl_decline conf "modify" "")
      (if s = "" then "</p>" else "</div>") ^
    s
  else s
;

value print_sub_part conf mode sub_fname cnt0 s =
  let sfn = if sub_fname = "" then "" else ";f=" ^ sub_fname in
  let s =
    if cnt0 < first_cnt && conf.wizard then
      sprintf "%s(<a href=\"%sm=MOD_%s;v=0%s\">%s</a>)%s\n"
        (if s = "" then "<p>"
         else "<div style=\"float:right;margin-left:5px\">")
        (commd conf) mode sfn (transl_decline conf "modify" "")
        (if s = "" then "</p>" else "</div>") ^
      s
    else s
  in
  do {
    tag "p" begin
      if cnt0 >= first_cnt then do {
        stag "a" "href=\"%sm=%s;v=%d%s\"" (commd conf) mode (cnt0 - 1) sfn begin
          Wserver.wprint "&lt;&lt;";
        end;
        Wserver.wprint "\n";
      }
      else ();
      if cnt0 >= first_cnt - 1 then do {
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
  let lines = html_of_tlsw_lines conf mode sub_fname cnt0 True lines [] in
  let s = syntax_links conf mode file_path (String.concat "\n" lines) in
  let s = string_with_macros conf False [] s in
  print_sub_part conf mode sub_fname cnt0 s
;

value read_notes base fnotes = base.data.bnotes.nread fnotes RnAll;

(**)
open TemplAst;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  ">Notes." ^ func ^ ": not impl " ^ desc ^ "<p>\n"
;

value eval_var conf env loc =
  fun
  [ ["doctype"] -> Util.doctype conf ^ "\n"
  | ["/"] -> conf.xhs
  | [s] -> List.assoc s env
  | _ -> raise Not_found ]
;

value eval_var_handled conf env loc sl =
  try eval_var conf env loc sl with
  [ Not_found -> Printf.sprintf " %%%s?" (String.concat "." sl) ]
;

value print_var conf env loc =
  fun
  [ ["copyright"] -> Util.print_copyright conf
  | sl -> Wserver.wprint "%s" (eval_var_handled conf env loc sl) ]
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
  | Atransl upp s c -> Templ.eval_transl conf upp s c
  | ast -> not_impl "eval_ast" ast ]
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

value copy_from_templ conf ic =
  let astl = Templ.parse_templ conf (Stream.of_channel ic) in
  do {
    close_in ic;
    let env = [] in
    List.iter (print_ast conf env) astl;
  }
;
(**)

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1))) conf.bname
  in
  let fnotes =
    match p_getenv conf.env "f" with
    [ Some f -> if check_file_name f then f else ""
    | None -> "" ]
  in
  let s = read_notes base fnotes in
  do {
    header_no_page_title conf title;
    match Util.open_etc_file "summary" with
    [ Some ic -> copy_from_templ conf ic
    | None -> () ];
    print_link_to_welcome conf False;
    html_p conf;
    match p_getint conf.env "v" with
    [ Some cnt0 ->
        let lines = List.rev (rev_extract_sub_part s cnt0) in
        print_notes_sub_part conf fnotes cnt0 lines
    | None ->
        let s = string_with_macros conf False [] s in
        let s = html_with_summary_of_tlsw conf "NOTES" file_path fnotes s in
        Wserver.wprint "%s\n" s ];
    trailer conf;
  }
;

value print_mod_page conf mode fname title s =
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
        if v >= first_cnt then do {
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
    [ Some f -> if check_file_name f then f else ""
    | None -> "" ]
  in
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s%s" (capitale (transl_decline conf "modify" s))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  let s = read_notes base fnotes in
  print_mod_page conf "NOTES" fnotes title s
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
    let (has_v, v) =
      match get_v with
      [ Some v -> (True, v)
      | None -> (False, 0) ]
    in
    History.record_notes conf base (get_v, fnotes) "mn";
    if has_v then print_notes_sub_part conf fnotes v (lines_list_of_string s)
    else
      let sfn = if fnotes = "" then "" else ";f=" ^ fnotes in
      Wserver.wprint "<a href=\"%sm=NOTES%s\">%s</a>\n" (commd conf) sfn
        (capitale (transl_nth conf "note/notes" 1));
    trailer conf
  }
;

type notes_links_db = list (string * list string);

value update_notes_links_db conf fnotes s =
  let slen = String.length s in
  let list =
    loop [] 0 where rec loop list i =
      if i = slen then list
      else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
        match ext_file_link s i with
        [ Some (j, lfname, _, _) -> loop [lfname :: list] j
        | None -> loop list (i + 3) ]
      else loop list (i + 1)
  in
  if list = [] then ()
  else
    let fname =
      let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
      Filename.concat bfile "notes_links"
    in
    let notes_links_db =
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic ->
          let r = try (input_value ic : notes_links_db) with _ -> [] in
          do { close_in ic ; r }
      | None -> [] ]
    in
    let new_db = [(fnotes, list) :: List.remove_assoc fnotes notes_links_db] in
    let oc = Secure.open_out fname in
    do {
      output_value oc (new_db : notes_links_db);
      close_out oc;
    }
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
    [ Some f -> if check_file_name f then f else ""
    | None -> "" ]
  in
  let old_notes = read_notes base fnotes in
  try
    if digest <> Iovalue.digest old_notes then Update.error_digest conf
    else
      let s =
        match p_getint conf.env "v" with
        [ Some v -> insert_sub_part old_notes v sub_part
        | None -> sub_part ]
      in
      do {
        base.func.commit_notes fnotes s;
        update_notes_links_db conf fnotes s;
        print_ok conf base fnotes sub_part
      }
  with
  [ Update.ModErr -> () ]
;
