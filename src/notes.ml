(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.25 2005-06-03 03:22:22 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value first_cnt = 1;

value rec rev_syntax_lists list =
  fun
  [ [s :: rsl] ->
      if String.length s > 0 && s.[0] = '*' then rev_syntax_ul list [s :: rsl]
      else rev_syntax_lists [s :: list] rsl
  | [] -> list ]
and rev_syntax_ul list rsl =
  let (list, rest) =
    loop ["</ul>" :: list] rsl where rec loop list =
      fun
      [ [s :: rsl] ->
          let len = String.length s in
          if len > 0 && s.[0] = '*' then
            loop ["<li>" ^ String.sub s 1 (len - 1) ^ "</li>" :: list] rsl
          else (list, rsl)
      | [] -> (list, []) ]
  in
  rev_syntax_lists ["<ul>" :: list] rest
;

value syntax_links conf s =
  let slen = String.length s in
  loop 0 0 where rec loop i len =
    if i = slen then Buff.get len
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
          Printf.sprintf "<a href=\"%sp=%s;n=%s%s\">%s</a>" (commd conf)
            (code_varenv fn) (code_varenv sn)
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
    loop [] 0 first_cnt lines where rec loop lines lev cnt =
      fun
      [ [s :: sl] ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            let nlev = section_level s len in
            if cnt = v then loop [""; sub_part :: lines] nlev (cnt + 1) sl
            else if cnt > v then
              if nlev > lev then loop lines lev (cnt + 1) sl
              else (lines, [s :: sl])
            else loop [s :: lines] lev (cnt + 1) sl
          else if cnt <= v then loop [s :: lines] lev cnt sl
          else loop lines lev cnt sl
      | [] -> (lines, []) ]
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

value make_summary conf lines =
  let tab lev = String.make (2 * lev) ' ' in
  let (rev_summary, lev, _, _) =
    let ul = "<ul style=\"list-style:none\">" in
    List.fold_left
      (fun (summary, lev, indent_stack, cnt) s ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          let slev = section_level s len in
          let (summary, lev, stack) =
            loop summary lev indent_stack where rec loop summary lev stack =
              match stack with
              [ [(prev_num, prev_slev) :: rest_stack] ->
                  if slev < prev_slev then
                    match rest_stack with
                    [ [(_, prev_prev_slev) :: _] ->
                        if slev > prev_prev_slev then
                          let stack = [(prev_num + 1, slev) :: rest_stack] in
                          loop summary lev stack
                        else
                          let summary = [tab (lev - 1) ^ "</li>" :: summary] in
                          let summary = [tab (lev - 1) ^ "</ul>" :: summary] in
                          loop summary (lev - 1) rest_stack
                    | [] ->
                        let summary = [tab (lev - 1) ^ "</li>" :: summary] in
                        let stack = [(prev_num + 1, slev) :: rest_stack] in
                        (summary, lev - 1, stack) ]
                  else if slev = prev_slev then
                    let summary = [tab (lev - 1) ^ "</li>" :: summary] in
                    let stack = [(prev_num + 1, slev) :: rest_stack] in
                    (summary, lev - 1, stack)
                  else
                    let summary = [tab lev ^ ul :: summary] in
                    let stack = [(1, slev) :: stack] in
                    (summary, lev, stack)
              | [] ->
                  let summary = [tab lev ^ ul :: summary] in
                  let stack = [(1, slev) :: stack] in
                  (summary, lev, stack) ]
          in
          let summary = [tab lev ^ "<li>" :: summary] in
          let s =
            let nums = List.map fst stack in
            Printf.sprintf "<a href=\"#a_%d\">%s %s</a>"
              cnt (String.concat "." (List.rev_map string_of_int nums))
              (String.sub s slev (len - 2 * slev))
          in
          let summary = [tab (lev + 1) ^ s :: summary] in
          (summary, lev + 1, stack, cnt + 1)
        else (summary, lev, indent_stack, cnt))
      ([], 0, [], first_cnt) lines
  in
  let rev_summary =
    loop lev rev_summary where rec loop lev summary =
      if lev > 0 then
        let summary = [tab (lev - 1) ^ "</li>" :: summary] in
        let summary = [tab (lev - 1) ^ "</ul>" :: summary] in
        loop (lev - 1) summary
      else summary
  in
  if rev_summary <> [] then
    ["<dl><dd>";
     "<table border=\"1\"><tr><td>";
     "<table><tr>";
     "<td align=\"center\"><b>" ^ capitale (transl conf "summary") ^
       "</b></td>";
     "</tr><tr><td>" ::
     List.rev_append rev_summary
       ["</td><td>";
        "<ul style=\"list-style:none\"><li>&nbsp;</li></ul>";
        "</td></tr></table"; "</td></tr></table>";
        "</dd></dl>"]]
  else []
;

value make_lines_after_summary conf cnt0 lines =
  let (rev_lines_after_summary, _) =
    List.fold_left
      (fun (lines, cnt) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let lev = section_level s len in
           let s =
             Printf.sprintf "<h%d>%s%s</h%d>" lev
               (String.sub s lev (len-2*lev))
               (if lev <= 3 then "<hr" ^ conf.xhs ^ ">" else "") lev
           in
           let n1 =
             if conf.wizard then
               Printf.sprintf
                 "<div style=\"float:right;margin-left:5px\">\
                  [<a href=\"%sm=MOD_NOTES;v=%d\">%s</a>]</div>"
                 (commd conf) cnt (transl_decline conf "modify" "")
             else ""
           in
           let n2 =
             Printf.sprintf "<p><a name=\"a_%d\" id=\"a_%d\"></a></p>"
               cnt cnt
           in
           ([s; n1; n2 :: lines], cnt + 1)
         else ([s :: lines], cnt))
      ([], cnt0) lines
  in
  rev_syntax_lists [] rev_lines_after_summary
;

value html_of_structure conf s =
  let lines = lines_list_of_string s in
  let summary = make_summary conf lines in
  let (rev_lines_before_summary, lines) =
    loop [] lines where rec loop lines_bef =
      fun
      [ [s :: sl] ->
          if String.length s > 1 && s.[0] = '=' then (lines_bef, [s :: sl])
          else loop [s :: lines_bef] sl
      | [] -> (lines_bef, []) ]
  in
  let lines_before_summary = rev_syntax_lists [] rev_lines_before_summary in
  let lines_after_summary = make_lines_after_summary conf first_cnt lines in
  let s =
    syntax_links conf
      (String.concat "\n"
        (lines_before_summary @ summary @ lines_after_summary))
  in
  if conf.wizard then
    Printf.sprintf "%s[<a href=\"%sm=MOD_NOTES\">%s</a>]%s\n"
      (if s = "" then "<p>" else "<div style=\"float:right;margin-left:5px\">")
      (commd conf) (transl_decline conf "modify" "")
      (if s = "" then "</p>" else "</div>") ^
    s
  else s
;

value print_sub_part conf cnt0 lines =
  let lines = make_lines_after_summary conf cnt0 lines in
  let s = syntax_links conf (String.concat "\n" lines) in
  let s = string_with_macros conf False [] s in
  do {
    tag "p" begin
      if cnt0 > 0 then do {
        stag "a" "href=\"%sm=NOTES;v=%d\"" (commd conf) (cnt0 - 1) begin
          Wserver.wprint "&lt;&lt;";
        end;
        Wserver.wprint "\n";
      }
      else ();
      stag "a" "href=\"%sm=NOTES;v=%d\"" (commd conf) (cnt0 + 1) begin
        Wserver.wprint "&gt;&gt;";
      end;
      Wserver.wprint "\n";
    end;
    Wserver.wprint "%s\n" s
  }
;

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1))) conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf False;
    html_p conf;
    match p_getint conf.env "v" with
    [ Some cnt0 ->
        let lines = List.rev (rev_extract_sub_part s cnt0) in
        print_sub_part conf cnt0 lines
    | None ->
        let s = html_of_structure conf s in
        let s = string_with_macros conf False [] s in
        Wserver.wprint "%s\n" s ];
    trailer conf;
  }
;

value print_mod conf base =
  let title _ =
    let s = transl_nth conf "note/notes" 1 in
    Wserver.wprint "%s - %s" (capitale (transl_decline conf "modify" s))
      conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  let v =
    match p_getint conf.env "v" with
    [ Some v -> v
    | None -> first_cnt - 1 ]
  in
  let sub_part = if v = first_cnt - 1 then s else extract_sub_part s v in
  do {
    header conf title;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_NOTES_OK\"";
        if v >= 0 then
          xtag "input" "type=\"hidden\" name=\"v\" value=\"%d\"" v
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

value print_ok conf base s =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "notes modified"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<a href=\"%sm=NOTES\">%s</a>\n" (commd conf)
      (capitale (transl_nth conf "note/notes" 1));
    History.record conf base None "mn";
    match p_getint conf.env "v" with
    [ Some cnt0 -> print_sub_part conf cnt0 (lines_list_of_string s)
    | _ -> () ];
    trailer conf
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
  let old_notes = base.data.bnotes.nread 0 in
  try
    if digest <> Iovalue.digest old_notes then Update.error_digest conf base
    else
      let s =
        match p_getint conf.env "v" with
        [ Some v -> insert_sub_part old_notes v sub_part
        | None -> sub_part ]
      in
      do { base.func.commit_notes s; print_ok conf base sub_part }
  with
  [ Update.ModErr -> () ]
;
