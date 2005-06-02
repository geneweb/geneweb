(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.20 2005-06-02 16:43:02 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value linkify conf s =
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
          let l = String.index_from b k '|'in
          let fn = String.sub b k (l - k) in
          let k = l + 1 in
          let l = String.index_from b k '|'in
          let sn = String.sub b k (l - k) in
          let (oc, name) =
            try
              let k = l + 1 in
              let l = String.index_from b k '|' in
              let x = String.sub b k (l - k) in
              (x, String.sub b (l + 1) (String.length b - l - 1))
            with
            [ Not_found ->
                ("", String.sub b (l + 1) (String.length b - l - 1)) ]
          in
          Printf.sprintf "<a href=\"%sp=%s;n=%s%s\">%s</a>" (commd conf)
            fn sn (if oc = "" then "" else ";oc=" ^ oc) name
        with
        [ Scanf.Scan_failure _ -> "[[" ^ b ^ "]]" ]
      in
      loop j (Buff.mstore len t)
    else loop (i + 1) (Buff.store len s.[i])
;

value section_level s len =
  if len > 4 && s.[1] = '=' && s.[len-2] = '=' then
    if len > 6 && s.[2] = '=' && s.[len-3] = '=' then
      if len > 8 && s.[3] = '=' && s.[len-4] = '=' then 4
      else 3
    else 2
  else 1
;

value insert_sub_part s v sub_part =
  let lines =
    loop [] 0 0 where rec loop lines len i =
      if i = String.length s then
        List.rev (if len = 0 then lines else [Buff.get len :: lines])
      else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
      else loop lines (Buff.store len s.[i]) (i + 1)
  in
  let (lines, sl) =
    loop [] 0 0 lines where rec loop lines lev cnt =
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

value extract_sub_part s v =
  let lines =
    loop [] 0 0 where rec loop lines len i =
      if i = String.length s then
        List.rev (if len = 0 then lines else [Buff.get len :: lines])
      else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
      else loop lines (Buff.store len s.[i]) (i + 1)
  in
  let lines =
    loop [] 0 0 lines where rec loop lines lev cnt =
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
  in
  String.concat "\n" (List.rev lines)
;

value html_of_structure conf s =
  let lines =
    loop [] 0 0 where rec loop lines len i =
      if i = String.length s then
        List.rev (if len = 0 then lines else [Buff.get len :: lines])
      else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
      else loop lines (Buff.store len s.[i]) (i + 1)
  in
  let tab lev = String.make (2 * lev) ' ' in
  let (rev_index, lev, _, _) =
    let ul = "<ul style=\"list-style:none\">" in
    List.fold_left
      (fun (index, lev, indent_stack, cnt) s ->
        let len = String.length s in
        if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
          let slev = section_level s len in
          let (index, lev, stack) =
            loop index lev indent_stack where rec loop index lev stack =
              match stack with
              [ [(prev_num, prev_slev) :: rest_stack] ->
                  if slev < prev_slev then
                    match rest_stack with
                    [ [(_, prev_prev_slev) :: _] ->
                        if slev > prev_prev_slev then
                          let stack = [(prev_num + 1, slev) :: rest_stack] in
                          loop index lev stack
                        else
                          let index = [tab (lev - 1) ^ "</li>" :: index] in
                          let index = [tab (lev - 1) ^ "</ul>" :: index] in
                          loop index (lev - 1) rest_stack
                    | [] ->
                        let index = [tab (lev - 1) ^ "</li>" :: index] in
                        let stack = [(prev_num + 1, slev) :: rest_stack] in
                        (index, lev - 1, stack) ]
                  else if slev = prev_slev then
                    let index = [tab (lev - 1) ^ "</li>" :: index] in
                    let stack = [(prev_num + 1, slev) :: rest_stack] in
                    (index, lev - 1, stack)
                  else
                    let index = [tab lev ^ ul :: index] in
                    let stack = [(1, slev) :: stack] in
                    (index, lev, stack)
              | [] ->
                  let index = [tab lev ^ ul :: index] in
                  let stack = [(1, slev) :: stack] in
                  (index, lev, stack) ]
          in
          let index = [tab lev ^ "<li>" :: index] in
          let s =
            let nums = List.map fst stack in
            Printf.sprintf "<a href=\"#a_%d\">%s %s</a>"
              cnt (String.concat "." (List.rev_map string_of_int nums))
              (String.sub s slev (len - 2 * slev))
          in
          let index = [tab (lev + 1) ^ s :: index] in
          (index, lev + 1, stack, cnt + 1)
        else (index, lev, indent_stack, cnt))
      ([], 0, [], 0) lines
  in
  let rev_index =
    loop lev rev_index where rec loop lev index =
      if lev > 0 then
        let index = [tab (lev - 1) ^ "</li>" :: index] in
        let index = [tab (lev - 1) ^ "</ul>" :: index] in
        loop (lev - 1) index
      else index
  in
  let index =
    if rev_index <> [] then
      ["<dl><dd>";
       "<table border=\"1\"><tr><td>";
       "<table><tr>";
       "<td align=\"center\"><b>" ^ capitale (transl conf "summary") ^
         "</b></td>";
       "</tr><tr><td>" ::
       List.rev_append rev_index
         ["</td><td>";
          "<ul style=\"list-style:none\"><li>&nbsp;</li></ul>";
          "</td></tr></table"; "</td></tr></table>";
          "</dd></dl>"]]
    else []
  in
  let (lines_before_index, lines) =
    loop [] lines where rec loop lines_bef =
      fun
      [ [s :: sl] ->
          if String.length s > 1 && s.[0] = '=' then (lines_bef, [s :: sl])
          else loop [s :: lines_bef] sl
      | [] -> (lines_bef, []) ]
  in
  let (lines_after_index, _) =
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
      ([], 0) lines
  in
  let s =
    linkify conf
      (String.concat "\n"
        (List.rev_append lines_before_index
          (index @ List.rev lines_after_index)))
  in
  if conf.wizard then
    Printf.sprintf "%s[<a href=\"%sm=MOD_NOTES\">%s</a>]%s\n"
      (if s = "" then "<p>" else "<div style=\"float:right;margin-left:5px\">")
      (commd conf) (transl_decline conf "modify" "")
      (if s = "" then "</p>" else "</div>") ^
    s
  else s
;

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1))) conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  let s = html_of_structure conf s in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf False;
    html_p conf;
    Wserver.wprint "%s\n" (string_with_macros conf False [] s);
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
    | None -> -1 ]
  in
  let sub_part = if v = -1 then s else extract_sub_part s v in
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

value print_ok conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "notes modified"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<a href=\"%sm=NOTES\">%s</a>" (commd conf)
      (capitale (transl_nth conf "note/notes" 1));
    History.record conf base None "mn";
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
      do { base.func.commit_notes s; print_ok conf base }
  with
  [ Update.ModErr -> () ]
;
