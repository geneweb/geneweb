(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.12 2005-06-01 20:34:37 ddr Exp $ *)

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

value html_of_structure conf s =
  let lines =
    loop [] 0 0 where rec loop lines len i =
      if i = String.length s then List.rev lines
      else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
      else loop lines (Buff.store len s.[i]) (i + 1)
  in
  let (_, _, _, _, index) =
    List.fold_left
      (fun (prev_lev, num, nums, cnt, index) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let lev =
             if len > 4 && s.[1] = '=' && s.[len-2] = '=' then
               if len > 6 && s.[2] = '=' && s.[len-3] = '=' then
                 if len > 8 && s.[3] = '=' && s.[len-4] = '=' then 4
                 else 3
               else 2
             else 1
           in             
           let (num, nums) =
             if lev > prev_lev then (0, [num :: nums])
             else if lev < prev_lev then (List.hd nums + 1, List.tl nums)
             else (num + 1, nums)
           in
           let s =
             let inx =
               List.tl
                 (List.rev_map (fun x -> string_of_int (x + 1)) [num :: nums])
             in
             Printf.sprintf "<li><a href=\"#a_%d\">%s %s</a></li>" cnt
               (String.concat "." inx) (String.sub s lev (len - 2 * lev))
           in
           let index =
             if lev > prev_lev then
               [s; "<ul style=\"list-style:none\">" :: index]
             else if lev < prev_lev then
               [s; "</ul>" :: index]
             else [s :: index]
           in
           (lev, num, nums, cnt + 1, index)
         else (prev_lev, num, nums, cnt, index))
      (0, 0, [], 0, []) lines
  in
  let index =
    if index <> [] then
      ["<dl><dd>";
       "<table border=\"1\"><tr><td>";
       "<table><tr>";
       "<td align=\"center\"><b>" ^ capitale (transl conf "summary") ^
         "</b></td>";
       "</tr><tr><td>" ::
       List.rev_append index
         ["</ul>"; "</td><td>";
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
           let lev = 
             if len > 4 && s.[1] = '=' && s.[len-2] = '=' then
               if len > 6 && s.[2] = '=' && s.[len-3] = '=' then
                 if len > 8 && s.[3] = '=' && s.[len-4] = '=' then 4
                 else 3
               else 2
             else 1
           in
           let s =
             Printf.sprintf "<h%d>%s%s</h%d>" lev
               (String.sub s lev (len-2*lev))
               (if lev <= 3 then "<hr" ^ conf.xhs ^ ">" else "") lev
           in
           let n1 =
             if conf.wizard && False (* not implemented *) then
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
           ([s; n2; n1 :: lines], cnt + 1)
         else ([s :: lines], cnt))
      ([], 0) lines
  in
  linkify conf (String.concat "\n" (List.rev lines_before_index)) ^
  String.concat "\n" index ^ "\n" ^
  linkify conf (String.concat "\n" (List.rev lines_after_index))
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
  do {
    header conf title;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_NOTES_OK\"";
        let digest = Iovalue.digest s in
        xtag "input" "type=\"hidden\" name=\"digest\" value=\"%s\"" digest;
        stagn "textarea" "name=\"notes\" rows=\"30\" cols=\"110\"" begin
          if s <> "" then Wserver.wprint "%s" (quote_escaped s) else ();
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
    trailer conf
  }
;

value print_mod_ok conf base =
  let s =
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
    else do { base.func.commit_notes s; print_ok conf base }
  with
  [ Update.ModErr -> () ]
;
