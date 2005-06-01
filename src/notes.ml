(* camlp4r ./pa_html.cmo *)
(* $Id: notes.ml,v 4.10 2005-06-01 13:38:38 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value html_of_structure s =
  let lines =
    loop [] 0 0 where rec loop lines len i =
      if i = String.length s then List.rev lines
      else if s.[i] = '\n' then loop [Buff.get len :: lines] 0 (i + 1)
      else loop lines (Buff.store len s.[i]) (i + 1)
  in
  let (_, _, _, _, index) =
    List.fold_left
      (fun (prev_lev, num, prev_nums, cnt, index) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let lev =
             if len > 4 && s.[1] = '=' && s.[len-2] = '=' then
               if len > 6 && s.[1] = '=' && s.[len-3] = '=' then 3
               else 2
             else 1
           in             
           let (num, nums) =
             if lev > prev_lev then
               (0, [num :: prev_nums])
             else if lev < prev_lev then
               (List.hd prev_nums + 1, List.tl prev_nums)
             else (num + 1, prev_nums)
           in
           let s =
             Printf.sprintf "<li><a href=\"#a_%d\">%d %s</a></li>" cnt
               (num + 1) (String.sub s lev (len - 2 * lev))
           in
           let index =
             if lev > prev_lev then
               [s; "<ul style=\"list-style:none\">" :: index]
             else if lev < prev_lev then
               [s; "</ul>" :: index]
             else [s :: index]
           in
           (lev, num, nums, cnt + 1, index)
         else (prev_lev, num, prev_nums, cnt, index))
      (0, 0, [], 0, []) lines
  in
  let index =
    if index <> [] then
      ["<table border=\"1\"><tr><td>"; "<table><tr><td>" ::
       List.rev_append index
         ["</ul>"; "</td><td>";
          "<ul style=\"list-style:none\"><li>&nbsp;</li></ul>";
          "</td></tr></table"; "</td></tr></table>"]]
    else []
  in
  let (lines, _) =
    List.fold_left
      (fun (lines, cnt) s ->
         let len = String.length s in
         if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
           let lev = 
             if len > 4 && s.[1] = '=' && s.[len-2] = '=' then
               if len > 6 && s.[1] = '=' && s.[len-3] = '=' then 3
               else 2
             else 1
           in
           let s =
             Printf.sprintf "<h%d>%s</h%d>" lev
               (String.sub s lev (len-2*lev)) lev
           in
           let n =
             Printf.sprintf "&nbsp;<p><a name=\"a_%d\" id=\"a_%d\"></a></p>"
               cnt cnt
           in
           ([s; n :: lines], cnt + 1)
         else ([s :: lines], cnt))
      ([], 0) lines
  in
  String.concat "\n" index ^ "\n" ^
  String.concat "\n" (List.rev lines)
;

value print conf base =
  let title _ =
    Wserver.wprint "%s - %s"
      (capitale (nominative (transl_nth conf "note/notes" 1))) conf.bname
  in
  let s = base.data.bnotes.nread 0 in
  let s = html_of_structure s in
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
        stagn "textarea" "name=\"notes\" rows=\"30\" cols=\"70\"" begin
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
