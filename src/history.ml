(* camlp4r ./pa_html.cmo *)
(* $Id: history.ml,v 2.12 1999-10-04 08:51:33 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value file_name conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  List.fold_right Filename.concat [Util.base_dir.val; bname] "history"
;

(* Record history when committing updates *)

value ext_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;

value record conf base (fn, sn, occ) action =
  let do_it =
    match p_getenv conf.base_env "history" with
    [ Some "yes" -> True
    | _ -> False ]
  in
  if do_it then
    let fname = file_name conf in
    let oc = open_out_gen ext_flags 0o644 fname in
    let (hh, mm, ss) = conf.time in
    do Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d [%s] %s %s.%d %s\n"
	 conf.today.year conf.today.month conf.today.day hh mm ss conf.user
         action fn occ sn;
       close_out oc;
    return ()
  else ()
;

(* Request for history printing *)

exception Begin_of_file;

value buff_get_rev len =
  let s = String.create len in
  do for i = 0 to len - 1 do s.[i] := Buff.buff.val.[len - 1 - i]; done;
  return s
;

value rev_input_line ic pos =
  if pos <= 0 then raise Begin_of_file
  else
    loop 0 (pos - 1) where rec loop len pos =
      if pos < 0 then (buff_get_rev len, pos)
      else
        do seek_in ic pos; return
        match input_char ic with
        [ '\n' -> (buff_get_rev len, pos)
        | c -> loop (Buff.store len c) (pos - 1) ]
;

value action_text conf =
  fun
  [ "ap" -> transl_decline conf "add" (transl_nth conf "person/persons" 0)
  | "mp" -> transl_decline conf "modify" (transl_nth conf "person/persons" 0)
  | "dp" -> transl_decline conf "delete" (transl_nth conf "person/persons" 0)
  | "fp" -> transl_decline conf "merge" (transl_nth conf "person/persons" 1)
  | "si" -> transl_decline conf "send" (transl conf "image")
  | "di" -> transl_decline conf "delete" (transl conf "image")
  | "af" -> transl_decline conf "add" (transl_nth conf "family/families" 0)
  | "mf" -> transl_decline conf "modify" (transl_nth conf "family/families" 0)
  | "df" -> transl_decline conf "delete" (transl_nth conf "family/families" 0)
  | "sf" -> transl_decline conf "switch" (transl_nth conf "family/families" 1)
  | "ff" -> transl_decline conf "merge" (transl_nth conf "family/families" 1)
  | "cn" -> transl conf "change children's names"
  | "aa" -> transl_decline conf "add" (transl conf "parents")
  | x -> x ]
;

value line_tpl = "0000-00-00 00:00:00 xx .";

value line_fields conf base line =
  if String.length line > String.length line_tpl then
    let time = String.sub line 0 19 in
    let (user, i) =
      match (line.[20], Gutil.lindex line ']') with
      [ ('[', Some i) ->
          let user = String.sub line 21 (i - 21) in
          (user, i + 2)
      | _ -> ("", 20) ]
    in
    let action = action_text conf (String.sub line i 2) in
    let key = String.sub line (i + 3) (String.length line - i - 3) in
    let p =
      match person_ht_find_all base key with
      [ [ip] -> Right (poi base ip)
      | _ -> Left key ]
    in
    Some (time, user, action, p)
  else None
;

value print_history_line conf base line i =
  match line_fields conf base line with
  [ Some (time, user, action, p) ->
      do if i = 0 then Wserver.wprint "<dl>\n" else ();
         Wserver.wprint "<dt><tt>%s</tt>\n" time;
         Wserver.wprint "(%s)\n" action;
         if user <> "" then Wserver.wprint "<em>%s</em>\n" user else ();
         Wserver.wprint "<dd>";
         match p with
         [ Left key -> Wserver.wprint "%s" key
         | Right p ->
             do Wserver.wprint "%s"
                  (referenced_person_title_text conf base p);
                Wserver.wprint "%s" (Date.short_dates_text conf base p);
             return () ];
         Wserver.wprint "\n";
      return i + 1
  | None -> i ]
;

value print_history conf base ic =
  let k =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ -> 3 ]
  in
  let pos =
    match p_getint conf.env "pos" with
    [ Some x -> x
    | _ -> in_channel_length ic - 1 ]
  in
  let (pos, n) =
    loop pos 0 where rec loop pos i =
      if i >= k then (pos, i)
      else
	match
          try Some (rev_input_line ic pos) with [ Begin_of_file -> None ]
        with
	[ Some (line, pos) ->
            let i = print_history_line conf base line i in
            loop pos i
	| _ -> (pos, i) ]
  in
  do if n > 0 then Wserver.wprint "</dl>\n" else ();
     if pos > 0 then
       Wserver.wprint "<a href=\"%sm=HIST;k=%d;pos=%d\">&gt;&gt;</a>\n"
         (commd conf) k pos
     else ();
  return ()
;

value print conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "history of updates"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     let fname = file_name conf in
     match try Some (open_in fname) with [ Sys_error _ -> None ] with
     [ Some ic ->
         do print_history conf base ic;
            close_in ic;
         return ()
     | _ -> () ];
     trailer conf;
  return ()
;
