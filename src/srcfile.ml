(* camlp4r ./pa_lock.cmo ./pa_html.cmo pa_extend.cmo *)
(* $Id: srcfile.ml,v 3.0 1999-10-29 10:31:36 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Util;

value get_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d/%02d/%d" tm.Unix.tm_mday
    (succ tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
;

value adm_file f =
  List.fold_right Filename.concat [Util.base_dir.val; "cnt"] f
;

value cnt conf ext = adm_file (conf.bname ^ ext);

value count conf =
  let fname = cnt conf ".txt" in
  try
    let ic = open_in fname in
    let rd =
      try
        let wc = int_of_string (input_line ic) in
        let rc = int_of_string (input_line ic) in
        let d = input_line ic in
        (wc, rc, d)
      with _ -> (0, 0, get_date ())
    in
    do close_in ic; return rd
  with _ ->
    (0, 0, get_date ())
;

value write_counter conf (welcome_cnt, request_cnt, start_date) =
  let fname = cnt conf ".txt" in
  try
    let oc = open_out_bin fname in
    do output_string oc (string_of_int welcome_cnt);
       output_string oc "\n";
       output_string oc (string_of_int request_cnt);
       output_string oc "\n";
       output_string oc start_date;
       output_string oc "\n";
       close_out oc;
    return ()
  with _ ->
      ()
;

value incr_welcome_counter conf =
  let lname = cnt conf ".lck" in
  lock_wait lname with
  [ Accept ->
      let (welcome_cnt, request_cnt, start_date) = count conf in
      let r = (welcome_cnt + 1, request_cnt, start_date) in
      do write_counter conf r; return Some r
  | Refuse -> None ]
;

value incr_request_counter conf =
  let lname = cnt conf ".lck" in
  lock_wait lname with
  [ Accept -> 
      let (welcome_cnt, request_cnt, start_date) = count conf in
      let r = (welcome_cnt, request_cnt + 1, start_date) in
      do write_counter conf r; return Some r
  | Refuse -> None ]
;

value hidden_env conf =
  List.iter
    (fun (k, v) -> Wserver.wprint "<input type=hidden name=%s value=%s>\n" k v)
    conf.henv
;

value lang_file_name conf fname =
  let fname1 =
    List.fold_right Filename.concat [Util.base_dir.val; "lang"; conf.lang]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    List.fold_right Filename.concat [Util.lang_dir.val; "lang"; conf.lang]
      (Filename.basename fname ^ ".txt")
;

value any_lang_file_name fname =
  let fname1 =
    List.fold_right Filename.concat [Util.base_dir.val; "lang"]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    List.fold_right Filename.concat [Util.lang_dir.val; "lang"]
      (Filename.basename fname ^ ".txt")
;

value digit =
  fun
  [ '0'..'9' as c -> Char.code c - Char.code '0'
  | _ -> failwith "digit" ]
;

module G = Grammar.Make (struct value lexer = Plexer.make (); end);
value date = G.Entry.create "date";
GEXTEND G
  date:
    [ [ d = INT; "/"; m = INT; "/"; y = INT; EOI ->
          (int_of_string d, int_of_string m, int_of_string y) ] ];
END;

value extract_date d =
  try Some (G.Entry.parse date (G.parsable (Stream.of_string d))) with
  [ Stdpp.Exc_located _ (Stream.Error _ | Token.Error _) -> None ]
;

value print_date conf =
  let (wc, rc, d) = count conf in
  match extract_date d with
  [ Some (d, m, y) ->
      let d =
        Dgreg {day = d; month = m; year = y; prec = Sure; delta = 0} Dgregorian
      in
      Wserver.wprint "%s" (Date.string_of_date conf d)
  | _ -> Wserver.wprint "%s" d ]
;

value rec src_translate conf nom ic =
  let (upp, s) =
    loop "" (input_char ic) where rec loop s c =
      if c = ']' then
        if String.length s > 0 && s.[0] == '*' then
          (True, String.sub s 1 (String.length s - 1))
        else (False, s)
      else loop (s ^ String.make 1 c) (input_char ic)
  in
  let (n, c) =
    match input_char ic with
    [ '0'..'9' as c -> (Char.code c - Char.code '0', "")
    | c -> (0, String.make 1 c) ]
  in
  let r =
    if c = "[" then Util.transl_decline conf s (src_translate conf False ic)
    else
      let r = Util.transl_nth conf s n in
      (if nom then Gutil.nominative r else r) ^ c
  in
  if upp then capitale r else r
;

value rec copy_from_channel conf base ic =
  let echo = ref True in
  try
    while True do
      match input_char ic with
      [ '[' ->
          let s = src_translate conf True ic in
          if not echo.val then ()
          else Wserver.wprint "%s" s
      | '%' ->
          let c = input_char ic in
          if not echo.val then
            match c with
            [ 'w' | 'x' | 'y' | 'z' | 'i' | 'j' | 'u' | 'o' -> echo.val := True
            | _ -> () ]
          else
            match c with
            [ '%' -> Wserver.wprint "%%"
            | '[' | ']' -> Wserver.wprint "%c" c
            | 'a' ->
                match Util.find_person_in_env conf base "z" with
                [ Some ip ->
                    Wserver.wprint "%s"
                      (referenced_person_title_text conf base ip)
                | None -> () ]
            | 'b' ->
                let s =
                  try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
                  [ Not_found -> "" ]
                in
                let s = s ^ " " ^ body_prop conf in
                do Wserver.wprint "%s" s;
                   List.iter (fun t -> Wserver.wprint "><%s" t)
                     (Util.enclosing_tags conf);
                return ()
            | 'c' ->
                let (wc, rc, d) = count conf in
                Num.print (fun x -> Wserver.wprint "%s" x)
                  (transl conf "(thousand separator)")
                  (Num.of_int wc)
            | 'd' -> print_date conf
            | 'f' -> Wserver.wprint "%s" conf.command
            | 'g' ->
                do Wserver.wprint "%s?" conf.command;
                   if conf.cgi then Wserver.wprint "b=%s;" conf.bname else ();
                return ()
            | 'h' -> hidden_env conf
            | 'i' ->
                if conf.cgi || conf.auth_file <> "" then ()
                else echo.val := False
            | 'j' ->
                if Sys.file_exists (History.file_name conf) then ()
                else echo.val := False
            | 'l' -> Wserver.wprint "%s" conf.lang
            | 'n' ->
                Num.print (fun x -> Wserver.wprint "%s" x)
                  (transl conf "(thousand separator)")
                  (Num.of_int base.data.persons.len)
            | 'o' ->
                if (* conf.lang <> conf.default_lang
                || *) base.data.bnotes.nread 1 = "" then echo.val := False
                else ()
            | 'q' ->
                let (wc, rc, d) = count conf in
                Num.print (fun x -> Wserver.wprint "%s" x)
                  (transl conf "(thousand separator)")
                  (Num.of_int (wc + rc))
            | 'r' -> copy_from_file conf base (input_line ic)
            | 's' -> Wserver.wprint "%s" (commd conf)
            | 't' -> Wserver.wprint "%s" conf.bname
            | 'u' ->
                match Util.find_person_in_env conf base "z" with
                [ Some _ -> ()
                | None -> echo.val := False ]
            | 'v' -> Wserver.wprint "%s" Version.txt
            | 'w' -> if conf.wizard then () else echo.val := False
            | 'x' ->
                if conf.wizard || conf.friend then () else echo.val := False
            | 'y' ->
                if not conf.wizard && not conf.just_friend_wizard
                && not conf.cgi && conf.auth_file = "" then ()
                else echo.val := False
            | 'z' ->
                if not conf.wizard && not conf.friend && not conf.cgi
                && conf.auth_file = "" then ()
                else echo.val := False
            | c -> Wserver.wprint "%%%c" c ]
      | c -> if echo.val then Wserver.wprint "%c" c else () ];
    done
  with
  [ End_of_file -> close_in ic ]
and copy_from_file conf base name =
  let fname = any_lang_file_name name in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic -> copy_from_channel conf base ic
  | None ->
      do Wserver.wprint "<em>... file not found: \"%s.txt\"</em>" name;
         html_br conf;
      return () ]
;

value gen_print with_logo conf base fname =
  match
    try Some (open_in (lang_file_name conf fname)) with
    [ Sys_error _ ->
        try Some (open_in (any_lang_file_name fname)) with
        [ Sys_error _ -> None ] ]
  with
  [ Some ic ->
      do Util.html conf;
         copy_from_channel conf base ic;
         Util.gen_trailer with_logo conf;
      return ()
  | _ ->
      let title _ = Wserver.wprint "Error" in
      do Util.header conf title;
         tag "ul" begin
           html_li conf;
           Wserver.wprint "Cannot access file \"%s.txt\".\n" fname;
         end;
         Util.gen_trailer with_logo conf;
      return raise Exit ]
;

value print = gen_print True;

value print_start conf base =
  let fname =
    if Sys.file_exists (lang_file_name conf conf.bname) then conf.bname
    else if Sys.file_exists (any_lang_file_name conf.bname) then conf.bname
    else "start"
  in
  gen_print False conf base fname
;

value print_lexicon conf base =
  let title _ = Wserver.wprint "Lexicon" in
  let fname =
    List.fold_right Filename.concat [Util.lang_dir.val; "lang"] "lexicon.txt"
  in
  do Util.header conf title;
     match try Some (open_in fname) with [ Sys_error _ -> None ] with
     [ Some ic ->
         do Wserver.wprint "<pre>\n";
            try while True do Wserver.wprint "%s\n" (input_line ic); done with
            [ End_of_file -> () ];
            Wserver.wprint "</pre>\n";
            close_in ic;
         return ()
     | None ->
         do Wserver.wprint "<em>... file not found: \"%s.txt\"</em>"
              "lexicon";
            html_br conf;
         return () ];
     Util.trailer conf;
  return ()
;
