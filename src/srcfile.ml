(* camlp4r ./pa_lock.cmo pa_extend.cmo *)
(* $Id: srcfile.ml,v 1.2 1998-09-02 17:54:19 ddr Exp $ *)

open Config;
open Def;
open Util;

value get_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d/%02d/%d" tm.Unix.tm_mday
    (succ tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
;

value cnt conf ext =
  List.fold_right Filename.concat ["cnt"] (conf.bname ^ ext)
;

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
      write_counter conf (welcome_cnt + 1, request_cnt, start_date)
  | Refuse -> () ]
;

value incr_request_counter conf =
  let lname = cnt conf ".lck" in
  lock_wait lname with
  [ Accept -> 
      let (welcome_cnt, request_cnt, start_date) = count conf in
      write_counter conf (welcome_cnt, request_cnt + 1, start_date)
  | Refuse -> () ]
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
  if Sys.file_exists fname then fname1
  else
    List.fold_right Filename.concat [Util.lang_dir.val; "lang"; conf.lang]
      (Filename.basename fname ^ ".txt")
;

value any_lang_file_name fname =
  let fname1 =
    List.fold_right Filename.concat [Util.base_dir.val; "lang"]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname then fname1
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
      Wserver.wprint "%s" (Date.string_of_date conf (Djma d m y))
  | _ -> Wserver.wprint "%s" d ]
;

value src_translate conf ic =
  let (upp, s) =
    loop "" (input_char ic) where rec loop s c =
      if c = ']' then
        if String.length s > 0 && s.[0] == '*' then
          (True, String.sub s 1 (String.length s - 1))
        else (False, s)
      else loop (s ^ String.make 1 c) (input_char ic)
  in
  let r =
    match input_char ic with
    [ '0'..'9' as c -> Util.transl_nth conf s (Char.code c - Char.code '0')
    | c -> Util.transl_nth conf s 0 ^ String.make 1 c ]
  in
  if upp then capitale r else r
;

value rec copy_from_channel conf base ic =
  let echo = ref True in
  try
    while True do
      match input_char ic with
      [ '[' ->
          let s = src_translate conf ic in
          if not echo.val then ()
          else Wserver.wprint "%s" s
      | '%' ->
          let c = input_char ic in
          if not echo.val then
            if c == 'w' || c == 'x' then echo.val := True else ()
          else
            match c with
            [ '%' -> Wserver.wprint "%%"
            | '[' | ']' -> Wserver.wprint "%c" c
            | 'b' ->
                try
                  Wserver.wprint " %s" (List.assoc "body_prop" conf.base_env)
                with [ Not_found -> () ]
            | 'c' ->
                let (wc, rc, d) = count conf in
                Wserver.wprint "%d" wc
            | 'd' -> print_date conf
            | 'f' -> Wserver.wprint "%s" conf.command
            | 'g' ->
                do Wserver.wprint "%s?" conf.command;
                   if conf.cgi then Wserver.wprint "b=%s;" conf.bname else ();
                return ()
            | 'h' -> hidden_env conf
            | 'l' -> Wserver.wprint "%s" conf.lang
            | 'n' -> Wserver.wprint "%d" (base.persons.len)
            | 'q' ->
                let (wc, rc, d) = count conf in
                Wserver.wprint "%d" (wc + rc)
            | 'r' -> copy_from_file conf base (input_line ic)
            | 's' ->
                do Wserver.wprint "%s?" conf.command;
                   List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v)
                     conf.henv;
                return ()
            | 't' -> Wserver.wprint "%s" conf.bname
            | 'v' -> Wserver.wprint "%s" Util.version
            | 'w' -> if not conf.wizard then echo.val := False else ()
            | 'x' ->
                if not (conf.wizard || conf.friend) then echo.val := False
                else ()
            | c -> Wserver.wprint "%%%c" c ]
      | c -> if echo.val then Wserver.wprint "%c" c else () ];
    done
  with
  [ End_of_file -> close_in ic ]
and copy_from_file conf base fname =
  let fname = any_lang_file_name fname in
  let ic = open_in fname in
  copy_from_channel conf base ic
;

value print conf base fname =
  match
    try Some (open_in (lang_file_name conf fname)) with
    [ Sys_error _ ->
        try Some (open_in (any_lang_file_name fname)) with
        [ Sys_error _ -> None ] ]
  with
  [ Some ic ->
      do Util.html conf;
         copy_from_channel conf base ic;
         Util.trailer conf;
      return ()
  | _ ->
      let title _ = Wserver.wprint "Error" in
      do Util.header conf title;
         Wserver.wprint "<ul><li>\n";
         Wserver.wprint "Cannot access file \"%s.txt\".\n" fname;
         Wserver.wprint "</ul>\n";
         Util.trailer conf;
      return raise Exit ]
;

value print_start conf base =
  let fname =
    if Sys.file_exists (lang_file_name conf conf.bname) then conf.bname
    else "start"
  in
  print conf base fname
;

value print_lexicon conf base =
  let title _ = Wserver.wprint "Lexicon" in
  let fname = any_lang_file_name "lexicon" in
  let ic = open_in fname in
  do Util.header conf title;
     Wserver.wprint "<pre>\n";
     try while True do Wserver.wprint "%s\n" (input_line ic); done with
     [ End_of_file -> () ];
     Wserver.wprint "</pre>\n";
     close_in ic;
     Util.trailer conf;
  return ()
;
