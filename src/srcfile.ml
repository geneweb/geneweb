(* camlp4r ./pa_lock.cmo ./pa_html.cmo pa_extend.cmo *)
(* $Id: srcfile.ml,v 3.34 2001-01-19 16:54:22 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Util;

type counter =
  {welcome_cnt : mutable int;
   request_cnt : mutable int;
   start_date : string;
   wizard_cnt : mutable int;
   friend_cnt : mutable int;
   normal_cnt : mutable int}
;

value get_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d/%02d/%d" tm.Unix.tm_mday
    (succ tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
;

value adm_file f =
  List.fold_right Filename.concat [Util.cnt_dir.val; "cnt"] f
;

value cnt conf ext = adm_file (conf.bname ^ ext);

value input_int ic =
  try int_of_string (input_line ic) with
  [ End_of_file | Failure _ -> 0 ]
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
        let wzc = input_int ic in
        let frc = input_int ic in
        let nrc = input_int ic in
        {welcome_cnt = wc; request_cnt = rc; start_date = d;
         wizard_cnt = wzc; friend_cnt = frc; normal_cnt = nrc}
      with _ ->
        {welcome_cnt = 0; request_cnt = 0; start_date = get_date ();
         wizard_cnt = 0; friend_cnt = 0; normal_cnt = 0}
    in
    do close_in ic; return rd
  with _ ->
    {welcome_cnt = 0; request_cnt = 0; start_date = get_date ();
     wizard_cnt = 0; friend_cnt = 0; normal_cnt = 0}
;

value write_counter conf r =
  let fname = cnt conf ".txt" in
  try
    let oc = open_out_bin fname in
    do output_string oc (string_of_int r.welcome_cnt);
       output_string oc "\n";
       output_string oc (string_of_int r.request_cnt);
       output_string oc "\n";
       output_string oc r.start_date;
       output_string oc "\n";
       output_string oc (string_of_int r.wizard_cnt);
       output_string oc "\n";
       output_string oc (string_of_int r.friend_cnt);
       output_string oc "\n";
       output_string oc (string_of_int r.normal_cnt);
       output_string oc "\n";
       close_out oc;
    return ()
  with _ ->
      ()
;

value std_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;

value read_wizard_trace fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let r = ref [] in
      do try while True do r.val := [input_line ic :: r.val]; done with
         [ End_of_file -> close_in ic ];
      return List.rev r.val
  | None -> [] ]
;

value write_wizard_trace fname wt =
  let oc = open_out fname in
  do List.iter (fun (dt, u) -> Printf.fprintf oc "%s %s\n" dt u) wt;
     close_out oc;
  return ()
;

value set_wizard_trace conf =
  if conf.wizard && conf.user <> "" then
    let wpf =
      try List.assoc "wizard_passwd_file" conf.base_env with
      [ Not_found -> "" ]
    in
    if wpf <> "" then
      let fname = adm_file (conf.bname ^ "_w.txt") in
      let dt = std_date () in
      let wt =
        let r = read_wizard_trace fname in
        let dtlen = String.length dt in
        loop False [] r where rec loop found r =
          fun
          [ [x :: l] ->
              if String.length x > dtlen + 2 then
                let u =
                  String.sub x (dtlen + 1) (String.length x - dtlen - 1)
                in
                if u = conf.user then loop True [(dt, u) :: r] l
                else loop found [(String.sub x 0 dtlen, u) :: r] l
              else loop found r l
          | [] -> if found then r else [(dt, conf.user) :: r] ]
      in
      write_wizard_trace fname (Sort.list \> wt)
    else ()
  else ()
;

value incr_welcome_counter conf =
  let lname = cnt conf ".lck" in
  lock_wait lname with
  [ Accept ->
      let r = count conf in
      do r.welcome_cnt := r.welcome_cnt + 1;
         if conf.wizard then r.wizard_cnt := r.wizard_cnt + 1
         else if conf.friend then r.friend_cnt := r.friend_cnt + 1
         else r.normal_cnt := r.normal_cnt + 1;
         write_counter conf r;
         set_wizard_trace conf;
      return Some (r.welcome_cnt, r.request_cnt, r.start_date)
  | Refuse -> None ]
;

value incr_request_counter conf =
  let lname = cnt conf ".lck" in
  lock_wait lname with
  [ Accept -> 
      let r = count conf in
      do r.request_cnt := r.request_cnt + 1;
         if conf.wizard then r.wizard_cnt := r.wizard_cnt + 1
         else if conf.friend then r.friend_cnt := r.friend_cnt + 1
         else r.normal_cnt := r.normal_cnt + 1;
         write_counter conf r;
         set_wizard_trace conf;
      return Some (r.welcome_cnt, r.request_cnt, r.start_date)
  | Refuse -> None ]
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

value source_file_name conf fname =
  let bname = conf.bname in
  let lang = conf.lang in
  let fname1 =
    List.fold_right Filename.concat [Util.base_dir.val; "src"; bname; lang]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    List.fold_right Filename.concat [Util.base_dir.val; "src"; bname]
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

value string_of_start_date conf =
  let r = count conf in
  match extract_date r.start_date with
  [ Some (d, m, y) ->
      let d =
        Dgreg {day = d; month = m; year = y; prec = Sure; delta = 0} Dgregorian
      in
      Date.string_of_date conf d
  | _ -> r.start_date ]
;

value string_of_num sep num =
  let len = ref 0 in
  do Num.print (fun x -> len.val := Buff.mstore len.val x) sep num; return
  Buff.get len.val
;

value macro conf base =
  fun
  [ 'a' ->
      match Util.find_person_in_env conf base "z" with
      [ Some ip -> referenced_person_title_text conf base ip
      | None -> "" ]
  | 'b' ->
      let s =
        try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
        [ Not_found -> "" ]
      in
      s ^ body_prop conf
  | 'c' ->
      let r = count conf in
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int r.welcome_cnt)
  | 'd' -> string_of_start_date conf
  | 'D' -> (count conf).start_date
  | 'e' -> conf.charset
  | 'f' -> conf.command
  | 'g' ->
      conf.command ^ "?" ^ (if conf.cgi then "b=" ^ conf.bname ^ ";" else "")
  | 'i' -> conf.highlight
  | 'k' -> conf.indep_command
  | 'l' -> conf.lang
  | 'm' ->
      try
        let s = List.assoc "latest_event" conf.base_env in
        if s = "" then "20" else s
      with
      [ Not_found -> "20" ]
  | 'n' ->
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int base.data.persons.len)
  | 'o' -> image_prefix conf
  | 'q' ->
      let r = count conf in
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int (r.welcome_cnt + r.request_cnt))
  | 's' -> commd conf
  | 't' -> conf.bname
  | 'v' -> Version.txt
  | 'w' ->
      let s = Util.link_to_referer conf in
      if s = "" then "&nbsp;" else s
  | c -> "%" ^ String.make 1 c ]
;

value rec src_translate conf base nomin ic =
  let (upp, s) =
    loop 0 (input_char ic) where rec loop len c =
      if c = ']' then
        let s = Buff.get len in
        if len > 0 && s.[0] == '*' then
          (True, String.sub s 1 (len - 1))
        else (False, s)
      else loop (Buff.store len c) (input_char ic)
  in
  let (n, c) =
    match input_char ic with
    [ '0'..'9' as c -> (Char.code c - Char.code '0', "")
    | c -> (0, String.make 1 c) ]
  in
  let r =
    if c = "[" then
      Util.transl_decline conf s (src_translate conf base False ic)
    else
      let r = Util.transl_nth conf s n in
      match Gutil.lindex r '%' with
      [ Some i when (*r.[i+1] == 'd' &&*) c = "(" ->
          let sa =
            loop 0 where rec loop len =
              let c = input_char ic in
              if c == ')' then Buff.get len
              else
                let len =
                  if c == '%' then
                    let c = input_char ic in
                    Buff.mstore len (macro conf base c)
                  else Buff.store len c
                in
                loop len
          in
          String.sub r 0 i ^ sa ^
          String.sub r (i + 2) (String.length r - i - 2)
      | _ ->
          (if nomin then Gutil.nominative r else r) ^ c ]
  in
  if upp then capitale r else r
;

value language_name conf lang =
  let str = transl conf " !languages" in
  loop 0 0 where rec loop beg i =
    if i == String.length str && i == beg then lang
    else if i == String.length str || str.[i] == '/' then
      if i > beg + 3 && String.sub str beg 2 = lang then
        String.sub str (beg + 3) (i - beg - 3)
      else if i == String.length str then lang
      else loop (i + 1) (i + 1)
    else loop beg (i + 1)
;

value browser_cannot_handle_passwords conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase user_agent = "konqueror"
;

value get_variable ic =
  loop 0 where rec loop len =
    match input_char ic with
    [ ';' -> Buff.get len
    | c -> loop (Buff.store len c) ]
;

type src_mode = [ Lang | Source ];

value rec copy_from_channel conf base ic mode =
  let echo = ref True in
  let no_tables = browser_doesnt_have_tables conf in
  let (push_echo, pop_echo) =
    let stack = ref [] in
    (fun x ->
       do stack.val := [echo.val :: stack.val]; echo.val := x; return (),
     fun () ->
       match stack.val with
       [ [x :: l] -> do stack.val := l; echo.val := x; return ()
       | [] -> echo.val := True ])
  in
  let rec if_expr =
    fun
    [ 'N' -> not (if_expr (input_char ic))
    | 'a' -> conf.auth_file <> ""
    | 'c' -> conf.cgi || browser_cannot_handle_passwords conf
    | 'f' -> conf.friend
    | 'h' -> Sys.file_exists (History.file_name conf)
    | 'j' -> conf.just_friend_wizard
    | 'l' -> no_tables
    | 'n' -> base.data.bnotes.nread 1 <> ""
    | 's' -> p_getenv conf.base_env (get_variable ic) <> Some "no"
    | 'w' -> conf.wizard
    | 'z' -> Util.find_person_in_env conf base "z" <> None
    | '|' ->
        let a = if_expr (input_char ic) in
        let b = if_expr (input_char ic) in
        a || b
    | '&' ->
        let a = if_expr (input_char ic) in
        let b = if_expr (input_char ic) in
        a && b
    | c -> do Wserver.wprint "!!!!!%c!!!!!" c; return True ]
  in
  try
    while True do
      match input_char ic with
      [ '[' ->
          let s = src_translate conf base True ic in
          if not echo.val then () else Wserver.wprint "%s" s
      | '<' when no_tables && echo.val ->
          let c = input_char ic in
          let (slash, c) =
            if c = '/' then ("/", input_char ic) else ("", c)
          in
          let (atag, c) =
            loop 0 c where rec loop len =
              fun
              [ '>' | ' ' | '\n' as c -> (Buff.get len, c)
              | c -> loop (Buff.store len c) (input_char ic) ]
          in
          match atag with
          [ "table" | "tr" | "td" ->
              loop c where rec loop =
                fun
                [ '>' -> ()
                | _ -> loop (input_char ic) ]
          | _ -> Wserver.wprint "<%s%s%c" slash atag c ]
      | '%' ->
          let c = input_char ic in
          match c with
          [ 'I' -> push_echo (echo.val && if_expr (input_char ic))
          | 'E' -> pop_echo ()
          | _ when not echo.val -> ()
          | '%' -> Wserver.wprint "%%"
          | '[' | ']' -> Wserver.wprint "%c" c
          | 'h' -> hidden_env conf
          | 'j' -> include_hed_trl conf (Some base) ".hed"
          | 'r' -> copy_from_file conf base (input_line ic) mode
          | 'u' ->
              let lang =
                let c = String.create 2 in
                do c.[0] := input_char ic;
                   c.[1] := input_char ic;
                return c
              in
              Wserver.wprint "%s" (language_name conf lang)
          | c -> Wserver.wprint "%s" (macro conf base c) ]
      | c -> if echo.val then Wserver.wprint "%c" c else () ];
    done
  with
  [ End_of_file -> close_in ic ]
and copy_from_file conf base name mode =
  let fname = 
    match mode with
    [ Lang -> any_lang_file_name name
    | Source -> source_file_name conf name ]
  in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic -> copy_from_channel conf base ic mode
  | None ->
      do Wserver.wprint "<em>... file not found: \"%s.txt\"</em>" name;
         html_br conf;
      return () ]
;

value gen_print with_logo mode conf base fname =
  let channel =
    match mode with
    [ Lang ->
        try Some (open_in (lang_file_name conf fname)) with
        [ Sys_error _ ->
            try Some (open_in (any_lang_file_name fname)) with
            [ Sys_error _ -> None ] ]
    | Source ->
        try Some (open_in (source_file_name conf fname)) with
        [ Sys_error _ -> None ] ]
  in
  match channel with
  [ Some ic ->
      do Util.html conf;
         copy_from_channel conf base ic mode;
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

value print = gen_print True Lang;

value print_source = gen_print True Source;

value print_start conf base =
  let fname =
    if Sys.file_exists (lang_file_name conf conf.bname) then conf.bname
    else if Sys.file_exists (any_lang_file_name conf.bname) then conf.bname
    else "start"
  in
  gen_print False Lang conf base fname
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
