(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open TemplAst
open Util

type counter = {
  mutable welcome_cnt : int;
  mutable request_cnt : int;
  start_date : string;
  mutable wizard_cnt : int;
  mutable friend_cnt : int;
  mutable normal_cnt : int;
}

let get_date conf =
  Printf.sprintf "%02d/%02d/%d" conf.today.day conf.today.month conf.today.year

let adm_file f = List.fold_right Filename.concat [ !Util.cnt_dir; "cnt" ] f
let cnt conf ext = adm_file (conf.bname ^ ext)

let input_int ic =
  try int_of_string (input_line ic) with End_of_file | Failure _ -> 0

let count conf =
  let fname = cnt conf ".txt" in
  try
    let ic = Secure.open_in fname in
    let rd =
      try
        let wc = int_of_string (input_line ic) in
        let rc = int_of_string (input_line ic) in
        let d = input_line ic in
        let wzc = input_int ic in
        let frc = input_int ic in
        let nrc = input_int ic in
        {
          welcome_cnt = wc;
          request_cnt = rc;
          start_date = d;
          wizard_cnt = wzc;
          friend_cnt = frc;
          normal_cnt = nrc;
        }
      with _ ->
        {
          welcome_cnt = 0;
          request_cnt = 0;
          start_date = get_date conf;
          wizard_cnt = 0;
          friend_cnt = 0;
          normal_cnt = 0;
        }
    in
    close_in ic;
    rd
  with _ ->
    {
      welcome_cnt = 0;
      request_cnt = 0;
      start_date = get_date conf;
      wizard_cnt = 0;
      friend_cnt = 0;
      normal_cnt = 0;
    }

let write_counter conf r =
  let fname = cnt conf ".txt" in
  try
    let oc = Secure.open_out_bin fname in
    output_string oc (string_of_int r.welcome_cnt);
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
    close_out oc
  with _ -> ()

let set_wizard_and_friend_traces conf =
  if conf.wizard && conf.user <> "" then (
    let wpf =
      try List.assoc "wizard_passwd_file" conf.base_env with Not_found -> ""
    in
    if wpf <> "" then
      let fname = adm_file (conf.bname ^ "_w.txt") in
      update_wf_trace conf fname)
  else if conf.friend && (not conf.just_friend_wizard) && conf.user <> "" then
    let fpf =
      try List.assoc "friend_passwd_file" conf.base_env with Not_found -> ""
    in
    let fp =
      try List.assoc "friend_passwd" conf.base_env with Not_found -> ""
    in
    if
      fpf <> ""
      && is_that_user_and_password conf.auth_scheme conf.user fp = false
    then
      let fname = adm_file (conf.bname ^ "_f.txt") in
      update_wf_trace conf fname

let incr_counter f conf =
  let lname = cnt conf ".lck" in
  Lock.control lname true
    ~onerror:(fun () -> None)
    (fun () ->
      let r = count conf in
      f r;
      if conf.wizard then r.wizard_cnt <- r.wizard_cnt + 1
      else if conf.friend then r.friend_cnt <- r.friend_cnt + 1
      else r.normal_cnt <- r.normal_cnt + 1;
      write_counter conf r;
      set_wizard_and_friend_traces conf;
      Some (r.welcome_cnt, r.request_cnt, r.start_date))

let incr_welcome_counter =
  incr_counter (fun r -> r.welcome_cnt <- r.welcome_cnt + 1)

let incr_request_counter =
  incr_counter (fun r -> r.request_cnt <- r.request_cnt + 1)

let lang_file_name conf fname =
  let fname1 =
    Util.base_path [ "lang"; conf.lang ] (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    search_in_assets
      (Filename.concat conf.lang (Filename.basename fname ^ ".txt"))

let any_lang_file_name fname =
  let fname1 = Util.base_path [ "lang" ] (Filename.basename fname ^ ".txt") in
  if Sys.file_exists fname1 then fname1
  else
    search_in_assets (Filename.concat "lang" (Filename.basename fname ^ ".txt"))

let source_file_name conf fname =
  let bname = conf.bname in
  let lang = conf.lang in
  let fname1 =
    List.fold_right Filename.concat
      [ Util.base_path [ "src" ] bname; lang ]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    Filename.concat
      (Util.base_path [ "src" ] bname)
      (Filename.basename fname ^ ".txt")

let extract_date s =
  try Scanf.sscanf s "%d/%d/%d" (fun d m y -> Some (d, m, y)) with _ -> None

let string_of_start_date conf =
  let r = count conf in
  match extract_date r.start_date with
  | None -> Util.safe_html r.start_date
  | Some (d, m, y) ->
      Date.Dgreg
        ({ day = d; month = m; year = y; prec = Sure; delta = 0 }, Dgregorian)
      |> DateDisplay.string_of_date conf

let string_of_int_sep_aux conf n =
  Mutil.string_of_int_sep (Util.transl conf "(thousand separator)") n
  |> Adef.safe

let macro conf base = function
  | 'a' -> (
      match Util.find_sosa_ref conf base with
      | Some p -> NameDisplay.referenced_person_title_text conf base p
      | None -> Adef.safe "")
  | 'b' ->
      let s =
        try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\""
        with Not_found -> ""
      in
      Adef.safe (s ^ body_prop conf)
  | 'c' -> string_of_int_sep_aux conf (count conf).welcome_cnt
  | 'd' -> string_of_start_date conf
  | 'D' -> Adef.safe (count conf).start_date
  | 'e' -> Adef.safe conf.charset
  | 'f' -> Adef.safe conf.command
  | 'g' -> (Util.prefix_base conf :> Adef.safe_string)
  | 'G' -> (Util.prefix_base_password conf :> Adef.safe_string)
  | 'i' -> Adef.safe conf.highlight
  | 'k' -> Adef.safe conf.indep_command
  | 'l' -> Adef.safe conf.lang
  | 'L' -> Adef.safe conf.left
  | 'm' -> (
      try
        let s = List.assoc "latest_event" conf.base_env in
        if s = "" then Adef.safe "20"
        else (Util.escape_html s :> Adef.safe_string)
      with Not_found -> Adef.safe "20")
  | 'n' -> string_of_int_sep_aux conf (nb_of_persons base)
  | 'N' -> (
      try
        (List.assoc "base_notes_title" conf.base_env |> Util.escape_html
          :> Adef.safe_string)
      with Not_found -> Adef.safe "")
  | 'o' -> (Image.prefix conf :> Adef.safe_string)
  | 'q' ->
      let r = count conf in
      string_of_int_sep_aux conf (r.welcome_cnt + r.request_cnt)
  | 'R' -> Adef.safe conf.right
  | 's' -> (commd conf :> Adef.safe_string)
  | 't' -> Adef.safe conf.bname
  | 'T' -> Util.doctype
  | 'U' ->
      if (conf.wizard || conf.just_friend_wizard) && conf.user <> "" then
        Adef.safe (": " ^ conf.user)
      else Adef.safe ""
  | 'v' -> Adef.safe Version.txt
  | 'w' ->
      let s = Hutil.link_to_referer conf in
      if (s :> string) = "" then Adef.safe "&nbsp;" else s
  | 'W' -> (Util.get_referer conf :> Adef.safe_string)
  | '/' -> Adef.safe ""
  | c -> Adef.safe ("%" ^ String.make 1 c)

module Lbuff = Buff.Make ()

let rec lexicon_translate conf base nomin strm first_c =
  let upp, s =
    let rec loop len c =
      if c = ']' then
        let s = Lbuff.get len in
        if len > 0 && s.[0] = '*' then (true, String.sub s 1 (len - 1))
        else (false, s)
      else loop (Lbuff.store len c) (Stream.next strm)
    in
    loop 0 first_c
  in
  let n, c =
    match Stream.next strm with
    | '0' .. '9' as c -> (Char.code c - Char.code '0', "")
    | c -> (0, String.make 1 c)
  in
  let r =
    if c = "[" then
      Util.transl_decline conf s
        (lexicon_translate conf base false strm (Stream.next strm))
    else
      let r = Util.transl_nth conf s n in
      match String.index_opt r '%' with
      | Some i when c = "(" ->
          let sa =
            let rec loop len =
              let c = Stream.next strm in
              if c = ')' then Lbuff.get len
              else
                let len =
                  if c = '%' then
                    let c = Stream.next strm in
                    Lbuff.mstore len
                      (macro conf base c : Adef.safe_string :> string)
                  else Lbuff.store len c
                in
                loop len
            in
            loop 0
          in
          String.sub r 0 i ^ sa ^ String.sub r (i + 2) (String.length r - i - 2)
      | _ -> (if nomin then Util.translate_eval r else r) ^ c
  in
  if upp then Utf8.capitalize_fst r else r

let browser_cannot_handle_passwords conf =
  let user_agent = Mutil.extract_param "user-agent: " '/' conf.request in
  String.lowercase_ascii user_agent = "konqueror"

let get_variable strm =
  let rec loop len =
    match Stream.next strm with
    | ';' -> Buff.get len
    | c -> loop (Buff.store len c)
  in
  loop 0

let rec stream_line (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some '\n' ->
      Stream.junk strm__;
      ""
  | Some c ->
      Stream.junk strm__;
      String.make 1 c ^ stream_line strm__
  | _ -> raise Stream.Failure

type src_mode = Lang | Source

let rec copy_from_stream conf base strm mode =
  let echo = ref true in
  let no_tables = browser_doesnt_have_tables conf in
  let push_echo, pop_echo =
    let stack = ref [] in
    ( (fun x ->
        stack := !echo :: !stack;
        echo := x),
      fun () ->
        match !stack with
        | x :: l ->
            stack := l;
            echo := x
        | [] -> echo := true )
  in
  let rec if_expr = function
    | 'N' -> not (if_expr (Stream.next strm))
    | 'a' -> conf.auth_file <> ""
    | 'c' -> conf.cgi || browser_cannot_handle_passwords conf
    | 'f' -> conf.friend
    | 'h' -> Sys.file_exists (History.file_name conf)
    | 'j' -> conf.just_friend_wizard
    | 'l' -> no_tables
    | 'm' -> Gwdb.read_nldb base <> []
    | 'n' -> not (base_notes_are_empty base "")
    | 'o' -> Sys.file_exists (WiznotesDisplay.dir conf base)
    | 'p' -> (
        match List.assoc_opt (get_variable strm) conf.base_env with
        | Some "" | None -> false
        | Some _ -> true)
    | 's' -> List.assoc_opt (get_variable strm) conf.base_env <> Some "no"
    | 'w' -> conf.wizard
    | 'z' -> Util.find_sosa_ref conf base <> None
    | '|' ->
        let a = if_expr (Stream.next strm) in
        let b = if_expr (Stream.next strm) in
        a || b
    | '&' ->
        let a = if_expr (Stream.next strm) in
        let b = if_expr (Stream.next strm) in
        a && b
    | c ->
        Output.printf conf "!!!!!%c!!!!!" c;
        true
  in
  try
    while true do
      match Stream.next strm with
      | '[' -> src_translate conf base true strm echo mode
      | '<' when no_tables && !echo -> (
          let c = Stream.next strm in
          let slash, c = if c = '/' then ("/", Stream.next strm) else ("", c) in
          let atag, c =
            let rec loop len = function
              | ('>' | ' ' | '\n') as c -> (Buff.get len, c)
              | c -> loop (Buff.store len c) (Stream.next strm)
            in
            loop 0 c
          in
          match atag with
          | "table" | "tr" | "td" ->
              let rec loop = function
                | '>' -> ()
                | _ -> loop (Stream.next strm)
              in
              loop c
          | _ -> Output.printf conf "<%s%s%c" slash atag c)
      | '%' -> (
          let c = Stream.next strm in
          match c with
          | 'I' -> push_echo (!echo && if_expr (Stream.next strm))
          | 'E' -> pop_echo ()
          | _ when not !echo -> ()
          | '%' -> Output.print_sstring conf "%"
          | '[' | ']' -> Output.printf conf "%c" c
          | 'h' -> hidden_env conf
          | 'j' -> Templ.include_hed_trl conf "hed"
          | 'P' ->
              let _ = Stream.next strm in
              ()
          | 'r' -> copy_from_file conf base (stream_line strm) mode
          | 'u' ->
              let lang =
                let rec loop len =
                  let c = Stream.next strm in
                  if c = ';' then Buff.get len else loop (Buff.store len c)
                in
                loop 0
              in
              let lang_def = transl conf "!languages" in
              Output.print_sstring conf (Translate.language_name lang lang_def)
          | 'V' ->
              let txt =
                try List.assoc (get_variable strm) conf.base_env
                with Not_found -> ""
              in
              copy_from_string conf base txt mode
          | c -> Output.print_string conf (macro conf base c))
      | c -> if !echo then Output.printf conf "%c" c
    done
  with Stream.Failure -> ()

and src_translate conf base nomin strm echo mode =
  let c = Stream.next strm in
  if c = '\n' then
    let s =
      let rec loop lev len = function
        | '[' -> loop (lev + 1) (Lbuff.store len '[') (Stream.next strm)
        | ']' ->
            if lev = 0 then Lbuff.get len
            else loop (lev - 1) (Lbuff.store len ']') (Stream.next strm)
        | c -> loop lev (Lbuff.store len c) (Stream.next strm)
      in
      loop 0 0 (Stream.next strm)
    in
    let s, _ =
      Translate.inline conf.lang '%'
        (macro conf base : char -> Adef.safe_string :> char -> string)
        s
    in
    if not !echo then ()
    else copy_from_stream conf base (Stream.of_string s) mode
  else
    let s = lexicon_translate conf base nomin strm c in
    if !echo then Output.print_sstring conf s

and copy_from_file conf base name mode =
  let fname =
    match mode with
    | Lang -> any_lang_file_name name
    | Source -> source_file_name conf name
  in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic -> copy_from_channel conf base ic mode
  | None ->
      Output.printf conf "<em>... file not found: \"%s.txt\"</em><br>" name

and copy_from_channel conf base ic mode =
  copy_from_stream conf base (Stream.of_channel ic) mode;
  close_in ic

and copy_from_string conf base str mode =
  copy_from_stream conf base (Stream.of_string str) mode

let gen_print mode conf base fname =
  let channel =
    match mode with
    | Lang -> (
        try Some (Secure.open_in (lang_file_name conf fname))
        with Sys_error _ -> (
          try Some (Secure.open_in (any_lang_file_name fname))
          with Sys_error _ -> None))
    | Source -> (
        try Some (Secure.open_in (source_file_name conf fname))
        with Sys_error _ -> None)
  in
  match channel with
  | Some ic ->
      let title _ = Output.print_string conf (Util.escape_html fname) in
      Hutil.header_without_page_title conf title;
      copy_from_channel conf base ic mode;
      Hutil.trailer conf
  | _ ->
      let title _ = Output.print_sstring conf "Error" in
      Hutil.header conf title;
      Output.print_sstring conf "<ul><li>Cannot access file \"";
      Output.print_string conf (Util.escape_html fname);
      Output.print_sstring conf ".txt\"</ul>";
      Hutil.trailer conf

let print_source = gen_print Source

(* welcome page *)

type 'a env = Vsosa_ref of person option Lazy.t | Vother of 'a | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let eval_var conf base env () _loc = function
  | [ "base"; "has_notes" ] -> VVbool (not (base_notes_are_empty base ""))
  | [ "base"; "name" ] -> VVstring conf.bname
  | [ "base"; "nb_persons" ] ->
      VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (nb_of_persons base))
  | [ "base"; "real_nb_persons" ] ->
      VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (Gwdb.nb_of_real_persons base))
  | [ "browsing_with_sosa_ref" ] -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> VVbool (Lazy.force v <> None)
      | _ -> raise Not_found)
  | [ "has_history" ] -> VVbool (Sys.file_exists (History.file_name conf))
  | [ "has_misc_notes" ] -> VVbool (Gwdb.read_nldb base <> [])
  | [ "nb_accesses" ] ->
      let r = count conf in
      let s =
        Mutil.string_of_int_sep
          (Util.transl conf "(thousand separator)")
          (r.welcome_cnt + r.request_cnt)
      in
      VVstring s
  | [ "nb_accesses_to_welcome" ] ->
      let r = count conf in
      let s =
        Mutil.string_of_int_sep
          (Util.transl conf "(thousand separator)")
          r.welcome_cnt
      in
      VVstring s
  | [ "random"; "init" ] ->
      Random.self_init ();
      VVstring ""
  | [ "random"; s ] -> (
      try VVstring (string_of_int (Random.int (int_of_string s)))
      with Failure _ | Invalid_argument _ -> raise Not_found)
  | [ "sosa_ref" ] -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> (
          match Lazy.force v with
          | Some p ->
              VVstring
                (NameDisplay.referenced_person_title_text conf base p
                  : Adef.safe_string
                  :> string)
          | None -> raise Not_found)
      | _ -> raise Not_found)
  | [ "start_date" ] ->
      VVstring (string_of_start_date conf : Adef.safe_string :> string)
  | [ "wiznotes_dir_exists" ] ->
      VVbool (Sys.file_exists (WiznotesDisplay.dir conf base))
  | _ -> raise Not_found

let print_foreach _conf _print_ast _eval_expr = raise Not_found
let eval_predefined_apply _conf _env _f _vl = raise Not_found

let print_start conf base =
  let env =
    let sosa_ref_l =
      let sosa_ref () = Util.find_sosa_ref conf base in
      Lazy.from_fun sosa_ref
    in
    [ ("sosa_ref", Vsosa_ref sosa_ref_l) ]
  in
  Hutil.interp conf "welcome"
    {
      Templ.eval_var = eval_var conf base;
      Templ.eval_transl = (fun _env -> Templ.eval_transl conf);
      Templ.eval_predefined_apply = eval_predefined_apply conf;
      Templ.get_vother;
      Templ.set_vother;
      Templ.print_foreach = print_foreach conf;
    }
    env ()

(* code déplacé et modifié pour gérer advanced.txt *)
let print conf base fname =
  if Sys.file_exists (Util.etc_file_name fname) then
    Hutil.interp conf fname
      {
        Templ.eval_var = eval_var conf base;
        Templ.eval_transl = (fun _env -> Templ.eval_transl conf);
        Templ.eval_predefined_apply = eval_predefined_apply conf;
        Templ.get_vother;
        Templ.set_vother;
        Templ.print_foreach = print_foreach conf;
      }
      [] ()
  else gen_print Lang conf base fname
