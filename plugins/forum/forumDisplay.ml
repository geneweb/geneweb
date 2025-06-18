(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
open Def
open Util
open Forum
module Gutil = Geneweb_db.Gutil

type 'a env =
  | Vmess of message * message option * MF.pos * MF.pos * string option
  | Vpos of MF.pos ref
  | Vother of 'a
  | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let print_foreach conf _base print_ast eval_expr =
  let eval_int_expr env e =
    let s = eval_expr env () e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env _xx _loc s sl el al =
    match s :: sl with
    | [ "message" ] -> print_foreach_message env el al
    | _ -> raise Not_found
  and print_foreach_message env el al =
    let eval_pos_expr env e = MF.pos_of_string (eval_expr env () e) in
    let to_pos, max_mess =
      match el with
      | [ [ e1 ]; [ e2 ] ] -> (eval_pos_expr env e1, eval_int_expr env e2)
      | _ -> raise Not_found
    in
    let fname = forum_file conf in
    match try Some (MF.open_in fname) with Sys_error _ -> None with
    | Some ic ->
        let rec loop prev_mess i =
          if i >= max_mess then MF.rpos_in ic
          else
            let pos = MF.rpos_in ic in
            match read_message conf ic with
            | Some (mess, accessible) ->
                if accessible && is_visible conf mess then (
                  let next_pos = MF.rpos_in ic in
                  let vmess = Vmess (mess, prev_mess, pos, next_pos, None) in
                  let env = Templ.Env.add "mess" vmess env in
                  List.iter (print_ast env ()) al;
                  loop (Some mess) (i + 1))
                else loop prev_mess i
            | None -> MF.not_a_pos
        in
        (if to_pos = MF.not_a_pos then ()
         else try MF.rseek_in ic to_pos with Sys_error _ -> ());
        let pos = loop None 0 in
        (match get_env "pos" env with Vpos r -> r := pos | _ -> ());
        MF.close_in ic
    | None -> ()
  in
  print_foreach

let str_val x = Templ.VVstring x

let safe_val (x : [< `encoded | `escaped | `safe ] Adef.astring) =
  Templ.VVstring ((x :> Adef.safe_string) :> string)

let rec eval_var conf base env _xx _loc = function
  | [ "can_post" ] -> Templ.VVbool (can_post conf)
  | [ "is_moderated_forum" ] -> VVbool (moderators conf <> [])
  | [ "is_moderator" ] -> VVbool (is_moderator conf)
  | "message" :: sl -> eval_message_var conf base env sl
  | [ "pos" ] -> (
      match get_env "pos" env with
      | Vpos r -> safe_val (MF.string_of_pos !r)
      | _ -> raise Not_found)
  | _ -> raise Not_found

and eval_message_var conf base env = function
  | [ "access" ] -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> str_val mess.m_access
      | _ -> raise Not_found)
  | "date" :: sl -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> eval_date_var conf mess.m_date sl
      | _ -> raise Not_found)
  | "email" :: sl -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, so) ->
          eval_message_string_var conf mess.m_email so sl
      | _ -> raise Not_found)
  | [ "friend" ] ->
      if passwd_in_file conf "friend" then
        match get_env "mess" env with
        | Vmess (mess, _, _, _, _) -> str_val mess.m_friend
        | _ -> raise Not_found
      else str_val ""
  | [ "from" ] -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> str_val mess.m_from
      | _ -> raise Not_found)
  | [ "hour" ] -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> str_val mess.m_hour
      | _ -> raise Not_found)
  | "ident" :: sl -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, so) ->
          eval_message_string_var conf mess.m_ident so sl
      | _ -> raise Not_found)
  | [ "is_waiting" ] -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> VVbool mess.m_waiting
      | _ -> raise Not_found)
  | [ "next_pos" ] -> (
      match get_env "mess" env with
      | Vmess (_, _, pos, _, _) ->
          let rec loop pos =
            let back_pos = backward_pos conf pos in
            match get_message conf back_pos with
            | Some (acc, mess, _, _) ->
                if back_pos = pos then str_val ""
                else if acc && is_visible conf mess then
                  safe_val (MF.string_of_pos back_pos)
                else loop back_pos
            | None -> str_val ""
          in
          loop pos
      | _ -> raise Not_found)
  | [ "pos" ] -> (
      match get_env "mess" env with
      | Vmess (_, _, pos, _, _) -> safe_val (MF.string_of_pos pos)
      | _ -> raise Not_found)
  | "prev_date" :: sl -> (
      match get_env "mess" env with
      | Vmess (_, prev_mess, _, _, _) -> (
          match prev_mess with
          | Some mess -> eval_date_var conf mess.m_date sl
          | None -> str_val "")
      | _ -> raise Not_found)
  | [ "prev_pos" ] -> (
      match get_env "mess" env with
      | Vmess (_, _, _, next_pos, _) ->
          let rec loop next_pos =
            match get_message conf next_pos with
            | Some (acc, mess, next_pos, next_next_pos) ->
                if acc && is_visible conf mess then
                  safe_val (MF.string_of_pos next_pos)
                else loop next_next_pos
            | None -> str_val ""
          in
          loop next_pos
      | _ -> raise Not_found)
  | "subject" :: sl -> (
      match get_env "mess" env with
      | Vmess (m, _, _, _, so) -> eval_message_string_var conf m.m_subject so sl
      | _ -> raise Not_found)
  | "text" :: sl -> (
      match get_env "mess" env with
      | Vmess (m, _, _, _, so) -> eval_message_text_var conf base m.m_text so sl
      | _ -> raise Not_found)
  | "time" :: sl -> (
      match get_env "mess" env with
      | Vmess (m, _, _, _, so) -> eval_message_text_var conf base m.m_time so sl
      | _ -> raise Not_found)
  | [ "wiki" ] -> (
      match get_env "mess" env with
      | Vmess (mess, _, _, _, _) -> str_val mess.m_wiki
      | _ -> raise Not_found)
  | [ "wizard" ] ->
      if passwd_in_file conf "wizard" then
        match get_env "mess" env with
        | Vmess (mess, _, _, _, _) -> str_val mess.m_wizard
        | _ -> raise Not_found
      else str_val ""
  | _ -> raise Not_found

and eval_date_var conf date = function
  | [ "month" ] -> (
      match date with
      | Dgreg (d, _) -> str_val (string_of_int d.month)
      | _ -> str_val "")
  | [] ->
      str_val
        (Util.translate_eval (DateDisplay.string_of_date conf date :> string))
  | _ -> raise Not_found

and eval_message_text_var conf base str so = function
  | [ "wiki" ] ->
      let s = string_with_macros conf [] str in
      let lines = Wiki.html_of_tlsw conf s in
      let s = String.concat "\n" lines in
      let s =
        let wi =
          {
            Wiki.wi_mode = "NOTES";
            Wiki.wi_file_path = Notes.file_path conf base;
            Wiki.wi_person_exists = person_exists conf base;
            Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
            Wiki.wi_always_show_link = conf.wizard || conf.friend;
          }
        in
        Wiki.syntax_links conf wi s
      in
      let s =
        match so with
        | Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h s
        | None -> s
      in
      str_val s
  | [ "nowiki" ] ->
      let s = string_with_macros conf [] str in
      let s =
        match so with
        | Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h s
        | None -> s
      in
      str_val s
  | [ "raw" ] -> str_val str
  | sl -> eval_message_string_var conf str so sl

and eval_message_string_var conf str so = function
  | [ "cut"; s ] -> (
      try str_val (sp2nbsp (int_of_string s) str)
      with Failure _ -> raise Not_found)
  | [ "v" ] -> safe_val (Util.escape_html str)
  | [] ->
      let s = Util.escape_html str in
      let s =
        match so with
        | Some h ->
            let case_sens = p_getenv conf.env "c" = Some "on" in
            html_highlight case_sens h (s : Adef.escaped_string :> string)
            |> Adef.escaped
        | None -> s
      in
      safe_val s
  | _ -> raise Not_found

let print conf base env =
  let ifun =
    Templ.
      {
        eval_var = eval_var conf base;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = print_foreach conf base;
      }
  in
  Templ.output conf ifun env () "forum"

let visualize conf base mess =
  let vmess = Vmess (mess, None, MF.not_a_pos, MF.not_a_pos, None) in
  let env = Templ.Env.(add "mess" vmess empty) in
  print conf base env

let message_txt conf n =
  transl_nth conf "message/previous message/previous messages/next message" n

let print_aux conf pos title =
  Hutil.header conf title;
  (match pos with
  | Some pos ->
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=FORUM&p=|};
      Output.print_string conf (MF.string_of_pos pos);
      Output.print_sstring conf {|">|};
      message_txt conf 3 (* FIXME: safe_string? *)
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf {|</a>|}
  | None ->
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=FORUM">|};
      transl conf "database forum"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf {|</a>|});
  Hutil.trailer conf

let print_del_ok conf next_pos =
  print_aux conf next_pos @@ fun _ ->
  transl conf "message deleted"
  |> Utf8.capitalize_fst |> Output.print_sstring conf

let print_valid_ok conf pos del =
  print_aux conf pos @@ fun _ ->
  if del then
    transl conf "message deleted"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  else
    transl conf "message added"
    |> Utf8.capitalize_fst |> Output.print_sstring conf

let print_forum_message conf base r so =
  let env =
    match r with
    | Some (acc, mess, pos, next_pos) when acc && is_visible conf mess ->
        Templ.Env.(
          empty
          |> add "mess" (Vmess (mess, None, pos, next_pos, so))
          |> add "pos" (Vpos (ref pos)))
    | Some _ | None -> Templ.Env.(add "pos" (Vpos (ref MF.not_a_pos)) empty)
  in
  print conf base env

let print_forum_headers conf base =
  let env = Templ.Env.(add "pos" (Vpos (ref MF.not_a_pos)) empty) in
  print conf base env

let valid_forum_message conf base pos =
  match get_message conf pos with
  | Some (a, _, _, _) ->
      if a && conf.wizard && List.mem conf.user (moderators conf) then
        let del =
          match p_getenv conf.env "d" with
          | Some "" | None -> false
          | Some _ -> true
        in
        if set_validator conf pos then (
          if del then forum_del conf pos;
          print_valid_ok conf (Some pos) del)
        else print_forum_headers conf base
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_valid conf base =
  match p_getenv conf.env "p" with
  | Some pos -> valid_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

let print conf base =
  let r =
    match p_getenv conf.env "p" with
    | Some pos -> get_message conf (MF.pos_of_string pos)
    | None -> None
  in
  print_forum_message conf base r None

let print_add_ok conf base =
  let mess =
    let time = Util.sprintf_today conf in
    let ident = String.trim (get conf "Ident") in
    let email = String.trim (get conf "Email") in
    let subject = String.trim (get conf "Subject") in
    let text = Gutil.trim_trailing_spaces (get1 conf "Text") in
    {
      m_time = (time :> string);
      m_date = Dtext "";
      m_hour = "";
      m_waiting = false;
      m_from = "";
      m_ident = ident;
      m_wizard = "";
      m_friend = "";
      m_email = email;
      m_access = "";
      m_subject = subject;
      m_wiki = "on";
      m_text = text;
    }
  in
  if not (can_post conf) then Hutil.incorrect_request conf
  else if match p_getenv conf.env "visu" with Some _ -> true | None -> false
  then visualize conf base mess
  else if mess.m_ident = "" || mess.m_text = "" then print conf base
  else
    let title _ =
      transl conf "message added"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    let mods = moderators conf in
    forum_add conf base (mods <> []) mess;
    Hutil.header conf title;
    if mods <> [] then (
      Output.print_sstring conf "<p>";
      transl conf "this forum is moderated"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf ". ";
      transl conf "your message is waiting for validation"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf ".</p>");
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=FORUM" id="reference">|};
    transl conf "database forum"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf {|</a> |};
    Hutil.trailer conf;
    Output.print_sstring conf
      (Printf.sprintf
         {|<script>
  if (document.title == "%s") {
    document.getElementById("reference").focus();
  }
</script>|}
         (transl conf "message added" |> Utf8.capitalize_fst))

let print_add conf base = print conf base

let delete_forum_message conf base pos =
  match get_message conf pos with
  | Some (a, m, _, _) ->
      if
        a && conf.wizard && conf.user <> "" && m.m_wizard = conf.user
        && passwd_in_file conf "wizard"
        || conf.manitou || conf.supervisor
      then (
        forum_del conf pos;
        print_del_ok conf (find_next_pos conf pos))
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_del conf base =
  match p_getenv conf.env "p" with
  | Some pos -> delete_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

(* access switch *)

let access_switch_forum_message conf base pos =
  match get_message conf pos with
  | Some (a, m, _, _) ->
      if
        (a && conf.wizard && conf.user <> "" && m.m_wizard = conf.user
         && passwd_in_file conf "wizard"
        || conf.manitou || conf.supervisor)
        && set_access conf pos
      then print_forum_message conf base (get_message conf pos) None
      else print_forum_headers conf base
  | None -> print_forum_headers conf base

let print_access_switch conf base =
  match p_getenv conf.env "p" with
  | Some pos -> access_switch_forum_message conf base (MF.pos_of_string pos)
  | None -> print_forum_headers conf base

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let fname = forum_file conf in
  match try Some (MF.open_in fname) with Sys_error _ -> None with
  | Some ic -> (
      let case_sens = p_getenv conf.env "c" = Some "on" in
      let rec loop () =
        let pos = MF.rpos_in ic in
        match read_message conf ic with
        | Some (m, accessible) ->
            if
              accessible
              && List.exists (in_text case_sens s)
                   [ m.m_ident; m.m_subject; m.m_time; m.m_text ]
            then Some (m, pos)
            else loop ()
        | None -> None
      in
      (match p_getenv conf.env "p" with
      | Some pos ->
          let pos = MF.pos_of_string pos in
          (try MF.rseek_in ic pos with Sys_error _ -> ());
          let _ = read_message conf ic in
          ()
      | None -> ());
      let messo = loop () in
      let next_pos = MF.rpos_in ic in
      MF.close_in ic;
      match messo with
      | Some (mess, pos) ->
          let r = Some (true, mess, pos, next_pos) in
          print_forum_message conf base r (Some s)
      | None -> print_forum_headers conf base)
  | None -> print_forum_headers conf base

let print_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
  | Some s -> search_text conf base (Mutil.gen_decode false s)
  | None -> print_forum_headers conf base
