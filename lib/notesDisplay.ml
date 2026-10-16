(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
open Notes

let src = Logs.Src.create ~doc:"NotesDisplay" "NOTE"

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Server = Geneweb_http.Server
module Connection = Geneweb_http.Connection

(* [deprecated] TYPE=album is accepted on read for backward compatibility
   with notes, all new notes always write TYPE=gallery. *)
let norm_type = function "album" -> "gallery" | t -> t

let is_ancestor conf base anc =
  let restrict_for_spouses =
    try List.assoc "restrict_for_spouses" conf.base_env = "yes"
    with Not_found -> false
  in
  let max =
    match List.assoc_opt "restrict_note_max" conf.base_env with
    | Some s -> ( try 2 lsl int_of_string s with Failure _ -> 32)
    | None -> 32
  in
  let test_ancestor p1 p2 =
    let ip1 = Driver.get_iper p1 in
    let ip2 = Driver.get_iper p2 in
    ip1 = ip2
    || try Gutil.is_ancestor ~max base p1 p2 with Gutil.Same_person -> true
  in
  let bad_format n =
    Log.warn (fun k -> k "bad pnoc format in RESTRICT (%d) (%s).\n" n anc)
  in
  if String.length anc <= 4 then (
    bad_format 1;
    false)
  else
    let anc = String.sub anc 2 (String.length anc - 4) in
    let parts = String.split_on_char '/' anc in
    if List.length parts <> 3 then (
      bad_format 2;
      false)
    else
      let fn = List.nth parts 0 |> Name.lower in
      let sn = List.nth parts 1 |> Name.lower in
      let oc =
        try int_of_string (List.nth parts 2)
        with Failure _ ->
          bad_format 3;
          0
      in
      match (Driver.person_of_key base fn sn oc, conf.user_iper) with
      | Some anc_ip, Some user_ip ->
          test_ancestor (Driver.poi base anc_ip) (Driver.poi base user_ip)
          || restrict_for_spouses
             &&
             let families = Driver.get_family (Driver.poi base user_ip) in
             Array.exists
               (fun fam ->
                 let fam = Driver.foi base fam in
                 let fath_ip = Driver.get_father fam in
                 let moth_ip = Driver.get_mother fam in
                 let spouse_ip =
                   if user_ip = fath_ip then moth_ip else fath_ip
                 in
                 test_ancestor (Driver.poi base anc_ip)
                   (Driver.poi base spouse_ip))
               families
      | _ -> false

let rec is_restricted conf base anc_l =
  match anc_l with
  | [] -> true
  | anc :: l ->
      if is_ancestor conf base anc then false else is_restricted conf base l

(* "full word" box of the notes search form: [word=on] *)
let full_word_search conf = p_getenv conf.env "word" = Some "on"

let print_search_form conf =
  Output.printf conf
    {|<div class="mt-3">
<form class="d-flex align-items-center gap-2" method="get" action="%s">|}
    conf.command;
  hidden_env conf;
  Output.print_sstring conf
    {|<input type="hidden" name="m" value="MISC_NOTES_SEARCH">|};
  Output.print_sstring conf
    {|<input type="text" name="s" size="40" class="form-control w-auto" value="|};
  (match p_getenv conf.env "s" with
  | Some s -> Output.print_string conf (Util.escape_html s)
  | None -> ());
  Output.print_sstring conf "\">";
  Output.print_sstring conf {|<div class="form-check form-check-inline mb-0">|};
  Output.printf conf
    {|<input type="checkbox" name="c" value="on" class="form-check-input" id="case"%s>|}
    (match p_getenv conf.env "c" with
    | Some "on" -> " checked"
    | Some _ | None -> "");
  Output.printf conf {|<label class="form-check-label" for="case">%s</label>|}
    (transl_nth conf "search/case sensitive" 1);
  Output.print_sstring conf "</div>";
  Output.print_sstring conf {|<div class="form-check form-check-inline mb-0">|};
  Output.printf conf
    {|<input type="checkbox" name="word" value="on" class="form-check-input" id="word"%s>|}
    (if full_word_search conf then " checked" else "");
  Output.printf conf {|<label class="form-check-label" for="word">%s</label>|}
    (transl conf "word match");
  Output.print_sstring conf "</div>";
  Output.print_sstring conf {|<button type="submit" class="btn btn-primary">|};
  transl_nth conf "search/case sensitive" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button>\n</form>\n";
  (* friends and wizards can also search person, family and event notes *)
  if conf.wizard || conf.friend then
    Output.printf conf
      {|<div class="mt-1"><a href="%sm=NOTES_SEARCH%s%s%s">%s</a></div>|}
      (commd conf :> string)
      (match p_getenv conf.env "s" with
      | Some s -> "&s=" ^ (Mutil.encode s :> string)
      | None -> "")
      (if p_getenv conf.env "c" = Some "on" then "&c=on" else "")
      (if full_word_search conf then "&word=on" else "")
      (transl conf "notes search" |> Utf8.capitalize_fst);
  Output.print_sstring conf "</div>"

let print_whole_notes conf base fnotes (title : Adef.safe_string) s ho =
  Hutil.header_without_title conf;
  let title_html =
    if (title :> string) <> "" then
      let title_text =
        match ho with
        | Some (case_sens, h) ->
            NotesSearch.highlight_html ~full_word:(full_word_search conf)
              case_sens
              (NotesSearch.terms case_sens h)
              (title : Adef.safe_string :> string)
            |> Adef.safe
        | None -> title
      in
      Format.sprintf "<h1>%s</h1>" (title_text :> string)
    else ""
  in
  let modbtn =
    if conf.wizard then Wiki.make_edit_button conf ~mode:"NOTES" fnotes ()
    else ""
  in
  let db = notes_links_db conf base false in
  let pgl =
    match List.assoc_opt fnotes db with Some _ -> true | None -> false
  in
  let from_where =
    if pgl then
      Format.sprintf
        {|
 <a href="%sm=NOTES&f=%s&ref=on"
     class="align-self-center ms-3 mb-1"
     title="%s">(<-)</a>|}
        (commd conf :> string)
        (fnotes :> string)
        (transl conf "linked pages (pages) help" |> Utf8.capitalize_fst)
    else ""
  in
  Output.printf conf {|<div class="d-flex mb-3">%s%s%s</div>|} title_html modbtn
    from_where;
  let file_path = file_path conf base in
  let s = Util.string_with_macros conf [] s in
  let edit_opt = Some (conf.wizard, "NOTES", fnotes) in
  let s =
    let wi =
      {
        Wiki.wi_mode = "NOTES";
        Wiki.wi_file_path = file_path;
        Wiki.wi_person_exists = person_exists conf base;
        Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
        Wiki.wi_always_show_link = conf.wizard || conf.friend;
      }
    in
    Wiki.html_with_summary_of_tlsw conf wi edit_opt s
  in
  let s = Util.safe_html s in
  let s =
    match ho with
    | Some (case_sens, h) ->
        NotesSearch.highlight_html ~full_word:(full_word_search conf) case_sens
          (NotesSearch.terms case_sens h)
          (s :> string)
        |> Adef.safe
    | None -> s
  in
  Output.print_string conf s;
  if ho <> None then print_search_form conf;
  Hutil.trailer conf

let print_notes_part conf base fnotes (title : Adef.safe_string) s cnt0 =
  Hutil.header_with_title conf (fun _ ->
      if (title :> string) = "" then
        Output.print_string conf (Util.escape_html fnotes)
      else Output.print_string conf title);
  let s = string_with_macros conf [] s in
  let lines = Wiki.extract_sub_part s cnt0 in
  let has_next = Wiki.extract_sub_part s (cnt0 + 1) <> [] in
  let mode = "NOTES" in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = person_exists conf base;
      Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  Wiki.print_sub_part conf wi ~has_next conf.wizard mode fnotes cnt0 lines;
  Hutil.trailer conf

let fmt_fnote_title conf base fnotes =
  let nenv, _ = read_notes base fnotes in
  let no_title = Utf8.capitalize_fst (transl conf "note without title") in
  let no_title = no_title |> Format.sprintf "<i>%s</i>" in
  try
    let fnote_title = List.assoc "TITLE" nenv in
    if String.trim fnote_title = "" then no_title else fnote_title
  with Not_found -> no_title

let person_query_suffix conf base = function
  | None -> ""
  | Some p -> "&" ^ (Util.acces conf base p :> string)

let person_to_key base p =
  let fn = Name.lower (Driver.sou base (Driver.get_first_name p)) in
  let sn = Name.lower (Driver.sou base (Driver.get_surname p)) in
  (fn, sn, Driver.get_occ p)

let linked_page_rows conf base ?person pg pgl =
  let typ = p_getenv conf.env "type" in
  match (pg, typ) with
  | Def.NLDB.PgInd ip, _ ->
      let p = pget conf base ip in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_IND&i=%s#notes" title="%s">
    <i class="fa fa-user"></i></a></td>|}
             (commd conf :> string)
             (Driver.Iper.to_string ip)
             (Utf8.capitalize_fst (transl conf "modify note")));
      Output.print_sstring conf
        (Format.sprintf {|
<td>%s%s</td>
<td><i>%s</i></td><td></td>|}
           (Util.referenced_person_title_text conf base p :> string)
           (DateDisplay.short_dates_text conf base p :> string)
           (Utf8.capitalize_fst (transl conf "individual notes")))
  | Def.NLDB.PgFam ifam, _ ->
      let fam = Driver.foi base ifam in
      let fath = pget conf base (Driver.get_father fam) in
      let moth = pget conf base (Driver.get_mother fam) in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="align-middle">
  <a href="%sm=MOD_FAM&i=%s&ip=%s#events" title="%s %s %s">
    <i class="fa fa-user fa-sm"></i><i class="fa fa-user fa-sm"></i></a></td>|}
             (commd conf :> string)
             (Driver.Ifam.to_string ifam)
             (Driver.get_iper fath |> Driver.Iper.to_string)
             (Utf8.capitalize_fst (transl conf "modify"))
             (transl_nth conf "event/events" 0)
             (transl_nth conf "family/families" 0));
      Output.print_sstring conf
        (Format.sprintf
           {|
<td>%s%s<br>& %s%s</td>
<td class="align-middle"><i>%s %s</i></td><td></td>|}
           (Util.referenced_person_title_text conf base fath :> string)
           (DateDisplay.short_dates_text conf base fath :> string)
           (Util.referenced_person_title_text conf base moth :> string)
           (DateDisplay.short_dates_text conf base moth :> string)
           (Utf8.capitalize_fst (transl conf "comment"))
           (transl_nth conf "family/families" 0))
  | Def.NLDB.PgNotes, _ ->
      let fnote_title = fmt_fnote_title conf base "" in
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_NOTES" title="%s"><i class="far fa-file-lines"></i></a>
</td>|}
             (commd conf :> string)
             (Utf8.capitalize_fst (transl conf "modify note")));
      Output.print_sstring conf
        (Format.sprintf
           {|
<td><a href="%sm=NOTES">%s</a></td>
<td>%s</td><td></td>|}
           (commd conf :> string)
           (Utf8.capitalize (transl conf "base notes"))
           (Util.safe_html fnote_title :> string))
  | Def.NLDB.PgMisc fnotes, typ ->
      let nenv, _ = read_notes base fnotes in
      let n_type =
        try norm_type (List.assoc "TYPE" nenv) with Not_found -> ""
      in
      let kq =
        if n_type = "gallery" then person_query_suffix conf base person else ""
      in
      let view_route = if n_type = "gallery" then "GALLERY" else "NOTES" in
      let mod_route =
        if n_type = "gallery" then "MOD_GALLERY" else "MOD_NOTES"
      in
      if match typ with Some t when t = "" -> t = n_type | _ -> true then (
        let fnote_title = fmt_fnote_title conf base fnotes in
        if conf.wizard then
          Output.print_sstring conf
            (Format.sprintf
               {|
<td class="text-center">
  <a href="%sm=%s&f=%s%s" title="%s"><i class="%s"></i></a>
</td>|}
               (commd conf :> string)
               mod_route (Util.uri_encode fnotes) kq
               (Utf8.capitalize_fst
                  (transl conf
                     (if n_type = "gallery" then "modify gallery"
                      else "modify note")))
               (if n_type = "gallery" then "far fa-image fa-fw"
                else "far fa-file-lines fa-fw"));
        Output.print_sstring conf
          (Format.sprintf
             {|
<td><a href="%sm=%s&f=%s%s">%s</a></td>
<td>%s</td>%s|}
             (commd conf :> string)
             view_route (Util.uri_encode fnotes) kq
             (fnotes :> string)
             (Util.safe_html fnote_title :> string)
             (if pgl || n_type = "gallery" then
                Format.sprintf
                  {|
<td><a href="%sm=%s&f=%s&ref=on"
       title="%s"><-</a></td>|}
                  (commd conf :> string)
                  view_route
                  (fnotes :> string)
                  (Utf8.capitalize_fst
                     (transl conf "linked pages (pages) help"))
              else "<td></td>")))
  | Def.NLDB.PgWizard wizname, _ ->
      if conf.wizard then
        Output.print_sstring conf
          (Format.sprintf
             {|
<td class="text-center">
  <a href="%sm=MOD_WIZNOTES&f=%s" title="%s %s">
    <i class="fas fa-hat-wizard"></i></a></td>|}
             (commd conf :> string)
             (Util.uri_encode wizname)
             (Utf8.capitalize_fst (transl conf "modify"))
             (transl conf "base wizard notes"));
      Output.print_sstring conf
        (Format.sprintf
           {|
<td><a href="%sm=WIZNOTES&f=%s">%s</a></td>
<td><i>%s</i></td><td></td>|}
           (commd conf :> string)
           (Util.uri_encode wizname)
           (wizname :> string)
           (Utf8.capitalize_fst (transl conf "base wizard notes")))

let create_gallery_item_idx conf base person fnotes title
    (img_idx, img_url, _img_name, desc) =
  let max_len = 40 in
  let caption =
    match (title, desc) with
    | "", "" -> fnotes
    | t, "" -> t
    | "", d -> d
    | t, d ->
        let s = t ^ " | " ^ d in
        if String.length s > max_len then String.sub s 0 (max_len - 1) ^ "…"
        else s
  in
  let img_param = Printf.sprintf "&img=%d" img_idx in
  let kq = person_query_suffix conf base person in
  Printf.sprintf
    {|<div class="imap-gallery"><a href="%sm=GALLERY&f=%s%s%s"><img src="%s" title="%s" alt="%s"><div class="gallery-legend">%s</div></a></div>|}
    (commd conf :> string)
    (Util.uri_encode fnotes) kq img_param img_url fnotes fnotes caption

let print_linked_list_gallery conf base ?person pgl =
  Output.printf conf "<div class=\"d-flex flex-wrap mt-3\">\n";
  List.iter
    (function
      | Def.NLDB.PgMisc fnotes ->
          let nenv, s = read_notes base fnotes in
          let typ =
            try norm_type (List.assoc "TYPE" nenv) with Not_found -> ""
          in
          let restrict_l =
            try List.assoc "RESTRICT" nenv with Not_found -> ""
          in
          let restrict_l =
            if restrict_l = "" then [] else String.split_on_char ',' restrict_l
          in
          if
            (restrict_l = [] || not (is_restricted conf base restrict_l))
            && typ = "gallery"
          then
            let title = try List.assoc "TITLE" nenv with Not_found -> "" in
            let items =
              match person with
              | Some p ->
                  Notes.json_gallery_items_for_key conf s (person_to_key base p)
              | None -> []
            in
            let items =
              if items <> [] then items
              else
                let u, n = Notes.json_extract_img conf s in
                [ (1, u, n, "") ]
            in
            List.iter
              (fun item ->
                Output.print_sstring conf
                  (create_gallery_item_idx conf base person fnotes title item))
              items
      | _ -> ())
    pgl;
  Output.print_sstring conf "</div>\n"

let print_linked_list_standard conf base ?person pgl =
  let db = notes_links_db conf base false in
  Output.print_sstring conf
    "\n<table class=\"table table-borderless table-striped w-auto mt-3\">";
  List.iter
    (fun pg ->
      match pg with
      | Def.NLDB.PgMisc fnotes ->
          let nenv, _ = read_notes base fnotes in
          let restrict_l =
            try List.assoc "RESTRICT" nenv with Not_found -> ""
          in
          let restrict_l =
            if restrict_l = "" then [] else String.split_on_char ',' restrict_l
          in
          if restrict_l = [] || not (is_restricted conf base restrict_l) then (
            Output.print_sstring conf "\n<tr>";
            let pgl =
              match List.assoc_opt fnotes db with
              | Some _ -> true
              | None -> false
            in
            linked_page_rows conf base ?person pg pgl;
            Output.print_sstring conf "</tr>\n")
      | _ ->
          Output.print_sstring conf "\n<tr>";
          linked_page_rows conf base ?person pg false;
          Output.print_sstring conf "</tr>\n")
    pgl;
  Output.print_sstring conf "</table>"

let print_linked_list conf base ?person pgl =
  match p_getenv conf.env "type" with
  | Some "gallery" -> print_linked_list_gallery conf base ?person pgl
  | _ -> print_linked_list_standard conf base ?person pgl

(* copied from perso.ml *)
let simple_person_text conf base p p_auth : Adef.safe_string =
  if p_auth then
    match main_title conf base p with
    | Some t -> titled_person_text conf base p t
    | None -> gen_person_text conf base p
  else if is_hide_names conf p then Adef.safe (Util.private_txt conf "")
  else gen_person_text conf base p

let print_what_links_p conf base p =
  if authorized_age conf base p then (
    let key = person_to_key base p in
    let db = Driver.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    let pgl = Notes.links_to_ind conf base db key None in
    let title h =
      let lnkd_typ, lnkd_typ_help =
        match p_getenv conf.env "type" with
        | Some "gallery" -> ("linked galleries", "linked galleries help")
        | _ -> ("linked pages", "linked pages help")
      in
      let lnkd_typ_label = Util.transl conf lnkd_typ |> Utf8.capitalize_fst in
      (* The page <title> only accepts plain text (see Hutil.header, which
         calls [title true] there); the tooltip span is only valid in the
         on-page <h1> (h = false). *)
      (if h then lnkd_typ_label
       else
         Format.sprintf {|<span title="%s">%s</span>|}
           (Util.transl conf lnkd_typ_help |> Utf8.capitalize_fst)
           lnkd_typ_label)
      |> Output.print_sstring conf;
      Util.transl conf ":" |> Output.print_sstring conf;
      Output.print_sstring conf " ";
      if h then Output.print_string conf (simple_person_text conf base p true)
      else (
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (commd conf);
        Output.print_string conf (acces conf base p);
        Output.print_sstring conf {|">|};
        Output.print_string conf (simple_person_text conf base p true);
        Output.print_sstring conf {|</a>|})
    in
    Hutil.header conf title;
    print_linked_list conf base ~person:p pgl;
    Hutil.trailer conf)
  else Hutil.incorrect_request conf

let print_what_links conf base fnotes =
  let title h =
    let label = Util.transl conf "linked pages" |> Utf8.capitalize_fst in
    let colon = Util.transl conf ":" in
    (* The page <title> only accepts plain text (see Hutil.header, which
       calls [title true] there); the tooltip span is only valid in the
       on-page <h1> (h = false). *)
    (if h then Printf.sprintf "%s%s " label colon
     else
       Format.sprintf {|<span title="%s">%s%s </span>|}
         (Util.transl conf "linked pages (pages) help" |> Utf8.capitalize_fst)
         label colon)
    |> Output.print_sstring conf;
    if h then (
      Output.print_sstring conf "[";
      Output.print_string conf (Util.escape_html fnotes);
      Output.print_sstring conf "]")
    else (
      Output.print_sstring conf {|<span><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=NOTES&f=";
      Output.print_string conf (Mutil.encode fnotes);
      Output.print_sstring conf {|">|};
      Output.print_sstring conf fnotes;
      Output.print_sstring conf "</a></span>")
  in
  let db = notes_links_db conf base false in
  Hutil.header conf title;
  Option.iter (print_linked_list conf base) (List.assoc_opt fnotes db);
  Hutil.trailer conf

let read_notes_from_conf conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  read_notes base fnotes

let print conf base =
  let fnotes =
    match p_getenv conf.env "f" with
    | Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let nenv, s = read_notes base fnotes in
  let restrict_l = try List.assoc "RESTRICT" nenv with Not_found -> "" in
  let restrict_l =
    if restrict_l = "" then [] else String.split_on_char ',' restrict_l
  in
  if restrict_l <> [] && is_restricted conf base restrict_l then
    Output.print_sstring conf
      (transl conf "note is restricted" |> Utf8.capitalize_fst)
  else
    let title = try List.assoc "TITLE" nenv with Not_found -> "" in
    let title = Util.safe_html title in
    match p_getint conf.env "v" with
    | Some cnt0 -> print_notes_part conf base fnotes title s cnt0
    | None -> print_whole_notes conf base fnotes title s None

let print_mod_gallery_json conf base =
  let nenv, s = read_notes_from_conf conf base in
  let s_digest =
    List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" nenv ^ s
  in
  let digest = Mutil.digest s_digest in
  let s = Notes.mark_pnocs_validity base s in
  Output.printf conf "{\"digest\":\"%s\",\"r\":%s}" digest s

let print_mod conf base =
  let is_new_note = Util.p_getenv conf.env "new" <> None in
  let fnotes =
    if is_new_note then ""
    else
      match p_getenv conf.env "f" with
      | Some f -> if NotesLinks.check_file_name f <> None then f else ""
      | None -> ""
  in
  let nenv, s = if is_new_note then ([], "") else read_notes base fnotes in
  let restrict_l = try List.assoc "RESTRICT" nenv with Not_found -> "" in
  let restrict_l =
    if restrict_l = "" then [] else String.split_on_char ',' restrict_l
  in
  if restrict_l <> [] && is_restricted conf base restrict_l then
    Output.print_sstring conf
      (transl conf "note is restricted" |> Utf8.capitalize_fst)
  else
    let title _ =
      Output.print_sstring conf (Utf8.capitalize_fst (transl conf "base notes"));
      if is_new_note then (
        Output.print_sstring conf " — ";
        Output.print_sstring conf
          (Utf8.capitalize_fst (transl_nth conf "new note name" 1)))
      else if fnotes <> "" then (
        Output.print_sstring conf " — ";
        Output.print_sstring conf fnotes)
    in
    Wiki.print_mod_view_page conf true (Adef.encoded "NOTES") fnotes title nenv
      s

let print_mod_ok conf base =
  let fname = function
    | Some f ->
        let f = Mutil.tr ' ' '_' f in
        if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let edit_mode _ = if conf.wizard then Some "NOTES" else None in
  let mode = "NOTES" in
  let read_string = read_notes base in
  let commit = Notes.commit_notes conf base in
  let string_filter = string_with_macros conf [] in
  let file_path = file_path conf base in
  let wi =
    {
      Wiki.wi_mode = mode;
      Wiki.wi_file_path = file_path;
      Wiki.wi_person_exists = person_exists conf base;
      Wiki.wi_mark_if_not_public = mark_if_not_public conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  Wiki.print_mod_ok conf wi edit_mode fname read_string commit string_filter
    true

let begin_text_without_html_tags lim s =
  let rec loop i size len =
    if i >= String.length s then Buff.get len
    else if size > lim && String.length s > i + 3 then Buff.get len ^ "..."
    else if s.[i] = '<' then
      let i =
        let rec loop i =
          if i = String.length s then i
          else if s.[i] = '>' then i + 1
          else loop (i + 1)
        in
        loop (i + 1)
      in
      loop i size len
    else if s.[i] = '=' then loop (i + 1) size len
    else
      let nbc = Utf8.nbc s.[i] in
      loop (i + nbc) (size + 1) (Buff.mstore len (String.sub s i nbc))
  in
  loop 0 0 0

(* Sorting directories first then files alphabetically *)
let sort_entries db =
  List.sort
    (fun (r1, opt1) (r2, opt2) ->
      match (opt1, opt2) with
      | None, Some _ -> -1
      | Some _, None -> 1
      | _ -> Gutil.alphabetic_order (Name.lower r1) (Name.lower r2))
    db

(* Build path hierarchy as directory entries *)
let build_path_hierarchy d =
  let parts = String.split_on_char NotesLinks.char_dir_sep d in
  let rec build_paths acc current = function
    | [] -> List.rev acc
    | part :: rest ->
        let new_current =
          if current = "" then part else current ^ NotesLinks.dir_sep ^ part
        in
        build_paths ((new_current, part) :: acc) new_current rest
  in
  build_paths [] "" parts

let can_see conf _f = conf.wizard || conf.friend

(* Format directory entry with proper indentation level *)
let format_folder_entry conf depth r path_to is_current is_path view =
  let view = view || can_see conf r in
  let margin =
    if depth = 0 then ""
    else
      Printf.sprintf " style=\"margin-left: %.1fem;\""
        (1.5 *. float_of_int depth)
  in
  let content =
    if is_current then
      Format.sprintf
        {|<span class="text-body-secondary">
          <i class="far fa-folder-open fa-fw me-2"></i>%s</span>|}
        (Util.escape_html r :> string)
    else
      let title_attr =
        let back_txt = Utf8.capitalize_fst (transl conf "back") in
        if is_path then
          if r = ".." then Format.sprintf {| title="%s .."|} back_txt
          else
            Format.sprintf {| title="%s %s"|} back_txt
              (Util.escape_html r :> string)
        else ""
      in
      if view then
        Format.sprintf
          {|<a href="%sm=MISC_NOTES&d=%s"%s>
            <i class="far fa-folder-open fa-fw me-2"></i>%s</a>|}
          (commd conf :> string)
          (Mutil.encode path_to :> string)
          title_attr
          (Util.escape_html r :> string)
      else
        Format.sprintf {|<i class="far fa-folder-open fa-fw me-2"%s></i>%s|}
          title_attr
          (Util.escape_html r :> string)
  in
  Format.sprintf {|<div class="mt-1"%s>%s</div>|} margin content

(* Format file entry with proper indentation level *)
let format_file_entry conf depth d f n_type title view =
  let view = view || can_see conf d in
  let icon = match n_type with "gallery" -> "image" | _ -> "file-lines" in
  let route = match n_type with "gallery" -> "GALLERY" | _ -> "NOTES" in
  let color, mod_edit =
    let notes_d = Filename.concat (!GWPARAM.bpath conf.bname) "notes_d" in
    let f = notes_d ^ Filename.dir_sep ^ d ^ NotesLinks.dir_sep ^ f ^ ".txt" in
    let f = Util.note_link_to_sys f in
    if Sys.file_exists f then ("", "") else (" text-danger", "MOD_")
  in
  let margin =
    if depth = 0 then ""
    else
      Printf.sprintf " style=\"margin-left: %.1fem;\""
        (1.5 *. float_of_int depth)
  in
  if view then
    Format.sprintf
      {|<div class="mt-1"%s>
         <a class="%s" href="%sm=%s%s&f=%s">
           <i class="far fa-%s fa-fw me-2"></i>%s</a>%s</div>|}
      margin color
      (commd conf :> string)
      mod_edit route
      (Mutil.encode (if d = "" then f else d ^ ":" ^ f) :> string)
      icon
      (Util.escape_html f :> string)
      (if (title :> string) <> "" then
         Format.sprintf {|<span class="text-body-secondary ms-2">%s</span>|}
           (title :> string)
       else "")
  else
    Format.sprintf
      {|<div class="mt-2"%s>
         <i class="far fa-%s fa-fw me-2"></i>
         <span class="text">%s</span></div>|}
      margin icon
      (Util.escape_html f :> string)

(* Format back button *)
let format_back_button conf d =
  Format.sprintf
    {|<div class="mb-3">
        <a href="%sm=MISC_NOTES%s" class="btn btn-outline-primary">
          <i class="fa fa-arrow-left me-2"></i>%s</a></div>|}
    (commd conf :> string)
    (match String.rindex_opt d NotesLinks.char_dir_sep with
    | Some i -> "&d=" ^ String.sub d 0 i
    | None -> "")
    (Utf8.capitalize_fst (transl conf "back"))

(* Format simple filename by removing parent directory *)
let format_simple_filename d f =
  let prefix = d ^ NotesLinks.dir_sep in
  let prefix_len = String.length prefix in
  if String.length f > prefix_len && String.sub f 0 prefix_len = prefix then
    String.sub f prefix_len (String.length f - prefix_len)
  else f

let check_note_access ~is_new conf base =
  let fnotes =
    if is_new then ""
    else
      match p_getenv conf.env "f" with
      | Some f -> if NotesLinks.check_file_name f <> None then f else ""
      | None -> ""
  in
  let nenv, _s = if is_new then ([], "") else read_notes base fnotes in
  let restrict_l = try List.assoc "RESTRICT" nenv with Not_found -> "" in
  let restrict_l =
    if restrict_l = "" then [] else String.split_on_char ',' restrict_l
  in
  restrict_l = [] || not (is_restricted conf base restrict_l)

let print_gallery conf base =
  if check_note_access ~is_new:false conf base then
    Templ.output_simple conf Templ.Env.empty "notes_gallery"
  else
    Output.print_sstring conf
      (transl conf "note is restricted" |> Utf8.capitalize_fst)

let print_mod_gallery_ok conn conf base =
  let fname = function
    | Some f ->
        let f = Mutil.tr ' ' '_' f in
        if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let edit_mode _ = if conf.wizard then Some "GALLERY" else None in
  let read_string = read_notes base in
  let commit = Notes.commit_notes conf base in
  let fname_saved = Wiki.do_mod_ok conf edit_mode fname read_string commit in
  let url =
    Printf.sprintf "%sm=GALLERY&f=%s"
      (commd conf :> string)
      (Mutil.encode fname_saved :> string)
  in
  Connection.http_redirect_temporarily conn url

let print_gallery_json conf base =
  let _, s = read_notes_from_conf conf base in
  let s = Notes.safe_gallery conf base (Notes.mark_pnocs_validity base s) in
  Output.print_sstring conf s

let print_mod_gallery conf base =
  let is_new = Util.p_getenv conf.env "new" <> None in
  if check_note_access ~is_new conf base then
    Templ.output_simple conf Templ.Env.empty "notes_upd_gallery"
  else
    Output.print_sstring conf
      (transl conf "note is restricted" |> Utf8.capitalize_fst)

let print_misc_notes conf base =
  let d = match p_getenv conf.env "d" with Some d -> d | None -> "" in
  let current_depth =
    if d = "" then 0
    else List.length (String.split_on_char NotesLinks.char_dir_sep d) + 1
  in
  (* ATTENTION check this if "notes_d" changes (REORG) *)
  let notes_d = Filename.concat (!GWPARAM.bpath conf.bname) "notes_d" in
  let path_hierarchy d =
    List.iteri
      (fun i (path_to, dirname) ->
        let parts = String.split_on_char NotesLinks.char_dir_sep d in
        let is_current = i = List.length parts - 1 in
        format_folder_entry conf (i + 1) dirname path_to is_current true true
        |> Output.print_sstring conf)
      (build_path_hierarchy d)
  in
  Hutil.header conf (fun _ -> ());
  Output.print_sstring conf
    (Format.sprintf
       {|<h1 class="mb-3" title="%s"><i class="far fa-clipboard fa-sm me-3"></i>%s</h1>|}
       (transl conf "miscellaneous notes help" |> Utf8.capitalize_fst)
       (if d <> "" then d
        else transl conf "miscellaneous notes" |> Utf8.capitalize_fst));
  if d <> "" then format_back_button conf d |> Output.print_sstring conf;
  let db = notes_links_db conf base true in
  let db =
    List.fold_right
      (fun (f, _) list ->
        if String.length f >= String.length d then
          if String.sub f 0 (String.length d) = d then
            let r =
              String.sub f (String.length d) (String.length f - String.length d)
            in
            if d = "" || (r <> "" && r.[0] = NotesLinks.char_dir_sep) then
              let r =
                if d = "" then r else String.sub r 1 (String.length r - 1)
              in
              try
                let i = String.index r NotesLinks.char_dir_sep in
                let r = String.sub r 0 i in
                match list with
                | (r', None) :: _ when r = r' -> list
                | _ -> (r, None) :: list
              with Not_found -> (r, Some f) :: list
            else list
          else list
        else list)
      db []
  in
  let db = sort_entries db in
  let one_folder r view =
    let simple_d = if d = "" then r else d ^ NotesLinks.dir_sep ^ r in
    format_folder_entry conf current_depth r simple_d false false view
    |> Output.print_sstring conf
  in
  let one_file f view =
    let envn, s = read_notes base f in
    let txt =
      let t = try List.assoc "TITLE" envn with Not_found -> "" in
      if t <> "" then (Util.escape_html t :> string)
      else if s = "" then ""
      else
        "<em>"
        ^ (begin_text_without_html_tags 50 s |> Util.escape_html :> string)
        ^ "</em>"
    in
    let n_type = try List.assoc "TYPE" envn with Not_found -> "" in
    let simple_f = format_simple_filename d f in
    format_file_entry conf current_depth d simple_f n_type txt view
    |> Output.print_sstring conf
  in
  let dirs_in_db, files_in_db =
    List.partition
      (fun f -> match f with _, None -> true | _, Some _f -> false)
      db
  in
  let files_in_db =
    List.fold_left
      (fun acc e -> match e with _, Some f -> f :: acc | _ -> acc)
      [] files_in_db
    |> List.rev
  in
  let dirs_in_db =
    List.fold_left
      (fun acc e -> match e with d, None -> d :: acc | _ -> acc)
      [] dirs_in_db
    |> List.rev
  in
  if db <> [] then (
    Output.print_sstring conf "<div>";
    if d <> "" then (
      format_folder_entry conf 0 ".." "" false true true
      |> Output.print_sstring conf;
      path_hierarchy d);
    Output.print_sstring conf {|<div class="ms-2">|};
    List.iter (fun d -> one_folder d true) dirs_in_db;
    List.iter (fun f -> one_file f true) files_in_db;
    Output.print_sstring conf "</div>");
  let d = Util.note_link_to_sys d in
  let cur_dir = Filename.concat notes_d d in
  let ls = Sys.readdir cur_dir |> Array.to_list in
  let files, dirs =
    List.fold_left
      (fun (acc_f, acc_d) f ->
        let stats = Unix.stat (Filename.concat cur_dir f) in
        match stats.st_kind with
        | S_DIR when f.[0] <> '.' ->
            if List.mem f dirs_in_db then (acc_f, acc_d) else (acc_f, f :: acc_d)
        | S_REG ->
            if f.[String.length f - 1] <> '~' && f.[0] <> '.' then
              let f =
                if d = "" then Filename.remove_extension f
                else d ^ NotesLinks.dir_sep ^ Filename.remove_extension f
              in
              if List.mem (Util.sys_to_note_link f) files_in_db then
                (acc_f, acc_d)
              else (f :: acc_f, acc_d)
            else (acc_f, acc_d)
        | _ -> (acc_f, acc_d))
      ([], []) ls
  in
  if dirs <> [] || files <> [] then (
    Output.print_sstring conf {|<div class="ms-2">|};
    Output.print_sstring conf
      (Format.sprintf
         {|<h2 class="h3 mt-3 mb-1" title="%s">%s
  <i class="far fa-circle-question fa-xs"></i></h2>|}
         (transl conf "files not in db help" |> Utf8.capitalize_fst)
         (transl conf "files not in db" |> Utf8.capitalize_fst));
    if d <> "" then (
      format_folder_entry conf 0 ".." "" false true true
      |> Output.print_sstring conf;
      path_hierarchy d);
    List.iter (fun r -> one_folder r false) dirs;
    Output.print_sstring conf "<p>";
    List.iter
      (fun f -> one_file (Filename.basename (Util.note_link_to_sys f)) false)
      files;
    Output.print_sstring conf {|</div>|});
  if d = "" then print_search_form conf;
  Hutil.trailer conf

(* searching *)
type search_hit = {
  sh_fnotes : string;
  sh_file : string;  (** file read, relative to the base directory *)
  sh_gallery : bool;  (** TYPE=gallery (or legacy album): open with m=GALLERY *)
  sh_title : string;  (** plain text *)
  sh_in_title : int;  (** number of terms found in the title *)
  sh_count : int;  (** occurrences in the body *)
  sh_snippet : string;  (** HTML, already escaped and highlighted *)
}

(* same rule as [print]: a note with a RESTRICT list is only visible to the
   descendants it names *)
let note_is_restricted conf base nenv =
  match List.assoc_opt "RESTRICT" nenv with
  | None | Some "" -> false
  | Some l -> is_restricted conf base (String.split_on_char ',' l)

(* File holding the note [fnotes], relative to the base directory, as read by
   [Driver.base_notes_read]: [notes] for the base notes. [None] for a name
   that [read_notes] cannot map to a file (it would read the base notes). *)
let note_file base fnotes =
  if fnotes = "" then Some "notes"
  else
    match NotesLinks.check_file_name fnotes with
    | Some (dl, f) ->
        Some
          (String.concat "/"
             (Driver.base_notes_dir base
             :: List.fold_right (fun d acc -> d :: acc) dl [ f ^ ".txt" ]))
    | None -> None

let is_gallery nenv =
  match List.assoc_opt "TYPE" nenv with
  | Some t -> norm_type t = "gallery"
  | None -> false

(* A note is a hit when every required term occurs in its title or its text
   and no excluded term does. *)
let collect_search_hits conf base ~full_word case_sens (terms, excluded) =
  let pages =
    ""
    :: List.filter_map
         (fun (fnotes, _) ->
           if fnotes = "" then None
           else
             match note_file base fnotes with
             | Some _ -> Some fnotes
             | None ->
                 Log.warn (fun k ->
                     k "notes search: skipping invalid page name %S" fnotes);
                 None)
         (notes_links_db conf base true)
  in
  List.fold_left
    (fun acc fnotes ->
      let nenv, nt = read_notes base fnotes in
      if note_is_restricted conf base nenv then acc
      else
        let tit = Option.value ~default:"" (List.assoc_opt "TITLE" nenv) in
        let plain = NotesSearch.plain_text nt in
        let plain_tit = NotesSearch.plain_text tit in
        (* Terms are matched on the decoded text ([plain]: entities, '' and
           = markup; wildcards). Without [full_word], [in_text] on the source
           is also tried; [full_word] needs to see the neighbouring
           characters, which only the decoded text gives. *)
        let in_tit t =
          NotesSearch.mem_all ~full_word case_sens [ t ] plain_tit
        in
        let in_title = List.length (List.filter in_tit terms) in
        let present t =
          in_tit t
          || ((not full_word) && in_text case_sens (NotesSearch.core t) nt)
          || NotesSearch.mem_all ~full_word case_sens [ t ] plain
        in
        if List.for_all present terms && not (List.exists present excluded) then
          {
            sh_fnotes = fnotes;
            sh_file = Option.value ~default:"" (note_file base fnotes);
            sh_gallery = is_gallery nenv;
            sh_title = plain_tit;
            sh_in_title = in_title;
            sh_count =
              NotesSearch.count_occurrences ~full_word case_sens terms plain;
            sh_snippet = NotesSearch.snippet ~full_word case_sens terms plain;
          }
          :: acc
        else acc)
    [] pages
  |> List.rev
  |> List.stable_sort (fun h1 h2 ->
      match Int.compare h2.sh_in_title h1.sh_in_title with
      | 0 -> Int.compare h2.sh_count h1.sh_count
      | c -> c)

let print_search_hit conf ~full_word case_sens s terms hit =
  let href =
    if hit.sh_gallery then
      Printf.sprintf "%sm=GALLERY&f=%s"
        (commd conf :> string)
        (Mutil.encode hit.sh_fnotes :> string)
    else
      Printf.sprintf "%sm=MISC_NOTES_SEARCH&s=%s%s%s&f=%s#first-found"
        (commd conf :> string)
        (Mutil.encode s :> string)
        (if case_sens then "&c=on" else "")
        (if full_word then "&word=on" else "")
        (Mutil.encode hit.sh_fnotes :> string)
  in
  let icon = if hit.sh_gallery then "image" else "file-lines" in
  let label =
    if hit.sh_title <> "" then
      NotesSearch.highlight ~full_word case_sens terms hit.sh_title
    else if hit.sh_fnotes = "" then
      (Util.escape_html (Utf8.capitalize_fst (transl conf "base notes"))
        :> string)
    else (Util.escape_html hit.sh_fnotes :> string)
  in
  let page_name =
    Format.sprintf {|<code class="small ms-2">%s</code>|}
      (Util.escape_html hit.sh_file :> string)
  in
  let count =
    if hit.sh_count = 0 then ""
    else
      Format.sprintf {|<span class="badge text-bg-secondary ms-2">%d</span>|}
        hit.sh_count
  in
  Output.printf conf
    {|<li class="list-group-item">
<div><i class="far fa-%s fa-fw me-2"></i><a href="%s">%s</a>%s%s</div>
<div class="small text-muted">%s</div>
</li>
|}
    icon href label page_name count hit.sh_snippet

let print_search_hits conf ~full_word case_sens s terms hits =
  let title _ =
    Printf.sprintf
      (ftransl conf "NOTIF all results for %s")
      (Util.escape_html s :> string)
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  (match hits with
  | [] ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "no match"))
  | _ :: _ ->
      Output.print_sstring conf {|<ul class="list-group mb-3">|};
      List.iter (print_search_hit conf ~full_word case_sens s terms) hits;
      Output.print_sstring conf "</ul>\n");
  print_search_form conf;
  Hutil.trailer conf

(* A gallery is a JSON document rendered by the notes_gallery template: show
   it as m=GALLERY&f=fnotes would, without highlighting. *)
let print_found_note conf base case_sens s fnotes =
  let nenv, nt = read_notes base fnotes in
  if is_gallery nenv then
    let env = ("f", Mutil.encode fnotes) :: List.remove_assoc "f" conf.env in
    print_gallery { conf with env } base
  else
    let tit = Option.value ~default:"" (List.assoc_opt "TITLE" nenv) in
    print_whole_notes conf base fnotes (Util.safe_html tit) nt
      (Some (case_sens, s))

(* [s] is split into terms (words, or "quoted phrases"), all required, except
   those written -word or -"phrase", which must not occur.
   [f] (as set by the links of the results page) names the note to show;
   [f=] is the base notes. Without [f], all notes are searched. *)
let search_text conf base s =
  let s = String.trim s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let terms, excluded = NotesSearch.query case_sens s in
  if terms = [] && excluded = [] then print_misc_notes conf base
  else
    let requested =
      match p_getenv conf.env "f" with
      | Some f when f = "" || NotesLinks.check_file_name f <> None ->
          let nenv, _ = read_notes base f in
          if note_is_restricted conf base nenv then None else Some f
      | Some _ | None -> None
    in
    match requested with
    | Some fnotes -> print_found_note conf base case_sens s fnotes
    | None -> (
        let full_word = full_word_search conf in
        match
          collect_search_hits conf base ~full_word case_sens (terms, excluded)
        with
        | [ hit ] -> print_found_note conf base case_sens s hit.sh_fnotes
        | hits -> print_search_hits conf ~full_word case_sens s terms hits)

let print_misc_notes_search conf base =
  match try Some (List.assoc "s" conf.env) with Not_found -> None with
  | Some s -> search_text conf base (Mutil.gen_decode false s)
  | None -> print_misc_notes conf base
