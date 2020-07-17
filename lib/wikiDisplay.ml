open Config
open Wiki

let print_sub_part_links conf edit_mode sfn cnt0 is_empty =
  Wserver.printf "<p>\n";
  if cnt0 >= first_cnt then
    begin
      Wserver.printf
        "<a href=\"%sm=%s%s&v=%d\">" (Util.commd conf) edit_mode sfn (cnt0 - 1);
      Wserver.printf "&lt;&lt;" ;
      Wserver.printf "</a>\n"
    end;
  Wserver.printf "<a href=\"%sm=%s%s\">" (Util.commd conf) edit_mode sfn;
  Wserver.printf "^^";
  Wserver.printf "</a>\n";
  if not is_empty then
    begin
      Wserver.printf
        "<a href=\"%sm=%s%s&v=%d\">" (Util.commd conf) edit_mode sfn (cnt0 + 1);
      Wserver.printf "&gt;&gt;" ;
      Wserver.printf "</a>\n"
    end;
  Wserver.printf "</p>\n"

(* FIXME: unused *)
let print_sub_part_text conf base _edit_opt _cnt0 doc =
  Wiki.replace_toc ("..." ^ Util.transl_nth conf "visualize/show/hide/summary" 3 ^ "...") doc
  |> Wiki.to_wktxt conf base
  |> Wikitext.output_document Wserver.print_string

let print_sub_part conf base can_edit edit_mode sub_fname cnt0 doc : unit =
  let edit_opt = Some (can_edit, edit_mode, sub_fname) in
  let sfn = if sub_fname = "" then "" else "&f=" ^ sub_fname in
  print_sub_part_links conf edit_mode sfn cnt0 (doc = Wiki.empty);
  print_sub_part_text conf base edit_opt cnt0 doc

(* FIXME: unused *)
let print_mod_view_page conf _base can_edit mode fname title env s =
  let s =
    List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env ^ s
  in
  let mode_pref = if can_edit then "MOD_" else "VIEW_" in
  let (has_v, v) =
    match Util.p_getint conf.env "v" with
      Some v -> true, v
    | None -> false, 0
  in
  let sub_part = if not has_v then s else extract_sub_part s v in
  let is_empty = sub_part = "" in
  let sfn = if fname = "" then "" else "&f=" ^ Util.code_varenv fname in
  Hutil.header conf title;
  if can_edit then
    begin
      Wserver.printf "<div style=\"font-size:80%%;float:%s;margin-%s:3em\">\n"
        conf.right conf.left;
      Wserver.printf "(";
      begin
        Wserver.printf "<a href=\"%sm=%s%s%s\">" (Util.commd conf) mode
          (if has_v then "&v=" ^ string_of_int v else "") sfn;
        Wserver.printf "%s" (Util.transl_nth conf "visualize/show/hide/summary" 0);
        Wserver.printf "</a>"
      end;
      Wserver.printf ")\n";
      Wserver.printf "</div>\n"
    end;
  Hutil.print_link_to_welcome conf true;
  if can_edit && has_v then
    print_sub_part_links conf (mode_pref ^ mode) sfn v is_empty;
  Wserver.printf "<form name=\"form_notes\" method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  if can_edit then
    Wserver.printf
      "<input type=\"hidden\" name=\"m\" value=\"MOD_%s_OK\"%s>\n" mode
      conf.xhs;
  if has_v then
    Wserver.printf "<input type=\"hidden\" name=\"v\" value=\"%d\"%s>\n" v
      conf.xhs;
  if fname <> "" then
    Wserver.printf "<input type=\"hidden\" name=\"f\" value=\"%s\"%s>\n" fname
      conf.xhs;
  print_endline fname ;
  if can_edit
  then
    Wserver.printf
      {|<input type="hidden" name="digest" value="%s">"|}
      (Iovalue.digest s) ;
  Wserver.printf "<div class=\"row ml-3\">\n";
  begin match Util.open_etc_file "toolbar" with
    Some ic ->
      Wserver.printf "<div class=\"d-inline col-9 py-1\">\n";
      Templ.copy_from_templ conf ["name", "notes"] ic;
      Wserver.printf "</div>\n";
  | None -> ()
  end;
  Wserver.printf "<textarea name=\"notes\" id=\"notes_comments\"";
  Wserver.printf " class=\"col-9 form-control\" rows=\"25\" cols=\"110\"%s>"
    (if can_edit then "" else " readonly=\"readonly\"");
  Wserver.print_string (Util.string_with_macros conf [] sub_part) ;
  Wserver.printf "</textarea>";
  if can_edit then
    begin
      begin
        Wserver.printf
          "<button type=\"submit\" class=\"btn btn-outline-primary btn-lg";
        Wserver.printf " col-4 py-3 mt-2 mb-3 mx-auto order-3\">";
        Wserver.printf "%s" (Utf8.capitalize (Util.transl_nth conf "validate/delete" 0));
        Wserver.printf "</button>\n"
      end
    end;
  begin match Util.open_etc_file "accent" with
    Some ic ->
      Wserver.printf "<div class=\"col my-1 mr-2 text-monospace\">\n";
      Templ.copy_from_templ conf ["name", "notes"] ic;
      Wserver.printf "</div>\n";
  | None -> ()
  end;
  Wserver.printf "</div>\n";
  Wserver.printf "</form>\n";
  Hutil.trailer conf

let section_level s len =
  let rec loop i j k =
    if i < 6 && len > k && s.[i] = '=' && s.[j] = '='
    then loop (i + 1) (j - 1) (k + 2)
    else i
  in
  loop 1 (len - 2) 4

let insert_sub_part s v sub_part =
  let lines = String.split_on_char '\n' s in
  let (lines, sl) =
    let rec loop sub_part_added lines lev cnt =
      function
        s :: sl ->
          let len = String.length s in
          if len > 2 && s.[0] = '=' && s.[len-1] = '=' then
            if v = first_cnt - 1 then
              (if sub_part = "" then [] else [""; sub_part]), s :: sl
            else
              let nlev = section_level s len in
              if cnt = v then
                let lines =
                  if sub_part = "" then lines else "" :: sub_part :: lines
                in
                loop true lines nlev (cnt + 1) sl
              else if cnt > v then
                if nlev > lev then loop sub_part_added lines lev (cnt + 1) sl
                else lines, s :: sl
              else loop sub_part_added (s :: lines) lev (cnt + 1) sl
          else if cnt <= v then loop sub_part_added (s :: lines) lev cnt sl
          else loop sub_part_added lines lev cnt sl
      | [] ->
          let lines =
            if sub_part_added then lines
            else if sub_part = "" then lines
            else "" :: sub_part :: lines
          in
          lines, []
    in
    loop false [] 0 first_cnt lines
  in
  String.concat "\n" (List.rev_append lines sl)

let print_ok conf base edit_mode fname title_is_1st s =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (Util.transl conf "notes modified"))
  in
  Hutil.header_no_page_title conf title;
  Wserver.printf "<div style=\"text-align:center\">\n";
  Wserver.printf "--- ";
  title ();
  Wserver.printf " ---\n";
  Wserver.printf "</div>\n";
  Hutil.print_link_to_welcome conf true;
  let get_v = Util.p_getint conf.env "v" in
  let v =
    match get_v with
      Some v -> v
    | None -> 0
  in
  let (title, s) =
    if v = 0 && title_is_1st then
      let (env, s) = split_title_and_text s in
      (try List.assoc "TITLE" env with Not_found -> ""), s
    else "", s
  in
  let doc = Wikitext.doc_from_string s |> Wiki.of_wktxt in
  let doc = if v = 0 && title <> "" then Wiki.add_h1 title doc else doc in
  print_sub_part conf base conf.wizard edit_mode fname v doc;
  Hutil.trailer conf

let print_mod_ok conf base edit_mode fname read_string commit string_filter
    title_is_1st =
  let fname = fname (Util.p_getenv conf.env "f") in
  match edit_mode fname with
    Some edit_mode ->
    let old_string =
      let (e, s) = read_string fname in
      List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" e ^ s
    in
    let sub_part =
      match Util.p_getenv conf.env "notes" with
        Some v -> Mutil.strip_all_trailing_spaces v
      | None -> failwith "notes unbound"
    in
    let digest =
      match Util.p_getenv conf.env "digest" with
        Some s -> s
      | None -> ""
    in
    if digest <> Iovalue.digest old_string then Update.error_digest conf
    else
      let s =
        match Util.p_getint conf.env "v" with
          Some v -> insert_sub_part old_string v sub_part
        | None -> sub_part
      in
      if s <> old_string then commit fname s;
      let sub_part = string_filter sub_part in
      print_ok conf base edit_mode fname title_is_1st sub_part
  | None -> Hutil.incorrect_request conf
