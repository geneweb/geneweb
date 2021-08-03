(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open MergeInd

let print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then
      begin
        Output.printf conf "<h4>%s</h4>\n" (Utf8.capitalize_fst title);
        Output.print_string conf "<div class=\"custom-control custom-radio ml-3\">\n";
        Output.printf conf "  <input class=\"custom-control-input\" type=\"radio\" id=\"%s1\" name=\"%s\" value=\"1\"%s>\n" name name chk1;
        Output.printf conf "  <label class=\"custom-control-label\" for=\"%s1\">%s</label>\n" name x1;
        Output.print_string conf "</div>\n";
        Output.print_string conf "<div class=\"custom-control custom-radio ml-3 mb-2\">\n";
        Output.printf conf "  <input class=\"custom-control-input\" type=\"radio\" id=\"%s2\" name=\"%s\" value=\"2\"%s>\n" name name chk2;
        Output.printf conf "  <label class=\"custom-control-label\" for=\"%s2\">%s</label>\n" name x2;
        Output.print_string conf "</div>\n";
      end
  in
  let string_field = gen_string_field " checked" "" in
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Output.print_string conf "<p>\n";
  Util.hidden_env conf;
  Output.print_string conf "<input type=\"hidden\" name=\"m\" value=\"MRG_IND_OK\">\n";
  Output.printf conf "<input type=\"hidden\" name=\"i1\" value=\"%s\">\n"
    (string_of_iper (get_iper p1));
  Output.printf conf "<input type=\"hidden\" name=\"i2\" value=\"%s\">\n"
    (string_of_iper (get_iper p2));
  begin let rec loop =
    function
      [ip1, ip2] ->
        Output.printf conf
          "<input type=\"hidden\" name=\"ini1\" value=\"%s\">\n"
          (string_of_iper ip1);
        Output.printf conf
          "<input type=\"hidden\" name=\"ini2\" value=\"%s\">\n"
          (string_of_iper ip2)
    | _ :: branches -> loop branches
    | _ -> ()
  in
    loop branches
  end;
  begin match p_getenv conf.env "m", p_getint conf.env "ip" with
    Some "MRG_DUP_IND_Y_N", Some ip ->
      Output.printf conf "<input type=\"hidden\" name=\"ip\" value=\"%d\">\n" ip;
      List.iter
        (fun excl_name ->
           match p_getenv conf.env excl_name with
             Some "" | None -> ()
           | Some s ->
               Output.printf conf
                 "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n"
                 excl_name s)
        ["iexcl"; "fexcl"]
  | _ -> ()
  end;
  Output.print_string conf "</p><p>";
  string_field (transl_nth conf "first name/first names" 0) "first_name"
    (fun p -> p_first_name base p);
  string_field (transl_nth conf "surname/surnames" 0) "surname"
    (fun p -> p_surname base p);
  begin let select_smallest_num =
    p_first_name base p1 = p_first_name base p2
  in
    gen_string_field
      (if get_occ p1 < get_occ p2 || not select_smallest_num then " checked"
       else "")
      (if get_occ p1 > get_occ p2 && select_smallest_num then " checked"
       else "")
      (transl conf "number") "number"
      (fun p -> string_of_int (get_occ p))
  end;
  string_field (transl_nth conf "image/images" 0) "image"
    (fun p ->
       let v = image_and_size conf base p (limited_image_size 75 100) in
       match v with
         Some (false, link, _) ->
           "<img src=\"" ^ link ^
           "\" style=\"max-width:75px; max-height:100px\" />"
       | _ -> sou base (get_image p));
  string_field (transl conf "public name") "public_name"
    (fun p -> sou base (get_public_name p));
  string_field (transl_nth conf "occupation/occupations" 0) "occupation"
    (fun p -> sou base (get_occupation p));
  string_field (transl conf "sex") "sex"
    (fun p ->
       match get_sex p with
         Male -> "M"
       | Female -> "F"
       | Neuter -> "");
  string_field (transl conf "birth") "birth"
    (fun p ->
       match Adef.od_of_cdate (get_birth p) with
         None -> ""
       | Some d -> DateDisplay.string_of_ondate conf d);
  string_field (transl conf "birth" ^ " / " ^ transl conf "place")
    "birth_place" (fun p -> sou base (get_birth_place p));
  string_field (transl conf "baptism") "baptism"
    (fun p ->
       match Adef.od_of_cdate (get_baptism p) with
         None -> ""
       | Some d -> DateDisplay.string_of_ondate conf d);
  string_field (transl conf "baptism" ^ " / " ^ transl conf "place")
    "baptism_place" (fun p -> sou base (get_baptism_place p));
  string_field (transl conf "death") "death"
    (fun p ->
       let is = 2 in
       match get_death p with
         NotDead -> transl_nth conf "alive" is
       | Death (dr, cd) ->
           let s =
             match dr with
               Killed -> transl_nth conf "killed (in action)" is
             | Murdered -> transl_nth conf "murdered" is
             | Executed -> transl_nth conf "executed (legally killed)" is
             | Disappeared -> transl_nth conf "disappeared" is
             | Unspecified -> transl_nth conf "died" is
           in
           s ^ " " ^ DateDisplay.string_of_ondate conf (Adef.date_of_cdate cd)
       | DeadYoung -> transl_nth conf "died young" is
       | DeadDontKnowWhen -> transl_nth conf "died" is
       | DontKnowIfDead | OfCourseDead -> "");
  string_field (transl conf "death" ^ " / " ^ transl conf "place")
    "death_place" (fun p -> sou base (get_death_place p));
  string_field (transl conf "burial") "burial"
    (fun p ->
       let is = 2 in
       match get_burial p with
         UnknownBurial -> ""
       | Buried cod ->
           transl_nth conf "buried" is ^
           (match Adef.od_of_cdate cod with
              None -> ""
            | Some d -> " " ^ DateDisplay.string_of_ondate conf d)
       | Cremated cod ->
           transl_nth conf "cremated" is ^
           (match Adef.od_of_cdate cod with
              None -> ""
            | Some d -> " " ^ DateDisplay.string_of_ondate conf d));
  string_field (transl conf "burial" ^ " / " ^ transl conf "place")
    "burial_place" (fun p -> sou base (get_burial_place p));
  Output.printf conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">%s</button></form>|}
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))


let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = transl_nth conf "person/persons" 1 in
    Output.print_string conf (Utf8.capitalize_fst (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then begin
    Output.printf conf
      "%s%s<ul><li><a href=\"%s%s\">"
      (Utf8.capitalize_fst (transl conf "you must first merge"))
      (transl conf ":")
      (commd conf)
      (acces conf base p1);
    MergeDisplay.print_someone conf base p1;
    Output.printf conf "</a> %s <a href=\"%s%s\">"
      (transl_nth conf "and" 0)
      (commd conf)
      (acces conf base p2);
    MergeDisplay.print_someone conf base p2;
    Output.print_string conf "</a></li></ul><p></p>\n"
  end;
  print_differences conf base branches p1 p2;
  if branches <> [] then
    begin
      Output.printf conf
        "<p><hr></p><p>%s%s</p><table>"
        (Utf8.capitalize_fst (transl_nth conf "branch/branches" 1)) (transl conf ":") ;
      List.iter
        (fun (ip1, ip2) ->
           let p1 = poi base ip1 in
           let p2 = poi base ip2 in
           Output.printf conf
             "<tr align=\"%s\"><td>%s%s</td><td>%s%s</td></tr>"
             conf.left
             (referenced_person_text conf base p1)
             (DateDisplay.short_dates_text conf base p1)
             (referenced_person_text conf base p2)
             (DateDisplay.short_dates_text conf base p2))
        ((get_iper p1, get_iper p2) :: branches);
      Output.print_string conf "</table>\n"
    end ;
  Hutil.trailer conf

let error_loop conf base p =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.printf conf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p);
  Output.printf conf "\n%s\n" (transl conf "would be his/her own ancestor");
  Output.print_string conf "\n";
  Hutil.trailer conf

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Output.print_string conf (Utf8.capitalize_fst (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  Output.printf conf "%s%s\n"
    (Utf8.capitalize_fst (transl conf "you must first merge the 2 families"))
    (transl conf ":");
  Output.printf conf "<ul><li><a href=\"%s%s\">"
    (commd conf) (acces conf base p1);
  MergeDisplay.print_someone conf base p1;
  Output.printf conf "</a> %s <a href=\"%s%s\">"
    (transl conf "with") (commd conf) (acces conf base p2);
  MergeDisplay.print_someone conf base p2;
  Output.print_string conf "</a></li></ul><p>";
  MergeFamDisplay.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let not_found_or_incorrect conf =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Output.printf conf "%s %s %s %s %s\n" (Utf8.capitalize_fst (transl conf "not found"))
    (transl conf "or") (transl conf "several answers") (transl conf "or")
    (transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Output.printf conf "%s\n" (Utf8.capitalize_fst (transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf base p1 p2 =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Output.print_string conf (Utf8.capitalize_fst (transl conf "incompatible sexes"));
  Output.print_string conf (transl conf ":");
  Output.print_string conf "<ul><li>";
  Output.printf conf
    {|<a href="%s%s">%s</a>|}
    (commd conf) (acces conf base p1) (Gutil.designation base p1) ;
  Output.print_string conf "</li><li>";
  Output.printf conf
    {|<a href="%s%s">%s</a>|}
    (commd conf) (acces conf base p2) (Gutil.designation base p2) ;
  Output.print_string conf "</li></ul>";
  Hutil.trailer conf

let print_merged conf base wl p =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "merge done")) in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_string conf "<ul>\n";
  Output.print_string conf "<li>\n";
  Output.printf conf "%s\n" (referenced_person_text conf base p);
  Output.print_string conf "</li>\n";
  Output.print_string conf "</ul>\n";
  begin match p_getenv conf.env "m", p_getenv conf.env "ip" with
    Some "MRG_DUP_IND_Y_N", Some ip ->
      let ip = iper_of_string ip in
      let s1 =
        match p_getenv conf.env "iexcl" with
          Some "" | None -> ""
        | Some s -> "&iexcl=" ^ s
      in
      let s2 =
        match p_getenv conf.env "fexcl" with
          Some "" | None -> ""
        | Some s -> "&fexcl=" ^ s
      in
      Output.print_string conf "<p>\n";
      Output.printf conf "<a href=%sm=MRG_DUP&ip=%s%s%s>" (commd conf) (string_of_iper ip) s1 s2;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "continue merging"));
      Output.print_string conf "</a>";
      begin
        let p =  poi base ip in
        let s = person_text conf base p in
        Output.printf conf "\n(%s)\n"
          (Util.transl_a_of_b conf
             (transl conf "possible duplications")
             (reference conf base p s) s)
      end ;
      Output.print_string conf "</p>\n"
  | _ -> ()
  end;
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print conf base =
  let p1 =
    match p_getenv conf.env "i" with
      Some i1 -> Some (poi base (iper_of_string i1))
    | None -> None
  in
  let p2 =
    match p_getenv conf.env "i2" with
      Some i2 -> Some (poi base (iper_of_string i2))
    | None ->
      match p_getenv conf.env "select", p_getenv conf.env "n" with
        (Some "input" | None), Some n ->
        let ipl = Gutil.person_ht_find_all base n in
        begin match ipl with
            [ip2] -> Some (poi base ip2)
          | _ -> None
        end
      | Some x, (Some "" | None) ->
        Some (poi base (iper_of_string x))
      | _ -> None
  in
  match p1, p2 with
  | Some p1, Some p2 ->
    begin
      try
        let (ok, warnings) = merge conf base p1 p2 propose_merge_ind propose_merge_fam in
        if ok then print_merged conf base warnings p1
      with Error_loop p -> error_loop conf base p
         | Different_sexes (p1, p2) -> different_sexes conf base p1 p2
         | Same_person -> same_person conf
    end
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

let print_killed conf base p nb_ind nb_fam =
  let title _ = Output.print_string conf "Ancestors killed" in
  Hutil.header conf title;
  Output.printf conf "%s's ancestors killed.<br>\n"
    (referenced_person_title_text conf base p);
  Output.printf conf "%d persons and %d families deleted<p>\n" nb_ind nb_fam;
  Hutil.trailer conf

let print_kill_ancestors conf base =
  match p_getenv conf.base_env "can_kill_ancestors" with
    Some "yes" ->
      begin match find_person_in_env conf base "" with
        Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          kill_ancestors conf base false p nb_ind nb_fam;
          Util.commit_patches conf base;
          let changed =
            U_Kill_ancestors
              (Util.string_gen_person base (gen_person_of_person p))
          in
          History.record conf base changed "ka";
          print_killed conf base p !nb_ind !nb_fam
      | None -> Hutil.incorrect_request conf
      end
  | _ -> Hutil.incorrect_request conf
