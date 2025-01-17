(* Copyright (c) 1998-2007 INRIA *)

let print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 (title : Adef.safe_string)
      (name : Adef.encoded_string) proj =
    let name = (name :> string) in
    let x1 : Adef.safe_string = proj p1 in
    let x2 : Adef.safe_string = proj p2 in
    if
      (x1 :> string) <> ""
      && (x1 :> string) <> "?"
      && (x2 :> string) <> ""
      && (x2 :> string) <> "?"
      && x1 <> x2
    then (
      let aux i x chk =
        Output.print_sstring conf
          {|<div class="custom-control custom-radio ml-3">|};
        Output.print_sstring conf
          {|<input class="custom-control-input" type="radio" id="|};
        Output.print_sstring conf name;
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|" name="|};
        Output.print_sstring conf name;
        Output.print_sstring conf {|" value="|};
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|"|};
        if chk then Output.print_sstring conf " checked";
        Output.print_sstring conf {|>|};
        Output.printf conf {|<label class="custom-control-label" for="|};
        Output.print_sstring conf name;
        Output.print_sstring conf {|">|};
        Output.print_string conf x;
        Output.print_sstring conf {|</label></div>|}
      in
      Output.print_sstring conf "<h4>";
      Output.print_string conf (Adef.safe_fn Utf8.capitalize_fst title);
      Output.print_sstring conf {|</h4>|};
      aux 1 x1 chk1;
      aux 2 x2 chk2)
  in
  let string_field = gen_string_field true false in
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.Config.command;
  Output.print_sstring conf {|">|};
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_IND_OK");
  Util.hidden_input conf "i1"
    (Gwdb.get_iper p1 |> Gwdb.string_of_iper |> Mutil.encode);
  Util.hidden_input conf "i2"
    (Gwdb.get_iper p2 |> Gwdb.string_of_iper |> Mutil.encode);
  let rec loop = function
    | [ (ip1, ip2) ] ->
        Util.hidden_input conf "ini1"
          (ip1 |> Gwdb.string_of_iper |> Mutil.encode);
        Util.hidden_input conf "ini2"
          (ip2 |> Gwdb.string_of_iper |> Mutil.encode)
    | _ :: branches -> loop branches
    | [] -> ()
  in
  loop branches;
  (match
     (Util.p_getenv conf.Config.env "m", Util.p_getint conf.Config.env "ip")
   with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      Util.hidden_input conf "ip" (ip |> string_of_int |> Adef.encoded);
      List.iter
        (fun excl ->
          match Util.p_getenv conf.Config.env excl with
          | None | Some "" -> ()
          | Some s -> Util.hidden_input conf excl (Mutil.encode s))
        [ "iexcl"; "fexcl" ]
  | _ -> ());
  Output.print_sstring conf "</p><p>";
  string_field
    (Util.transl_nth conf "first name/first names" 0 |> Adef.safe)
    ("first_name" |> Adef.encoded)
    (fun p ->
      (Gwdb.p_first_name base p |> Util.escape_html :> Adef.safe_string));
  string_field
    (Util.transl_nth conf "surname/surnames" 0 |> Adef.safe)
    ("surname" |> Adef.encoded)
    (fun p -> (Gwdb.p_surname base p |> Util.escape_html :> Adef.safe_string));
  let select_smallest_num =
    Gwdb.p_first_name base p1 = Gwdb.p_first_name base p2
  in
  gen_string_field
    (Gwdb.get_occ p1 < Gwdb.get_occ p2 || not select_smallest_num)
    (Gwdb.get_occ p1 > Gwdb.get_occ p2 && select_smallest_num)
    (Util.transl conf "number" |> Adef.safe)
    ("number" |> Adef.encoded)
    (fun p -> Gwdb.get_occ p |> string_of_int |> Adef.safe);
  string_field
    (Util.transl_nth conf "image/images" 0 |> Adef.safe)
    ("image" |> Adef.encoded)
    (fun p ->
      match Image.get_portrait conf base p with
      | Some (`Url url) ->
          let open Def in
          ({|<img src="|} ^<^ Util.escape_html url
           ^>^ {|" style="max-width:75px;max-height:100px">|}
            :> Adef.safe_string)
      | Some (`Path path) ->
          (* TODO: ?? *)
          (Util.escape_html path :> Adef.safe_string)
      | None -> Adef.safe "");
  string_field
    (Util.transl conf "public name" |> Adef.safe)
    ("public_name" |> Adef.encoded)
    (fun p ->
      (Gwdb.get_public_name p |> Gwdb.sou base |> Util.escape_html
        :> Adef.safe_string));
  string_field
    (Util.transl_nth conf "occupation/occupations" 0 |> Adef.safe)
    ("occupation" |> Adef.encoded)
    (fun p ->
      (Gwdb.get_occupation p |> Gwdb.sou base |> Util.escape_html
        :> Adef.safe_string));
  string_field
    (Util.transl conf "sex" |> Adef.safe)
    ("sex" |> Adef.encoded)
    (fun p ->
      Adef.safe
      @@ match Gwdb.get_sex p with Male -> "M" | Female -> "F" | Neuter -> "");
  let date_field trans name get =
    string_field
      (Util.transl conf trans |> Adef.safe)
      name
      (fun p ->
        match Date.od_of_cdate (get p) with
        | None -> Adef.safe ""
        | Some d -> DateDisplay.string_of_ondate conf d)
  in
  let place_field trans name get =
    let open Def in
    string_field
      (Util.transl conf trans ^<^ Adef.safe " / " ^>^ Util.transl conf "place")
      name
      (fun p -> get p |> Gwdb.sou base |> Util.safe_html)
  in
  date_field "birth" (Adef.encoded "birth") Gwdb.get_birth;
  place_field "birth" (Adef.encoded "birth_place") Gwdb.get_birth_place;
  date_field "baptism" (Adef.encoded "baptism") Gwdb.get_baptism;
  place_field "baptism" (Adef.encoded "baptism_place") Gwdb.get_baptism_place;
  string_field
    (Util.transl conf "death" |> Adef.safe)
    (Adef.encoded "death")
    (fun p ->
      let is = 2 in
      match Gwdb.get_death p with
      | NotDead -> Util.transl_nth conf "alive" is |> Adef.safe
      | Death (dr, cd) ->
          let s =
            match dr with
            | Killed -> Util.transl_nth conf "killed (in action)" is
            | Murdered -> Util.transl_nth conf "murdered" is
            | Executed -> Util.transl_nth conf "executed (legally killed)" is
            | Disappeared -> Util.transl_nth conf "disappeared" is
            | Unspecified -> Util.transl_nth conf "died" is
          in
          let open Def in
          s ^<^ " "
          ^<^ DateDisplay.string_of_ondate conf (Date.date_of_cdate cd)
      | DeadYoung -> Util.transl_nth conf "died young" is |> Adef.safe
      | DeadDontKnowWhen -> Util.transl_nth conf "died" is |> Adef.safe
      | DontKnowIfDead | OfCourseDead -> Adef.safe "");
  place_field "death" (Adef.encoded "death_place") Gwdb.get_death_place;
  string_field
    (Util.transl conf "burial" |> Adef.safe)
    (Adef.encoded "burial")
    (fun p ->
      let is = 2 in
      (* TODO burial_to_string *)
      match Gwdb.get_burial p with
      | UnknownBurial -> Adef.safe ""
      | Buried cod -> (
          let open Def in
          Util.transl_nth conf "buried" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Adef.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d)
      | Cremated cod -> (
          let open Def in
          Util.transl_nth conf "cremated" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Adef.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d));
  place_field "burial" (Adef.encoded "burial_place") Gwdb.get_burial_place;
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl_nth conf "validate/delete" 0));
  Output.print_sstring conf {|</button></form>|}

let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = Util.transl_nth conf "person/persons" 1 in
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then (
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "you must first merge"));
    Output.print_sstring conf (Util.transl conf ":");
    Output.print_sstring conf "<ul><li><a href=\"";
    Output.print_string conf (Util.commd conf);
    Output.print_string conf (Util.acces conf base p1);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p1;
    Output.print_sstring conf "</a> ";
    Output.print_sstring conf (Util.transl_nth conf "and" 0);
    Output.print_sstring conf " <a href=\"";
    Output.print_string conf (Util.commd conf);
    Output.print_string conf (Util.acces conf base p2);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p2;
    Output.print_sstring conf "</a></li></ul><p></p>");
  print_differences conf base branches p1 p2;
  if branches <> [] then (
    Output.print_sstring conf "<p><hr></p><p>";
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl_nth conf "branch/branches" 1));
    Output.print_sstring conf (Util.transl conf ":");
    Output.print_sstring conf "</p><table>";
    List.iter
      (fun (ip1, ip2) ->
        let p1 = Gwdb.poi base ip1 in
        let p2 = Gwdb.poi base ip2 in
        Output.print_sstring conf "<tr align=\"";
        Output.print_sstring conf conf.Config.left;
        Output.print_sstring conf "\"><td>";
        Output.print_string conf
          (NameDisplay.referenced_person_text conf base p1);
        Output.print_string conf (DateDisplay.short_dates_text conf base p1);
        Output.print_sstring conf "</td><td>";
        Output.print_string conf
          (NameDisplay.referenced_person_text conf base p2);
        Output.print_string conf (DateDisplay.short_dates_text conf base p2);
        Output.print_sstring conf "</td></tr>")
      ((Gwdb.get_iper p1, Gwdb.get_iper p2) :: branches);
    Output.print_sstring conf "</table>");
  Hutil.trailer conf

let error_loop conf base p =
  let title _ =
    Util.transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<strong>";
  Output.print_string conf (Gwdb.p_first_name base p |> Util.escape_html);
  if Gwdb.get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (Gwdb.get_occ p |> string_of_int));
  Output.print_sstring conf " ";
  Output.print_string conf (Gwdb.p_surname base p |> Util.escape_html);
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "would be his/her own ancestor");
  Output.print_sstring conf "";
  Hutil.trailer conf

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    Util.transl_nth conf "family/families" 1
    |> Util.transl_decline conf "merge"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Util.transl conf "you must first merge the 2 families"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf "<ul><li><a href=\"";
  Output.print_string conf (Util.commd conf);
  Output.print_string conf (Util.acces conf base p1);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p1;
  Output.print_sstring conf "</a> ";
  Output.print_sstring conf (Util.transl conf "with");
  Output.print_sstring conf " <a href=\"";
  Output.print_string conf (Util.commd conf);
  Output.print_string conf (Util.acces conf base p2);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p2;
  Output.print_sstring conf "</a></li></ul><p>";
  MergeFamDisplay.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let not_found_or_incorrect conf =
  let title _ =
    Util.transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "not found"));
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "several answers");
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf base p1 p2 =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl conf "incompatible sexes"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf {|<ul><li><a href="|};
  Output.print_string conf (Util.commd conf);
  Output.print_string conf (Util.acces conf base p1);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p1);
  Output.print_sstring conf {|</a></li><li>|};
  Output.print_string conf (Util.commd conf);
  Output.print_string conf (Util.acces conf base p2);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p2);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let print_merged conf base wl p =
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "merge done"))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (NameDisplay.referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  (match
     (Util.p_getenv conf.Config.env "m", Util.p_getenv conf.Config.env "ip")
   with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      let ip = Gwdb.iper_of_string ip in
      let s1 =
        match Util.p_getenv conf.Config.env "iexcl" with
        | Some "" | None -> Adef.encoded ""
        | Some s ->
            let open Def in
            "&iexcl=" ^<^ Mutil.encode s
      in
      let s2 =
        match Util.p_getenv conf.Config.env "fexcl" with
        | Some "" | None -> Adef.encoded ""
        | Some s ->
            let open Def in
            "&fexcl=" ^<^ Mutil.encode s
      in
      Output.print_sstring conf "<p>";
      Output.print_sstring conf "<a href=";
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf "m=MRG_DUP&ip=";
      Output.print_string conf (Gwdb.string_of_iper ip |> Mutil.encode);
      Output.print_string conf s1;
      Output.print_string conf s2;
      Output.print_sstring conf ">";
      Output.print_sstring conf
        (Utf8.capitalize_fst (Util.transl conf "continue merging"));
      Output.print_sstring conf "</a>";
      (let p = Gwdb.poi base ip in
       let s = NameDisplay.fullname_html_of_person conf base p in
       Output.print_sstring conf "\n(";
       Output.print_sstring conf
         (Util.transl_a_of_b conf
            (Util.transl conf "possible duplications")
            (NameDisplay.reference conf base p s :> string)
            (s :> string));
       Output.print_sstring conf ")\n");
      Output.print_sstring conf "</p>\n"
  | _ -> ());
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print conf base =
  let p1 =
    match Util.p_getenv conf.Config.env "i" with
    | Some i1 -> Some (Gwdb.poi base (Gwdb.iper_of_string i1))
    | None -> None
  in
  let p2 =
    match Util.p_getenv conf.Config.env "i2" with
    | Some i2 -> Some (Gwdb.poi base (Gwdb.iper_of_string i2))
    | None -> (
        match
          ( Util.p_getenv conf.Config.env "select",
            Util.p_getenv conf.Config.env "n" )
        with
        | (Some "input" | None), Some n -> (
            let ipl = Gutil.person_ht_find_all base n in
            match ipl with [ ip2 ] -> Some (Gwdb.poi base ip2) | _ -> None)
        | Some x, (Some "" | None) ->
            Some (Gwdb.poi base (Gwdb.iper_of_string x))
        | _ -> None)
  in
  match (p1, p2) with
  | Some p1, Some p2 -> (
      try
        let ok, warnings =
          MergeInd.merge conf base p1 p2 propose_merge_ind propose_merge_fam
        in
        if ok then print_merged conf base warnings p1
      with
      | MergeInd.Error_loop p -> error_loop conf base p
      | MergeInd.Different_sexes (p1, p2) -> different_sexes conf base p1 p2
      | MergeInd.Same_person -> same_person conf)
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

let print_killed conf base p nb_ind nb_fam =
  let title _ = Output.print_sstring conf "Ancestors killed" in
  Hutil.header conf title;
  Output.print_string conf
    (NameDisplay.referenced_person_title_text conf base p);
  Output.print_sstring conf "'s ancestors killed.<br>";
  Output.print_sstring conf (string_of_int nb_ind);
  Output.print_sstring conf " persons and ";
  Output.print_sstring conf (string_of_int nb_fam);
  Output.print_sstring conf " families deleted<p>";
  Hutil.trailer conf

let print_kill_ancestors conf base =
  match List.assoc_opt "can_kill_ancestors" conf.Config.base_env with
  | Some "yes" -> (
      match Util.find_person_in_env conf base "" with
      | Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          MergeInd.kill_ancestors conf base false p nb_ind nb_fam;
          Util.commit_patches conf base;
          let changed =
            Def.U_Kill_ancestors
              (Util.string_gen_person base (Gwdb.gen_person_of_person p))
          in
          History.record conf base changed "ka";
          print_killed conf base p !nb_ind !nb_fam
      | None -> Hutil.incorrect_request conf)
  | _ -> Hutil.incorrect_request conf
