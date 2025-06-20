(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open MergeInd
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let print_differences conf base branches p1 p2 =
  let gen_string_field ?(check = true) chk1 chk2 (title : Adef.safe_string)
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
        if chk && check then Output.print_sstring conf " checked";
        Output.print_sstring conf {|>|};
        Output.printf conf {|<label class="custom-control-label" for="|};
        Output.print_sstring conf name;
        Output.print_sstring conf (string_of_int i);
        Output.print_sstring conf {|">|};
        Output.print_string conf x;
        Output.print_sstring conf {|</label></div>|}
      in
      Output.print_sstring conf "<h4>";
      Output.print_string conf (Adef.safe_fn Utf8.capitalize_fst title);
      if not check then
        Output.print_sstring conf
          (Format.sprintf
             {|<sup title="%s">
            <i class="fa fa-info fa-xs"></i></sup>|}
             (transl conf "help merge" |> Utf8.capitalize_fst));
      Output.print_sstring conf {|</h4>|};
      aux 1 x1 chk1;
      aux 2 x2 chk2)
  in
  let string_field ?(check = true) = gen_string_field ~check true false in
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "MRG_IND_OK");
  Util.hidden_input conf "i1"
    (Driver.get_iper p1 |> Driver.Iper.to_string |> Mutil.encode);
  Util.hidden_input conf "i2"
    (Driver.get_iper p2 |> Driver.Iper.to_string |> Mutil.encode);
  let rec loop = function
    | [ (ip1, ip2) ] ->
        Util.hidden_input conf "ini1"
          (ip1 |> Driver.Iper.to_string |> Mutil.encode);
        Util.hidden_input conf "ini2"
          (ip2 |> Driver.Iper.to_string |> Mutil.encode)
    | _ :: branches -> loop branches
    | [] -> ()
  in
  loop branches;
  (match (p_getenv conf.env "m", p_getint conf.env "ip") with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      Util.hidden_input conf "ip" (ip |> string_of_int |> Adef.encoded);
      List.iter
        (fun excl ->
          match p_getenv conf.env excl with
          | None | Some "" -> ()
          | Some s -> Util.hidden_input conf excl (Mutil.encode s))
        [ "iexcl"; "fexcl" ]
  | _ -> ());
  Output.print_sstring conf "</p><p>";
  string_field
    (transl_nth conf "first name/first names" 0 |> Adef.safe)
    ("first_name" |> Adef.encoded)
    (fun p -> (Driver.p_first_name base p |> escape_html :> Adef.safe_string));
  string_field
    (transl_nth conf "surname/surnames" 0 |> Adef.safe)
    ("surname" |> Adef.encoded)
    (fun p -> (Driver.p_surname base p |> escape_html :> Adef.safe_string));
  let select_smallest_num =
    Driver.p_first_name base p1 = Driver.p_first_name base p2
  in
  gen_string_field
    (Driver.get_occ p1 < Driver.get_occ p2 || not select_smallest_num)
    (Driver.get_occ p1 > Driver.get_occ p2 && select_smallest_num)
    (transl conf "number" |> Adef.safe)
    ("number" |> Adef.encoded)
    (fun p -> Driver.get_occ p |> string_of_int |> Adef.safe);
  string_field
    (transl_nth conf "image/images" 0 |> Adef.safe)
    ("image" |> Adef.encoded)
    (fun p ->
      match Image.get_portrait conf base p with
      | Some (`Url url) ->
          ({|<img src="|} ^<^ escape_html url
           ^>^ {|" style="max-width:75px;max-height:100px">|} ^ url
            :> Adef.safe_string)
      | Some (`Path path) ->
          let k = Image.default_image_filename "portraits" base p in
          let s = Unix.stat path in
          Printf.sprintf
            {|<img src="%sm=IM&d=%s&%s&k=%s" \
              style="max-width:75px;max-height:100px"> %s|}
            (commd conf :> string)
            (string_of_int
            @@ int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
            (acces conf base p : Adef.escaped_string :> string)
            k path
          |> Adef.safe
      | None -> Adef.safe "");
  string_field
    (transl_nth conf "blason/blasons" 0 |> Adef.safe)
    ("blason" |> Adef.encoded)
    (fun p ->
      match Image.get_blason conf base p true with
      | Some (`Url url) ->
          ({|<img src="|} ^<^ escape_html url
           ^>^ {|" style="max-width:75px;max-height:100px">|} ^ url
            :> Adef.safe_string)
      | Some (`Path path) ->
          let s = Unix.stat path in
          Printf.sprintf
            {|<img src="%sm=FIM&d=%s&p=%s&n=%s&oc=%d&k=%s" \
              style="max-width:75px;max-height:100px"> %s|}
            (commd conf :> string)
            (string_of_int
            @@ int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
            (Driver.get_first_name p |> Driver.sou base)
            (Driver.get_surname p |> Driver.sou base)
            (Driver.get_occ p)
            ((acces conf base p : Adef.escaped_string :> string) ^ ".blason")
            path
          |> Adef.safe
      | None -> Adef.safe "");
  string_field
    (transl conf "public name" |> Adef.safe)
    ("public_name" |> Adef.encoded)
    (fun p ->
      (Driver.get_public_name p |> Driver.sou base |> escape_html
        :> Adef.safe_string));
  string_field
    (transl_nth conf "occupation/occupations" 0 |> Adef.safe)
    ("occupation" |> Adef.encoded)
    (fun p ->
      (Driver.get_occupation p |> Driver.sou base |> escape_html
        :> Adef.safe_string));
  string_field
    (transl conf "sex" |> Adef.safe)
    ("sex" |> Adef.encoded)
    (fun p ->
      Adef.safe
      @@
      match Driver.get_sex p with Male -> "M" | Female -> "F" | Neuter -> "");
  let date_field trans name get =
    string_field
      (transl conf trans |> Adef.safe)
      name
      (fun p ->
        match Date.od_of_cdate (get p) with
        | None -> Adef.safe ""
        | Some d -> DateDisplay.string_of_ondate conf d)
  in
  let place_field trans name get =
    string_field
      (transl conf trans ^<^ Adef.safe " / "
      ^>^ transl_nth conf "place/places" 0)
      name
      (fun p -> get p |> Driver.sou base |> safe_html)
  in
  let note_field trans name get =
    string_field ~check:false
      (transl conf trans ^<^ Adef.safe " / " ^>^ transl_nth conf "note/notes" 0)
      name
      (fun p -> get p |> Driver.sou base |> safe_html)
  in
  let source_field trans name get =
    string_field ~check:false
      (transl conf trans ^<^ Adef.safe " / "
      ^>^ transl_nth conf "source/sources" 0)
      name
      (fun p -> get p |> Driver.sou base |> safe_html)
  in

  date_field "birth" (Adef.encoded "birth") Driver.get_birth;
  place_field "birth" (Adef.encoded "birth_place") Driver.get_birth_place;
  note_field "birth" (Adef.encoded "birth_note") Driver.get_birth_note;
  source_field "birth" (Adef.encoded "birth_source") Driver.get_birth_src;
  date_field "baptism" (Adef.encoded "baptism") Driver.get_baptism;
  place_field "baptism" (Adef.encoded "baptism_place") Driver.get_baptism_place;
  note_field "baptism" (Adef.encoded "baptism_note") Driver.get_baptism_note;
  source_field "baptism" (Adef.encoded "baptism_source") Driver.get_baptism_src;
  string_field
    (transl conf "death" |> Adef.safe)
    (Adef.encoded "death")
    (fun p ->
      let is = 2 in
      match Driver.get_death p with
      | NotDead -> transl_nth conf "alive" is |> Adef.safe
      | Death (dr, cd) ->
          let s =
            match dr with
            | Killed -> transl_nth conf "killed (in action)" is
            | Murdered -> transl_nth conf "murdered" is
            | Executed -> transl_nth conf "executed (legally killed)" is
            | Disappeared -> transl_nth conf "disappeared" is
            | Unspecified -> transl_nth conf "died" is
          in
          s ^<^ " "
          ^<^ DateDisplay.string_of_ondate conf (Date.date_of_cdate cd)
      | DeadYoung -> transl_nth conf "died young" is |> Adef.safe
      | DeadDontKnowWhen -> transl_nth conf "died" is |> Adef.safe
      | DontKnowIfDead | OfCourseDead -> Adef.safe "");
  place_field "death" (Adef.encoded "death_place") Driver.get_death_place;
  note_field "death" (Adef.encoded "death_note") Driver.get_death_note;
  source_field "death" (Adef.encoded "death_source") Driver.get_death_src;
  string_field
    (transl conf "burial" |> Adef.safe)
    (Adef.encoded "burial")
    (fun p ->
      let is = 2 in
      (* TODO burial_to_string *)
      match Driver.get_burial p with
      | UnknownBurial -> Adef.safe ""
      | Buried cod -> (
          transl_nth conf "buried" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Adef.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d)
      | Cremated cod -> (
          transl_nth conf "cremated" is
          ^<^
          match Date.od_of_cdate cod with
          | None -> Adef.safe ""
          | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d));
  place_field "burial" (Adef.encoded "burial_place") Driver.get_burial_place;
  note_field "burial" (Adef.encoded "burial_note") Driver.get_burial_note;
  source_field "burial" (Adef.encoded "burial_source") Driver.get_burial_src;
  string_field ~check:false
    (transl_nth conf "note/notes" 1 |> Adef.safe)
    ("notes" |> Adef.encoded)
    (fun p ->
      (Driver.get_notes p |> Driver.sou base |> escape_html :> Adef.safe_string));
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf {|</button></form>|}

let propose_merge_ind conf base branches p1 p2 =
  let title _ =
    let s = transl_nth conf "person/persons" 1 in
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  if branches <> [] then (
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "you must first merge"));
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf "<ul><li><a href=\"";
    Output.print_string conf (commd conf);
    Output.print_string conf (acces conf base p1);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p1;
    Output.print_sstring conf "</a> ";
    Output.print_sstring conf (transl_nth conf "and" 0);
    Output.print_sstring conf " <a href=\"";
    Output.print_string conf (commd conf);
    Output.print_string conf (acces conf base p2);
    Output.print_sstring conf "\">";
    MergeDisplay.print_someone conf base p2;
    Output.print_sstring conf "</a></li></ul><p></p>");
  print_differences conf base branches p1 p2;
  if branches <> [] then (
    Output.print_sstring conf "<p><hr></p><p>";
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "branch/branches" 1));
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf "</p><table>";
    List.iter
      (fun (ip1, ip2) ->
        let p1 = Driver.poi base ip1 in
        let p2 = Driver.poi base ip2 in
        Output.print_sstring conf "<tr align=\"";
        Output.print_sstring conf conf.left;
        Output.print_sstring conf "\"><td>";
        Output.print_string conf (referenced_person_text conf base p1);
        Output.print_string conf (DateDisplay.short_dates_text conf base p1);
        Output.print_sstring conf "</td><td>";
        Output.print_string conf (referenced_person_text conf base p2);
        Output.print_string conf (DateDisplay.short_dates_text conf base p2);
        Output.print_sstring conf "</td></tr>")
      ((Driver.get_iper p1, Driver.get_iper p2) :: branches);
    Output.print_sstring conf "</table>");
  Hutil.trailer conf

let error_loop conf base p =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.print_sstring conf "<strong>";
  Output.print_string conf (Driver.p_first_name base p |> escape_html);
  if Driver.get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (Driver.get_occ p |> string_of_int));
  Output.print_sstring conf " ";
  Output.print_string conf (Driver.p_surname base p |> escape_html);
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "would be his/her own ancestor");
  Output.print_sstring conf "";
  Hutil.trailer conf

let propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title _ =
    transl_nth conf "family/families" 1
    |> transl_decline conf "merge"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  transl conf "you must first merge the 2 families"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf "<ul><li><a href=\"";
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p1);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p1;
  Output.print_sstring conf "</a> ";
  Output.print_sstring conf (transl conf "with");
  Output.print_sstring conf " <a href=\"";
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p2);
  Output.print_sstring conf "\">";
  MergeDisplay.print_someone conf base p2;
  Output.print_sstring conf "</a></li></ul><p>";
  MergeFamDisplay.print_differences conf base branches fam1 fam2;
  Hutil.trailer conf

let not_found_or_incorrect conf =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "not found"));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "several answers");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "or");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "incorrect request");
  Hutil.trailer conf

let same_person conf =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl conf "it is the same person!"));
  Hutil.trailer conf

let different_sexes conf base p1 p2 =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
  in
  Hutil.rheader conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl conf "incompatible sexes"));
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf {|<ul><li><a href="|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p1);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p1);
  Output.print_sstring conf {|</a></li><li>|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p2);
  Output.print_sstring conf {|">|};
  Output.print_string conf (Util.designation base p2);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let print_merged conf base wl p =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "merge done"))
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  (match (p_getenv conf.env "m", p_getenv conf.env "ip") with
  | Some "MRG_DUP_IND_Y_N", Some ip ->
      let ip = Driver.Iper.of_string ip in
      let s1 =
        match p_getenv conf.env "iexcl" with
        | Some "" | None -> Adef.encoded ""
        | Some s -> "&iexcl=" ^<^ Mutil.encode s
      in
      let s2 =
        match p_getenv conf.env "fexcl" with
        | Some "" | None -> Adef.encoded ""
        | Some s -> "&fexcl=" ^<^ Mutil.encode s
      in
      Output.print_sstring conf "<p>";
      Output.print_sstring conf "<a href=";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=MRG_DUP&ip=";
      Output.print_string conf (Driver.Iper.to_string ip |> Mutil.encode);
      Output.print_string conf s1;
      Output.print_string conf s2;
      Output.print_sstring conf ">";
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "continue merging"));
      Output.print_sstring conf "</a>";
      (let p = Driver.poi base ip in
       let s = gen_person_text conf base p in
       Output.print_sstring conf "\n(";
       Output.print_sstring conf
         (Util.transl_a_of_b conf
            (transl conf "possible duplications")
            (reference conf base p s :> string)
            (s :> string));
       Output.print_sstring conf ")\n");
      Output.print_sstring conf "</p>\n"
  | _ -> ());
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print conf base =
  let p1 =
    match p_getenv conf.env "i" with
    | Some i1 -> Some (Driver.poi base (Driver.Iper.of_string i1))
    | None -> None
  in
  let p2 =
    match p_getenv conf.env "i2" with
    | Some i2 -> Some (Driver.poi base (Driver.Iper.of_string i2))
    | None -> (
        match (p_getenv conf.env "select", p_getenv conf.env "n") with
        | (Some "input" | None), Some n -> (
            let ipl = Gutil.person_ht_find_all base n in
            match ipl with [ ip2 ] -> Some (Driver.poi base ip2) | _ -> None)
        | Some x, (Some "" | None) ->
            Some (Driver.poi base (Driver.Iper.of_string x))
        | _ -> None)
  in
  match (p1, p2) with
  | Some p1, Some p2 -> (
      try
        let ok, warnings =
          merge conf base p1 p2 propose_merge_ind propose_merge_fam
        in
        if ok then print_merged conf base warnings p1
      with
      | Error_loop p -> error_loop conf base p
      | Different_sexes (p1, p2) -> different_sexes conf base p1 p2
      | Same_person -> same_person conf)
  | _ -> not_found_or_incorrect conf

(* Undocumented feature... Kill someone's ancestors *)

let print_killed conf base p nb_ind nb_fam =
  let title _ = Output.print_sstring conf "Ancestors killed" in
  Hutil.header conf title;
  Output.print_string conf (referenced_person_title_text conf base p);
  Output.print_sstring conf "'s ancestors killed.<br>";
  Output.print_sstring conf (string_of_int nb_ind);
  Output.print_sstring conf " persons and ";
  Output.print_sstring conf (string_of_int nb_fam);
  Output.print_sstring conf " families deleted<p>";
  Hutil.trailer conf

let print_kill_ancestors conf base =
  match List.assoc_opt "can_kill_ancestors" conf.base_env with
  | Some "yes" -> (
      match find_person_in_env conf base "" with
      | Some p ->
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          kill_ancestors conf base false p nb_ind nb_fam;
          Util.commit_patches conf base;
          let changed =
            U_Kill_ancestors
              (Util.string_gen_person base (Driver.gen_person_of_person p))
          in
          History.record conf base changed "ka";
          print_killed conf base p !nb_ind !nb_fam
      | None -> Hutil.incorrect_request conf)
  | _ -> Hutil.incorrect_request conf
