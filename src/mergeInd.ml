(* camlp4r ./pa_html.cmo *)
(* $Id: mergeInd.ml,v 2.1 1999-03-08 11:18:53 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value f_aoc conf s =
  if conf.charset = "iso-8859-1" then Ansel.of_iso_8859_1 s
  else s
;

value print_differences conf base branches p1 p2 =
  let string_field str_orig title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then
      do Wserver.wprint "<h4>%s</h4>\n" (capitale title);
         tag "ul" begin
           html_li conf;
           Wserver.wprint "<input type=radio name=\"%s\" value=1 checked>\n"
             name;
           Wserver.wprint "%s\n" (if str_orig then coa conf x1 else x1);
           html_li conf;
           Wserver.wprint "<input type=radio name=\"%s\" value=2>\n" name;
           Wserver.wprint "%s\n" (if str_orig then coa conf x2 else x2);
         end;
      return ()
    else ()
  in
  tag "form" "method=POST action=\"%s\"" conf.command begin
    Srcfile.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=MRG_IND_OK>\n";
    Wserver.wprint "<input type=hidden name=i1 value=%d>\n"
      (Adef.int_of_iper p1.cle_index);
    Wserver.wprint "<input type=hidden name=i2 value=%d>\n"
      (Adef.int_of_iper p2.cle_index);
    loop branches where rec loop =
      fun
      [ [(ip1, ip2)] ->
          do Wserver.wprint "<input type=hidden name=ini1 value=%d>\n"
               (Adef.int_of_iper ip1);
             Wserver.wprint "<input type=hidden name=ini2 value=%d>\n"
               (Adef.int_of_iper ip2);
          return ()
      | [_ :: branches] -> loop branches
      | _ -> () ];
    html_p conf;
    string_field True (transl_nth conf "first name/first names" 0) "first_name"
      (fun p -> sou base p.first_name);
    string_field True (transl_nth conf "surname/surnames" 0) "surname"
      (fun p -> sou base p.surname);
    string_field False (transl conf "number") "number"
      (fun p -> string_of_int p.occ);
    string_field True (transl conf "photo") "photo"
      (fun p -> sou base p.photo);
    string_field True (transl conf "public name") "public_name"
      (fun p -> sou base p.public_name);
    string_field True (transl conf "occupation") "occupation"
      (fun p -> sou base p.occupation);
    string_field False (transl conf "sex") "sex"
      (fun p ->
         match p.sex with
         [ Masculine -> "M"
         | Feminine -> "F"
         | Neuter -> "" ]);
    string_field False (transl conf "access") "access"
      (fun p ->
         match p.access with
         [ IfTitles -> "IfTitles"
         | Private -> "Private"
         | Public -> "Public" ]);
    string_field False (transl conf "birth") "birth"
      (fun p ->
         match Adef.od_of_codate p.birth with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "birth" ^ " / " ^ transl conf "place")
      "birth_place" (fun p -> sou base p.birth_place);
    string_field False (transl conf "baptism") "baptism"
      (fun p ->
         match Adef.od_of_codate p.baptism with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "baptism" ^ " / " ^ transl conf "place")
      "baptism_place" (fun p -> sou base p.baptism_place);
    string_field False (transl conf "death") "death"
      (fun p ->
         let is = 2 in
         match p.death with
         [ NotDead -> transl_nth conf "not dead" is
         | Death dr cd ->
             let s =
               match dr with
               [ Killed -> transl_nth conf "killed (in action)" is
               | Murdered -> transl_nth conf "murdered" is
               | Executed -> transl_nth conf "executed (legally killed)" is
               | Disappeared -> transl_nth conf "disappeared" is
               | Unspecified -> transl_nth conf "died" is ]
             in
             s ^ " " ^ Date.string_of_ondate conf (Adef.date_of_cdate cd)
         | DeadYoung -> transl_nth conf "dead young" is
         | DeadDontKnowWhen -> transl_nth conf "died" is
         | DontKnowIfDead -> "" ]);
    string_field True (transl conf "death" ^ " / " ^ transl conf "place")
      "death_place" (fun p -> sou base p.death_place);
    string_field False (transl conf "burial") "burial"
      (fun p ->
         let is = 2 in
         match p.burial with
         [ UnknownBurial -> ""
         | Buried cod ->
             transl_nth conf "buried" is ^
             (match Adef.od_of_codate cod with
              [ None -> ""
              | Some d -> " " ^ Date.string_of_ondate conf d ])
         | Cremated cod ->
             transl_nth conf "cremated" is ^
             (match Adef.od_of_codate cod with
              [ None -> ""
              | Some d -> " " ^ Date.string_of_ondate conf d ]) ]);
    string_field True (transl conf "burial" ^ " / " ^ transl conf "place")
      "burial_place" (fun p -> sou base p.burial_place);
    html_p conf;
    Wserver.wprint "<input type=submit value=Ok>\n";
  end
;

value merge_ind conf base branches p1 p2 =
  let title h =
    let s = transl_nth conf "person/persons" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do header conf title;
     if branches <> [] then
       do Wserver.wprint "%s:\n"
            (capitale (transl conf "you must first merge"));
          tag "ul" begin
            html_li conf;
            stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
              Merge.print_someone conf base p1;
            end;
            Wserver.wprint "\n%s\n" (transl conf "and");
            stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
              Merge.print_someone conf base p2;
            end;
            Wserver.wprint "\n";
          end;
          html_p conf;
       return ()
     else ();
     print_differences conf base branches p1 p2;
     if branches <> [] then
       do html_p conf;
          Wserver.wprint "<hr>";
          html_p conf;
          Wserver.wprint "%s:\n"
            (capitale (transl_nth conf "branch/branches" 1));
          html_p conf;
          tag "table" begin
            List.iter
              (fun (ip1, ip2) ->
                 let p1 = poi base ip1 in
                 let p2 = poi base ip2 in
                 do tag "tr" begin
                      tag "td" begin
                        afficher_personne_referencee conf base p1;
                        Date.afficher_dates_courtes conf base p1;
                      end;
                      tag "td" begin
                        afficher_personne_referencee conf base p2;
                        Date.afficher_dates_courtes conf base p2;
                      end;
                    end;
                 return ())
              branches;
          end;
       return ()
     else ();
     trailer conf;
  return ()
;

value merge_fam_first conf base branches fam1 fam2 p1 p2 =
  let title h =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do header conf title;
     Wserver.wprint "%s:\n"
       (capitale (transl conf "you must first merge the 2 families"));
     tag "ul" begin
       html_li conf;
       stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
         Merge.print_someone conf base p1;
       end;
       Wserver.wprint "\n%s\n" (transl conf "with");
       stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
         Merge.print_someone conf base p2;
       end;
       Wserver.wprint "\n";
     end;
     html_p conf;
     MergeFam.print_differences conf base branches fam1 fam2;
     trailer conf;
  return ()
;

value not_found_or_incorrect conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s %s %s %s %s\n"
       (capitale (transl conf "not found"))
       (transl conf "or")
       (transl conf "several answers")
       (transl conf "or")
       (transl conf "incorrect request");
     trailer conf;
  return ()
;

value same_person conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s\n" (capitale (transl conf "it is the same person!"));
     trailer conf;
  return ()
;

value different_sexes conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s.\n" (capitale (transl conf "incompatible sexes"));
     trailer conf;
  return ()
;

value rec propose_ancestors_merge conf base branches ip1 ip2 =
  let a1 = aoi base ip1 in
  let a2 = aoi base ip2 in
  let branches = [(ip1, ip2) :: branches] in
  match (a1.parents, a2.parents) with
  [ (Some ifam1, Some ifam2) when ifam1 <> ifam2 ->
      let cpl1 = coi base ifam1 in
      let cpl2 = coi base ifam2 in
      if cpl1.father <> cpl2.father then
        propose_ancestors_merge conf base branches cpl1.father cpl2.father
      else if cpl1.mother <> cpl2.mother then
        propose_ancestors_merge conf base branches cpl1.mother cpl2.mother
      else
        merge_fam_first conf base branches (foi base ifam1) (foi base ifam2)
          (poi base cpl1.father) (poi base cpl1.mother)
  | _ ->
      merge_ind conf base branches (poi base ip1) (poi base ip2) ]
;

value print conf base =
  let p1 =
    match p_getint conf.env "i" with
    [ Some i1 -> Some (base.data.persons.get i1)
    | None -> None  ]
  in
  let p2 =
    match (p_getenv conf.env "n", p_getint conf.env "i2") with
    [ (Some n, _) ->
        let n = f_aoc conf n in
        let ipl = Gutil.person_ht_find_all base n in
        match ipl with
        [ [ip2] -> Some (poi base ip2)
        | _ -> None ]
    | (_, Some i2) -> Some (base.data.persons.get i2)
    | _ -> None ]
  in
  match (p1, p2) with
  [ (Some p1, Some p2) ->
      if p1.cle_index = p2.cle_index then same_person conf
      else if p1.sex <> p2.sex && p1.sex <> Neuter && p2.sex <> Neuter
      then different_sexes conf
      else
        let a1 = aoi base p1.cle_index in
        let a2 = aoi base p2.cle_index in
        if a1.parents <> None && a2.parents <> None
        && a1.parents <> a2.parents then
          propose_ancestors_merge conf base [] p1.cle_index p2.cle_index
        else merge_ind conf base [] p1 p2
  | _ -> not_found_or_incorrect conf ]
;
