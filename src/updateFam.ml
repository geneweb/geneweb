(* camlp4r ./pa_html.cmo *)
(* $Id: updateFam.ml,v 2.17 1999-10-26 22:35:45 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value bogus_family_index = Adef.ifam_of_int (-1);

value person_key base ip =
  let p = poi base ip in
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip else p.occ
  in
  (first_name, surname, occ, Update.Link)
;

value string_family_of base fam cpl =
  let sfam = Gutil.map_family_ps (person_key base) (sou base) fam in
  let scpl = Gutil.map_couple_p (person_key base) cpl in
  (sfam, scpl)
;

value print_child_person conf base var (first_name, surname, occ, create) =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" "colspan=3" begin
        Wserver.wprint "<input name=%s_fn size=23 maxlength=200" var;
        Wserver.wprint " value=\"%s\">"
          (quote_escaped first_name);
      end;
      tag "td" "align=right" begin
        let s = capitale (transl conf "number") in
        Wserver.wprint "%s" s;
      end;
      tag "td" begin
        Wserver.wprint "<input name=%s_occ size=5 maxlength=8%s>" var
          (if occ == 0 then "" else " value=" ^ string_of_int occ);
      end;
    end;
    Wserver.wprint "\n";
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=5" begin
        Wserver.wprint
          "<input name=%s_sn size=40 maxlength=200 value=\"%s\">"
          var surname;
      end;
    end;
    Wserver.wprint "\n";
    tag "tr" begin
      tag "td" begin
        tag "select" "name=%s_p" var begin
          Wserver.wprint "<option value=link%s>%s\n"
            (if create = Update.Link then " selected" else "")
            (capitale (transl conf "link"));
          Wserver.wprint "<option value=create%s>%s\n"
            (if create <> Update.Link then " selected" else "")
            (capitale (transl conf "create"));
        end;
      end;
      tag "td" begin
        Wserver.wprint "%s" (capitale (transl conf "sex"));
      end;
      tag "td" begin
        Wserver.wprint "<input type=radio name=%s_sex value=N%s>?\n" var
          (match create with
           [ Update.Create Neuter _ -> " checked" | _ -> "" ]);
        Wserver.wprint "<input type=radio name=%s_sex value=M%s>%s\n" var
          (match create with [ Update.Create Male _ -> " checked" | _ -> "" ])
          (transl_nth conf "M/F" 0);
        Wserver.wprint "<input type=radio name=%s_sex value=F%s>%s\n" var
          (match create with
           [ Update.Create Female _ -> " checked" | _ -> "" ])
          (transl_nth conf "M/F" 1);
      end;
      tag "td" begin
        Wserver.wprint "%s" (capitale (transl conf "birth"));
      end;
      tag "td" "colspan=2" begin
        Wserver.wprint "<input name=%s_yyyy size=5 maxlength=5%s>\n" var
          (match create with
           [ Update.Create _ (Some (Dgreg {year = y} _)) ->
               " value=" ^ string_of_int y
           | _ -> "" ]);
        Wserver.wprint "<input name=%s_mm size=2 maxlength=2%s>\n" var
          (match create with
           [ Update.Create _ (Some (Dgreg {month = m} _)) when m <> 0 ->
               " value=" ^ string_of_int m
           | _ -> "" ]);
        Wserver.wprint "<input name=%s_dd size=2 maxlength=2%s>\n" var
          (match create with
           [ Update.Create _ (Some (Dgreg {day = d} _)) when d <> 0 ->
               " value=" ^ string_of_int d
           | _ -> "" ]);
      end;
    end;
  end
;

value print_father conf base cpl =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "him/her" 0));
     end;
     Wserver.wprint "\n";
     Update.print_parent_person conf base "his" cpl.father;
     Wserver.wprint "\n";
  return ()
;

value print_mother conf base cpl =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "him/her" 1));
     end;
     Wserver.wprint "\n";
     Update.print_parent_person conf base "her" cpl.mother;
     Wserver.wprint "\n";
  return ()
;

value print_insert_witness conf base cnt =
  tag "table" "border=1" begin
    tag "tr" begin
      let var = "ins_witn" ^ string_of_int cnt in
      tag "td" begin
        let s = transl_nth conf "witness/witnesses" 0 in
        Wserver.wprint "%s <input type=checkbox name=%s value=on>"
          (capitale (transl_decline conf "insert" s))
          var;
      end;
    end;
  end
;

value print_witness conf base var key =
  Update.print_parent_person conf base var key
;

value print_witnesses conf base fam =
  let witnesses =
    match Array.to_list fam.witnesses with
    [ [] -> let t = ("", "", 0, Update.Create Neuter None) in [t; t]
    | ipl -> ipl ]
  in
  do tag "h4" begin
       Wserver.wprint "%s"
         (capitale (transl_nth conf "witness/witnesses" 1));
     end;
     Wserver.wprint "\n";
     print_insert_witness conf base 0;
     let _ = List.fold_left
       (fun cnt n ->
          do print_witness conf base ("witn" ^ string_of_int cnt) n;
             print_insert_witness conf base cnt;
          return cnt + 1)
       1 witnesses
    in ();
  return ()
;

value print_marriage conf base fam =
  do tag "h4" begin
       Wserver.wprint "%s"
         (capitale (transl_nth conf "marriage/marriages" 0));
     end;
     Wserver.wprint "\n";
     Wserver.wprint "<input type=radio name=not_married value=false%s>"
       (match fam.not_married with [ False -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl_nth conf "married" 1));
     Wserver.wprint "<input type=radio name=not_married value=true%s>"
       (match fam.not_married with [ True -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl conf "not married"));
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s\n" (capitale (transl conf "place"));
         end;
         tag "td" begin
           Wserver.wprint
             "<input name=marriage_place size=40 maxlength=200%s>\n"
             (if fam.marriage_place = "" then ""
             else " value=\"" ^ fam.marriage_place ^ "\"");
         end;
       end;
     end;
     Update.print_date conf base (capitale (transl conf "date")) "marriage"
       (Adef.od_of_codate fam.marriage);
     Update.print_src conf "marr_src" fam.marriage_src;
  return ()
;

value print_divorce conf base fam =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "divorce" 0));
     end;
     Wserver.wprint "\n";
     Wserver.wprint
       "<input type=radio name=divorce value=not_divorced%s>"
       (match fam.divorce with [ NotDivorced -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl conf "not divorced"));
     Wserver.wprint "<input type=radio name=divorce value=divorced%s>"
       (match fam.divorce with [ Divorced _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl conf "divorced"));
     Update.print_date conf base (capitale (transl conf "date")) "divorce"
       (match fam.divorce with
        [ Divorced d -> Adef.od_of_codate d
        | _ -> None ]);
  return ()
;

value print_insert_child conf base cnt =
  tag "table" "border=1" begin
    tag "tr" begin
      let var = "ins_child" ^ string_of_int cnt in
      tag "td" begin
        let s = transl_nth conf "child/children" 0 in
        Wserver.wprint "%s <input type=checkbox name=%s value=on>"
          (capitale (transl_decline conf "insert" s))
          var;
      end;
    end;
  end
;

value print_child conf base cnt n =
  do Wserver.wprint "\n";
     html_li conf;
     print_child_person conf base ("child" ^ string_of_int cnt) n;
     html_li conf;
     print_insert_child conf base cnt;
  return ()
;

value print_children conf base fam cpl force_children_surnames =
  let children =
    match Array.to_list fam.children with
    [ [] -> [("", "", 0, Update.Create Neuter None)]
    | ipl ->
        let (_, father_surname, _, _) = cpl.father in
        List.map
          (fun (first_name, surname, occ, create) ->
             let surname =
               if not force_children_surnames && surname = father_surname then
                 ""
               else surname
             in
             (first_name, surname, occ, create))
          ipl ]
  in
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "child/children" 1));
     end;
     Wserver.wprint "\n";
     tag "ul" begin
       html_li conf;
       print_insert_child conf base 0;
       let _ = List.fold_left
          (fun cnt n -> do print_child conf base cnt n; return cnt + 1)
          1 children
       in ();
     end;
  return ()
;

value print_comment conf base fam =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "comment"));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=comment size=50 maxlength=200%s>\n"
             (match fam.comment with
              [ s when s <> "" -> " value=\"" ^ s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_source conf base field =
  do tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "source/sources" 0));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=src size=50 maxlength=200%s>\n"
             (match field with
              [ s when s <> "" ->
                  " value=\"" ^ quote_escaped s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_family conf base fam cpl force_children_surnames =
  do print_father conf base cpl;
     Wserver.wprint "\n";
     print_mother conf base cpl;
     Wserver.wprint "\n";
     print_marriage conf base fam;
     Wserver.wprint "\n";
     print_witnesses conf base fam;
     Wserver.wprint "\n";
     print_divorce conf base fam;
     Wserver.wprint "\n";
     print_comment conf base fam;
     Wserver.wprint "\n";
     print_children conf base fam cpl force_children_surnames;
     Wserver.wprint "\n";
     print_source conf base fam.fsources;
  return ()
;

value merge_call conf =
  do Wserver.wprint "<input type=hidden name=m value=MRG_MOD_FAM_OK>\n";
     match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
     [ (Some i1, Some i2) ->
         do Wserver.wprint "<input type=hidden name=ini1 value=%d>\n" i1;
            Wserver.wprint "<input type=hidden name=ini2 value=%d>\n" i2;
         return ()
     | _ -> () ];
     match p_getint conf.env "i2" with
     [ Some i2 -> Wserver.wprint "<input type=hidden name=i2 value=%d>\n" i2
     | _ -> () ];
  return ()
;

value print_mod1 conf base fam cpl digest =
  let title _ =
    match p_getenv conf.env "m" with
    [ Some "MRG_MOD_FAM_OK" ->
        let s = transl_nth conf "family/families" 1 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "merge" s))
          (Adef.int_of_ifam fam.fam_index)
    | _ ->
        let s = transl_nth conf "family/families" 0 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "modify" s))
          (Adef.int_of_ifam fam.fam_index) ]
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       match p_getenv conf.env "m" with
       [ Some "MRG_MOD_FAM_OK" -> merge_call conf
       | _ -> Wserver.wprint "<input type=hidden name=m value=MOD_FAM_OK>\n" ];
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_ifam fam.fam_index);
       match p_getenv conf.env "ip" with
       [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
       | None -> () ];
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       print_family conf base fam cpl False;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_del1 conf base fam =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_ifam fam.fam_index);
       match p_getenv conf.env "ip" with
       [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
       | None -> () ];
       Wserver.wprint "<input type=hidden name=m value=DEL_FAM_OK>\n";
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_swi1 conf base p fam1 fam2 =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "switch"))
  in
  let cpl1 = coi base fam1.fam_index in
  let cpl2 = coi base fam2.fam_index in
  do header conf title;
     Wserver.wprint "%s:"
       (capitale (transl conf "switch the order of the following families"));
     tag "ul" begin
       html_li conf;
       Update.print_someone conf base (poi base cpl1.father);
       Wserver.wprint " %s " (transl conf "and");
       Update.print_someone conf base (poi base cpl1.mother);
       html_li conf;
       Update.print_someone conf base (poi base cpl2.father);
       Wserver.wprint " %s " (transl conf "and");
       Update.print_someone conf base (poi base cpl2.mother);
     end;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=f value=%d>\n\n"
         (Adef.int_of_ifam fam2.fam_index);
       Wserver.wprint "<input type=hidden name=m value=SWI_FAM_OK>\n";
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add1 conf base fam cpl force_children_surnames =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "add" s))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       match p_getenv conf.env "i" with
       [ Some ip -> Wserver.wprint "<input type=hidden name=i value=%s>\n" ip
       | None -> () ];
       Wserver.wprint "<input type=hidden name=m value=ADD_FAM_OK>\n";
       print_family conf base fam cpl force_children_surnames;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add conf base =
  let (fath, moth) =
    match p_getint conf.env "i" with
    [ Some i ->
        let p = base.data.persons.get i in
        let fath =
          match p.sex with
          [ Male | Neuter -> person_key base p.cle_index
          | Female -> ("", "", 0, Update.Create Neuter None) ]
        in
        let moth =
          match p.sex with
          [ Female -> person_key base p.cle_index
          | Male | Neuter -> ("", "", 0, Update.Create Neuter None) ]
        in
        (fath, moth)
    | None ->
        (("", "", 0, Update.Create Neuter None),
         ("", "", 0, Update.Create Neuter None)) ]
  in
  let fam =
    {marriage = Adef.codate_None; marriage_place = "";
     marriage_src = ""; witnesses = [| |]; not_married = False;
     divorce = NotDivorced; children = [| |];
     comment = ""; origin_file = ""; fsources = "";
     fam_index = bogus_family_index}
  and cpl =
    {father = fath; mother = moth}
  in
  print_add1 conf base fam cpl False
;

value print_add_parents conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      let fam =
        {marriage = Adef.codate_None; marriage_place = "";
         marriage_src = ""; witnesses = [| |]; not_married = False;
         divorce = NotDivorced;
         children =
           [| (sou base p.first_name, sou base p.surname, p.occ,
               Update.Link) |];
         comment = ""; origin_file = ""; fsources = "";
         fam_index = bogus_family_index}
      and cpl =
        {father = ("", sou base p.surname, 0, Update.Create Neuter None);
         mother = ("", "", 0, Update.Create Neuter None)}
      in
      print_add1 conf base fam cpl True
  | _ -> incorrect_request conf ]
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      let cpl = coi base (Adef.ifam_of_int i) in
      let (sfam, scpl) = string_family_of base fam cpl in
      print_mod1 conf base sfam scpl (Update.digest_family fam)
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      print_del1 conf base fam
  | _ -> incorrect_request conf ]
;

value rec find_families ifam =
  fun
  [ [ifam1; ifam2 :: ifaml] ->
      if ifam2 = ifam then Some (ifam1, ifam2)
      else find_families ifam [ifam2 :: ifaml]
  | _ -> None ]
;

value print_swi conf base =
  match (p_getint conf.env "i", p_getint conf.env "f") with
  [ (Some ip, Some ifam)  ->
      let p = base.data.persons.get ip in
      match find_families (Adef.ifam_of_int ifam) (Array.to_list p.family) with
      [ Some (ifam1, ifam2) ->
          print_swi1 conf base p (foi base ifam1) (foi base ifam2)
      | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
