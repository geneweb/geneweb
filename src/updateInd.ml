(* camlp4r ./pa_html.cmo *)
(* $Id: updateInd.ml,v 1.1 1998-09-01 14:32:09 ddr Exp $ *)

open Config;
open Def;
open Util;
open Gutil;

value bogus_person_index = Adef.iper_of_int (-1);

value string_title_of base t =
  {t_name =
     match t.t_name with
     [ Tmain -> Tmain
     | Tname s -> Tname (sou base s)
     | Tnone -> Tnone ];
   t_title = sou base t.t_title; t_place = sou base t.t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end;
   t_nth = t.t_nth}
;

value string_person_of base p =
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper p.cle_index
    else p.occ
  in
  Gutil.map_person_strings (sou base) p
;

value print_first_name conf base p =
  let occ =
    if p.first_name = "?" || p.surname = "?" then Adef.int_of_iper p.cle_index
    else p.occ
  in
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s"
        (capitale (transl_nth conf "first name/first names" 0));
    end;
    tag "td" begin
      Wserver.wprint
        "<input name=\"first_name\" size=30 maxlength=200 value=\"%s\">"
        p.first_name;
    end;
    tag "td" begin
      let s = capitale (transl conf "number") in
      let s = if String.length s > 3 then String.sub s 0 3 else s in
      Wserver.wprint "%s" s;
    end;
    tag "td" begin
      Wserver.wprint "<input name=occ size=5 maxlength=8";
      if occ <> 0 then Wserver.wprint " value=%d" occ else ();
      Wserver.wprint ">";
    end;
  end
;

value print_surname conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s"
        (capitale (transl_nth conf "surname/surnames" 0));
    end;
    tag "td" begin
      Wserver.wprint "<input name=surname size=40 maxlength=200 value=\"%s\">"
        p.surname;
    end;
    tag "td" begin Wserver.wprint "%s" (capitale (transl conf "sex")); end;
    tag "td" begin
      Wserver.wprint "<input type=radio name=sex value=M%s>%s\n"
        (if p.sexe = Masculin then " checked" else "")
        (transl_nth conf "M/F" 0);
      Wserver.wprint "<input type=radio name=sex value=F%s>%s\n"
        (if p.sexe = Feminin then " checked" else "")
        (transl_nth conf "M/F" 1);
      Wserver.wprint "<input type=radio name=sex value=N%s>?\n"
        (if p.sexe = Neutre then " checked" else "");
    end;
  end
;

value print_public_name conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s" (capitale (transl conf "public name"));
    end;
    tag "td" "colspan=3" begin
      Wserver.wprint "<input name=public_name size=40";
      if p.public_name <> "" then Wserver.wprint " value=\"%s\"" p.public_name
      else ();
      Wserver.wprint ">";
    end;
  end
;

value print_photo conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s" (capitale (transl conf "photo"));
    end;
    tag "td" "colspan=3" begin
      Wserver.wprint "<input name=photo size=50";
      if p.photo <> "" then Wserver.wprint " value=\"%s\"" p.photo
      else ();
      Wserver.wprint ">";
    end;
  end
;

type item =
  { i_name : string; i_txt_name : string; i_txt_add : string }
;

value gen_print_ext_item conf base item i_cnt i_val =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s" (capitale item.i_txt_name);
    end;
    tag "td" begin
      Wserver.wprint "<input name=\"%s%d\" size=30" item.i_name i_cnt;
      if i_val <> "" then Wserver.wprint " value=\"%s\"" i_val else ();
      Wserver.wprint ">";
    end;
    tag "td" begin Wserver.wprint "%s" (capitale item.i_txt_add); end;
    tag "td" begin
      Wserver.wprint "<input type=checkbox name=\"add_%s%d\">"
        item.i_name i_cnt;
    end;
  end
;

value gen_print_ext_items conf base item i_proj =
  let il =
    match i_proj with
    [ [] -> [""]
    | il -> il ]
  in
  let _ = List.fold_left
    (fun i_cnt i_val ->
       do gen_print_ext_item conf base item i_cnt i_val; return
       i_cnt + 1)
    0 il
  in ()
;

value cons_update verb name = capitale verb;

value print_nick_names conf base p =
  gen_print_ext_items conf base
    {i_name = "nickname"; i_txt_name = transl conf "qualifier";
     i_txt_add = cons_update (transl conf "insert") (transl conf "qualifier")}
    p.nick_names
;

value print_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "alias"; i_txt_name = transl conf "alias";
     i_txt_add = cons_update (transl conf "insert") (transl conf "alias")}
    p.aliases
;

value print_first_names_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "first_name_alias"; i_txt_name = transl conf "first name alias";
     i_txt_add =
       cons_update (transl conf "insert") (transl conf "first name alias")}
    p.first_names_aliases
;

value print_surnames_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "surname_alias"; i_txt_name = transl conf "surname alias";
     i_txt_add =
       cons_update (transl conf "insert") (transl conf "surname alias")}
    p.surnames_aliases
;

value print_birth_place conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s...\n" (capitale (transl_nth conf "born" 2));
    end;
    tag "td" begin
      Wserver.wprint "%s\n" (capitale (transl conf "place"));
    end;
    tag "td" begin
      Wserver.wprint "<input name=birth_place size=40 maxlength=200%s>\n"
        (if p.birth_place = "" then ""
         else " value=\"" ^ p.birth_place ^ "\"");
    end;
  end
;

value print_bapt_place conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s...\n" (capitale (transl_nth conf "baptized" 2));
    end;
    tag "td" begin
      Wserver.wprint "%s\n" (capitale (transl conf "place"));
    end;
    tag "td" begin
      Wserver.wprint "<input name=bapt_place size=40 maxlength=200%s>\n"
        (if p.baptism_place = "" then ""
         else " value=\"" ^ p.baptism_place ^ "\"");
    end;
  end
;

value print_birth_date conf base p =
  let d = Adef.od_of_codate p.birth in
  Update.print_date conf base (capitale (transl conf "date")) "birth" d
;

value print_bapt_date conf base p =
  let d = Adef.od_of_codate p.baptism in
  Update.print_date conf base (capitale (transl conf "date")) "bapt" d
;

value print_death_type conf base p =
  tag "select" "name=death" begin
    Wserver.wprint "<option value=NotDead%s>"
      (match p.death with [ NotDead -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "not dead" 2));
    Wserver.wprint "<option value=DontKnowIfDead%s>"
      (match p.death with [ DontKnowIfDead -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "dont know"));
    Wserver.wprint "<option value=Death%s>"
      (match p.death with
       [ Death _ _ | DeadDontKnowWhen -> " selected"
       | _ -> "" ]);
    Wserver.wprint "%s...\n" (capitale (transl_nth conf "died" 2));
    Wserver.wprint "<option value=DeadYoung%s>"
      (match p.death with [ DeadYoung -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "dead young" 2));
  end
;

value print_death_place conf base p =
  do tag "td" begin
       Wserver.wprint "%s\n" (capitale (transl conf "place"));
     end;
     tag "td" begin
       Wserver.wprint "<input name=death_place size=40 maxlength=200%s>\n"
         (if p.death_place = "" then ""
          else " value=\"" ^ p.death_place ^ "\"");
     end;
  return ()
;

value print_death_date conf base p =
  let d =
    match p.death with
    [ Death _ d -> Some (Adef.date_of_cdate d)
    | _ -> None ]
  in
  Update.print_date conf base (capitale (transl conf "date")) "death" d
;

value print_death_reason conf base p =
  do Wserver.wprint "<input type=radio name=death_reason value=Killed%s>"
       (match p.death with [ Death Killed _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl_nth conf "killed (in action)" 2));
     Wserver.wprint "<input type=radio name=death_reason value=Murdered%s>"
       (match p.death with [ Death Murdered _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl_nth conf "murdered" 2));
     Wserver.wprint "<input type=radio name=death_reason value=Executed%s>"
       (match p.death with [ Death Executed _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n"
       (capitale (transl_nth conf "executed (legally killed)" 2));
     Wserver.wprint "<input type=radio name=death_reason value=Disappeared%s>"
       (match p.death with [ Death Disappeared _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl_nth conf "disappeared" 2));
     Wserver.wprint "<input type=radio name=death_reason value=Unspecified%s>"
       (match p.death with [ Death Unspecified _ -> " checked" | _ -> "" ]);
     Wserver.wprint "%s\n" (capitale (transl_nth conf "unspecified" 2));
  return ()
;

value print_burial_type conf base p =
  tag "select" "name=burial" begin
    Wserver.wprint "<option value=UnknownBurial%s>"
      (match p.burial with [ UnknownBurial -> " selected" | _ -> "" ]);
    Wserver.wprint "-\n";
    Wserver.wprint "<option value=Buried%s>"
      (match p.burial with [ Buried _ -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "buried" 2));
    Wserver.wprint "<option value=Cremated%s>"
      (match p.burial with [ Cremated _ -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "cremated" 2));
  end
;

value print_burial_place conf base p =
  do tag "td" begin
       Wserver.wprint "%s\n" (capitale (transl conf "place"));
     end;
     tag "td" begin
       Wserver.wprint "<input name=burial_place size=40 maxlength=200%s>\n"
         (if p.burial_place = "" then ""
          else " value=\"" ^ p.burial_place ^ "\"");
     end;
  return ()
;

value print_burial_date conf base p =
  let d =
    match p.burial with
    [ Buried d -> Adef.od_of_codate d
    | Cremated d -> Adef.od_of_codate d
    | _ -> None ]
  in
  Update.print_date conf base (capitale (transl conf "date")) "burial" d
;

value print_add_title conf base cnt =
  do tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s / %s <input type=checkbox name=add_title%d>"
             (capitale (transl conf "insert"))
             (capitale (transl_nth conf "title/titles" 0)) cnt;
         end;
       end;
     end;
     Wserver.wprint "\n<p>\n";
  return ()
;

value print_title conf base t cnt =
  do tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s\n"
             (capitale (transl_nth conf "title/titles" 0));
         end;
         tag "td" begin
           Wserver.wprint "<input name=t_title%d size=15%s>" cnt
             (match t with
              [ Some {t_title = n} -> " value=\"" ^ n ^ "\""
              | _ -> "" ]);
         end;
         tag "td" begin
           Wserver.wprint "%s\n" (capitale (transl conf "place"));
         end;
         tag "td" "colspan=2" begin
           Wserver.wprint "<input name=t_place%d size=30%s>" cnt
             (match t with
              [ Some {t_place = n} -> " value=\"" ^ n ^ "\""
            | _ -> "" ]);
         end;
       end;
       Wserver.wprint "\n";
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s" (capitale (transl conf "name"));
         end;
         tag "td" begin
           Wserver.wprint "<input name=t_name%d size=20%s>" cnt
             (match t with
              [ Some {t_name = Tname n} -> " value=\"" ^ n ^ "\""
              | _ -> "" ]);
         end;
         tag "td" begin
           Wserver.wprint "%s" (capitale (transl_nth conf "nth" 0));
         end;
         tag "td" begin
           Wserver.wprint "<input name=t_nth%d size=3%s>"
             cnt
             (match t with
              [ Some {t_nth = n} when n <> 0 -> " value=" ^ string_of_int n
              | _ -> "" ]);
         end;
         tag "td" begin
           Wserver.wprint "%s <input type=checkbox name=t_main_title%d%s>"
             (capitale (transl conf "main title")) cnt
             (match t with
              [ Some {t_name = Tmain} -> " checked"
              | _ -> "" ]);
         end;
       end;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       Update.print_date conf base (capitale (transl conf "begin"))
         ("t_date_start" ^ string_of_int cnt)
         (match t with
          [ Some t -> Adef.od_of_codate t.t_date_start
          | _ -> None ]);
       Wserver.wprint "\n";
       Update.print_date conf base (capitale (transl conf "end"))
         ("t_date_end" ^ string_of_int cnt)
         (match t with
          [ Some t -> Adef.od_of_codate t.t_date_end
          | _ -> None ]);
     end;
     Wserver.wprint "\n<p>\n";
     print_add_title conf base cnt;
  return ()
;

value print_titles conf base p =
  let tl =
    match p.titles with
    [ [] -> [None]
    | tl -> List.map (fun t -> Some t) tl ]
  in
  do print_add_title conf base 0; return
  let _ = List.fold_left
    (fun cnt t -> do print_title conf base t cnt; return cnt + 1)
    1 tl
  in ()
;

value print_sources conf base field =
  do tag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "sources"));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=src size=50 maxlength=200%s>\n"
             (match field with
              [ s when s <> "" -> " value=\"" ^ quote_escaped s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_occupation conf base p =
  do tag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "occupation"));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=occu size=50 maxlength=200%s>\n"
             (match p.occupation with
              [ s when s <> "" -> " value=\"" ^ quote_escaped s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_access conf base p =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s" (capitale (transl conf "access"));
      end;
      tag "td" begin
        Wserver.wprint "<input type=radio name=access value=IfTitles%s>"
          (match p.access with [ IfTitles -> " checked" | _ -> "" ]);
        Wserver.wprint "%s\n" (capitale (transl conf "if titles"));
        Wserver.wprint "<input type=radio name=access value=Public%s>"
          (match p.access with [ Public -> " checked" | _ -> "" ]);
        Wserver.wprint "%s\n" (capitale (transl conf "public"));
        Wserver.wprint "<input type=radio name=access value=Private%s>"
          (match p.access with [ Private -> " checked" | _ -> "" ]);
        Wserver.wprint "%s\n" (capitale (transl conf "private"));
      end;
    end;
  end
;

value print_notes conf base p =
  do stag "textarea" "name=notes rows=6 cols=70 wrap=virtual" begin
       if p.notes <> "" then Wserver.wprint "%s" (quote_escaped p.notes)
       else ();
     end;
     Wserver.wprint "\n";
  return ()
;

value print_person conf base p =
  do tag "table" "border=1" begin
       print_first_name conf base p;
       Wserver.wprint "\n";
       print_surname conf base p;
       Wserver.wprint "\n";
       print_public_name conf base p;
       Wserver.wprint "\n";
       print_photo conf base p;
     end;
     Wserver.wprint "\n<br>\n\n";
     tag "table" "border=1" begin
       print_nick_names conf base p;
       Wserver.wprint "\n";
       print_aliases conf base p;
       Wserver.wprint "\n";
       print_first_names_aliases conf base p;
       Wserver.wprint "\n";
       print_surnames_aliases conf base p;
     end;
     Wserver.wprint "\n";
     tag "h4" begin Wserver.wprint "%s" (capitale (transl conf "birth")); end;
     tag "table" "border=1" begin
       print_birth_place conf base p;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       print_birth_date conf base p;
     end;
     Wserver.wprint "<p>\n";
     tag "table" "border=1" begin
       print_bapt_place conf base p;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       print_bapt_date conf base p;
     end;
     Wserver.wprint "\n";
     tag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "death"));
     end;
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin print_death_type conf base p; end;
         print_death_place conf base p;
       end;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       print_death_date conf base p;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           print_death_reason conf base p;
         end;
       end;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin print_burial_type conf base p; end;
         print_burial_place conf base p;
       end;
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       print_burial_date conf base p;
     end;
     Wserver.wprint "\n";
     print_occupation conf base p;
     Wserver.wprint "\n";
     tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "title/titles" 1));
     end;
     Wserver.wprint "\n";
     print_titles conf base p;
     Wserver.wprint "\n";
     print_access conf base p;
     Wserver.wprint "\n";
     tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "note/notes" 1));
     end;
     print_notes conf base p;
     Wserver.wprint "\n";
     print_sources conf base p.psources;
  return ()
;

value merge_call conf =
  do Wserver.wprint "<input type=hidden name=m value=MRG_MOD_IND_OK>\n";
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

value print_mod1 conf base p digest =
  let title _ =
    Wserver.wprint "%s / %s # %d" (capitale (transl conf "modify"))
      (capitale (transl_nth conf "person/persons" 0))
      (Adef.int_of_iper p.cle_index)
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       match p_getenv conf.env "m" with
       [ Some "MRG_MOD_IND_OK" -> merge_call conf
       | _ -> Wserver.wprint "<input type=hidden name=m value=MOD_IND_OK>\n" ];
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "\n";
       print_person conf base p;
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add1 conf base p =
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "add"))
      (capitale (transl_nth conf "person/persons" 0))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=ADD_IND_OK>\n";
       print_person conf base p;
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_del1 conf base p =
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "delete"))
      (capitale (transl_nth conf "person/persons" 0))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=DEL_IND_OK>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add conf base =
  let p =
    {first_name = ""; surname = ""; occ = 0; photo = "";
     first_names_aliases = []; surnames_aliases = [];
     public_name = ""; nick_names = []; aliases = [];
     titles = []; occupation = "";
     sexe = Neutre; access = IfTitles;
     birth = Adef.codate_None; birth_place = "";
     baptism = Adef.codate_None; baptism_place = "";
     death = DontKnowIfDead; death_place = "";
     burial = UnknownBurial; burial_place = "";
     family = [| |];
     notes = ""; psources = "";
     cle_index = bogus_person_index}
  in
  print_add1 conf base p
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.persons.get i in
      print_mod1 conf base (string_person_of base p) (Update.digest_person p)
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.persons.get i in
      print_del1 conf base (string_person_of base p)
  | _ -> incorrect_request conf ]
;
