(* camlp4r ./pa_html.cmo *)
(* $Id: updateInd.ml,v 3.20 2000-12-19 16:25:07 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

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
   t_ident = sou base t.t_ident; t_place = sou base t.t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end;
   t_nth = t.t_nth}
;

value string_person_of base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper p.cle_index
    else p.occ
  in
  let fp ip =
    let p = poi base ip in
    (sou base p.first_name, sou base p.surname, p.occ, Update.Link, "")
  in
  Gutil.map_person_ps fp (sou base) p
;

value print_first_name conf base p =
  let occ =
    if p.first_name = "?" || p.surname = "?" then
      (* Adef.int_of_iper p.cle_index *) 0
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
        (quote_escaped p.first_name);
    end;
    tag "td" begin
      let s = capitale (transl conf "number") in
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
        (if p.sex = Male then " checked" else "")
        (transl_nth conf "M/F" 0);
      Wserver.wprint "<input type=radio name=sex value=F%s>%s\n"
        (if p.sex = Female then " checked" else "")
        (transl_nth conf "M/F" 1);
      Wserver.wprint "<input type=radio name=sex value=N%s>?\n"
        (if p.sex = Neuter then " checked" else "");
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
      if p.public_name <> "" then
        Wserver.wprint " value=\"%s\"" (quote_escaped p.public_name)
      else ();
      Wserver.wprint ">";
    end;
  end
;

value print_image conf base p =
  tag "tr" begin
    tag "td" begin
      Wserver.wprint "%s"
        (capitale (nominative (transl_nth conf "image/images" 0)));
    end;
    tag "td" "colspan=3" begin
      Wserver.wprint "<input name=image size=50";
      if p.image <> "" then Wserver.wprint " value=\"%s\"" p.image
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
    tag "td" begin Wserver.wprint "%s" (capitale item.i_txt_name); end;
    tag "td" begin
      Wserver.wprint "<input name=\"%s%d\" size=30" item.i_name i_cnt;
      if i_val <> "" then
        Wserver.wprint " value=\"%s\"" (quote_escaped i_val)
      else ();
      Wserver.wprint ">";
    end;
    tag "td" begin Wserver.wprint "%s" (capitale item.i_txt_add); end;
    tag "td" begin
      Wserver.wprint "<input type=checkbox name=\"add_%s%d\" value=on>"
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

value cons_insert conf name = capitale (transl_decline conf "insert" name);

value print_qualifiers conf base p =
  gen_print_ext_items conf base
    {i_name = "qualifier"; i_txt_name = nominative (transl conf "qualifier");
     i_txt_add = cons_insert conf (transl conf "qualifier")}
    p.qualifiers
;

value print_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "alias"; i_txt_name = nominative (transl conf "alias");
     i_txt_add = cons_insert conf (transl conf "alias")}
    p.aliases
;

value print_first_names_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "first_name_alias";
     i_txt_name = nominative (transl conf "first name alias");
     i_txt_add = cons_insert conf (transl conf "first name alias")}
    p.first_names_aliases
;

value print_surnames_aliases conf base p =
  gen_print_ext_items conf base
    {i_name = "surname_alias";
     i_txt_name = nominative (transl conf "surname alias");
     i_txt_add = cons_insert conf (transl conf "surname alias")}
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

value birth_is_empty base p =
  match Adef.od_of_codate p.birth with
  [ Some _ -> False
  | None -> True ]
;

value print_death_type conf base p =
  let be = birth_is_empty base p in
  tag "select" "name=death" begin
    if be && p.death = DontKnowIfDead then
      Wserver.wprint "<option value=Auto selected> -\n"
    else ();
    Wserver.wprint "<option value=NotDead%s>"
      (match p.death with [ NotDead -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "alive" 2));
    Wserver.wprint "<option value=DontKnowIfDead%s>"
      (if be then ""
       else match p.death with [ DontKnowIfDead -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "don't know"));
    Wserver.wprint "<option value=Death%s>"
      (match p.death with
       [ Death _ _ | DeadDontKnowWhen -> " selected"
       | _ -> "" ]);
    Wserver.wprint "%s...\n" (capitale (transl_nth conf "died" 2));
    Wserver.wprint "<option value=DeadYoung%s>"
      (match p.death with [ DeadYoung -> " selected" | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl_nth conf "died young" 2));
  end
;

value print_death_place conf base p =
  do tag "td" begin
       Wserver.wprint "%s\n" (capitale (transl conf "place"));
     end;
     tag "td" begin
       Wserver.wprint "<input name=death_place size=40 maxlength=200%s>\n"
         (if p.death_place = "" then ""
          else " value=\"" ^ quote_escaped p.death_place ^ "\"");
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

value print_insert_title conf base cnt =
  do tag "table" "border=1" begin
       tag "tr" begin
         let var = "ins_title" ^ string_of_int cnt in
         tag "td" begin
           let s = transl_nth conf "title/titles" 0 in
           let sn = "<input name=" ^ var ^ "_n size=1 maxlength=1 value=1> " in
           Wserver.wprint "%s <input type=checkbox name=%s value=on>"
             (capitale (transl_decline conf "insert" (sn ^ s))) var;
         end;
       end;
     end;
     Wserver.wprint "\n";
     html_p conf;
  return ()
;

value print_title conf base t cnt =
  do tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s\n"
             (capitale (nominative (transl_nth conf "title/titles" 0)));
         end;
         tag "td" begin
           Wserver.wprint "<input name=t_ident%d size=15%s>" cnt
             (match t with
              [ Some {t_ident = n} -> " value=\"" ^ quote_escaped n ^ "\""
              | _ -> "" ]);
         end;
         tag "td" begin
           Wserver.wprint "%s\n" (capitale (transl conf "estate"));
         end;
         tag "td" "colspan=2" begin
           Wserver.wprint "<input name=t_place%d size=30%s>" cnt
             (match t with
              [ Some {t_place = n} -> " value=\"" ^ quote_escaped n ^ "\""
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
              [ Some {t_name = Tname n} -> " value=\"" ^ quote_escaped n ^ "\""
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
           Wserver.wprint
             "%s <input type=checkbox name=t_main_title%d%s value=on>"
             (capitale (transl conf "main title")) cnt
             (match t with
              [ Some {t_name = Tmain} -> " checked"
              | _ -> "" ]);
         end;
       end;
     end;
     Wserver.wprint "\n";
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
     Wserver.wprint "\n";
     html_p conf;
     print_insert_title conf base cnt;
  return ()
;

value print_titles conf base p =
  let tl =
    match p.titles with
    [ [] -> [None]
    | tl -> List.map (fun t -> Some t) tl ]
  in
  do print_insert_title conf base 0; return
  let _ = List.fold_left
    (fun cnt t -> do print_title conf base t cnt; return cnt + 1)
    1 tl
  in ()
;

value print_add_relation conf base cnt =
  do tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           let s = transl_nth conf "relation/relations" 0 in
           Wserver.wprint "%s
             <input type=checkbox name=add_relation%d value=on>"
             (capitale (transl_decline conf "insert" s)) cnt;
         end;
       end;
     end;
     Wserver.wprint "\n";
     html_p conf;
  return ()
;

value print_relation_type conf base r var =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        tag "select" "name=%s_type" var begin
          Wserver.wprint "<option value=Undef%s> -\n"
            (match r with
             [ Some {r_fath = None; r_moth = None} -> " selected"
             | Some _ -> ""
             | None -> " selected" ]);
          Wserver.wprint "<option value=Adoption%s>"
            (match r with
             [ Some {r_fath = None; r_moth = None} -> ""
             | Some {r_type = Adoption} -> " selected"
             | _ -> "" ]);
          Wserver.wprint "%s\n"
            (capitale (relation_type_text conf Adoption 2));
          Wserver.wprint "<option value=Recognition%s>"
            (match r with
             [ Some {r_type = Recognition} -> " selected" | _ -> "" ]);
          Wserver.wprint "%s\n"
            (capitale (relation_type_text conf Recognition 2));
          Wserver.wprint "<option value=CandidateParent%s>"
            (match r with
             [ Some {r_type = CandidateParent} -> " selected" | _ -> "" ]);
          Wserver.wprint "%s\n"
            (capitale (relation_type_text conf CandidateParent 2));
          Wserver.wprint "<option value=GodParent%s>"
            (match r with
             [ Some {r_type = GodParent} -> " selected" | _ -> "" ]);
          Wserver.wprint "%s\n"
            (capitale (relation_type_text conf GodParent 2));
          Wserver.wprint "<option value=FosterParent%s>"
            (match r with
             [ Some {r_type = FosterParent} -> " selected" | _ -> "" ]);
          Wserver.wprint "%s\n"
            (capitale (relation_type_text conf FosterParent 2));
        end;
      end;
    end;
  end
;

value print_relation_person conf base r proj var =
  let key =
    match r with
    [ Some r ->
        match proj r with
        [ Some x -> x
        | None -> ("", "", 0, Update.Link, "") ]
    | None -> ("", "", 0, Update.Link, "") ]
  in
  Update.print_simple_person conf base var key
;

value print_relation conf base r cnt =
  let rcnt = "r" ^ string_of_int cnt in
  do print_relation_type conf base r rcnt;
     print_relation_person conf base r (fun r -> r.r_fath) (rcnt ^ "_fath");
     print_relation_person conf base r (fun r -> r.r_moth) (rcnt ^ "_moth");
     html_p conf;
     print_add_relation conf base cnt;
  return ()
;

value print_relations conf base p =
  let rl =
    match p.rparents with
    [ [] -> [None]
    | rl -> List.map (fun r -> Some r) rl ]
  in
  do print_add_relation conf base 0; return
  let _ = List.fold_left
    (fun cnt r -> do print_relation conf base r cnt; return cnt + 1)
    1 rl
  in ()
;

value print_source conf base field =
  do tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "source/sources" 0));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=%s size=50 maxlength=200%s>\n"
             "src"
             (match field with
              [ s when s <> "" ->
                  " value=\"" ^ quote_escaped s ^ "\""
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
              [ s when s <> "" ->
                  " value=\"" ^ quote_escaped s ^ "\""
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
       if p.notes <> "" then
         Wserver.wprint "%s" (quote_escaped p.notes)
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
       print_image conf base p;
     end;
     Wserver.wprint "\n";
     html_br conf;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       print_qualifiers conf base p;
       Wserver.wprint "\n";
       let propose_alias =
         try List.assoc "propose_alias" conf.base_env <> "no" with
         [ Not_found -> True ]
       in
       if propose_alias then
         do print_aliases conf base p;
            Wserver.wprint "\n";
         return ()
       else
         let _ = List.fold_left
           (fun i_cnt a ->
              do Wserver.wprint
                   "<input type=hidden name=alias%d value=\"%s\">\n"
                   i_cnt (quote_escaped a);
              return i_cnt + 1)
           0 p.aliases
         in
         ();
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
     print_birth_date conf base p;
     Wserver.wprint "\n";
     Update.print_src conf "birth_src" p.birth_src;
     Wserver.wprint "\n";
     html_p conf;
     tag "table" "border=1" begin
       print_bapt_place conf base p;
     end;
     Wserver.wprint "\n";
     print_bapt_date conf base p;
     Wserver.wprint "\n";
     Update.print_src conf "bapt_src" p.baptism_src;
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
     print_death_date conf base p;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           print_death_reason conf base p;
         end;
       end;
     end;
     Wserver.wprint "\n";
     Update.print_src conf "death_src" p.death_src;
     Wserver.wprint "\n";
     tag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "burial"));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin print_burial_type conf base p; end;
         print_burial_place conf base p;
       end;
     end;
     Wserver.wprint "\n";
     print_burial_date conf base p;
     Wserver.wprint "\n";
     Update.print_src conf "burial_src" p.burial_src;
     Wserver.wprint "\n";
     print_occupation conf base p;
     Wserver.wprint "\n";
     tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "relation/relations" 1));
     end;
     Wserver.wprint "\n";
     print_relations conf base p;
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
       Wserver.wprint "%s"
         (capitale (nominative (transl_nth conf "note/notes" 1)));
     end;
     print_notes conf base p;
     Wserver.wprint "\n";
     print_source conf base p.psources;
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
    match p_getenv conf.env "m" with
    [ Some "MRG_MOD_IND_OK" ->
        let s = transl_nth conf "person/persons" 1 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "merge" s))
          (Adef.int_of_iper p.cle_index)
    | _ ->
        let s = transl_nth conf "person/persons" 0 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "modify" s))
          (Adef.int_of_iper p.cle_index) ]
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       match p_getenv conf.env "m" with
       [ Some "MRG_MOD_IND_OK" -> merge_call conf
       | _ -> Wserver.wprint "<input type=hidden name=m value=MOD_IND_OK>\n" ];
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "\n";
       print_person conf base p;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add1 conf base p =
  let title _ =
    let s = transl_nth conf "person/persons" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "add" s))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=ADD_IND_OK>\n";
       print_person conf base p;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_del1 conf base p =
  let title _ =
    let s = transl_nth conf "person/persons" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=DEL_IND_OK>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add conf base =
  let p =
    {first_name = ""; surname = ""; occ = 0; image = "";
     first_names_aliases = []; surnames_aliases = [];
     public_name = ""; qualifiers = []; aliases = [];
     titles = []; rparents = []; related = []; occupation = "";
     sex = Neuter; access = IfTitles;
     birth = Adef.codate_None; birth_place = ""; birth_src = "";
     baptism = Adef.codate_None; baptism_place = ""; baptism_src = "";
     death = DontKnowIfDead; death_place = ""; death_src = "";
     burial = UnknownBurial; burial_place = ""; burial_src = "";
     notes = ""; psources = "";
     cle_index = bogus_person_index}
  in
  print_add1 conf base p
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      print_mod1 conf base (string_person_of base p) (Update.digest_person p)
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      print_del1 conf base (string_person_of base p)
  | _ -> incorrect_request conf ]
;
