(* camlp4r ./pa_html.cmo *)
(* $Id: updateInd.ml,v 3.39 2001-02-18 09:30:15 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

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

value if_propose conf base var f p proj =
  let propose_var =
    try List.assoc ("propose_" ^ var) conf.base_env <> "no" with
    [ Not_found -> True ]
  in
  if propose_var || proj <> [] then
    do f conf base p; Wserver.wprint "\n"; return ()
  else
    let _ = List.fold_left
      (fun i_cnt a ->
         do Wserver.wprint "<input type=hidden name=%s%d value=\"%s\">\n"
              var i_cnt (quote_escaped a);
        return i_cnt + 1)
      0 proj
    in
    ()
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
       if_propose conf base "qualifier" print_qualifiers p p.qualifiers;
       if_propose conf base "alias" print_aliases p p.aliases;
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

value print_update_ind_aux conf base p digest =
  let title _ =
    match p_getenv conf.env "m" with
    [ Some ("MRG_IND_OK" | "MRG_MOD_IND_OK") ->
        let s = transl_nth conf "person/persons" 1 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "merge" s))
          (Adef.int_of_iper p.cle_index)
    | Some ("MOD_IND" | "MOD_IND_OK") ->
        let s = transl_nth conf "person/persons" 0 in
        Wserver.wprint "%s # %d" (capitale (transl_decline conf "modify" s))
          (Adef.int_of_iper p.cle_index)
    | Some ("ADD_IND" | "ADD_IND_OK") ->
        let s = transl_nth conf "person/persons" 0 in
        Wserver.wprint "%s" (capitale (transl_decline conf "add" s))
    | _ -> assert False ]
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       match p_getenv conf.env "m" with
       [ Some ("MRG_IND_OK" | "MRG_MOD_IND_OK") ->
           do match p_getint conf.env "i2" with
              [ Some i2 ->
                  Wserver.wprint "<input type=hidden name=i2 value=%d>\n" i2
              | _ -> () ];
              match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
              [ (Some i1, Some i2) ->
                  do Wserver.wprint
                       "<input type=hidden name=ini1 value=%d>\n" i1;
                     Wserver.wprint
                       "<input type=hidden name=ini2 value=%d>\n" i2;
                  return ()
              | _ -> () ];
              Wserver.wprint
                "<input type=hidden name=m value=MRG_MOD_IND_OK>\n";
           return ()
       | Some ("MOD_IND" | "MOD_IND_OK") ->
           Wserver.wprint "<input type=hidden name=m value=MOD_IND_OK>\n"
       | Some ("ADD_IND" | "ADD_IND_OK") ->
           Wserver.wprint "<input type=hidden name=m value=ADD_IND_OK>\n"
       | _ -> assert False ];
       print_person conf base p;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

(* Interpretation of template file 'updind.txt' *)

type ast = Templ.ast ==
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and char
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of string and list ast_expr ]
and ast_expr = Templ.ast_expr ==
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eop of string and ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Evar of string and list string
  | Etransl of bool and string and char ]
;

type env =
  [ Vstring of string
  | Vfun of list string and list ast
  | Vint of int
  | Vnone ]
;

type variable_value =
  [ VVgen of string
  | VVdate of option date and string
  | VVrelation of option (gen_relation Update.key string) and list string
  | VVtitle of option (gen_title string) and list string
  | VVcvar of string
  | VVnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value rec eval_variable conf base env p =
  fun
  [ ["bapt"; s] -> VVdate (Adef.od_of_codate p.baptism) s
  | ["birth"; s] -> VVdate (Adef.od_of_codate p.birth) s
  | ["burial"; s] ->
      let d =
        match p.burial with
        [ Buried cod -> Adef.od_of_codate cod
        | Cremated cod -> Adef.od_of_codate cod
        | _ -> None ]
      in
      VVdate d s
  | ["death"; s] ->
      let d =
        match p.death with
        [ Death _ cd -> Some (Adef.date_of_cdate cd)
        | _ -> None ]
      in
      VVdate d s
  | ["relation" :: sl] ->
      let r =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.rparents (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      VVrelation r sl
  | ["title" :: sl] ->
      let t =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.titles (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      VVtitle t sl
  | ["title_date_start"; s] ->
      let d =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_start
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      VVdate d s
  | ["title_date_end"; s] ->
      let d =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_end
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      VVdate d s
  | [] -> VVgen ""
  | [s] ->
      let v = extract_var "cvar_" s in
      if v <> "" then VVcvar v else VVgen s
  | [s :: sl] -> VVnone ]
;

(* string values *)

value eval_base_env_variable conf v =
  try List.assoc v conf.base_env with [ Not_found -> "" ]
;

value eval_string_env var env =
   match get_env var env with
   [ Vstring x -> x
   | _ -> "" ]
;

value eval_int_env var env =
   match get_env var env with
   [ Vint x -> string_of_int x
   | _ -> "" ]
;

value try_eval_gen_variable conf base env p =
  fun
  [ "bapt_place" -> quote_escaped p.baptism_place
  | "bapt_src" -> quote_escaped p.baptism_src
  | "birth_place" -> quote_escaped p.birth_place
  | "birth_src" -> quote_escaped p.birth_src
  | "burial_place" -> quote_escaped p.burial_place
  | "burial_src" -> quote_escaped p.burial_src
  | "death_place" -> quote_escaped p.death_place
  | "death_src" -> quote_escaped p.death_src
  | "cnt" -> eval_int_env "cnt" env
  | "digest" -> eval_string_env "digest" env
  | "first_name" -> quote_escaped p.first_name
  | "image" -> quote_escaped p.image
  | "index" -> string_of_int (Adef.int_of_iper p.cle_index)
  | "item" -> eval_string_env "item" env
  | "occ" -> if p.occ <> 0 then string_of_int p.occ else ""
  | "occupation" -> quote_escaped p.occupation
  | "public_name" -> quote_escaped p.public_name
  | "surname" -> quote_escaped p.surname
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value eval_date_field =
  fun
  [ Some d  ->
      match d with
      [ Dgreg d Dgregorian -> Some d
      | Dgreg d Djulian -> Some (Calendar.julian_of_gregorian d)
      | Dgreg d Dfrench -> Some (Calendar.french_of_gregorian d)
      | Dgreg d Dhebrew -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None ]
  | None -> None ]
;

value eval_date_text =
  fun
  [ Some (Dtext s) -> s
  | _ -> "" ]
;

value eval_date_variable conf base env od =
  fun
  [ "day" ->
      match eval_date_field od with
      [ Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "" ]
  | "month" ->
      match eval_date_field od with
      [ Some d -> if d.month = 0 then "" else string_of_int d.month
      | None -> "" ]
  | "text" -> eval_date_text od
  | "year" ->
      match eval_date_field od with
      [ Some d -> string_of_int d.year
      | None -> "" ]
  | "oryear" ->
      match od with
      [ Some (Dgreg {prec = OrYear y} _) -> string_of_int y
      | Some (Dgreg {prec = YearInt y} _) -> string_of_int y
      | _ -> "" ]
  | v -> ">%" ^ v ^ "???" ]
;

value eval_key_variable conf base env (fn, sn, oc, create, var) =
  fun
  [ "first_name" -> quote_escaped fn
  | "occ" -> if oc = 0 then "" else string_of_int oc
  | "surname" -> quote_escaped sn
  | s -> ">%" ^ s ^ "???" ]
;

value eval_relation_variable conf base env p r =
  fun
  [ ["r_father"; s] ->
      match r with
      [ Some {r_fath = Some x} -> eval_key_variable conf base env x s
      | _ -> "" ]
  | ["r_mother"; s] ->
      match r with
      [ Some {r_moth = Some x} -> eval_key_variable conf base env x s
      | _ -> "" ]
  | [s :: _] -> ">%" ^ s ^ "???"
  | _ -> ">???" ]
;

value eval_title_variable conf base env p t =
  fun
  [ ["t_ident"] ->
      match t with
      [ Some {t_ident = x} -> quote_escaped x
      | _ -> "" ]
  | ["t_estate"] ->
      match t with
      [ Some {t_place = x} -> quote_escaped x
      | _ -> "" ]
  | ["t_name"] ->
      match t with
      [ Some {t_name = Tname x} -> quote_escaped x
      | _ -> "" ]
  | ["t_nth"] ->
      match t with
      [ Some {t_nth = x} -> if x = 0 then "" else string_of_int x
      | _ -> "" ]
  | [s :: _] -> ">%" ^ s ^ "???"
  | _ -> ">???" ]
;

value split_at_coloncolon s =
  loop 0 where rec loop i =
    if i >= String.length s - 1 then None
    else
      match (s.[i], s.[i+1]) with
      [ (':', ':') ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 2) (String.length s - i - 2) in
          Some (s1, s2)
      | _ -> loop (i + 1) ]
;

value eval_transl conf base env upp s c =
  let r =
    match c with
    [ '0'..'9' ->
        let n = Char.code c - Char.code '0' in
        match split_at_coloncolon s with
        [ None -> nominative (Util.transl_nth conf s n)
        | Some (s1, s2) ->
            Util.transl_decline conf s1 (Util.transl_nth conf s2 n) ]
    | _ -> nominative (Util.transl conf s) ^ String.make 1 c ]
  in
  if upp then capitale r else r
;

value eval_expr conf base env p =
  fun
  [ Estr s -> s
  | Evar s [] ->
      try try_eval_gen_variable conf base env p s with
      [ Not_found -> ">" ^ s ^ "???" ]
  | Etransl upp s c -> eval_transl conf base env upp s c
  | _ -> ">parse_error" ]
;

(* bool values *)

value is_death_reason dr =
  fun
  [ Death dr1 _ -> dr = dr1
  | _ -> False ]
;

value eval_gen_bool_variable conf base env p =
  fun 
  [ "dead_dont_know_when" -> p.death = DeadDontKnowWhen
  | "died_young" -> p.death = DeadYoung
  | "dont_know_if_dead" -> p.death = DontKnowIfDead
  | "bt_buried" -> match p.burial with [ Buried _ -> True | _ -> False ]
  | "bt_cremated" -> match p.burial with [ Cremated _ -> True | _ -> False ]
  | "bt_unknown_burial" -> p.burial = UnknownBurial
  | "dr_killed" -> is_death_reason Killed p.death
  | "dr_murdered" -> is_death_reason Murdered p.death
  | "dr_executed" -> is_death_reason Executed p.death
  | "dr_disappeared" -> is_death_reason Disappeared p.death
  | "dr_unspecified" -> is_death_reason Unspecified p.death
  | "has_aliases" -> p.aliases <> []
  | "has_birth_date" -> Adef.od_of_codate p.birth <> None
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_qualifiers" -> p.qualifiers <> []
  | "has_relations" -> p.rparents <> []
  | "has_surnames_aliases" -> p.surnames_aliases <> []
  | "has_titles" -> p.titles <> []
  | "is_female" -> p.sex = Female
  | "is_male" -> p.sex = Male
  | "not_dead" -> p.death = NotDead
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv conf.env v with
        [ Some "" | None -> False
        | _ -> True ]
      else do Wserver.wprint ">%%%s???" s; return False ]
;

value is_calendar cal =
  fun
  [ Some (Dgreg _ x) -> x = cal
  | _ -> False ]
;

value is_precision cond =
  fun
  [ Some (Dgreg {prec = x} _) -> cond x
  | _ -> False ]
;

value eval_date_bool_variable conf base env od =
  fun
  [ "cal_gregorian" -> is_calendar Dgregorian od
  | "cal_julian" -> is_calendar Djulian od
  | "cal_french" -> is_calendar Dfrench od
  | "cal_hebrew" -> is_calendar Dhebrew od
  | "prec_no" -> od = None
  | "prec_sure" -> is_precision (fun [ Sure -> True | _ -> False ]) od
  | "prec_about" -> is_precision (fun [ About -> True | _ -> False ]) od
  | "prec_maybe" -> is_precision (fun [ Maybe -> True | _ -> False ]) od
  | "prec_before" -> is_precision (fun [ Before -> True | _ -> False ]) od
  | "prec_after" -> is_precision (fun [ After -> True | _ -> False ]) od
  | "prec_oryear" -> is_precision (fun [ OrYear _ -> True | _ -> False ]) od
  | "prec_yearint" -> is_precision (fun [ YearInt _ -> True | _ -> False ]) od
  | s -> do Wserver.wprint ">%%%s???" s; return False ]
;

value is_relation_type rt =
  fun
  [ Some {r_type = x} -> x = rt
  | _ -> False ]
;

value eval_relation_person_bool_variable conf base env
  (fn, sn, oc, create, var)
=
  fun
  [ ["create"] -> match create with [ Update.Create _ _ -> True | _ -> False ]
  | ["link"] -> create = Update.Link
  | [s] -> do Wserver.wprint ">%%%s???" s; return False
  | _ -> do Wserver.wprint ">???"; return False ]
;

value eval_relation_bool_variable conf base env r =
  fun
  [ ["r_father" :: sl] ->
      match r with
      [ Some {r_fath = Some x} ->
         eval_relation_person_bool_variable conf base env x sl
      | _ -> False ]
  | ["r_mother" :: sl] ->
      match r with
      [ Some {r_moth = Some x} ->
         eval_relation_person_bool_variable conf base env x sl
      | _ -> False ]
  | ["rt_adoption"] -> is_relation_type Adoption r
  | ["rt_candidate_parent"] -> is_relation_type CandidateParent r
  | ["rt_empty"] ->
      match r with
      [ Some {r_fath = None; r_moth = None} | None -> True
      | _ -> False ]
  | ["rt_foster_parent"] -> is_relation_type FosterParent r
  | ["rt_godparent"] -> is_relation_type GodParent r
  | ["rt_regognition"] -> is_relation_type Recognition r
  | [s] -> do Wserver.wprint ">%%%s???" s; return False
  | _ -> do Wserver.wprint ">???"; return False ]
;

value eval_title_bool_variable conf base env t =
  fun
  [ ["t_main"] ->
      match t with
      [ Some {t_name = Tmain} -> True
      | _ -> False ]
  | _ -> do Wserver.wprint ">???"; return False ]
;

value eval_bool_variable conf base env p s sl =
  match eval_variable conf base env p [s :: sl] with
  [ VVgen s -> eval_gen_bool_variable conf base env p s
  | VVdate od s -> eval_date_bool_variable conf base env od s
  | VVrelation r sl -> eval_relation_bool_variable conf base env r sl
  | VVtitle t sl -> eval_title_bool_variable conf base env t sl
  | VVcvar _ -> do Wserver.wprint ">%%%s???" s; return False
  | VVnone -> do Wserver.wprint ">%%%s???" s; return False ]
;

value eval_bool_value conf base env p =
  let rec bool_eval =
    fun
    [ Eor e1 e2 -> bool_eval e1 || bool_eval e2
    | Eand e1 e2 -> bool_eval e1 && bool_eval e2
    | Eop op e1 e2 ->
        match op with
        [ "=" -> string_eval e1 = string_eval e2
        | "!=" -> string_eval e1 <> string_eval e2
        | _ -> do Wserver.wprint "op %s???" op; return False ]
    | Enot e -> not (bool_eval e)
    | Evar s sl -> eval_bool_variable conf base env p s sl
    | Estr s -> do Wserver.wprint "\"%s\"???" s; return False
    | Etransl _ s _ -> do Wserver.wprint "[%s]???" s; return False ]
  and string_eval =
    fun
    [ Estr s -> s
    | Evar s sl ->
        try
          match eval_variable conf base env p [s :: sl] with
          [ VVgen s -> try_eval_gen_variable conf base env p s
          | VVdate od s -> eval_date_variable conf base env od s
          | VVcvar s -> eval_base_env_variable conf s
          | VVrelation _ _ -> do Wserver.wprint ">%%%s???" s; return ""
          | VVtitle _ _ -> do Wserver.wprint ">%%%s???" s; return ""
          | VVnone -> do Wserver.wprint ">%%%s???" s; return "" ]
        with
        [ Not_found -> do Wserver.wprint ">%%%s???" s; return "" ]
    | x -> do Wserver.wprint "val???"; return "" ]
  in
  bool_eval
;

(* print *)

value print_variable conf base env p sl =
  match eval_variable conf base env p sl with
  [ VVgen s ->
      try Wserver.wprint "%s" (try_eval_gen_variable conf base env p s) with
      [ Not_found -> Templ.print_variable conf base s ]
  | VVdate od s -> Wserver.wprint "%s" (eval_date_variable conf base env od s)
  | VVcvar s ->
      try Wserver.wprint "%s" (List.assoc s conf.base_env) with
      [ Not_found -> () ]
  | VVrelation r sl ->
      Wserver.wprint "%s" (eval_relation_variable conf base env p r sl)
  | VVtitle t sl ->
      Wserver.wprint "%s" (eval_title_variable conf base env p t sl)
  | VVnone ->
      do Wserver.wprint ">%%";
         list_iter_first
           (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
           sl;
         Wserver.wprint "???";
      return () ]
;

value subst_text x v s =
  if String.length x = 0 then s
  else
    loop 0 0 0 where rec loop len i i_ok =
      if i = String.length s then
        if i_ok > 0 then loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
        else Buff.get len
      else if s.[i] = x.[i_ok] then
        if i_ok = String.length x - 1 then
          loop (Buff.mstore len v) (i + 1) 0
        else loop len (i + 1) (i_ok + 1)
      else if i_ok > 0 then
        loop (Buff.store len s.[i - i_ok]) (i - i_ok + 1) 0
      else loop (Buff.store len s.[i]) (i + 1) 0
;

value rec subst sf =
  fun
  [ Atext s -> Atext (sf s)
  | Avar s sl -> Avar (sf s) (List.map sf sl)
  | Atransl b s c -> Atransl b (sf s) c
  | Awid_hei s -> Awid_hei (sf s)
  | Aif e alt ale -> Aif (subste sf e) (substl sf alt) (substl sf ale)
  | Aforeach s sl al -> Aforeach (sf s) (List.map sf sl) (substl sf al)
  | Adefine f xl al alk ->
      Adefine (sf f) (List.map sf xl) (substl sf al) (substl sf alk)
  | Aapply f el -> Aapply (sf f) (substel sf el) ]
and substl sf al =
  List.map (subst sf) al
and subste sf =
  fun
  [ Eor e1 e2 -> Eor (subste sf e1) (subste sf e2)
  | Eand e1 e2 -> Eand (subste sf e1) (subste sf e2)
  | Eop op e1 e2 -> Eop (sf op) (subste sf e1) (subste sf e2)
  | Enot e -> Enot (subste sf e)
  | Estr s -> Estr (sf s)
  | Evar s sl -> Evar (sf s) (List.map sf sl)
  | Etransl upp s c -> Etransl upp s c ]
and substel sf el =
  List.map (subste sf) el
;

value rec print_ast conf base env p =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n -> Wserver.wprint "%s" (eval_transl conf base env upp s n)
  | Avar s sl -> print_variable conf base env p [s :: sl]
  | Awid_hei s -> Wserver.wprint "Awid_hei"
  | Aif e alt ale -> print_if conf base env p e alt ale
  | Aforeach s sl al -> print_foreach conf base env p s sl al
  | Adefine f xl al alk -> print_define conf base env p f xl al alk
  | Aapply f el -> print_apply conf base env p f el ]
and print_define conf base env p f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] p) alk
and print_apply conf base env p f el =
  match get_env f env with
  [ Vfun xl al ->
      let vl = List.map (eval_expr conf base env p) el in
      List.iter
        (fun a ->
           let a =
             loop a xl vl where rec loop a xl vl =
               match (xl, vl) with
               [ ([x :: xl], [v :: vl]) ->
                   loop (subst (subst_text x v) a) xl vl
               | ([], []) -> a
               | _ -> Atext "parse_error" ]
           in
           print_ast conf base env p a)
        al
  | _ -> Wserver.wprint ">%%%s???" f ]
and print_if conf base env p e alt ale =
  let al = if eval_bool_value conf base env p e then alt else ale in
  List.iter (print_ast conf base env p) al
and print_foreach conf base env p s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in
    (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env p sl with
  [ VVgen "" -> print_simple_foreach conf base env p al s
  | VVgen _ ->
      do Wserver.wprint "foreach ";
         List.iter (fun s -> Wserver.wprint "%s." s) sl;
         Wserver.wprint "%s???" s;
      return ()
  | VVcvar _ | VVdate _ _ | VVrelation _ _ | VVtitle _ _ | VVnone -> () ]
and print_simple_foreach conf base env p al s =
  let list =
    match s with
    [ "alias" -> Some p.aliases
    | "first_name_alias" -> Some p.first_names_aliases
    | "qualifier" -> Some p.qualifiers
    | "surname_alias" -> Some p.surnames_aliases
    | _ -> None ]
  in
  match list with
  [ Some list -> print_foreach_string conf base env p al list
  | None ->
      match s with
      [ "relation" -> print_foreach_relation conf base env p al p.rparents
      | "title" -> print_foreach_title conf base env p al p.titles
      | _ -> Wserver.wprint "foreach %s???" s ] ]
and print_foreach_string conf base env p al list =
  let _ = List.fold_left
    (fun cnt nn ->
       let env = [("item", Vstring nn) :: env] in
       let env = [("cnt", Vint cnt) :: env] in
       do List.iter (print_ast conf base env p) al; return cnt + 1)
    0 list
  in ()
and print_foreach_relation conf base env p al list =
  let _ = List.fold_left
    (fun cnt nn ->
       let env = [("cnt", Vint cnt) :: env] in
       do List.iter (print_ast conf base env p) al; return cnt + 1)
    1 list
  in ()
and print_foreach_title conf base env p al list =
  let _ = List.fold_left
    (fun cnt nn ->
       let env = [("cnt", Vint cnt) :: env] in
       do List.iter (print_ast conf base env p) al; return cnt + 1)
    1 list
  in ()
;

value interp_templ conf base p digest astl =
  let env = [("digest", Vstring digest)] in
  List.iter (print_ast conf base env p) astl
;

value print_update_ind conf base p digest =
  match p_getenv conf.env "m" with
  [ Some ("MRG_IND_OK" | "MRG_MOD_IND_OK")
  | Some ("MOD_IND" | "MOD_IND_OK")
  | Some ("ADD_IND" | "ADD_IND_OK") ->
      if p_getenv conf.env "opt" = Some "new" then
        let astl = Templ.input conf base "updind" in
        do html conf; interp_templ conf base p digest astl; return ()
      else
        print_update_ind_aux conf base p digest
  | _ -> incorrect_request conf ]
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
  print_update_ind conf base p ""
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      let digest = Update.digest_person p in
      print_update_ind conf base (string_person_of base p) digest
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.data.persons.get i in
      print_del1 conf base (string_person_of base p)
  | _ -> incorrect_request conf ]
;
