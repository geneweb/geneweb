(* camlp4r ./pa_html.cmo *)
(* $Id: updateFam.ml,v 4.12 2001-06-15 15:25:30 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value bogus_family_index = Adef.ifam_of_int (-1);

value default_source conf =
  match p_getenv conf.env "dsrc" with
  [ Some s -> s
  | None -> "" ]
;

value person_key base ip =
  let p = poi base ip in
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip else p.occ
  in
  (first_name, surname, occ, Update.Link, "")
;

value string_family_of base fam cpl des =
  let sfam = Gutil.map_family_ps (person_key base) (sou base) fam in
  let scpl = Gutil.map_couple_p (person_key base) cpl in
  let sdes = Gutil.map_descend_p (person_key base) des in
  (sfam, scpl, sdes)
;

module Old = struct

value print_birth conf var create verbose =
  do {
    tag "td" begin Wserver.wprint "%s" (capitale (transl conf "birth")); end;
    tag "td" begin
      Wserver.wprint "<input name=%sb_yyyy size=5 maxlength=5%s>-\n" var
        (match create with
         [ Update.Create _ (Some (Some (Dgreg {year = y} _), _, _, _)) ->
             " value=" ^ string_of_int y
         | _ -> "" ]);
      Wserver.wprint "<input name=%sb_mm size=2 maxlength=2%s>-\n" var
        (match create with
         [ Update.Create _ (Some (Some (Dgreg {month = m} _), _, _, _))
           when m <> 0 ->
             " value=" ^ string_of_int m
         | _ -> "" ]);
      Wserver.wprint "<input name=%sb_dd size=2 maxlength=2%s>\n" var
        (match create with
         [ Update.Create _ (Some (Some (Dgreg {day = d} _), _, _, _))
           when d <> 0 ->
             " value=" ^ string_of_int d
         | _ -> "" ]);
    end;
    if verbose then
      tag "td" begin Wserver.wprint "%s" (capitale (transl conf "place")); end
    else ();
    tag "td" "colspan=2" begin
      Wserver.wprint "<input name=%sb_pl size=20 maxlength=200%s>\n" var
        (match create with
         [ Update.Create _ (Some (_, pl, _, _)) when pl <> "" ->
             " value=\"" ^ quote_escaped pl ^ "\""
         | _ -> "" ]);
    end
  }
;

value print_death conf var create verbose =
  do {
    tag "td" begin Wserver.wprint "%s" (capitale (transl conf "death")); end;
    tag "td" begin
      Wserver.wprint "<input name=%sd_yyyy size=5 maxlength=5%s>-\n" var
        (match create with
         [ Update.Create _ (Some (_, _, Some (Dgreg {year = y} _), _)) ->
             " value=" ^ string_of_int y
         | _ -> "" ]);
      Wserver.wprint "<input name=%sd_mm size=2 maxlength=2%s>-\n" var
        (match create with
         [ Update.Create _ (Some (_, _, Some (Dgreg {month = m} _), _))
           when m <> 0 ->
             " value=" ^ string_of_int m
         | _ -> "" ]);
      Wserver.wprint "<input name=%sd_dd size=2 maxlength=2%s>\n" var
        (match create with
         [ Update.Create _ (Some (_, _, Some (Dgreg {day = d} _), _))
           when d <> 0 ->
             " value=" ^ string_of_int d
         | _ -> "" ]);
    end;
    if verbose then
      tag "td" begin Wserver.wprint "%s" (capitale (transl conf "place")); end
    else ();
    tag "td" "colspan=2" begin
      Wserver.wprint "<input name=%sd_pl size=20 maxlength=200%s>\n" var
        (match create with
         [ Update.Create _ (Some (_, _, _, pl)) when pl <> "" ->
             " value=\"" ^ quote_escaped pl ^ "\""
         | _ -> "" ]);
    end
  }
;

value
  print_parent_person conf base var (first_name, surname, occ, create, _) =
  do {
    tag "table" "border=1" begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s"
            (capitale (transl_nth conf "first name/first names" 0));
        end;
        tag "td" begin
          Wserver.wprint "<input name=%s_fn size=23 maxlength=200" var;
          Wserver.wprint " value=\"%s\">" (quote_escaped first_name);
        end;
        tag "td" "align=right" begin
          let s = capitale (transl conf "number") in
          Wserver.wprint "%s" s;
        end;
        tag "td" begin
          Wserver.wprint "<input name=%s_occ size=5 maxlength=8%s>" var
            (if occ == 0 then "" else " value=" ^ string_of_int occ);
        end;
        tag "td" begin
          tag "select" "name=%s_p" var begin
            Wserver.wprint "<option value=link%s>%s\n"
              (if create = Update.Link then " selected" else "")
              (capitale (transl conf "link"));
            Wserver.wprint "<option value=create%s>%s\n"
              (match create with
               [ Update.Create _ _ -> " selected"
               | _ -> "" ])
              (capitale (transl conf "create"));
          end;
        end;
      end;
      Wserver.wprint "\n";
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s"
            (capitale (transl_nth conf "surname/surnames" 0));
        end;
        tag "td" "colspan=4" begin
          Wserver.wprint
            "<input name=%s_sn size=40 maxlength=200 value=\"%s\">" var
            (quote_escaped surname);
        end;
      end;
    end;
    tag "table" "border=1" begin
      tag "tr" begin print_birth conf var create True; end;
      tag "tr" begin print_death conf var create True; end;
    end
  }
;

value print_child_person conf base var (first_name, surname, occ, create, _) =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" "colspan=2" begin
        Wserver.wprint "<input name=%s_fn size=23 maxlength=200" var;
        Wserver.wprint " value=\"%s\">" (quote_escaped first_name);
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
        Wserver.wprint "%s" (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=4" begin
        Wserver.wprint "<input name=%s_sn size=40 maxlength=200 value=\"%s\">"
          var (quote_escaped surname);
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
      print_birth conf var create False;
    end;
    Wserver.wprint "\n";
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "<input type=radio name=%s_sex value=N%s>?\n" var
          (match create with
           [ Update.Create Neuter _ -> " checked"
           | _ -> "" ]);
        Wserver.wprint "<input type=radio name=%s_sex value=M%s>%s\n" var
          (match create with
           [ Update.Create Male _ -> " checked"
           | _ -> "" ])
          (transl_nth conf "M/F" 0);
        Wserver.wprint "<input type=radio name=%s_sex value=F%s>%s\n" var
          (match create with
           [ Update.Create Female _ -> " checked"
           | _ -> "" ])
          (transl_nth conf "M/F" 1);
      end;
      print_death conf var create False;
    end;
  end
;

value print_father conf base cpl =
  do {
    stag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "him/her" 0));
    end;
    Wserver.wprint "\n";
    print_parent_person conf base "him" cpl.father;
    Wserver.wprint "\n"
  }
;

value print_mother conf base cpl =
  do {
    stag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "him/her" 1));
    end;
    Wserver.wprint "\n";
    print_parent_person conf base "her" cpl.mother;
    Wserver.wprint "\n"
  }
;

value print_insert_witness conf base cnt =
  tag "table" "border=1" begin
    tag "tr" begin
      let var = "ins_witn" ^ string_of_int cnt in
      tag "td" begin
        let s = transl_nth conf "witness/witnesses" 0 in
        Wserver.wprint "%s <input type=checkbox name=%s value=on>"
          (capitale (transl_decline conf "insert" s)) var;
      end;
    end;
  end
;

value print_witness conf base var key =
  Update.print_simple_person conf base var key
;

value print_witnesses conf base fam =
  let witnesses =
    match Array.to_list fam.witnesses with
    [ [] ->
        let t = ("", "", 0, Update.Create Neuter None, "") in
        [t; t]
    | ipl -> ipl ]
  in
  do {
    tag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "witness/witnesses" 1));
    end;
    Wserver.wprint "\n";
    print_insert_witness conf base 0;
    let _ =
      List.fold_left
        (fun cnt n ->
           do {
             print_witness conf base ("witn" ^ string_of_int cnt) n;
             print_insert_witness conf base cnt;
             cnt + 1
           })
        1 witnesses
    in
    ()
  }
;

value print_marriage conf base fam =
  do {
    tag "h4" begin
      Wserver.wprint "%s"
        (capitale (nominative (transl_nth conf "marriage/marriages" 0)));
    end;
    Wserver.wprint "\n";
    Wserver.wprint "<input type=radio name=mrel value=marr%s>"
      (match fam.relation with
       [ Married -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "married"));
    Wserver.wprint "<input type=radio name=mrel value=not_marr%s>"
      (match fam.relation with
       [ NotMarried -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "not married"));
    Wserver.wprint "<input type=radio name=mrel value=engaged%s>"
      (match fam.relation with
       [ Engaged -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "engaged"));
    Wserver.wprint "<input type=radio name=mrel value=nsck%s>"
      (match fam.relation with
       [ NoSexesCheck -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "no sexes check"));
    tag "table" "border=1" begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s\n" (capitale (transl conf "place"));
        end;
        tag "td" begin
          Wserver.wprint
            "<input name=marriage_place size=40 maxlength=200%s>\n"
            (if fam.marriage_place = "" then ""
             else " value=\"" ^ quote_escaped fam.marriage_place ^ "\"");
        end;
      end;
    end;
    Update.print_date conf base (capitale (transl conf "date")) "marriage"
      (Adef.od_of_codate fam.marriage);
    Update.print_src conf "marriage_src" fam.marriage_src
  }
;

value print_divorce conf base fam =
  do {
    stag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "divorce" 0));
    end;
    Wserver.wprint "\n";
    Wserver.wprint "<input type=radio name=divorce value=not_divorced%s>"
      (match fam.divorce with
       [ NotDivorced -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "not divorced"));
    Wserver.wprint "<input type=radio name=divorce value=divorced%s>"
      (match fam.divorce with
       [ Divorced _ -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "divorced"));
    Wserver.wprint "<input type=radio name=divorce value=separated%s>"
      (match fam.divorce with
       [ Separated -> " checked"
       | _ -> "" ]);
    Wserver.wprint "%s\n" (capitale (transl conf "separated"));
    Update.print_date conf base (capitale (transl conf "date")) "divorce"
      (match fam.divorce with
       [ Divorced d -> Adef.od_of_codate d
       | _ -> None ])
  }
;

value print_insert_child conf base cnt =
  tag "table" "border=1" begin
    tag "tr" begin
      let var = "ins_ch" ^ string_of_int cnt in
      tag "td" begin
        let s = transl_nth conf "child/children" 0 in
        let sn = "<input name=" ^ var ^ "_n size=1 maxlength=1 value=1> " in
        Wserver.wprint "%s <input type=checkbox name=%s value=on>"
          (capitale (transl_decline conf "insert" (sn ^ s))) var;
      end;
    end;
  end
;

value print_child conf base cnt n =
  do {
    Wserver.wprint "\n";
    html_li conf;
    print_child_person conf base ("ch" ^ string_of_int cnt) n;
    html_li conf;
    print_insert_child conf base cnt
  }
;

value print_children conf base des cpl force_children_surnames =
  let children =
    match Array.to_list des.children with
    [ [] -> [("", "", 0, Update.Create Neuter None, "")]
    | ipl ->
        let (_, father_surname, _, _, _) = cpl.father in
        List.map
          (fun (first_name, surname, occ, create, var) ->
             let surname =
               if not force_children_surnames && surname = father_surname then
                 ""
               else surname
             in
             (first_name, surname, occ, create, var))
          ipl ]
  in
  do {
    stag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "child/children" 1));
    end;
    Wserver.wprint "\n";
    tag "ul" begin
      html_li conf;
      print_insert_child conf base 0;
      let _ =
        List.fold_left
          (fun cnt n -> do { print_child conf base cnt n; cnt + 1 }) 1
          children
      in
      ();
    end
  }
;

value print_comment conf base fam =
  do {
    stag "h4" begin
      Wserver.wprint "%s" (capitale (nominative (transl conf "comment")));
    end;
    Wserver.wprint "\n";
    tag "table" "border=1" begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "<input name=comment size=50 maxlength=200%s>\n"
            (match fam.comment with
             [ s when s <> "" -> " value=\"" ^ quote_escaped s ^ "\""
             | _ -> "" ]);
        end;
      end;
    end
  }
;

value print_source conf base field =
  let p_field =
    match p_getenv conf.env "psrc" with
    [ Some s -> s
    | None -> default_source conf ]
  in
  do {
    tag "h4" begin
      Wserver.wprint "%s" (capitale (transl_nth conf "source/sources" 0));
    end;
    Wserver.wprint "\n";
    tag "table" "border=1" begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s"
            (nominative (capitale (transl_nth conf "person/persons" 1)));
        end;
        tag "td" begin
          Wserver.wprint "<input name=psrc size=50 maxlength=200%s>\n"
            (if p_field = "" then ""
             else " value=\"" ^ quote_escaped p_field ^ "\"");
        end;
      end;
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "%s"
            (capitale (nominative (transl_nth conf "family/families" 0)));
        end;
        tag "td" begin
          Wserver.wprint "<input name=src size=50 maxlength=200%s>\n"
            (if field = "" then ""
             else " value=\"" ^ quote_escaped field ^ "\"");
          Wserver.wprint "<input type=checkbox name=rdsrc value=on%s>\n"
            (match p_getenv conf.env "rdsrc" with
             [ Some "on" -> " checked"
             | _ -> "" ]);
        end;
      end;
    end
  }
;

value print_family conf base fam cpl des force_children_surnames =
  do {
    print_father conf base cpl;
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
    print_children conf base des cpl force_children_surnames;
    Wserver.wprint "\n";
    print_source conf base fam.fsources;
    match p_getenv conf.env "origin_file" with
    [ Some "on" ->
        Wserver.wprint "<input name=origin_file size=50 maxlength=200%s>\n"
          (if fam.origin_file = "" then ""
           else " value=" ^ quote_escaped fam.origin_file)
    | _ -> () ]
  }
;

value print_add1 conf base fam cpl des force_children_surnames =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "add" s))
  in
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      match p_getenv conf.env "ip" with
      [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
      | None -> () ];
      Wserver.wprint "<input type=hidden name=m value=ADD_FAM_OK>\n";
      print_family conf base fam cpl des force_children_surnames;
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value merge_call conf =
  do {
    Wserver.wprint "<input type=hidden name=m value=MRG_MOD_FAM_OK>\n";
    match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
    [ (Some i1, Some i2) ->
        do {
          Wserver.wprint "<input type=hidden name=ini1 value=%d>\n" i1;
          Wserver.wprint "<input type=hidden name=ini2 value=%d>\n" i2;
          ()
        }
    | _ -> () ];
    match p_getint conf.env "i2" with
    [ Some i2 -> Wserver.wprint "<input type=hidden name=i2 value=%d>\n" i2
    | _ -> () ]
  }
;

value print_mod1 conf base fam cpl des digest =
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
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      match p_getenv conf.env "m" with
      [ Some "MRG_MOD_FAM_OK" -> merge_call conf
      | _ -> Wserver.wprint "<input type=hidden name=m value=MOD_FAM_OK>\n" ];
      Wserver.wprint "<input type=hidden name=i value=%d>\n"
        (Adef.int_of_ifam fam.fam_index);
      match p_getenv conf.env "ip" with
      [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
      | None -> () ];
      Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
      print_family conf base fam cpl des False;
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf
  }
;

value print_merge1 conf base fam des fam2 digest =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s # %d" (capitale (transl_decline conf "merge" s))
      (Adef.int_of_ifam fam.fam_index)
  in
  let cpl = Gutil.map_couple_p (person_key base) (coi base fam.fam_index) in
  do {
    header conf title;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=MRG_MOD_FAM_OK>\n";
      Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
      Wserver.wprint "<input type=hidden name=i value=%d>\n"
        (Adef.int_of_ifam fam.fam_index);
      Wserver.wprint "<input type=hidden name=i2 value=%d>\n"
        (Adef.int_of_ifam fam2.fam_index);
      match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
      [ (Some i1, Some i2) ->
          do {
            Wserver.wprint "<input type=hidden name=ini1 value=%d>\n" i1;
            Wserver.wprint "<input type=hidden name=ini2 value=%d>\n" i2;
          }
      | _ -> () ];
      match p_getenv conf.env "ip" with
      [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
      | None -> () ];
      Wserver.wprint "\n";
      print_family conf base fam cpl des False;
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<input type=submit value=Ok>\n";
    end;
    Wserver.wprint "\n";
    trailer conf;
  }
;

end;

(* Interpretation of template file 'updfam.txt' *)

type ast =
  Templ.ast ==
    [ Atext of string
    | Avar of string and list string
    | Atransl of bool and string and char
    | Awid_hei of string
    | Aif of ast_expr and list ast and list ast
    | Aforeach of string and list string and list ast
    | Adefine of string and list string and list ast and list ast
    | Aapply of string and list ast_expr ]
and ast_expr =
  Templ.ast_expr ==
    [ Eor of ast_expr and ast_expr
    | Eand of ast_expr and ast_expr
    | Eop of string and ast_expr and ast_expr
    | Enot of ast_expr
    | Estr of string
    | Eint of string
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
  | VVcreate of Update.create and string
  | VVdate of option date and string
  | VVcvar of string
  | VVind of Update.key and string
  | VVnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  Wserver.wprint ">%s<p>\n" ("UpdateFam." ^ func ^ ": not impl " ^ desc)
;

(* string values *)

value get_create (fn, sn, oc, create, var) = create;

value rec eval_variable conf base env ((fam, cpl, des) as fcd) =
  fun
  [ ["father"; s] -> VVind cpl.father s
  | ["father"; "create"; s] -> VVcreate (get_create cpl.father) s
  | ["mother"; s] -> VVind cpl.mother s
  | ["mother"; "create"; s] -> VVcreate (get_create cpl.mother) s
  | ["marriage"; s] -> VVdate (Adef.od_of_codate fam.marriage) s
  | ["divorce"; s] ->
      let d =
        match fam.divorce with
        [ Divorced d -> Adef.od_of_codate d
        | _ -> None ]
      in
      VVdate d s
  | ["witness"; s] ->
        match get_env "cnt" env with
        [ Vint i ->
            let i = i - 1 in
            if i >= 0 && i < Array.length fam.witnesses then
              VVind fam.witnesses.(i) s
            else if i >= 0 && i < 2 && Array.length fam.witnesses = 0 then
              VVind ("", "", 0, Update.Create Neuter None, "") s
            else VVnone
        | _ -> VVnone ]
  | ["child" :: sl] ->
        let r =
          match get_env "cnt" env with
          [ Vint i ->
              let i = i - 1 in
              if i >= 0 && i < Array.length des.children then
                Some des.children.(i)
              else if i >= 0 && i < 1 && Array.length des.children = 0 then
                Some ("", "", 0, Update.Create Neuter None, "")
              else None
          | _ -> None ]
        in
        match (r, sl) with
        [ (Some ind, [s]) -> VVind ind s
        | (Some ind, ["create"; s]) -> VVcreate (get_create ind) s
        | _ -> VVnone ]
  | [] -> VVgen ""
  | [s] ->
      let v = extract_var "cvar_" s in if v <> "" then VVcvar v else VVgen s
  | [s :: sl] -> VVnone ]
;

value eval_relation_kind =
  fun
  [ Married -> "marr"
  | NotMarried -> "not_marr"
  | Engaged -> "engaged"
  | NoSexesCheck -> "nsck" ]
;

value eval_divorce =
  fun
  [ Divorced _ -> "divorced"
  | NotDivorced -> "not_divorced"
  | Separated -> "separated" ]
;

value eval_string_env var env =
  match get_env var env with
  [ Vstring x -> quote_escaped x
  | _ -> "" ]
;

value eval_int_env var env =
  match get_env var env with
  [ Vint x -> string_of_int x
  | _ -> "" ]
;

value try_eval_gen_variable conf base env ((fam, cpl, des) as fcd) =
  fun
  [ "cnt" -> eval_int_env "cnt" env
  | "comment" -> quote_escaped fam.comment
  | "digest" -> eval_string_env "digest" env
  | "divorce" -> eval_divorce fam.divorce
  | "mrel" -> eval_relation_kind fam.relation
  | "marriage_place" -> quote_escaped fam.marriage_place
  | "marriage_src" -> quote_escaped fam.marriage_src
  | "fsources" -> quote_escaped fam.fsources
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value eval_key_variable (fn, sn, oc, create, var) =
  fun
  [ "first_name" -> quote_escaped fn
  | "occ" -> if oc = 0 then "" else string_of_int oc
  | "surname" -> quote_escaped sn
  | "create" -> if create <> Update.Link then "create" else "link"
  | s -> ">%" ^ s ^ "???" ]
;

value eval_create_variable c =
  fun
  [ "birth_year" ->
      match c with
      [ Update.Create _ (Some (Some (Dgreg {year = y} _), _, _, _)) ->
          string_of_int y
      | _ -> "" ]
  | "birth_month" ->
      match c with
      [ Update.Create _ (Some (Some (Dgreg {month = m} _), _, _, _))
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "birth_day" ->
      match c with
      [ Update.Create _ (Some (Some (Dgreg {day = d} _), _, _, _))
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "birth_place" ->
      match c with
      [ Update.Create _ (Some (_, pl, _, _)) -> quote_escaped pl
      | _ -> "" ]
  | "death_year" ->
      match c with
      [ Update.Create _ (Some (_, _, Some (Dgreg {year = y} _), _)) ->
          string_of_int y
      | _ -> "" ]
  | "death_month" ->
      match c with
      [ Update.Create _ (Some (_, _, Some (Dgreg {month = m} _), _))
        when m <> 0 ->
          string_of_int m
      | _ -> "" ]
  | "death_day" ->
      match c with
      [ Update.Create _ (Some (_, _, Some (Dgreg {day = d} _), _))
        when d <> 0 ->
          string_of_int d
      | _ -> "" ]
  | "death_place" ->
      match c with
      [ Update.Create _ (Some (_, _, _, pl)) -> quote_escaped pl
      | _ -> "" ]
  | s -> ">%" ^ s ^ "???" ]
;

value eval_expr conf base env p =
  fun
  [ Estr s -> s
  | Evar s [] ->
      try try_eval_gen_variable conf base env p s with
      [ Not_found -> ">" ^ s ^ "???" ]
  | Etransl upp s c -> Templ.eval_transl conf base env upp s c
  | _ -> ">parse_error" ]
;

(* bool values *)

value eval_gen_bool_variable conf base env fcd =
  fun
  [ s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv conf.env v with
        [ Some "" | None -> False
        | _ -> True ]
      else do { Wserver.wprint ">%%%s???" s; False } ]
;

value eval_bool_variable conf base env fcd s sl =
  match eval_variable conf base env fcd [s :: sl] with
  [ VVgen s -> eval_gen_bool_variable conf base env fcd s
  | VVcreate _ _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVdate _ _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVcvar _ -> do { Wserver.wprint ">%%%s???" s; False }
  | VVind _ _ -> do { Wserver.wprint ">%%VVind???"; False }
  | VVnone -> do { Wserver.wprint ">%%%s???" s; False } ]
;

value eval_bool_value conf base env fcd =
  let rec bool_eval =
    fun
    [ Eor e1 e2 -> bool_eval e1 || bool_eval e2
    | Eand e1 e2 -> bool_eval e1 && bool_eval e2
    | Eop op e1 e2 ->
        match op with
        [ "=" -> string_eval e1 = string_eval e2
        | "!=" -> string_eval e1 <> string_eval e2
        | _ -> do { Wserver.wprint "op %s???" op; False } ]
    | Enot e -> not (bool_eval e)
    | Evar s sl -> eval_bool_variable conf base env fcd s sl
    | Estr s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Eint s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Etransl _ s _ -> do { Wserver.wprint "[%s]???" s; False } ]
  and string_eval =
    fun
    [ Estr s -> s
    | Evar s sl ->
        try
          match eval_variable conf base env fcd [s :: sl] with
          [ VVgen s -> try_eval_gen_variable conf base env fcd s
          | VVcreate c s -> do { Wserver.wprint ">%%%s???" s; "" }
          | VVdate od s -> Templ.eval_date_variable od s
          | VVcvar s -> do { Wserver.wprint ">%%%s???" s; "" }
          | VVind pk s -> eval_key_variable pk s
          | VVnone -> do { Wserver.wprint ">%%%s???" s; "" } ]
        with
        [ Not_found -> do { Wserver.wprint ">%%%s???" s; "" } ]
    | x -> do { Wserver.wprint "val???"; "" } ]
  in
  bool_eval
;

(* print *)

value print_variable conf base env fcd sl =
  match eval_variable conf base env fcd sl with
  [ VVcreate c s -> Wserver.wprint "%s" (eval_create_variable c s)
  | VVdate od s -> Wserver.wprint "%s" (Templ.eval_date_variable od s)
  | VVgen s ->
      try Wserver.wprint "%s" (try_eval_gen_variable conf base env fcd s) with
      [ Not_found -> Templ.print_variable conf base s ]
  | VVind pk s -> Wserver.wprint "%s" (eval_key_variable pk s)
  | VVnone ->
      do {
        Wserver.wprint ">%%";
        list_iter_first
          (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
          sl;
        Wserver.wprint "???";
      }
  | x -> not_impl "print_variable" x ]
;

value rec print_ast conf base env fcd =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n ->
      Wserver.wprint "%s" (Templ.eval_transl conf base env upp s n)
  | Avar s sl -> print_variable conf base env fcd [s :: sl]
  | Aif e alt ale -> print_if conf base env fcd e alt ale
  | Aforeach s sl al -> print_foreach conf base env fcd s sl al
  | Adefine f xl al alk -> print_define conf base env fcd f xl al alk
  | Aapply f el -> print_apply conf base env fcd f el
  | x -> not_impl "print_ast" x ]
and print_define conf base env fcd f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] fcd) alk
and print_apply conf base env fcd f el =
  match get_env f env with
  [ Vfun xl al ->
      let vl = List.map (eval_expr conf base env fcd) el in
      List.iter
        (fun a ->
           let a =
             loop a xl vl where rec loop a xl vl =
               match (xl, vl) with
               [ ([x :: xl], [v :: vl]) ->
                   loop (Templ.subst (Templ.subst_text x v) a) xl vl
               | ([], []) -> a
               | _ -> Atext "parse_error" ]
           in
           print_ast conf base env fcd a)
        al
  | _ -> Wserver.wprint ">%%%s???" f ]
and print_if conf base env fcd e alt ale =
  let al = if eval_bool_value conf base env fcd e then alt else ale in
  List.iter (print_ast conf base env fcd) al
and print_foreach conf base env fcd s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env fcd sl with
  [ VVgen "" -> print_simple_foreach conf base env fcd al s
  | VVgen _ ->
      do {
        Wserver.wprint "foreach ";
        List.iter (fun s -> Wserver.wprint "%s." s) sl;
        Wserver.wprint "%s???" s;
      }
  | VVcvar _ | VVdate _ _ | VVind _ _ | VVcreate _ _ | VVnone -> () ]
and print_simple_foreach conf base env ((fam, cpl, des) as fcd) al s =
  match s with
  [ "child" -> print_foreach_child conf base env fcd al des.children s
  | "witness" -> print_foreach_witness conf base env fcd al fam.witnesses s
  | _ -> Wserver.wprint "foreach %s???" s ]
and print_foreach_child conf base env fcd al arr lab =
  for i = 0 to max 1 (Array.length arr) - 1 do {
    let env = [("cnt", Vint (i + 1)) :: env] in
    List.iter (print_ast conf base env fcd) al
  }
and print_foreach_witness conf base env fcd al arr lab =
  for i = 0 to max 2 (Array.length arr) - 1 do {
    let env = [("cnt", Vint (i + 1)) :: env] in
    List.iter (print_ast conf base env fcd) al
  }
;

value interp_templ conf base fcd digest astl =
  let env = [("digest", Vstring digest)] in
  List.iter (print_ast conf base env fcd) astl
;

value print_update_fam conf base fcd digest =
  match p_getenv conf.env "m" with
  [ Some ("ADD_FAM" | "ADD_PAR" | "MOD_FAM" | "MOD_FAM_OK") ->
      let astl = Templ.input conf base "updfam" in
      do { html conf; interp_templ conf base fcd digest astl }
  | _ -> incorrect_request conf ]
;

value print_add1 conf base fam cpl des force_children_surnames =
  if p_getenv conf.env "updfam" = Some "on" then
    print_update_fam conf base (fam, cpl, des) ""
  else
    Old.print_add1 conf base fam cpl des force_children_surnames
;

value print_mod1 conf base fam cpl des digest =
  if p_getenv conf.env "updfam" = Some "on" then
    print_update_fam conf base (fam, cpl, des) digest
  else Old.print_mod1 conf base fam cpl des digest
;

value print_merge1 conf base fam des fam2 digest =
  if p_getenv conf.env "updfam" = Some "on" then
    let cpl = Gutil.map_couple_p (person_key base) (coi base fam.fam_index) in
    print_update_fam conf base (fam, cpl, des) digest
  else
    Old.print_merge1 conf base fam des fam2 digest
;

value print_del1 conf base fam =
  let title _ =
    let s = transl_nth conf "family/families" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n";
    tag "form" "method=POST action=\"%s\"" conf.command begin
      Util.hidden_env conf;
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
    trailer conf
  }
;

value print_swi1 conf base p fam1 fam2 =
  let title _ =
    Wserver.wprint "%s" (capitale (transl_decline conf "switch" ""))
  in
  let cpl1 = coi base fam1.fam_index in
  let cpl2 = coi base fam2.fam_index in
  do {
    header conf title;
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
      Util.hidden_env conf;
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
    trailer conf
  }
;

value print_add conf base =
  let (fath, moth) =
    match p_getint conf.env "ip" with
    [ Some i ->
        let p = base.data.persons.get i in
        let fath =
          if p.sex = Male ||
             p.sex = Neuter && p_getenv conf.env "sex" = Some "M" then
            person_key base p.cle_index
          else ("", "", 0, Update.Create Male None, "")
        in
        let moth =
          if p.sex = Female ||
             p.sex = Neuter && p_getenv conf.env "sex" = Some "F" then
            person_key base p.cle_index
          else ("", "", 0, Update.Create Female None, "")
        in
        (fath, moth)
    | None ->
        (("", "", 0, Update.Create Male None, ""),
         ("", "", 0, Update.Create Female None, "")) ]
  in
  let fam =
    {marriage = Adef.codate_None; marriage_place = ""; marriage_src = "";
     witnesses = [| |]; relation = Married; divorce = NotDivorced;
     comment = ""; origin_file = ""; fsources = default_source conf;
     fam_index = bogus_family_index}
  and cpl = {father = fath; mother = moth}
  and des = {children = [| |]} in
  print_add1 conf base fam cpl des False
;

value print_add_parents conf base =
  match p_getint conf.env "ip" with
  [ Some i ->
      let p = base.data.persons.get i in
      let fam =
        {marriage = Adef.codate_None; marriage_place = ""; marriage_src = "";
         witnesses = [| |]; relation = Married; divorce = NotDivorced;
         comment = ""; origin_file = ""; fsources = default_source conf;
         fam_index = bogus_family_index}
      and cpl =
        {father = ("", sou base p.surname, 0, Update.Create Neuter None, "");
         mother = ("", "", 0, Update.Create Neuter None, "")}
      and des =
        {children =
           [| (sou base p.first_name, sou base p.surname, p.occ, Update.Link,
               "") |]}
      in
      print_add1 conf base fam cpl des True
  | _ -> incorrect_request conf ]
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      let cpl = coi base (Adef.ifam_of_int i) in
      let des = doi base (Adef.ifam_of_int i) in
      let (sfam, scpl, sdes) = string_family_of base fam cpl des in
      let digest = Update.digest_family fam cpl des in
      print_mod1 conf base sfam scpl sdes digest
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
  [ (Some ip, Some ifam) ->
      let u = base.data.unions.get ip in
      match
        find_families (Adef.ifam_of_int ifam) (Array.to_list u.family)
      with
      [ Some (ifam1, ifam2) ->
          let p = base.data.persons.get ip in
          print_swi1 conf base p (foi base ifam1) (foi base ifam2)
      | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
