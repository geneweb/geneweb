(* camlp4r ./pa_html.cmo *)
(* $Id: update.ml,v 2.17 1999-07-28 15:49:36 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

exception ModErr;
type create = [ Create of sex and option date | Link ];
type key = (string * string * int * create);

value rec find_free_occ base f s i =
  match
    try Some (person_ht_find_unique base f s i) with [ Not_found -> None ]
  with
  [ Some _ -> find_free_occ base f s (i + 1)
  | None -> i ]
;

value has_children base p =
  List.exists
    (fun ifam ->
       let fam = foi base ifam in
       Array.length fam.children > 0)
    (Array.to_list p.family)
;

value infer_death conf birth =
  match birth with
  [ Some d ->
      let a = Gutil.annee (Gutil.temps_ecoule d conf.today) in
      if a > 120 then DeadDontKnowWhen
      else if a <= 80 then NotDead
      else DontKnowIfDead
  | None -> DontKnowIfDead ]
;

value print_same_name conf base p =
  let f = p_first_name base p in
  let s = p_surname base p in
  let ipl = Gutil.person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
         let p = poi base ip in
         if Name.strip_lower (p_first_name base p) = f
         && Name.strip_lower (p_surname base p) = s then
           [p :: pl]
         else pl)
      [] ipl
  in
  let pl = Sort.list (fun p1 p2 -> p1.occ < p2.occ) pl in
  match pl with
  [ [_] -> ()
  | _ ->
      do html_p conf;
         Wserver.wprint "%s:\n"
           (capitale (transl conf "persons having the same name"));
         tag "ul" begin
           List.iter
             (fun p ->
                do html_li conf;
                   stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p)
                   begin
                     Wserver.wprint "%s.%d %s"
                       (p_first_name base p) p.occ
                       (p_surname base p);
                   end;
                return ())
             pl;
         end;
      return () ]
;

value print_return conf =
  do html_p conf; return
  tag "form" "method=POST action=\"%s\"" conf.command begin
    List.iter
      (fun (x, v) ->
         Wserver.wprint "<input type=hidden name=%s value=\"%s\">\n" x
         (decode_varenv v))
      conf.env;
    Wserver.wprint "<input type=hidden name=return value=on>\n";
    Wserver.wprint "<input type=submit value=\"%s\">\n"
      (capitale (transl conf "back"));
  end
;

value print_err_unknown conf base (f, s, o) =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "error"))
  in
  do header conf title;
     Wserver.wprint "%s: <strong>%s.%d %s</strong>\n"
       (capitale (transl conf "unknown person")) f o s;
     print_return conf;
     trailer conf;
  return raise ModErr
;

value insert_string conf base s =
  try base.func.index_of_string s with
  [ Not_found ->
      let i = Adef.istr_of_int base.data.strings.len in
      do base.func.patch_string i s; return i ]
;

value update_misc_names_of_family base p =
  match p.sex with
  [ Male ->
      List.iter
        (fun ifam ->
           let fam = foi base ifam in
           let cpl = coi base ifam in
           List.iter
             (fun ip ->
                List.iter
                  (fun name ->
                     if not (List.memq ip (person_ht_find_all base name)) then
                       person_ht_add base name ip
                     else ())
                  (person_misc_names base (poi base ip)))
             [cpl.mother :: Array.to_list fam.children])
        (Array.to_list p.family)
  | _ -> () ]
;

value gen_someone_txt (p_first_name, p_surname) conf base p =
  p_first_name base p
  ^ (if p.occ = 0 then "" else "." ^ string_of_int p.occ) ^ " "
  ^ p_surname base p
;

value print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
    (p_surname base p)
;

value print_first_name conf base p =
  Wserver.wprint "%s%s" (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
;

value print_someone_strong conf base p =
  Wserver.wprint "<strong>%s%s %s</strong>" (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
    (p_surname base p)
;

value print_first_name_strong conf base p =
  Wserver.wprint "<strong>%s%s</strong>" (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
;

value print_src conf name field =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s" (capitale (transl_nth conf "source/sources" 0));
      end;
      tag "td" begin
        Wserver.wprint "<input name=%s size=40 maxlength=200%s>\n"
          name
          (match field with
           [ s when s <> "" ->
               " value=\"" ^ quote_escaped s ^ "\""
           | _ -> "" ]);
      end;
    end;
  end
;

value print_error conf base =
  fun
  [ AlreadyDefined p ->
      Wserver.wprint
        (fcapitale
           (ftransl conf "name \"%s.%d %s\" already used by %tthis person%t"))
        (p_first_name base p) p.occ
        (p_surname base p)
        (fun _ ->
           Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base p))
        (fun _ -> Wserver.wprint "</a>.")
  | OwnAncestor p ->
      do print_someone_strong conf base p;
         Wserver.wprint "\n%s"
           (transl conf "would be his/her own ancestor");
      return ()
  | BadSexOfMarriedPerson p ->
      Wserver.wprint "%s."
        (capitale (transl conf "cannot change sex of a married person")) ]
;

value print_someone_ref conf base p =
  Wserver.wprint "<a href=\"%s%s\">\n%s%s %s</a>"
    (commd conf) (acces conf base p)
    (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
    (p_surname base p)
;

value print_first_name_ref conf base p =
  Wserver.wprint "<a href=\"%s%s\">\n%s%s</a>"
    (commd conf) (acces conf base p)
    (p_first_name base p)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
;

value print_warning conf base =
  fun
  [ BirthAfterDeath p ->
      Wserver.wprint (ftransl conf "%t died before his/her birth")
        (fun _ ->
           do print_someone_strong conf base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | ChangedOrderOfChildren fam before ->
      let cpl = coi base fam.fam_index in
      let fath = poi base cpl.father in
      let moth = poi base cpl.mother in
      do Wserver.wprint "%s\n"
           (capitale (transl conf "changed order of children"));
         Wserver.wprint "%s\n" (transl_decline conf "of" "");
         print_someone_ref conf base fath;
         Wserver.wprint "\n%s\n" (transl conf "and");
         print_someone_ref conf base moth;
         Wserver.wprint "\n<ul>\n";
         html_li conf;
         Wserver.wprint "%s:\n" (capitale (transl conf "before"));
         Wserver.wprint "\n";
         tag "ul" begin
           Array.iter
             (fun ip ->
                let p = poi base ip in
                do html_li conf;
                   if p.surname = fath.surname then
                     print_first_name conf base p
                   else print_someone conf base p;
                   Date.afficher_dates_courtes conf base p;
                   Wserver.wprint "\n";
                return ())
             before;
         end;
         html_li conf;
         Wserver.wprint "%s:\n" (capitale (transl conf "after"));
         Wserver.wprint "\n";
         tag "ul" begin
           Array.iter
             (fun ip ->
                let p = poi base ip in
                do html_li conf;
                   if p.surname = fath.surname then
                     print_first_name_ref conf base p
                   else print_someone_ref conf base p;
                   Date.afficher_dates_courtes conf base p;
                   Wserver.wprint "\n";
                return ())
             fam.children;
         end;
         Wserver.wprint "</ul>";
      return ()
  | ChildrenNotInOrder fam elder x ->
      let cpl = coi base fam.fam_index in
      do Wserver.wprint
           (fcapitale
               (ftransl conf
                  "the following children of %t and %t are not in order"))
           (fun _ -> print_someone_strong conf base (poi base cpl.father))
           (fun _ -> print_someone_strong conf base (poi base cpl.mother));
         Wserver.wprint ":\n";
         Wserver.wprint "<ul>\n";
         html_li conf;
         print_first_name_strong conf base elder;
         Date.afficher_dates_courtes conf base elder;
         Wserver.wprint "\n";
         html_li conf;
         print_first_name_strong conf base x;
         Date.afficher_dates_courtes conf base x;
         Wserver.wprint "</ul>";
      return ()
  | DeadTooEarlyToBeFather father child ->
      Wserver.wprint
        (ftransl conf
    "%t is born more than 2 years after the death of his/her father %t")
        (fun _ ->
           do print_someone_strong conf base child;
              Date.afficher_dates_courtes conf base child;
           return ())
        (fun _ ->
           do print_someone_strong conf base father;
              Date.afficher_dates_courtes conf base father;
           return ())
  | MarriageDateAfterDeath p ->
      Wserver.wprint
        (fcapitale (ftransl conf "marriage of %t after his/her death"))
        (fun _ ->
           do print_someone_strong conf base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | MarriageDateBeforeBirth p ->
      Wserver.wprint
        (fcapitale (ftransl conf "marriage of %t before his/her birth"))
        (fun _ ->
           do print_someone_strong conf base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | MotherDeadAfterChildBirth mother child ->
      Wserver.wprint
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ ->
           do print_someone_strong conf base child;
              Date.afficher_dates_courtes conf base child;
           return ())
        (fun _ ->
           do print_someone_strong conf base mother;
              Date.afficher_dates_courtes conf base mother;
           return ())
  | ParentBornAfterChild p c ->
      do print_someone_strong conf base p;
         Wserver.wprint "\n%s\n"
           (transl conf "is born after his/her child");
         print_someone_strong conf base c;
      return ()
  | ParentTooYoung p a ->
      do print_someone_strong conf base p;
         Wserver.wprint "\n%s\n" (transl conf "is a very young parent");
         Wserver.wprint "(";
         Date.print_age conf a;
         Wserver.wprint ")";
      return ()
  | TitleDatesError p t ->
      Wserver.wprint
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ ->
           do print_someone_strong conf base p;
              Date.afficher_dates_courtes conf base p;
           return ())
        (fun _ ->
           Wserver.wprint "<strong>%s %s</strong> <em>%s-%s</em>"
             (sou base t.t_ident)
             (sou base t.t_place)
             (match Adef.od_of_codate t.t_date_start with
              [ Some d -> string_of_int (annee d)
              | _ -> "" ])
             (match Adef.od_of_codate t.t_date_end with
              [ Some d -> string_of_int (annee d)
              | _ -> "" ]))
  | YoungForMarriage p a ->
      do print_someone_strong conf base p;
         Wserver.wprint "\n";
         Wserver.wprint (ftransl conf "married at age %t")
           (fun _ -> Date.print_age conf a);
      return () ]
;

value print_warnings conf base wl =
  if wl = [] then ()
  else
    do html_p conf;
       Wserver.wprint "%s\n" (capitale (transl conf "warnings"));
       tag "ul" begin
         List.iter
           (fun w ->
              do html_li conf;
                 print_warning conf base w;
                 Wserver.wprint "\n";
              return ())
           wl;
       end;
    return ()
;

value error conf base x =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     print_error conf base x;
     Wserver.wprint "\n";
     print_return conf;
     trailer conf;
  return raise ModErr
;

value error_locked conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint
       (fcapitale
          (ftransl conf
             "the file is temporarily locked: please try again"));
     Wserver.wprint ".\n";
     trailer conf;
  return ()
;

value error_digest conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint
       (fcapitale
          (ftransl conf
             "\
the base has changed; do \"back\", \"reload\", and refill the form"));
     Wserver.wprint ".\n";
     trailer conf;
  return raise ModErr
;

value digest_person (p : person) = Iovalue.digest p;
value digest_family (fam : family) = Iovalue.digest fam;

value get var key env =
  match p_getenv env (var ^ "_" ^ key) with
  [ Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound") ]
;

value get_number var key env = p_getint env (var ^ "_" ^ key);

value bad_date conf d =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s:\n" (capitale (transl conf "incorrect date"));
     match d with
     [ {day = 0; month = 0; year = a} -> Wserver.wprint "%d" a
     | {day = 0; month = m; year = a} -> Wserver.wprint "%d/%d" m a
     | {day = j; month = m; year = a} -> Wserver.wprint "%d/%d/%d" j m a ];
     trailer conf;
  return raise ModErr
;

value reconstitute_date conf var =
  match get_number var "yyyy" conf.env with
  [ Some y ->
      let prec =
        match p_getenv conf.env (var ^ "_prec") with
        [ Some "about" -> About
        | Some "maybe" -> Maybe
        | Some "before" -> Before
        | Some "after" -> After
        | Some "oryear" ->
            match get_number var "oryear" conf.env with
            [ Some y -> OrYear y
            | None -> Sure ]
        | Some "yearint" ->
            match get_number var "oryear" conf.env with
            [ Some y -> YearInt y
            | None -> Sure ]
        | _ -> Sure ]
      in
      match get_number var "mm" conf.env with
      [ Some m ->
          match get_number var "dd" conf.env with
          [ Some d ->
              let d = {day = d; month = m; year = y; prec = prec} in
              if d.day >= 1 && d.day <= 31 && d.month >= 1
              && d.month <= 12 then
                Some d
              else bad_date conf d
          | None ->
              let d = {day = 0; month = m; year = y; prec = prec} in
              if d.month >= 1 && d.month <= 12 then Some d
              else bad_date conf d ]
      | None -> Some {day = 0; month = 0; year = y; prec = prec} ]
  | None -> None ]
;

value print_date conf base lab var d =
  do tag "tr" begin
       stag "td" begin Wserver.wprint "%s" lab; end;
       tag "td" begin
         Wserver.wprint "<input name=%s_dd size=2 maxlength=2%s>\n" var
           (match d with
            [ Some {day = d} when d <> 0 -> " value=" ^ string_of_int d
            | _ -> "" ]);
         Wserver.wprint "<input name=%s_mm size=2 maxlength=2%s>\n" var
           (match d with
            [ Some {month = m} when m <> 0 -> " value=" ^ string_of_int m
            | _ -> "" ]);
         Wserver.wprint "<input name=%s_yyyy size=5 maxlength=5%s>\n" var
           (match d with
            [ Some {year = y} -> " value=" ^ string_of_int y
            | _ -> "" ]);
       end;
       tag "td" begin
         tag "select" "name=%s_prec" var begin
           Wserver.wprint "<option%s>-\n"
             (match d with
              [ None -> " selected"
              | _ -> "" ]);
           Wserver.wprint "<option value=sure%s>&lt;- %s\n"
             (match d with [ Some {prec = Sure} -> " selected" | _ -> "" ])
             (capitale (transl conf "exact"));
           Wserver.wprint "<option value=about%s>&lt;- %s\n"
             (match d with [ Some {prec = About} -> " selected" | _ -> "" ])
             (capitale (transl conf "about (date)"));
           Wserver.wprint "<option value=maybe%s>&lt;- %s\n"
             (match d with [ Some {prec = Maybe} -> " selected" | _ -> "" ])
             (capitale (transl conf "maybe (date)"));
           Wserver.wprint "<option value=before%s>&lt;- %s\n"
             (match d with [ Some {prec = Before} -> " selected" | _ -> "" ])
             (capitale (transl conf "before (date)"));
           Wserver.wprint "<option value=after%s>&lt;- %s\n"
             (match d with [ Some {prec = After} -> " selected" | _ -> "" ])
             (capitale (transl conf "after (date)"));
           Wserver.wprint "<option value=oryear%s>&lt;- %s -&gt;\n"
             (match d with [ Some {prec = OrYear _} -> " selected" | _ -> "" ])
             (capitale (transl conf "or"));
           Wserver.wprint "<option value=yearint%s>&lt;- %s -&gt;\n"
             (match d with
              [ Some {prec = YearInt _} -> " selected"
              | _ -> "" ])
             (capitale (transl conf "between (date)"));
         end;
         Wserver.wprint "<input name=%s_oryear size=5 maxlength=5%s>\n" var
           (match d with
            [ Some {prec = OrYear y} -> " value=" ^ string_of_int y
            | Some {prec = YearInt y} -> " value=" ^ string_of_int y
            | _ -> "" ]);
       end;
     end;
  return ()
;

value print_parent_person conf base var (first_name, surname, occ, create) =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" begin
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
      tag "td" begin
        tag "select" "name=%s_p" var begin
          Wserver.wprint "<option value=link%s>%s\n"
            (if create = Link then " selected" else "")
            (capitale (transl conf "link"));
          Wserver.wprint "<option value=create%s>%s\n"
            (match create with [ Create _ _ -> " selected" | _ -> "" ])
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
          "<input name=%s_sn size=40 maxlength=200 value=\"%s\">"
          var surname;
      end;
    end;
  end
;

value print_create_conflict conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  let n =
    find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  do header conf title;
     print_error conf base (AlreadyDefined p);
     html_p conf;
     Wserver.wprint "<ul>\n";
     html_li conf;
     Wserver.wprint "%s: %d\n" (capitale (transl conf "first free number")) n;
     html_li conf;
     Wserver.wprint "%s\n"
       (capitale (transl conf "or use \"link\" instead of \"create\""));
     Wserver.wprint "</ul>\n";
     print_same_name conf base p;
     print_return conf;
     trailer conf;
  return raise ModErr
;

value add_misc_names_for_new_persons base new_persons =
  List.iter
    (fun p ->
       List.iter (fun n -> person_ht_add base n p.cle_index)
         (person_misc_names base p))
    new_persons
;

value insert_person conf base src new_persons (f, s, o, create) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  [ Create sex birth ->
      try
        if f = "?" || s = "?" then
          if o <= 0 || o >= base.data.persons.len then raise Not_found
          else
            let ip = Adef.iper_of_int o in
            let p = poi base ip in
            if p_first_name base p = f && p_surname base p = s then ip
            else raise Not_found
        else
          let ip = person_ht_find_unique base f s o in
          print_create_conflict conf base (poi base ip)
      with
      [ Not_found ->
          let o = if f = "?" || s = "?" then 0 else o in
          let ip = Adef.iper_of_int (base.data.persons.len) in
          let death = infer_death conf birth in
          let empty_string = insert_string conf base "" in
          let p =
            {first_name = insert_string conf base f;
             surname = insert_string conf base s;
             occ = o; image = empty_string;
             first_names_aliases = []; surnames_aliases = [];
             public_name = empty_string;
             nick_names = []; aliases = []; titles = [];
             rparents = []; rchildren = [];
             occupation = empty_string;
             sex = sex; access = IfTitles;
             birth = Adef.codate_of_od birth; birth_place = empty_string;
             birth_src = empty_string;
             baptism = Adef.codate_None; baptism_place = empty_string;
             baptism_src = empty_string;
             death = death; death_place = empty_string;
             death_src = empty_string;
             burial = UnknownBurial; burial_place = empty_string;
             burial_src = empty_string;
             family = [| |];
             notes = empty_string;
             psources = insert_string conf base src;
             cle_index = ip}
          and a =
            {parents = None;
             consang = Adef.fix (-1)}
          in
          do base.func.patch_person p.cle_index p;
             base.func.patch_ascend p.cle_index a;
             if f <> "?" && s <> "?" then
               do person_ht_add base (nominative (f ^ " " ^ s)) ip;
                  new_persons.val := [p :: new_persons.val];
               return ()
             else ();
          return ip ]
  | Link ->
      if f = "?" || s = "?" then
        if o < 0 || o >= base.data.persons.len then
          print_err_unknown conf base (f, s, o)
        else
          let ip = Adef.iper_of_int o in
          let p = poi base ip in
          if p_first_name base p = f && p_surname base p = s then ip
          else print_err_unknown conf base (f, s, o)
      else
        try person_ht_find_unique base f s o with
        [ Not_found -> print_err_unknown conf base (f, s, o) ] ]
;

value print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if p.occ == 0 then ""else "." ^ string_of_int p.occ)
    (p_surname base p)
;

value print_family_stuff conf base p a =
  tag "ul" begin
   let _ = List.fold_left
      (fun prev fi ->
         do match prev with
            [ Some prev_fi ->
                let cpl1 = coi base prev_fi in
                let cpl2 = coi base fi in
                do Wserver.wprint "\n";
                   html_li conf;
                   Wserver.wprint "<a href=\"%sm=SWI_FAM;i=%d;f=%d\">"
                     (commd conf) (Adef.int_of_iper p.cle_index)
                     (Adef.int_of_ifam fi);
                   Wserver.wprint "%s</a>\n" (capitale (transl conf "switch"));
                   if cpl1.father = cpl2.father && cpl1.mother = cpl2.mother
                   then
                     do html_li conf;
                        stag "a" "href=\"%sm=MRG_FAM;f1=%d;f2=%d\""
                          (commd conf) (Adef.int_of_ifam prev_fi)
                          (Adef.int_of_ifam fi)
                        begin
                          Wserver.wprint "%s"
                            (capitale (transl_decline conf "merge" ""));
                        end;
                        Wserver.wprint "\n";
                     return ()
                   else ();
                return ()
            | None -> () ];
         return
         let c = spouse p (coi base fi) in
         do Wserver.wprint "\n";
            html_li conf;
            Wserver.wprint "<a href=\"%sm=MOD_FAM;i=%d;ip=%d\">" (commd conf)
              (Adef.int_of_ifam fi) (Adef.int_of_iper p.cle_index);
            let s = transl_nth conf "family/families" 0 in
            Wserver.wprint "%s</a>\n"
              (capitale (transl_decline conf "modify" s));
            Wserver.wprint "\n<em>%s</em>\n"
              (transl_decline conf "with"
                 (gen_someone_txt raw_access conf base (poi base c)));
            Wserver.wprint "\n";
            html_li conf;
            Wserver.wprint "<a href=\"%sm=DEL_FAM;i=%d;ip=%d\">" (commd conf)
              (Adef.int_of_ifam fi) (Adef.int_of_iper p.cle_index);
            let s = transl_nth conf "family/families" 0 in
            Wserver.wprint "%s</a>\n"
              (capitale (transl_decline conf "delete" s));
            Wserver.wprint "\n<em>%s</em>\n"
              (transl_decline conf "with"
                 (gen_someone_txt raw_access conf base (poi base c)));
         return Some fi)
      None (Array.to_list p.family)
    in ();
    if (p_first_name base p = "?" || p_surname base p = "?")
    && (Array.length p.family <> 0 || a.parents <> None) then ()
    else
      let s = transl_nth conf "family/families" 0 in
      do Wserver.wprint "\n";
         html_li conf;
         Wserver.wprint "<a href=\"%sm=ADD_FAM;i=%d\">%s</a>\n"
           (commd conf) (Adef.int_of_iper p.cle_index)
           (capitale (transl_decline conf "add" s));
      return ();
  end
;

value print conf base p =
  let title h =
    do Wserver.wprint "%s" (capitale (transl conf "update"));
       if h then ()
       else
         let fn = p_first_name base p in
         let sn = p_surname base p in
         let occ =
           if fn = "?" || sn = "?" then Adef.int_of_iper p.cle_index
           else p.occ
         in
         do Wserver.wprint ": ";
            Wserver.wprint "%s.%d %s" fn occ sn;
         return ();
    return ()
  in
  let a = aoi base p.cle_index in
  do header conf title;
     Wserver.wprint "<ul>\n";
     html_li conf;
     Wserver.wprint "<a href=\"%sm=MOD_IND;i=%d\">%s</a>\n" (commd conf)
       (Adef.int_of_iper p.cle_index)
       (capitale (transl_decline conf "modify" ""));
     Wserver.wprint "\n";
     html_li conf;
     Wserver.wprint "<a href=\"%sm=DEL_IND;i=%d\">%s</a>\n"
       (commd conf) (Adef.int_of_iper p.cle_index)
       (capitale (transl_decline conf "delete" ""));
     if conf.can_send_image && sou base p.image = "" then
       do Wserver.wprint "\n";
          html_li conf;
          Wserver.wprint "<a href=\"%sm=SND_IMAGE;i=%d\">%s</a>\n"
            (commd conf) (Adef.int_of_iper p.cle_index)
            (capitale (transl_decline conf "send" (transl conf "image")));
          match auto_image_file conf base p with
          [ Some _ ->
              do Wserver.wprint "\n";
                 html_li conf;
                 Wserver.wprint "<a href=\"%sm=DEL_IMAGE;i=%d\">%s</a>\n"
                   (commd conf) (Adef.int_of_iper p.cle_index)
                   (capitale
                      (transl_decline conf "delete" (transl conf "image")));
              return ()
          | None -> () ];
       return ()
     else ();
     Wserver.wprint "</ul>\n";
     Wserver.wprint "\n";
     print_family_stuff conf base p a;
     if has_children base p then
       do Wserver.wprint "<p>\n";
          tag "ul" begin
            html_li conf;
            stag "a" "href=\"%sm=CHG_CHN;i=%d\"" (commd conf)
              (Adef.int_of_iper p.cle_index)
            begin
              Wserver.wprint "%s"
                (capitale (transl conf "change children's names"));
            end;
            Wserver.wprint "\n";
          end;
       return ()
     else ();
     match a.parents with
     [ Some _ -> ()
     | None ->
         if p_first_name base p = "?" || p_surname base p = "?" then ()
         else
           do Wserver.wprint "<ul>\n";
              html_li conf;
              let s = transl conf "parents" in              
              Wserver.wprint "<a href=\"%sm=ADD_PAR;i=%d\">%s</a>\n"
                (commd conf) (Adef.int_of_iper p.cle_index)
                (capitale (transl_decline conf "add" s));
              Wserver.wprint "</ul>\n";
           return () ];
     Wserver.wprint "\n";
     html_p conf;
     tag "ul" begin
       html_li conf;
       stag "a" "href=\"%sm=MRG;i=%d\"" (commd conf)
         (Adef.int_of_iper p.cle_index)
       begin
         Wserver.wprint "%s" (capitale (transl_decline conf "merge" ""));
       end;
     end;
     trailer conf;
  return ()
;
