(* camlp4r ./pa_html.cmo *)
(* $Id: update.ml,v 1.1 1998-09-01 14:32:06 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

exception ModErr;

value rec find_free_occ base f s i =
  match
    try Some (person_ht_find_unique base f s i) with [ Not_found -> None ]
  with
  [ Some _ -> find_free_occ base f s (i + 1)
  | None -> i ]
;

value print_same_name conf base p =
  let f = sou base p.first_name in
  let s = sou base p.surname in
  let ipl = Gutil.person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
         let p = poi base ip in
         if Name.strip_lower (sou base p.first_name) = f
         && Name.strip_lower (sou base p.surname) = s then
           [p :: pl]
         else pl)
      [] ipl
  in
  let pl = Sort.list (fun p1 p2 -> p1.occ < p2.occ) pl in
  match pl with
  [ [_] -> ()
  | _ ->
      do Wserver.wprint "<p>%s:\n"
           (capitale (transl conf "persons having the same name"));
         tag "ul" begin
           List.iter
             (fun p ->
                do Wserver.wprint "<li>\n";
                   stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p)
                   begin
                     Wserver.wprint "%s.%d %s" (sou base p.first_name) p.occ
                       (sou base p.surname);
                   end;
                return ())
             pl;
         end;
      return () ]
;

value insert_string conf base s =
  try base.index_of_string s with
  [ Not_found ->
      let i = Adef.istr_of_int base.strings.len in
      do base.patch_string i s; return i ]
;

value update_misc_names_of_family base p =
  match p.sexe with
  [ Masculin ->
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

value print_someone base p =
  Wserver.wprint "<strong>%s%s %s</strong>" (sou base p.first_name)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
    (sou base p.surname)
;

value print_first_name base p =
  Wserver.wprint "<strong>%s%s</strong>" (sou base p.first_name)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
;

value print_error conf base =
  fun
  [ AlreadyDefined p ->
      Wserver.wprint
        (fcapitale
           (ftransl conf "name \"%s.%d %s\" already used by %tthis person%t"))
        (sou base p.first_name) p.occ (sou base p.surname)
        (fun _ ->
           Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base p))
        (fun _ -> Wserver.wprint "</a>.")
  | OwnAncestor p ->
      do print_someone base p;
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
    (sou base p.first_name)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
    (sou base p.surname)
;

value print_first_name_ref conf base p =
  Wserver.wprint "<a href=\"%s%s\">\n%s%s</a>"
    (commd conf) (acces conf base p)
    (sou base p.first_name)
    (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
;

value print_warning conf base =
  fun
  [ BirthAfterDeath p ->
      Wserver.wprint (ftransl conf "%t died before his/her birth")
        (fun _ ->
           do print_someone base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | ChangedOrderOfChildren fam before ->
      let cpl = coi base fam.fam_index in
      let fath = poi base cpl.father in
      let moth = poi base cpl.mother in
      do Wserver.wprint "%s\n"
           (capitale (transl conf "changed order of children"));
         Wserver.wprint "%s\n" (transl_nth conf "of" 0);
         print_someone_ref conf base fath;
         Wserver.wprint "\n%s\n" (transl conf "and");
         print_someone_ref conf base moth;
         Wserver.wprint "\n<ul>";
         Wserver.wprint "\n<li> %s:\n" (capitale (transl conf "before"));
         Wserver.wprint "\n";
         tag "ul" begin
           Array.iter
             (fun ip ->
                let p = poi base ip in
                do Wserver.wprint "<li>\n";
                   if p.surname = fath.surname then
                     print_first_name_ref conf base p
                   else print_someone_ref conf base p;
                   Date.afficher_dates_courtes conf base p;
                   Wserver.wprint "\n";
                return ())
             before;
         end;
         Wserver.wprint "\n<li> %s:\n" (capitale (transl conf "after"));
         Wserver.wprint "\n";
         tag "ul" begin
           Array.iter
             (fun ip ->
                let p = poi base ip in
                do Wserver.wprint "<li>\n";
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
           (fun _ -> print_someone base (poi base cpl.father))
           (fun _ -> print_someone base (poi base cpl.mother));
         Wserver.wprint ":\n";
         Wserver.wprint "<ul>";
         Wserver.wprint "\n<li>\n";
         print_first_name base elder;
         Date.afficher_dates_courtes conf base elder;
         Wserver.wprint "\n<li>\n";
         print_first_name base x;
         Date.afficher_dates_courtes conf base x;
         Wserver.wprint "</ul>";
      return ()
  | DeadTooEarlyToBeFather father child ->
      Wserver.wprint
        (ftransl conf
    "%t is born more than 2 years after the death of his/her father %t")
        (fun _ ->
           do print_someone base child;
              Date.afficher_dates_courtes conf base child;
           return ())
        (fun _ ->
           do print_someone base father;
              Date.afficher_dates_courtes conf base father;
           return ())
  | MarriageDateAfterDeath p ->
      Wserver.wprint
        (fcapitale (ftransl conf "marriage of %t after his/her death"))
        (fun _ ->
           do print_someone base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | MarriageDateBeforeBirth p ->
      Wserver.wprint
        (fcapitale (ftransl conf "marriage of %t before his/her birth"))
        (fun _ ->
           do print_someone base p;
              Date.afficher_dates_courtes conf base p;
           return ())
  | MotherDeadAfterChildBirth mother child ->
      Wserver.wprint
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ ->
           do print_someone base child;
              Date.afficher_dates_courtes conf base child;
           return ())
        (fun _ ->
           do print_someone base mother;
              Date.afficher_dates_courtes conf base mother;
           return ())
  | ParentBornAfterChild p c ->
      do print_someone base p;
         Wserver.wprint "\n%s\n"
           (transl conf "is born after his/her child");
         print_someone base c;
      return ()
  | ParentTooYoung p a ->
      do print_someone base p;
         Wserver.wprint "\n%s\n" (transl conf "is a very young parent");
         Wserver.wprint "(";
         Date.print_age conf a;
         Wserver.wprint ")";
      return ()
  | TitleDatesError p t ->
      Wserver.wprint
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ ->
           do print_someone base p;
              Date.afficher_dates_courtes conf base p;
           return ())
        (fun _ ->
           Wserver.wprint "<strong>%s %s</strong> <em>%s-%s</em>"
             (sou base t.t_title) (sou base t.t_place)
             (match Adef.od_of_codate t.t_date_start with
              [ Some d -> string_of_int (annee d)
              | _ -> "" ])
             (match Adef.od_of_codate t.t_date_end with
              [ Some d -> string_of_int (annee d)
              | _ -> "" ]))
  | YoungForMarriage p a ->
      do print_someone base p;
         Wserver.wprint "\n";
         Wserver.wprint (ftransl conf "married at age %t")
           (fun _ -> Date.print_age conf a);
      return () ]
;

value print_warnings conf base wl =
  if wl = [] then ()
  else
    do Wserver.wprint "<p>%s\n" (capitale (transl conf "warnings"));
       tag "ul" begin
         List.iter
           (fun w ->
              do Wserver.wprint "<li>\n";
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

value digest_person (p : base_person) = Iovalue.digest p;
value digest_family (fam : base_family) = Iovalue.digest fam;

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
     [ Djma j m a -> Wserver.wprint "%d/%d/%d" j m a
     | Dma m a -> Wserver.wprint "%d/%d" m a
     | Da _ a -> Wserver.wprint "%d" a ];
     trailer conf;
  return raise ModErr
;

value reconstitute_date conf var =
  match get_number var "yyyy" conf.env with
  [ Some y ->
      match get_number var "mm" conf.env with
      [ Some m ->
          match get_number var "dd" conf.env with
          [ Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then Some (Djma d m y)
              else bad_date conf (Djma d m y)
          | None ->
              if m >= 1 && m <= 12 then Some (Dma m y)
              else bad_date conf (Dma m y) ]
      | None ->
          let prec =
            match get var "prec" conf.env with
            [ "about" -> About
            | "maybe" -> Maybe
            | "before" -> Before
            | "after" -> After
            | "oryear" -> OrYear (int_of_string (get var "oryear" conf.env))
            | _ -> Sure ]
          in
          Some (Da prec y) ]
  | None -> None ]
;

value print_date conf base lab var d =
  do tag "tr" begin
       stag "td" begin Wserver.wprint "%s" lab; end;
       tag "td" begin
         Wserver.wprint "<input name=%s_dd size=2 maxlength=2%s>\n" var
           (match d with
            [ Some (Djma j _ _) -> " value=" ^ string_of_int j
            | _ -> "" ]);
         Wserver.wprint "<input name=%s_mm size=2 maxlength=2%s>\n" var
           (match d with
            [ Some (Djma _ m _) -> " value=" ^ string_of_int m
            | Some (Dma m _) -> " value=" ^ string_of_int m
            | _ -> "" ]);
         Wserver.wprint "<input name=%s_yyyy size=5 maxlength=5%s>\n" var
           (match d with
            [ Some (Djma _ _ y) -> " value=" ^ string_of_int y
            | Some (Dma _ y) -> " value=" ^ string_of_int y
            | Some (Da _ y) -> " value=" ^ string_of_int y
            | _ -> "" ]);
       end;
       tag "td" begin
         Wserver.wprint "%s\n" (capitale (transl conf "year precision"));
         tag "select" "name=%s_prec" var begin
           Wserver.wprint "<option%s>-\n"
             (match d with
              [ Some (Djma _ _ _) | Some (Dma _ _) | None -> " selected"
              | _ -> "" ]);
           Wserver.wprint "<option value=sure%s>%s\n"
             (match d with [ Some (Da Sure _) -> " selected" | _ -> "" ])
             (capitale (transl conf "exact"));
           Wserver.wprint "<option value=about%s>%s\n"
             (match d with [ Some (Da About _) -> " selected" | _ -> "" ])
             (capitale (transl conf "about (year)"));
           Wserver.wprint "<option value=maybe%s>%s\n"
             (match d with [ Some (Da Maybe _) -> " selected" | _ -> "" ])
             (capitale (transl conf "maybe in (year)"));
           Wserver.wprint "<option value=before%s>%s\n"
             (match d with [ Some (Da Before _) -> " selected" | _ -> "" ])
             (capitale (transl conf "before (year)"));
           Wserver.wprint "<option value=after%s>%s\n"
             (match d with [ Some (Da After _) -> " selected" | _ -> "" ])
             (capitale (transl conf "after (year)"));
           Wserver.wprint "<option value=oryear%s>%s ->\n"
             (match d with [ Some (Da (OrYear _) _) -> " selected" | _ -> "" ])
             (capitale (transl conf "or" ^ " " ^ transl conf "year"));
         end;
         Wserver.wprint "<input name=%s_oryear size=5 maxlength=5%s>\n" var
           (match d with
            [ Some (Da (OrYear y) _) -> " value=" ^ string_of_int y
            | _ -> "" ]);
       end;
     end;
  return ()
;

value print_someone base p =
  Wserver.wprint "%s%s %s" (sou base p.first_name)
    (if p.occ == 0 then ""else "." ^ string_of_int p.occ)
    (sou base p.surname)
;

value print_family_stuff conf base p a =
  tag "ul" begin
   let _ = List.fold_left
      (fun prev fi ->
         do match prev with
            [ Some prev_fi ->
                let cpl1 = coi base prev_fi in
                let cpl2 = coi base fi in
                do Wserver.wprint "\n<li>\n";
                   Wserver.wprint "<a href=\"%sm=SWI_FAM;i=%d;f=%d\">"
                     (commd conf) (Adef.int_of_iper p.cle_index)
                     (Adef.int_of_ifam fi);
                   Wserver.wprint "%s</a>\n" (capitale (transl conf "switch"));
                   if cpl1.father = cpl2.father && cpl1.mother = cpl2.mother
                   then
                     do Wserver.wprint "<li>\n";
                        stag "a" "href=\"%sm=MRG_FAM;f1=%d;f2=%d\""
                          (commd conf) (Adef.int_of_ifam prev_fi)
                          (Adef.int_of_ifam fi)
                        begin
                          Wserver.wprint "%s" (capitale (transl conf "merge"));
                        end;
                        Wserver.wprint "\n";
                     return ()
                   else ();
                return ()
            | None -> () ];
         return
         let c = conjoint p (coi base fi) in
         do Wserver.wprint "\n<li>\n";
            Wserver.wprint "<a href=\"%sm=MOD_FAM;i=%d\">" (commd conf)
              (Adef.int_of_ifam fi);
            Wserver.wprint "%s / %s</a>\n"
              (capitale (transl conf "modify"))
              (capitale (transl_nth conf "family/families" 0));
            Wserver.wprint "\n<em>%s</em>\n" (transl conf "with");
            print_someone base (poi base c);
            Wserver.wprint "\n<li>\n";
            Wserver.wprint "<a href=\"%sm=DEL_FAM;i=%d\">" (commd conf)
              (Adef.int_of_ifam fi);
            Wserver.wprint "%s / %s</a>\n"
              (capitale (transl conf "delete"))
              (capitale (transl_nth conf "family/families" 0));
            Wserver.wprint "\n<em>%s</em>\n" (transl conf "with");
            print_someone base (poi base c);
         return Some fi)
      None (Array.to_list p.family)
    in ();
    if (sou base p.first_name = "?" || sou base p.surname = "?")
    && (Array.length p.family <> 0 || a.parents <> None) then ()
    else
      do Wserver.wprint "\n<li>\n";
         Wserver.wprint "<a href=\"%sm=ADD_FAM;i=%d\">%s / %s</a>\n"
           (commd conf) (Adef.int_of_iper p.cle_index)
           (capitale (transl conf "add"))
           (capitale (transl_nth conf "family/families" 0));
      return ();
  end
;

value print conf base p =
  let title h =
    do Wserver.wprint "%s" (capitale (transl conf "update"));
       if h then ()
       else
         let fn = sou base p.first_name in
         let sn = sou base p.surname in
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
     Wserver.wprint "<ul>";
     Wserver.wprint "\n<li>\n";
     Wserver.wprint "<a href=\"%sm=MOD_IND;i=%d\">%s</a>\n" (commd conf)
       (Adef.int_of_iper p.cle_index)
       (capitale (transl conf "modify"));
     Wserver.wprint "\n<li>\n";
     Wserver.wprint "<a href=\"%sm=DEL_IND;i=%d\">%s</a>\n"
       (commd conf) (Adef.int_of_iper p.cle_index)
       (capitale (transl conf "delete"));
     Wserver.wprint "</ul>\n";
     Wserver.wprint "\n";
     print_family_stuff conf base p a;
     match a.parents with
     [ Some _ -> ()
     | None ->
         if sou base p.first_name = "?" || sou base p.surname = "?" then ()
         else
           do Wserver.wprint "<ul>";
              Wserver.wprint "\n<li>\n";
              Wserver.wprint "<a href=\"%sm=ADD_PAR;i=%d\">%s / %s</a>\n"
                (commd conf) (Adef.int_of_iper p.cle_index)
                (capitale (transl conf "add"))
                (capitale (transl conf "parents"));
              Wserver.wprint "</ul>\n";
           return () ];
     Wserver.wprint "\n";
     Wserver.wprint "<p>\n";
     tag "ul" begin
       Wserver.wprint "<li>\n";
       stag "a" "href=\"%sm=MRG;i=%d\"" (commd conf)
         (Adef.int_of_iper p.cle_index)
       begin
         Wserver.wprint "%s" (capitale (transl conf "merge"));
       end;
     end;
     trailer conf;
  return ()
;
