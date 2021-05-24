(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Dag2html
open Gwdb
open Util

let limit_by_tree conf =
  match p_getint conf.base_env "max_desc_tree" with
    Some x -> max 1 x
  | None -> 4

let text_to conf =
  function
    0 ->
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
  | 1 -> transl conf "to the children"
  | 2 -> transl conf "to the grandchildren"
  | 3 -> transl conf "to the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i)

let text_level conf =
  function
    0 ->
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
  | 1 -> transl conf "the children"
  | 2 -> transl conf "the grandchildren"
  | 3 -> transl conf "the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i)

let descendants_title conf base p h =
  let s1 = gen_person_text raw_access conf base p in
  let s2 = if h then gen_person_text_no_html raw_access conf base p else s1 in
  let s =
    translate_eval
      (transl_a_of_gr_eq_gen_lev conf (transl conf "descendants") s1 s2)
  in
  Output.print_string conf (Utf8.capitalize_fst s)

let display_descendants_level conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let (levt, _) = Perso.make_desc_level_table conf base max_level ancestor in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
  let rec get_level level u list =
    Array.fold_left
      (fun list ifam ->
         let des = foi base ifam in
         let enfants = get_children des in
         Array.fold_left
           (fun list ix ->
              let x = pget conf base ix in
              if Gwdb.Marker.get mark ix then list
              else
                let _ = Gwdb.Marker.set mark ix true in
                if Gwdb.Marker.get levt ix > max_level then list
                else if level = max_level then
                  if p_first_name base x = "x"
                  || Gwdb.Marker.get levt ix != level
                  then list
                  else x :: list
                else if level < max_level then
                  get_level (succ level) (pget conf base ix) list
                else list)
           list enfants)
      list (get_family u)
  in
  let len = ref 0 in
  let list = get_level 1 (pget conf base (get_iper ancestor)) [] in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = Gutil.alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           let c = Gutil.alphabetic (p_first_name base p2) (p_first_name base p1) in
           if c = 0 then compare (get_occ p2) (get_occ p1) else c
         else c)
      list
  in
  let list =
    List.fold_left
      (fun pl p ->
         match pl with
           (p1, n) :: pl when get_iper p = get_iper p1 ->
             (p1, succ n) :: pl
         | _ -> incr len; (p, 1) :: pl)
      [] list
  in
  Hutil.header conf (descendants_title conf base ancestor);
  Output.print_string conf (Utf8.capitalize_fst (text_level conf max_level));
  if !len > 1 then
    Output.printf conf " (%d %s)" !len
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
  Output.print_string conf ".\n";
  Output.print_string conf "<p>" ;
  print_alphab_list conf
    (fun (p, _) ->
       if is_hidden p then "?"
       else String.sub (p_surname base p) (Mutil.initial (p_surname base p)) 1)
    (fun (p, c) ->
       Output.printf conf "\n%s" (referenced_person_title_text conf base p);
       Output.print_string conf (DateDisplay.short_dates_text conf base p);
       if not (is_hidden p) && c > 1 then Output.printf conf " <em>(%d)</em>" c;
       Output.print_string conf "\n")
    list;
  Hutil.trailer conf

(* With number *)

let mark_descendants conf base marks max_lev ip =
  let rec loop lev ip u =
    if lev <= max_lev then
      begin
        Gwdb.Marker.set marks ip true ;
        Array.iter
          (fun ifam ->
             let el = get_children (foi base ifam) in
             Array.iter (fun e -> loop (succ lev) e (pget conf base e)) el)
          (get_family u)
      end
  in
  loop 0 ip (pget conf base ip)

let label_descendants conf base marks paths max_lev =
  let rec loop path lev p =
    if lev < max_lev then
      ignore @@
      Array.fold_left
        (fun cnt ifam ->
           let fam = foi base ifam in
           let c = Gutil.spouse (get_iper p) fam in
           let el = get_children fam in
           Array.fold_left
             (fun cnt e ->
                if get_sex p = Male || not (Gwdb.Marker.get marks c) then
                  begin let path = Char.chr (Char.code 'A' + cnt) :: path in
                    Gwdb.Marker.set paths e path ;
                    loop path (succ lev) (pget conf base e)
                  end;
                succ cnt)
             cnt el)
        0 (get_family p)
  in
  loop [] 0

let close_lev = 2

let close_to_end conf base marks max_lev lev p =
  if lev + close_lev >= max_lev then true
  else
    let rec short dlev p =
      Array.for_all
        (fun ifam ->
           let fam = foi base ifam in
           let c = Gutil.spouse (get_iper p) fam in
           let el = get_children fam in
           if get_sex p = Male || not (Gwdb.Marker.get marks c) then
             if dlev = close_lev then Array.length el = 0
             else
               Array.for_all (fun e -> short (succ dlev) (pget conf base e)) el
           else true)
        (get_family p)
    in
    short 1 p

let labelled conf base marks max_lev lev ip =
  let a = pget conf base ip in
  let u = a in
  Array.length (get_family u) <> 0 &&
  (match get_parents a with
     Some ifam ->
     let fam = foi base ifam in
     let el = get_children fam in
     Array.exists
       (fun ie ->
          let e = pget conf base ie in
          let u = e in
          Array.length (get_family u) <> 0 &&
          not (close_to_end conf base marks max_lev lev e))
       el
   | _ -> false)

let label_of_path paths p =
  let rec loop = function
    | [] -> ""
    | c :: cl -> loop cl ^ String.make 1 c
  in
  loop @@ Gwdb.Marker.get paths (get_iper p)

let print_child conf base p1 p2 e =
  Output.print_string conf "<strong>";
  if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
     get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
  then
    Output.print_string conf (referenced_person_text_without_surname conf base e)
  else Output.printf conf "\n%s" (referenced_person_text conf base e);
  Output.print_string conf "</strong>";
  Output.print_string conf (DateDisplay.short_dates_text conf base e)

let print_repeat_child conf base p1 p2 e =
  Output.print_string conf "<em>";
  if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
     get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
  then
    Output.print_string conf (person_text_without_surname conf base e)
  else Output.print_string conf (person_text conf base e);
  Output.print_string conf "</em>"

let display_spouse conf base marks paths fam p c =
  Output.print_string conf "\n&amp;";
  Output.print_string conf (DateDisplay.short_marriage_date_text conf base fam p c);
  Output.print_string conf " ";
  Output.print_string conf "<strong>";
  Output.printf conf "\n%s" (referenced_person_text conf base c);
  Output.print_string conf "</strong>";
  if Gwdb.Marker.get marks (get_iper c)
  then Output.printf conf " (<tt><b>%s</b></tt>)" (label_of_path paths c)
  else Output.print_string conf (DateDisplay.short_dates_text conf base c)

let print_family_locally conf base ~total marks paths max_lev lev p1 c1 e =
  let rec loop lev p =
    if lev < max_lev then
      ignore @@
      Array.fold_left
        begin fun (cnt, first, need_br) ifam ->
          let fam = foi base ifam in
          let c = Gutil.spouse (get_iper p) fam in
          let el = get_children fam in
          let c = pget conf base c in
          if need_br then Output.print_string conf "<br>" ;
          if not first then print_repeat_child conf base p1 c1 p;
          display_spouse conf base marks paths fam p c;
          Output.print_string conf "\n";
          let print_children =
            get_sex p = Male || not (Gwdb.Marker.get marks (get_iper c))
          in
          if print_children then Output.printf conf "<ol start=\"%d\">\n" (succ cnt);
          let cnt =
            Array.fold_left
              begin fun cnt ie ->
                let e = pget conf base ie in
                if print_children then
                  begin
                    Output.print_string conf "<li type=\"A\"> ";
                    print_child conf base p c e;
                    Output.print_string conf "\n";
                    incr total;
                    if succ lev = max_lev then
                      Array.iteri
                        begin fun i ifam ->
                          let fam = foi base ifam in
                          let c1 = Gutil.spouse ie fam in
                          let el = get_children fam in
                          let c1 = pget conf base c1 in
                          if i <> 0 then
                            begin
                              Output.print_string conf "<br>" ;
                              print_repeat_child conf base p c e
                            end ;
                          display_spouse conf base marks paths fam e c1;
                          if Array.length el <> 0 then Output.print_string conf ".....";
                          Output.print_string conf "\n"
                        end
                        (get_family (pget conf base ie))
                    else loop (succ lev) e
                  end ;
                succ cnt
              end
              cnt el
          in
          if print_children then Output.print_string conf "</ol>\n";
          cnt, false, not print_children
        end
        (0, true, false) (get_family p)
  in
  loop lev e

let print_family conf base ~total ~last_label marks paths max_lev lev p =
  if lev <> 0 then Output.printf conf "<tt><b>%s</b></tt>.<br>" (label_of_path paths p);
  let lab = label_of_path paths p in
  if lab < !last_label then failwith "print_family" else last_label := lab;
  ignore @@
  Array.fold_left
    (fun cnt ifam ->
       let fam = foi base ifam in
       let c = Gutil.spouse (get_iper p) fam in
       let el = get_children fam in
       let c = pget conf base c in
       Output.print_string conf "<strong>";
       Output.printf conf "\n%s" (referenced_person_text conf base p);
       Output.print_string conf "</strong>";
       display_spouse conf base marks paths fam p c;
       Output.printf conf "<ol start=\"%d\">\n" (succ cnt);
       let cnt =
         Array.fold_left
           (fun cnt ie ->
              let e = pget conf base ie in
              if get_sex p = Male
              || not (Gwdb.Marker.get marks (get_iper c))
              then
                begin
                  Output.print_string conf "<li type=\"A\">";
                  print_child conf base p c e;
                  incr total;
                  Output.print_string conf "\n";
                  if labelled conf base marks max_lev lev ie then
                    Output.printf conf " => <tt><b>%s</b></tt>\n"
                      (label_of_path paths e)
                  else if succ lev = max_lev then
                    Array.iter
                      (fun ifam ->
                         let fam = foi base ifam in
                         let c = Gutil.spouse ie fam in
                         let el = get_children fam in
                         let c = pget conf base c in
                         display_spouse conf base marks paths fam e c;
                         if Array.length el <> 0 then
                           Output.print_string conf ".....";
                         Output.print_string conf "\n")
                      (get_family (pget conf base ie))
                  else
                    print_family_locally conf base ~total marks paths max_lev
                      (succ lev) p c e
                end;
              succ cnt)
           cnt el
       in
       Output.print_string conf "</ol>\n"; cnt)
    0 (get_family p)

let print_families conf base ~total ~last_label marks paths max_lev =
  let rec loop lev p =
    if lev < max_lev then begin
      print_family conf base ~total ~last_label marks paths max_lev lev p;
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let c = Gutil.spouse (get_iper p) fam in
           let el = get_children fam in
           let c = pget conf base c in
           if get_sex p = Male || not (Gwdb.Marker.get marks (get_iper c))
           then
             Array.iter
               (fun ie ->
                  let e = pget conf base ie in
                  if labelled conf base marks max_lev lev ie then
                    loop (succ lev) e)
               el)
        (get_family p)
    end
  in
  loop 0

let display_descendants_with_numbers conf base ~total ~last_label max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    if h then descendants_title conf base ancestor h
    else
      wprint_geneweb_link conf
        ("m=D&i=" ^
         string_of_iper (get_iper ancestor) ^ "&v=" ^
         string_of_int max_level ^ "&t=G")
        (Utf8.capitalize_fst
           (let s1 = person_text conf base ancestor in
            let s2 = person_text_no_html conf base ancestor in
            transl_a_of_gr_eq_gen_lev conf (transl conf "descendants") s1 s2))
  in
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  Hutil.header conf title;
  total := 0;
  Output.print_string conf (DateDisplay.short_dates_text conf base ancestor);
  let p = ancestor in
  if authorized_age conf base p then
    begin match Adef.od_of_cdate (get_birth p), get_death p with
      Some _, _ | _, Death (_, _) -> Output.print_string conf "<br>"
    | _ -> ()
    end;
  Output.printf conf "%s." (Utf8.capitalize_fst (text_to conf max_level));
  Output.print_string conf "<p>" ;
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  print_families conf base ~total ~last_label marks paths max_level ancestor;
  if !total > 1 then
    begin
      Output.print_string conf "<p>" ;
      Output.printf conf "%s%s %d %s" (Utf8.capitalize_fst (transl conf "total"))
        (Util.transl conf ":") !total
        (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
      if max_level > 1 then
        Output.printf conf " (%s)" (transl conf "spouses not included");
      Output.print_string conf ".\n"
    end;
  Hutil.trailer conf

let print_ref conf base paths p =
  if Gwdb.Marker.get paths (get_iper p) <> [] then
    Output.printf conf " => <tt><b>%s</b></tt>" (label_of_path paths p)
  else
    Array.iter
      (fun ifam ->
         let c = Gutil.spouse (get_iper p) (foi base ifam) in
         if Gwdb.Marker.get paths c <> [] then
           let c = pget conf base c in
           Output.printf conf " => %s %s <tt><b>%s</b></tt>" (p_first_name base c)
             (p_surname base c) (label_of_path paths c))
      (get_family p)

let print_elem conf base paths precision (n, pll) =
  Output.print_string conf "<li>" ;
  match List.rev pll with
    [[p]] ->
      Output.printf conf "<strong>%s %s %s</strong>" (surname_without_particle base n)
        (reference conf base p (person_text_without_surname conf base p))
        (surname_particle base n);
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      print_ref conf base paths p;
      Output.print_string conf "\n"
  | pll ->
      Output.printf conf "<strong>%s%s</strong>\n" (surname_without_particle base n)
        (surname_particle base n);
      Output.print_string conf "<ul>\n";
      List.iter
        (fun pl ->
           let several =
             match pl with
               [_] -> false
             | _ -> true
           in
           List.iter
             (fun p ->
                Output.print_string conf "<li>" ;
                Output.print_string conf "<strong>";
                wprint_geneweb_link conf (acces conf base p)
                  (p_first_name base p);
                Output.print_string conf "</strong>";
                if several && precision then
                  begin
                    Output.print_string conf "<em>";
                    specify_homonymous conf base p true;
                    Output.print_string conf "</em>"
                  end;
                Output.print_string conf (DateDisplay.short_dates_text conf base p);
                print_ref conf base paths p;
                Output.print_string conf "\n")
             pl)
        pll;
      Output.print_string conf "</ul>\n"

let sort_and_display conf base paths precision list =
  let list = List.map (pget conf base) list in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = Gutil.alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           Gutil.alphabetic (p_first_name base p2) (p_first_name base p1)
         else c)
      list
  in
  let list =
    List.fold_left
      (fun npll p ->
         match npll with
           (n, pl) :: npll when n = p_surname base p -> (n, p :: pl) :: npll
         | _ -> (p_surname base p, [p]) :: npll)
      [] list
  in
  let list =
    List.map
      (fun (n, pl) ->
         let pll =
           List.fold_left
             (fun pll p ->
                match pll with
                  (p1 :: _ as pl) :: pll
                  when eq_istr (get_first_name p1) (get_first_name p) ->
                    (p :: pl) :: pll
                | _ -> [p] :: pll)
             [] pl
         in
         n, pll)
      list
  in
  if list <> [] then
    begin
      Output.print_string conf "<ul>\n";
      List.iter (print_elem conf base paths precision) list;
      Output.print_string conf "</ul>\n"
    end

let display_descendant_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    let txt = Utf8.capitalize_fst (transl conf "index of the descendants") in
    if not h then
      wprint_geneweb_link conf
        ("m=D&i=" ^
         string_of_iper (get_iper ancestor) ^ "&v=" ^
         string_of_int max_level ^ "&t=C")
        txt
    else Output.print_string conf txt
  in
  Hutil.header conf title;
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Gwdb.Collection.fold begin fun acc i ->
      let p = pget conf base i in
      if p_first_name base p <> "?"
      && p_surname base p <> "?"
      && p_first_name base p <> "x"
      && (not (is_hide_names conf p) || authorized_age conf base p)
      then get_iper p :: acc
      else acc
    end [] (ipers base)
  in
  sort_and_display conf base paths true list;
  Hutil.trailer conf

let display_spouse_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title _ =
    Output.print_string conf
      (Utf8.capitalize_fst (transl conf "index of the spouses (non descendants)"))
  in
  Hutil.header conf title;
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Gwdb.Collection.fold begin fun acc i ->
      let p = pget conf base i in
      if Gwdb.Marker.get paths i <> [] then
        if p_first_name base p <> "?"
        && p_surname base p <> "?"
        && p_first_name base p <> "x"
        then
          Array.fold_left begin fun acc ifam ->
            let c = Gutil.spouse (get_iper p) (foi base ifam) in
            if Gwdb.Marker.get paths c = [] then
              let c = pget conf base c in
              if p_first_name base c <> "?"
              && p_surname base c <> "?"
              && p_first_name base p <> "x"
              && (not (is_hide_names conf c) || authorized_age conf base c)
              && not (List.mem (get_iper c) acc)
              then get_iper c :: acc
              else acc
            else acc
          end acc (get_family p)
        else acc
      else acc end [] (ipers base)
  in
  sort_and_display conf base paths false list;
  Hutil.trailer conf

(* *********************************************************************** *)
(*  [Fonc] print_desc_table_header : config -> base -> int                 *)
(** [Description] : Affiche en fonction des options qui sont sélectionnées
                    le header du tableau de descendance.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Le nombre de colonnes à afficher (nombre d'options sélectionnées).
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let print_desc_table_header conf =
  let nb_col = ref 2 in
  Output.print_string conf "<tr class=\"descends_table_header\">\n";
  Output.print_string conf "<th>\n";
  Output.print_string conf (Utf8.capitalize_fst (transl conf "n° d'Aboville"));
  Output.print_string conf "</th>\n";
  Output.print_string conf "<th>\n";
  Output.print_string conf (Utf8.capitalize_fst (transl_nth conf "person/persons" 0));
  Output.print_string conf "</th>\n";
  if p_getenv conf.env "birth" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "date of birth"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "birth_place" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "where born"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "marr" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl_nth conf "spouse/spouses" 1));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "marr_date" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      Output.print_string conf (Utf8.capitalize_fst (transl conf "date of marriage"));
      incr nb_col;
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "marr_place" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "where married"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "child" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "nb children"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "death" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      Output.print_string conf (Utf8.capitalize_fst (transl conf "date of death"));
      incr nb_col;
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "death_place" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "where dead"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "death_age" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf (Utf8.capitalize_fst (transl conf "age at death"));
      Output.print_string conf "</th>\n"
    end;
  if p_getenv conf.env "occu" = Some "on" then
    begin
      Output.print_string conf "<th>\n";
      incr nb_col;
      Output.print_string conf
        (Utf8.capitalize_fst (transl_nth conf "occupation/occupations" 1));
      Output.print_string conf "</th>\n"
    end;
  Output.print_string conf "</tr>\n";
  !nb_col


(* *********************************************************************** *)
(*  [Fonc] print_person_table : config -> base -> person -> string -> unit *)
(** [Description] : Affiche en fonction des options qui sont sélectionnées
                    les informations d'une personne (correspond à une ligne
                    du tableau).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
      - lab  : numéro d'Aboville de p
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let print_person_table conf base p lab =
  let p_auth = Util.authorized_age conf base p in
  let nb_families = Array.length (get_family p) in
  let (birth, birth_place) =
    if (p_getenv conf.env "birth" = Some "on" ||
        p_getenv conf.env "birth_place" = Some "on") &&
       p_auth
    then
      let (date, place) = Util.get_approx_birth_date_place conf base p in
      let date =
        match date with
          Some d -> DateDisplay.string_slash_of_date conf d
        | None -> ""
      in
      date, place
    else "&nbsp;", ""
  in
  let (death, death_place) =
    if (p_getenv conf.env "death" = Some "on" ||
        p_getenv conf.env "death_place" = Some "on") &&
       p_auth
    then
      let (date, place) = Util.get_approx_death_date_place conf base p in
      let date =
        match date with
          Some d -> DateDisplay.string_slash_of_date conf d
        | None -> ""
      in
      date, place
    else "&nbsp;", ""
  in
  (* On calcul le nombre de rowspan pour avoir un affichage joli. *)
  let rowspan =
    if nb_families > 1 &&
       (p_getenv conf.env "marr" = Some "on" ||
        p_getenv conf.env "marr_date" = Some "on" ||
        p_getenv conf.env "marr_place" = Some "on")
    then
      "rowspan=\"" ^ string_of_int nb_families ^ "\""
    else ""
  in
  (* On met partout un &nbsp; dans le cas où ce que l'on souhaite *)
  (* afficher est vide, comme ça, on ne casse pas le rowspan.     *)
  Output.print_string conf "<tr>\n";
  Output.printf conf "<td %s>\n" rowspan;
  Output.print_string conf lab;
  Output.print_string conf "</td>\n";
  Output.printf conf "<td %s>\n" rowspan;
  Util.print_image_sex conf p 11;
  Output.printf conf " %s &nbsp;" (referenced_person_title_text conf base p);
  Output.print_string conf "</td>\n";
  if p_getenv conf.env "birth" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      Output.print_string conf birth;
      Output.print_string conf "</td>\n"
    end;
  if p_getenv conf.env "birth_place" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      Output.printf conf "%s &nbsp;" birth_place;
      Output.print_string conf "</td>\n"
    end;
  (* On affiche que la première famille (get_family u).(0). *)
  (* Les autres familles seront affichées après qu'on ait   *)
  (* fini de remplir le <tr>.                               *)
  if p_getenv conf.env "marr" = Some "on" then
    begin
      Output.printf conf "<td %s>\n"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "");
      begin let u = p in
        if nb_families > 0 then
          let cpl = foi base (get_family u).(0) in
          let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
          Util.print_image_sex conf spouse 11;
          Output.printf conf " %s &nbsp;"
            (referenced_person_text conf base spouse)
        else Output.print_string conf "&nbsp;"
      end;
      Output.print_string conf "</td>\n"
    end;
  (* On affiche que la première famille (get_family u).(0). *)
  (* Les autres familles seront affichées après qu'on ait   *)
  (* fini de remplir le <tr>.                               *)
  if p_getenv conf.env "marr_date" = Some "on" then
    begin
      Output.printf conf "<td %s>\n"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "");
      begin let u = p in
        if nb_families > 0 then
          let cpl = foi base (get_family u).(0) in
          let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
          let mdate =
            if authorized_age conf base p && authorized_age conf base spouse
            then
              let fam = foi base (get_family u).(0) in
              match Adef.od_of_cdate (get_marriage fam) with
                Some d -> DateDisplay.string_slash_of_date conf d
              | _ -> "&nbsp;"
            else "&nbsp;"
          in
          Output.print_string conf mdate
        else Output.print_string conf "&nbsp;"
      end;
      Output.print_string conf "</td>\n"
    end;
  (* On affiche que la première famille (get_family u).(0). *)
  (* Les autres familles seront affichées après qu'on ait   *)
  (* fini de remplir le <tr>.                               *)
  if p_getenv conf.env "marr_place" = Some "on" then
    begin
      Output.printf conf "<td %s>\n"
        (if nb_families > 1 then "style=\"border-bottom:none\"" else "");
      begin let u = p in
        if nb_families > 0 then
          let cpl = foi base (get_family u).(0) in
          let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
          let mplace =
            if authorized_age conf base p && authorized_age conf base spouse
            then
              Util.string_of_place conf (sou base (get_marriage_place cpl))
            else ""
          in
          Output.printf conf "%s &nbsp;" mplace
        else Output.print_string conf "&nbsp;"
      end;
      Output.print_string conf "</td>\n"
    end;
  (* On affiche que la première famille (get_family u).(0). *)
  (* Les autres familles seront affichées après qu'on ait   *)
  (* fini de remplir le <tr>.                               *)
  if p_getenv conf.env "child" = Some "on" then
    if p_getenv conf.env "marr" = Some "on" ||
       p_getenv conf.env "marr_place" = Some "on" ||
       p_getenv conf.env "marr_date" = Some "on"
    then
      begin
        Output.printf conf "<td align=\"center\" %s>\n"
          (if nb_families > 1 then "style=\"border-bottom:none\"" else "");
        begin let u = p in
          if nb_families > 0 then
            let fam = foi base (get_family u).(0) in
            Output.printf conf "%d &nbsp;" (Array.length (get_children fam))
          else Output.print_string conf "&nbsp;"
        end;
        Output.print_string conf "</td>\n"
      end
    else
      begin
        let n =
          Array.fold_left
            (fun n ifam -> n + Array.length (get_children (foi base ifam))) 0
            (get_family p)
        in
        Output.print_string conf "<td>\n";
        Output.printf conf "%d &nbsp;" n;
        Output.print_string conf "</td>\n"
      end;
  if p_getenv conf.env "death" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      Output.print_string conf death;
      Output.print_string conf "</td>\n"
    end;
  if p_getenv conf.env "death_place" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      Output.printf conf "%s &nbsp;" death_place;
      Output.print_string conf "</td>\n"
    end;
  if p_getenv conf.env "death_age" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      begin let d =
        if not p_auth then ""
        else
          match Date.get_birth_death_date p with
            Some (Dgreg (({prec = Sure | About | Maybe} as d1), _)),
            Some (Dgreg (({prec = Sure | About | Maybe} as d2), _)), approx
            when d1 <> d2 ->
              let a = Date.time_elapsed d1 d2 in
              let s =
                if not approx && d1.prec = Sure && d2.prec = Sure then ""
                else transl_decline conf "possibly (date)" "" ^ " "
              in
              s ^ DateDisplay.string_of_age conf a
          | _ -> ""
      in
        Output.printf conf "%s &nbsp;" d
      end;
      Output.print_string conf "</td>\n"
    end;
  if p_getenv conf.env "occu" = Some "on" then
    begin
      Output.printf conf "<td %s>\n" rowspan;
      Output.printf conf "%s &nbsp;"
        (if p_auth then sou base (get_occupation p) else "");
      Output.print_string conf "</td>\n"
    end;
  (* On met partout un &nbsp; dans le cas où ce que l'on souhaite *)
  (* afficher est vide, comme ça, on ne casse pas le rowspan.     *)
  Output.print_string conf "</tr>\n";
  (* Maintenant qu'on a fini d'afficher le <tr> complet, si il y a  *)
  (* plusieurs familles, il faut alors afficher chacune d'elle dans *)
  (* un <tr> afin d'avoir une mise en page utilisant des rowspan.   *)
  if nb_families > 1 then
    if p_getenv conf.env "marr" = Some "on" ||
       p_getenv conf.env "marr_date" = Some "on" ||
       p_getenv conf.env "marr_place" = Some "on"
    then
      let u = p in
      for i = 1 to nb_families - 1 do
        let cpl = foi base (get_family u).(i) in
        let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
        let fam = foi base (get_family u).(i) in
        Output.print_string conf "<tr>\n";
        if p_getenv conf.env "marr" = Some "on" then
          begin
            Output.printf conf "<td style=\"border-top:none; %s\">\n"
              (if nb_families - 1 <> i then "border-bottom:none;" else "");
            Util.print_image_sex conf spouse 11;
            Output.printf conf " %s &nbsp;"
              (referenced_person_text conf base spouse);
            Output.print_string conf "</td>\n"
          end;
        if p_getenv conf.env "marr_date" = Some "on" then
          begin
            Output.printf conf "<td style=\"border-top:none; %s\">\n"
              (if nb_families - 1 <> i then "border-bottom:none;" else "");
            begin let mdate =
              if authorized_age conf base p && authorized_age conf base spouse
              then
                let fam = foi base (get_family u).(i) in
                match Adef.od_of_cdate (get_marriage fam) with
                  Some d -> DateDisplay.string_slash_of_date conf d
                | _ -> "&nbsp;"
              else "&nbsp;"
            in
              Output.print_string conf mdate
            end;
            Output.print_string conf "</td>\n"
          end;
        if p_getenv conf.env "marr_place" = Some "on" then
          begin
            Output.printf conf "<td style=\"border-top:none; %s\">\n"
              (if nb_families - 1 <> i then "border-bottom:none;" else "");
            begin let mplace =
              if authorized_age conf base p && authorized_age conf base spouse
              then
                Util.string_of_place conf (sou base (get_marriage_place cpl))
              else ""
            in
              Output.printf conf "%s &nbsp;" mplace
            end;
            Output.print_string conf "</td>\n"
          end;
        if p_getenv conf.env "child" = Some "on" then
          begin
            Output.printf conf
              "<td align=\"center\" style=\"border-top:none; %s\">\n"
              (if nb_families - 1 <> i then "border-bottom:none;" else "");
            Output.printf conf "%d &nbsp;" (Array.length (get_children fam));
            Output.print_string conf "</td>\n"
          end;
        Output.print_string conf "</tr>\n"
      done


(* ********************************************************************** *)
(*  [Fonc] build_desc : config -> base -> person list -> person list      *)
(** [Description] : Construit la liste des descendants de la liste des
                    personnes (passée en paramètre). Correspond à un
                    parcours en largeur.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - l    : person list
    [Retour] : person list
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let build_desc conf base l =
  let rec loop l accu =
    match l with
      [] ->
        (* Comme on a fait un fold_left pour avoir le bon ordre *)
        (* des enfants, on renverse l'accumulateur pour l'avoir *)
        (* lui aussi dans le bon ordre.                         *)
        List.rev accu
    | (p, lab) :: l ->
        let cnt = ref 0 in
        let nx_accu =
          (* On fait des fold_left pour garder l'ordre des enfants. *)
          (* lab correspond au numéro d'Aboville de p.              *)
          Array.fold_left
            (fun accu ifam ->
               let fam = foi base ifam in
               Array.fold_left
                 (fun accu ip ->
                    let _ = incr cnt in
                    (pget conf base ip, lab ^ string_of_int !cnt ^ ".") ::
                    accu)
                 accu (get_children fam))
            accu (get_family p)
        in
        loop l nx_accu
  in
  loop l []


(* ********************************************************************** *)
(*  [Fonc] display_descendant_with_table :
      config -> base -> int -> person -> unit                             *)
(** [Description] : Affiche sous la forme d'un tableau la descendance
                    d'une personne.
    [Args] :
      - conf    : configuration de la base
      - base    : base de donnée
      - max_lev : le nombre de générations à afficher
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let display_descendant_with_table conf base max_lev p =
  let max_lev = min (Perso.limit_desc conf) max_lev in
  let nb_pers = ref 0 in
  (* Affiche la liste des personnes, génération par génération.    *)
  (* On affiche toutes les personnes de l, et quand la liste est   *)
  (* vide, on construit la list des descendants de l (mais comme l *)
  (* est vide, on les calcul par rapport à refl)                   *)
  let rec loop lev nb_col first refl l =
    match l with
      [] ->
        if lev < max_lev then
          let nl = build_desc conf base refl in
          loop (lev + 1) nb_col true nl nl
    | (p, lab) :: q ->
        if first && lev > 0 && p_getenv conf.env "gen" = Some "on" then
          begin
            Output.print_string conf "<tr>\n";
            begin
              Output.printf conf "<th align=\"left\" colspan=\"%d\">\n" nb_col;
              Output.printf conf "%s %d"
                (Utf8.capitalize_fst (transl_nth conf "generation/generations" 0)) lev;
              Output.print_string conf "</th>\n"
            end;
            Output.print_string conf "</tr>\n"
          end;
        print_person_table conf base p lab;
        incr nb_pers;
        loop lev nb_col false refl q
  in
  Hutil.header_fluid conf (descendants_title conf base p);
  Output.print_string conf "<p>\n";
  Output.printf conf "%s." (Utf8.capitalize_fst (text_to conf max_lev));
  Output.print_string conf "</p>\n";
  Output.print_string conf "<table class=descends_table>\n";
  (* On affiche l'entête et on en profite pour récupèrer *)
  (* le nombre de colonnes à afficher pour les colspans. *)
  begin let nb_col = print_desc_table_header conf in
    loop 0 nb_col true [p, ""] [p, ""]
  end;
  Output.print_string conf "</table>\n";
  Output.print_string conf "<p>\n";
  Output.printf conf "%s%s %d %s" (Utf8.capitalize_fst (transl conf "total"))
    (Util.transl conf ":")
    !nb_pers
    (transl_nth conf "person/persons" 1);
  Output.print_string conf "</p>\n";
  Hutil.trailer conf

let make_tree_hts conf base gv p =
  let bd =
    match Util.p_getint conf.env "bd" with
      Some x -> x
    | None -> 0
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
      Some x -> " " ^ x
    | _ ->
        match Util.p_getenv conf.env "color" with
          None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\""
  in
  let rec nb_column n v u =
    if v = 0 then n + max 1 (Array.length (get_family u))
    else if Array.length (get_family u) = 0 then n + 1
    else
      Array.fold_left
        (fun n ifam -> fam_nb_column n v (foi base ifam)) n
        (get_family u)
  and fam_nb_column n v des =
    if Array.length (get_children des) = 0 then n + 1
    else
      Array.fold_left
        (fun n iper -> nb_column n (v - 1) (pget conf base iper))
        n (get_children des)
  in
  let vertical_bar_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    let td =
      match po with
        Some (p, _) ->
          (* Récupère les options d'affichage. *)
          let options = Util.display_options conf in
          let ncol = nb_column 0 (v - 1) p in
          let vbar_txt =
            Printf.sprintf "%sm=D&t=T&v=%d&%s&%s" (commd conf) gv options
              (acces conf base p)
          in
          2 * ncol - 1, CenterA, TDbar (Some vbar_txt)
      | None -> 1, LeftA, TDnothing
    in
    td :: tdl
  in
  let children_vertical_bars v gen =
    let tdl = List.fold_left (vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let spouses_vertical_bar_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    match po with
    | Some (p, _) when Array.length (get_family p) > 0 ->
      fst @@
      Array.fold_left
        (fun (tdl, first) ifam ->
           let tdl =
             if first then tdl else (1, LeftA, TDnothing) :: tdl
           in
           let des = foi base ifam in
           let td =
             if Array.length (get_children des) = 0 then
               1, LeftA, TDnothing
             else
               let ncol = fam_nb_column 0 (v - 1) des in
               2 * ncol - 1, CenterA, TDbar None
           in
           td :: tdl, false)
        (tdl, true) (get_family p)
    | _ -> (1, LeftA, TDnothing) :: tdl
  in
  let spouses_vertical_bar v gen =
    let tdl = List.fold_left (spouses_vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let horizontal_bar_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    match po with
    | Some (p, _) when Array.length (get_family p) > 0 ->
      fst @@
      Array.fold_left
        (fun (tdl, first) ifam ->
           let tdl =
             if first then tdl else (1, LeftA, TDnothing) :: tdl
           in
           let des = foi base ifam in
           let tdl =
             if Array.length (get_children des) = 0 then
               (1, LeftA, TDnothing) :: tdl
             else if Array.length (get_children des) = 1 then
               let u = pget conf base (get_children des).(0) in
               let ncol = nb_column 0 (v - 1) u in
               (2 * ncol - 1, CenterA, TDbar None) :: tdl
             else
               let rec loop tdl i =
                 if i = Array.length (get_children des) then tdl
                 else
                   let iper = (get_children des).(i) in
                   let u = pget conf base iper in
                   let tdl =
                     if i > 0 then
                       let align = CenterA in
                       (1, align, TDhr align) :: tdl
                     else tdl
                   in
                   let ncol = nb_column 0 (v - 1) u in
                   let align =
                     if i = 0 then RightA
                     else if i = Array.length (get_children des) - 1 then
                       LeftA
                     else CenterA
                   in
                   let td = 2 * ncol - 1, align, TDhr align in
                   loop (td :: tdl) (i + 1)
               in
               loop tdl 0
           in
           tdl, false)
        (tdl, true) (get_family p)
    | _ -> (1, LeftA, TDnothing) :: tdl
  in
  let horizontal_bars v gen =
    let tdl = List.fold_left (horizontal_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let person_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    let td =
      match po with
        Some (p, auth) ->
          let ncol =
            if v > 1 then nb_column 0 (v - 1) p
            else Array.length (get_family p)
          in
          let txt = person_title_text conf base p in
          let txt = reference conf base p txt in
          let txt =
            if auth then txt ^ DateDisplay.short_dates_text conf base p else txt
          in
          let txt = txt ^ DagDisplay.image_txt conf base p in
          let txt =
            if bd > 0 || td_prop <> "" then
              Printf.sprintf
                "<table style=\"border: %dpx solid\"><tr><td align=\"center\"%s>%s</td>\
                 </tr></table>"
                bd td_prop txt
            else txt
          in
          2 * ncol - 1, CenterA, TDitem txt
      | None -> 1, LeftA, TDnothing
    in
    td :: tdl
  in
  let spouses_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    match po with
      Some (p, auth) when Array.length (get_family p) > 0 ->
        let rec loop tdl i =
          if i = Array.length (get_family p) then tdl
          else
            let ifam = (get_family p).(i) in
            let tdl =
              if i > 0 then (1, LeftA, TDtext "...") :: tdl else tdl
            in
            let td =
              let fam = foi base ifam in
              let ncol = if v > 1 then fam_nb_column 0 (v - 1) fam else 1 in
              let s =
                let sp = pget conf base (Gutil.spouse (get_iper p) fam) in
                let txt = person_title_text conf base sp in
                let txt = reference conf base sp txt in
                let txt =
                  if auth then txt ^ DateDisplay.short_dates_text conf base sp
                  else txt
                in
                "&amp;" ^
                (if auth then DateDisplay.short_marriage_date_text conf base fam p sp
                 else "") ^
                "&nbsp;" ^ txt ^ DagDisplay.image_txt conf base sp
              in
              let s =
                if bd > 0 || td_prop <> "" then
                  Printf.sprintf "<table style=\"border: %dpx solid\"><tr>\
                     <td align=\"center\"%s>%s</td></tr></table>"
                    bd td_prop s
                else s
              in
              2 * ncol - 1, CenterA, TDitem s
            in
            loop (td :: tdl) (i + 1)
        in
        loop tdl 0
    | _ -> (1, LeftA, TDnothing) :: tdl
  in
  let next_gen gen =
    List.fold_right
      (fun po gen ->
         match po with
           Some (p, _) ->
             if Array.length (get_family p) = 0 then None :: gen
             else
               Array.fold_right
                 (fun ifam gen ->
                    let des = foi base ifam in
                    if Array.length (get_children des) = 0 then None :: gen
                    else
                      let age_auth =
                        Array.for_all
                          (fun ip -> authorized_age conf base (pget conf base ip))
                          (get_children des)
                      in
                      Array.fold_right
                        (fun iper gen ->
                           let g = pget conf base iper, age_auth in
                           Some g :: gen)
                        (get_children des) gen)
                 (get_family p) gen
         | None -> None :: gen)
      gen []
  in
  let tdal =
    let rec loop tdal prev_gen gen v =
      let tdal =
        if prev_gen <> [] then
          children_vertical_bars v gen :: horizontal_bars v prev_gen ::
          spouses_vertical_bar (v + 1) prev_gen :: tdal
        else tdal
      in
      let tdal =
        let tdl = List.fold_left (person_txt v) [] gen in
        Array.of_list (List.rev tdl) :: tdal
      in
      let tdal =
        let tdl = List.fold_left (spouses_txt v) [] gen in
        Array.of_list (List.rev tdl) :: tdal
      in
      if v > 1 then loop tdal gen (next_gen gen) (v - 1) else tdal
    in
    loop [] [] [Some (p, true)] (gv + 1)
  in
  Array.of_list (List.rev tdal)

let print_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    translate_eval
      (let s = person_text_no_html conf base p in
       transl_a_of_gr_eq_gen_lev conf (transl conf "descendants") s s)
  in
  let hts = make_tree_hts conf base gv p in
  DagDisplay.print_slices_menu_or_dag_page conf page_title hts ""

let print_aboville conf base max_level p =
  let max_level = min (Perso.limit_desc conf) max_level in
  let num_aboville = p_getenv conf.env "num" = Some "on" in
  Hutil.header conf (descendants_title conf base p);
  Hutil.print_link_to_welcome conf true;
  Output.printf conf "%s.<br><p>" (Utf8.capitalize_fst (text_to conf max_level));
  let rec loop_ind lev lab p =
    if num_aboville then Output.printf conf "<tt>%s</tt>\n" lab
    else Output.printf conf "%s\n" lab;
    Output.printf conf "%s%s\n" (referenced_person_title_text conf base p)
      (DateDisplay.short_dates_text conf base p);
    let u = p in
    if lev < max_level then
      for i = 0 to Array.length (get_family u) - 1 do
        let cpl = foi base (get_family u).(i) in
        let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
        let mdate =
          if authorized_age conf base p && authorized_age conf base spouse
          then
            let fam = foi base (get_family u).(i) in
            match Adef.od_of_cdate (get_marriage fam) with
              Some (Dgreg (d, _)) ->
                let date = DateDisplay.prec_year_text conf d in
                "<font size=\"-2\"><em>" ^ date ^ "</em></font>"
            | _ -> ""
          else ""
        in
        Output.printf conf "&amp;%s %s%s\n" mdate
          (referenced_person_title_text conf base spouse)
          (DateDisplay.short_dates_text conf base spouse)
      done;
    Output.print_string conf "<br>\n";
    if lev < max_level then
      let rec loop_fam cnt_chil i =
        if i = Array.length (get_family u) then ()
        else
          let des = foi base (get_family u).(i) in
          let rec loop_chil cnt_chil j =
            if j = Array.length (get_children des) then
              loop_fam cnt_chil (i + 1)
            else
              begin
                loop_ind (lev + 1)
                  (if num_aboville then lab ^ string_of_int cnt_chil ^ "."
                   else
                     lab ^
                     "<span class=\"descends_aboville_pipe\">&nbsp;</span>")
                  (pget conf base (get_children des).(j));
                loop_chil (cnt_chil + 1) (j + 1)
              end
          in
          loop_chil cnt_chil 0
      in
      loop_fam 1 0
  in
  loop_ind 0 "" p; Hutil.trailer conf

let desmenu_print = Perso.interp_templ "desmenu"

let print conf base p =
  let templ =
    match p_getenv conf.env "t" with
      Some ("F" | "L" | "M") -> "deslist"
    | Some "D" -> "deslist_hr"
    | Some ("H" | "I" | "A") -> "destable"
    | Some "V" -> "destree"
    | Some _ -> ""
    | _ -> "desmenu"
  in
  if templ <> "" then Perso.interp_templ templ conf base p
  else
    match p_getenv conf.env "t", p_getint conf.env "v" with
      Some "B", Some v -> print_aboville conf base v p
    | Some "S", Some v -> display_descendants_level conf base v p
    | Some "K", Some v -> display_descendant_with_table conf base v p
    | Some "N", Some v -> display_descendants_with_numbers ~total:(ref 0) ~last_label:(ref "") conf base v p
    | Some "G", Some v -> display_descendant_index conf base v p
    | Some "C", Some v -> display_spouse_index conf base v p
    | Some "T", Some v -> print_tree conf base v p
    | _ -> desmenu_print conf base p
