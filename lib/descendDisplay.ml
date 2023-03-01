(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Dag2html
open Gwdb
open Util

let limit_by_tree conf =
  match List.assoc_opt "max_desc_tree" conf.base_env with
  | None -> 4
  | Some x -> ( try max 1 (int_of_string x) with _ -> 4)

let text_to conf = function
  | 0 ->
      transl_nth conf "generation/generations" 0
      |> transl_decline conf "specify"
      |> Adef.safe
  | 1 -> transl conf "to the children" |> Adef.safe
  | 2 -> transl conf "to the grandchildren" |> Adef.safe
  | 3 -> transl conf "to the great-grandchildren" |> Adef.safe
  | i ->
      Printf.sprintf
        (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i)
      |> Adef.safe

let text_level conf = function
  | 0 ->
      transl_nth conf "generation/generations" 0
      |> transl_decline conf "specify"
      |> Adef.safe
  | 1 -> transl conf "the children" |> Adef.safe
  | 2 -> transl conf "the grandchildren" |> Adef.safe
  | 3 -> transl conf "the great-grandchildren" |> Adef.safe
  | i ->
      Printf.sprintf
        (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i)
      |> Adef.safe

let descendants_title conf base p h =
  let s1 = gen_person_text conf base p in
  let s2 = if h then gen_person_text ~html:false conf base p else s1 in
  translate_eval
    (transl_a_of_gr_eq_gen_lev conf
       (transl conf "descendants")
       (s1 :> string)
       (s2 :> string))
  |> Utf8.capitalize_fst |> Output.print_sstring conf

let display_descendants_level conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let levt, _ = Perso.make_desc_level_table conf base max_level ancestor in
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
                if p_first_name base x = "x" || Gwdb.Marker.get levt ix != level
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
          let c =
            Gutil.alphabetic (p_first_name base p2) (p_first_name base p1)
          in
          if c = 0 then compare (get_occ p2) (get_occ p1) else c
        else c)
      list
  in
  let list =
    List.fold_left
      (fun pl p ->
        match pl with
        | (p1, n) :: pl when get_iper p = get_iper p1 -> (p1, succ n) :: pl
        | _ ->
            incr len;
            (p, 1) :: pl)
      [] list
  in
  Hutil.header conf (descendants_title conf base ancestor);
  (text_level conf max_level : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  if !len > 1 then (
    Output.print_sstring conf " (";
    Output.print_sstring conf (string_of_int !len);
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
    Output.print_sstring conf ")");
  Output.print_sstring conf ".<p>";
  print_alphab_list conf
    (fun (p, _) ->
      if is_hidden p then "?"
      else String.sub (p_surname base p) (Mutil.initial (p_surname base p)) 1)
    (fun (p, c) ->
      Output.print_sstring conf " ";
      Output.print_string conf (referenced_person_title_text conf base p);
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      if (not (is_hidden p)) && c > 1 then Output.printf conf " <em>(%d)</em>" c;
      Output.print_sstring conf "\n")
    list;
  Hutil.trailer conf

(* With number *)

let mark_descendants conf base marks max_lev ip =
  let rec loop lev ip u =
    if lev <= max_lev then (
      Gwdb.Marker.set marks ip true;
      Array.iter
        (fun ifam ->
          let el = get_children (foi base ifam) in
          Array.iter (fun e -> loop (succ lev) e (pget conf base e)) el)
        (get_family u))
  in
  loop 0 ip (pget conf base ip)

let label_descendants conf base marks paths max_lev =
  let rec loop path lev p =
    if lev < max_lev then
      ignore
      @@ Array.fold_left
           (fun cnt ifam ->
             let fam = foi base ifam in
             let c = Gutil.spouse (get_iper p) fam in
             let el = get_children fam in
             Array.fold_left
               (fun cnt e ->
                 if get_sex p = Male || not (Gwdb.Marker.get marks c) then (
                   let path = Char.chr (Char.code 'A' + cnt) :: path in
                   Gwdb.Marker.set paths e path;
                   loop path (succ lev) (pget conf base e));
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
  Array.length (get_family u) <> 0
  &&
  match get_parents a with
  | Some ifam ->
      let fam = foi base ifam in
      let el = get_children fam in
      Array.exists
        (fun ie ->
          let e = pget conf base ie in
          let u = e in
          Array.length (get_family u) <> 0
          && not (close_to_end conf base marks max_lev lev e))
        el
  | _ -> false

let label_of_path paths p =
  let rec loop = function
    | [] -> Adef.escaped ""
    | c :: cl -> loop cl ^^^ Util.escape_html (String.make 1 c)
  in
  get_iper p |> Gwdb.Marker.get paths |> loop

let print_child conf base p1 p2 e =
  Output.print_sstring conf "<strong>";
  if
    (get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1))
    || (get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2))
  then
    Output.print_string conf
      (referenced_person_text_without_surname conf base e)
  else (
    Output.print_sstring conf " ";
    Output.print_string conf (referenced_person_text conf base e));
  Output.print_sstring conf "</strong>";
  Output.print_string conf (DateDisplay.short_dates_text conf base e)

let print_repeat_child conf base p1 p2 e =
  Output.print_sstring conf "<em>";
  if
    (get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1))
    || (get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2))
  then Output.print_string conf (gen_person_text ~sn:false conf base e)
  else Output.print_string conf (gen_person_text conf base e);
  Output.print_sstring conf "</em>"

let display_spouse conf base marks paths fam p c =
  Output.print_sstring conf " &amp;";
  Output.print_string conf
    (DateDisplay.short_marriage_date_text conf base fam p c);
  Output.print_sstring conf " <strong> ";
  Output.print_string conf (referenced_person_text conf base c);
  Output.print_sstring conf "</strong>";
  if Gwdb.Marker.get marks (get_iper c) then (
    Output.print_sstring conf " (<tt><b>";
    Output.print_string conf (label_of_path paths c);
    Output.print_sstring conf "</b></tt>)")
  else Output.print_string conf (DateDisplay.short_dates_text conf base c)

let total = ref 0

let print_family_locally conf base marks paths max_lev lev p1 c1 e =
  let rec loop lev p =
    if lev < max_lev then
      ignore
      @@ Array.fold_left
           (fun (cnt, first, need_br) ifam ->
             let fam = foi base ifam in
             let c = Gutil.spouse (get_iper p) fam in
             let el = get_children fam in
             let c = pget conf base c in
             if need_br then Output.print_sstring conf "<br>";
             if not first then print_repeat_child conf base p1 c1 p;
             display_spouse conf base marks paths fam p c;
             Output.print_sstring conf "\n";
             let print_children =
               get_sex p = Male || not (Gwdb.Marker.get marks (get_iper c))
             in
             if print_children then
               Output.printf conf "<ol start=\"%d\">\n" (succ cnt);
             let cnt =
               Array.fold_left
                 (fun cnt ie ->
                   let e = pget conf base ie in
                   if print_children then (
                     Output.print_sstring conf "<li type=\"A\"> ";
                     print_child conf base p c e;
                     Output.print_sstring conf "\n";
                     incr total;
                     if succ lev = max_lev then
                       Array.iteri
                         (fun i ifam ->
                           let fam = foi base ifam in
                           let c1 = Gutil.spouse ie fam in
                           let el = get_children fam in
                           let c1 = pget conf base c1 in
                           if i <> 0 then (
                             Output.print_sstring conf "<br>";
                             print_repeat_child conf base p c e);
                           display_spouse conf base marks paths fam e c1;
                           if Array.length el <> 0 then
                             Output.print_sstring conf ".....";
                           Output.print_sstring conf "\n")
                         (get_family (pget conf base ie))
                     else loop (succ lev) e);
                   succ cnt)
                 cnt el
             in
             if print_children then Output.print_sstring conf "</ol>\n";
             (cnt, false, not print_children))
           (0, true, false) (get_family p)
  in
  loop lev e

let last_label = ref (Adef.escaped "")

let print_family conf base marks paths max_lev lev p =
  if lev <> 0 then (
    Output.print_sstring conf "<tt><b>";
    Output.print_string conf (label_of_path paths p);
    Output.print_sstring conf "</b></tt>.<br>");
  let lab = label_of_path paths p in
  if lab < !last_label then failwith "print_family" else last_label := lab;
  ignore
  @@ Array.fold_left
       (fun cnt ifam ->
         let fam = foi base ifam in
         let c = Gutil.spouse (get_iper p) fam in
         let el = get_children fam in
         let c = pget conf base c in
         Output.print_sstring conf "<strong> ";
         Output.print_string conf (referenced_person_text conf base p);
         Output.print_sstring conf "</strong>";
         display_spouse conf base marks paths fam p c;
         Output.print_sstring conf {|<ol start="|};
         Output.print_sstring conf (succ cnt |> string_of_int);
         Output.print_sstring conf {|">|};
         let cnt =
           Array.fold_left
             (fun cnt ie ->
               let e = pget conf base ie in
               if get_sex p = Male || not (Gwdb.Marker.get marks (get_iper c))
               then (
                 Output.print_sstring conf {|<li type="A">|};
                 print_child conf base p c e;
                 incr total;
                 Output.print_sstring conf " ";
                 if labelled conf base marks max_lev lev ie then (
                   Output.print_sstring conf " =&gt; <tt><b>";
                   Output.print_string conf (label_of_path paths e);
                   Output.print_sstring conf "</b></tt> ")
                 else if succ lev = max_lev then
                   Array.iter
                     (fun ifam ->
                       let fam = foi base ifam in
                       let c = Gutil.spouse ie fam in
                       let el = get_children fam in
                       let c = pget conf base c in
                       display_spouse conf base marks paths fam e c;
                       if Array.length el <> 0 then
                         Output.print_sstring conf ".....";
                       Output.print_sstring conf "\n")
                     (get_family (pget conf base ie))
                 else
                   print_family_locally conf base marks paths max_lev (succ lev)
                     p c e);
               succ cnt)
             cnt el
         in
         Output.print_sstring conf "</ol>";
         cnt)
       0 (get_family p)

let print_families conf base marks paths max_lev =
  let rec loop lev p =
    if lev < max_lev then (
      print_family conf base marks paths max_lev lev p;
      Array.iter
        (fun ifam ->
          let fam = foi base ifam in
          let c = Gutil.spouse (get_iper p) fam in
          let el = get_children fam in
          let c = pget conf base c in
          if get_sex p = Male || not (Gwdb.Marker.get marks (get_iper c)) then
            Array.iter
              (fun ie ->
                let e = pget conf base ie in
                if labelled conf base marks max_lev lev ie then
                  loop (succ lev) e)
              el)
        (get_family p))
  in
  loop 0

let display_descendants_with_numbers conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    if h then descendants_title conf base ancestor h
    else
      wprint_geneweb_link conf
        ("m=D&i="
         ^ string_of_iper (get_iper ancestor)
         ^ "&v=" ^ string_of_int max_level ^ "&t=G"
        |> Adef.escaped)
        (let s1 = gen_person_text conf base ancestor in
         let s2 = gen_person_text ~html:true conf base ancestor in
         transl_a_of_gr_eq_gen_lev conf
           (transl conf "descendants")
           (s1 : Adef.safe_string :> string)
           (s2 : Adef.safe_string :> string)
         |> Utf8.capitalize_fst |> Adef.safe)
  in
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  Hutil.header conf title;
  total := 0;
  Output.print_string conf (DateDisplay.short_dates_text conf base ancestor);
  let p = ancestor in
  (if authorized_age conf base p then
   match (Date.od_of_cdate (get_birth p), get_death p) with
   | Some _, _ | _, Death (_, _) -> Output.print_sstring conf "<br>"
   | _ -> ());
  (text_to conf max_level : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf ".<p>";
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  print_families conf base marks paths max_level ancestor;
  if !total > 1 then (
    Output.print_sstring conf "<p>";
    Output.printf conf "%s%s %d %s"
      (Utf8.capitalize_fst (transl conf "total"))
      (Util.transl conf ":") !total
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
    if max_level > 1 then
      Output.printf conf " (%s)" (transl conf "spouses not included");
    Output.print_sstring conf ".\n");
  Hutil.trailer conf

let print_ref conf base paths p =
  if Gwdb.Marker.get paths (get_iper p) <> [] then (
    Output.print_sstring conf " =&gt; <tt><b>";
    Output.print_string conf (label_of_path paths p);
    Output.print_sstring conf "</b></tt>")
  else
    Array.iter
      (fun ifam ->
        let c = Gutil.spouse (get_iper p) (foi base ifam) in
        if Gwdb.Marker.get paths c <> [] then (
          let c = pget conf base c in
          Output.print_sstring conf " =&gt; ";
          Output.print_string conf (p_first_name base c |> Util.escape_html);
          Output.print_sstring conf " ";
          Output.print_string conf (p_surname base c |> Util.escape_html);
          Output.print_sstring conf " <tt><b>";
          Output.print_string conf (label_of_path paths c);
          Output.print_sstring conf "</b></tt>"))
      (get_family p)

let print_elem conf base paths precision (n, pll) =
  Output.print_sstring conf "<li>";
  match List.rev pll with
  | [ [ p ] ] ->
      Output.print_sstring conf "<strong>";
      Output.print_string conf
        (surname_without_particle base n |> Util.escape_html);
      Output.print_sstring conf " ";
      gen_person_text ~sn:false conf base p
      (* FIXME où est ce reference ?? <> au reference l.1384 *)
      |> reference conf base p
      |> Output.print_string conf;
      Output.print_sstring conf " ";
      Output.print_string conf (surname_particle base n |> Util.escape_html);
      Output.print_sstring conf "</strong>";
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      print_ref conf base paths p;
      Output.print_sstring conf "\n"
  | pll ->
      Output.print_sstring conf "<strong>";
      Output.print_string conf
        (surname_without_particle base n |> Util.escape_html);
      Output.print_string conf (surname_particle base n |> Util.escape_html);
      Output.print_sstring conf "</strong><ul>";
      List.iter
        (fun pl ->
          let several = match pl with [ _ ] -> false | _ -> true in
          List.iter
            (fun p ->
              Output.print_sstring conf "<li><strong>";
              wprint_geneweb_link conf (acces conf base p)
                (p_first_name base p |> Util.escape_html :> Adef.safe_string);
              Output.print_sstring conf "</strong>";
              if several && precision then (
                Output.print_sstring conf "<em>";
                specify_homonymous conf base p true;
                Output.print_sstring conf "</em>");
              Output.print_string conf
                (DateDisplay.short_dates_text conf base p);
              print_ref conf base paths p)
            pl)
        pll;
      Output.print_sstring conf "</ul>"

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
        | (n, pl) :: npll when n = p_surname base p -> (n, p :: pl) :: npll
        | _ -> (p_surname base p, [ p ]) :: npll)
      [] list
  in
  let list =
    List.map
      (fun (n, pl) ->
        let pll =
          List.fold_left
            (fun pll p ->
              match pll with
              | (p1 :: _ as pl) :: pll
                when eq_istr (get_first_name p1) (get_first_name p) ->
                  (p :: pl) :: pll
              | _ -> [ p ] :: pll)
            [] pl
        in
        (n, pll))
      list
  in
  if list <> [] then (
    Output.print_sstring conf "<ul>\n";
    List.iter (print_elem conf base paths precision) list;
    Output.print_sstring conf "</ul>\n")

let display_descendant_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    let txt =
      transl conf "index of the descendants" |> Utf8.capitalize_fst |> Adef.safe
    in
    if not h then
      wprint_geneweb_link conf
        ("m=D&i="
         ^ string_of_iper (get_iper ancestor)
         ^ "&v=" ^ string_of_int max_level ^ "&t=C"
        |> Adef.escaped)
        txt
    else Output.print_string conf txt
  in
  Hutil.header conf title;
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = pget conf base i in
        if
          p_first_name base p <> "?"
          && p_surname base p <> "?"
          && p_first_name base p <> "x"
          && ((not (is_hide_names conf p)) || authorized_age conf base p)
        then get_iper p :: acc
        else acc)
      [] (ipers base)
  in
  sort_and_display conf base paths true list;
  Hutil.trailer conf

let display_spouse_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title _ =
    transl conf "index of the spouses (non descendants)"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  let marks = Gwdb.iper_marker (Gwdb.ipers base) false in
  let paths = Gwdb.iper_marker (Gwdb.ipers base) [] in
  mark_descendants conf base marks max_level (get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = pget conf base i in
        if Gwdb.Marker.get paths i <> [] then
          if
            p_first_name base p <> "?"
            && p_surname base p <> "?"
            && p_first_name base p <> "x"
          then
            Array.fold_left
              (fun acc ifam ->
                let c = Gutil.spouse (get_iper p) (foi base ifam) in
                if Gwdb.Marker.get paths c = [] then
                  let c = pget conf base c in
                  if
                    p_first_name base c <> "?"
                    && p_surname base c <> "?"
                    && p_first_name base p <> "x"
                    && ((not (is_hide_names conf c))
                       || authorized_age conf base c)
                    && not (List.mem (get_iper c) acc)
                  then get_iper c :: acc
                  else acc
                else acc)
              acc (get_family p)
          else acc
        else acc)
      [] (ipers base)
  in
  sort_and_display conf base paths false list;
  Hutil.trailer conf

(* *********************************************************************** *)
(*  [Fonc] print_desc_table_header : config -> base -> int                 *)

(* *********************************************************************** *)

(** [Description] : Affiche en fonction des options qui sont sélectionnées
                    le header du tableau de descendance.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Le nombre de colonnes à afficher (nombre d'options sélectionnées).
    [Rem] : Non exporté en clair hors de ce module.                        *)
let print_desc_table_header conf =
  let nb_col = ref 2 in
  Output.print_sstring conf {|<tr class="descends_table_header"><th>|};
  transl conf "n° d'Aboville"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</th><th>";
  transl_nth conf "person/persons" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</th>";
  let aux get txt =
    if p_getenv conf.env get = Some "on" then (
      Output.print_sstring conf "<th>";
      incr nb_col;
      txt |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf "</th>")
  in
  aux "birth" (transl conf "date of birth");
  aux "birth_place" (transl conf "where born");
  aux "marr" (transl_nth conf "spouse/spouses" 1);
  aux "marr_date" (transl conf "date of marriage");
  aux "marr_place" (transl conf "where married");
  aux "child" (transl conf "nb children");
  aux "death" (transl conf "date of death");
  aux "death_place" (transl conf "where dead");
  aux "death_age" (transl conf "age at death");
  aux "occu" (transl_nth conf "occupation/occupations" 1);
  Output.print_sstring conf "</tr>";
  !nb_col

(* *********************************************************************** *)
(*  [Fonc] print_person_table : config -> base -> person -> string -> unit *)

(* *********************************************************************** *)

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
let print_person_table conf base p lab =
  let p_auth = Util.authorized_age conf base p in
  let nb_families = Array.length (get_family p) in
  let birth, birth_place =
    if
      p_auth
      && (p_getenv conf.env "birth" = Some "on"
         || p_getenv conf.env "birth_place" = Some "on")
    then
      let date, place = Util.get_approx_birth_date_place conf base p in
      let date =
        match date with
        | Some d -> DateDisplay.string_slash_of_date conf d
        | None -> Adef.safe ""
      in
      (date, place)
    else (Adef.safe "&nbsp;", Adef.safe "")
  in
  let death, death_place =
    if
      p_auth
      && (p_getenv conf.env "death" = Some "on"
         || p_getenv conf.env "death_place" = Some "on")
    then
      let date, place = Util.get_approx_death_date_place conf base p in
      let date =
        match date with
        | Some d -> DateDisplay.string_slash_of_date conf d
        | None -> Adef.safe ""
      in
      (date, place)
    else (Adef.safe "&nbsp;", Adef.safe "")
  in
  (* On calcul le nombre de rowspan pour avoir un affichage joli. *)
  let rowspan =
    if
      nb_families > 1
      && (p_getenv conf.env "marr" = Some "on"
         || p_getenv conf.env "marr_date" = Some "on"
         || p_getenv conf.env "marr_place" = Some "on")
    then Adef.safe ("rowspan=\"" ^ string_of_int nb_families ^ "\"")
    else Adef.safe ""
  in
  let td txt =
    Output.print_sstring conf "<td ";
    Output.print_string conf rowspan;
    Output.print_sstring conf ">";
    txt ();
    Output.print_sstring conf "</td>"
  in
  (* On met partout un &nbsp; dans le cas où ce que l'on souhaite *)
  (* afficher est vide, comme ça, on ne casse pas le rowspan.     *)
  Output.print_sstring conf "<tr>";
  td (fun () -> Output.print_string conf lab);
  td (fun () ->
      ImageDisplay.print_placeholder_gendered_portrait conf p 11;
      Output.print_sstring conf " ";
      Output.print_string conf (referenced_person_title_text conf base p);
      Output.print_sstring conf "&nbsp;");
  if p_getenv conf.env "birth" = Some "on" then
    td (fun () -> Output.print_string conf birth);
  if p_getenv conf.env "birth_place" = Some "on" then
    td (fun () ->
        Output.print_string conf birth_place;
        Output.print_sstring conf "&nbsp;");
  let aux ?alt ?attr gets f =
    if List.exists (fun get -> p_getenv conf.env get = Some "on") gets then (
      Output.print_sstring conf "<td";
      (match attr with
      | Some attr ->
          (* TODO?: why is only the last string used? *)
          let attr =
            List.fold_left
              (fun _acc (a, v) -> " " ^ a ^ "=" ^ "\"" ^ v ^ "\"")
              "" attr
          in
          Output.print_sstring conf attr
      | None -> ());
      if nb_families > 1 then
        Output.print_sstring conf {| style="border-bottom:none"|};
      Output.print_sstring conf ">";
      if nb_families > 0 then
        let fam = foi base (get_family p).(0) in
        let spouse = pget conf base (Gutil.spouse (get_iper p) fam) in
        f fam spouse
      else Output.print_sstring conf "&nbsp;";
      Output.print_sstring conf "</td>")
    else match alt with None -> () | Some fn -> fn ()
  in
  aux [ "marr" ] (fun _fam spouse ->
      ImageDisplay.print_placeholder_gendered_portrait conf spouse 11;
      Output.print_sstring conf " ";
      Output.print_string conf (referenced_person_text conf base spouse);
      Output.print_sstring conf " &nbsp;");
  aux [ "marr_date" ] (fun fam spouse ->
      let mdate =
        if authorized_age conf base p && authorized_age conf base spouse then
          match Date.od_of_cdate (get_marriage fam) with
          | Some d -> DateDisplay.string_slash_of_date conf d
          | None -> Adef.safe "&nbsp;"
        else Adef.safe "&nbsp;"
      in
      Output.print_string conf mdate);
  aux [ "marr_place" ] (fun fam spouse ->
      if authorized_age conf base p && authorized_age conf base spouse then
        get_marriage_place fam |> sou base |> Util.string_of_place conf
        |> Output.print_string conf;
      Output.print_sstring conf " &nbsp;");
  aux [ "child" ]
    ~attr:[ ("align", "center") ]
    (fun fam _spouse ->
      Output.print_sstring conf
        (get_children fam |> Array.length |> string_of_int);
      Output.print_sstring conf " &nbsp;");
  if p_getenv conf.env "death" = Some "on" then
    td (fun () -> Output.print_string conf death);
  if p_getenv conf.env "death_place" = Some "on" then
    td (fun () ->
        Output.print_string conf death_place;
        Output.print_sstring conf " &nbsp;");
  if p_getenv conf.env "death_age" = Some "on" then
    td (fun () ->
        (if p_auth then
         match Gutil.get_birth_death_date p with
         | ( Some (Dgreg (({ prec = Sure | About | Maybe } as d1), _)),
             Some (Dgreg (({ prec = Sure | About | Maybe } as d2), _)),
             approx )
           when d1 <> d2 ->
             if not ((not approx) && d1.prec = Sure && d2.prec = Sure) then (
               transl_decline conf "possibly (date)" ""
               |> Output.print_sstring conf;
               Output.print_sstring conf " ");
             Date.time_elapsed d1 d2
             |> DateDisplay.string_of_age conf
             |> Output.print_string conf
         | _ -> ());
        Output.print_sstring conf " &nbsp;");
  if p_getenv conf.env "occu" = Some "on" then
    td (fun () ->
        if p_auth then
          get_occupation p |> sou base |> Util.escape_html
          |> Output.print_string conf;
        Output.print_sstring conf " &nbsp;");
  Output.print_sstring conf "</tr>";
  (* Maintenant qu'on a fini d'afficher le <tr> complet, si il y a  *)
  (* plusieurs familles, il faut alors afficher chacune d'elle dans *)
  (* un <tr> afin d'avoir une mise en page utilisant des rowspan.   *)
  if nb_families > 1 then
    if
      p_getenv conf.env "marr" = Some "on"
      || p_getenv conf.env "marr_date" = Some "on"
      || p_getenv conf.env "marr_place" = Some "on"
    then
      let aux ?attr i get fn =
        if p_getenv conf.env get = Some "on" then (
          Output.print_sstring conf {|<td style="border-top:none; |};
          if nb_families - 1 <> i then
            Output.print_sstring conf "border-bottom:none;";
          Output.print_sstring conf "\"";
          (match attr with
          | Some attr ->
              (* TODO?: why is only the last string used? *)
              let attr =
                List.fold_left
                  (fun _acc (a, v) -> " " ^ a ^ "=" ^ "\"" ^ v ^ "\"")
                  "" attr
              in
              Output.print_sstring conf attr
          | None -> ());
          Output.print_sstring conf {|>|};
          fn ();
          Output.print_sstring conf "</td>")
      in
      let u = p in
      for i = 1 to nb_families - 1 do
        let cpl = foi base (get_family u).(i) in
        let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
        let fam = foi base (get_family u).(i) in
        Output.print_sstring conf "<tr>\n";
        aux i "marr" (fun () ->
            ImageDisplay.print_placeholder_gendered_portrait conf spouse 11;
            Output.print_sstring conf " ";
            Output.print_string conf (referenced_person_text conf base spouse);
            Output.print_sstring conf "&nbsp;");
        aux i "marr_date" (fun () ->
            if authorized_age conf base p && authorized_age conf base spouse
            then
              let fam = foi base (get_family u).(i) in
              match Date.od_of_cdate (get_marriage fam) with
              | Some d ->
                  DateDisplay.string_slash_of_date conf d
                  |> Output.print_string conf
              | None -> Output.print_sstring conf "&nbsp;"
            else Output.print_sstring conf "&nbsp;");
        aux i "marr_place" (fun () ->
            if authorized_age conf base p && authorized_age conf base spouse
            then
              get_marriage_place cpl |> sou base |> Util.string_of_place conf
              |> Output.print_string conf;
            Output.print_sstring conf " &nbsp;");
        aux
          ~attr:[ ("align", "center") ]
          i "child"
          (fun () ->
            Output.print_sstring conf
              (get_children fam |> Array.length |> string_of_int);
            Output.print_sstring conf " &nbsp;");
        Output.print_sstring conf "</tr>"
      done

(* ********************************************************************** *)
(*  [Fonc] build_desc : config -> base -> person list -> person list      *)

(* ********************************************************************** *)

(** [Description] : Construit la liste des descendants de la liste des
                    personnes (passée en paramètre). Correspond à un
                    parcours en largeur.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - l    : person list
    [Retour] : person list
    [Rem] : Non exporté en clair hors de ce module.                       *)
let build_desc conf base l : ('a * Adef.safe_string) list =
  let rec loop l accu =
    match l with
    | [] ->
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
                  (pget conf base ip, lab ^>^ string_of_int !cnt ^ ".") :: accu)
                accu (get_children fam))
            accu (get_family p)
        in
        loop l nx_accu
  in
  loop l []

(* ********************************************************************** *)
(* [Fonc] display_descendant_with_table :
     config -> base -> int -> person -> unit *)

(* ********************************************************************** *)

(** [Description] : Affiche sous la forme d'un tableau la descendance
                    d'une personne.
    [Args] :
      - conf    : configuration de la base
      - base    : base de donnée
      - max_lev : le nombre de générations à afficher
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                       *)
let display_descendant_with_table conf base max_lev p =
  let max_lev = min (Perso.limit_desc conf) max_lev in
  let nb_pers = ref 0 in
  (* Affiche la liste des personnes, génération par génération.    *)
  (* On affiche toutes les personnes de l, et quand la liste est   *)
  (* vide, on construit la list des descendants de l (mais comme l *)
  (* est vide, on les calcul par rapport à refl)                   *)
  let rec loop lev nb_col first refl l =
    match l with
    | [] ->
        if lev < max_lev then
          let nl = build_desc conf base refl in
          loop (lev + 1) nb_col true nl nl
    | (p, (lab : Adef.safe_string)) :: q ->
        if first && lev > 0 && p_getenv conf.env "gen" = Some "on" then (
          Output.print_sstring conf "<tr>";
          Output.print_sstring conf {|<th align="left" colspan="|};
          Output.print_sstring conf (string_of_int nb_col);
          Output.print_sstring conf {|">|};
          transl_nth conf "generation/generations" 0
          |> Utf8.capitalize_fst |> Output.print_sstring conf;
          Output.print_sstring conf " ";
          Output.print_sstring conf (string_of_int lev);
          Output.print_sstring conf "</th></tr>");
        print_person_table conf base p lab;
        incr nb_pers;
        loop lev nb_col false refl q
  in
  Hutil.header_fluid conf (descendants_title conf base p);
  Output.print_sstring conf "<p>";
  (text_to conf max_lev : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|.</p><table class="descends_table">|};
  (* On affiche l'entête et on en profite pour récupèrer *)
  (* le nombre de colonnes à afficher pour les colspans. *)
  loop 0
    (print_desc_table_header conf)
    true
    [ (p, Adef.safe "") ]
    [ (p, Adef.safe "") ];
  Output.print_sstring conf "</table><p>";
  transl conf "total" |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf (string_of_int !nb_pers);
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "person/persons" 1);
  Output.print_sstring conf "</p>";
  Hutil.trailer conf

let make_tree_hts conf base gv p =
  let bd = match Util.p_getint conf.env "bd" with Some x -> x | None -> 0 in
  let sps =
    match Util.p_getenv conf.env "sp" with Some "on" -> true | _ -> false
  in
  let _marr =
    match Util.p_getenv conf.env "ma" with Some "on" -> true | _ -> false
  in
  let img =
    match Util.p_getenv conf.env "im" with Some "off" -> false | _ -> true
  in
  let _cgl =
    match Util.p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  let td_prop =
    match Util.p_getenv conf.env "color" with
    | None | Some "" -> Adef.safe ""
    | Some x ->
        " class=\"" ^<^ (Util.escape_html x :> Adef.safe_string) ^>^ "\""
  in
  let rec nb_column n v u =
    if v = 0 then n + max 1 (Array.length (get_family u))
    else if Array.length (get_family u) = 0 then n + 1
    else
      Array.fold_left
        (fun n ifam -> fam_nb_column n v (foi base ifam))
        n (get_family u)
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
      | Some (p, _) ->
          (* Récupère les options d'affichage. *)
          let options = Util.display_options conf in
          let ncol = nb_column 0 (v - 1) p in
          let vbar_txt =
            commd conf ^^^ "m=D&t=T&v=" ^<^ string_of_int gv ^<^ "&" ^<^ options
            ^^^ "&" ^<^ acces conf base p
          in
          ((2 * ncol) - 1, CenterA, TDbar (Some vbar_txt))
      | None -> (1, LeftA, TDnothing)
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
        fst
        @@ Array.fold_left
             (fun (tdl, first) ifam ->
               let tdl = if first then tdl else (1, LeftA, TDnothing) :: tdl in
               let des = foi base ifam in
               let td =
                 if Array.length (get_children des) = 0 then
                   (1, LeftA, TDnothing)
                 else
                   let ncol = fam_nb_column 0 (v - 1) des in
                   ((2 * ncol) - 1, CenterA, TDbar None)
               in
               (td :: tdl, false))
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
        fst
        @@ Array.fold_left
             (fun (tdl, first) ifam ->
               let tdl = if first then tdl else (1, LeftA, TDnothing) :: tdl in
               let des = foi base ifam in
               let tdl =
                 if Array.length (get_children des) = 0 then
                   (1, LeftA, TDnothing) :: tdl
                 else if Array.length (get_children des) = 1 then
                   let u = pget conf base (get_children des).(0) in
                   let ncol = nb_column 0 (v - 1) u in
                   ((2 * ncol) - 1, CenterA, TDbar None) :: tdl
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
                       let td = ((2 * ncol) - 1, align, TDhr align) in
                       loop (td :: tdl) (i + 1)
                   in
                   loop tdl 0
               in
               (tdl, false))
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
      | Some (p, auth) ->
          let ncol =
            if v > 1 then nb_column 0 (v - 1) p else Array.length (get_family p)
          in
          let txt = reference conf base p (person_title_text conf base p) in
          let txt =
            if auth then txt ^^^ DateDisplay.short_dates_text conf base p
            else txt
          in
          let txt =
            if img then txt ^^^ DagDisplay.image_txt conf base p else txt
          in
          let txt =
            if bd > 0 || (td_prop :> string) <> "" then
              {|<table style="border:|} ^<^ string_of_int bd
              ^<^ {|px solid"><tr><td align="center"|} ^<^ td_prop ^^^ {|>|}
              ^<^ txt ^>^ {|</td></tr></table>|}
            else txt
          in
          ((2 * ncol) - 1, CenterA, TDitem (get_iper p, txt, Adef.safe ""))
      | None -> (1, LeftA, TDnothing)
    in
    td :: tdl
  in
  let spouses_txt v tdl po =
    let tdl = if tdl = [] then [] else (1, LeftA, TDnothing) :: tdl in
    match po with
    | Some (p, auth) when Array.length (get_family p) > 0 ->
        let rec loop tdl i =
          if i = Array.length (get_family p) then tdl
          else
            let ifam = (get_family p).(i) in
            let tdl =
              if i > 0 then
                (1, LeftA, TDtext (Gwdb.dummy_iper, Adef.safe "...")) :: tdl
              else tdl
            in
            let td =
              let fam = foi base ifam in
              let ncol = if v > 1 then fam_nb_column 0 (v - 1) fam else 1 in
              let sp = pget conf base (Gutil.spouse (get_iper p) fam) in
              let s =
                let sp = pget conf base (Gutil.spouse (get_iper p) fam) in
                let txt =
                  reference conf base sp (person_title_text conf base sp)
                in
                let txt =
                  if auth then txt ^^^ DateDisplay.short_dates_text conf base sp
                  else txt
                in
                "&amp;"
                ^<^ (if auth then
                     DateDisplay.short_marriage_date_text conf base fam p sp
                    else Adef.safe "")
                ^^^ "&nbsp;" ^<^ txt
                ^^^ DagDisplay.image_txt conf base sp
              in
              let s =
                if bd > 0 || (td_prop :> string) <> "" then
                  {|<table style="border:|} ^<^ string_of_int bd
                  ^<^ {|px solid"><tr><td align="center" |} ^<^ td_prop
                  ^^^ {|>|} ^<^ s ^>^ {|</td></tr></table>|}
                else s
              in
              ( (2 * ncol) - 1,
                CenterA,
                TDitem (get_iper sp, s, Adef.safe "spouse_x") )
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
        | Some (p, _) ->
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
                        let g = (pget conf base iper, age_auth) in
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
          children_vertical_bars v gen
          :: horizontal_bars v prev_gen
          :: spouses_vertical_bar (v + 1) prev_gen
          :: tdal
        else tdal
      in
      let tdal =
        let tdl = List.fold_left (person_txt v) [] gen in
        Array.of_list (List.rev tdl) :: tdal
      in
      let tdal =
        if sps then
          let tdl = List.fold_left (spouses_txt v) [] gen in
          Array.of_list (List.rev tdl) :: tdal
        else tdal
      in
      if v > 1 then loop tdal gen (next_gen gen) (v - 1) else tdal
    in
    loop [] [] [ Some (p, true) ] (gv + 1)
  in
  Array.of_list (List.rev tdal)

let print_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    let s = gen_person_text ~html:false conf base p in
    translate_eval
      (transl_a_of_gr_eq_gen_lev conf
         (transl conf "descendants")
         (s : Adef.safe_string :> string)
         (s : Adef.safe_string :> string))
    |> Adef.safe
  in
  let hts = make_tree_hts conf base gv p in
  DagDisplay.print_slices_menu_or_dag_page conf base page_title hts
    (Adef.escaped "")

let print_aboville conf base max_level p =
  let max_level = min (Perso.limit_desc conf) max_level in
  let num_aboville = p_getenv conf.env "num" = Some "on" in
  Hutil.header conf (descendants_title conf base p);
  Hutil.print_link_to_welcome conf true;
  (text_to conf max_level : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf ".<br><p>";
  let rec loop_ind lev lab p =
    if num_aboville then (
      Output.print_sstring conf "<tt>";
      Output.print_string conf lab;
      Output.print_sstring conf "</tt>")
    else Output.print_string conf lab;
    Output.print_string conf (referenced_person_title_text conf base p);
    Output.print_string conf (DateDisplay.short_dates_text conf base p);
    let u = p in
    if lev < max_level then
      for i = 0 to Array.length (get_family u) - 1 do
        let cpl = foi base (get_family u).(i) in
        let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
        Output.print_sstring conf "&amp;";
        if authorized_age conf base p && authorized_age conf base spouse then
          let fam = foi base (get_family u).(i) in
          match Date.cdate_to_dmy_opt (get_marriage fam) with
          | Some d ->
              Output.print_sstring conf {|<font size="-2"><em>|};
              Output.print_sstring conf (DateDisplay.prec_year_text conf d);
              Output.print_sstring conf "</em></font> "
          | None -> Output.print_sstring conf " "
        else Output.print_sstring conf " ";
        Output.print_string conf (referenced_person_title_text conf base spouse);
        Output.print_string conf (DateDisplay.short_dates_text conf base spouse)
      done;
    Output.print_sstring conf "<br>";
    if lev < max_level then
      let rec loop_fam cnt_chil i =
        if i = Array.length (get_family u) then ()
        else
          let des = foi base (get_family u).(i) in
          let rec loop_chil cnt_chil j =
            if j = Array.length (get_children des) then loop_fam cnt_chil (i + 1)
            else (
              loop_ind (lev + 1)
                (if num_aboville then lab ^>^ string_of_int cnt_chil ^ "."
                else
                  lab ^>^ {|<span class="descends_aboville_pipe">&nbsp;</span>|})
                (pget conf base (get_children des).(j));
              loop_chil (cnt_chil + 1) (j + 1))
          in
          loop_chil cnt_chil 0
      in
      loop_fam 1 0
  in
  loop_ind 0 (Adef.safe "") p;
  Hutil.trailer conf

let desmenu_print = Perso.interp_templ "desmenu"

(* new descendant tree algorithm *)
(* based on the work of Jean Vaucher (U of Montreal) *)
(* - see issue #414 for comments and details *)

(* *****************************************************************
   L'algo fait un parcours PRE-ORDRE de l'arbre en donnant une colonne
   initiale "logique" pour chaque personne, par exemple:  X-1 et X+1
   pour les 2 enfants d'un pere (P) dans la colonne X.
   Ce "X" initial peut etre augmenté (déplacé à droite) pour 2 raisons:

   a) La case proposée est déjà occupée.  Un tableau global,
   last_x, garde la position des derniers noeuds dans chaque rangée.
   On modifie X = max( X, last_x[i]+2)  gardant au moins une colonne
   vide entre chaque noeud.
   b) Pour la meme raison, il se peut que les enfants de la personne
   soient déplacés. ON termine en mettant X a mi-chemin entre les position
   finale du premier et dernier enfant.

    ***************************************************************** *)

(* one td is [nb_cols * align * content]  see dag2Html.ml *)

(* empty fill over n columns *)
let td_fill x1 xn = [ (xn - x1, CenterA, TDnothing) ]

(* hbar over xn - x1 columns. First and last are left/right and 50% *)
let td_hbar x1 xn =
  match xn - x1 with
  | 0 -> [ (1, CenterA, TDnothing) ]
  | 1 -> [ (1, CenterA, TDhr CenterA) ]
  | _n ->
      [
        (1, LeftA, TDhr LeftA);
        (xn - x1 - 1, CenterA, TDhr CenterA);
        (1, RightA, TDhr RightA);
      ]

(* regular cell, centered, with text as content (may contain |<br>) *)
let td_cell cols align ip text flags =
  [ (cols, align, TDitem (ip, Adef.safe text, flags)) ]

(* tdal is   (int   *    list      ) list           *)
(*           (lastx      list of td) list of rows   *)

(* add elem to row ir. Update lastx *)
let tdal_add tdal ir elem nx =
  let tdal =
    let rec loop tdal new_tdal ir elem nx =
      match tdal with
      | [] -> new_tdal
      | (x, row) :: tdal ->
          if ir = 0 then
            let new_row = (nx, row @ elem) in
            new_row :: loop tdal new_tdal (ir - 1) elem nx
          else (x, row) :: loop tdal new_tdal (ir - 1) elem nx
    in
    loop tdal [] ir elem nx
  in
  tdal

let get_bd_td_prop conf =
  let bd = match Util.p_getint conf.env "bd" with Some x -> x | None -> 0 in
  let td_prop =
    match Util.p_getenv conf.env "td" with
    | Some x -> " " ^ x
    | None -> (
        match Util.p_getenv conf.env "color" with
        | None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\"")
  in
  (bd, td_prop)

let reference conf base p s =
  let cgl =
    match Util.p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  let iper = get_iper p in
  if is_hidden p || cgl then s
  else
    Printf.sprintf
      {|<a href="%s%s" id="i%s" class="normal_anchor" title="%s">%s</a>|}
      (commd conf :> string)
      (acces conf base p :> string)
      (string_of_iper iper)
      (Utf8.capitalize_fst (Util.transl conf "open individual page"))
      s

let get_text conf base p filler img cgl =
  let bd, td_prop = get_bd_td_prop conf in
  let auth = authorized_age conf base p in
  let txt = person_title_text conf base p in
  let txt =
    if cgl then (txt :> string) else reference conf base p (txt :> string)
  in
  let txt =
    if auth then txt ^ (DateDisplay.short_dates_text conf base p :> string)
    else txt
  in
  let vbar_image =
    if filler && img then
      Printf.sprintf
        "<div class=\"flex-grow-1\"><img src=\"%sm=IM&v=vbar.jpg\" \
         class=\"img-fluid\"></div>\n"
        (commd conf :> string)
    else ""
  in
  let has_image = Image.get_portrait conf base p |> Option.is_some in
  let txt =
    txt
    ^
    if has_image && img then (DagDisplay.image_txt conf base p :> string)
    else vbar_image
  in
  let txt =
    if bd > 0 || td_prop <> "" then
      Printf.sprintf
        "<table style=\"border: %dpx solid\"><tr><td \
         align=\"center\"%s>%s</td></tr></table>"
        bd td_prop txt
    else txt
  in
  txt

let lastx tdal ir =
  let x, _ = List.nth tdal ir in
  x

let _both_parents_in_only_anc p sp only_anc =
  let ip = get_iper p in
  let isp = get_iper sp in
  List.mem ip only_anc && List.mem isp only_anc

let get_spouse base iper ifam =
  let f = foi base ifam in
  if iper = get_father f then poi base (get_mother f)
  else poi base (get_father f)

(* the heart of Jean Vaucher algo                                    *)
(* to manage sps=on, one must handle different ir numbering *)
(* insufficient to skip "display" of spouses WIP  default is on *)

(* for the "only" option , prune the family list according to   *)
(* relationship with target                                     *)

(*
        /                 |           (except for first row)
  ir   |               person
        \                 |           (except if no spouse )
  ir+1         ---- hbar over spouses ----  (| if only one)
        /                 |
  ir+2 |               spouse
        \                 |           (except if no child)
  ir+3          --- hbar over children ---- (| if only one)

  option sps=off -> only 3 rows
  option only  -> ?

*)

let rec p_pos conf base p x0 v ir tdal only_anc sps img marr cgl =
  let lx = lastx tdal ir in
  let x = if lx + 2 > x0 then lx + 2 else x0 in
  (*let x1 = x in
    let xn = x in *)
  let ifaml = List.rev (Array.to_list (get_family p)) in
  let ifam_nbr = List.length ifaml in
  let descendants = ifaml <> [] in
  (* find right family there *)
  let ifaml =
    if only_anc <> [] then
      List.fold_left
        (fun acc ifam -> if List.mem ifam only_anc then ifam :: acc else acc)
        [] ifaml
    else ifaml
  in
  let tdal, x, _x1, _xn =
    if v >= 1 && ifaml <> [] then
      let xn = if only_anc = [] then x - List.length ifaml - 1 else x in
      let tdal, x1, xn =
        let rec loop ifaml ifam_nbr only_one first last x1 xn tdal =
          match ifaml with
          | [] -> (tdal, x1, xn)
          | ifam :: ifaml ->
              let tdal, xn =
                f_pos conf base ifam ifam_nbr only_one first last p (xn + 2) v
                  (ir + 1) tdal only_anc sps img marr cgl
              in
              loop ifaml (ifam_nbr - 1) only_one false
                (List.length ifaml = 1)
                (if first then xn else x1)
                xn tdal
        in
        loop ifaml ifam_nbr
          (List.length ifaml = 1)
          true
          (List.length ifaml = 1)
          0 xn tdal
      in
      (tdal, (x1 + xn) / 2, x1, xn)
    else (tdal, x, x, x)
    (* ?? correct ?? *)
  in
  (* row 1: person *)
  let vv =
    match p_getenv conf.env "v" with Some v -> "&v=" ^ v | None -> "&v=4"
  in
  let pp = Util.find_person_in_env conf base "" in
  let pp_index =
    match pp with Some p -> "&i=" ^ string_of_iper (get_iper p) | None -> ""
  in
  let pz = Util.find_person_in_env conf base "z" in
  let pz_index =
    match pz with Some p -> "&iz=" ^ string_of_iper (get_iper p) | None -> ""
  in
  let txt = get_text conf base p (ifaml <> [] && sps) img cgl in
  let only =
    if cgl then "|"
    else
      Printf.sprintf "<a href=\"%sm=D&t=TV%s%s%s%s%s%s\" %s title=\"%s\">│</a>"
        (commd conf :> string)
        vv pz_index pp_index
        ("&oi=" ^ string_of_iper (get_iper p))
        (if sps then "" else "&sp=0")
        (if img then "" else "&im=0")
        "class=\"normal_anchor px-3 btn-outline-primary border-0\""
        (Utf8.capitalize_fst
           (Util.transl conf "limit tree to ancestors and siblings"))
  in
  let txt = if ir > 0 then only ^ "<br>" ^ txt else txt in
  (* ajouter un marqueur ici si enfants et on ne continue pas !!   *)
  let continue = only_anc = [] || ifaml <> [] in
  let br = if img then "" else "<br>" in
  let txt = if (not continue) && descendants then txt ^ br ^ "+" else txt in
  let lx = if lx > -1 then lx else -1 in
  let tdal =
    tdal_add tdal ir
      (td_fill lx (x - 1) @ td_cell 1 CenterA (get_iper p) txt (Adef.safe ""))
      x
  in
  (* row 2: Hbar over sps *)
  (* TODO review
     let tdal =
       if v >= 1 && ifaml<>[] then
         let lx = lastx tdal (ir+1) in
         let lx = if lx > -1 then lx else -1 in
         if only_anc = [] && x1 <> xn then
           tdal_add tdal (ir+1)
             ((td_fill lx (x1 - 1))
             @ (td_hbar x1 xn))
             xn
         else
           tdal_add tdal (ir+1)
             ((td_fill lx (x - 1))
             @ (td_cell 1 CenterA Gwdb.dummy_iper (Adef.safe "|") (Adef.safe "")))
             x
       else tdal
     in
  *)
  (tdal, x)

(* ifam_nbr = -1 = 1 family, otherwize rank *)
and f_pos conf base ifam ifam_nbr only_one first last p x0 v ir2 tdal only_anc
    sps img marr cgl =
  let sp = get_spouse base (get_iper p) ifam in
  let continue = only_anc = [] || List.mem ifam only_anc in
  let lx = lastx tdal ir2 + 2 in
  let x = if lx > x0 then lx else x0 in
  (*let x1 = x in
    let xn = x in *)
  let kids =
    Array.fold_left
      (fun l k -> poi base k :: l)
      []
      (get_children (foi base ifam))
  in
  let tdal, x, x1, xn =
    if kids <> [] && continue then
      let xn = x - List.length kids - 1 in
      let tdal, x1, xn =
        let rec loop kids first_kid tdal x1 xn =
          match kids with
          | [] -> (tdal, x1, xn)
          | kid :: kids ->
              let tdal, xn =
                p_pos conf base kid (xn + 2) (v - 1) (ir2 + 2) tdal only_anc sps
                  img marr cgl
              in
              loop kids false tdal (if first_kid then xn else x1) xn
        in
        loop kids true tdal 0 xn
      in
      (tdal, (x1 + xn) / 2, x1, xn)
    else (tdal, x, x, x)
  in
  (* row 3: spouses *)
  let txt = get_text conf base sp (kids <> [] && sps) img cgl in
  let has_image = Image.get_portrait conf base p |> Option.is_some in
  let br_sp = if has_image && img then "" else "<br>" in
  let fam = foi base ifam in
  let marr_d =
    if marr then DateDisplay.short_family_dates_text conf base true fam
    else Adef.safe ""
  in
  let m_txt =
    (* families are scanned in reverse order *)
    let f_nbr = string_of_int ifam_nbr in
    "<span class=\"text-nowrap\">"
    ^ (if last || only_one then "" else "…")
    ^ (if only_one then " &" else " &<sup>" ^ f_nbr ^ "</sup>")
    ^ (marr_d :> string)
    ^ (if first || only_one then "" else "…")
    ^ "</span>"
    ^ if only_one && not marr then "" else "<br>"
  in
  let txt = txt ^ if kids <> [] then br_sp else "" in
  let txt = if sps then m_txt ^ txt else m_txt in
  let txt = if kids <> [] then txt ^ "|" else txt in
  let flag =
    string_of_ifam ifam ^ if kids <> [] then "-spouse_no_d" else "-spouse"
  in
  let lx = lastx tdal ir2 in
  let lx = if lx > -1 then lx else -1 in
  let tdal =
    if true then
      tdal_add tdal ir2
        (td_fill lx (x - 1)
        @ td_cell 1 CenterA (get_iper sp) txt (Adef.safe flag))
        x
    else tdal
  in
  (* rox 4: Hbar over kids *)
  if kids <> [] then
    let lx = lastx tdal (ir2 + 1) in
    let lx = if lx > -1 then lx else -1 in
    let tdal =
      tdal_add tdal (ir2 + 1) (td_fill lx (x1 - 1) @ td_hbar x1 xn) xn
    in
    (tdal, x)
  else (tdal, x)

let complete_rows tdal =
  let max_col =
    let rec loop tdal max_col =
      match tdal with
      | [] -> max_col
      | (x, _row) :: tdal -> loop tdal (if x > max_col then x else max_col)
    in
    loop tdal (-2)
  in
  let tdal =
    let rec loop tdal new_td =
      match tdal with
      | [] -> new_td
      | (x, row) :: tdal ->
          if max_col - x > 0 then
            loop tdal ((max_col, row @ td_fill x max_col) :: new_td)
          else loop tdal ((x, row) :: new_td)
    in
    loop tdal []
  in
  List.rev tdal

let init_tdal gv =
  let rec loop tdal v =
    match v with
    | -1 -> tdal (* 4 rows per generation  (3 if sps=off) *)
    | _ -> loop ((0, []) :: (0, []) :: (0, []) :: (0, []) :: tdal) (v - 1)
  in
  loop [] gv

(* bring back spouses together *)
(* Spouse_no_d, TDnothing, Spouse becomes TDnothing, Spouse_no_d, Spouse *)
let correct_spouses tdal =
  let rec regroup row new_row =
    match row with
    | (nc1, a1, t1) :: (nc2, a2, t2) :: (nc3, a3, t3) :: row -> (
        match (t1, t2, t3) with
        | TDitem (_, _, f1), TDnothing, TDitem (_, _, f3) ->
            let f11 = String.split_on_char '-' (f1 :> string) in
            let f31 = String.split_on_char '-' (f3 :> string) in
            if List.length f11 = 2 && List.length f31 = 2 then
              let fam1 = List.nth f11 0 in
              let fl1 = List.nth f11 1 in
              let fam3 = List.nth f31 0 in
              let fl3 = List.nth f31 1 in
              if fam1 = fam3 && fl1 = "spouse_no_d" && fl3 = "spouse" then
                regroup
                  ([ (nc3, a3, t3); (nc2, a2, t2) ] @ row)
                  ([ (nc1, a1, t1) ] @ new_row)
              else if fam1 = fam3 && fl1 = "spouse" && fl3 = "spouse_no_d" then
                regroup
                  ([ (nc1, a1, t1); (nc3, a3, t3) ] @ row)
                  ([ (nc2, a2, t2) ] @ new_row)
              else
                regroup
                  ([ (nc2, a2, t2); (nc3, a3, t3) ] @ row)
                  ([ (nc1, a1, t1) ] @ new_row)
            else
              regroup
                ([ (nc2, a2, t2); (nc3, a3, t3) ] @ row)
                ([ (nc1, a1, t1) ] @ new_row)
        | _ ->
            regroup
              ([ (nc2, a2, t2); (nc3, a3, t3) ] @ row)
              ([ (nc1, a1, t1) ] @ new_row))
    | _ -> List.rev (List.rev row @ new_row)
  in
  let tdal =
    let rec loop tdal new_tdal =
      match tdal with
      | [] -> new_tdal
      | row :: tdal -> loop tdal (regroup row [] :: new_tdal)
    in
    loop tdal []
  in
  List.rev tdal

(* expand cell size if possible *)
let expand_cell tdal =
  let rec expand row new_row =
    match row with
    | (nc1, a1, t1) :: (nc2, a2, t2) :: (nc3, a3, t3) :: row -> (
        match (t1, t2, t3) with
        | TDnothing, TDitem _, TDnothing ->
            if nc1 > 2 && nc3 > 2 then
              expand
                ([ (nc2 + 2, a2, t2); (nc3 - 1, a3, t3) ] @ row)
                ([ (nc1 - 1, a1, t1) ] @ new_row)
            else
              expand
                ([ (nc2, a2, t2); (nc3, a3, t3) ] @ row)
                ([ (nc1, a1, t1) ] @ new_row)
        | _ ->
            expand
              ([ (nc2, a2, t2); (nc3, a3, t3) ] @ row)
              ([ (nc1, a1, t1) ] @ new_row))
    | _ -> List.rev (List.rev row @ new_row)
  in
  let tdal =
    let rec loop tdal new_tdal =
      match tdal with
      | [] -> new_tdal
      | row :: tdal -> loop tdal (expand row [] :: new_tdal)
    in
    loop tdal []
  in
  List.rev tdal

(* remove x entry *)
let clean_rows tdal =
  let tdal =
    let rec loop tdal new_tdal =
      match tdal with
      | [] -> new_tdal
      | (_x, row) :: tdal ->
          let rec loop2 row new_row =
            match row with
            | [] -> loop tdal (new_row :: new_tdal)
            | td :: row -> loop2 row (td :: new_row)
          in
          loop2 row []
    in
    loop tdal []
  in
  List.rev tdal

(* manage vbars    *)
(* suppress lines containing only vbars *)
(* in one colums, if there is nothing between two vbars, insert vbars *)
(* vbar = [1, CenterA,  TDtext "|"] filler is [(xn - x1), CenterA, TDnothing] *)
let manage_vbars tdal =
  let vbar_only_in_row row =
    List.fold_left
      (fun res (_, _, td) ->
        if td <> TDnothing && td <> TDtext (Gwdb.dummy_iper, Adef.safe "|") then
          false && res
        else true && res)
      true row
  in
  let tdal =
    List.fold_left
      (fun acc row -> if vbar_only_in_row row then acc else row :: acc)
      [] tdal
  in
  List.rev tdal

(* build a list of families (ifam) between iap and ip *)
let rec find_ancestors base iap ip list v =
  match get_parents (poi base ip) with
  | Some ifam ->
      let cpl = foi base ifam in
      let ifath = get_father cpl in
      let imoth = get_mother cpl in
      let list =
        if v > 1 && not (iap = ifath) then
          find_ancestors base iap ifath list (v - 1)
        else list
      in
      let list =
        if v > 1 && not (iap = ifath) then
          find_ancestors base iap imoth list (v - 1)
        else list
      in
      ifam :: list
  | None -> list

let make_vaucher_tree_hts conf base gv p =
  let sps =
    match Util.p_getenv conf.env "sp" with Some "on" -> true | _ -> false
  in
  let marr =
    match Util.p_getenv conf.env "ma" with Some "on" -> true | _ -> false
  in
  let img =
    match Util.p_getenv conf.env "im" with Some "off" -> false | _ -> true
  in
  let cgl =
    match Util.p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  let only_anc, op =
    match Util.find_person_in_env_pref conf base "o" with
    | Some p -> (true, p)
    | None -> (false, Gwdb.empty_person base Gwdb.dummy_iper)
  in
  let only_anc =
    if only_anc then find_ancestors base (get_iper p) (get_iper op) [] gv
    else []
  in
  let tdal = init_tdal gv in
  let tdal, _ = p_pos conf base p 0 gv 0 tdal only_anc sps img marr cgl in
  let tdal = complete_rows tdal in
  let tdal = clean_rows tdal in
  let tdal = expand_cell tdal in
  let tdal = expand_cell tdal in
  let tdal = correct_spouses tdal in
  let tdal = manage_vbars tdal in
  let hts0 = List.fold_left (fun acc row -> Array.of_list row :: acc) [] tdal in
  let hts = Array.of_list (List.rev hts0) in
  hts

let safe_vaucher_tree hts =
  Array.map
    (fun x ->
      Array.map
        (fun (i, al, s) ->
          match s with
          | TDitem (ip, s, t) -> (i, al, TDitem (ip, s, t))
          | TDtext (ip, t) -> (i, al, TDtext (ip, t))
          | TDhr align -> (i, al, TDhr align)
          | TDbar s -> (i, al, TDbar s)
          | TDnothing -> (i, al, TDnothing))
        x)
    hts

let print_vaucher_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    translate_eval
      (let s = (Util.gen_person_text conf base p ~html:false :> string) in
       transl_a_of_gr_eq_gen_lev conf (transl conf "descendants") s s)
    |> Adef.safe
  in
  let hts = make_vaucher_tree_hts conf base gv p in
  let hts = safe_vaucher_tree hts in
  (* just to verify hts structure !! *)
  DagDisplay.print_slices_menu_or_dag_page conf base page_title hts
    (Adef.escaped "")
(* ******** end of J Vaucher tree ********* *)

let print conf base p =
  let templ =
    match p_getenv conf.env "t" with
    | Some ("F" | "L" | "M") -> "deslist"
    | Some "D" -> "deslist_hr"
    | Some ("H" | "I" | "A") -> "destable"
    | Some "V" -> "destree"
    | Some _ -> ""
    | _ -> "desmenu"
  in
  if templ <> "" then Perso.interp_templ templ conf base p
  else
    match (p_getenv conf.env "t", p_getint conf.env "v") with
    | Some "B", Some v -> print_aboville conf base v p
    | Some "S", Some v -> display_descendants_level conf base v p
    | Some "K", Some v -> display_descendant_with_table conf base v p
    | Some "N", Some v -> display_descendants_with_numbers conf base v p
    | Some "G", Some v -> display_descendant_index conf base v p
    | Some "C", Some v -> display_spouse_index conf base v p
    | Some "T", Some v -> print_tree conf base v p
    | Some "TV", Some v -> print_vaucher_tree conf base v p
    | _ -> desmenu_print conf base p
