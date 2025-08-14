(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module StrSet = Mutil.StrSet
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let not_found conf txt x =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst txt);
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf {| "|};
    Output.print_string conf (Util.escape_html x);
    Output.print_sstring conf {|"|}
  in
  Hutil.header ~error:true conf title;
  Hutil.trailer conf

let first_name_not_found conf =
  not_found conf (transl conf "first name not found")

let surname_not_found conf = not_found conf (transl conf "surname not found")

(* **********************************************************************)
(*  [Fonc] print_branch_to_alphabetic : conf -> string -> int -> unit   *)

(* ******************************************************************** *)

(** [Description] : A partir de l'affichage par branches, permet d'afficher les
    liens pour un affichage par ordre alphabétique. [Args] :
    - conf : configuration de la base
    - base : base
    - x : 'nom/prénom/sosa...' recherché
    - nb_branch : nombre de branches dans le résultat de la recherche [Retour] :
      Néant [Rem] : Non exporté en clair hors de ce module. *)
let print_branch_to_alphabetic conf x nb_branch =
  Output.print_sstring conf "<div class=\"mb-3\">";
  Output.print_sstring conf
    (Utf8.capitalize_fst
       (transl_nth conf "display by/branch/alphabetic order" 0));
  Output.print_sstring conf "<i class=\"fa fa-code-fork ml-3 mr-1\"></i>";
  Output.print_sstring conf
    (transl_nth conf "display by/branch/alphabetic order" 1);
  Output.print_sstring conf " (";
  Output.print_sstring conf (string_of_int nb_branch);
  Output.print_sstring conf ")<i class=\"fa fa-arrow-down-a-z ml-3 mr-1\"></i>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  if p_getenv conf.env "t" = Some "A" then (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=N&o=i&t=A&v=";
    Output.print_string conf (Mutil.encode x);
    Output.print_sstring conf {|" rel="nofollow">|};
    Output.print_sstring conf
      (transl_nth conf "display by/branch/alphabetic order" 2);
    Output.print_sstring conf "</a>")
  else (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=N&o=i&t=N&v=|};
    Output.print_string conf (Mutil.encode x);
    Output.print_sstring conf {|" rel="nofollow">|};
    Output.print_sstring conf
      (transl_nth conf "display by/branch/alphabetic order" 2);
    Output.print_sstring conf "</a>");
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Output.print_sstring conf "</div>"

(* **********************************************************************)
(*  [Fonc] print_alphabetic_to_branch : conf -> string -> int -> unit   *)

(* ******************************************************************** *)

(** [Description] : A partir de l'affichage alphabétique, permet d'afficher les
    liens pour un affichage par branches. [Args] :
    - conf : configuration de la base
    - base : base
    - x : 'nom/prénom/sosa...' recherché [Retour] : Néant [Rem] : Non exporté en
      clair hors de ce module. *)
let print_alphabetic_to_branch conf x =
  Output.print_sstring conf "<div class=\"mb-3\">";
  Output.print_sstring conf
    (Utf8.capitalize_fst
       (transl_nth conf "display by/branch/alphabetic order" 0));
  Output.print_sstring conf "<i class=\"fa fa-code-fork ml-3 mr-1\"></i>";
  if p_getenv conf.env "t" = Some "A" then (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=N&t=A&v=";
    Output.print_string conf (Mutil.encode x);
    Output.print_sstring conf {|" rel="nofollow">|};
    Output.print_sstring conf
      (transl_nth conf "display by/branch/alphabetic order" 1);
    Output.print_sstring conf "</a>")
  else (
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=NG&sn=";
    Output.print_string conf (Mutil.encode x);
    Output.print_sstring conf {|" rel="nofollow">|};
    Output.print_sstring conf
      (transl_nth conf "display by/branch/alphabetic order" 1);
    Output.print_sstring conf "</a>");
  Output.print_sstring conf "<i class=\"fa fa-arrow-down-a-z ml-3 mr-1\"></i>";
  Output.print_sstring conf
    (transl_nth conf "display by/branch/alphabetic order" 2);
  Output.print_sstring conf "</div>"

let persons_of_fsname conf base base_strings_of_fsname find proj x =
  (* list of strings index corresponding to the crushed lower first name
     or surname "x" *)
  let istrl = base_strings_of_fsname base x in
  (* selecting the persons who have this first name or surname *)
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
        let str = Mutil.nominative (Driver.sou base istr) in
        if
          Name.crush_lower str = x
          || List.mem x (List.map Name.crush_lower (Mutil.surnames_pieces str))
        then
          let iperl = find istr in
          (* maybe they are not the good ones because of changes in the
             database; checking... *)
          let iperl =
            List.fold_left
              (fun iperl iper ->
                if Driver.Istr.equal (proj (pget conf base iper)) istr then
                  iper :: iperl
                else iperl)
              [] iperl
          in
          if iperl = [] then l else (str, istr, iperl) :: l
        else l)
      istrl []
  in
  let l, name_inj =
    let l1, name_inj =
      let x = Name.lower x in
      ( List.fold_right
          (fun (str, istr, iperl) l ->
            if x = Name.lower str then (str, istr, iperl) :: l else l)
          l [],
        Name.lower )
    in
    let l1, name_inj =
      if l1 = [] then
        let x = Name.strip_lower x in
        ( List.fold_right
            (fun (str, istr, iperl) l ->
              if x = Name.strip_lower str then (str, istr, iperl) :: l else l)
            l [],
          Name.strip_lower )
      else (l1, name_inj)
    in
    if l1 = [] then (l, Name.crush_lower) else (l1, name_inj)
  in
  (l, name_inj)

let print_elem conf base is_surname (p, xl) =
  Mutil.list_iter_first
    (fun first x ->
      let iper = Driver.get_iper x in
      if not first then Output.print_sstring conf "</li><li> ";
      SosaCache.print_sosa conf base x true;
      Output.print_sstring conf {|<a href="|};
      Output.print_string conf (commd conf);
      Output.print_string conf (acces conf base x);
      Output.print_sstring conf {|" id="i|};
      Output.print_sstring conf (Driver.Iper.to_string iper);
      Output.print_sstring conf {|">|};
      if is_surname then (
        Output.print_string conf (escape_html @@ surname_without_particle base p);
        Output.print_string conf (escape_html @@ surname_particle base p);
        Output.print_sstring conf " (<small>";
        Output.print_string conf
          (escape_html @@ Driver.sou base (Driver.get_first_name x));
        Output.print_sstring conf "</small>)")
      else
        Output.print_string conf
          (if p = "" then Adef.escaped "?" else escape_html p);
      Output.print_sstring conf "</a>";
      Output.print_string conf (DateDisplay.short_dates_text conf base x);
      Output.print_sstring conf "<em>";
      specify_homonymous conf base x true;
      Output.print_sstring conf "</em>")
    xl

let first_char s =
  (* Si la personne n'a pas de prénom/nom, on renvoie '?' *)
  if s = "" then "?"
  else
    let len = Utf8.next s 0 in
    if len < String.length s then String.sub s 0 len else s

let name_unaccent s =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else
      let t, j = Name.unaccent_utf_8 false s i in
      copy j (Buff.mstore len t)
  in
  copy 0 0

let first_name_print_list conf base x1 xl listes =
  let surnames_liste l =
    List.fold_left
      (fun l x ->
        let px = Driver.p_surname base x in
        match l with
        | (p, l1) :: l when Gutil.alphabetic px p = 0 -> (p, x :: l1) :: l
        | _ -> (px, [ x ]) :: l)
      [] l
  in

  let title h =
    if h || p_getenv conf.env "t" = Some "A" then
      Output.print_string conf (escape_html x1)
    else
      Mutil.list_iter_first
        (fun first x ->
          if not first then Output.print_sstring conf ", ";
          Output.print_sstring conf {|<a href="|};
          Output.print_string conf (commd conf);
          Output.print_sstring conf {|m=P&t=A&v=|};
          Output.print_string conf (Mutil.encode x);
          Output.print_sstring conf {|">|};
          Output.print_string conf (escape_html x);
          Output.print_sstring conf {|</a>|})
        (StrSet.elements xl)
  in
  Hutil.header conf title;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;

  List.iter
    (fun (str, liste) ->
      if liste <> [] then (
        let list = surnames_liste liste in
        let list =
          List.rev_map
            (fun (sn, ipl) ->
              let txt =
                Util.surname_without_particle base sn
                ^ Util.surname_particle base sn
              in
              let ord = name_unaccent txt in
              (ord, txt, ipl))
            list
        in
        let list = List.sort compare list in
        if str <> "" then (
          Output.print_sstring conf {|<div class="my-3">|};
          Output.print_sstring conf str;
          Output.print_sstring conf "</div>\n");
        print_alphab_list conf
          (fun (ord, _, _) -> first_char ord)
          (fun (_, txt, ipl) -> print_elem conf base true (txt, ipl))
          list))
    listes;
  Hutil.trailer conf

let mk_specify_title conf kw n _ =
  Output.print_sstring conf (Utf8.capitalize_fst kw);
  Output.print_sstring conf {| "|};
  Output.print_string conf (escape_html n);
  Output.print_sstring conf {|"|};
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf {| |};
  Output.print_sstring conf (transl conf "specify")

let select_first_name conf n list =
  Hutil.header conf
    (mk_specify_title conf (transl_nth conf "first name/first names" 0) n);
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (sstr, (strl, _)) ->
      Output.print_sstring conf {|<li><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=P&v=|};
      Output.print_string conf (Mutil.encode sstr);
      Output.print_sstring conf {|">|};
      Mutil.list_iter_first
        (fun first str ->
          if not first then Output.print_sstring conf ", ";
          Output.print_string conf (escape_html str))
        (StrSet.elements strl);
      Output.print_sstring conf "</a>\n")
    list;
  Output.print_sstring conf "</ul>\n";
  Hutil.trailer conf

let rec merge_insert ((sstr, (strl, iperl)) as x) = function
  | ((sstr1, (strl1, iperl1)) as y) :: l ->
      if sstr < sstr1 then x :: y :: l
      else if sstr > sstr1 then y :: merge_insert x l
      else (sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l
  | [] -> [ x ]

let persons_of_absolute base_strings_of persons_of get_field conf base x =
  let istrl = base_strings_of base x in
  List.fold_right
    (fun istr l ->
      let str = Driver.sou base istr in
      if str = x then
        let iperl = Driver.spi_find (persons_of base) istr in
        let iperl =
          List.fold_left
            (fun iperl iper ->
              let p = pget conf base iper in
              if
                Driver.Istr.equal (get_field p) istr
                && ((not (is_hide_names conf p))
                   || Util.authorized_age conf base p)
              then iper :: iperl
              else iperl)
            [] iperl
        in
        if iperl = [] then l else (str, istr, iperl) :: l
      else l)
    istrl []

let persons_of_absolute_first_name =
  persons_of_absolute Driver.base_strings_of_first_name
    Driver.persons_of_first_name Driver.get_first_name

let persons_of_absolute_surname =
  persons_of_absolute Driver.base_strings_of_surname Driver.persons_of_surname
    Driver.get_surname

let has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (Driver.p_surname base (pget conf base ip)) name)
    (Array.to_list (Driver.get_children des))

(* List selection bullets *)

let bullet_sel_txt = Adef.safe "o"
let bullet_unsel_txt = Adef.safe "+"
let bullet_nosel_txt = Adef.safe "o"

let print_selection_bullet conf = function
  | Some (txt, sel) ->
      let req : Adef.encoded_string =
        List.fold_left
          (fun (req : Adef.encoded_string) (k, (v : Adef.encoded_string)) ->
            if (not sel) && k = "u" && v = txt then req
            else
              let s : Adef.encoded_string = Adef.encoded k ^^^ "=" ^<^ v in
              if (req :> string) = "" then s else req ^^^ "&" ^<^ s)
          (Adef.encoded "") conf.env
      in
      Output.print_sstring conf {|<a id="if|};
      Output.print_string conf txt;
      Output.print_sstring conf {|" href="|};
      Output.print_string conf (prefix_base conf);
      Output.print_string conf req;
      if sel then Output.print_string conf ("&u=" ^<^ txt);
      if sel || List.mem_assoc "u" conf.env then
        Output.print_string conf ("#if" ^<^ txt);
      Output.print_sstring conf {|" rel="nofollow">|};
      Output.print_string conf
        (if sel then bullet_sel_txt else bullet_unsel_txt);
      Output.print_sstring conf "</a>\n"
  | None ->
      Output.print_string conf bullet_nosel_txt;
      Output.print_sstring conf "\n"

let unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
      try if k = "u" then Driver.Ifam.of_string (Mutil.decode v) :: sl else sl
      with Failure _ -> sl)
    [] conf.env

let alphabetic1 n1 n2 = Gutil.alphabetic_utf_8 n1 n2

type 'a branch_head = { bh_ancestor : 'a; bh_well_named_ancestors : 'a list }

let print_branch conf base psn name =
  let unsel_list = unselected_bullets conf in
  let rec loop p =
    let u = pget conf base (Driver.get_iper p) in
    let family_list =
      Array.map
        (fun ifam ->
          let fam = Driver.foi base ifam in
          let c = Gutil.spouse (Driver.get_iper p) fam in
          let c = pget conf base c in
          let down = has_children_with_that_name conf base fam name in
          let down =
            if Driver.get_sex p = Female && Driver.p_surname base c = name then
              false
            else down
          in
          let i = ifam in
          let sel = not (List.mem i unsel_list) in
          ( fam,
            c,
            if down then Some (Mutil.encode @@ Driver.Ifam.to_string i, sel)
            else None ))
        (Driver.get_family u)
    in
    let first_select =
      if family_list = [||] then None
      else (fun (_, _, s) -> s) (Array.unsafe_get family_list 0)
    in
    let print_elem p with_link with_id with_sn =
      let render p =
        if with_link then
          if with_id then Util.reference conf base p
          else Util.reference_noid conf base p
        else fun s -> s
      in
      SosaCache.print_sosa conf base p with_link;
      Output.print_sstring conf @@ if with_link then "<strong>" else "<em>";
      Output.print_string conf
        (render p
           (if is_hide_names conf p && not (authorized_age conf base p) then
              Adef.safe "x"
            else if (not psn) && (not with_sn) && Driver.p_surname base p = name
            then gen_person_text ~sn:false conf base p
            else gen_person_text conf base p));
      Output.print_sstring conf @@ if with_link then "</strong>" else "</em>";
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      Output.print_sstring conf "\n"
    in
    Output.print_sstring conf "<li>";
    print_selection_bullet conf first_select;
    print_elem p true true false;
    if Array.length (Driver.get_family u) <> 0 then
      ignore
      @@ Array.fold_left
           (fun first (fam, sp, select) ->
             if not first then (
               Output.print_sstring conf "<li>";
               print_selection_bullet conf select;
               print_elem p false true false);
             Output.print_sstring conf " &amp;";
             Output.print_string conf
               (DateDisplay.short_marriage_date_text conf base fam p sp);
             Output.print_sstring conf "\n";
             print_elem sp true false true;
             let children = Driver.get_children fam in
             (match select with
             | Some (_, true) ->
                 Output.print_sstring conf "<ul>";
                 Array.iter
                   (fun e ->
                     loop (pget conf base e);
                     Output.print_sstring conf "</li>")
                   children;
                 Output.print_sstring conf "</ul>"
             | Some (_, false) -> ()
             | None ->
                 if Array.length children <> 0 then
                   Output.print_sstring conf
                     {|<ul class="posterity"><li>...</li></ul>|});
             Output.print_sstring conf "</li>";
             false)
           true family_list;
    Output.print_sstring conf "</li>"
  in
  loop

let print_one_branch conf base bh psn =
  Output.print_sstring conf "<ul>";
  let p = bh.bh_ancestor in
  if bh.bh_well_named_ancestors = [] then
    let x = Driver.sou base (Driver.get_surname p) in
    print_branch conf base psn x p
  else (
    Output.print_sstring conf "<li>";
    if is_hidden p then Output.print_sstring conf "&lt;&lt;"
    else
      wprint_geneweb_link conf (Util.acces conf base p) (Adef.safe "&lt;&lt;");
    Output.print_sstring conf "<ul>";
    List.iter
      (fun p ->
        let x = Driver.sou base (Driver.get_surname p) in
        print_branch conf base psn x p)
      bh.bh_well_named_ancestors;
    Output.print_sstring conf "</ul></li>");
  Output.print_sstring conf "</ul>"

let print_one_surname_by_branch conf base x xl (bhl, str) =
  let ancestors =
    match p_getenv conf.env "order" with
    | Some "d" ->
        let born_before p1 p2 =
          match
            ( Date.od_of_cdate (Driver.get_birth p1),
              Date.od_of_cdate (Driver.get_birth p2) )
          with
          | Some d1, Some d2 -> Date.compare_date d1 d2
          | _, None -> -1
          | None, _ -> 1
        in
        List.sort (fun p1 p2 -> born_before p1.bh_ancestor p2.bh_ancestor) bhl
    | _ ->
        List.sort
          (fun p1 p2 ->
            alphabetic1
              (Driver.p_first_name base p1.bh_ancestor)
              (Driver.p_first_name base p2.bh_ancestor))
          bhl
  in
  let len = List.length ancestors in
  let psn =
    match p_getenv conf.env "alwsurn" with
    | Some x -> x = "yes"
    | None -> (
        try List.assoc "always_surname" conf.base_env = "yes"
        with Not_found -> false)
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then
      Output.print_string conf (escape_html x)
    else
      Mutil.list_iter_first
        (fun first x ->
          if not first then Output.print_sstring conf ", ";
          Output.print_sstring conf {|<a href="|};
          Output.print_string conf (commd conf);
          Output.print_sstring conf {|m=N&t=A&v=|};
          Output.print_string conf (Mutil.encode x);
          Output.print_sstring conf {|">|};
          Output.print_string conf (escape_html x);
          Output.print_sstring conf {|</a>|})
        (StrSet.elements xl)
  in
  let br = p_getint conf.env "br" in
  Hutil.header conf title;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  (* Menu afficher par branche/ordre alphabetique *)
  if br = None then print_branch_to_alphabetic conf x len;
  Output.print_sstring conf {|<div id="surname_by_branch">|};
  if len > 1 && br = None then (
    Output.print_sstring conf "<dl>";
    ignore
    @@ List.fold_left
         (fun n bh ->
           Output.print_sstring conf {|<dt><a href="|};
           Output.print_string conf (commd conf);
           Output.print_sstring conf {|m=N&v=|};
           Output.print_string conf (Mutil.encode str);
           Output.print_sstring conf {|&br=|};
           Output.print_sstring conf (string_of_int n);
           Output.print_sstring conf {|" rel="nofollow">|};
           Output.print_sstring conf (string_of_int n);
           Output.print_sstring conf ".</a></dt><dd>";
           print_one_branch conf base bh psn;
           Output.print_sstring conf "</dd>";
           n + 1)
         1 ancestors;
    Output.print_sstring conf "</dl>")
  else
    ignore
    @@ List.fold_left
         (fun n bh ->
           if br = None || br = Some n then print_one_branch conf base bh psn;
           n + 1)
         1 ancestors;
  Output.print_sstring conf "</div>";
  Hutil.trailer conf

let print_several_possible_surnames x conf base (_, homonymes) =
  let fx = x in
  let x = match homonymes with x :: _ -> x | _ -> x in
  let title = mk_specify_title conf (transl_nth conf "surname/surnames" 0) fx in
  Hutil.header conf title;
  let list =
    List.map
      (fun sn ->
        let txt =
          Util.surname_without_particle base sn ^ Util.surname_particle base sn
        in
        let ord = name_unaccent txt in
        (ord, txt, sn))
      homonymes
  in
  let list = List.sort compare list in
  let access txt sn =
    geneweb_link conf
      ("m=N&v=" ^<^ Mutil.encode sn ^>^ "&t=N" :> Adef.escaped_string)
      (escape_html txt :> Adef.safe_string)
  in
  Util.wprint_in_columns conf
    (fun (ord, _, _) -> ord)
    (fun (_, txt, sn) -> Output.print_string conf (access txt sn))
    list;
  Output.print_sstring conf {|<p><em style="font-size:80%">|};
  Output.print_sstring conf {| <a |};
  Output.print_sstring conf {| href="|};
  Output.print_string conf (commd conf);
  Output.print_sstring conf {|m=N&o=i&t=N&v=|};
  Output.print_string conf
    (if List.length homonymes = 1 then Mutil.encode x else Mutil.encode fx);
  Output.print_sstring conf {|">|};
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "click"));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "here");
  Output.print_sstring conf " ";
  Output.print_sstring conf
    (transl conf "for the first names by alphabetic order");
  Output.print_sstring conf {|</a> |};
  Output.print_sstring conf ".</em></p>";
  Hutil.trailer conf

let print_family_alphabetic x conf base liste =
  let homonymes =
    let list =
      List.fold_left
        (fun list p ->
          if List.exists (Driver.Istr.equal (Driver.get_surname p)) list then
            list
          else Driver.get_surname p :: list)
        [] liste
    in
    let set =
      List.fold_left
        (fun set istr -> StrSet.add (Driver.sou base istr) set)
        StrSet.empty list
    in
    List.sort compare (StrSet.elements set)
  in
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
          match
            alphabetic1
              (Driver.p_first_name base x2)
              (Driver.p_first_name base x1)
          with
          | 0 -> compare (Driver.get_occ x1) (Driver.get_occ x2)
          | n -> n)
        liste
    in
    List.fold_left
      (fun l x ->
        let px = Driver.p_first_name base x in
        match l with
        | (p, l1) :: l when alphabetic1 px p = 0 -> (p, x :: l1) :: l
        | _ -> (px, [ x ]) :: l)
      [] l
  in
  match liste with
  | [] -> surname_not_found conf x
  | _ ->
      let title h =
        let access x =
          if h || List.length homonymes = 1 then
            (Util.escape_html x :> Adef.safe_string)
          else
            geneweb_link conf
              ("m=N&o=i&v=" ^<^ Mutil.encode x ^>^ "&t=A"
                :> Adef.escaped_string)
              (escape_html x :> Adef.safe_string)
        in
        Mutil.list_iter_first
          (fun first x ->
            if not first then Output.print_sstring conf ", ";
            Output.print_string conf (access x))
          homonymes
      in
      Hutil.header conf title;
      (* Si on est dans un calcul de parenté, on affiche *)
      (* l'aide sur la sélection d'un individu.          *)
      Util.print_tips_relationship conf;
      (* Menu afficher par branche/ordre alphabetique *)
      print_alphabetic_to_branch conf x;
      print_alphab_list conf
        (fun (p, _) -> first_char p)
        (print_elem conf base false)
        liste;
      Hutil.trailer conf

let insert_at_position_in_family children ip ipl =
  let rec loop child_list ipl =
    match (child_list, ipl) with
    | ip1 :: ipl1, ip2 :: ipl2 ->
        if ip1 = ip2 then if ip = ip1 then ipl else ip2 :: loop ipl1 ipl2
        else if ip = ip1 then ip1 :: ipl
        else loop ipl1 ipl
    | _ :: _, [] -> [ ip ]
    | [], _ -> assert false
  in
  loop (Array.to_list children) ipl

let select_ancestors conf base name_inj ipl =
  let str_inj s = name_inj (Driver.sou base s) in
  List.fold_left
    (fun bhl ip ->
      let p = pget conf base ip in
      match Driver.get_parents p with
      | Some ifam ->
          let fam = Driver.foi base ifam in
          let ifath = Driver.get_father fam in
          let imoth = Driver.get_mother fam in
          let fath = pget conf base ifath in
          let moth = pget conf base imoth in
          let s = str_inj (Driver.get_surname p) in
          if
            str_inj (Driver.get_surname fath) <> s
            && str_inj (Driver.get_surname moth) <> s
          then
            let rec loop = function
              | bh :: bhl ->
                  if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
                    let bh =
                      {
                        bh with
                        bh_well_named_ancestors =
                          insert_at_position_in_family (Driver.get_children fam)
                            ip bh.bh_well_named_ancestors;
                      }
                    in
                    bh :: bhl
                  else bh :: loop bhl
              | [] ->
                  [ { bh_ancestor = ifath; bh_well_named_ancestors = [ ip ] } ]
            in
            loop bhl
          else bhl
      | _ ->
          let bh = { bh_ancestor = ip; bh_well_named_ancestors = [] } in
          bh :: bhl)
    [] ipl

module PerSet = Set.Make (struct
  type t = Driver.iper

  let compare = compare
end)

let search_surname_list conf base x =
  let list, name_inj =
    if p_getenv conf.env "t" = Some "A" then
      (persons_of_absolute_surname conf base x, fun x -> x)
    else if x = "" then
      ([], fun _ -> raise (Match_failure ("src/some.ml", 942, 29)))
    else
      persons_of_fsname conf base Driver.base_strings_of_surname
        (Driver.spi_find (Driver.persons_of_surname base))
        Driver.get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
        (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let iperl, _ =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
        let len = List.length iperl1 in
        let strl =
          try
            let len1 = List.assoc str strl in
            (str, len + len1) :: List.remove_assoc str strl
          with Not_found -> (str, len) :: strl
        in
        (List.fold_right PerSet.add iperl1 iperl, strl))
      list (PerSet.empty, [])
  in
  (list, PerSet.elements iperl, name_inj)

let search_surname conf base x =
  let list, iperl, name_inj = search_surname_list conf base x in
  let bhl = select_ancestors conf base name_inj iperl in
  let bhl =
    List.map
      (fun bh ->
        {
          bh_ancestor = pget conf base bh.bh_ancestor;
          bh_well_named_ancestors =
            List.map (pget conf base) bh.bh_well_named_ancestors;
        })
      bhl
  in
  match (bhl, list) with
  | [], _ -> []
  | _, [ (_, (_, iperl)) ] -> iperl
  | _ -> []

let search_surname_print conf base _not_found_fun x =
  let list, iperl, name_inj = search_surname_list conf base x in
  (* Construction de la table des sosa de la base *)
  let () = SosaCache.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
  | Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> pget conf base ip :: ipl) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
            if (not (is_hide_names conf p)) || authorized_age conf base p then
              p :: pl
            else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ -> (
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
            {
              bh_ancestor = pget conf base bh.bh_ancestor;
              bh_well_named_ancestors =
                List.map (pget conf base) bh.bh_well_named_ancestors;
            })
          bhl
      in
      match (bhl, list) with
      | [], _ -> SrcfileDisplay.print_welcome conf base
      | _, [ (s, (strl, _)) ] ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map (fun (s, _) -> s) list in
          print_several_possible_surnames x conf base (bhl, strl))

let search_first_name conf base x =
  let list, _ =
    if p_getenv conf.env "t" = Some "A" then
      ( persons_of_absolute_first_name conf base x,
        fun _ -> raise (Match_failure ("src/some.ml", 1007, 51)) )
    else if x = "" then
      ([], fun _ -> raise (Match_failure ("src/some.ml", 1008, 29)))
    else
      persons_of_fsname conf base Driver.base_strings_of_first_name
        (Driver.spi_find (Driver.persons_of_first_name base))
        Driver.get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
        (Name.lower str, (StrSet.add str StrSet.empty, iperl)))
      list
  in
  List.fold_right merge_insert list []

let search_first_name_print conf base x =
  let list = search_first_name conf base x in
  (* Construction de la table des sosa de la base *)
  let () = SosaCache.build_sosa_ht conf base in
  match list with
  | [] -> first_name_not_found conf x
  | [ (_, (strl, iperl)) ] ->
      let iperl = List.sort_uniq compare iperl in
      let rev_pl = List.rev_map (pget conf base) iperl in
      let pl =
        List.fold_left
          (fun pl p ->
            if (not (is_hide_names conf p)) || authorized_age conf base p then
              p :: pl
            else pl)
          rev_pl []
      in
      first_name_print_list conf base x strl [ ("", pl) ]
  | _ -> select_first_name conf x list
