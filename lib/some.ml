(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

module StrSet = Mutil.StrSet

let not_found conf txt x =
  let title _ = Wserver.printf "%s: \"%s\"" (Utf8.capitalize txt) x in
  Hutil.rheader conf title; Hutil.print_link_to_welcome conf false; Hutil.trailer conf

let first_name_not_found conf =
  not_found conf (transl conf "first name not found")

let surname_not_found conf = not_found conf (transl conf "surname not found")


(* **********************************************************************)
(*  [Fonc] print_branch_to_alphabetic : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage par branches, permet
                    d'afficher les liens pour un affichage par ordre
                    alphabétique.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
      - nb_branch : nombre de branches dans le résultat de la recherche
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_branch_to_alphabetic conf x nb_branch =
  Wserver.printf "<table class=\"display_search\">\n";
  Wserver.printf "<tr>";
  Wserver.printf "<td>";
  Wserver.printf "<b>";
  Wserver.printf "%s"
    (Utf8.capitalize (transl_nth conf "display by/branch/alphabetic order" 0));
  Wserver.printf "</b>";
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_branch.png" conf.xhs;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf "%s (%d)"
    (transl_nth conf "display by/branch/alphabetic order" 1) nb_branch;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png" conf.xhs;
  Wserver.printf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Wserver.printf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (code_varenv x ^ "&t=A");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Wserver.printf "</a>"
    end
  else
    begin
      Wserver.printf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (code_varenv x ^ "&t=A");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Wserver.printf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "</td>";
  Wserver.printf "</tr>";
  Wserver.printf "</table>\n";
  Wserver.printf "<br%s>\n" conf.xhs


(* **********************************************************************)
(*  [Fonc] print_alphabetic_to_branch : conf -> string -> int -> unit   *)
(** [Description] : A partir de l'affichage alphabétique, permet
                    d'afficher les liens pour un affichage par branches.
    [Args] :
      - conf      : configuration de la base
      - base      : base
      - x         : 'nom/prénom/sosa...' recherché
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_alphabetic_to_branch conf x =
  Wserver.printf "<table class=\"display_search\">";
  Wserver.printf "<tr>";
  Wserver.printf "<td>";
  Wserver.printf "<b>";
  Wserver.printf "%s"
    (Utf8.capitalize (transl_nth conf "display by/branch/alphabetic order" 0));
  Wserver.printf "</b>";
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_branch.png" conf.xhs;
  Wserver.printf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Wserver.printf "<a href=\"%sm=N&v=%s\" rel=\"nofollow\">" (commd conf)
        (code_varenv x ^ "&t=A");
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Wserver.printf "</a>"
    end
  else
    begin
      Wserver.printf "<a href=\"%sm=NG&sn=%s\" rel=\"nofollow\">" (commd conf)
        (code_varenv x);
      Wserver.printf "%s"
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Wserver.printf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf
    "<img src=\"%s/%s\" align=\"middle\" alt=\"\" title=\"\"%s>\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png" conf.xhs;
  Wserver.printf "</td>";
  Wserver.printf "<td>";
  Wserver.printf "%s"
    (transl_nth conf "display by/branch/alphabetic order" 2);
  Wserver.printf "</td>";
  Wserver.printf "</tr>";
  Wserver.printf "</table>";
  Wserver.printf "<br%s>\n" conf.xhs

let new_persons_of_fsname base base_strings_of_fsname find split x =
  let list =
    let filter = Name.crush_lower in
    let x = filter x in
    List.fold_left begin fun acc istr ->
      let str = Mutil.nominative (sou base istr) in
      if filter str = x
      || List.mem x (List.map filter @@ split str)
      then
        let iperl = find istr in
        if iperl = [] then acc else (str, istr, iperl) :: acc
      else acc
    end [] (base_strings_of_fsname base x)
  in
  let list' =
    let filter = Name.lower in
    let x = filter x in
    List.fold_left begin fun acc (str, istr, iperl) ->
      if x = filter str
      || List.mem x (List.map filter @@ split str)
      then (str, istr, iperl) :: acc else acc
    end [] list
  in
  if list' = [] then list, Name.crush_lower else list', Name.lower

let print_elem conf base is_surname (p, xl) =
  Mutil.list_iter_first
    (fun first x ->
       let iper = get_iper x in
       if not first then Wserver.printf "</li>\n<li>\n  ";
       Perso.print_sosa conf base x true;
       Wserver.printf "<a href=\"%s%s\" id=\"i%s\">" (commd conf)
         (acces conf base x) (string_of_iper iper);
       if is_surname then
         Wserver.printf "%s%s" (surname_without_particle base p) (surname_particle base p)
       else Wserver.printf "%s" (if p = "" then "?" else p);
       Wserver.printf "</a>";
       Wserver.printf "%s" (DateDisplay.short_dates_text conf base x);
       Wserver.printf "<em>";
       specify_homonymous conf base x true;
       Wserver.printf "</em>")
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
      let (t, j) = Name.unaccent_utf_8 false s i in copy j (Buff.mstore len t)
  in
  copy 0 0

let first_name_print_list conf base x1 xl liste =
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match Utf8.compare (p_surname base x1) (p_surname base x2) with
             0 ->
             begin match
                 Adef.od_of_cdate (get_birth x1),
                 Adef.od_of_cdate (get_birth x2)
               with
               | Some d1, Some d2 -> Date.compare_date d1 d2
               | Some _, _ -> 1
               | _ -> -1
             end
           | n -> -n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_surname base x in
         match l with
           (p, l1) :: l when Utf8.compare px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.printf "%s" x1
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.printf "%s<a href=\"%sm=P&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  let list =
    List.map
      (fun (sn, ipl) ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = name_unaccent txt in ord, txt, ipl)
      liste
  in
  let list = List.sort compare list in
  print_alphab_list (fun (ord, _, _) -> first_char ord)
    (fun (_, txt, ipl) -> print_elem conf base true (txt, ipl)) list;
  Hutil.trailer conf

let select_first_name conf n list =
  let title _ =
    Wserver.printf "%s \"%s\" : %s"
      (Utf8.capitalize (transl_nth conf "first name/first names" 0)) n
      (transl conf "specify")
  in
  Hutil.header conf title;
  Wserver.printf "<ul>";
  List.iter
    (fun (sstr, (strl, _)) ->
       Wserver.printf "\n";
       Wserver.printf "<li>" ;
       Wserver.printf "<a href=\"%sm=P&v=%s&t=A\">" (commd conf)
         (code_varenv sstr);
       Mutil.list_iter_first
         (fun first str ->
            Wserver.printf "%s%s" (if first then "" else ", ") str)
         (StrSet.elements strl);
       Wserver.printf "</a>\n")
    list;
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let rec merge_insert acc (sstr, (strl, iperl) as x) = match acc with
  | (sstr1, (strl1, iperl1) as y) :: l ->
    if sstr < sstr1 then x :: y :: l
    else if sstr > sstr1 then y :: merge_insert l x
    else (sstr, (StrSet.union strl strl1, List.rev_append iperl iperl1)) :: l
  | [] -> [x]

(** A person with name [n] is selected if [String.lowercase_ascii n = x].
    Until full utf8 case mapping is supported.
*)
let persons_of_absolute_aux strings spi get split conf base x =
  let lower = String.lowercase_ascii in
  let x = lower x in
  ( let istrs = strings base @@ List.hd @@ split x in
    List.fold_right begin fun istr l ->
      let str = sou base istr in
      if lower str = x then
        let ipers = spi_find (spi base) istr in
        let ipers =
          (* Filter out hidden persons *)
          List.fold_left begin fun ipers iper ->
            if eq_istr (get (pget conf base iper)) istr
            then iper :: ipers
            else ipers
          end [] ipers
        in
        if ipers = [] then l else (str, istr, ipers) :: l
      else l
    end istrs []
  , lower )

let persons_of_absolute_first_name =
  persons_of_absolute_aux
    base_strings_of_first_name
    persons_of_first_name
    get_first_name
    Name.split_fname

let persons_of_absolute_surname =
  persons_of_absolute_aux
    base_strings_of_surname
    persons_of_surname
    get_surname
    Name.split_sname

let has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (p_surname base (pget conf base ip)) name)
    (Array.to_list (get_children des))

(* List selection bullets *)

let bullet_sel_txt = "<tt>o</tt>"
let bullet_unsel_txt = "<tt>+</tt>"
let bullet_nosel_txt = "<tt>o</tt>"
let print_selection_bullet conf =
  function
    Some (txt, sel) ->
      let req =
        List.fold_left
          (fun req (k, v) ->
             if not sel && k = "u" && v = txt then req
             else
               let s = k ^ "=" ^ v in if req = "" then s else req ^ "&" ^ s)
          "" conf.env
      in
      if conf.cancel_links then ()
      else
        Wserver.printf "<a id=\"i%s\" href=\"%s%s%s%s\" rel=\"nofollow\">" txt
          (commd conf) req (if sel then "&u=" ^ txt else "")
          (if sel || List.mem_assoc "u" conf.env then "#i" ^ txt else "");
      Wserver.printf "%s" (if sel then bullet_sel_txt else bullet_unsel_txt);
      if conf.cancel_links then () else Wserver.printf "</a>";
      Wserver.printf "\n"
  | None -> Wserver.printf "%s\n" bullet_nosel_txt

let unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
       try if k = "u" then ifam_of_string v :: sl else sl with Failure _ -> sl)
    [] conf.env

type 'a branch_head = { bh_ancestor : 'a; bh_well_named_ancestors : 'a list }

let print_branch conf base psn name =
  let unsel_list = unselected_bullets conf in
  let rec loop is_first_level p =
    let u = pget conf base (get_iper p) in
    let family_list =
      List.map
        (fun ifam ->
           let fam = foi base ifam in
           let c = Gutil.spouse (get_iper p) fam in
           let c = pget conf base c in
           let down = has_children_with_that_name conf base fam name in
           let down =
             if get_sex p = Female && p_surname base c = name then false
             else down
           in
           let i = ifam in
           let sel = not (List.mem i unsel_list) in
           fam, c, (if down then Some (string_of_ifam i, sel) else None))
        (Array.to_list (get_family u))
    in
    let first_select =
      match family_list with
        (_, _, select) :: _ -> select
      | _ -> None
    in
    print_selection_bullet conf first_select;
    Perso.print_sosa conf base p true;
    Wserver.printf "<strong>";
    Wserver.printf "%s"
      (Util.reference conf base p
         (if is_hide_names conf p && not (authorized_age conf base p) then "x"
          else if not psn && p_surname base p = name then
            person_text_without_surname conf base p
          else person_text conf base p));
    Wserver.printf "</strong>";
    Wserver.printf "%s" (DateDisplay.short_dates_text conf base p);
    Wserver.printf "\n";
    if Array.length (get_family u) = 0 then ()
    else
      let _ =
        List.fold_left
          (fun first (fam, c, select) ->
             if not first then
               begin
                 if is_first_level then Wserver.printf "<br%s>\n" conf.xhs
                 else Wserver.printf "</dd>\n<dd>\n";
                 print_selection_bullet conf select;
                 Perso.print_sosa conf base p false;
                 begin
                   Wserver.printf "<em>";
                   Wserver.printf "%s"
                     (if is_hide_names conf p &&
                         not (authorized_age conf base p)
                      then
                        "x"
                      else if not psn && p_surname base p = name then
                        person_text_without_surname conf base p
                      else person_text conf base p);
                   Wserver.printf "</em>"
                 end;
                 Wserver.printf "%s" (DateDisplay.short_dates_text conf base p);
                 Wserver.printf "\n"
               end;
             Wserver.printf "  &amp;";
             Wserver.printf "%s\n"
               (DateDisplay.short_marriage_date_text conf base fam p c);
             Perso.print_sosa conf base c true;
             Wserver.printf "<strong>";
             Wserver.printf "%s"
               (reference conf base c
                  (if is_hide_names conf c && not (authorized_age conf base c)
                   then
                     "x"
                   else person_text conf base c));
             Wserver.printf "</strong>";
             Wserver.printf "%s" (DateDisplay.short_dates_text conf base c);
             Wserver.printf "\n";
             let children = get_children fam in
             begin match select with
               Some (_, true) ->
                 Wserver.printf "<dl>\n";
                 List.iter
                   (fun e ->
                      Wserver.printf "<dd>\n";
                      loop false (pget conf base e);
                      Wserver.printf "</dd>\n")
                   (Array.to_list children);
                 Wserver.printf "</dl>\n"
             | Some (_, false) -> ()
             | None ->
                 if Array.length children <> 0 then
                   begin
                     Wserver.printf "<dl>";
                     begin
                       Wserver.printf "<dd>";
                       Wserver.printf "...";
                       Wserver.printf "</dd>"
                     end;
                     Wserver.printf "</dl>\n"
                   end
             end;
             false)
          true family_list
      in
      ()
  in
  loop

let print_one_branch conf base bh psn lev =
  let p = bh.bh_ancestor in
  if bh.bh_well_named_ancestors = []
  then let x = sou base (get_surname p) in print_branch conf base psn x lev p
  else begin
    if is_hidden p then Wserver.printf "&lt;&lt;"
    else wprint_geneweb_link conf (Util.acces conf base p) "&lt;&lt;" ;
    Wserver.printf "\n";
    List.iter
      (fun p ->
         let x = sou base (get_surname p) in
         Wserver.printf "<dl>\n";
         Wserver.printf "<dd>\n";
         print_branch conf base psn x false p;
         Wserver.printf "</dd>\n";
         Wserver.printf "</dl>\n")
      bh.bh_well_named_ancestors
  end

let print_one_surname_by_branch conf base x xl (bhl, str) =
  let ancestors =
    match p_getenv conf.env "order" with
      Some "d" ->
        let born_before p1 p2 =
          match
            Adef.od_of_cdate (get_birth p1), Adef.od_of_cdate (get_birth p2)
          with
          | Some d1, Some d2 -> Date.compare_date d1 d2
          | _, None -> -1
          | None, _ -> 1
        in
        List.sort (fun p1 p2 -> born_before p1.bh_ancestor p2.bh_ancestor) bhl
    | _ ->
        List.sort
          (fun p1 p2 ->
             Utf8.compare (p_first_name base p1.bh_ancestor)
               (p_first_name base p2.bh_ancestor))
          bhl
  in
  let len = List.length ancestors in
  let psn =
    match p_getenv conf.env "alwsurn" with
      Some x -> x = "yes"
    | None ->
        try List.assoc "always_surname" conf.base_env = "yes" with
          Not_found -> false
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.printf "%s" x
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.printf "%s<a href=\"%sm=N&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (code_varenv x) x)
        (StrSet.elements xl)
  in
  let br = p_getint conf.env "br" in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  (* Menu afficher par branche/ordre alphabetique *)
  if br = None then print_branch_to_alphabetic conf x len;
  Wserver.printf "<div style=\"white-space:nowrap\">\n";
  if len > 1 && br = None then
    begin
      Wserver.printf "<dl>\n";
      begin let _ =
        List.fold_left
          (fun n bh ->
             Wserver.printf "<dt>";
             if conf.cancel_links then Wserver.printf "%d." n
             else
               begin
                 Wserver.printf
                   "<a href=\"%sm=N&v=%s&br=%d\" rel=\"nofollow\">"
                   (commd conf) (Util.code_varenv str) n;
                 Wserver.printf "%d." n;
                 Wserver.printf "</a>"
               end;
             Wserver.printf "</dt>\n";
             Wserver.printf "<dd>\n";
             print_one_branch conf base bh psn false;
             Wserver.printf "</dd>\n";
             n + 1)
          1 ancestors
      in
        ()
      end;
      Wserver.printf "</dl>\n"
    end
  else
    begin let _ =
      List.fold_left
        (fun n bh ->
           if br = None || br = Some n then
             print_one_branch conf base bh psn true;
           n + 1)
        1 ancestors
    in
      ()
    end;
  Wserver.printf "</div>\n";
  Hutil.trailer conf

let print_several_possible_surnames x conf base (_, homonymes) =
  let fx = x in
  List.iter (fun sn -> print_endline @@ Printf.sprintf "%s: %s%!\n" __LOC__ sn) homonymes ;
  let x = match homonymes with x :: _ -> x | _ -> x in
  let title _ =
    Wserver.printf "%s \"%s\" : %s"
      (Utf8.capitalize (transl_nth conf "surname/surnames" 0)) fx
      (transl conf "specify")
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  let list =
    List.map
      (fun sn ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = name_unaccent txt in ord, txt, sn)
      homonymes
  in
  let list = List.sort compare list in
  let access txt sn =
    geneweb_link conf ("m=N&v=" ^ code_varenv sn ^ "&t=A") txt
  in
  Util.wprint_in_columns conf (fun (ord, _, _) -> ord)
    (fun (_, txt, sn) -> Wserver.printf "%s" (access txt sn)) list;
  Wserver.printf "<p>\n";
  Wserver.printf "<em style=\"font-size:80%%\">\n";
  Wserver.printf "%s " (Utf8.capitalize (transl conf "click"));
  Wserver.printf "<a href=\"%sm=N&o=i&v=%s\">%s</a>\n" (commd conf)
    (if List.length homonymes = 1 then code_varenv x ^ "&t=A" else code_varenv fx)
    (transl conf "here");
  Wserver.printf "%s" (transl conf "for the first names by alphabetic order");
  Wserver.printf ".</em>\n";
  Wserver.printf "</p>\n";
  Hutil.trailer conf

let print_family_alphabetic x conf base liste =
  let homonymes =
    let list =
      List.fold_left
        (fun list p ->
           if List.exists (eq_istr (get_surname p)) list then list
           else get_surname p :: list)
        [] liste
    in
    let set =
      List.fold_left (fun set istr -> StrSet.add (sou base istr) set)
        StrSet.empty list
    in
    List.sort compare (StrSet.elements set)
  in
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match
             Utf8.compare (p_first_name base x2) (p_first_name base x1)
           with
             0 -> compare (get_occ x1) (get_occ x2)
           | n -> n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_first_name base x in
         match l with
           (p, l1) :: l when Utf8.compare px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  match liste with
    [] -> surname_not_found conf x
  | _ ->
      let title h =
        let access x =
          if h || List.length homonymes = 1 then x
          else geneweb_link conf ("m=N&o=i&v=" ^ code_varenv x ^ "&t=A") x
        in
        Mutil.list_iter_first
          (fun first x ->
             Wserver.printf "%s%s" (if first then "" else ", ") (access x))
          homonymes
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      (* Si on est dans un calcul de parenté, on affiche *)
      (* l'aide sur la sélection d'un individu.          *)
      Util.print_tips_relationship conf;
      (* Menu afficher par branche/ordre alphabetique *)
      print_alphabetic_to_branch conf x;
      print_alphab_list (fun (p, _) -> first_char p)
        (print_elem conf base false) liste;
      Hutil.trailer conf

let search_aux persons_of_absolute strings spi split conf base x =
  let (list, inj) =
    if p_getenv conf.env "t" = Some "A"
    then persons_of_absolute conf base x
    else if x = ""
    then ([], (fun _ -> assert false))
    else new_persons_of_fsname base strings (spi_find (spi base)) split x
  in
  let lower = String.lowercase_ascii in
  let list =
    List.map begin fun (str, _, iperl) ->
      lower str, (StrSet.add str StrSet.empty, iperl)
    end list
    |> List.fold_left merge_insert []
  in
  let x = lower x in
  (* Put the best match at the first place of the list *)
  let rec sort acc = function
    | [] -> list
    | (x_hd, _) as hd :: tl ->
      if x_hd = x then hd :: List.rev_append acc tl
      else sort (hd :: acc) tl
  in
  sort [] list, inj

let filter_hidden conf base =
  List.filter_map begin fun i ->
    let p = pget conf base i in
    if not (is_hide_names conf p) || authorized_age conf base p
    then Some p
    else None
  end

let print_first_name conf base x list =
  match list with
  | [] -> first_name_not_found conf x
  | [_, (strl, iperl)] ->
    let () = Perso.build_sosa_ht conf base in
    filter_hidden conf base (List.sort_uniq compare iperl)
    |> first_name_print_list conf base x strl
  | _ -> select_first_name conf x list

let convert_ancestors =
  List.map begin fun br ->
    { bh_ancestor = List.hd br ; bh_well_named_ancestors = List.tl br }
  end

module PerSet = Set.Make (struct type t = iper let compare = compare end)

let build_branches conf base name_inj ipers =
  Util.branches conf base name_inj ipers
  |> List.map (List.map (pget conf base))
  |> convert_ancestors

let print_surname conf base not_found_fun x (list, inj) =
  let ipers =
    List.map (fun (_, (_, list)) -> list) list
    |> List.flatten
    |> List.sort_uniq compare
  in
  let () = Perso.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
  | Some "i" ->
    filter_hidden conf base ipers
    |> print_family_alphabetic x conf base
  | _ ->
    let bhl = build_branches conf base inj ipers in
    match bhl, list with
    | [], _ -> not_found_fun conf x
    | _, [s, (strl, _)] ->
      print_one_surname_by_branch conf base x strl (bhl, s)
    | _ ->
      let strl = List.map fst list in
      print_several_possible_surnames x conf base (bhl, strl)

let search_surname base x =
  search_aux
    persons_of_absolute_surname
    base_strings_of_surname
    persons_of_surname
    Name.split_sname base x

let search_first_name base x =
  search_aux
    persons_of_absolute_first_name
    base_strings_of_first_name
    persons_of_first_name
    Name.split_fname base x

let first_name_print conf base x =
  search_first_name conf base x
  |> fst
  |> print_first_name conf base x

let surname_print conf base not_found_fun x =
  search_surname conf base x
  |> print_surname conf base not_found_fun x
