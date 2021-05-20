(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Def
open Geneweb.Gwdb
open Geneweb.Util

module Alln = Geneweb.Alln
module Date = Geneweb.Date
module DateDisplay = Geneweb.DateDisplay
module Hutil = Geneweb.Hutil
module Gutil = Geneweb.Gutil
module Output = Geneweb.Output
module Perso = V7_perso
module StrSet = Mutil.StrSet
module Templ = V7_templ
module Util = Geneweb.Util

let not_found conf txt x =
  let title _ = Output.printf conf "%s: \"%s\"" (Utf8.capitalize_fst txt) (Util.escape_html x) in
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
  Output.print_string conf "<table class=\"display_search\">\n";
  Output.print_string conf "<tr>";
  Output.print_string conf "<td>";
  Output.print_string conf "<b>";
  Output.print_string conf
    (Utf8.capitalize_fst (transl_nth conf "display by/branch/alphabetic order" 0));
  Output.print_string conf "</b>";
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.printf conf
    "<img src=\"%s/%s\" alt=\"\" title=\"\">\n"
    (Util.image_prefix conf) "picto_branch.png";
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.printf conf "%s (%d)"
    (transl_nth conf "display by/branch/alphabetic order" 1) nb_branch;
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.printf conf
    "<img src=\"%s/%s\" alt=\"\" title=\"\">\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png";
  Output.print_string conf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Output.print_string conf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Output.printf conf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (Mutil.encode x ^ "&t=A");
      Output.print_string conf
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Output.print_string conf "</a>"
    end
  else
    begin
      Output.printf conf "<a href=\"%sm=N&o=i&v=%s\" rel=\"nofollow\">"
        (commd conf) (Mutil.encode x ^ "&t=N");
      Output.print_string conf
        (transl_nth conf "display by/branch/alphabetic order" 2);
      Output.print_string conf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Output.print_string conf "</td>";
  Output.print_string conf "</tr>";
  Output.print_string conf "</table>\n";
  Output.print_string conf "<br>\n"


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
  Output.print_string conf "<table class=\"display_search\">";
  Output.print_string conf "<tr>";
  Output.print_string conf "<td>";
  Output.print_string conf "<b>";
  Output.print_string conf
    (Utf8.capitalize_fst (transl_nth conf "display by/branch/alphabetic order" 0));
  Output.print_string conf "</b>";
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.printf conf
    "<img src=\"%s/%s\" alt=\"\" title=\"\">\n"
    (Util.image_prefix conf) "picto_branch.png";
  Output.print_string conf "</td>";
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Output.print_string conf "<td>";
  if p_getenv conf.env "t" = Some "A" then
    begin
      Output.printf conf "<a href=\"%sm=N&v=%s\" rel=\"nofollow\">" (commd conf)
        (Mutil.encode x ^ "&t=A");
      Output.print_string conf
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Output.print_string conf "</a>"
    end
  else
    begin
      Output.printf conf "<a href=\"%sm=NG&sn=%s\" rel=\"nofollow\">" (commd conf)
        (Mutil.encode x);
      Output.print_string conf
        (transl_nth conf "display by/branch/alphabetic order" 1);
      Output.print_string conf "</a>"
    end;
  (* Ne pas oublier l'attribut nofollow pour les robots *)
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.printf conf
    "<img src=\"%s/%s\" alt=\"\" title=\"\">\n"
    (Util.image_prefix conf) "picto_alphabetic_order.png";
  Output.print_string conf "</td>";
  Output.print_string conf "<td>";
  Output.print_string conf
    (transl_nth conf "display by/branch/alphabetic order" 2);
  Output.print_string conf "</td>";
  Output.print_string conf "</tr>";
  Output.print_string conf "</table>";
  Output.print_string conf "<br>\n"

let match_fnames word str x =
  if word then
    let rexp = Str.regexp (".*\\b" ^ x ^ "\\b.*") in
    Str.string_match rexp str 0
  else Mutil.contains str x

let persons_of_fsname conf base base_strings_of_fsname find proj x =
  (* list of strings index corresponding to the crushed lower first name
     or surname "x" *)
  let istrl = base_strings_of_fsname base x in
  (* selecting the persons who have this first name or surname *)
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
         let str = Mutil.nominative (sou base istr) in
         if Name.crush_lower str = x ||
            List.mem x (List.map Name.crush_lower (Mutil.surnames_pieces str))
         then
           let iperl = find istr in
           (* maybe they are not the good ones because of changes in the
              database; checking... *)
           let iperl =
             List.fold_left
               (fun iperl iper ->
                  if eq_istr (proj (pget conf base iper)) istr then
                    iper :: iperl
                  else iperl)
               [] iperl
           in
           if iperl = [] then l else (str, istr, iperl) :: l
         else l)
      istrl []
  in
  let (l, name_inj) =
    let (l1, name_inj) =
      let x = Name.lower x in
      List.fold_right
        (fun (str, istr, iperl) l ->
           if x = Name.lower str then (str, istr, iperl) :: l else l)
        l [],
      Name.lower
    in
    let (l1, name_inj) =
      if l1 = [] then
        let x = Name.strip_lower x in
        List.fold_right
          (fun (str, istr, iperl) l ->
             if x = Name.strip_lower str then (str, istr, iperl) :: l else l)
          l [],
        Name.strip_lower
      else l1, name_inj
    in
    if l1 = [] then l, Name.crush_lower else l1, name_inj
  in
  l, name_inj

let print_elem conf base is_surname (p, xl) =
  Mutil.list_iter_first
    (fun first x ->
       let iper = get_iper x in
       if not first then Output.print_string conf "</li>\n<li>\n  ";
       V7_sosa.print_sosa conf base x true;
       Output.printf conf "<a href=\"%s%s\" id=\"i%s\">" (commd conf)
         (acces conf base x) (string_of_iper iper);
       if is_surname then
         Output.printf conf "%s%s" (surname_without_particle base p) (surname_particle base p)
       else Output.print_string conf (if p = "" then "?" else p);
       Output.print_string conf "</a>";
       Output.print_string conf (DateDisplay.short_dates_text conf base x);
       Output.print_string conf "<em>";
       specify_homonymous conf base x true;
       Output.print_string conf "</em>")
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

type 'a env =
    Vlist_data of (string * (string * int) list) list
  | Vlist_ini of string list
  | Vlist_value of (string * (string * int) list) list
  | Venv_keys of (string * int) list
  | Vint of int
  | Vstring of string
  | Vbool of bool
  | Vother of 'a
  | Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

(* TODO find a way tu use get_vother, set_vother from templ.camlp5 *)
let buttons_fnames conf =
  V7_interp.gen_interp false conf "buttons_fnames"
    {Templ.eval_var = (fun _ -> raise Not_found);
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = (fun _ -> raise Not_found) }
    [] ()

let print_other_list conf _base list =
  let s_title = Printf.sprintf "%s" (Utf8.capitalize (transl conf "see also")) in
  let s_colon = Printf.sprintf "%s" (transl conf ":") in
  Wserver.printf "<span>%s%s</span>\n" s_title s_colon;
  Mutil.list_iter_first (fun first (fn, c) ->
      Wserver.printf "%s<a href=\"%sm=P&v=%s&other=on\">%s</a> (%d)"
        (if first then "" else ", ") (commd conf) (Mutil.encode fn) fn c)
    list

let other_fnames conf base x =
  match Alln.select_names conf base false "" max_int with
  | (Alln.Result list, _len) ->
    let exact = p_getenv conf.env "t" = Some "A" in
    let word = p_getenv conf.env "word" = Some "on" in
    let x = if exact then x else Name.lower x in
    List.fold_left
      (fun l (_k, str, c) ->
         let strl = if exact then str else Name.lower str in
         if (match_fnames word strl x && strl <> x)
         then (str, c) :: l else l)
      [] list
  | (Alln.Specify _l, _len) -> [] (* TODO is [] ok? *)

let first_name_print_list conf base x1 xl liste =
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match Gutil.alphabetic (p_surname base x1) (p_surname base x2) with
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
           (p, l1) :: l when Gutil.alphabetic px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Output.print_string conf x1
    else
      Mutil.list_iter_first
        (fun first x ->
           Output.printf conf "%s<a href=\"%sm=P&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (Mutil.encode x) x)
        (StrSet.elements xl)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  buttons_fnames conf;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  let other = p_getenv conf.env "other" = Some "on" in
  if other then
    let listo =
        List.fold_left (fun l x ->
            (other_fnames conf base x) :: l) [] (StrSet.elements xl)
    in
    let listo = List.flatten listo |> List.sort_uniq compare in
    if listo <> [] then print_other_list conf base listo else ()
  else ();
  let list =
    List.map
      (fun (sn, ipl) ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = name_unaccent txt in ord, txt, ipl)
      liste
  in
  let list = List.sort compare list in
  print_alphab_list conf (fun (ord, _, _) -> first_char ord)
    (fun (_, txt, ipl) -> print_elem conf base true (txt, ipl)) list;
  Hutil.trailer conf

let select_first_name conf n list =
  let title _ =
    Output.printf conf "%s \"%s\" : %s"
      (Utf8.capitalize_fst (transl_nth conf "first name/first names" 0)) n
      (transl conf "specify")
  in
  Hutil.header conf title;
  buttons_fnames conf;
  Output.print_string conf "<ul>";
  List.iter
    (fun (sstr, (strl, _)) ->
       Output.print_string conf "\n";
       Output.print_string conf "<li>" ;
       Output.printf conf "<a href=\"%sm=P&v=%s\">" (commd conf)
         (Mutil.encode sstr);
       Mutil.list_iter_first
         (fun first str ->
            Output.printf conf "%s%s" (if first then "" else ", ") str)
         (StrSet.elements strl);
       Output.print_string conf "</a>\n")
    list;
  Output.print_string conf "</ul>\n";
  Hutil.trailer conf

let rec merge_insert (sstr, (strl, iperl) as x) =
  function
    (sstr1, (strl1, iperl1) as y) :: l ->
      if sstr < sstr1 then x :: y :: l
      else if sstr > sstr1 then y :: merge_insert x l
      else (sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l
  | [] -> [x]

let persons_of_absolute base_strings_of persons_of get_field conf base x =
  let istrl = base_strings_of base x in
  List.fold_right begin fun istr l ->
    let str = sou base istr in
    if str = x then
      let iperl = spi_find (persons_of base) istr in
      let iperl =
        List.fold_left begin fun iperl iper ->
          let p = pget conf base iper in
          if eq_istr (get_field p) istr
          && (not (is_hide_names conf p) || Util.authorized_age conf base p)
          then iper :: iperl
          else iperl
        end [] iperl
      in
      if iperl = [] then l else (str, istr, iperl) :: l
    else l
  end istrl []

let persons_of_absolute_first_name =
  persons_of_absolute base_strings_of_first_name persons_of_first_name get_first_name

let persons_of_absolute_surname =
  persons_of_absolute base_strings_of_surname persons_of_surname get_surname


let first_name_print conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 347, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 348, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  match list with
    [] -> first_name_not_found conf x
  | [_, (strl, iperl)] ->
      let iperl = List.sort_uniq compare iperl in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf x list

let has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (p_surname base (pget conf base ip)) name)
    (Array.to_list (get_children des))

(* List selection bullets *)

let bullet_sel_txt = "o"
let bullet_unsel_txt = "+"
let bullet_nosel_txt = "o"
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
      Output.printf conf "<a id=\"if%s\" href=\"%s%s%s%s\" rel=\"nofollow\">" txt
        (prefix_base conf) req (if sel then "&u=" ^ txt else "")
        (if sel || List.mem_assoc "u" conf.env then "#if" ^ txt else "");
      Output.print_string conf (if sel then bullet_sel_txt else bullet_unsel_txt);
      Output.print_string conf "</a>";
      Output.print_string conf "\n"
  | None -> Output.printf conf "%s\n" bullet_nosel_txt

let unselected_bullets conf =
  List.fold_left
    (fun sl (k, v) ->
       try if k = "u" then ifam_of_string v :: sl else sl with Failure _ -> sl)
    [] conf.env

let alphabetic1 n1 n2 = Gutil.alphabetic_utf_8 n1 n2

type 'a branch_head = { bh_ancestor : 'a; bh_well_named_ancestors : 'a list }

let print_branch conf base psn name =
  let unsel_list = unselected_bullets conf in
  let rec loop p =
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
    let print_elem p with_link with_id with_sn =
      let hl =
        if with_link then "strong" else "em"
      in
      let render p =
        if with_link then
          if with_id then Util.reference conf base p
          else Util.reference_noid conf base p 
        else (fun s -> s)
      in
      V7_sosa.print_sosa conf base p with_link;
      Output.printf conf "<%s>%s</%s>%s\n"
        hl
        (render p
           (if is_hide_names conf p && not (authorized_age conf base p) then "x"
            else if not psn && not with_sn && p_surname base p = name then
              person_text_without_surname conf base p
            else person_text conf base p))
        hl
        (DateDisplay.short_dates_text conf base p)
    in
    Output.print_string conf "<li>";
    print_selection_bullet conf first_select;
    print_elem p true true false;
    if Array.length (get_family u) = 0 then ()
    else
      let _ =
        List.fold_left
          (fun first (fam, sp, select) ->
             if not first then begin
               Output.print_string conf "<li>";
               print_selection_bullet conf select;
               print_elem p false true false
             end;
             Output.print_string conf " &amp;";
             Output.printf conf "%s\n"
               (DateDisplay.short_marriage_date_text conf base fam p sp);
             print_elem sp true false true;
             let children = get_children fam in
             begin match select with
               Some (_, true) ->
                 Output.print_string conf "<ul>\n";
                 List.iter
                   (fun e ->
                      loop (pget conf base e);
                      Output.print_string conf "</li>\n")
                   (Array.to_list children);
                 Output.print_string conf "</ul>\n"
             | Some (_, false) -> ()
             | None ->
                 if Array.length children <> 0 then
                   Output.print_string conf "<ul class=\"posterity\">\
                                   <li>...</li>\
                                   </ul>\n";
             end;
             Output.print_string conf "</li>";
             false)
          true family_list
      in
      ();
    Output.print_string conf "</li>"
  in
  loop

let print_one_branch conf base bh psn =
  Output.print_string conf "<ul>\n";
  let p = bh.bh_ancestor in
  if bh.bh_well_named_ancestors = []
  then
    let x = sou base (get_surname p) in
    print_branch conf base psn x p
  else begin
    Output.print_string conf "<li>\n";
    if is_hidden p then Output.print_string conf "&lt;&lt;"
    else wprint_geneweb_link conf (Util.acces conf base p) "&lt;&lt;" ;
    Output.print_string conf "\n<ul>\n";
    List.iter
      (fun p ->
         let x = sou base (get_surname p) in
         print_branch conf base psn x p)
      bh.bh_well_named_ancestors;
    Output.print_string conf "</ul></li>\n"
  end;
  Output.print_string conf "</ul>\n"

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
             alphabetic1 (p_first_name base p1.bh_ancestor)
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
    if h || p_getenv conf.env "t" = Some "A" then Output.print_string conf x
    else
      Mutil.list_iter_first
        (fun first x ->
           Output.printf conf "%s<a href=\"%sm=N&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (Mutil.encode x) x)
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
  Output.print_string conf "<div id=\"surname_by_branch\">\n";
  if len > 1 && br = None then
    begin
      Output.print_string conf "<dl>\n";
      begin let _ =
        List.fold_left
          (fun n bh ->
             Output.print_string conf "<dt>";
             Output.printf conf
               "<a href=\"%sm=N&v=%s&br=%d\" rel=\"nofollow\">"
               (commd conf) (Mutil.encode str) n;
             Output.printf conf "%d." n;
             Output.print_string conf "</a>";
             Output.print_string conf "</dt>\n";
             Output.print_string conf "<dd>\n";
             print_one_branch conf base bh psn;
             Output.print_string conf "</dd>";
             n + 1)
          1 ancestors
      in
        ()
      end;
      Output.print_string conf "</dl>\n"
    end
  else
    begin let _ =
      List.fold_left
        (fun n bh ->
           if br = None || br = Some n then
             print_one_branch conf base bh psn;
           n + 1)
        1 ancestors
    in
      ()
    end;
  Output.print_string conf "</div>\n";
  Hutil.trailer conf

let print_several_possible_surnames x conf base (_, homonymes) =
  let fx = x in
  let x =
    match homonymes with
      x :: _ -> x
    | _ -> x
  in
  let title _ =
    Output.printf conf "%s \"%s\" : %s"
      (Utf8.capitalize_fst (transl_nth conf "surname/surnames" 0)) fx
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
    geneweb_link conf ("m=N&v=" ^ Mutil.encode sn ^ "&t=N") txt
  in
  buttons_fnames conf;
  Util.wprint_in_columns conf (fun (ord, _, _) -> ord)
    (fun (_, txt, sn) -> Output.print_string conf (access txt sn)) list;
  Output.print_string conf "<p>\n";
  Output.print_string conf "<em style=\"font-size:80%%\">\n";
  Output.printf conf "%s " (Utf8.capitalize_fst (transl conf "click"));
  Output.printf conf "<a href=\"%sm=N&o=i&v=%s\">%s</a>\n" (commd conf)
    (if List.length homonymes = 1 then Mutil.encode x ^ "&t=A"
     else Mutil.encode fx)
    (transl conf "here");
  Output.print_string conf (transl conf "for the first names by alphabetic order");
  Output.print_string conf ".</em>\n";
  Output.print_string conf "</p>\n";
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
             alphabetic1 (p_first_name base x2) (p_first_name base x1)
           with
             0 -> compare (get_occ x1) (get_occ x2)
           | n -> n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_first_name base x in
         match l with
           (p, l1) :: l when alphabetic1 px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  match liste with
    [] -> surname_not_found conf x
  | _ ->
      let title h =
        let access x =
          if h || List.length homonymes = 1 then x
          else geneweb_link conf ("m=N&o=i&v=" ^ Mutil.encode x ^ "&t=A") x
        in
        Mutil.list_iter_first
          (fun first x ->
             Output.printf conf "%s%s" (if first then "" else ", ") (access x))
          homonymes
      in
      Hutil.header conf title;
      Hutil.print_link_to_welcome conf true;
      (* Si on est dans un calcul de parenté, on affiche *)
      (* l'aide sur la sélection d'un individu.          *)
      Util.print_tips_relationship conf;
      (* Menu afficher par branche/ordre alphabetique *)
      print_alphabetic_to_branch conf x;
      print_alphab_list conf (fun (p, _) -> first_char p)
        (print_elem conf base false) liste;
      Hutil.trailer conf

let insert_at_position_in_family children ip ipl =
  let rec loop child_list ipl =
    match child_list, ipl with
      ip1 :: ipl1, ip2 :: ipl2 ->
        if ip1 = ip2 then if ip = ip1 then ipl else ip2 :: loop ipl1 ipl2
        else if ip = ip1 then ip1 :: ipl
        else loop ipl1 ipl
    | _ :: _, [] -> [ip]
    | [], _ -> assert false
  in
  loop (Array.to_list children) ipl

let select_ancestors conf base name_inj ipl =
  let str_inj s = name_inj (sou base s) in
  List.fold_left
    (fun bhl ip ->
       let p = pget conf base ip in
       match get_parents p with
         Some ifam ->
           let fam = foi base ifam in
           let ifath = get_father fam in
           let imoth = get_mother fam in
           let fath = pget conf base ifath in
           let moth = pget conf base imoth in
           let s = str_inj (get_surname p) in
           if str_inj (get_surname fath) <> s &&
              str_inj (get_surname moth) <> s
           then
             let rec loop =
               function
                 bh :: bhl ->
                   if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
                     let bh =
                       {bh with bh_well_named_ancestors =
                         insert_at_position_in_family (get_children fam) ip
                           bh.bh_well_named_ancestors}
                     in
                     bh :: bhl
                   else bh :: loop bhl
               | [] -> [{bh_ancestor = ifath; bh_well_named_ancestors = [ip]}]
             in
             loop bhl
           else bhl
       | _ ->
           let bh = {bh_ancestor = ip; bh_well_named_ancestors = []} in
           bh :: bhl)
    [] ipl

module PerSet = Set.Make (struct type t = iper let compare = compare end)

let surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 825, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
    Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> pget conf base ip :: ipl) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
             {bh_ancestor = pget conf base bh.bh_ancestor;
              bh_well_named_ancestors =
                List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match bhl, list with
        [], _ -> not_found_fun conf x
      | _, [s, (strl, _)] ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map fst list in
          print_several_possible_surnames x conf base (bhl, strl)


(**/**)
(* TODO: refactoring avec les fonctions ci-dessus !!! *)


let search_surname conf base x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 896, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  let bhl = select_ancestors conf base name_inj iperl in
  let bhl =
    List.map
      (fun bh ->
         {bh_ancestor = pget conf base bh.bh_ancestor;
          bh_well_named_ancestors =
            List.map (pget conf base) bh.bh_well_named_ancestors})
      bhl
  in
  match bhl, list with
    [], _ -> []
  | _, [_, (_, iperl)] -> iperl
  | _ -> []

let search_surname_print conf base not_found_fun x =
  let (list, name_inj) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_surname conf base x, (fun x -> x)
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 942, 29)))
    else
      persons_of_fsname conf base base_strings_of_surname
        (spi_find (persons_of_surname base)) get_surname x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  let (iperl, _) =
    List.fold_right
      (fun (str, (_, iperl1)) (iperl, strl) ->
         let len = List.length iperl1 in
         let strl =
           try
             let len1 = List.assoc str strl in
             (str, len + len1) :: List.remove_assoc str strl
           with Not_found -> (str, len) :: strl
         in
         List.fold_right PerSet.add iperl1 iperl, strl)
      list (PerSet.empty, [])
  in
  let iperl = PerSet.elements iperl in
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  match p_getenv conf.env "o" with
    Some "i" ->
      let pl =
        List.fold_right (fun ip ipl -> pget conf base ip :: ipl) iperl []
      in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      print_family_alphabetic x conf base pl
  | _ ->
      let bhl = select_ancestors conf base name_inj iperl in
      let bhl =
        List.map
          (fun bh ->
             {bh_ancestor = pget conf base bh.bh_ancestor;
              bh_well_named_ancestors =
                List.map (pget conf base) bh.bh_well_named_ancestors})
          bhl
      in
      match bhl, list with
        [], _ -> not_found_fun conf x
      | _, [s, (strl, _)] ->
          print_one_surname_by_branch conf base x strl (bhl, s)
      | _ ->
          let strl = List.map (fun (s, _) -> s) list in
          print_several_possible_surnames x conf base (bhl, strl)

let search_first_name conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 1007, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 1008, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  List.fold_right merge_insert list []

let search_first_name_print conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 1025, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 1026, 29)))
    else
      persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  match list with
    [] -> first_name_not_found conf x
  | [_, (strl, iperl)] ->
      let iperl = List.sort_uniq compare iperl in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> select_first_name conf x list
