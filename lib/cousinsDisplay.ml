(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util
open Cousins

let default_max_cnt = Cousins.default_max_cnt

(* there is a mismatch between cousins degree and "v1" parameter
   which is the number of generation we go back to find a common ancestor *)
let brother_label conf x =
  match x with
  | 1 -> transl conf "siblings"
  | 2 -> transl conf "cousins"
  | 3 -> transl conf "2nd cousins"
  | 4 -> transl conf "3rd cousins"
  | n ->
      Printf.sprintf
        (ftransl conf "%s cousins")
        (transl_nth conf "nth (cousin)" (n - 1))

let cnt = ref 0
let cnt_sp = ref 0

let give_access conf base ~cnt_sp ia_asex p1 b1 p2 b2 =
  let reference _ _ p (s : Adef.safe_string) =
    if is_empty_person p then s
    else
      let href =
        commd conf ^^^ "m=RL&"
        ^<^ acces_n conf base (Adef.escaped "1") p1
        ^^^ "&b1="
        ^<^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1))
        ^<^ "&"
        ^<^ acces_n conf base (Adef.escaped "2") p2
        ^^^ "&b2="
        ^<^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2))
        ^<^ ((if (List.assoc_opt "spouse" conf.env :> string option) = Some "on"
             then Adef.encoded "&spouse=on"
             else Adef.encoded "")
             ^^^ (if
                  (List.assoc_opt "image" conf.env :> string option)
                  = Some "off"
                 then Adef.encoded "&image=off"
                 else Adef.encoded "")
             ^^^ "&bd="
             ^<^ Option.value ~default:(Adef.encoded "0")
                   (List.assoc_opt "bd" conf.env)
              :> Adef.escaped_string)
      in
      "<a href=\"" ^<^ (href :> Adef.safe_string) ^^^ "\">" ^<^ s ^>^ "</a>"
  in
  let reference_sp p3 _ _ p s =
    if is_empty_person p then s
    else
      let href =
        commd conf ^^^ "m=RL&"
        ^<^ acces_n conf base (Adef.escaped "1") p1
        ^^^ "&b1="
        ^<^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1))
        ^<^ "&"
        ^<^ acces_n conf base (Adef.escaped "2") p2
        ^^^ "&b2="
        ^<^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2))
        ^<^ "&"
        ^<^ acces_n conf base (Adef.escaped "4") p3
        ^^^ ((if (List.assoc_opt "spouse" conf.env :> string option) = Some "on"
             then Adef.encoded "&spouse=on"
             else Adef.encoded "")
             ^^^ (if
                  (List.assoc_opt "image" conf.env :> string option)
                  = Some "off"
                 then Adef.encoded "&image=off"
                 else Adef.encoded "")
             ^^^ "&bd="
             ^<^ Option.value ~default:(Adef.encoded "0")
                   (List.assoc_opt "bd" conf.env)
              :> Adef.escaped_string)
      in
      "<a href=\"" ^<^ (href :> Adef.safe_string) ^^^ "\">" ^<^ s ^>^ "</a>"
  in
  let print_nospouse _ =
    SosaCache.print_sosa conf base p2 true;
    Output.print_string conf
      (NameDisplay.gen_person_title_text reference conf base p2);
    Output.print_string conf (DateDisplay.short_dates_text conf base p2)
  in
  let print_spouse sp first =
    incr cnt_sp;
    if first then (
      SosaCache.print_sosa conf base p2 true;
      Output.print_string conf
        (NameDisplay.gen_person_title_text reference conf base p2))
    else (
      Output.print_sstring conf "<br>";
      Output.print_string conf (NameDisplay.person_title_text conf base p2));
    Output.print_string conf (DateDisplay.short_dates_text conf base p2);
    Output.print_sstring conf " &amp; ";
    SosaCache.print_sosa conf base sp true;
    Output.print_string conf
      (NameDisplay.gen_person_title_text (reference_sp sp) conf base sp);
    Output.print_string conf (DateDisplay.short_dates_text conf base sp)
  in
  if p_getenv conf.env "spouse" = Some "on" then
    match get_family p2 with
    | [||] -> print_nospouse ()
    | u ->
        Array.iteri
          (fun i ifam ->
            let cpl = foi base ifam in
            let sp =
              if get_sex p2 = Female then pget conf base (get_father cpl)
              else pget conf base (get_mother cpl)
            in
            print_spouse sp (i = 0))
          u
  else print_nospouse ()

let rec print_descend_upto conf base max_cnt ini_p ini_br lev children =
  if lev > 0 && !cnt < max_cnt then (
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun (ip, ia_asex, rev_br) ->
        let p = pget conf base ip in
        (* détecter l'époux de p, parent des enfants qui seront listés *)
        let get_spouse base iper ifam =
          let f = foi base ifam in
          if iper = get_father f then poi base (get_mother f)
          else poi base (get_father f)
        in
        (* if more than one spouse, this will be split on multiple lines *)
        (* we ignore the case where two spouses, but only one with descendants! *)
        let with_sp =
          if Array.length (get_family p) = 1 then
            let sp = get_spouse base ip (get_family p).(0) in
            " " ^<^ Util.transl conf "with" ^<^ " "
            ^<^ NameDisplay.person_title_text conf base sp
          else Adef.safe ""
        in
        let br = List.rev ((ip, get_sex p) :: rev_br) in
        let is_valid_rel = br_inter_is_empty ini_br br in
        if is_valid_rel && !cnt < max_cnt && has_desc_lev conf base lev p then (
          if lev <= 2 then (
            Output.print_sstring conf "<li>";
            if lev = 1 then (
              give_access conf base ~cnt_sp ia_asex ini_p ini_br p br;
              incr cnt)
            else
              let s : Adef.safe_string =
                NameDisplay.person_title_text conf base p
              in
              transl_a_of_gr_eq_gen_lev conf
                (transl_nth conf "child/children" 1)
                (s :> string)
                (s :> string)
              |> Util.translate_eval |> Utf8.capitalize_fst
              |> Output.print_sstring conf;
              Output.print_string conf with_sp;
              Output.print_sstring conf (Util.transl conf ":");
              Output.print_sstring conf
                (if (with_sp :> string) = "" then "<br>" else " "));
          (* the function children_of returns *all* the children of ip *)
          Array.iter
            (fun ifam ->
              let children =
                List.map
                  (fun i -> (i, ia_asex, (get_iper p, get_sex p) :: rev_br))
                  (children_of_fam base ifam)
              in
              let sp = get_spouse base ip ifam in
              if
                Array.length (get_family p) > 1
                && lev >= 2
                && List.length children > 0
                && has_desc_lev conf base lev sp
              then (
                Output.print_sstring conf (Util.transl conf "with");
                Output.print_sstring conf " ";
                Output.print_string conf
                  (NameDisplay.person_title_text conf base sp);
                Output.print_sstring conf (Util.transl conf ":"));
              print_descend_upto conf base max_cnt ini_p ini_br (lev - 1)
                children)
            (get_family p);
          if lev <= 2 then Output.print_sstring conf "</li>"))
      children;
    Output.print_sstring conf "</ul>")

let print_cousins_side_of conf base max_cnt a ini_p ini_br lev1 lev2 tips =
  let sib = siblings conf base (get_iper a) in
  if List.exists (sibling_has_desc_lev conf base lev2) sib then (
    if tips then Util.print_tips_relationship conf;
    if lev1 > 1 then (
      Output.print_sstring conf "<li>";
      [
        (NameDisplay.gen_person_title_text NameDisplay.no_reference conf base a
          : Adef.safe_string
          :> string);
      ]
      |> cftransl conf "on %s's side"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf (Util.transl conf ":"));
    let sib = List.map (fun (ip, ia_asex) -> (ip, ia_asex, [])) sib in
    print_descend_upto conf base max_cnt ini_p ini_br lev2 sib;
    if lev1 > 1 then Output.print_sstring conf "</li>";
    true)
  else false

let print_cousins_lev conf base max_cnt p lev1 lev2 =
  let first_sosa =
    let rec loop sosa lev =
      if lev <= 1 then sosa else loop (Sosa.twice sosa) (lev - 1)
    in
    loop Sosa.one lev1
  in
  let last_sosa = Sosa.twice first_sosa in
  if lev1 > 1 then Output.print_sstring conf "<ul>";
  let some =
    let rec loop sosa some print_tips =
      if !cnt < max_cnt && Sosa.gt last_sosa sosa then
        let some =
          match Util.old_branch_of_sosa conf base (get_iper p) sosa with
          | Some ((ia, _) :: _ as br) ->
              print_cousins_side_of conf base max_cnt (pget conf base ia) p br
                lev1 lev2 print_tips
              || some
          | _ -> some
        in
        loop (Sosa.inc sosa 1) some false
      else some
    in
    loop first_sosa false true
  in
  if not some then (
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "no match"));
    Output.print_sstring conf ". ");
  if lev1 > 1 then Output.print_sstring conf "</ul>"

(* HTML main *)

let print_cousins conf base p lev1 lev2 =
  let title h =
    let txt_fun a =
      let txt = NameDisplay.fullname_html_of_person conf base p in
      transl_a_of_gr_eq_gen_lev conf a
        (if h then
         ((Util.escape_html (NameDisplay.fullname_str_of_person conf base p)
            :> Adef.safe_string)
           :> string)
        else (txt : Adef.safe_string :> string))
        (txt : Adef.safe_string :> string)
    in
    if lev1 = lev2 then
      let s = txt_fun (brother_label conf lev1) in
      Output.print_sstring conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 2 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "an uncle/an aunt" 4) in
      Output.print_sstring conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 3 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "a great-uncle/a great-aunt" 4) in
      Output.print_sstring conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 2 then
      let s = txt_fun (transl_nth conf "a nephew/a niece" 4) in
      Output.print_sstring conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 3 then
      let s = txt_fun (transl_nth conf "a great-nephew/a great-niece" 4) in
      Output.print_sstring conf (Utf8.capitalize_fst (Util.translate_eval s))
    else (
      Output.print_sstring conf (Utf8.capitalize_fst (transl conf "ancestors"));
      Output.print_sstring conf " ";
      Output.print_sstring conf (string_of_int lev1);
      Output.print_sstring conf " / ";
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "descendants"));
      Output.print_sstring conf " ";
      Output.print_sstring conf (string_of_int lev2))
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env)
    with Not_found | Failure _ -> default_max_cnt
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_sstring conf "<div>";
  (*include_templ conf "cousins_tools";*)
  Output.print_sstring conf "<h2>";
  title false;
  Output.print_sstring conf "</h2>";
  Output.print_sstring conf "</div>";
  cnt := 0;
  (* Construction de la table des sosa de la base *)
  let () = SosaCache.build_sosa_ht conf base in
  print_cousins_lev conf base max_cnt p lev1 lev2;
  Output.print_sstring conf "<div><p>";
  if !cnt >= max_cnt then Output.print_sstring conf "etc... "
  else if !cnt > 1 then (
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "total"));
    Output.print_sstring conf (Util.transl conf ":");
    Output.print_sstring conf " ";
    Output.print_sstring conf (string_of_int !cnt);
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1)));
  if p_getenv conf.env "spouse" = Some "on" then (
    Output.print_sstring conf " ";
    Output.print_sstring conf (transl conf "and");
    Output.print_sstring conf " ";
    Output.print_sstring conf (string_of_int !cnt_sp);
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ transl_nth conf "spouse/spouses" 1));
    Output.print_sstring conf ".");
  Output.print_sstring conf "</p></div>";
  Hutil.trailer conf

(* TODO use Sosa module instead *)
let sosa_of_persons conf base =
  let rec loop n = function
    | [] -> n
    | ip :: list ->
        (* do no works if sex = Neuter *)
        loop
          (if get_sex (pget conf base ip) = Male then 2 * n else (2 * n) + 1)
          list
  in
  loop 1

let print_anniv conf base p dead_people level =
  let module S = Map.Make (struct
    type t = iper

    let compare = compare
  end) in
  let s_mem x m =
    try
      let _ = S.find x m in
      true
    with Not_found -> false
  in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = S.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = get_family (pget conf base ip) in
        let down_br = ip :: down_br in
        let rec loop set i =
          if i = Array.length u then set
          else
            let chil = get_children (foi base u.(i)) in
            let set =
              let rec loop set i =
                if i = Array.length chil then set
                else
                  let set = insert_desc set up_sosa down_br (n - 1) chil.(i) in
                  loop set (i + 1)
              in
              loop set 0
            in
            loop set (i + 1)
        in
        loop set 0
  in
  let set =
    let module P = Pqueue.Make (struct
      type t = iper * int * int

      let leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2
    end) in
    let a = P.add (get_iper p, 0, 1) P.empty in
    let rec loop set a =
      if P.is_empty a then set
      else
        let (ip, n, up_sosa), a = P.take a in
        let set = insert_desc set up_sosa [] (n + 3) ip in
        if n >= level then set
        else
          let a =
            match get_parents (pget conf base ip) with
            | Some ifam ->
                let cpl = foi base ifam in
                let n = n + 1 in
                let up_sosa = 2 * up_sosa in
                let a = P.add (get_father cpl, n, up_sosa) a in
                P.add (get_mother cpl, n, up_sosa + 1) a
            | None -> a
          in
          loop set a
    in
    loop S.empty a
  in
  let set =
    S.fold
      (fun ip (up_sosa, down_br) set ->
        let u = get_family (pget conf base ip) in
        let set = S.add ip (up_sosa, down_br, None) set in
        if Array.length u = 0 then set
        else
          let rec loop set i =
            if i = Array.length u then set
            else
              let cpl = foi base u.(i) in
              let c = Gutil.spouse ip cpl in
              loop (S.add c (up_sosa, down_br, Some ip) set) (i + 1)
          in
          loop set 0)
      set S.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    let href : Adef.escaped_string =
      commd conf ^^^ "m=RL&"
      ^<^ acces_n conf base (Adef.escaped "1") p
      ^^^ "&b1=" ^<^ string_of_int up_sosa ^<^ "&"
      ^<^ acces_n conf base (Adef.escaped "2")
            (Option.fold ~none:c ~some:(pget conf base) spouse)
      ^^^ "&b2="
      ^<^ string_of_int (sosa_of_persons conf base down_br)
      ^<^ (if spouse = None then "&" ^<^ acces_n conf base (Adef.escaped "4") c
          else Adef.escaped "")
      ^>^ "&spouse=on"
    in
    "<a href=\""
    ^<^ (href :> Adef.safe_string)
    ^^^ "\">"
    ^<^ NameDisplay.person_title_text conf base c
    ^>^ "</a>"
  in
  let f_scan =
    let list = ref (S.fold (fun ip b list -> (ip, b) :: list) set []) in
    fun () ->
      match !list with
      | (x, b) :: l ->
          list := l;
          (pget conf base x, txt_of b)
      | [] -> raise Not_found
  in
  let mode () =
    Util.hidden_input conf "m" (Adef.encoded "C");
    Util.hidden_input conf "i" (get_iper p |> string_of_iper |> Adef.encoded);
    Util.hidden_input conf "t"
      (Adef.encoded (if dead_people then "AD" else "AN"))
  in
  match p_getint conf.env "v" with
  | Some i -> BirthdayDisplay.gen_print conf base i f_scan dead_people
  | _ ->
      if dead_people then
        BirthdayDisplay.gen_print_menu_dead conf base f_scan mode
      else BirthdayDisplay.gen_print_menu_birth conf base f_scan mode

let cousmenu_print = Perso.interp_templ "cousmenu"

let print conf base p =
  let max_lvl = max_cousin_level conf base p in
  (* v1 is the number of generation we go up to get a common ancestor,
     v2 is the number of generation we go down from the ancestor.
     e.g.
          (v1,v2) = (1,1) are their sisters/brothers
          (v1,v2) = (2,2) are their "cousins" *)
  match
    (p_getint conf.env "v1", p_getint conf.env "v2", p_getenv conf.env "t")
  with
  | Some 1, Some 1, _ | Some 0, _, _ | _, Some 0, _ ->
      Perso.interp_templ "cousins" conf base p
  | Some lvl1, _, _ ->
      let lvl1 = min (max 1 lvl1) max_lvl in
      let lvl2 =
        match p_getint conf.env "v2" with
        | Some lvl2 -> min (max 1 lvl2) max_lvl
        | None -> lvl1
      in
      print_cousins conf base p lvl1 lvl2
  | _, _, Some (("AN" | "AD") as t) when conf.wizard || conf.friend ->
      print_anniv conf base p (t = "AD") max_lvl
  | _ -> cousmenu_print conf base p
