(* Copyright (c) 1998-2007 INRIA *)

(* Descendant rendering for the [m=D] request.

   [print] dispatches on the [t] URL parameter: most modes are rendered by
   a template (see descendDisplay.mli), the others by functions of this
   module. Two families of functions coexist:

   1. List / table / index renderings ([display_descendants_*],
      [display_descendant_*], [display_spouse_index]).
   2. Tree renderings laid out on an HTML cell matrix consumed by DagDisplay:
      [print_tree] (centred tree) and [print_vaucher_tree] (compact tree,
      after Jean Vaucher, U. of Montreal — see issue #414).

   Vaucher layout vocabulary:
   - a cell is [(ncols, align, table_data)]: [ncols] logical columns spanned,
     horizontal [align], and the [Dag2html.table_data] payload;
   - a row is [(lastx, cells)] where [lastx] is the rightmost column already
     filled, used to pack cells left to right;
   - [tdal] is the list of rows being built. [p_pos]/[f_pos] place persons and
     spouses by a pre-order walk (one logical column per node, shifted right on
     collision); [complete_rows]/[strip_lastx]//[correct_spouses]/
     [drop_empty_rows] then normalise the matrix before conversion to [hts]. *)

open Config
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Collection = Geneweb_db.Collection

let limit_by_tree conf =
  match List.assoc_opt "max_desc_tree" conf.base_env with
  | None -> 4
  | Some x -> ( try max 1 (int_of_string x) with _ -> 4)

let text_to conf = function
  | 0 ->
      Util.transl_nth conf "generation/generations" 0
      |> Util.transl_decline conf "specify"
      |> Adef.safe
  | 1 -> Util.transl conf "to the children" |> Adef.safe
  | 2 -> Util.transl conf "to the grandchildren" |> Adef.safe
  | 3 -> Util.transl conf "to the great-grandchildren" |> Adef.safe
  | i ->
      Printf.sprintf
        (Util.ftransl conf "upto the %s generation")
        (Util.transl_nth conf "nth (generation)" i)
      |> Adef.safe

let descendants_title conf base p h =
  let s1 = Util.gen_person_text conf base p in
  let s2 = if h then Util.gen_person_text ~html:false conf base p else s1 in
  Util.translate_eval
    (Util.transl_a_of_gr_eq_gen_lev conf
       (Util.transl conf "descendants")
       (s1 :> string)
       (s2 :> string))
  |> Utf8.capitalize_fst |> Output.print_sstring conf

(* With number *)

let mark_descendants conf base marks max_lev ip =
  let rec loop lev ip u =
    if lev <= max_lev then (
      Collection.Marker.set marks ip true;
      Array.iter
        (fun ifam ->
          let el = Driver.get_children (Driver.foi base ifam) in
          Array.iter (fun e -> loop (succ lev) e (Util.pget conf base e)) el)
        (Driver.get_family u))
  in
  loop 0 ip (Util.pget conf base ip)

let label_descendants conf base marks paths max_lev =
  let rec loop path lev p =
    if lev < max_lev then
      ignore
      @@ Array.fold_left
           (fun cnt ifam ->
             let fam = Driver.foi base ifam in
             let c = Gutil.spouse (Driver.get_iper p) fam in
             let el = Driver.get_children fam in
             Array.fold_left
               (fun cnt e ->
                 if
                   Driver.get_sex p = Male
                   || not (Collection.Marker.get marks c)
                 then (
                   let path = Char.chr (Char.code 'A' + cnt) :: path in
                   Collection.Marker.set paths e path;
                   loop path (succ lev) (Util.pget conf base e));
                 succ cnt)
               cnt el)
           0 (Driver.get_family p)
  in
  loop [] 0

let close_lev = 2

let close_to_end conf base marks max_lev lev p =
  if lev + close_lev >= max_lev then true
  else
    let rec short dlev p =
      Array.for_all
        (fun ifam ->
          let fam = Driver.foi base ifam in
          let c = Gutil.spouse (Driver.get_iper p) fam in
          let el = Driver.get_children fam in
          if Driver.get_sex p = Male || not (Collection.Marker.get marks c) then
            if dlev = close_lev then Array.length el = 0
            else
              Array.for_all
                (fun e -> short (succ dlev) (Util.pget conf base e))
                el
          else true)
        (Driver.get_family p)
    in
    short 1 p

let labelled conf base marks max_lev lev ip =
  let a = Util.pget conf base ip in
  Array.length (Driver.get_family a) <> 0
  &&
  match Driver.get_parents a with
  | Some ifam ->
      let fam = Driver.foi base ifam in
      let el = Driver.get_children fam in
      Array.exists
        (fun ie ->
          let e = Util.pget conf base ie in
          Array.length (Driver.get_family e) <> 0
          && not (close_to_end conf base marks max_lev lev e))
        el
  | _ -> false

let label_of_path paths p =
  let rec loop = function
    | [] -> Adef.escaped ""
    | c :: cl -> loop cl ^^^ Util.escape_html (String.make 1 c)
  in
  Driver.get_iper p |> Collection.Marker.get paths |> loop

let child_drops_surname p1 p2 e =
  Driver.get_sex p1 = Male
  && Driver.Istr.equal (Driver.get_surname e) (Driver.get_surname p1)
  || Driver.get_sex p2 = Male
     && Driver.Istr.equal (Driver.get_surname e) (Driver.get_surname p2)

let print_child conf base p1 p2 e =
  Output.print_sstring conf "<strong>";
  if child_drops_surname p1 p2 e then
    Output.print_string conf
      (Util.referenced_person_text_without_surname conf base e)
  else (
    Output.print_sstring conf " ";
    Output.print_string conf (Util.referenced_person_text conf base e));
  Output.print_sstring conf "</strong>";
  Output.print_string conf (DateDisplay.short_dates_text conf base e)

let print_repeat_child conf base p1 p2 e =
  Output.print_sstring conf "<em>";
  if child_drops_surname p1 p2 e then
    Output.print_string conf (Util.gen_person_text ~sn:false conf base e)
  else Output.print_string conf (Util.gen_person_text conf base e);
  Output.print_sstring conf "</em>"

let display_spouse conf base marks paths fam p c =
  Output.print_sstring conf " &amp;";
  Output.print_string conf
    (DateDisplay.short_marriage_date_text conf base fam p c);
  Output.print_sstring conf " <strong> ";
  Output.print_string conf (Util.referenced_person_text conf base c);
  Output.print_sstring conf "</strong>";
  if Collection.Marker.get marks (Driver.get_iper c) then (
    Output.print_sstring conf " (<b class=\"font-monospace\">";
    Output.print_string conf (label_of_path paths c);
    Output.print_sstring conf "</b>)")
  else Output.print_string conf (DateDisplay.short_dates_text conf base c)

let print_family_locally conf base marks paths max_lev lev p1 c1 e total =
  let rec loop total lev p =
    if lev < max_lev then
      let _, _, _, total =
        Array.fold_left
          (fun (cnt, first, need_br, total) ifam ->
            let fam = Driver.foi base ifam in
            let c = Gutil.spouse (Driver.get_iper p) fam in
            let el = Driver.get_children fam in
            let c = Util.pget conf base c in
            if need_br then Output.print_sstring conf "<br>";
            if not first then print_repeat_child conf base p1 c1 p;
            display_spouse conf base marks paths fam p c;
            Output.print_sstring conf "\n";
            let print_children =
              Driver.get_sex p = Male
              || not (Collection.Marker.get marks (Driver.get_iper c))
            in
            if print_children then
              Output.printf conf "<ol start=\"%d\">\n" (succ cnt);
            let cnt, total =
              Array.fold_left
                (fun (cnt, total) ie ->
                  let e = Util.pget conf base ie in
                  let total =
                    if print_children then (
                      Output.print_sstring conf "<li type=\"A\"> ";
                      print_child conf base p c e;
                      Output.print_sstring conf "\n";
                      let total = total + 1 in
                      if succ lev = max_lev then (
                        Array.iteri
                          (fun i ifam ->
                            let fam = Driver.foi base ifam in
                            let c1 = Gutil.spouse ie fam in
                            let el = Driver.get_children fam in
                            let c1 = Util.pget conf base c1 in
                            if i <> 0 then (
                              Output.print_sstring conf "<br>";
                              print_repeat_child conf base p c e);
                            display_spouse conf base marks paths fam e c1;
                            if Array.length el <> 0 then
                              Output.print_sstring conf ".....";
                            Output.print_sstring conf "\n")
                          (Driver.get_family (Util.pget conf base ie));
                        total)
                      else loop total (succ lev) e)
                    else total
                  in
                  (succ cnt, total))
                (cnt, total) el
            in
            if print_children then Output.print_sstring conf "</ol>\n";
            (cnt, false, not print_children, total))
          (0, true, false, total) (Driver.get_family p)
      in
      total
    else total
  in
  loop total lev e

let print_family conf base marks paths max_lev lev p =
  if lev <> 0 then (
    Output.print_sstring conf "<b class=\"font-monospace\">";
    Output.print_string conf (label_of_path paths p);
    Output.print_sstring conf "</b>.<br>");
  snd
  @@ Array.fold_left
       (fun (cnt, total) ifam ->
         let fam = Driver.foi base ifam in
         let c = Gutil.spouse (Driver.get_iper p) fam in
         let el = Driver.get_children fam in
         let c = Util.pget conf base c in
         Output.print_sstring conf "<strong> ";
         Output.print_string conf (Util.referenced_person_text conf base p);
         Output.print_sstring conf "</strong>";
         display_spouse conf base marks paths fam p c;
         Output.print_sstring conf {|<ol start="|};
         Output.print_sstring conf (succ cnt |> string_of_int);
         Output.print_sstring conf {|">|};
         let cnt, total =
           Array.fold_left
             (fun (cnt, total) ie ->
               let e = Util.pget conf base ie in
               let total =
                 if
                   Driver.get_sex p = Male
                   || not (Collection.Marker.get marks (Driver.get_iper c))
                 then (
                   Output.print_sstring conf {|<li type="A">|};
                   print_child conf base p c e;
                   let total = total + 1 in
                   Output.print_sstring conf " ";
                   if labelled conf base marks max_lev lev ie then (
                     Output.print_sstring conf
                       " =&gt; <b class=\"font-monospace\">";
                     Output.print_string conf (label_of_path paths e);
                     Output.print_sstring conf "</b> ";
                     total)
                   else if succ lev = max_lev then (
                     Array.iter
                       (fun ifam ->
                         let fam = Driver.foi base ifam in
                         let c = Gutil.spouse ie fam in
                         let el = Driver.get_children fam in
                         let c = Util.pget conf base c in
                         display_spouse conf base marks paths fam e c;
                         if Array.length el <> 0 then
                           Output.print_sstring conf ".....";
                         Output.print_sstring conf "\n")
                       (Driver.get_family (Util.pget conf base ie));
                     total)
                   else
                     print_family_locally conf base marks paths max_lev
                       (succ lev) p c e total)
                 else total
               in
               (succ cnt, total))
             (cnt, total) el
         in
         Output.print_sstring conf "</ol>";
         (cnt, total))
       (0, 0) (Driver.get_family p)

let print_families conf base marks paths max_lev =
  let rec loop total lev p =
    if lev < max_lev then
      let total = total + print_family conf base marks paths max_lev lev p in
      Array.fold_left
        (fun total ifam ->
          let fam = Driver.foi base ifam in
          let c = Gutil.spouse (Driver.get_iper p) fam in
          let el = Driver.get_children fam in
          let c = Util.pget conf base c in
          if
            Driver.get_sex p = Male
            || not (Collection.Marker.get marks (Driver.get_iper c))
          then
            Array.fold_left
              (fun total ie ->
                let e = Util.pget conf base ie in
                if labelled conf base marks max_lev lev ie then
                  loop total (succ lev) e
                else total)
              total el
          else total)
        total (Driver.get_family p)
    else total
  in
  loop 0 0

let display_descendants_with_numbers conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    if h then descendants_title conf base ancestor h
    else
      Util.wprint_geneweb_link conf
        ("m=D&i="
         ^ Driver.Iper.to_string (Driver.get_iper ancestor)
         ^ "&v=" ^ string_of_int max_level ^ "&t=G"
        |> Adef.escaped)
        (let s1 = Util.gen_person_text conf base ancestor in
         let s2 = Util.gen_person_text ~html:true conf base ancestor in
         Util.transl_a_of_gr_eq_gen_lev conf
           (Util.transl conf "descendants")
           (s1 : Adef.safe_string :> string)
           (s2 : Adef.safe_string :> string)
         |> Utf8.capitalize_fst |> Adef.safe)
  in
  let marks =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
  in
  let paths = Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) [] in
  Hutil.header conf title;
  Output.print_string conf (DateDisplay.short_dates_text conf base ancestor);
  let p = ancestor in
  (if Util.authorized_age conf base p then
     match (Date.od_of_cdate (Driver.get_birth p), Driver.get_death p) with
     | Some _, _ | _, Death (_, _) -> Output.print_sstring conf "<br>"
     | _ -> ());
  (text_to conf max_level : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf ".<p>";
  mark_descendants conf base marks max_level (Driver.get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let total = print_families conf base marks paths max_level ancestor in
  if total > 1 then (
    Output.print_sstring conf "<p>";
    Output.printf conf "%s%s %d %s"
      (Utf8.capitalize_fst (Util.transl conf "total"))
      (Util.transl conf ":") total
      (Util.translate_eval ("@(c)" ^ Util.transl_nth conf "person/persons" 1));
    if max_level > 1 then
      Output.printf conf " (%s)" (Util.transl conf "spouses not included");
    Output.print_sstring conf ".\n");
  Hutil.trailer conf

let print_ref conf base paths p =
  if Collection.Marker.get paths (Driver.get_iper p) <> [] then (
    Output.print_sstring conf " =&gt; <b class=\"font-monospace\">";
    Output.print_string conf (label_of_path paths p);
    Output.print_sstring conf "</b>")
  else
    Array.iter
      (fun ifam ->
        let c = Gutil.spouse (Driver.get_iper p) (Driver.foi base ifam) in
        if Collection.Marker.get paths c <> [] then (
          let c = Util.pget conf base c in
          Output.print_sstring conf " =&gt; ";
          Output.print_string conf
            (Driver.p_first_name base c |> Util.escape_html);
          Output.print_sstring conf " ";
          Output.print_string conf (Driver.p_surname base c |> Util.escape_html);
          Output.print_sstring conf {| <b class="font-monospace">|};
          Output.print_string conf (label_of_path paths c);
          Output.print_sstring conf "</b>"))
      (Driver.get_family p)

let print_elem conf base paths precision (n, pll) =
  Output.print_sstring conf "<li>";
  match List.rev pll with
  | [ [ p ] ] ->
      Output.print_sstring conf "<strong>";
      Output.print_string conf
        (Util.surname_without_particle base n |> Util.escape_html);
      Output.print_sstring conf " ";
      Util.gen_person_text ~sn:false conf base p
      |> Util.reference conf base p |> Output.print_string conf;
      Output.print_sstring conf " ";
      Output.print_string conf (Util.surname_particle base n |> Util.escape_html);
      Output.print_sstring conf "</strong>";
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      print_ref conf base paths p;
      Output.print_sstring conf "\n"
  | pll ->
      Output.print_sstring conf "<strong>";
      Output.print_string conf
        (Util.surname_without_particle base n |> Util.escape_html);
      Output.print_string conf (Util.surname_particle base n |> Util.escape_html);
      Output.print_sstring conf "</strong><ul>";
      List.iter
        (fun pl ->
          let several = match pl with [ _ ] -> false | _ -> true in
          List.iter
            (fun p ->
              Output.print_sstring conf "<li><strong>";
              Util.wprint_geneweb_link conf (Util.acces conf base p)
                (Driver.p_first_name base p |> Util.escape_html
                  :> Adef.safe_string);
              Output.print_sstring conf "</strong>";
              if several && precision then (
                Output.print_sstring conf "<em>";
                Util.specify_homonymous conf base p true;
                Output.print_sstring conf "</em>");
              Output.print_string conf
                (DateDisplay.short_dates_text conf base p);
              print_ref conf base paths p)
            pl)
        pll;
      Output.print_sstring conf "</ul>"

let sort_and_display conf base paths precision list =
  let list = List.map (Util.pget conf base) list in
  let list =
    List.sort
      (fun p1 p2 ->
        let c =
          Gutil.alphabetic (Driver.p_surname base p2) (Driver.p_surname base p1)
        in
        if c = 0 then
          Gutil.alphabetic
            (Driver.p_first_name base p2)
            (Driver.p_first_name base p1)
        else c)
      list
  in
  let list =
    List.fold_left
      (fun npll p ->
        match npll with
        | (n, pl) :: npll when n = Driver.p_surname base p ->
            (n, p :: pl) :: npll
        | _ -> (Driver.p_surname base p, [ p ]) :: npll)
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
                when Driver.Istr.equal (Driver.get_first_name p1)
                       (Driver.get_first_name p) ->
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
      Util.transl conf "index of the descendants"
      |> Utf8.capitalize_fst |> Adef.safe
    in
    if not h then
      Util.wprint_geneweb_link conf
        ("m=D&i="
         ^ Driver.Iper.to_string (Driver.get_iper ancestor)
         ^ "&v=" ^ string_of_int max_level ^ "&t=C"
        |> Adef.escaped)
        txt
    else Output.print_string conf txt
  in
  Hutil.header conf title;
  let marks =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
  in
  let paths = Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) [] in
  mark_descendants conf base marks max_level (Driver.get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Collection.fold
      (fun acc i ->
        let p = Util.pget conf base i in
        if Collection.Marker.get paths i <> [] then
          if
            Driver.p_first_name base p <> "?"
            && Driver.p_surname base p <> "?"
            && Driver.p_first_name base p <> "x"
            && ((not (Util.is_hide_names conf p))
               || Util.authorized_age conf base p)
          then Driver.get_iper p :: acc
          else acc
        else acc)
      [] (Driver.ipers base)
  in
  sort_and_display conf base paths true list;
  Hutil.trailer conf

let display_spouse_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title _ =
    Util.transl conf "index of the spouses (non descendants)"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  let marks =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
  in
  let paths = Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) [] in
  mark_descendants conf base marks max_level (Driver.get_iper ancestor);
  label_descendants conf base marks paths max_level ancestor;
  let list =
    Collection.fold
      (fun acc i ->
        let p = Util.pget conf base i in
        if Collection.Marker.get paths i <> [] then
          if
            Driver.p_first_name base p <> "?"
            && Driver.p_surname base p <> "?"
            && Driver.p_first_name base p <> "x"
          then
            Array.fold_left
              (fun acc ifam ->
                let c =
                  Gutil.spouse (Driver.get_iper p) (Driver.foi base ifam)
                in
                if Collection.Marker.get paths c = [] then
                  let c = Util.pget conf base c in
                  if
                    Driver.p_first_name base c <> "?"
                    && Driver.p_surname base c <> "?"
                    && Driver.p_first_name base p <> "x"
                    && ((not (Util.is_hide_names conf c))
                       || Util.authorized_age conf base c)
                    && not (List.mem (Driver.get_iper c) acc)
                  then Driver.get_iper c :: acc
                  else acc
                else acc)
              acc (Driver.get_family p)
          else acc
        else acc)
      [] (Driver.ipers base)
  in
  sort_and_display conf base paths false list;
  Hutil.trailer conf

let nowrap s = {|<span class="text-nowrap">|} ^<^ s ^>^ "</span>"
let dag_date s = {|<span class="text-nowrap dag-date">|} ^<^ s ^>^ "</span>"

let person_lines ?(pre = Adef.safe "") ~link conf base p auth =
  let name =
    match Util.main_title conf base p with
    | Some _ -> link (Util.person_title_text conf base p)
    | None ->
        let fn = Util.gen_person_text ~sn:false conf base p in
        let sn =
          (Driver.p_surname base p |> Util.escape_html :> Adef.safe_string)
        in
        link ((nowrap fn ^>^ "<br>") ^^^ nowrap sn)
  in
  let name =
    if (pre :> string) = "" then name else (dag_date pre ^>^ "<br>") ^^^ name
  in
  if auth then
    (name ^>^ "<br>") ^^^ dag_date (DateDisplay.short_dates_text conf base p)
  else name

let make_tree_hts conf base gv p =
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let all_children p =
    Array.concat
      (List.map
         (fun ifam -> Driver.get_children (Driver.foi base ifam))
         (Array.to_list (Driver.get_family p)))
  in
  let rec nb_column n v u =
    if v = 0 then
      n + if sps then max 1 (Array.length (Driver.get_family u)) else 1
    else if Array.length (Driver.get_family u) = 0 then n + 1
    else if not sps then
      let ch = all_children u in
      if Array.length ch = 0 then n + 1
      else
        Array.fold_left
          (fun n ip -> nb_column n (v - 1) (Util.pget conf base ip))
          n ch
    else
      Array.fold_left
        (fun n ifam -> fam_nb_column n v (Driver.foi base ifam))
        n (Driver.get_family u)
  and fam_nb_column n v des =
    if Array.length (Driver.get_children des) = 0 then n + 1
    else
      Array.fold_left
        (fun n iper -> nb_column n (v - 1) (Util.pget conf base iper))
        n (Driver.get_children des)
  in
  let bracket_over v children tdl =
    let len = Array.length children in
    if len = 0 then (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    else if len = 1 then
      let u = Util.pget conf base children.(0) in
      let ncol = nb_column 0 (v - 1) u in
      ((2 * ncol) - 1, Dag2html.CenterA, Dag2html.TDnothing) :: tdl
    else
      let rec loop tdl i =
        if i = len then tdl
        else
          let u = Util.pget conf base children.(i) in
          let tdl =
            if i > 0 then
              (1, Dag2html.CenterA, Dag2html.TDhr Dag2html.CenterA) :: tdl
            else tdl
          in
          let ncol = nb_column 0 (v - 1) u in
          let align =
            if i = 0 then Dag2html.RightA
            else if i = len - 1 then Dag2html.LeftA
            else Dag2html.CenterA
          in
          loop (((2 * ncol) - 1, align, Dag2html.TDhr align) :: tdl) (i + 1)
      in
      loop tdl 0
  in
  let vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    in
    let td =
      match po with
      | Some (p, _) ->
          (* Récupère les options d'affichage. *)
          let options = Util.display_options conf in
          let ncol = nb_column 0 (v - 1) p in
          let vbar_txt =
            Util.commd conf ^^^ "m=D&t=T&v=" ^<^ string_of_int gv ^<^ "&"
            ^<^ options ^^^ "&" ^<^ Util.acces conf base p
          in
          ((2 * ncol) - 1, Dag2html.CenterA, Dag2html.TDbar (Some vbar_txt))
      | None -> (1, Dag2html.LeftA, Dag2html.TDnothing)
    in
    td :: tdl
  in
  let children_vertical_bars v gen =
    let tdl = List.fold_left (vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let spouses_vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    in
    match po with
    | Some (p, _) when Array.length (Driver.get_family p) > 0 ->
        if not sps then
          if Array.length (all_children p) = 0 then
            (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
          else
            let ncol = nb_column 0 (v - 1) p in
            ((2 * ncol) - 1, Dag2html.CenterA, Dag2html.TDbar None) :: tdl
        else
          fst
          @@ Array.fold_left
               (fun (tdl, first) ifam ->
                 let tdl =
                   if first then tdl
                   else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
                 in
                 let des = Driver.foi base ifam in
                 let td =
                   if Array.length (Driver.get_children des) = 0 then
                     (1, Dag2html.LeftA, Dag2html.TDnothing)
                   else
                     let ncol = fam_nb_column 0 (v - 1) des in
                     ((2 * ncol) - 1, Dag2html.CenterA, Dag2html.TDbar None)
                 in
                 (td :: tdl, false))
               (tdl, true) (Driver.get_family p)
    | _ -> (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
  in
  let spouses_vertical_bar v gen =
    let tdl = List.fold_left (spouses_vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let horizontal_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    in
    match po with
    | Some (p, _) when Array.length (Driver.get_family p) > 0 ->
        if not sps then bracket_over v (all_children p) tdl
        else
          fst
          @@ Array.fold_left
               (fun (tdl, first) ifam ->
                 let tdl =
                   if first then tdl
                   else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
                 in
                 ( bracket_over v
                     (Driver.get_children (Driver.foi base ifam))
                     tdl,
                   false ))
               (tdl, true) (Driver.get_family p)
    | _ -> (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
  in
  let horizontal_bars v gen =
    let tdl = List.fold_left (horizontal_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let person_txt v tdl po =
    let tdl =
      if tdl = [] then [] else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    in
    let td =
      match po with
      | Some (p, auth) ->
          let ncol =
            if v > 1 then nb_column 0 (v - 1) p
            else Array.length (Driver.get_family p)
          in
          let txt =
            person_lines ~link:(Util.reference conf base p) conf base p auth
          in
          let txt =
            if not img then txt
            else if sps then DagDisplay.image_txt conf base p ^^^ txt
            else txt ^^^ DagDisplay.image_txt conf base p
          in
          ( (2 * ncol) - 1,
            Dag2html.CenterA,
            Dag2html.TDitem (Driver.get_iper p, txt, Adef.safe "") )
      | None -> (1, Dag2html.LeftA, Dag2html.TDnothing)
    in
    td :: tdl
  in
  let spouses_txt v tdl po =
    let tdl =
      if tdl = [] then [] else (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
    in
    match po with
    | Some (p, auth) when Array.length (Driver.get_family p) > 0 ->
        let rec loop tdl i =
          if i = Array.length (Driver.get_family p) then tdl
          else
            let ifam = (Driver.get_family p).(i) in
            let tdl =
              if i > 0 then
                ( 1,
                  Dag2html.LeftA,
                  Dag2html.TDtext (Driver.Iper.dummy, Adef.safe "...") )
                :: tdl
              else tdl
            in
            let td =
              let fam = Driver.foi base ifam in
              let ncol = if v > 1 then fam_nb_column 0 (v - 1) fam else 1 in
              let sp =
                Util.pget conf base (Gutil.spouse (Driver.get_iper p) fam)
              in
              let s =
                let md =
                  if auth then
                    DateDisplay.short_marriage_date_text conf base fam p sp
                  else Adef.safe ""
                in
                let pre = {|<i>|} ^<^ "&amp;" ^<^ md ^>^ "</i>" in
                person_lines ~pre
                  ~link:(Util.reference conf base sp)
                  conf base sp auth
                ^^^ DagDisplay.image_txt conf base sp
              in
              ( (2 * ncol) - 1,
                Dag2html.CenterA,
                Dag2html.TDitem (Driver.get_iper sp, s, Adef.safe "spouse_x") )
            in
            loop (td :: tdl) (i + 1)
        in
        loop tdl 0
    | _ -> (1, Dag2html.LeftA, Dag2html.TDnothing) :: tdl
  in
  let next_gen gen =
    List.fold_right
      (fun po gen ->
        match po with
        | Some (p, _) ->
            if not sps then
              let ch = all_children p in
              if Array.length ch = 0 then None :: gen
              else
                let age_auth =
                  Array.for_all
                    (fun ip ->
                      Util.authorized_age conf base (Util.pget conf base ip))
                    ch
                in
                Array.fold_right
                  (fun iper gen ->
                    Some (Util.pget conf base iper, age_auth) :: gen)
                  ch gen
            else if Array.length (Driver.get_family p) = 0 then None :: gen
            else
              Array.fold_right
                (fun ifam gen ->
                  let des = Driver.foi base ifam in
                  if Array.length (Driver.get_children des) = 0 then None :: gen
                  else
                    let age_auth =
                      Array.for_all
                        (fun ip ->
                          Util.authorized_age conf base (Util.pget conf base ip))
                        (Driver.get_children des)
                    in
                    Array.fold_right
                      (fun iper gen ->
                        Some (Util.pget conf base iper, age_auth) :: gen)
                      (Driver.get_children des) gen)
                (Driver.get_family p) gen
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
    let s = Util.gen_person_text ~html:false conf base p in
    Util.translate_eval
      (Util.transl_a_of_gr_eq_gen_lev conf
         (Util.transl conf "descendants")
         (s : Adef.safe_string :> string)
         (s : Adef.safe_string :> string))
    |> Adef.safe
  in
  let hts = make_tree_hts conf base gv p in
  DagDisplay.print_dag_page conf base page_title hts (Adef.escaped "")

(* === Compact descendant tree (Jean Vaucher layout) ===

   [print_vaucher_tree] lays descendants out with the compact
   tree-positioning algorithm of Jean Vaucher, "Pretty-Printing of
   Trees", Software Practice & Experience 10 (1980), 553-561.
   Reference implementation: GTree.java (Vaucher, 2016), function
   [position]. See issue #414.

   Principle ([position]): nodes are placed by a depth-first walk and
   each node collides only with its already-placed neighbour on the
   SAME level, so subtrees from different parents interleave
   column-wise instead of each reserving its full width; the parent is
   then recentred on the midpoint of its first and last child.

   Mapping to GeneWeb and divergences from the reference:
   - Mirror. The 1980 paper is post-order, right-son-first, binary,
     neighbour on the right. This code (like the 2016 [position])
     mirrors it to a left-to-right n-ary pre-order walk with the
     neighbour on the left: [x = max x0 (last_x.(row) + 2)], parent
     [= (x1 + xn) / 2].
   - Family layer. Vaucher positions individuals; GeneWeb positions a
     person then each union (spouse + children) over a 4-row band per
     generation (person / spouse-bracket / spouse / child-bracket).
     Seeding uses the union count in [p_pos] and the child count in
     [f_pos]. This yields 12 columns instead of 10 on the #414 tree:
     an accepted consequence of the family-node model, not a
     positioning error.
   - Emission. Vaucher prints two passes per level directly; here
     nodes are placed into a sparse matrix [tdal] ([init_tdal]) and
     normalised ([complete_rows], [clean_rows], [correct_spouses], 
     [drop_empty_rows]) before conversion to [hts].

   [last_x.(row)] is the rightmost filled column of each row, held in
   the per-call array threaded through [p_pos]/[f_pos], not a module
   global. *)

let td_fill x1 xn = [ (xn - x1, Dag2html.CenterA, Dag2html.TDnothing) ]

let td_hbar x1 xn =
  match xn - x1 with
  | 0 -> [ (1, Dag2html.CenterA, Dag2html.TDnothing) ]
  | 1 -> [ (1, Dag2html.CenterA, Dag2html.TDhr Dag2html.CenterA) ]
  | _ ->
      [
        (1, Dag2html.LeftA, Dag2html.TDhr Dag2html.LeftA);
        (xn - x1 - 1, Dag2html.CenterA, Dag2html.TDhr Dag2html.CenterA);
        (1, Dag2html.RightA, Dag2html.TDhr Dag2html.RightA);
      ]

let td_cell cols align ip text flags =
  [ (cols, align, Dag2html.TDitem (ip, text, flags)) ]

let tdal_add tdal ir elem nx =
  let _, row = tdal.(ir) in
  tdal.(ir) <- (nx, List.rev_append elem row);
  tdal

let anchored_reference conf base p s =
  if Util.is_hidden p then s
  else
    Printf.sprintf
      {|<a href="%s%s" id="i%s" class="normal_anchor" title="%s">%s</a>|}
      (Util.commd conf :> string)
      (Util.acces conf base p :> string)
      (Driver.Iper.to_string (Driver.get_iper p))
      (Utf8.capitalize_fst (Util.transl conf "open individual page"))
      s

let get_text conf base p img cgl =
  let auth = Util.authorized_age conf base p in
  let link (s : Adef.safe_string) =
    if cgl then s else Adef.safe (anchored_reference conf base p (s :> string))
  in
  let txt = person_lines ~link conf base p auth in
  let has_image = Image.get_portrait conf base p |> Option.is_some in
  if has_image && img then txt ^^^ DagDisplay.image_txt conf base p else txt

let lastx tdal ir = fst tdal.(ir)

let get_spouse base iper ifam =
  let f = Driver.foi base ifam in
  if iper = Driver.get_father f then Driver.poi base (Driver.get_mother f)
  else Driver.poi base (Driver.get_father f)

(* A generation spans three rows: [ir] the person, [ir + 1] the spouse
   (the vertical connector is carried inline in the text) and [ir + 2]
   the bracket over the children; each child recurses at [ir + 3].
   [f_pos] is called by [p_pos] at [ir + 1]. *)
let rec p_pos conf base p x0 v ir tdal only_anc sps img marr cgl =
  let lx = lastx tdal ir in
  let x = if lx + 2 > x0 then lx + 2 else x0 in
  let ifaml = List.rev (Array.to_list (Driver.get_family p)) in
  let ifam_nbr = List.length ifaml in
  let descendants = ifaml <> [] in
  let ifaml =
    if sps then ifaml
    else if v > 0 then
      List.filter
        (fun ifam ->
          Array.length (Driver.get_children (Driver.foi base ifam)) > 0)
        ifaml
    else []
  in
  let tdal, x, _x1, _xn =
    if v >= 0 && ifaml <> [] then
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
  in
  let vv =
    match Util.p_getenv conf.env "v" with Some v -> "&v=" ^ v | None -> "&v=4"
  in
  let pp = Util.find_person_in_env conf base "" in
  let pp_index =
    match pp with
    | Some p -> "&i=" ^ Driver.Iper.to_string (Driver.get_iper p)
    | None -> ""
  in
  let pz = Util.find_person_in_env conf base "z" in
  let pz_index =
    match pz with
    | Some p -> "&iz=" ^ Driver.Iper.to_string (Driver.get_iper p)
    | None -> ""
  in
  let txt = get_text conf base p img cgl in
  let only =
    if cgl then "│"
    else
      Printf.sprintf "<a href=\"%sm=D&t=TV%s%s%s%s%s%s\" %s title=\"%s\">│</a>"
        (Util.commd conf :> string)
        vv pz_index pp_index
        ("&oi=" ^ Driver.Iper.to_string (Driver.get_iper p))
        (if sps then "" else "&sp=0")
        (if img then "" else "&im=0")
        "class=\"normal_anchor px-3 btn-outline-primary border-0\""
        (Utf8.capitalize_fst (Util.transl conf "partial"))
  in
  let txt = if ir > 0 then Adef.safe (only ^ "<br>") ^^^ txt else txt in
  let continue = only_anc = [] || ifaml <> [] in
  let br = if img then "" else "<br>" in
  let txt =
    if (not continue) && descendants then txt ^^^ Adef.safe (br ^ "+") else txt
  in
  let lx = if lx > -1 then lx else -1 in
  let tdal =
    tdal_add tdal ir
      (List.rev_append
         (td_fill lx (x - 1))
         (td_cell 1 Dag2html.CenterA (Driver.get_iper p) txt (Adef.safe "")))
      x
  in
  (tdal, x)

and f_pos conf base ifam ifam_nbr only_one first last p x0 v ir2 tdal only_anc
    sps img marr cgl =
  let sp = get_spouse base (Driver.get_iper p) ifam in
  let continue = only_anc = [] || List.mem ifam only_anc in
  let lx = lastx tdal ir2 + 2 in
  let x = if lx > x0 then lx else x0 in
  let kids =
    Array.fold_left
      (fun l k -> Driver.poi base k :: l)
      []
      (Driver.get_children (Driver.foi base ifam))
  in
  let tdal, x, x1, xn =
    if kids <> [] && continue && v > 0 then
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
  let txt = get_text conf base sp img cgl in
  let has_image = Image.get_portrait conf base p |> Option.is_some in
  let br_sp = if has_image && img then "" else "<br>" in
  let auth =
    Util.authorized_age conf base p && Util.authorized_age conf base sp
  in
  let fam = Driver.foi base ifam in
  let marr_d =
    if marr && auth then DateDisplay.short_family_dates_text conf base true fam
    else Adef.safe " "
  in
  let m_txt =
    (* families are scanned in reverse order *)
    let f_nbr = string_of_int ifam_nbr in
    Adef.safe "<span class=\"text-nowrap\">"
    ^^^ (if last || only_one then Adef.safe "" else Adef.safe "…")
    ^^^ (if only_one then Adef.safe " &amp;"
         else Adef.safe (" &amp;<sup>" ^ f_nbr ^ "</sup>"))
    ^^^ marr_d
    ^^^ (if first || only_one then Adef.safe "" else Adef.safe "…")
    ^^^ Adef.safe "</span>"
    ^^^ if only_one && not marr then Adef.safe "" else Adef.safe "<br>"
  in
  let txt =
    if sps then
      let txt = if kids <> [] then txt ^^^ Adef.safe br_sp else txt in
      let txt = m_txt ^^^ txt in
      if v > 0 && kids <> [] then txt ^^^ Adef.safe "│" else txt
    else if v > 0 && kids <> [] then Adef.safe "│"
    else Adef.safe ""
  in
  let flag =
    Driver.Ifam.to_string ifam
    ^ if v > 0 && kids <> [] then "-spouse_no_d" else "-spouse"
  in
  let lx = lastx tdal ir2 in
  let lx = if lx > -1 then lx else -1 in
  let tdal =
    tdal_add tdal ir2
      (List.rev_append
         (td_fill lx (x - 1))
         (td_cell 1 Dag2html.CenterA (Driver.get_iper sp) txt (Adef.safe flag)))
      x
  in
  if v > 0 && kids <> [] then
    let lx = lastx tdal (ir2 + 1) in
    let lx = if lx > -1 then lx else -1 in
    let tdal =
      tdal_add tdal (ir2 + 1)
        (List.rev_append (td_fill lx (x1 - 1)) (td_hbar x1 xn))
        xn
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
            loop tdal
              ((max_col, List.rev_append (td_fill x max_col) row) :: new_td)
          else loop tdal ((x, row) :: new_td)
    in
    loop tdal []
  in
  List.rev tdal

(* Three rows per generation: person, spouse, child bracket. Vaucher's
   bracket over multiple unions is not drawn; GeneWeb marks them with
   &1/&2 superscripts instead (see #414). *)
let init_tdal gv = Array.make (3 * (gv + 1)) (0, [])

(* Put the two cells of one union back side by side: f_pos tags them
   "<ifam>-spouse" and "<ifam>-spouse_no_d", and expand_cell may have
   pushed a filler between them. *)
let correct_spouses tdal =
  let rec regroup row new_row =
    match row with
    | (nc1, a1, t1) :: (nc2, a2, t2) :: (nc3, a3, t3) :: row -> (
        match (t1, t2, t3) with
        | ( Dag2html.TDitem (_, _, f1),
            Dag2html.TDnothing,
            Dag2html.TDitem (_, _, f3) ) -> (
            match
              ( String.split_on_char '-' (f1 :> string),
                String.split_on_char '-' (f3 :> string) )
            with
            | [ fam1; fl1 ], [ fam3; fl3 ] ->
                if fam1 = fam3 && fl1 = "spouse_no_d" && fl3 = "spouse" then
                  regroup
                    ((nc3, a3, t3) :: (nc2, a2, t2) :: row)
                    ((nc1, a1, t1) :: new_row)
                else if fam1 = fam3 && fl1 = "spouse" && fl3 = "spouse_no_d"
                then
                  regroup
                    ((nc1, a1, t1) :: (nc3, a3, t3) :: row)
                    ((nc2, a2, t2) :: new_row)
                else
                  regroup
                    ((nc2, a2, t2) :: (nc3, a3, t3) :: row)
                    ((nc1, a1, t1) :: new_row)
            | _ ->
                regroup
                  ((nc2, a2, t2) :: (nc3, a3, t3) :: row)
                  ((nc1, a1, t1) :: new_row))
        | _ ->
            regroup
              ((nc2, a2, t2) :: (nc3, a3, t3) :: row)
              ((nc1, a1, t1) :: new_row))
    | _ -> List.rev (List.rev_append row new_row)
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

let strip_lastx tdal = List.map snd tdal

let drop_empty_rows tdal =
  let row_is_empty = List.for_all (fun (_, _, td) -> td = Dag2html.TDnothing) in
  List.filter (fun row -> not (row_is_empty row)) tdal

(* Families on the path from [ip] up to ancestor [iap]: used by the
   o=/oi= option to prune the tree to that single branch. *)
let rec find_ancestors base iap ip list v =
  match Driver.get_parents (Driver.poi base ip) with
  | Some ifam ->
      let cpl = Driver.foi base ifam in
      let ifath = Driver.get_father cpl in
      let imoth = Driver.get_mother cpl in
      let list =
        if v > 1 && not (iap = ifath) then
          find_ancestors base iap ifath list (v - 1)
        else list
      in
      let list =
        if v > 1 && not (iap = imoth) then
          find_ancestors base iap imoth list (v - 1)
        else list
      in
      ifam :: list
  | None -> list

let make_vaucher_tree_hts conf base gv p =
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let marr = Util.get_opt conf "ma" true in
  let cgl =
    match Util.p_getenv conf.env "cgl" with Some "on" -> true | _ -> false
  in
  let only_anc, op =
    match Util.find_person_in_env_pref conf base "o" with
    | Some p -> (true, p)
    | None -> (false, Driver.empty_person base Driver.Iper.dummy)
  in
  let only_anc =
    if only_anc then
      find_ancestors base (Driver.get_iper p) (Driver.get_iper op) [] gv
    else []
  in
  let tdal = init_tdal gv in
  let tdal, _ = p_pos conf base p 0 gv 0 tdal only_anc sps img marr cgl in
  let tdal = Array.to_list tdal in
  let tdal = complete_rows tdal in
  let tdal = strip_lastx tdal in
  let tdal = correct_spouses tdal in
  let tdal = drop_empty_rows tdal in
  let hts0 = List.fold_left (fun acc row -> Array.of_list row :: acc) [] tdal in
  let hts = Array.of_list (List.rev hts0) in
  hts

let print_vaucher_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    Util.translate_eval
      (let s = (Util.gen_person_text conf base p ~html:false :> string) in
       Util.transl_a_of_gr_eq_gen_lev conf (Util.transl conf "descendants") s s)
    |> Adef.safe
  in
  let hts = make_vaucher_tree_hts conf base gv p in
  DagDisplay.print_dag_page conf base page_title hts (Adef.escaped "")
(* ******** end of J. Vaucher tree ********* *)

let print conf base p =
  let templ =
    match Util.p_getenv conf.env "t" with
    | Some ("F" | "L" | "M") -> "deslist"
    | Some "D" -> "deslist_hr"
    | Some ("H" | "I" | "A") -> "destable"
    | Some "V" -> "destree"
    | Some _ -> ""
    | _ -> "desmenu"
  in
  if templ <> "" then Perso.interp_templ templ conf base p
  else
    match (Util.p_getenv conf.env "t", Util.p_getint conf.env "v") with
    | Some "N", Some v -> display_descendants_with_numbers conf base v p
    | Some "G", Some v -> display_descendant_index conf base v p
    | Some "C", Some v -> display_spouse_index conf base v p
    | Some "T", Some v -> print_tree conf base v p
    | Some "TV", Some v -> print_vaucher_tree conf base v p
    | _ -> Perso.interp_templ "desmenu" conf base p
