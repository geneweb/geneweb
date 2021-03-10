open Config
open Def
open Gwdb
open Util
open Title

let my_alphabetic n1 n2 =
  (*
    compare (Name.abbrev (Name.lower n1)) (Name.abbrev (Name.lower n2))
  *)
  compare (Name.lower n1) (Name.lower n2)

let string_cnt_list_uniq l =
  let l =
    List.fold_left
      (fun l (e, c) ->
         match l with
           [] -> [e, c]
         | (x, d) :: l1 ->
             if my_alphabetic e x = 0 then (x, c + d) :: l1 else (e, c) :: l)
      [] l
  in
  List.rev l

let compare_titles2 (t1, _) (t2, _) = my_alphabetic t1 t2

let give_access_someone conf base (x, t) list =
  let t_date_start = Adef.od_of_cdate t.t_date_start in
  let t_date_end = Adef.od_of_cdate t.t_date_end in
  let has_dates =
    match t_date_start, t_date_end with
      Some (Dgreg (_, _)), _ | _, Some (Dgreg (_, _)) -> true
    | _ -> false
  in
  if has_dates then Output.print_string conf "<em>";
  begin match t_date_start with
    Some (Dgreg (d, _)) -> Output.print_string conf (string_of_int d.year)
  | _ -> ()
  end;
  begin match t_date_end with
    Some (Dgreg (d, _)) ->
    Output.print_string conf "-" ;
    Output.print_string conf (string_of_int d.year) ;
  | _ -> ()
  end;
  if has_dates then Output.print_string conf "</em>: ";
  if List.mem x list then Output.print_string conf "<em>"
  else Output.printf conf "<a href=\"%s%s\">" (commd conf) (acces conf base x);
  begin match t.t_name, get_public_name x, get_qualifiers x with
    Tmain, pn, nn :: _ when sou base pn <> "" ->
      Output.printf conf "%s <em>%s</em> %s" (sou base pn) (sou base nn)
        (p_surname base x)
  | Tmain, pn, [] when sou base pn <> "" ->
      Output.printf conf "%s %s" (sou base pn) (p_surname base x)
  | Tname n, _, nn :: _ ->
      Output.printf conf "%s <em>%s</em> %s" (sou base n) (sou base nn)
        (p_surname base x)
  | Tname n, _, [] -> Output.printf conf "%s %s" (sou base n) (p_surname base x)
  | _ -> Output.print_string conf (person_text conf base x)
  end;
  Output.print_string conf "\n";
  Output.print_string conf (DateDisplay.short_dates_text conf base x);
  if t.t_nth <> 0 then
    Output.printf conf " (%s)"
      (if t.t_nth >= 100 then string_of_int t.t_nth
       else transl_nth conf "nth" t.t_nth);
  if List.mem x list then Output.print_string conf "</em>" else Output.print_string conf "</a>"

let give_access_title conf t p =
  Output.printf conf "<a href=\"%sm=TT&sm=S&t=%s&p=%s\">" (commd conf)
    (Mutil.encode t) (Mutil.encode p);
  Output.print_string conf (Utf8.capitalize_fst t);
  Output.print_string conf "</a>\n"

let give_access_all_titles conf t absolute =
  Output.printf conf "<a href=\"%sm=TT&sm=S&t=%s%s\">" (commd conf)
    (Mutil.encode t) (if absolute then "&a=A" else "");
  Output.print_string conf (if absolute then t else Utf8.capitalize_fst t);
  Output.print_string conf "</a>"

let give_access_all_places conf t =
  Output.printf conf "<a href=\"%sm=TT&sm=S&p=%s\">" (commd conf) (Mutil.encode t);
  Output.printf conf "... %s" t;
  Output.print_string conf "</a>\n"

let propose_tree_for_list list conf =
  let (list, _) =
    List.fold_left
      (fun (list, n) (p, _) ->
         let list = if List.mem_assq p list then list else (p, n) :: list in
         list, n + 1)
      ([], 1) list
  in
  begin match List.rev list with
    _ :: _ :: _ as list ->
      Output.print_string conf "<p>\n";
      Output.printf conf "<a href=\"%sm=RLM" (commd conf);
      begin let _ =
        List.fold_left
          (fun i (p, n) ->
             Output.printf conf "&i%d=%s&t%d=%d" i
               (Gwdb.string_of_iper (get_iper p)) i n;
             i + 1)
          1 list
      in
        Output.printf conf "&lim=6\">%s</a>\n" (Utf8.capitalize_fst (transl conf "tree"))
      end;
      Output.print_string conf "</p>\n"
  | _ -> ()
  end

let print_title_place_list conf base t p t_equiv list =
  let absolute = p_getenv conf.env "a" = Some "A" in
  let title h =
    if h || absolute then
      begin Output.print_string conf t; if p <> "" then Output.printf conf " %s" p end
    else
      Mutil.list_iter_first
        (fun first t ->
           if not first then Output.print_string conf ",\n";
           Output.printf conf "<a href=\"%sm=TT&sm=S&t=%s&a=A\">" (commd conf)
             (Mutil.encode t);
           Output.printf conf "%s</a>" t;
           if p <> "" then
             begin
               Output.printf conf "\n<a href=\"%sm=TT&sm=S&p=%s&a=A\">"
                 (commd conf) (Mutil.encode p);
               Output.printf conf "%s</a>" p
             end)
        t_equiv
  in
  Hutil.header conf title;
  Output.print_string conf "<ul>\n";
  begin let _ =
    List.fold_left
      (fun list x ->
         Output.print_string conf "<li>";
         give_access_someone conf base x list;
         Output.print_string conf "</li>\n";
         fst x :: list)
      [] list
  in
    ()
  end;
  Output.print_string conf "</ul>\n";
  propose_tree_for_list list conf;
  Hutil.trailer conf

let print_all_with_place_list conf base p list =
  let title _ = Output.printf conf "... %s\n" p in
  Hutil.header conf title;
  Output.print_string conf "<ul>\n";
  List.iter
    (fun ((_, t) as x) ->
       Output.print_string conf "<li>" ;
       give_access_someone conf base x [];
       Output.printf conf ", %s<li>" (sou base t.t_ident) )
    list ;
  Output.print_string conf "</ul>\n";
  propose_tree_for_list list conf;
  Hutil.trailer conf

let select_title_place conf base title place =
  select_title_place conf base title place ~absolute:(p_getenv conf.env "a" = Some "A")

let select_title conf base title =
  select_title conf base title  ~absolute:(p_getenv conf.env "a" = Some "A")

let print_title_place conf base t p =
  let (l, t, p, t_equiv) = select_title_place conf base t p in
  let list = List.sort (compare_title_order conf base) l in
  print_title_place_list conf base t p t_equiv list

let print_all_with_place conf base p =
  let (l, p) = select_all_with_place conf base p in
  let list = List.sort (compare_title_dates conf base) l in
  print_all_with_place_list conf base p list

let print_places_list conf base t t_equiv list =
  let title h =
    if h || List.length t_equiv = 1 then Output.print_string conf t
    else
      Mutil.list_iter_first
        (fun first t ->
           Output.print_string conf (if first then "" else ", ");
           give_access_all_titles conf t true)
        t_equiv
  in
  let order s = Utf8.capitalize_fst (Name.lower (surname_without_particle base s)) in
  let list = List.sort (fun s1 s2 -> compare (order s1) (order s2)) list in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let wprint_elem p =
    Output.printf conf "<a href=\"%sm=TT&sm=S&t=%s&p=%s%s\">" (commd conf)
      (Mutil.encode t) (Mutil.encode p) (if absolute then "&a=A" else "");
    if p = "" then Output.print_string conf "..."
    else Output.printf conf "%s%s" (surname_without_particle base p) (surname_particle base p);
    Output.print_string conf "</a>"
  in
  Hutil.header conf title;
  wprint_in_columns conf order wprint_elem list;
  Hutil.trailer conf

let print_places conf base t =
  let (l, t, t_equiv) = select_title conf base t in
  let list = List.sort_uniq my_alphabetic l in
  match list with
    [p] -> print_title_place conf base t p
  | _ -> print_places_list conf base t t_equiv list

let print_titles conf base p =
  let (l, p) = select_place conf base p in
  let list = List.sort_uniq my_alphabetic l in
  let title _ = Output.printf conf "... %s" p in
  Hutil.header conf title;
  Output.print_string conf "<ul>\n";
  List.iter
    (fun t ->
       Output.print_string conf "<li>";
       give_access_title conf t p;
       Output.print_string conf "</li>\n")
    list;
  Output.print_string conf "</ul>\n";
  if List.length list > 1 then
    begin
      Output.printf conf "<a href=\"%sm=TT&sm=A&p=%s\">" (commd conf)
        (Mutil.encode p);
      Output.print_string conf (Utf8.capitalize_fst (transl conf "the whole list"));
      Output.print_string conf "</a>\n"
    end;
  Hutil.trailer conf

let print_all_titles conf base =
  let title _ =
    Output.print_string conf (Utf8.capitalize_fst (transl conf "all the titles"))
  in
  let list =
    let l = select_all_titles conf base in
    string_cnt_list_uniq (List.sort compare_titles2 l)
  in
  let order (s, _) = Utf8.capitalize_fst (Name.lower s) in
  let wprint_elem (t, cnt) =
    give_access_all_titles conf t false; Output.printf conf " (%d)" cnt
  in
  Hutil.header conf title;
  wprint_in_columns conf order wprint_elem list;
  Hutil.trailer conf

let print_all_places conf base =
  let title _ =
    Output.print_string conf (Utf8.capitalize_fst (transl conf "all the estates"))
  in
  let list =
    let l = select_all_places conf base in
    List.sort_uniq my_alphabetic l
  in
  Hutil.header conf title;
  Output.print_string conf "<ul>\n";
  List.iter
    (fun t ->
       Output.print_string conf "<li>";
       give_access_all_places conf t;
       Output.print_string conf "</li>\n")
    list;
  Output.print_string conf "</ul>\n";
  Hutil.trailer conf

let print conf base =
  match
    p_getenv conf.env "sm", p_getenv conf.env "t", p_getenv conf.env "p"
  with
    Some "S", Some t, Some p -> print_title_place conf base t p
  | Some "S", Some t, None -> print_places conf base t
  | Some "S", None, Some p -> print_titles conf base p
  | Some "A", None, Some p -> print_all_with_place conf base p
  | _, (Some "" | None), (Some "" | None) -> print_all_titles conf base
  | _, (Some "" | None), Some "*" -> print_all_places conf base
  | _, (Some "" | None), Some p -> print_titles conf base p
  | _, Some t, (Some "" | None) -> print_places conf base t
  | _, Some t, Some p -> print_title_place conf base t p
