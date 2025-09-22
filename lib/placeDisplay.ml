(* Copyright (c) 1998-2007 INRIA *)

let print_html_places_surnames conf base
    (array : (string list * (string * Gwdb.iper list) list) array) =
  let link_to_ind =
    List.assoc_opt "place_surname_link_to_ind" conf.Config.base_env = Some "yes"
  in
  let print_sn (sn, ips) =
    let len = List.length ips in
    Output.print_sstring conf "<a href=\"";
    Output.print_string conf (Util.commd conf);
    if link_to_ind && len = 1 then
      Output.print_string conf
        (Util.acces conf base @@ Util.pget conf base @@ List.hd ips)
    else (
      Output.print_sstring conf "m=N&v=";
      Output.print_string conf (Mutil.encode sn));
    Output.print_sstring conf "\">";
    Output.print_string conf (Util.escape_html sn);
    Output.print_sstring conf "</a> (";
    Output.print_sstring conf (string_of_int len);
    Output.print_sstring conf ")"
  in
  let print_sn_list (snl : (string * Gwdb.iper list) list) =
    let snl =
      List.sort (fun (sn1, _) (sn2, _) -> Utf8.alphabetic_order sn1 sn2) snl
    in
    Output.print_sstring conf "<li>\n";
    Ext_list.iter_first
      (fun first x ->
        if not first then Output.print_sstring conf ",\n";
        print_sn x)
      snl;
    Output.print_sstring conf "\n";
    Output.print_sstring conf "</li>\n"
  in
  let rec loop prev = function
    | (pl, snl) :: list ->
        let rec loop1 prev pl =
          match (prev, pl) with
          | [], l2 ->
              List.iter (fun x -> Output.printf conf "<li>%s<ul>\n" x) l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 l2
              else (
                List.iter
                  (fun _ -> Output.print_sstring conf "</ul></li>\n")
                  (x1 :: l1);
                loop1 [] (x2 :: l2))
          | _ -> assert false
        in
        loop1 prev pl;
        print_sn_list snl;
        loop pl list
    | [] -> List.iter (fun _ -> Output.print_sstring conf "</ul></li>\n") prev
  in
  Output.print_sstring conf "<ul>\n";
  loop [] (Array.to_list array);
  Output.print_sstring conf "</ul>\n"

let print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage =
  Adef.encoded
  @@ (if add_birth then "&bi=on" else "")
  ^ (if add_baptism then "&bp=on" else "")
  ^ (if add_death then "&de=on" else "")
  ^ (if add_burial then "&bu=on" else "")
  ^ if add_marriage then "&ma=on" else ""

let print_aux conf title fn =
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  fn ();
  Hutil.trailer conf

let print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death
    ~add_burial ~add_marriage =
  let inverted =
    List.assoc_opt "places_inverted" conf.Config.base_env = Some "yes"
  in
  let array =
    Place.get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage "" 0
      (Place.fold_place_short inverted)
      (fun _ -> true)
      (fun prev _ -> match prev with Some n -> n + 1 | None -> 1)
      Fun.id max_int
  in
  Array.sort (fun (s1, _) (s2, _) -> Utf8.alphabetic_order s1 s2) array;
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "place"))
  in
  print_aux conf title (fun () ->
      let opt =
        print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial
          ~add_marriage
      in
      Output.print_sstring conf "<p><a href=\"";
      Output.print_string conf (Util.commd conf);
      Output.print_sstring conf "m=PS";
      Output.print_string conf opt;
      Output.print_sstring conf "&display=long\">";
      Output.print_sstring conf (Util.transl conf "long display");
      Output.print_sstring conf "</a></p><p>";
      let last = Array.length array - 1 in
      Array.iteri
        (fun i (s, x) ->
          Output.print_sstring conf "<a href=\"";
          Output.print_string conf (Util.commd conf);
          Output.print_sstring conf "m=PS";
          Output.print_string conf opt;
          Output.print_sstring conf "&k=";
          Output.print_string conf (Mutil.encode s);
          Output.print_sstring conf "\">";
          Output.print_string conf (Util.escape_html s);
          Output.print_sstring conf "</a> (";
          Output.print_sstring conf (string_of_int x);
          Output.print_sstring conf ")";
          if i <> last then Output.print_sstring conf ", ")
        array;
      Output.print_sstring conf "</p>\n")

let print_all_places_surnames_long conf base ini ~add_birth ~add_baptism
    ~add_death ~add_burial ~add_marriage max_length =
  let filter =
    if ini = "" then ( <> ) []
    else function x :: _ when x = ini -> true | _ -> false
  in
  let inverted =
    List.assoc_opt "places_inverted" conf.Config.base_env = Some "yes"
  in
  let array =
    Place.get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage [] []
      (Place.fold_place_long inverted)
      filter
      (fun prev p ->
        let value = (Gwdb.get_surname p, Gwdb.get_iper p) in
        match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
        let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
        let rec loop acc list =
          match (list, acc) with
          | [], _ -> acc
          | (sn, iper) :: tl_list, (sn', iper_list) :: tl_acc
            when Gwdb.sou base sn = sn' ->
              loop ((sn', iper :: iper_list) :: tl_acc) tl_list
          | (sn, iper) :: tl_list, _ ->
              loop ((Gwdb.sou base sn, [ iper ]) :: acc) tl_list
        in
        loop [] v)
      max_length
  in
  let rec sort_place_utf8 pl1 pl2 =
    match (pl1, pl2) with
    | _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 -> (
        match Utf8.alphabetic_order s1 s2 with
        | 0 -> sort_place_utf8 pl11 pl22
        | x -> x)
  in
  Array.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) array;
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "place"));
    Output.print_sstring conf " / ";
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl_nth conf "surname/surnames" 0))
  in
  print_aux conf title (fun () ->
      if ini = "" then (
        Output.print_sstring conf "<p><a href=\"";
        Output.print_string conf (Util.commd conf);
        Output.print_sstring conf "m=PS";
        Output.print_string conf
          (print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial
             ~add_marriage);
        Output.print_sstring conf "&display=short\">";
        Output.print_sstring conf (Util.transl conf "short display");
        Output.print_sstring conf "</a></p><p>");
      if array <> [||] then print_html_places_surnames conf base array)

let print_all_places_surnames conf base =
  let add_marriage = Util.p_getenv conf.Config.env "ma" = Some "on" in
  let add_birth = Util.p_getenv conf.Config.env "bi" = Some "on" in
  let add_baptism = Util.p_getenv conf.Config.env "bp" = Some "on" in
  let add_death = Util.p_getenv conf.Config.env "de" = Some "on" in
  let add_burial = Util.p_getenv conf.Config.env "bu" = Some "on" in
  match Util.p_getenv conf.Config.env "k" with
  | Some ini ->
      print_all_places_surnames_long conf base ini ~add_birth ~add_baptism
        ~add_death ~add_burial ~add_marriage max_int
  | None -> (
      match Util.p_getenv conf.Config.env "display" with
      | Some "long" ->
          print_all_places_surnames_long conf base "" ~add_birth ~add_baptism
            ~add_death ~add_burial ~add_marriage max_int
      | Some "short" ->
          print_all_places_surnames_short conf base ~add_birth ~add_baptism
            ~add_death ~add_burial ~add_marriage
      | Some _ -> assert false
      | None -> (
          try
            let lim =
              Option.value ~default:500
                (Option.bind
                   (List.assoc_opt "short_place_threshold" conf.Config.base_env)
                   int_of_string_opt)
            in
            print_all_places_surnames_long conf base "" ~add_birth ~add_baptism
              ~add_death ~add_burial ~add_marriage lim
          with Place.List_too_long ->
            print_all_places_surnames_short conf base ~add_birth ~add_baptism
              ~add_death ~add_burial ~add_marriage))
