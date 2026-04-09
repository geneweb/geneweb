(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let print_html_places_surnames conf base
    (array : (string list * (string * Driver.iper list) list) array) =
  let link_to_ind =
    List.assoc_opt "place_surname_link_to_ind" conf.base_env = Some "yes"
  in
  let print_sn (sn, ips) =
    let len = List.length ips in
    Output.print_sstring conf "<a href=\"";
    Output.print_string conf (commd conf);
    match ips with
    | [ ips ] when link_to_ind ->
        Output.print_string conf (acces conf base @@ pget conf base @@ ips)
    | _ ->
        Output.print_sstring conf "m=N&v=";
        Output.print_string conf (Mutil.encode sn);
        Output.print_sstring conf "\">";
        Output.print_string conf (escape_html sn);
        Output.print_sstring conf "</a> (";
        Output.print_sstring conf (string_of_int len);
        Output.print_sstring conf ")"
  in
  let print_sn_list (snl : (string * Driver.iper list) list) =
    let snl =
      List.sort (fun (sn1, _) (sn2, _) -> Gutil.alphabetic_order sn1 sn2) snl
    in
    Output.print_sstring conf "<li>\n";
    Mutil.list_iter_first
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
  fn ();
  Hutil.trailer conf

let print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death
    ~add_burial ~add_marriage =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    Place.get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage "" 0
      (Place.fold_place_short inverted)
      (fun _ -> true)
      (fun prev _ -> match prev with Some n -> n + 1 | None -> 1)
      (fun x -> x)
      max_int
  in
  Array.sort (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2) array;
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "place/places" 0))
  in
  print_aux conf title (fun () ->
      let opt =
        print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial
          ~add_marriage
      in
      Output.print_sstring conf "<p><a href=\"";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=PS";
      Output.print_string conf opt;
      Output.print_sstring conf "&display=long\">";
      Output.print_sstring conf (transl conf "long display");
      Output.print_sstring conf "</a></p><p>";
      let last = Array.length array - 1 in
      Array.iteri
        (fun i (s, x) ->
          Output.print_sstring conf "<a href=\"";
          Output.print_string conf (commd conf);
          Output.print_sstring conf "m=PS";
          Output.print_string conf opt;
          Output.print_sstring conf "&k=";
          Output.print_string conf (Mutil.encode s);
          Output.print_sstring conf "\">";
          Output.print_string conf (escape_html s);
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
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    Place.get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage [] []
      (Place.fold_place_long_v6 inverted)
      filter
      (fun prev p ->
        let value = (Driver.get_surname p, Driver.get_iper p) in
        match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
        let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
        let rec loop acc list =
          match (list, acc) with
          | [], _ -> acc
          | (sn, iper) :: tl_list, (sn', iper_list) :: tl_acc
            when Driver.sou base sn = sn' ->
              loop ((sn', iper :: iper_list) :: tl_acc) tl_list
          | (sn, iper) :: tl_list, _ ->
              loop ((Driver.sou base sn, [ iper ]) :: acc) tl_list
        in
        loop [] v)
      max_length
  in
  let rec sort_place_utf8 pl1 pl2 =
    match (pl1, pl2) with
    | _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 -> (
        match Gutil.alphabetic_order s1 s2 with
        | 0 -> sort_place_utf8 pl11 pl22
        | x -> x)
  in
  Array.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) array;
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "place/places" 0));
    Output.print_sstring conf " / ";
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_nth conf "surname/surnames" 0))
  in
  print_aux conf title (fun () ->
      if ini = "" then (
        Output.print_sstring conf "<p><a href=\"";
        Output.print_string conf (commd conf);
        Output.print_sstring conf "m=PS";
        Output.print_string conf
          (print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial
             ~add_marriage);
        Output.print_sstring conf "&display=short\">";
        Output.print_sstring conf (transl conf "short display");
        Output.print_sstring conf "</a></p><p>");
      if array <> [||] then print_html_places_surnames conf base array)

let print_all_places_surnames conf base =
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  match p_getenv conf.env "k" with
  | Some ini ->
      print_all_places_surnames_long conf base ini ~add_birth ~add_baptism
        ~add_death ~add_burial ~add_marriage max_int
  | None -> (
      match p_getenv conf.env "display" with
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
              try
                int_of_string
                @@ List.assoc "short_place_threshold" conf.base_env
              with _ -> 500
            in
            print_all_places_surnames_long conf base "" ~add_birth ~add_baptism
              ~add_death ~add_burial ~add_marriage lim
          with Place.List_too_long ->
            print_all_places_surnames_short conf base ~add_birth ~add_baptism
              ~add_death ~add_burial ~add_marriage))
