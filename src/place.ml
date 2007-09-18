(* camlp5r ./pa_html.cmo *)
(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

value fold_place inverted s =
  let rec loop iend list i ibeg =
    if i = iend then
      if i > ibeg then [String.sub s ibeg (i - ibeg) :: list] else list
    else
      let (list, ibeg) =
        match s.[i] with
        [ ',' ->
            let list =
              if i > ibeg then [String.sub s ibeg (i - ibeg) :: list]
              else list
            in
            (list, i + 1)
        | ' ' -> if i = ibeg then (list, i + 1) else (list, ibeg)
        | _ -> (list, ibeg) ]
      in
      loop iend list (i + 1) ibeg
  in
  let (iend, rest) =
    if String.length s > 0 && s.[String.length s - 1] = ')' then
      match Mutil.rindex s '(' with
      [ Some i when i < String.length s - 2 ->
          let j =
            loop (i - 1) where rec loop i =
              if i >= 0 && s.[i] = ' ' then loop (i - 1) else i + 1
          in
          (j, [String.sub s (i + 1) (String.length s - i - 2)])
      | _ -> (String.length s, []) ]
    else (String.length s, [])
  in
  let list = rest @ loop iend [] 0 0 in
  if inverted then List.rev list else list
;

value get_all conf base =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes" with
    [ Not_found -> False ]
  in
  let ini =
    match p_getenv conf.env "k" with
    [ Some s -> s
    | None -> "" ]
  in
  let ht = Hashtbl.create 5003 in
  let ht_add istr p =
    let (cnt, _) =
      try Hashtbl.find ht (istr, get_surname p) with
      [ Not_found ->
          let cnt = (ref 0, get_key_index p) in
          do { Hashtbl.add ht (istr, get_surname p) cnt; cnt } ]
    in
    incr cnt
  in
  do {
    if add_birth || add_death then
      let rec loop i =
        if i = nb_of_persons base then ()
        else do {
          let p = pget conf base (Adef.iper_of_int i) in
          let pl_bi = get_birth_place p in
          let pl_bp = get_baptism_place p in
          let pl_de = get_death_place p in
          let pl_bu = get_burial_place p in
          if (not add_birth || is_empty_string pl_bi) &&
             (not add_birth || is_empty_string pl_bp) &&
             (not add_death || is_empty_string pl_de) &&
             (not add_death || is_empty_string pl_bu)
          then
            ()
          else do {
            if add_birth && not (is_empty_string pl_bi) then ht_add pl_bi p
            else ();
            if add_birth && not (is_empty_string pl_bp) then ht_add pl_bp p
            else ();
            if add_death && not (is_empty_string pl_de) then ht_add pl_de p
            else ();
            if add_death && not (is_empty_string pl_bu) then ht_add pl_bu p
            else ()
          };
          loop (i + 1)
        }
      in
      loop 0
    else ();
    if add_marriage then
      let rec loop i =
        if i = nb_of_families base then ()
        else do {
          let fam = foi base (Adef.ifam_of_int i) in
          if is_deleted_family fam then ()
          else
            let pl_ma = get_marriage_place fam in
            if not (is_empty_string pl_ma) then
              let fath = pget conf base (get_father fam) in
              let moth = pget conf base (get_mother fam) in
              if fast_auth_age conf fath && fast_auth_age conf moth then do {
                ht_add pl_ma fath; ht_add pl_ma moth
              }
              else ()
            else ();
          loop (i + 1)
        }
      in
      loop 0
    else ();
    let list = ref [] in
    let len = ref 0 in
    Hashtbl.iter
      (fun (istr_pl, _) (cnt, ip) ->
         let s = Util.string_with_macros conf [] (sou base istr_pl) in
         let s = fold_place inverted s in
         if s <> [] && (ini = "" || List.hd s = ini) then do {
           list.val := [(s, cnt.val, ip) :: list.val]; incr len
         }
         else ())
      ht;
    let list =
      List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list.val
    in
    (list, len.val)
  }
;

value max_len = ref 2000;

value print_html_places_surnames conf base list =
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
    [ Some "yes" -> True
    | _ -> False ]
  in
  do {
    Wserver.wprint "<ul>\n";
    let print_li_place x = Wserver.wprint "<li>%s\n" x in
    let print_ul_li_place x =
      do {
        Wserver.wprint "<ul>\n";
        print_li_place x;
      }
    in
    let rec loop prev =
      fun
      [ [(pl, snl) :: list] ->
          let rec loop1 prev pl =
            match (prev, pl) with
            [ ([], [x2 :: l2]) ->
                do {
                  print_li_place x2;
                  List.iter print_ul_li_place l2;
                }
            | ([x1], [x2 :: l2]) ->
                do {
                  if x1 = x2 then () else print_li_place x2;
                  List.iter print_ul_li_place l2
                }
            | ([x1 :: l1], [x2 :: l2]) ->
                if x1 = x2 then loop1 l1 l2
                else do {
                  List.iter (fun _ -> Wserver.wprint "</ul>\n") l1;
                  print_li_place x2;
                  List.iter print_ul_li_place l2;
                }
            | _ -> assert False ]
          in
          do {
            loop1 prev pl;
            Wserver.wprint "<ul>\n<li>\n";
            let snl =
              List.map
                (fun (len, ip) ->
                   let p = pget conf base ip in
                   let sn = p_surname base p in
                   (len, p, sn))
                snl
            in
            let snl =
              List.sort (fun (_, _, sn1) (_, _, sn2) -> compare sn1 sn2) snl
            in
            let snl =
              List.fold_right
                (fun (len, p, sn) ->
                   fun
                   [ [(len1, p1, sn1) :: snl] ->
                       if sn = sn1 then [(len + len1, p, sn) :: snl]
                       else [(len, p, sn); (len1, p1, sn1) :: snl]
                   | [] -> [(len, p, sn)] ])
                snl []
            in
            List.iter
              (fun (len, p, sn) ->
                 do {
                   Wserver.wprint "<a href=\"%s" (commd conf);
                   if link_to_ind then
                     Wserver.wprint "%s" (acces conf base p)
                   else
                     Wserver.wprint "m=N;v=%s" (code_varenv sn);
                   Wserver.wprint "\">%s</a> (%d),\n" sn len
                 })
              snl;
            Wserver.wprint "</ul>\n";
            loop pl list
          }
      | [] -> List.iter (fun _ -> Wserver.wprint "</ul>\n") prev ]
    in
    loop [] list
  }
;

value print_all_places_surnames_short conf list =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "place")) in
  let list =
    List.map
      (fun (s, len, ip) ->
         let s = List.hd s in
         (s, len, ip) )
      list
  in
  let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
  let list =
    List.fold_left
      (fun list (p, len, ip) ->
         match list with
         [ [(p1, len1, ip1) :: list1] when p1 = p ->
             [(p1, len1 + len, ip1) :: list1]
         | _ -> [(p, len, ip) :: list] ])
      [] (List.rev list)
  in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let opt =
    (if add_birth then ";bi=on" else "") ^
      (if add_death then ";de=on" else "") ^
      (if add_marriage then ";ma=on" else "")
  in
  do {
    Hutil.header conf title;
    print_link_to_welcome conf True;
    stag "a" "href=\"%sm=PS%s;k=\"" (commd conf) opt begin
      Wserver.wprint "%s" (transl conf "long display");
    end;
    Wserver.wprint "\n<p>\n";
    List.iter
      (fun (s, len, ip) ->
         do {
           Wserver.wprint "<a href=\"%sm=PS%s;k=%s\">" (commd conf) opt
             (Util.code_varenv s);
           Wserver.wprint "%s</a> (%d),\n" s len;
         })
      list;
    Hutil.trailer conf
  }
;

value print_all_places_surnames_long conf base list =
  let list =
    List.fold_left
      (fun list (pl, len, ip) ->
         match list with
         [ [(pl1, lpl1) :: list1] when pl = pl1 ->
             [(pl1, [(len, ip) :: lpl1]) :: list1]
         | _ -> [(pl, [(len, ip)]) :: list] ])
      [] list
  in
  let list = List.sort (fun (pl1, _) (pl2, _) -> compare pl1 pl2) list in
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  do {
    Hutil.header conf title;
    print_link_to_welcome conf True;
    if list = [] then () else print_html_places_surnames conf base list;
    Hutil.trailer conf
  }
;

value print_all_places_surnames conf base =
  let ini = p_getenv conf.env "k" in
  let (list, len) = get_all conf base in
  if ini = None && len > max_len.val then
    print_all_places_surnames_short conf list
  else print_all_places_surnames_long conf base list
;
