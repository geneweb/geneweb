(* camlp4r ./pa_html.cmo *)
(* $Id: place.ml,v 4.2 2001-06-17 11:05:39 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value fold_place inverted s =
  let rec loop iend list i ibeg =
    if i == iend then
      if i > ibeg then [String.sub s ibeg (i - ibeg) :: list] else list
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
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
    if String.length s > 0 && s.[String.length s - 1] == ')' then
      match Gutil.rindex s '(' with
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
      try Hashtbl.find ht (istr, p.surname) with
      [ Not_found ->
          let cnt = (ref 0, p.cle_index) in
          do { Hashtbl.add ht (istr, p.surname) cnt; cnt } ]
    in
    incr cnt
  in
  let empty =
    try base.func.index_of_string "" with [ Not_found -> Adef.istr_of_int 0 ]
  in
  do {
    if add_birth || add_death then
      let rec loop i =
        if i = base.data.persons.len then ()
        else do {
          let p = base.data.persons.get i in
          let pl_bi = if add_birth then p.birth_place else empty in
          let pl_bp = if add_birth then p.baptism_place else empty in
          let pl_de = if add_death then p.death_place else empty in
          let pl_bu = if add_death then p.burial_place else empty in
          if pl_bi == empty && pl_bp == empty && pl_de == empty &&
             pl_bu == empty ||
             not (fast_auth_age conf p) then
            ()
          else do {
            if pl_bi != empty then ht_add pl_bi p else ();
            if pl_bp != empty then ht_add pl_bp p else ();
            if pl_de != empty then ht_add pl_de p else ();
            if pl_bu != empty then ht_add pl_bu p else ();
          };
          loop (i + 1)
        }
      in
      loop 0
    else ();
    if add_marriage then
      let rec loop i =
        if i = base.data.families.len then ()
        else do {
          let fam = base.data.families.get i in
          if is_deleted_family fam then ()
          else
            let pl_ma = fam.marriage_place in
            if pl_ma <> empty then
              let cpl = coi base fam.fam_index in
              let fath = poi base cpl.father in
              let moth = poi base cpl.mother in
              if fast_auth_age conf fath && fast_auth_age conf moth then do {
                ht_add pl_ma fath; ht_add pl_ma moth;
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
         let s = fold_place inverted (sou base istr_pl) in
         if s <> [] && (ini = "" || List.hd s = ini) then do {
           list.val := [(s, cnt.val, ip) :: list.val]; incr len;
         }
         else ())
      ht;
    let list = Sort.list (fun (s1, _, _) (s2, _, _) -> s1 <= s2) list.val in
    (list, len.val)
  }
;

value max_len = ref 2000;

value print_html_places_surnames conf base =
  do {
    Wserver.wprint "<ul>\n";
    let rec loop prev =
      fun
      [ [(pl, snl) :: list] ->
          do {
            let rec loop1 prev pl =
              match (prev, pl) with
              [ ([], [x2 :: l2]) ->
                  do {
                    Wserver.wprint "<li>%s\n" x2;
                    List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p)
                      l2;
                  }
              | ([x1], [x2 :: l2]) ->
                  do {
                    if x1 = x2 then () else Wserver.wprint "<li>%s\n" x2;
                    List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p)
                      l2;
                  }
              | ([x1 :: l1], [x2 :: l2]) ->
                  if x1 = x2 then loop1 l1 l2
                  else do {
                    List.iter (fun _ -> Wserver.wprint "</ul>\n") l1;
                    Wserver.wprint "<li>%s\n" x2;
                    List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p)
                      l2;
                  }
              | _ -> assert False ]
            in
            loop1 prev pl;
            Wserver.wprint "<ul>\n<li>\n";
            let snl =
              List.map
                (fun (len, ip) ->
                   let p = poi base ip in
                   let sn = p_surname base p in (len, p, sn))
                snl
            in
            let snl =
              Sort.list
                (fun (_, _, sn1) (_, _, sn2) ->
                   Iobase.name_key sn1 <= Iobase.name_key sn2)
                snl
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
                   Wserver.wprint "%s" (acces conf base p);
                   Wserver.wprint "\">%s</a> (%d),\n" sn len;
                 })
              snl;
            Wserver.wprint "</ul>\n";
            loop pl list
          }
      | [] -> List.iter (fun _ -> Wserver.wprint "</ul>\n") prev ]
    in
    loop []
  }
;

value print_all_places_surnames_short conf list =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "place")) in
  let list =
    List.fold_left
      (fun list (pl, len, ip) ->
         let p = List.hd pl in
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
    Util.header conf title;
    print_link_to_welcome conf True;
    stag "a" "href=\"%sm=PS%s;k=\"" (commd conf) opt begin
      Wserver.wprint "%s" (transl conf "long display");
    end;
    Wserver.wprint "\n<p>\n";
    List.iter
      (fun (s, len, ip) ->
         Wserver.wprint "<a href=\"%sm=PS%s;k=%s\">%s</a> (%d),\n"
           (commd conf) opt (Util.code_varenv s) s len)
      list;
    Util.trailer conf;
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
  let list = Sort.list (fun (pl1, _) (pl2, _) -> pl1 <= pl2) list in
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  do {
    Util.header conf title;
    print_link_to_welcome conf True;
    if list = [] then () else print_html_places_surnames conf base list;
    Util.trailer conf;
  }
;

value print_all_places_surnames conf base =
  let ini = p_getenv conf.env "k" in
  let (list, len) = get_all conf base in
  if ini = None && len > max_len.val then
    print_all_places_surnames_short conf list
  else print_all_places_surnames_long conf base list
;
