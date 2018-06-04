(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Hutil
open Util


let fold_place inverted s =
  (* petit hack en attendant une vraie gestion des lieux transforme
     "[foo-bar] - boobar (baz)" en "foo-bar, boobar (baz)" *)
  let s =
    Str.global_replace (Str.regexp "^\\[\\([^]]+\\)\\] *- *\\(.*\\)") "\\1, \\2" s
  in
  let rec loop iend list i ibeg =
    if i = iend then
      if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg) =
        match s.[i] with
          ',' ->
            let list =
              if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
            in
            list, i + 1
        | ' ' -> if i = ibeg then list, i + 1 else list, ibeg
        | _ -> list, ibeg
      in
      loop iend list (i + 1) ibeg
  in
  let (iend, rest) =
    if String.length s > 0 && s.[String.length s - 1] = ')' then
      match String.rindex_opt s '(' with
        Some i when i < String.length s - 2 ->
          let j =
            let rec loop i =
              if i >= 0 && s.[i] = ' ' then loop (i - 1) else i + 1
            in
            loop (i - 1)
          in
          j, [String.sub s (i + 1) (String.length s - i - 2)]
      | _ -> String.length s, []
    else String.length s, []
  in
  let list = rest @ loop iend [] 0 0 in
  if inverted then List.rev list else list

let get_all conf base =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes" with
      Not_found -> false
  in
  let ini =
    match p_getenv conf.env "k" with
      Some s -> s
    | None -> ""
  in
  let ht = Hashtbl.create 5003 in
  let ht_add istr p =
    let (cnt, _) =
      try Hashtbl.find ht (istr, get_surname p) with
        Not_found ->
          let cnt = ref 0, get_key_index p in
          Hashtbl.add ht (istr, get_surname p) cnt; cnt
    in
    incr cnt
  in
  if add_birth || add_death || add_baptism || add_burial then
    begin let rec loop i =
      if i = nb_of_persons base then ()
      else
        let p = pget conf base (Adef.iper_of_int i) in
        let pl_bi = get_birth_place p in
        let pl_bp = get_baptism_place p in
        let pl_de = get_death_place p in
        let pl_bu = get_burial_place p in
        if (not add_birth || is_empty_string pl_bi) &&
           (not add_baptism || is_empty_string pl_bp) &&
           (not add_death || is_empty_string pl_de) &&
           (not add_burial || is_empty_string pl_bu)
        then
          ()
        else if authorized_age conf base p then
          begin
            if add_birth && not (is_empty_string pl_bi) then ht_add pl_bi p;
            if add_baptism && not (is_empty_string pl_bp) then ht_add pl_bp p;
            if add_death && not (is_empty_string pl_de) then ht_add pl_de p;
            if add_burial && not (is_empty_string pl_bu) then ht_add pl_bu p
          end;
        loop (i + 1)
    in
      loop 0
    end;
  if add_marriage then
    begin let rec loop i =
      if i = nb_of_families base then ()
      else
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else
          begin let pl_ma = get_marriage_place fam in
            if not (is_empty_string pl_ma) then
              let fath = pget conf base (get_father fam) in
              let moth = pget conf base (get_mother fam) in
              if authorized_age conf base fath &&
                 authorized_age conf base moth
              then
                begin ht_add pl_ma fath; ht_add pl_ma moth end
          end;
        loop (i + 1)
    in
      loop 0
    end;
  let list = ref [] in
  let len = ref 0 in
  Hashtbl.iter
    (fun (istr_pl, _) (cnt, ip) ->
       let s = Util.string_with_macros conf [] (sou base istr_pl) in
       let s = fold_place inverted s in
       if s <> [] && (ini = "" || List.hd s = ini) then
         begin list := (s, !cnt, ip) :: !list; incr len end)
    ht;
  let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) !list in
  list, !len

let max_len = ref 2000

let print_html_places_surnames conf base list =
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
      Some "yes" -> true
    | _ -> false
  in
  let print_sn len p sn sep =
    Wserver.printf "%s<a href=\"%s" sep (commd conf);
    if link_to_ind then Wserver.printf "%s" (acces conf base p)
    else Wserver.printf "m=N;v=%s" (code_varenv sn);
    Wserver.printf "\">%s</a> (%d)" sn len
  in
  let print_sn_list snl =
    let snl =
      List.map
        (fun (len, ip) ->
           let p = pget conf base ip in
           let sn = p_surname base p in len, p, sn)
        snl
    in
    let snl =
      List.sort
        (fun (_, _, sn1) (_, _, sn2) -> Gutil.alphabetic_order sn1 sn2) snl
    in
    let snl =
      List.fold_right
        (fun (len, p, sn) ->
           function
             (len1, p1, sn1) :: snl ->
               if sn = sn1 then (len + len1, p, sn) :: snl
               else (len, p, sn) :: (len1, p1, sn1) :: snl
           | [] -> [len, p, sn])
        snl []
    in
    let (len, p, sn, snl) =
      match snl with
        (len, p, sn) :: snl -> len, p, sn, snl
      | _ -> assert false
    in
    Wserver.printf "<li>\n";
    print_sn len p sn "";
    List.iter (fun (len, p, sn) -> print_sn len p sn ",\n") snl;
    Wserver.printf "\n";
    Wserver.printf "</li>\n"
  in
  let rec loop prev =
    function
      (pl, snl) :: list ->
        let rec loop1 prev pl =
          match prev, pl with
            [], l2 -> List.iter (fun x -> Wserver.printf "<li>%s<ul>\n" x) l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 l2
              else
                begin
                  List.iter (fun _ -> Wserver.printf "</ul></li>\n")
                    (x1 :: l1);
                  loop1 [] (x2 :: l2)
                end
          | _ -> assert false
        in
        loop1 prev pl; print_sn_list snl; loop pl list
    | [] -> List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev
  in
  Wserver.printf "<ul>\n"; loop [] list; Wserver.printf "</ul>\n"

let print_all_places_surnames_short conf list =
  let title _ = Wserver.printf "%s" (capitale (transl conf "place")) in
  let list =
    List.map (fun (s, len, ip) -> let s = List.hd s in s, len, ip) list
  in
  let list =
    List.sort (fun (s1, _, _) (s2, _, _) -> Gutil.alphabetic_order s1 s2) list
  in
  let list =
    List.fold_left
      (fun list (p, len, ip) ->
         match list with
           (p1, len1, ip1) :: list1 when p1 = p ->
             (p1, len1 + len, ip1) :: list1
         | _ -> (p, len, ip) :: list)
      [] (List.rev list)
  in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let opt =
    (if add_birth then ";bi=on" else "") ^
    (if add_baptism then ";bp=on" else "") ^
    (if add_death then ";de=on" else "") ^
    (if add_burial then ";bu=on" else "") ^
    (if add_marriage then ";ma=on" else "")
  in
  Hutil.header conf title;
  print_link_to_welcome conf true;
  Wserver.printf "<p>\n";
  Wserver.printf "<a href=\"%sm=PS%s;k=\">" (commd conf) opt;
  Wserver.printf "%s" (transl conf "long display");
  Wserver.printf "</a>";
  Wserver.printf "</p>\n";
  Wserver.printf "<p>\n";
  List.iter
    (fun (s, len, _) ->
       Wserver.printf "<a href=\"%sm=PS%s;k=%s\">" (commd conf) opt
         (Util.code_varenv s);
       Wserver.printf "%s" s;
       Wserver.printf "</a>";
       Wserver.printf " (%d),\n" len)
    list;
  Wserver.printf "</p>\n";
  Hutil.trailer conf

let print_all_places_surnames_long conf base list =
  let list =
    List.fold_left
      (fun list (pl, len, ip) ->
         match list with
           (pl1, lpl1) :: list1 when pl = pl1 ->
             (pl1, (len, ip) :: lpl1) :: list1
         | _ -> (pl, [len, ip]) :: list)
      [] list
  in
  let rec sort_place_utf8 pl1 pl2 =
    match pl1, pl2 with
      _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 ->
        if Gutil.alphabetic_order s1 s2 = 0 then sort_place_utf8 pl11 pl22
        else Gutil.alphabetic_order s1 s2
  in
  let list =
    List.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) list
  in
  let title _ =
    Wserver.printf "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  Hutil.header conf title;
  print_link_to_welcome conf true;
  if list = [] then () else print_html_places_surnames conf base list;
  Hutil.trailer conf

let print_all_places_surnames conf base =
  let ini = p_getenv conf.env "k" in
  let (list, len) = get_all conf base in
  if ini = None && len > !max_len then
    print_all_places_surnames_short conf list
  else print_all_places_surnames_long conf base list
