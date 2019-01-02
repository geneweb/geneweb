(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let normalize =
  (* petit hack en attendant une vraie gestion des lieux transforme
     "[foo-bar] - boobar (baz)" en "foo-bar, boobar (baz)" *)
  let r = Str.regexp "^\\[\\([^]]+\\)\\] *- *\\(.*\\)" in
  fun s -> Str.global_replace r "\\1, \\2" s

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  let len = String.length s in
  (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
  let rec loop iend list i ibeg =
    if i = iend
    then if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
        | ',' ->
          let list =
            if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
          in
          list, i + 1
        | ' ' when i = ibeg -> (list, i + 1)
        | _ -> list, ibeg
      in
      loop iend list (i + 1) ibeg
  in
  let (iend, rest) =
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        let j =
          let rec loop i =
            if i >= 0 && String.unsafe_get s i = ' '
            then loop (i - 1) else i + 1
          in
          loop (i - 1)
        in
        j, [ String.sub s (i + 1) (len - i - 2) ]
      | _ -> len, []
    else len, []
  in
  let list = List.rev_append rest @@ loop iend [] 0 0 in
  if inverted then List.rev list else list

let fold_place_short s =
  let len = String.length s in
  let default () =
    let i =
      match String.rindex_opt s ',' with
      | Some i ->
        let rec l i =
          if i < len && String.unsafe_get s i = ' '
          then l (i + 1) else i in l (i + 1)
      | None -> 0
    in
    let i = if i = len then 0 else i in
    String.sub s i (len - i)
  in
  if String.unsafe_get s (len - 1) = ')'
  then match String.rindex_opt s '(' with
    | Some i when i < len - 2 ->
      String.sub s (i + 1) (len - i - 2)
    | _ -> default ()
  else default ()

let get_all =
  fun conf base ~add_birth ~add_baptism ~add_death ~add_burial
    (dummy_key : 'a)
    (dummy_value : 'c)
    (fold_place : string -> 'a)
    (filter : 'a -> bool)
    (mk_value : 'b option -> person -> 'b)
    (foo : 'b -> 'c) :
    ('a * 'c) array ->
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr p =
    let key : 'a = sou base istr |> normalize |> fold_place in
    if filter key then
      match Hashtbl.find_opt ht key with
      | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
      | None -> Hashtbl.add ht key (mk_value None p)
  in
  if add_birth || add_death || add_baptism || add_burial then begin
    let len = nb_of_persons base in
    let aux b fn p =
      if b then let x = fn p in if not (is_empty_string x) then ht_add x p
    in
    let rec loop i =
      if i < len then begin
        let p = pget conf base (Adef.iper_of_int i) in
        if authorized_age conf base p then begin
          aux add_birth get_birth_place p ;
          aux add_baptism get_baptism_place p ;
          aux add_death get_death_place p ;
          aux add_burial get_burial_place p ;
        end ;
        loop (i + 1)
      end
    in
    loop 0 ;
  end ;
  if add_marriage then begin
    let rec loop i =
      let len = nb_of_families base in
      if i < len then begin
        let fam = foi base (Adef.ifam_of_int i) in
        if not @@ is_deleted_family fam then begin
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
            end
        end ;
        loop (i + 1) ;
      end
    in
    loop 0 ;
  end ;
  let len = Hashtbl.length ht in
  let array = Array.make len (dummy_key, dummy_value) in
  let i = ref 0 in
  Hashtbl.iter
    (fun k v ->
       Array.unsafe_set array !i (k, foo v) ;
       incr i)
    ht ;
  array

let cons_uniq xs x = if List.mem x xs then xs else x :: xs

let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

let get_opt conf =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  (if add_birth then "&bi=on" else "") ^
  (if add_baptism then "&bp=on" else "") ^
  (if add_death then "&de=on" else "") ^
  (if add_burial then "&bu=on" else "") ^
  (if add_marriage then "&ma=on" else "")

let print_html_places_surnames_long conf base place1 place2
  (array : (string list * (string * Adef.iper list) list) array) =
  let list = Array.to_list array in
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
      Some "yes" -> true
    | _ -> false
  in
  let opt = get_opt conf in
  if place1 = "" && place2 = ""
  then Wserver.printf "<p>\n<a href=\"%sm=PS%s\">%s</a></p>\n"
      (commd conf) opt (transl conf "short display")
  else Wserver.printf "<p>\n<a href=\"%sm=PS%s&long=on\">%s</a></p>\n"
      (commd conf) opt (transl conf "long display");
  let _ = if place1 <> "" then
    Wserver.printf
      "<p><a href=\"%sm=PS%s&k1=%s\" title=\"%s for %s\">%s</a>\n</p>\n"
    (commd conf) opt place1 (transl conf "short display") place1 place1
  in
  let _ =
    if place2 <> "" then Wserver.printf "</p>&nbsp;&nbsp;&nbsp;%s<p>" place2
  in
  let print_sn (sn, ips) =
    let ips = remove_from_left ips in
    let len = List.length ips in
    if link_to_ind && len = 1
    then
      Wserver.printf "<a href=\"%s%s\">%s</a> (%d)" (commd conf)
      (acces conf base @@ pget conf base @@ List.hd ips) sn len
    else
      begin
        Wserver.printf "<a href=\"%sm=N&v=%s\">%s</a>" (commd conf)
          (code_varenv sn) sn;
        if link_to_ind then
          begin
            Wserver.printf " (<a href=\"%sm=LIST" (commd conf);
            List.iteri (fun i ip ->
              Wserver.printf "&i%d=%d" i (Adef.int_of_iper ip))
            ips;
            Wserver.printf "\">%d</a>)" len
          end
        else Wserver.printf " (%d)" len
      end
  in
  let print_sn_list (snl : (string * Adef.iper list) list) =
    let snl = List.sort
      (fun (sn1, _) (sn2, _) -> Gutil.alphabetic_order sn1 sn2) snl
    in
    Wserver.printf "<li>\n";
    Mutil.list_iter_first
      (fun first x -> if not first then Wserver.printf ",\n" ; print_sn x) snl ;
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
        loop1 prev pl;
        print_sn_list snl;
        loop pl list
    | [] -> List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev
  in
  Wserver.printf "<ul>\n";
  loop [] list;
  Wserver.printf "</ul>\n"

let print_html_places_surnames_short conf place1 place2
  (array : (string list * (string * Adef.iper list) list) array) =
  let opt = get_opt conf in
  Wserver.printf "<p>\n<a href=\"%sm=PS%s&long=on\">%s</a></p>\n"
  (commd conf) opt (transl conf "long display");
  let list = Array.to_list array in
  let print_sn_list (snl : (string * Adef.iper list) list) =
    (List.fold_left (fun cnt (_sn, ips) -> cnt + (List.length ips)) 0 snl)
  in
  let _ = if place1 <> ""
    then Wserver.printf
      "<a href=\"%sm=PS%s&long=on&k1=%s\" title=\"%s for %s\">%s</a>\n</p>"
      (commd conf) opt place1 (transl conf "long display") place1 place1;
  in
  let _ = if place2 <> "" then
    Wserver.printf "</p>&nbsp;&nbsp;&nbsp;%s<p>" place2
  in
  let rec loop cnt prev =
    function
       (pl, snl) :: list ->
        if (if pl <> [] then List.hd pl else "") <>
           (if prev <> [] then List.hd prev else "")
        then
          begin
            Wserver.printf "%s"
              (if cnt <> 0 then (Printf.sprintf " (%d), " cnt) else "");
            Wserver.printf "\n<a href=\"%sm=PS%s%s%s%s\">%s</a>" (commd conf) opt
              (Printf.sprintf "&k1=%s"
                (if place1 = "" then (Util.code_varenv (List.hd pl)) else place1))
              (Printf.sprintf "&k2=%s"
                (if place1 = "" then "" else (Util.code_varenv (List.hd pl))))
              (if place1 <> "" && place2 <> "" then "&long=on" else "")
              (List.hd pl);
          end
        else ();
        loop
          (if (if pl <> [] then List.hd pl else "") <>
              (if prev <> [] then List.hd prev else "")
          then (print_sn_list snl) else cnt + (print_sn_list snl))
          pl list;
    | [] -> Wserver.printf "%s\n</p>\n"
              (if cnt <> 0 then (Printf.sprintf " (%d)" cnt) else "")
  in
  loop 0 [] list

let print_places_surnames conf base place1 place2 array short =
  let rec sort_place_utf8 pl1 pl2 =
    match pl1, pl2 with
      _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 ->
        match Gutil.alphabetic_order s1 s2 with
        | 0 -> sort_place_utf8 pl11 pl22
        | x -> x
  in
  Array.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) array ;
  let title _ =
    Wserver.printf "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  if array <> [||] then
    if short
    then print_html_places_surnames_short conf place1 place2 array
    else print_html_places_surnames_long conf base place1 place2 array;
  Hutil.trailer conf

let filter_array array place =
  let list = Array.to_list array in
  if place <> "" then
    let rec loop acc =
      function
        (pl, snl) :: list ->
          loop (if (if List.length pl > 0 then List.hd pl else "") = place
            then ((List.tl pl), snl) :: acc else acc)
          list;
      | [] -> acc
    in
    (Array.of_list (loop [] list))
  else array

let print_places_surnames_some conf base array =
  let (array, place1) =
    match p_getenv conf.env "k1" with
    | Some place ->
        if place <> "" then (filter_array array place, place)
        else (array, "")
    | None -> (array, "")
  in
  let (array, place2) =
    match p_getenv conf.env "k2" with
    | Some place ->
        if place <> "" then (filter_array array place, place)
        else (array, "")
    | None -> (array, "")
  in
  let len = Array.length array in
  let len_max =
    int_of_string
      (try List.assoc "nb_places_short" conf.base_env with Not_found -> "200")
  in
  match p_getenv conf.env "long" with
  | Some "on" -> print_places_surnames conf base place1 place2 array false
  | _ ->
      if len > len_max
      then print_places_surnames conf base place1 place2 array true
      else print_places_surnames conf base place1 place2 array false

let print_list conf base =
  let title _ =
    Wserver.printf "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "person/persons" 1))
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p>\n";
  Wserver.printf "</p>\n";
  let rec loop i =
    match p_getenv conf.env ("i" ^ (string_of_int i)) with
    | Some ip ->
        let p = poi base (Adef.iper_of_int (int_of_string ip)) in
        Wserver.printf "<a href=\"%s%s\">%s %s %s</a><br>\n"
          (commd conf)
          (acces conf base @@ pget conf base
            (Adef.iper_of_int (int_of_string ip)))
          (p_first_name base p) (p_surname base p)
          (if (get_occ p) > 0 then (Printf.sprintf "(%d)" (get_occ p)) else "");
        loop (i + 1)
    | None -> ()
  in loop 0;
  Hutil.trailer conf

let print_all_places_surnames conf base =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      [] [] (fold_place_long inverted) (fun _ -> true)
      (fun prev p ->
         let value = (get_surname p, get_key_index p) in
         match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
         let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
         let rec loop acc list = match list, acc with
           | [], _ -> acc
           | (sn, iper) :: tl_list,
              (sn', iper_list) :: tl_acc when (sou base sn) = sn' ->
             loop ((sn', iper:: iper_list) :: tl_acc) tl_list
           | (sn, iper) :: tl_list, _ ->
             loop ((sou base sn, [iper]) :: acc) tl_list
         in
         loop [] v)
  in
  print_places_surnames_some conf base array


