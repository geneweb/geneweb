(* camlp5r ./pa_html.cmo *)
(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

module PlaceSet = Set.Make
  (struct
    type t = (Gwdb.istr * string * int) ;
    value compare (_, s1, i1) (_, s2, i2) = 
      let comp_s = Pervasives.compare s1 s2 in
      if comp_s = 0 then Pervasives.compare i1 i2
      else comp_s ;
  end)
;

module PersMap = Map.Make
  (struct 
    type t = int;
    value compare = compare;
  end)
;

module PersSet = Set.Make 
  (struct 
    type t = person; 
    value compare p1 p2 = 
      let i1 = Adef.int_of_iper (get_key_index p1) in
      let i2 = Adef.int_of_iper (get_key_index p2) in
      Pervasives.compare i1 i2; 
  end)
;

module StringSet = Set.Make
  (struct
    type t = string;
    value compare = compare;
  end)
;

value fold_place inverted s =
  (* petit hack (pour GeneaNet) en attendant une vraie gestion des lieux *)
  (* transforme "[foo-bar] - boobar (baz)" en "foo-bar, boobar (baz)"    *)
  let s =
   Str.global_replace (Str.regexp "^\[\([^]]+\)\] *- *\(.*\)") "\1, \2" s 
  in
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
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
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
    if add_birth || add_death || add_baptism || add_burial then
      let rec loop i =
        if i = nb_of_persons base then ()
        else do {
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
          else do {
            if (fast_auth_age conf p) then do {
              if add_birth && not (is_empty_string pl_bi) then ht_add pl_bi p
              else ();
              if add_baptism && not (is_empty_string pl_bp) then ht_add pl_bp p
              else ();
              if add_death && not (is_empty_string pl_de) then ht_add pl_de p
              else ();
              if add_burial && not (is_empty_string pl_bu) then ht_add pl_bu p
              else () }
            else ();
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


(* ******************************************************************** *)
(*  [Fonc] get_all_places : config -> base ->  PlaceSet.elt list        *)
(** [Description] : Construit la liste de tous les lieux de la base et
                    leur associe une clé unique.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * int) list : La liste sans doublon de tous les 
          lieux de la base avec leur clé.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value get_all_places conf base = do {
  let places = ref PlaceSet.empty in
  (* Ajoute tous les lieux liés à un individu *)
  let rec loop i = 
    if i = nb_of_persons base then ()
    else 
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        let pl_bi = get_birth_place p in
        let pl_bp = get_baptism_place p in
        let pl_de = get_death_place p in
        let pl_bu = get_burial_place p in
        if not (is_empty_string pl_bi) then 
          places.val := 
            PlaceSet.add (pl_bi, "bi", Hashtbl.hash pl_bi) places.val
        else ();
        if not (is_empty_string pl_bp) then 
          places.val := 
            PlaceSet.add (pl_bp, "bp", Hashtbl.hash pl_bp) places.val
        else ();
        if not (is_empty_string pl_de) then 
          places.val := 
            PlaceSet.add (pl_de, "de", Hashtbl.hash pl_de) places.val
        else ();
        if not (is_empty_string pl_bu) then 
          places.val := 
            PlaceSet.add (pl_bu, "bu", Hashtbl.hash pl_bu) places.val
        else ();
        loop (i+1)
      }
  in loop 0;
  (* Ajoute tous les lieux liés à une famille *)
  let rec loop i = 
    if i = nb_of_families base then ()
    else
      do {
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else do {
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then 
            places.val := 
              PlaceSet.add (pl_ma, "ma", Hashtbl.hash pl_ma) places.val
          else (); } ;
        loop (i+1)
      }
  in loop 0;
  PlaceSet.elements places.val }
;


(* ************************************************************************** *)
(*  [Fonc] get_person_places : config -> base -> 
                                 (Gwdb.istr * PersSet.elt list) list          *)
(** [Description] : Construit a partir de la clé d'un lieu, l'ensemble des
                    personnes en relation avec ce lieu.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * person list) : retourne la liste des adresses de lieux 
          ainsi que la liste des personnes en relation avec ce lieu.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value get_person_places conf base = do {
  let env_keys = 
    List.map
      (fun key -> 
        match p_getenv conf.env key with
        [ Some s -> (key, int_of_string s)
        | None -> (key, (-1)) ] )
      [ "bi"; "bp"; "de"; "bu"; "ma" ] 
  in
  let pers_map = ref PersMap.empty in
  (* Fonction d'ajout dans la map des personnes (PersMap).        *)
  (* k = key, istr = place, p = person.                           *)
  (* A la clé k est associé le binding (istr, ensemble d'individu *)
  let map_add k istr p =
    try
      let (istr, set) = PersMap.find k pers_map.val in
      let set = PersSet.add p set in
      pers_map.val := PersMap.add k (istr, set) pers_map.val
    with 
    [ Not_found ->
      let set = PersSet.add p PersSet.empty in
      pers_map.val := PersMap.add k (istr, set) pers_map.val ]
  in
  (* Parcours tous les individus et ajoute dans la map les  *)
  (* individus en relation avec le lieu donné par la clé k. *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else 
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        let pl_bi = get_birth_place p in
        let hash_pl_bi = Hashtbl.hash pl_bi in
        let pl_bp = get_baptism_place p in
        let hash_pl_bp = Hashtbl.hash pl_bp in
        let pl_de = get_death_place p in
        let hash_pl_de = Hashtbl.hash pl_de in
        let pl_bu = get_burial_place p in
        let hash_pl_bu = Hashtbl.hash pl_bu in
        let key = List.assoc "bi" env_keys in
        if not (is_empty_string pl_bi) && (hash_pl_bi = key) then 
          map_add key pl_bi p
        else ();
        let key = List.assoc "bp" env_keys in
        if not (is_empty_string pl_bp) && (hash_pl_bp = key) then 
          map_add key pl_bp p
        else ();
        let key = List.assoc "de" env_keys in
        if not (is_empty_string pl_de) && (hash_pl_de = key) then 
          map_add key pl_de p
        else ();
        let key = List.assoc "bu" env_keys in
        if not (is_empty_string pl_bu) && (hash_pl_bu = key) then 
          map_add key pl_bu p
        else ();
        loop (i+1)
      }
  in loop 0;
  (* Parcours toutes les familles et ajoute dans la map les  *)
  (* individus en relation avec le lieu donnée par la clé k. *)
  let rec loop i =
    if i = nb_of_families base then ()
    else
      do {
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else 
          do {
            let pl_ma = get_marriage_place fam in
            let hash_pl_ma = Hashtbl.hash pl_ma in
            let key = List.assoc "ma" env_keys in
            if not (is_empty_string pl_ma) && (hash_pl_ma = key) then 
              do {
                let p = pget conf base (get_father fam) in
                map_add key pl_ma p;
                let p = pget conf base (get_mother fam) in
                map_add key pl_ma p
              }
            else ();
          };       
        loop (i+1) 
      }
  in loop 0;
  (* On retourne la liste des couples (lieu, persons list) *)
  let list = ref [] in
  PersMap.iter
    (fun hash (istr, pset) -> 
      list.val := [ (istr, PersSet.elements pset) :: list.val ] )
    pers_map.val ;
  list.val }
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


(* ************************************************************************** *)
(*  [Fonc] combine_by_ini : string -> (string * 'a * 'b) list -> 
                              (string * ('a * 'b) list) list                  *)
(** [Description] : Retourne une liste en recombinant toutes les chaines en
                    fonction de leur début de chaîne.
    [Args] :
      - ini  : les premières lettres de la chaîne
      - list : la liste composée des premières lettres de la chaîne, 'a, 'b
    [Retour] :
      - (string * ('a * 'b) list) list
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value combine_by_ini ini list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(k, s, cnt) :: list] -> do {
          let ini_k =
            if String.length k > String.length ini then
              String.sub k 0 (index_of_next_char k (String.length ini))
            else k ^ String.make (String.length ini + 1 - String.length k) '_'
          in
          for i = 0 to String.length ini_k - 1 do {
            if ini_k.[i] = ' ' then ini_k.[i] := '_' else ()
          };
          let new_list =
            if ini_k = "_" then new_list
            else
              match new_list with
              [ [] -> [(ini_k, [(s, cnt)])]
              | [(ini_k1, l) :: ll] ->
                  if ini_k1 = ini_k then [(ini_k1, [(s, cnt) :: l]) :: ll]
                  else [(ini_k, [(s, cnt)]); (ini_k1, l) :: ll] ]
          in
          loop new_list list
        } ]
  in
  List.fold_left (fun new_l (ini_k, l) -> [(ini_k, List.rev l) :: new_l]) []
    list
;


(* ************************************************************************** *)
(*  [Fonc] combine_by_ini : ('a * 'b * 'c) list -> ('a * ('b * 'c) list) list *)
(** [Description] : 
    [Args] :
      - list : la liste de triplets
    [Retour] :
      - ('a * ('b * 'c) list) list
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value combine list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(k, s, cnt) :: list] -> do {
          let new_list =
              match new_list with
              [ [] -> [(k, [(s, cnt)])]
              | [(ini_k1, l) :: ll] ->
                  if ini_k1 = k then [(ini_k1, [(s, cnt) :: l]) :: ll] 
                  else [(k, [(s, cnt)]); (ini_k1, l) :: ll] ]
          in
          loop new_list list
        } ]
  in
  List.fold_left (fun new_l (ini_k, l) -> [(ini_k, List.rev l) :: new_l]) []
    list
;


(* ************************************************************************** *)
(*  [Fonc] print_title : config -> base -> string -> int -> unit              *)
(** [Description] : Affiche le titre du dictionnaire des lieux en fonction
                    de la longueur de la liste
    [Args] :
      - conf : configuration
      - base : base
      - ini  : les premières lettres du lieu
      - len  : le nombre de lieu commençant par ini
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_title conf base ini len = do {
  Wserver.wprint "%s" (capitale (transl conf "book of places"));
  if ini = "" then
    Wserver.wprint " (%d %s)" len (transl conf "places")
  else do {
    Wserver.wprint " - ";
    Wserver.wprint 
      (fcapitale (ftransl conf "%d places starting with %s")) len ini }
};


(* ************************************************************************** *)
(*  [Fonc] print_place : config -> base -> (string * (string * int) list) list
                           -> int -> unit                                     *)
(** [Description] : Affiche le tableau des lieux en leur associant une clé
                    unique afin de pouvoir mettre un champs de modification.
    [Args] :
      - conf : configuration
      - base : base
      - list : liste de couple (lieu, (type (= bi, bp, de, bu, ma) * clé) list)
      - len  : la longueur de la liste
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_place conf base list len =
  let env_keys = 
    let list = ref [] in
    let _ = 
      List.map
        (fun key -> 
          match p_getenv conf.env key with
          [ Some s -> list.val := [ (key, int_of_string s) :: list.val ]
          | None -> () ] )
        [ "bi"; "bp"; "de"; "bu"; "ma" ] 
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
  in
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit à partir de la liste de (place * hash) la liste dont    *)
  (* le premier composant est les premières lettres du lieu.           *)
  (* Attention, il ne faut pas faire String.length ini + 1 parce qu'en *)
  (* utf8, il se peut que le caractère soit codé sur plusieurs octets. *)
  let list =
    List.map
      (fun (s, k) -> 
        let ini = String.sub s 0 (index_of_next_char s (String.length ini)) in
        (ini, s, k))
      list
  in
  (* Re-combine la liste en fonction des premières *)
  (* lettres afin de pouvoir poser des ancres.     *)
  let list = combine_by_ini ini list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun (ini, l) -> (Mutil.tr ' ' '_' ini, l)) list in
  do {
    let title _ = print_title conf base ini len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<p>%s</p>" (capitale (transl conf "help places")) ;
    (* Ancre en haut de page afin de naviguer plus facilement. *)
    tag "p" begin
      List.iter
        (fun (ini, _) ->
           stagn "a" "href=\"#%s\"" ini begin
             Wserver.wprint "%s" ini;
           end)
      list;
    end;
    Wserver.wprint 
      "<form method=\"post\" action=\"%s\">\n" conf.command;
    tag "ul" begin
      List.iter 
        (fun (ini_k, list) -> do {
          tag "li" begin
            stagn "a" "id=\"%s\"" ini_k begin
              Wserver.wprint "%s" ini_k;
            end;
            Wserver.wprint "<table>\n" ;
            List.iter
              (fun (s, k) -> 
                do {
                  let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
                  Wserver.wprint "<tr>"; 
                  if k <> env_keys then
                    do { 
                      Wserver.wprint "<td>%s</td>" s; 
                      let k =
                        List.fold_left 
                          (fun accu (k, i) -> 
                            accu ^ k ^ "=" ^ (string_of_int i) ^ ";")
                          "" k
                      in
                      Wserver.wprint 
                        "<td><a style=\"font-size:80%%\" href=\"%sm=MOD_P;%s;s=%s#mod\">(%s)</a></td>"
                        (commd conf) k (code_varenv ini)
                        (capitale (Util.transl_decline conf "modify" ""));
                    } 
                  else 
                    do { 
                      Wserver.wprint "<td><a name=\"mod\">&nbsp;</a>"; 
                      (* envoie les données de façon masquée *)
                      Util.hidden_env conf; 
                      List.iter
                        (fun (s,i) -> 
                          Wserver.wprint 
                            "<input type=\"hidden\" name=\"%s\" value=\"%d\"/>" s i
                        )
                        env_keys
                      ;
                      Wserver.wprint 
                        "<input type=\"hidden\" name=\"m\" value=\"MOD_P_OK\"/>" ;
                      Wserver.wprint 
                        "<input type=\"text\" name=\"place\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"place\" /></td>" 
                        (quote_escaped (no_html_tags (only_printable s))); 
                      Wserver.wprint "<td><input type=\"submit\" value=\"Ok\" /></td>" ; 
                    };
                  Wserver.wprint "</tr>\n"; 
                }
              )
              list;
            Wserver.wprint "</table>\n";
            Wserver.wprint "<br />\n";
          end; }
        ) 
        list;
    end;
    Wserver.wprint "</form>\n";
    Hutil.trailer conf ;
  }
;


(* ************************************************************************* *)
(*  [Fonc] print_short : config -> base -> (string * 'a) list -> int -> unit *)
(** [Description] : Si le nombre de lieux est trop grand, on affiche les
                    première lettre de chaque lieu en fonction des premières
                    lettres données par le paramètre s dans la configuration.
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des les lieux
      - len  : la longueur de la liste
    [Retour] :
      - unit
    [Rem] : Non xporté en clair hors de ce module.                           *)
(* ************************************************************************* *)
value print_short conf base list len =
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit la liste des string commençant par ini. *)
  (* Attention, il ne faut pas faire String.length     *)
  (* ini + 1 parce qu'en utf8, il se peut que le       *)
  (* caractère soit codé sur plusieurs octets.         *)
  let list =
    List.map
      (fun (s, _) -> String.sub s 0 (index_of_next_char s (String.length ini)))
      list
  in
  (* Fonction pour supprimer les doublons. *)
  let remove_dup list =
    StringSet.elements 
      (List.fold_right StringSet.add list StringSet.empty)
  in
  let list = remove_dup list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun p -> Mutil.tr ' ' '_' p) list in
  do {
    let title _ = print_title conf base ini len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    List.iter
      (fun s ->
        Wserver.wprint "<a href=\"%sm=MOD_P;s=%s\">%s</a> " 
          (commd conf) (code_varenv s) s
      )
      list;
    Hutil.trailer conf 
  }
;


value max_nb_places = 1000 ;


(* ********************************************************************* *)
(*  [Fonc] print_mod : config -> base -> unit                            *)
(** [Description] : Récupère la liste de tous les lieux de la base et en
                    fonction du nombre de résultats, fait un affichage
                    court ou un afficahge long.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
value print_mod conf base = 
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Astuce pour gérer les espaces. *)
  let ini = Mutil.tr '_' ' ' ini in
  let list = get_all_places conf base in
  let list = List.map (fun (istr, s, k) -> (sou base istr, s, k)) list in
  (* On tri la liste avant de la combiner *)
  (* sinon on n'élimine pas les doublons. *)
  let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
  (* On combine la liste parce qu'en gwc2, les *)
  (* lieux ont tous des adresses différentes.  *)
  let list = combine list in
  (* Fonction qui à une liste de lieux retourne la *)
  (* liste de tous les lieux commençant par ini.   *)
  let reduce l =
    (* fold_right pour conserver le tri précédent. *)
    List.fold_right
      (fun (place, k) acc -> 
        if Mutil.start_with ini place then
          [ (place, k) :: acc ]
        else acc
      )
      l []
  in
  let list = 
    if ini <> "" then reduce list
    else list
  in
  let len = List.length list in
  if len > max_nb_places then
    print_short conf base list len
  else print_place conf base list len
;


(* ********************************************************************** *)
(*  [Fonc] reduce_sub_list : config -> base -> string -> list -> unit     *)
(** [Description] : 
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value reduce_sub_list size list =
  let rec aux size cnt reduced_list list = 
    if cnt > size then reduced_list 
    else
      match list with
       [ [] -> reduced_list
       | [x :: l] -> aux size (cnt+1) [x::reduced_list] l ]
  in aux size 0 [] list
;


(* ********************************************************************** *)
(*  [Fonc] reduce_list : config -> base -> string -> list -> unit         *)
(** [Description] : 
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value reduce_list size list =
  let rec aux size cnt redu list =
    if cnt > size then redu 
    else
      match list with
       [ [] -> redu 
       | [(a, sl) :: l] ->
           if List.length sl > (size-cnt) then
             [(a, reduce_sub_list (size-cnt) sl) :: redu]
           else
             aux size (cnt+List.length sl) [(a,sl)::redu] l ]
  in aux size 0 [] list
;


(* ********************************************************************** *)
(*  [Fonc] update_partial_list : config -> base -> string -> list -> unit *)
(** [Description] : 
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value update_person_list conf base new_place list nb_pers max_updates = do {
  let list = 
    if nb_pers > max_updates then
      reduce_list max_updates list 
    else list
  in
  List.iter
    (fun (old_place, perl) ->
      if old_place <> new_place then do {
        (* mise à jour de toutes les personnes concernées par le nouveau lieu *)
        let place_istr = Gwdb.insert_string base new_place in 
        List.iter 
          (fun p -> do {
            let pl_bi = get_birth_place p in
            let s_bi = sou base pl_bi in 
            let pl_bp = get_baptism_place p in
            let s_bp = sou base pl_bp in 
            let pl_de = get_death_place p in
            let s_de = sou base pl_de in 
            let pl_bu = get_burial_place p in
            let s_bu = sou base pl_bu in 
            let birth_place = 
              if old_place = s_bi then place_istr
              else pl_bi
            in
            let baptism_place = 
              if old_place = s_bp then place_istr
              else pl_bp
            in
            let death_place = 
              if old_place = s_de then place_istr
              else pl_de
            in
            let burial_place = 
              if old_place = s_bu then place_istr
              else pl_bu
            in
            let np = {(gen_person_of_person p) with birth_place = birth_place; 
                     baptism_place = baptism_place; death_place = death_place; 
                     burial_place = burial_place} in
            patch_person base np.key_index np; 
            let fam = Array.to_list (get_family p) in
            List.iter
              (fun f ->
                let ifam = foi base f in
                let p_ma = get_marriage_place ifam in
                let s_ma = sou base p_ma in
                if old_place = s_ma then
                    let fam = {(gen_family_of_family ifam) with 
                               marriage_place = place_istr} 
                    in
                    patch_family base fam.fam_index fam 
                else ()
              )
              fam;
            (* On met aussi à jour l'historique. *)
            let fn = sou base (get_first_name p) in
            let sn = sou base (get_surname p) in
            let occ = get_occ p in
            let ip = get_key_index p in
            History.record conf base (fn, sn, occ, ip) "mp"; }
          )
          perl  }
      else () )
    list;
  Util.commit_patches conf base; }
;


(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)
(** [Description] : Met à jour toutes les personnes en relation avec
                    le lieu que l'on veut modifié, donné par le 
                    paramètre new_place
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value print_mod_ok conf base = do {
  let env_keys = 
    let list = ref [] in
    let _ = 
      List.map
        (fun key -> 
          match p_getenv conf.env key with
          [ Some s -> list.val := [ (key, int_of_string s) :: list.val ]
          | None -> () ] )
        [ "bi"; "bp"; "de"; "bu"; "ma" ] 
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
  in
  let new_place = 
    match p_getenv conf.env "place" with 
    [ Some s -> (no_html_tags (only_printable s))
    | None -> "" ] 
  in
  let list = get_person_places conf base in
  let list = List.map (fun (istr, perl) -> (sou base istr, perl)) list in
  let nb_pers = 
    List.fold_left
      (fun accu (_, perl) -> accu + List.length perl)
      0 list
  in
  let max_updates = 10000 in
  update_person_list conf base new_place list nb_pers max_updates;
  if nb_pers <> 0 then do {
    let title _ = 
      Wserver.wprint "%s" (capitale (transl conf "place modified")) 
    in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint 
      (fcapitale (ftransl conf "the modification impacted %d persons")) 
      (min nb_pers max_updates);
    if nb_pers > max_updates then do {
      Wserver.wprint "<p>" ;
      Wserver.wprint 
        "<form method=\"post\" action=\"%s\">\n" conf.command;
      Util.hidden_env conf;
      List.iter
        (fun (s,i) -> 
          Wserver.wprint 
            "<input type=\"hidden\" name=\"%s\" value=\"%d\"/>" s i )
         env_keys ;
      Wserver.wprint 
        "<input type=\"hidden\" name=\"m\" value=\"MOD_P_OK\"/>" ;
      Wserver.wprint 
        "<input type=\"hidden\" name=\"place\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"place\" />" 
        (quote_escaped (no_html_tags (only_printable new_place))) ;
      Wserver.wprint 
        "%s ? </a></p>" (capitale (transl conf "continue merging")) ;
      Wserver.wprint "<input type=\"submit\" value=\"Ok\" />" ; 
      Wserver.wprint "</form>\n";
      }
    else ();
    Wserver.wprint "<p><a href=\"%sm=MOD_P\">%s ?</a></p>" 
      (commd conf) (capitale (transl conf "new modification")) ;
    Hutil.trailer conf }
  else do {
    let title _ = 
      Wserver.wprint "%s" (capitale (transl conf "no modification"))
    in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<p><a href=\"%sm=MOD_P\">%s ?</a></p>" 
      (commd conf) (capitale (transl conf "new modification")) ;
    Hutil.trailer conf 
  }
};


