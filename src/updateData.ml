(* camlp5r ./pa_html.cmo *)
(* $Id: source.ml,v 0.01 2012-07-12 10:19:08 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open TemplAst;
open Util;

module DataSet = Set.Make
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

type fun_data_p_f =
  [ Person of person -> istr
  | Family of family -> istr ]
;


(* ******************************************************************** *)
(*  [Fonc] get_data : config -> (string * fun x -> istr) list * bool    *)
(** [Description] : Renvoie la liste des labels et fonctions pour
                    récupérer les données que l'on veut modifier.
    [Args] :
      - conf : configuration
    [Retour] :
      - (string * fun x -> istr) list * bool : La liste des données que
           l'on souhaite modifier ainsi qu'un boolean pour savoir s'il
           faut tester la famille.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value get_data conf =
  match p_getenv conf.env "data" with
  [ Some "occu" -> ([ ("occu", Person get_occupation) ], False)
  | Some "place" ->
      ([ ("bi", Person get_birth_place); ("bp", Person get_baptism_place);
         ("de", Person get_death_place); ("bu", Person get_burial_place);
         ("ma", Family get_marriage_place) ], True)
  | Some "src" ->
      ([ ("bi", Person get_birth_src); ("bp", Person get_baptism_src);
         ("de", Person get_death_src); ("bu", Person get_burial_src);
         ("p", Person get_psources);
         ("ma", Family get_marriage_src); ("f", Family get_fsources) ], True)
  | _ -> ([], False) ]
;


(* ******************************************************************** *)
(*  [Fonc] get_all_data : config -> base -> DataSet.elt list            *)
(** [Description] : Construit la liste de toutes les données de la base
                    que l'on veut modifier et leur associe une clé unique.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * int) list : La liste sans doublon de toutes les
          sources de la base avec leur clé.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value get_all_data conf base = do {
  let data_set = ref DataSet.empty in
  let (data, test_family) = get_data conf in
  (* Ajoute toutes les "data" liés à un individu *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        List.iter
          (fun (label, fun_data) ->
            match fun_data with
            [ Person fun_data ->
                let istr = fun_data p in
                if not (is_empty_string istr) then
                  data_set.val :=
                    DataSet.add (istr, label, Hashtbl.hash istr) data_set.val
                else ()
            | _ -> () ] )
          data;
        loop (i+1)
      }
  in loop 0;
  (* Ajoute toutes les "data" liés à une famille *)
  if test_family then
    let rec loop i =
      if i = nb_of_families base then ()
      else
        do {
          let fam = foi base (Adef.ifam_of_int i) in
          if is_deleted_family fam then ()
          else
            List.iter
              (fun (label, fun_data) ->
                match fun_data with
                [ Family fun_data ->
                    let istr = fun_data fam in
                    if not (is_empty_string istr) then
                      data_set.val :=
                        DataSet.add (istr, label, Hashtbl.hash istr) data_set.val
                    else ()
                | _ -> () ] )
              data;
          loop (i+1)
        }
    in loop 0
  else ();
  DataSet.elements data_set.val }
;


(* ************************************************************************** *)
(*  [Fonc] get_person_from_data :
             config -> base -> (Gwdb.istr * PersSet.elt list) list            *)
(** [Description] : Construit a partir de la clé d'une donnée, l'ensemble des
                    personnes en relation avec cette donnée.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * person list) : retourne la liste des adresses des données
          ainsi que la liste des personnes en relation avec cette donnée.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value get_person_from_data conf base = do {
  let (data, test_family) = get_data conf in
  let env_keys =
    let list = List.map fst data in
    List.map
      (fun key ->
        match p_getint conf.env key with
        [ Some hash -> (key, hash)
        | None -> (key, (-1)) ] )
      list
  in
  let pers_map = ref PersMap.empty in
  (* Fonction d'ajout dans la map des personnes (PersMap).         *)
  (* k = key, istr = data, p = person.                             *)
  (* A la clé k est associé le binding (istr, ensemble d'individu) *)
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
  (* Parcours tous les individus et ajoute dans la map les    *)
  (* individus en relation avec la "data" donné par la clé k. *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        List.iter
          (fun (label, fun_data) ->
            match fun_data with
            [ Person fun_data ->
                let istr = fun_data p in
                let hash_istr = Hashtbl.hash istr in
                let key =  List.assoc label env_keys in
                if not (is_empty_string istr) && (hash_istr = key) then
                  map_add key istr p
                else ()
            | _ -> () ] )
          data;
        loop (i+1)
      }
  in loop 0;
  (* Parcours toutes les familles et ajoute dans la map les    *)
  (* individus en relation avec la "data" donnée par la clé k. *)
  if test_family then
    let rec loop i =
      if i = nb_of_families base then ()
      else
        do {
          let fam = foi base (Adef.ifam_of_int i) in
          if is_deleted_family fam then ()
          else
            do {
              List.iter
              (fun (label, fun_data) ->
                match fun_data with
                [ Family fun_data ->
                    let istr = fun_data fam in
                    let hash_istr = Hashtbl.hash istr in
                    let key = List.assoc label env_keys in
                    if not (is_empty_string istr) && (hash_istr = key) then
                      do {
                        let p = pget conf base (get_father fam) in
                        map_add key istr p;
                        let p = pget conf base (get_mother fam) in
                        map_add key istr p
                      }
                  else ()
                | _ -> () ] )
              data;
            };
          loop (i+1)
        }
    in loop 0
  else ();
  (* On retourne la liste des couples ("data", persons list) *)
  let list = ref [] in
  PersMap.iter
    (fun hash (istr, pset) ->
      list.val := [ (istr, PersSet.elements pset) :: list.val ] )
    pers_map.val ;
  list.val }
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
          let ini_k = Bytes.unsafe_of_string ini_k in
          for i = 0 to Bytes.length ini_k - 1 do {
            if Bytes.get ini_k i = ' ' then Bytes.set ini_k i '_' else ()
          };
          let ini_k = Bytes.unsafe_to_string ini_k in
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
  List.fold_left
    (fun new_l (ini_k, l) -> [(ini_k, l) :: new_l])
    [] list
;


(* ************************************************************************** *)
(*  [Fonc] combine : ('a * 'b * 'c) list -> ('a * ('b * 'c) list) list        *)
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
  List.fold_left
    (fun new_l (ini_k, l) -> [(ini_k, l) :: new_l])
    [] list
;


(* ************************************************************************** *)
(*  [Fonc] translate_title : config -> string * string                       *)
(** [Description] : Affiche le titre du dictionnaire
    [Args] :
      - conf : configuration
    [Retour] :
      - string : le nom du dictionnaire
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value translate_title conf =
  let title =
    match p_getenv conf.env "data" with
    [ Some "occu" -> transl_nth conf "occupation/occupations" 1
    | Some "place" -> transl conf "places"
    | Some "src" -> transl_nth conf "source/sources" 1
    | _ -> "" ]
  in
  (Printf.sprintf (ftransl conf "book of %s") title, title)
;


(* ************************************************************************** *)
(*  [Fonc] print_title : config -> base -> string -> int -> unit              *)
(** [Description] : Affiche le titre du dictionnaire en fonction de data et
                    de la longueur de la liste
    [Args] :
      - conf : configuration
      - base : base
      - ini  : les premières lettres de la source
      - len  : le nombre de sources commençant par ini
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_title conf base ini len = do {
  let (book_of, title) = translate_title conf in
  Wserver.wprint "%s" (capitale book_of);
  if ini = "" then
    Wserver.wprint " (%d %s)" len title
  else do {
    Wserver.wprint " - ";
    Wserver.wprint
      (fcapitale (ftransl conf "%d %s starting with %s")) len title ini }
};


(* ************************************************************************** *)
(*  [Fonc] print_long : config -> base -> (string * (string * int) list) list
                           -> int -> unit                                     *)
(** [Description] : Affiche le tableau des "data" en leur associant une clé
                    unique afin de pouvoir mettre un champs de modification.
    [Args] :
      - conf : configuration
      - base : base
      - list : liste de couple (data, (type (= bi, occu ...) * clé) list)
      - len  : la longueur de la liste
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_long conf base list len =
  let env_keys =
    let list = ref [] in
    let keys = List.map fst (fst (get_data conf)) in
    let _ =
      List.map
        (fun key ->
          match p_getint conf.env key with
          [ Some hash -> list.val := [ (key, hash) :: list.val ]
          | None -> () ] )
        keys
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
  in
  let data =
    match p_getenv conf.env "data" with
    [ Some s -> s
    | None -> "" ]
  in
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit à partir de la liste de (src * hash) la liste dont      *)
  (* le premier composant est les premières lettres de la sources.     *)
  (* Attention, il ne faut pas faire String.length ini + 1 parce qu'en *)
  (* utf8, il se peut que le caractère soit codé sur plusieurs octets. *)
  let list =
    List.map
      (fun (s, k) ->
        let ini =
          if String.length s > String.length ini then
            String.sub s 0 (index_of_next_char s (String.length ini))
          else ini
        in (ini, s, k))
      list
  in
  (* Re-combine la liste en fonction des premières *)
  (* lettres afin de pouvoir poser des ancres.     *)
  let list = combine_by_ini ini list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun (ini, l) -> (Mutil.tr ' ' '_' ini, l)) list in
  let list =
    List.sort (fun (ini1, _) (ini2,_) -> Gutil.alphabetic_order ini1 ini2) list
  in
  do {
    let title _ = print_title conf base (Mutil.tr '_' ' ' ini) len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "div" "class=\"tips\"" begin
      tag "table" begin
        tag "tr" begin
          tag "td" begin
            Wserver.wprint "%s" (capitale (transl conf "help modify data")) ;
          end;
        end;
      end;
    end;
    xtag "br";
    (* Ancre en haut de page afin de naviguer plus facilement. *)
    tag "table" "class=\"display_search\"" begin
      stag "tr" begin
        List.iter
          (fun (ini, _) ->
             stag "td" begin
               stagn "a" "href=\"#%s\"" ini begin
                 Wserver.wprint "%s" (no_html_tags ini);
               end;
             end)
        list;
      end;
    end;
    xtag "br";
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "ul" begin
        List.iter
          (fun (ini_k, list) -> do {
            tag "li" begin
              stagn "a" "id=\"%s\"" ini_k begin
                Wserver.wprint "%s" (no_html_tags ini_k);
            end;
            tag "ul" "class=\"mod_data_ul\"" begin
              let list =
                List.sort
                  (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2)
                  list
              in
              List.iter
                (fun (s, k) -> do {
                  let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
                  tag "li" begin
                    if k <> env_keys then
                      let k =
                        List.fold_left
                          (fun accu (k, i) ->
                            accu ^ k ^ "=" ^ (string_of_int i) ^ ";")
                          "" k
                      in
                      stag "a" "href=\"%sm=MOD_DATA;data=%s;%s;s=%s#mod\""
                        (commd conf) data k (code_varenv ini)
                        begin
                          Wserver.wprint "%s" (quote_escaped s);
                        end
                    else
                      tag "table" "class=\"mod_data_table\"" begin
                        tag "tr" begin
                          tag "td" begin
                            Wserver.wprint "<a name=\"mod\">&nbsp;</a>";
                            (* envoie les données de façon masquée *)
                            Util.hidden_env conf;
                            List.iter
                              (fun (s,i) ->
                                xtag "input" "type=\"hidden\" name=\"%s\" value=\"%d\"" s i )
                              env_keys ;
                            xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_DATA_OK\"" ;
                            xtag "input" "type=\"hidden\" name=\"data\" value=\"%s\"" data;
                            xtag "input" "type=\"hidden\" name=\"s\" value=\"%s\"" ini;
                            xtag "input" "type=\"text\" name=\"nx_input\" size=\"80\" maxlength=\"%d\" value=\"%s\" id=\"nx_input\""
                              (if data = "src" then 300 else 200) (quote_escaped (only_printable s)) ;
                          end;
                          tag "td" begin
                            xtag "input" "type=\"submit\" value=\"Ok\"" ;
                          end;
                        end;
                      end;
                  end; } )
                list;
              end;
            end; } )
          list;
      end;
    end;
    Hutil.trailer conf ;
  }
;


(* ************************************************************************* *)
(*  [Fonc] print_short : config -> base -> (string * 'a) list -> int -> unit *)
(** [Description] : Si le nombre de "data" est trop grand, on affiche les
                    premières lettres de chaque "data".
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
      - len  : la longueur de la liste
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value print_short conf base list len =
  let data =
    match p_getenv conf.env "data" with
    [ Some s -> s
    | None -> "" ]
  in
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit la liste des string commençant par ini. *)
  (* Pour certaines données comme les sources, on peut *)
  (* avoir beaucoup de sources qui commencent par les  *)
  (* mêmes lettres. On calcul alors à partir de quelle *)
  (* lettre de ini, les sources sont différentes.      *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l len =
    (* Attention, il ne faut pas faire String.length     *)
    (* ini + 1 parce qu'en utf8, il se peut que le       *)
    (* caractère soit codé sur plusieurs octets.         *)
    let ini_list =
      List.rev_map
        (fun (s, _) ->
          if String.length s > len then
            String.sub s 0 (index_of_next_char s len)
          else s ^ String.make (len + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left
           (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    (* Astuce pour gérer les espaces. *)
    let ini_list = List.rev_map (fun p -> Mutil.tr ' ' '_' p) ini_list in
    let ini_list = remove_dup ini_list in
    (* Si la liste des ini n'a qu'un élément, on calcul on 'rang' d'après *)
    if List.length ini_list = 1 then build_ini list (len + 1)
    else List.sort Gutil.alphabetic_order ini_list
  in
  let ini_list = build_ini list (String.length ini) in
  do {
    let title _ = print_title conf base (Mutil.tr '_' ' ' ini) len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "%s :" (capitale (transl conf "select a letter"));
    tag "p" "class=\"list_ini\"" begin
      List.iter
        (fun s ->
          stagn "a" "href=\"%sm=MOD_DATA;data=%s;s=%s\""
            (commd conf) data (code_varenv s)
            begin
              Wserver.wprint "%s" (no_html_tags s);
            end)
        ini_list;
    end;
    Hutil.trailer conf
  }
;


value max_results = 1000 ;


(* ********************************************************************* *)
(*  [Fonc] print_mod : config -> base -> unit                            *)
(** [Description] : Récupère la liste de toutes les "données" de la base
                    et en fonction du nombre de résultats, fait un
                    affichage court ou un afficahge long.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
value print_mod_old conf base =
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Astuce pour gérer les espaces. *)
  let ini = Mutil.tr '_' ' ' ini in
  let list = get_all_data conf base in
  (* On fait un rev_map (tail-rec) parce que si le nombre de *)
  (* données est trop important, on casser la pile d'appels. *)
  let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
  (* On tri la liste avant de la combiner *)
  (* sinon on n'élimine pas les doublons. *)
  let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
  (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
  (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
  (* les sources dans un seul index pour de meilleures performances.   *)
  let list = combine list in
  (* Fonction qui à une liste de données retourne la *)
  (* liste de toutes les données commençant par ini. *)
  let reduce l =
    List.fold_left
      (fun acc (data, k) ->
        let data_tmp =  Mutil.tr '_' ' ' data in
        if Mutil.start_with ini data_tmp || (data_tmp ^ " " = ini) then
          [ (data, k) :: acc ]
        else acc )
      [] l
  in
  let list = if ini <> "" then reduce list else list in
  let len = List.length list in
  if len > max_results then print_short conf base list len
  else print_long conf base list len
;


(* ************************************************************************** *)
(*  [Fonc] reduce_cpl_list : int -> ('a, 'b list) list -> ('a, 'b list) list  *)
(** [Description] : Retourne la sous liste telle que la somme des longueurs
                    des ('b list) soit égale à size.
    [Args] :
      - size : la taille de la liste retournée
      - list : la liste originale
    [Retour] :
      - list : la nouvelle liste dont la somme des ('b list) est égale à size
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value reduce_cpl_list size list =
  let rec loop size cnt reduced_list list =
    if cnt >= size then reduced_list
    else
      match list with
       [ [] -> reduced_list
       | [(a, sl) :: l] ->
           if List.length sl >= (size - cnt) then
             [(a, Util.reduce_list (size - cnt) sl) :: reduced_list]
           else
             loop size (cnt + List.length sl) [(a,sl) :: reduced_list] l ]
  in loop size 0 [] list
;


(* ************************************************************************** *)
(*  [Fonc] update_person : conf -> base -> string -> string -> person ->
                             gen_person iper istr                             *)
(** [Description] : Met à jour le/les champ(s) de la personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - old  : l'ancien contenu
      - new_input : le nouveau contenu
      - p : person
    [Retour] :
      - gen_person iper istr : gen_person avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value update_person conf base old new_input p =
  match p_getenv conf.env "data" with
  [ Some "occu" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let occupation = get_occupation p in
      let s_occupation = sou base occupation in
      let occupation = if old = s_occupation then new_istr else occupation in
      { (gen_person_of_person p) with occupation = occupation }
  | Some "place" ->
      let new_istr =
        Gwdb.insert_string base (no_html_tags (only_printable new_input))
      in
      let pl_bi = get_birth_place p in let s_bi = sou base pl_bi in
      let pl_bp = get_baptism_place p in let s_bp = sou base pl_bp in
      let pl_de = get_death_place p in let s_de = sou base pl_de in
      let pl_bu = get_burial_place p in let s_bu = sou base pl_bu in
      let birth_place = if old = s_bi then new_istr else pl_bi in
      let baptism_place = if old = s_bp then new_istr else pl_bp in
      let death_place = if old = s_de then new_istr else pl_de in
      let burial_place = if old = s_bu then new_istr else pl_bu in
      { (gen_person_of_person p) with birth_place = birth_place;
        baptism_place = baptism_place; death_place = death_place;
        burial_place = burial_place }
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let src_bi = get_birth_src p in let s_bi = sou base src_bi in
      let src_bp = get_baptism_src p in let s_bp = sou base src_bp in
      let src_de = get_death_src p in let s_de = sou base src_de in
      let src_bu = get_burial_src p in let s_bu = sou base src_bu in
      let src_p = get_psources p in let s_p = sou base src_p in
      let birth_src = if old = s_bi then new_istr else src_bi in
      let baptism_src = if old = s_bp then new_istr else src_bp in
      let death_src = if old = s_de then new_istr else src_de in
      let burial_src = if old = s_bu then new_istr else src_bu in
      let psources_src = if old = s_p then new_istr else src_p in
      { (gen_person_of_person p) with birth_src = birth_src;
        baptism_src = baptism_src; death_src = death_src;
        burial_src = burial_src; psources = psources_src }
  | _ ->  gen_person_of_person p ]
;


(* ************************************************************************** *)
(*  [Fonc] update_family : conf -> base -> string -> string -> person ->
                             gen_family ifam istr                             *)
(** [Description] : Met à jour le/les champ(s) de la famille.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - old       : l'ancien contenu
      - new_input : le nouveau contenu
      - fam       : family
    [Retour] :
      - gen_family ifam istr : gen_family avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value update_family conf base old new_istr fam =
  match p_getenv conf.env "data" with
  [ Some "place" ->
      let new_istr =
        Gwdb.insert_string base (no_html_tags (only_printable new_istr))
      in
      let p_ma = get_marriage_place fam in let s_ma = sou base p_ma in
      let marriage_place = if old = s_ma then new_istr else p_ma in
      { (gen_family_of_family fam) with marriage_place = marriage_place }
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_istr) in
      let src_ma = get_marriage_src fam in let s_ma = sou base src_ma in
      let src_f = get_fsources fam in let s_f = sou base src_f in
      let marriage_src = if old = s_ma then new_istr else src_ma in
      let fsources = if old = s_f then new_istr else src_f in
      { (gen_family_of_family fam) with
        marriage_src = marriage_src; fsources = fsources }
  | _ -> gen_family_of_family fam ]
;


(* ********************************************************************** *)
(*  [Fonc] update_person_list :
             config -> base -> string -> (string * person) list -> int
               -> int -> unit                                             *)
(** [Description] :
    [Args] :
      - conf      : configuration
      - base      : base
      - new_input : le nouveau contenu
      - list      : la liste des (clé, person list)
      - nb_pers   : le nombre de personnes concernées par la mise à jour
      - max_updates = le nombre maximum de persons que l'on met à jour
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value update_person_list conf base new_input list nb_pers max_updates = do {
  let (_, test_family) = get_data conf in
  let action =
    match p_getenv conf.env "data" with
    [ Some "occu" -> "co"
    | Some "place" -> "cp"
    | Some "src" -> "cs"
    | _ -> "" ]
  in
  let list =
    if nb_pers > max_updates then reduce_cpl_list max_updates list else list
  in
  List.iter
    (fun (old, perl) -> do {
      (* Mise à jour de toutes les personnes concernées. *)
      List.iter
        (fun p -> do {
          let np = update_person conf base old new_input p in
          patch_person base np.key_index np;
          if test_family then
            let fam = Array.to_list (get_family p) in
            List.iter
              (fun ifam ->
                let fam = foi base ifam in
                let nfam = update_family conf base old new_input fam in
                patch_family base nfam.fam_index nfam)
              fam
          else ();
          (* On met aussi à jour l'historique. *)
          let changed = U_Multi (Util.string_gen_person base np) in
          History.record conf base changed action; } )
        perl } )
    list;
  Util.commit_patches conf base;
  (* On appelle explicitement notify_change car la base est modifiée.  *)
  (* On fait cet appel à la fin de chaque mise à jour de la liste des  *)
  (* personnes, car si l'administrateur de la base ne modifie pas tous *)
  (* les évènements liés à cette donnée, on ne sera pas mis au courant *)
  (* que la base à été mise à jour.                                    *)
  History.notify conf base action; }
;


(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)
(** [Description] : Met à jour toutes les personnes en relation avec
                    la donnée que l'on veut modifié.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value print_mod_ok conf base = do {
  let data =
    match p_getenv conf.env "data" with
    [ Some s -> s
    | None -> "" ]
  in
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  let env_keys =
    let list = ref [] in
    let keys = List.map fst (fst (get_data conf)) in
    let _ =
      List.map
        (fun key ->
          match p_getint conf.env key with
          [ Some hash -> list.val := [ (key, hash) :: list.val ]
          | None -> () ] )
        keys
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
  in
  let new_input =
    match p_getenv conf.env "nx_input" with
    [ Some s -> only_printable s
    | None -> "" ]
  in
  let list = get_person_from_data conf base in
  let list = List.map (fun (istr, perl) -> (sou base istr, perl)) list in
  let nb_pers =
    List.fold_left
      (fun accu (_, perl) -> accu + List.length perl)
      0 list
  in
  let data_modified = List.for_all (fun (old, _) -> new_input <> old) list in
  (* Indication : 1000 fiches prend environ 1 seconde de traitement. *)
  (* Attention à ne pas mettre une limite trop grande (d'où le test) *)
  (* pour ne pas dépasser le time out du serveur.                    *)
  let max_updates =
    match p_getint conf.base_env "max_nb_update" with
    [ Some n -> if n > 50000 then 5000 else n
    | _ -> 5000 ]
  in
  if nb_pers <> 0 && data_modified then do {
    update_person_list conf base new_input list nb_pers max_updates;
    let title _ =
      Wserver.wprint "%s" (capitale (transl conf "modification successful"))
    in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "p" begin
      (* En attendant mieux ... *)
      Wserver.wprint "%s: %d "
        (capitale (transl conf "modification successful"))
        (min nb_pers max_updates);
      if p_getenv conf.base_env "history" = Some "yes" then
        stag "a" "href=\"%sm=HIST;k=20\"" (commd conf) begin
          Wserver.wprint "%s."
            (transl_nth conf "modification/modifications"
               (if nb_pers > 1 then 1 else 0));
        end
      else
        Wserver.wprint "%s."
          (transl_nth conf "modification/modifications"
             (if nb_pers > 1 then 1 else 0));
    end;
    if nb_pers > max_updates then do {
      tag "form" "method=\"post\" action=\"%s\"" conf.command begin
        tag "p" begin
          Util.hidden_env conf;
          List.iter
            (fun (s,i) ->
              xtag "input" "type=\"hidden\" name=\"%s\" value=\"%d\"" s i)
             env_keys ;
          xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_DATA_OK\"" ;
          xtag "input" "type=\"hidden\" name=\"data\" value=\"%s\"" data;
          xtag "input" "type=\"hidden\" name=\"s\" value=\"%s\"" ini;
          xtag "input" "type=\"hidden\" name=\"nx_input\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"data\""
            (quote_escaped (only_printable new_input));
          Wserver.wprint
            "%s" (capitale (transl conf "continue correcting")) ;
          xtag "input" "type=\"submit\" value=\"Ok\"" ;
        end;
      end
      }
    else ();
    tag "p" begin
      stag "a" "href=\"%sm=MOD_DATA;data=%s;s=%s\"" (commd conf) data ini begin
        Wserver.wprint "%s" (capitale (transl conf "new modification"));
      end;
    end;
    Hutil.trailer conf }
  else do {
    let title _ =
      Wserver.wprint "%s" (capitale (transl conf "no modification"))
    in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "p" begin
      stag "a" "href=\"%sm=MOD_DATA;data=%s;s=%s\"" (commd conf) data ini begin
        Wserver.wprint "%s" (capitale (transl conf "new modification"));
      end;
    end;
    Hutil.trailer conf
  }
};


(**/**) (* template *)


(* ********************************************************************* *)
(*  [Fonc] build_list : config -> base ->
                          (string * (string * int) list) list            *)
(** [Description] : Récupère la liste de toutes les "données" de la base.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] : Retourne la liste des données
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
value build_list conf base =
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Astuce pour gérer les espaces. *)
  let ini = Mutil.tr '_' ' ' ini in
  let list = get_all_data conf base in
  (* ! rev_map  = tail-rec ! *)
  let list = List.rev_map (fun (istr, s, k) -> (sou base istr, s, k)) list in
  (* On tri la liste avant de la combiner *)
  (* sinon on n'élimine pas les doublons. *)
  let list = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) list in
  (* On combine la liste parce qu'en gwc2, les données peuvent être à  *)
  (* des adresses différentes. NB: on pourrait rassembler les lieux et *)
  (* les sources dans un seul index pour de meilleures performances.   *)
  let list = combine list in
  (* Fonction qui à une liste de données retourne la *)
  (* liste de toutes les données commençant par ini. *)
  let reduce l =
    List.fold_left
      (fun acc (data, k) ->
        let data_tmp =  Mutil.tr '_' ' ' data in
        if Mutil.start_with ini data_tmp || (data_tmp ^ " " = ini) then
          [ (data, k) :: acc ]
        else acc )
      [] l
  in
  if ini <> "" then reduce list else list
;


(* ************************************************************************* *)
(*  [Fonc] build_list_short : config -> base -> (string * 'a) list
                                -> string list                               *)
(** [Description] :
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value build_list_short conf base list =
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit la liste des string commençant par ini. *)
  (* Pour certaines données comme les sources, on peut *)
  (* avoir beaucoup de sources qui commencent par les  *)
  (* mêmes lettres. On calcul alors à partir de quelle *)
  (* lettre de ini, les sources sont différentes.      *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l len =
    (* Attention, il ne faut pas faire String.length     *)
    (* ini + 1 parce qu'en utf8, il se peut que le       *)
    (* caractère soit codé sur plusieurs octets.         *)
    let ini_list =
      List.rev_map
        (fun (s, _) ->
          if String.length s > len then
            String.sub s 0 (index_of_next_char s len)
          else s ^ String.make (len + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left
           (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    (* Astuce pour gérer les espaces. *)
    let ini_list = List.rev_map (fun p -> Mutil.tr ' ' '_' p) ini_list in
    let ini_list = remove_dup ini_list in
    (* Si la liste des ini n'a qu'un élément, on calcul on 'rang' d'après *)
    if List.length ini_list = 1 then build_ini list (len + 1)
    else List.sort Gutil.alphabetic_order ini_list
  in
  build_ini list (String.length ini)
;


(* ************************************************************************* *)
(*  [Fonc] build_list_long : config -> base -> (string * 'a) list ->
                               (string * (string * 'a) list) list            *)
(** [Description] :
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des "data"
    [Retour] : La liste des couples (initiale de "data", "data")
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value build_list_long conf base list =
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit à partir de la liste de (src * hash) la liste dont      *)
  (* le premier composant est les premières lettres de la sources.     *)
  (* Attention, il ne faut pas faire String.length ini + 1 parce qu'en *)
  (* utf8, il se peut que le caractère soit codé sur plusieurs octets. *)
  let list =
    List.map
      (fun (s, k) ->
        let ini =
          if String.length s > String.length ini then
            String.sub s 0 (index_of_next_char s (String.length ini))
          else ini
        in (ini, s, k))
      list
  in
  (* Re-combine la liste en fonction des premières *)
  (* lettres afin de pouvoir poser des ancres.     *)
  let list = combine_by_ini ini list in
  (* Astuce pour gérer les espaces. *)
  let list = List.map (fun (ini, l) -> (Mutil.tr ' ' '_' ini, l)) list in
  List.sort (fun (ini1, _) (ini2,_) -> Gutil.alphabetic_order ini1 ini2) list
;


type env 'a =
  [ Vlist_data of list (string * list (string * int))
  | Vlist_ini of list string
  | Vlist_value of list (string * list (string * int))
  | Venv_keys of list (string * int)
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;
value bool_val x = VVbool x;
value str_val x = VVstring x;


value rec eval_var conf base env xx loc sl =
  try eval_simple_var conf base env xx sl with
  [ Not_found -> eval_compound_var conf base env xx sl ]
and eval_simple_var conf base env xx =
  fun
  [ [s] ->
      try bool_val (eval_simple_bool_var conf base env xx s) with
      [ Not_found -> str_val (eval_simple_str_var conf base env xx s) ]
  | _ -> raise Not_found ]
and eval_simple_bool_var conf base env xx =
  fun
  [ "is_modified" ->
      let k =
        match get_env "keys" env with
        [ Venv_keys k -> k
        | _ -> [] ]
      in
      let env_keys =
        match get_env "env_keys" env with
        [ Venv_keys env_keys -> env_keys
        | _ -> [] ]
      in
      k = env_keys
  | _ -> raise Not_found ]
and eval_simple_str_var conf base env xx =
  fun
  [ "entry_ini" -> eval_string_env "entry_ini" env
  | "entry_value" -> eval_string_env "entry_value" env
  | "ini" -> eval_string_env "ini" env
  | "keys" ->
      let k =
        match get_env "keys" env with
        [ Venv_keys k -> k
        | _ -> [] ]
      in
      List.fold_left
        (fun accu (k, i) ->
          accu ^ k ^ "=" ^ (string_of_int i) ^ ";")
        "" k
  | "key_name" -> eval_string_env "key_name" env
  | "key_value" -> eval_int_env "key_value" env
  | "nb_results" ->
      match get_env "list" env with
      [ Vlist_data l -> string_of_int (List.length l)
      | _ -> "0" ]
  | "title" ->
      let len =
        match get_env "list" env with
        [ Vlist_data l -> List.length l
        | _ -> 0 ]
      in
      let ini =
        match p_getenv conf.env "s" with
        [ Some s -> s
        | None -> "" ]
      in
      let (book_of, title) = translate_title conf in
      let result =
        if ini = "" then Printf.sprintf " (%d %s)" len title
        else
          " - " ^
            Printf.sprintf
              (ftransl conf "%d %s starting with %s") len title ini
      in
      (capitale book_of) ^ result
  | _ -> raise Not_found ]
and eval_compound_var conf base env xx sl =
  let rec loop =
    fun
    [ [s] -> eval_simple_str_var conf base env xx s
    | ["evar"; s] ->
        match p_getenv conf.env s with
        [ Some s -> s
        | None -> "" ]
    | ["encode" :: sl] -> code_varenv (loop sl)
    | ["escape" :: sl] -> quote_escaped (loop sl)
    | ["html_encode" :: sl] -> no_html_tags (loop sl)
    | ["printable" :: sl] -> only_printable (loop sl)
    | _ -> raise Not_found ]
  in
  str_val (loop sl)
and eval_string_env s env =
  match get_env s env with
  [ Vstring s -> s
  | _ -> raise Not_found ]
and eval_int_env s env =
  match get_env s env with
  [ Vint i -> string_of_int i
  | _ -> raise Not_found ]
;

value print_foreach conf base print_ast eval_expr =
  let rec print_foreach env xx loc s sl el al =
    match [s :: sl] with
    [ ["initial"] -> print_foreach_initial env xx el al
    | ["entry"] -> print_foreach_entry env xx el al
    | ["value"] -> print_foreach_value env xx el al
    | ["env_keys"] -> print_foreach_env_keys env xx el al
    | _ -> raise Not_found ]
  and print_foreach_entry env xx el al =
    let env_keys =
      let keys = List.map fst (fst (get_data conf)) in
      let list =
        List.fold_left
          (fun accu key ->
            match p_getint conf.env key with
            [ Some hash -> [(key, hash) :: accu]
            | None -> accu ] )
          [] keys
      in
      List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list
    in
    let list =
      match get_env "list" env with
      [ Vlist_data l -> l
      | _ -> [] ]
    in
    let list = build_list_long conf base list in
    let env = [("env_keys", Venv_keys env_keys) :: env] in
    loop list where rec loop =
      fun
      [ [(ini_k, list_v) :: l] ->
          let env =
            [("entry_ini", Vstring ini_k);
             ("list_value", Vlist_value list_v) :: env]
          in
          do { List.iter (print_ast env xx) al; loop l }
      | [] -> () ]
  and print_foreach_value env xx el al =
    let list =
      match get_env "list_value" env with
      [ Vlist_value l ->
          List.sort (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2) l
      | _ -> [] ]
    in
    loop list where rec loop =
      fun
      [ [(s, k) :: l] ->
          let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
          let env =
            [("entry_value", Vstring s); ("keys", Venv_keys k) :: env]
          in
          do { List.iter (print_ast env xx) al; loop l }
      | [] -> () ]
  and print_foreach_initial env xx el al =
    let list =
      match get_env "list" env with
      [ Vlist_data l -> l
      | _ -> [] ]
    in
    let ini_list = build_list_short conf base list in
    loop ini_list where rec loop =
      fun
      [ [ini :: l] ->
          let env = [("ini", Vstring ini) :: env] in
          do { List.iter (print_ast env xx) al; loop l }
      | [] -> () ]
  and print_foreach_env_keys env xx el al =
    let env_keys =
      match get_env "env_keys" env with
      [ Venv_keys env_keys -> env_keys
      | _ -> [] ]
    in
    loop env_keys where rec loop =
      fun
      [ [(s, i) :: l] ->
          let env = [("key_name", Vstring s); ("key_value", Vint i) :: env] in
          do { List.iter (print_ast env xx) al; loop l }
      | [] -> () ]
  in
  print_foreach
;

value print_mod conf base =
  (* Pas forcément nécessaire mais bon ... On pourra faire le ménage après. *)
  if p_getenv conf.env "old" = Some "on" then print_mod_old conf base
  else
    match p_getenv conf.env "data" with
    [ Some ("place" | "src" | "occu") ->
        let list = build_list conf base in
        let env = [("list", Vlist_data list)] in
        Hutil.interp conf base "upddata"
          {Templ.eval_var = eval_var conf base;
           Templ.eval_transl _ = Templ.eval_transl conf;
           Templ.eval_predefined_apply _ = raise Not_found;
           Templ.get_vother = get_vother; Templ.set_vother = set_vother;
           Templ.print_foreach = print_foreach conf base}
          env ()
    | _ ->
        Hutil.interp conf base "upddatamenu"
          {Templ.eval_var _ = raise Not_found;
           Templ.eval_transl _ = Templ.eval_transl conf;
           Templ.eval_predefined_apply _ = raise Not_found;
           Templ.get_vother = get_vother; Templ.set_vother = set_vother;
           Templ.print_foreach _ = raise Not_found}
          [] () ]
;
