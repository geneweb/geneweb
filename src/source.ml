(* camlp5r ./pa_html.cmo *)
(* $Id: source.ml,v 0.01 2012-07-12 10:19:08 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

module SourceSet = Set.Make
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


(* ******************************************************************** *)
(*  [Fonc] get_all_sources : config -> base ->  SourceSet.elt list      *)
(** [Description] : Construit la liste de toutes les sources de la base 
                    et leur associe une clé unique.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * int) list : La liste sans doublon de toutes les 
          sources de la base avec leur clé.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value get_all_sources conf base = do {
  let sources = ref SourceSet.empty in
  (* Ajoute toutes les sources liés à un individu *)
  let rec loop i = 
    if i = nb_of_persons base then ()
    else 
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        let src_bi = get_birth_src p in
        let src_bp = get_baptism_src p in
        let src_de = get_death_src p in
        let src_bu = get_burial_src p in
        let src_p = get_psources p in
        if not (is_empty_string src_bi) then 
          sources.val := 
            SourceSet.add (src_bi, "bi", Hashtbl.hash src_bi) sources.val
        else ();
        if not (is_empty_string src_bp) then 
          sources.val := 
            SourceSet.add (src_bp, "bp", Hashtbl.hash src_bp) sources.val
        else ();
        if not (is_empty_string src_de) then 
          sources.val := 
            SourceSet.add (src_de, "de", Hashtbl.hash src_de) sources.val
        else ();
        if not (is_empty_string src_bu) then 
          sources.val := 
            SourceSet.add (src_bu, "bu", Hashtbl.hash src_bu) sources.val
        else ();
        if not (is_empty_string src_p) then 
          sources.val := 
            SourceSet.add (src_p, "p", Hashtbl.hash src_p) sources.val
        else ();
        loop (i+1)
      }
  in loop 0;
  (* Ajoute toutes les sources liés à une famille *)
  let rec loop i = 
    if i = nb_of_families base then ()
    else
      do {
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else do {
          let src_ma = get_marriage_src fam in
          if not (is_empty_string src_ma) then 
            sources.val := 
              SourceSet.add (src_ma, "ma", Hashtbl.hash src_ma) sources.val
          else (); 
          let src_f = get_fsources fam in
          if not (is_empty_string src_f) then 
            sources.val := 
              SourceSet.add (src_f, "f", Hashtbl.hash src_f) sources.val
          else (); } ;
        loop (i+1)
      }
  in loop 0;
  SourceSet.elements sources.val }
;


(* ************************************************************************** *)
(*  [Fonc] get_person_sources : config -> base -> 
                                 (Gwdb.istr * PersSet.elt list) list          *)
(** [Description] : Construit a partir de la clé d'une source, l'ensemble des
                    personnes en relation avec cette source.
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - (Gwdb.istr * person list) : retourne la liste des adresses des sources
          ainsi que la liste des personnes en relation avec cette source.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value get_person_sources conf base = do {
  let env_keys = 
    List.map
      (fun key -> 
        match p_getint conf.env key with
        [ Some hash -> (key, hash)
        | None -> (key, (-1)) ] )
      [ "bi"; "bp"; "de"; "bu"; "p"; "ma"; "f" ] 
  in
  let pers_map = ref PersMap.empty in
  (* Fonction d'ajout dans la map des personnes (PersMap).         *)
  (* k = key, istr = source, p = person.                           *)
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
  (* individus en relation avec la source donné par la clé k. *)
  let rec loop i =
    if i = nb_of_persons base then ()
    else 
      do {
        let p = pget conf base (Adef.iper_of_int i) in
        let src_bi = get_birth_src p in
        let hash_src_bi = Hashtbl.hash src_bi in
        let src_bp = get_baptism_src p in
        let hash_src_bp = Hashtbl.hash src_bp in
        let src_de = get_death_src p in
        let hash_src_de = Hashtbl.hash src_de in
        let src_bu = get_burial_src p in
        let hash_src_bu = Hashtbl.hash src_bu in
        let src_p = get_psources p in
        let hash_src_p = Hashtbl.hash src_p in
        let key = List.assoc "bi" env_keys in
        if not (is_empty_string src_bi) && (hash_src_bi = key) then 
          map_add key src_bi p
        else ();
        let key = List.assoc "bp" env_keys in
        if not (is_empty_string src_bp) && (hash_src_bp = key) then 
          map_add key src_bp p
        else ();
        let key = List.assoc "de" env_keys in
        if not (is_empty_string src_de) && (hash_src_de = key) then 
          map_add key src_de p
        else ();
        let key = List.assoc "bu" env_keys in
        if not (is_empty_string src_bu) && (hash_src_bu = key) then 
          map_add key src_bu p
        else ();
        let key = List.assoc "p" env_keys in
        if not (is_empty_string src_p) && (hash_src_p = key) then 
          map_add key src_p p
        else ();
        loop (i+1)
      }
  in loop 0;
  (* Parcours toutes les familles et ajoute dans la map les    *)
  (* individus en relation avec la source donnée par la clé k. *)
  let rec loop i =
    if i = nb_of_families base then ()
    else
      do {
        let fam = foi base (Adef.ifam_of_int i) in
        if is_deleted_family fam then ()
        else 
          do {
            let src_ma = get_marriage_src fam in
            let hash_src_ma = Hashtbl.hash src_ma in
            let src_f = get_fsources fam in
            let hash_src_f = Hashtbl.hash src_f in
            let key = List.assoc "ma" env_keys in
            if not (is_empty_string src_ma) && (hash_src_ma = key) then 
              do {
                let p = pget conf base (get_father fam) in
                map_add key src_ma p;
                let p = pget conf base (get_mother fam) in
                map_add key src_ma p
              }
            else ();
            let key = List.assoc "f" env_keys in
            if not (is_empty_string src_f) && (hash_src_f = key) then 
              do {
                let p = pget conf base (get_father fam) in
                map_add key src_f p;
                let p = pget conf base (get_mother fam) in
                map_add key src_f p
              }
            else ();
          };       
        loop (i+1) 
      }
  in loop 0;
  (* On retourne la liste des couples (source, persons list) *)
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
(** [Description] : Affiche le titre du dictionnaire des sources en fonction
                    de la longueur de la liste
    [Args] :
      - conf : configuration
      - base : base
      - ini  : les premières lettres de la source
      - len  : le nombre de sources commençant par ini
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_title conf base ini len = do {
  Wserver.wprint "%s" (capitale (transl conf "book of sources"));
  if ini = "" then
    Wserver.wprint " (%d %s)" len (transl conf "sources")
  else do {
    Wserver.wprint " - ";
    Wserver.wprint 
      (fcapitale (ftransl conf "%d sources starting with \"%s\"")) len ini }
};


(* ************************************************************************** *)
(*  [Fonc] print_src : config -> base -> (string * (string * int) list) list
                           -> int -> unit                                     *)
(** [Description] : Affiche le tableau des sources en leur associant une clé
                    unique afin de pouvoir mettre un champs de modification.
    [Args] :
      - conf : configuration
      - base : base
      - list : liste de couple (source, (type (= bi, bp, de, bu, p) * clé) list)
      - len  : la longueur de la liste
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
value print_src conf base list len =
  let env_keys = 
    let list = ref [] in
    let _ = 
      List.map
        (fun key -> 
          match p_getint conf.env key with
          [ Some hash -> list.val := [ (key, hash) :: list.val ]
          | None -> () ] )
        [ "bi"; "bp"; "de"; "bu"; "p"; "ma"; "f" ] 
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
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
  do {
    let title _ = print_title conf base (Mutil.tr '_' ' ' ini) len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "p" begin
      Wserver.wprint "%s" (capitale (transl conf "help sources")) ;
    end;
    (* Ancre en haut de page afin de naviguer plus facilement. *)
    tag "p" begin
      List.iter
        (fun (ini, _) ->
           stagn "a" "href=\"#%s\"" ini begin
             Wserver.wprint "%s" ini;
           end)
      list;
    end;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "ul" "class=mod_sources_ul" begin
        List.iter 
          (fun (ini_k, list) -> do {
            tag "li" begin
              stagn "a" "id=\"%s\"" ini_k begin
                Wserver.wprint "%s" ini_k;
            end;
            tag "table" "class=mod_sources_table" begin
              List.iter
                (fun (s, k) -> do {
                  let k = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) k in
                  tag "tr" begin
                    if k <> env_keys then do { 
                      tag "td" begin
                        Wserver.wprint "%s" (quote_escaped s);
                      end;
                      let k =
                        List.fold_left 
                          (fun accu (k, i) -> 
                            accu ^ k ^ "=" ^ (string_of_int i) ^ ";")
                          "" k
                      in
                      tag "td" "class=mod_sources_table_modify" begin
                        stag "a" "style=\"font-size:80%%\" href=\"%sm=MOD_SRC;%s;s=%s#mod\""
                          (commd conf) k (code_varenv ini)
                        begin
                          Wserver.wprint "%s" 
                            (capitale (Util.transl_decline conf "modify sources" ""));
                        end;
                      end; } 
                    else do { 
                      tag "td" begin
                        Wserver.wprint "<a name=\"mod\">&nbsp;</a>";
                        (* envoie les données de façon masquée *)
                        Util.hidden_env conf; 
                        List.iter
                          (fun (s,i) -> 
                            xtag "input" "type=\"hidden\" name=\"%s\" value=\"%d\"" s i )
                          env_keys ;
                        xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_SRC_OK\"" ;
                        xtag "input" "type=\"hidden\" name=\"s\" value=\"%s\"" ini;
                        xtag "input" "type=\"text\" name=\"src\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"src\"" 
                          (quote_escaped (only_printable s)) ;
                      end;
                      tag "td" begin
                        xtag "input" "type=\"submit\" value=\"Ok\"" ;
                      end; };
                  end; } )
                list;
              end; 
              xtag "br"; 
            end; } ) 
          list;
      end;
    end;
    Hutil.trailer conf ;
  }
;


(* ************************************************************************* *)
(*  [Fonc] print_short : config -> base -> (string * 'a) list -> int -> unit *)
(** [Description] : Si le nombre de sources est trop grand, on affiche les
                    première lettre de chaque source en fonction des premières
                    lettres données par le paramètre s dans la configuration.
    [Args] :
      - conf : configuration
      - base : base
      - list : la liste des sources
      - len  : la longueur de la liste
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value print_short conf base list len =
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  (* Construit la liste des string commençant par ini. *)
  (* Fonctionne différement du dictionnaire des lieux  *)
  (* parce que l'on peut avoir beaucoup de sources qui *)
  (* commencent par les mêmes lettres. On calcul alors *)
  (* à partir de quelle lettre de ini, les sources     *)
  (* sont différentes.                                 *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l len =
    (* Attention, il ne faut pas faire String.length     *)
    (* ini + 1 parce qu'en utf8, il se peut que le       *)
    (* caractère soit codé sur plusieurs octets.         *)
    let ini_list =
      List.map
        (fun (s, _) -> 
          if String.length s > len then
            String.sub s 0 (index_of_next_char s len)
          else s ^ String.make (len + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements 
        (List.fold_right StringSet.add list StringSet.empty)
    in
    (* Astuce pour gérer les espaces. *)
    let ini_list = List.map (fun p -> Mutil.tr ' ' '_' p) ini_list in
    let ini_list = remove_dup ini_list in
    (* Si la liste des ini n'a qu'un élément, on calcul on 'rang' d'après *)
    if List.length ini_list = 1 then build_ini list (len + 1)
    else ini_list
  in
  let ini_list = build_ini list (String.length ini) in
  do {
    let title _ = print_title conf base (Mutil.tr '_' ' ' ini) len in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "p" begin
      List.iter
        (fun s ->
          stagn "a" "href=\"%sm=MOD_SRC;s=%s\"" (commd conf) (code_varenv s) 
            begin
              Wserver.wprint "%s" s;
            end)
        ini_list;
    end;
    Hutil.trailer conf 
  }
;


value max_nb_sources = 1000 ;


(* ********************************************************************* *)
(*  [Fonc] print_mod : config -> base -> unit                            *)
(** [Description] : Récupère la liste de toutes les sources de la base et 
                    en fonction du nombre de résultats, fait un affichage
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
  let list = get_all_sources conf base in
  let list = List.map (fun (istr, s, k) -> (sou base istr, s, k)) list in
  (* On tri la liste avant de la combiner *)
  (* sinon on n'élimine pas les doublons. *)
  let list = 
    List.sort (fun (s1, _, _) (s2, _, _) -> Gutil.alphabetic_order s1 s2) list
  in
  (* On combine la liste parce qu'en gwc2, les  *)
  (* sources ont tous des adresses différentes. *)
  let list = combine list in
  (* Fonction qui à une liste de sources retourne la *)
  (* liste de tous les souces commençant par ini.   *)
  let reduce l =
    (* fold_right pour conserver le tri précédent. *)
    List.fold_right
      (fun (src, k) acc -> 
        let src_tmp =  Mutil.tr '_' ' ' src in
        if Mutil.start_with ini src_tmp || (src_tmp ^ " " = ini) then
          [ (src, k) :: acc ]
        else acc )
      l []
  in
  let list = 
    if ini <> "" then reduce list
    else list
  in
  let len = List.length list in
  if len > max_nb_sources then
    print_short conf base list len
  else print_src conf base list len
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


(* ********************************************************************** *)
(*  [Fonc] update_person_list : config -> base -> string -> list -> unit  *)
(** [Description] : 
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
value update_person_list conf base new_src list nb_pers max_updates = do {
  let list = 
    if nb_pers > max_updates then
      reduce_cpl_list max_updates list 
    else list
  in
  List.iter
    (fun (old_src, perl) -> do {
      (* mise à jour de toutes les personnes *)
      (* concernées par la nouvelle source.  *)
      let src_istr = Gwdb.insert_string base new_src in 
      List.iter 
        (fun p -> do {
          let src_bi = get_birth_src p in
          let s_bi = sou base src_bi in 
          let src_bp = get_baptism_src p in
          let s_bp = sou base src_bp in 
          let src_de = get_death_src p in
          let s_de = sou base src_de in 
          let src_bu = get_burial_src p in
          let s_bu = sou base src_bu in 
          let src_p = get_psources p in
          let s_p = sou base src_p in 
          let birth_src = 
            if old_src = s_bi then src_istr
            else src_bi
          in
          let baptism_src = 
            if old_src = s_bp then src_istr
            else src_bp
          in
          let death_src = 
            if old_src = s_de then src_istr
            else src_de
          in
          let burial_src = 
            if old_src = s_bu then src_istr
            else src_bu
          in
          let psources_src = 
            if old_src = s_p then src_istr
            else src_p
          in
          let np = {(gen_person_of_person p) with birth_src = birth_src; 
                   baptism_src = baptism_src; death_src = death_src; 
                   burial_src = burial_src; psources = psources_src} 
          in
          patch_person base np.key_index np; 
          let fam = Array.to_list (get_family p) in
          List.iter
            (fun f ->
              let ifam = foi base f in
              let src_ma = get_marriage_src ifam in
              let s_ma = sou base src_ma in
              let src_f = get_fsources ifam in
              let s_f = sou base src_f in
              let marriage_src =
                if old_src = s_ma then src_istr
                else src_ma
              in
              let fsources =
                if old_src = s_f then src_istr
                else src_f
              in
              let nfam = {(gen_family_of_family ifam) with 
                         marriage_src = marriage_src; fsources = fsources}
              in
              patch_family base nfam.fam_index nfam)
            fam;
          (* On met aussi à jour l'historique. *)
          let fn = sou base (get_first_name p) in
          let sn = sou base (get_surname p) in
          let occ = get_occ p in
          let ip = get_key_index p in
          History.record conf base (fn, sn, occ, ip) "cs"; } )
        perl } )
    list;
  Util.commit_patches conf base; 
  (* On appelle explicitement notify_change car la base est modifiée.  *)
  (* On fait cet appel à la fin de chaque mise à jour de la liste des  *)
  (* personnes, car si l'administrateur de la base ne modifie pas tous *)
  (* les évènements liés à cette source, on ne sera pas mis au courant *)
  (* que la base à été mise à jour.                                    *)
  History.notify_sources conf base "cs"; }
;


(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)
(** [Description] : Met à jour toutes les personnes en relation avec
                    la source que l'on veut modifié, donné par le 
                    paramètre new_src
    [Args] :
      - conf : configuration
      - base : base
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
value print_mod_ok conf base = do {
  let ini =
    match p_getenv conf.env "s" with
    [ Some s -> s
    | None -> "" ]
  in
  let env_keys = 
    let list = ref [] in
    let _ = 
      List.map
        (fun key -> 
          match p_getint conf.env key with
          [ Some hash -> list.val := [ (key, hash) :: list.val ]
          | None -> () ] )
        [ "bi"; "bp"; "de"; "bu"; "p"; "ma"; "f" ] 
    in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list.val
  in
  let new_src = 
    match p_getenv conf.env "src" with 
    [ Some s -> only_printable s
    | None -> "" ] 
  in
  let list = get_person_sources conf base in
  let list = List.map (fun (istr, perl) -> (sou base istr, perl)) list in
  let nb_pers = 
    List.fold_left
      (fun accu (_, perl) -> accu + List.length perl)
      0 list
  in
  let src_modified =
    List.for_all (fun (old_src, _) -> new_src <> old_src) list
  in
  (* Indication : 1000 fiches prend environ 1 seconde de traitement. *)
  (* Attention à ne pas mettre une limite trop grande (d'où le test) *)
  (* pour ne pas dépasser le time out du serveur.                    *)
  let max_updates = 
    match p_getint conf.base_env "max_nb_update" with
    [ Some n -> if n > 50000 then 5000 else n
    | _ -> 5000 ]
  in
  if nb_pers <> 0 && src_modified then do {
    update_person_list conf base new_src list nb_pers max_updates;
    let title _ = 
      Wserver.wprint "%s" (capitale (transl conf "src modified")) 
    in
    Hutil.header conf title;
    print_link_to_welcome conf True;
    tag "p" begin
      Wserver.wprint 
        (fcapitale (ftransl conf "the modification impacted %d persons")) 
        (min nb_pers max_updates);
    end;
    if nb_pers > max_updates then do {
      tag "form" "method=\"post\" action=\"%s\"" conf.command begin
        tag "p" begin
          Util.hidden_env conf;
          List.iter
            (fun (s,i) -> 
              xtag "input" "type=\"hidden\" name=\"%s\" value=\"%d\"" s i)
             env_keys ;
          xtag "input" "type=\"hidden\" name=\"m\" value=\"MOD_SRC_OK\"" ;
          xtag "input" "type=\"hidden\" name=\"src\" size=\"80\" maxlength=\"200\" value=\"%s\" id=\"src\"" 
            (quote_escaped (only_printable new_src));
          Wserver.wprint 
            "%s" (capitale (transl conf "continue correcting")) ;
          xtag "input" "type=\"submit\" value=\"Ok\"" ;
        end;
      end
      }
    else ();
    tag "p" begin
      stag "a" "href=\"%sm=MOD_SRC;s=%s\"" (commd conf) ini begin
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
      stag "a" "href=\"%sm=MOD_SRC;s=%s\"" (commd conf) ini begin
        Wserver.wprint "%s" (capitale (transl conf "new modification"));
      end;
    end;
    Hutil.trailer conf 
  }
};


