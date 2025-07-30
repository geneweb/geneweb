(* lib/relationMatrix.ml *)
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa
module IperSet = Set.Make (Driver.Iper)

(* Structure minimale pour une cellule *)
type cell_data = { paths : Relation.path_f list; total : Sosa.t; coeff : float }

(* Type variant pour les différents modes de liens R et RL *)
type relationship_link_mode =
  | MatrixLink of Driver.person * Driver.person
  (* Pour la matrice : deux pers. i1/i2 *)
  | AncestorLink of Driver.person * Driver.person * Driver.person * int * int
(* Pour la popup : ancêtre i + 2 pers. i1/i2 + degrés l1/l2 *)

(* Type pour la matrice triangulaire *)
type 'a triangular_matrix = 'a option array

(* Gestion de la matrice triangulaire *)
let create_triangular_matrix n =
  let size = n * (n - 1) / 2 in
  Array.make size None

let rec triangular_index i j n =
  if i = j then raise (Invalid_argument "diagonal access")
  else if i > j then triangular_index j i n
  else (j * (j - 1) / 2) + i

let triangular_get t i j n = t.(triangular_index i j n)
let triangular_set t i j n v = t.(triangular_index i j n) <- v

(* Trouver le chemin le plus court dans une liste de chemins *)
let find_shortest_path paths =
  match paths with
  | [] -> None
  | _ ->
      let sorted =
        List.sort
          (fun p1 p2 ->
            let d1 = p1.Relation.l1 + p1.Relation.l2 in
            let d2 = p2.Relation.l1 + p2.Relation.l2 in
            if d1 = d2 then compare p1.Relation.l1 p2.Relation.l1
            else compare d1 d2)
          paths
      in
      Some (List.hd sorted)

(* Trouver le chemin le plus court “inversé” (len2/len1 au lieu de len1/len2) *)
let find_best_inverted_path paths =
  let inverted_paths =
    List.map
      (fun path ->
        { path with Relation.l1 = path.Relation.l2; l2 = path.Relation.l1 })
      paths
  in
  find_shortest_path inverted_paths

(* Calculer les données d'une cellule *)
let compute_cell_data conf base tstab p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  match Relation.compute_simple_relationship conf base tstab ip1 ip2 with
  | None -> None
  | Some (paths, total, coeff, _) -> Some { paths; total; coeff }

(* Fonction utilitaire pour obtenir le nom simple d'une personne *)
let get_person_simple_name base person =
  let first_name = Driver.sou base (Driver.get_first_name person) in
  let surname = Driver.sou base (Driver.get_surname person) in
  Printf.sprintf "%s %s" first_name surname

(* Fonction générique pour créer des liens de parenté *)
let encoded_key name = (Mutil.encode (Name.lower name) :> string)

let make_relationship_link conf base mode =
  let params = ref [] in
  let person_list, mode_str, extra_params =
    match mode with
    | MatrixLink (p1, p2) -> ([ p1; p2 ], "R", [])
    | AncestorLink (ancestor, p1, p2, l1, l2) ->
        ( [ ancestor; p1; p2 ],
          "RL",
          [ ("l1", string_of_int l1); ("l2", string_of_int l2) ] )
  in
  params := [ ("m", mode_str) ];
  List.iteri
    (fun idx person ->
      let i_param =
        match mode with
        | MatrixLink _ -> "i" ^ string_of_int (idx + 1)
        | AncestorLink _ -> (
            match idx with
            | 0 -> "i"
            | 1 -> "i1"
            | 2 -> "i2"
            | _ -> "i" ^ string_of_int idx (* au cas où *))
      in
      let accessible =
        Util.accessible_by_key conf base person
          (Driver.p_first_name base person)
          (Driver.p_surname base person)
      in
      if accessible then
        let p = Driver.p_first_name base person in
        let n = Driver.p_surname base person in
        let oc = Driver.get_occ person in
        let num =
          match mode with
          | MatrixLink _ -> string_of_int (idx + 1)
          | AncestorLink _ -> (
              match idx with
              | 0 -> ""
              | 1 -> "1"
              | 2 -> "2"
              | _ -> string_of_int idx)
        in
        params :=
          !params
          (* Ajouter à la fin pour garder l'ordre *)
          @ [ ("p" ^ num, encoded_key p); ("n" ^ num, encoded_key n) ]
          @ if oc = 0 then [] else [ ("oc" ^ num, string_of_int oc) ]
      else
        params :=
          !params
          @ [ (i_param, Driver.Iper.to_string (Driver.get_iper person)) ])
    person_list;
  params := !params @ extra_params;
  let param_str =
    String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) !params)
  in
  (Util.commd conf :> string) ^ param_str

(* Formatter le pourcentage pour l'affichage dans la cellule *)
let format_percentage_for_cell conf coefficient =
  let percentage = coefficient *. 100.0 in
  let abs_percentage = abs_float percentage in
  let formatted_number =
    if abs_percentage >= 1.0 then
      let rounded = Float.round (percentage *. 100.0) /. 100.0 in
      Util.string_of_decimal_num conf rounded
    else if abs_percentage >= 0.001 then
      let rounded = Float.round (percentage *. 1000.0) /. 1000.0 in
      Util.string_of_decimal_num conf rounded
    else ""
  in
  if formatted_number = "" then "" else formatted_number ^ "&nbsp;%"

(* Créer les données inversées pour la cellule symétrique *)
let create_inverted_cell_data cell =
  match find_best_inverted_path cell.paths with
  | None -> None
  | Some _ ->
      let inverted_paths =
        List.map
          (fun path ->
            { path with Relation.l1 = path.Relation.l2; l2 = path.Relation.l1 })
          cell.paths
      in
      Some { cell with paths = inverted_paths }

(* Grouper les ancêtres par couples en utilisant les ifams *)
let group_ancestors_by_couples (anc_list : Relation.anc_f list) =
  (* Créer une table des familles pour identifier les couples *)
  let fam_groups = Hashtbl.create 17 in
  List.iter
    (fun anc ->
      List.iter
        (fun ifam ->
          let members =
            try Hashtbl.find fam_groups ifam with Not_found -> []
          in
          if not (List.mem anc.Relation.p members) then
            Hashtbl.replace fam_groups ifam (anc.Relation.p :: members))
        anc.Relation.f)
    anc_list;
  (* Créer une table des compteurs *)
  let count_map = Hashtbl.create 17 in
  List.iter
    (fun anc -> Hashtbl.add count_map anc.Relation.p anc.Relation.c)
    anc_list;
  (* Construire la liste des items (couples ou individus seuls) *)
  let used = Hashtbl.create 17 in
  let items = ref [] in
  Hashtbl.iter
    (fun ifam members ->
      match members with
      | [ a; b ]
        when a <> b && (not (Hashtbl.mem used a)) && not (Hashtbl.mem used b) ->
          let ca = try Hashtbl.find count_map a with Not_found -> 1 in
          let cb = try Hashtbl.find count_map b with Not_found -> 1 in
          items := `Couple (a, ca, b, cb, ifam) :: !items;
          Hashtbl.add used a ();
          Hashtbl.add used b ()
      | _ -> ())
    fam_groups;
  (* Ajouter les individus non utilisés *)
  List.iter
    (fun anc ->
      if not (Hashtbl.mem used anc.Relation.p) then (
        items := `Unique (anc.Relation.p, anc.Relation.c) :: !items;
        Hashtbl.add used anc.Relation.p ()))
    anc_list;
  List.rev !items

type ancestor_item =
  [ `Couple of Driver.iper * int * Driver.iper * int * Driver.ifam
  | `Unique of Driver.iper * int ]

(* Regrouper par ifam pour trouver les partenaires potentiels *)
let format_ancestors_with_couples base anc_list =
  let by_ifam = Hashtbl.create 17 in
  List.iter
    (fun anc ->
      List.iter
        (fun ifam ->
          let members = try Hashtbl.find by_ifam ifam with Not_found -> [] in
          if not (List.mem anc members) then
            Hashtbl.replace by_ifam ifam (anc :: members))
        anc.Relation.f)
    anc_list;
  let processed = Hashtbl.create 17 in
  let groups = ref [] in
  List.iter
    (fun anc ->
      if not (Hashtbl.mem processed anc.Relation.p) then (
        let partners = ref [] in
        List.iter
          (fun ifam ->
            match try Hashtbl.find by_ifam ifam with Not_found -> [] with
            | members ->
                List.iter
                  (fun partner ->
                    if
                      partner.Relation.p <> anc.Relation.p
                      && (not (Hashtbl.mem processed partner.Relation.p))
                      && not (List.mem partner !partners)
                    then partners := partner :: !partners)
                  members)
          anc.Relation.f;
        Hashtbl.add processed anc.Relation.p ();
        List.iter (fun p -> Hashtbl.add processed p.Relation.p ()) !partners;
        groups := (anc, !partners) :: !groups))
    anc_list;
  let format_group (main, partners) =
    let main_p = Driver.poi base main.Relation.p in
    let main_name = get_person_simple_name base main_p in
    let main_str = Printf.sprintf "%s [%d]" main_name main.Relation.c in
    match partners with
    | [] -> main_str
    | _ ->
        let partner_strs =
          List.mapi
            (fun idx partner ->
              let p = Driver.poi base partner.Relation.p in
              let name = get_person_simple_name base p in
              let sep = if idx = 0 then " & " else " && " in
              Printf.sprintf "%s%s [%d]" sep name partner.Relation.c)
            partners
        in
        main_str ^ String.concat "" partner_strs
  in
  String.concat ", " (List.map format_group (List.rev !groups))

(* Générer le texte pour les tooltips *)
let make_tooltip_text conf base p1 p2 shortest_path total rel_coeff =
  let p1_name = get_person_simple_name base p1 in
  let p2_name = get_person_simple_name base p2 in
  let colon = Util.transl conf ":" in
  let total_links =
    if Sosa.eq total Sosa.zero then 0
    else
      match int_of_string (Sosa.to_string total) with
      | n when n >= 0 -> n
      | _ -> 0
  in
  let link_word =
    Util.transl_nth conf "relationship link/relationship links"
      (if total_links = 1 then 0 else 1)
  in
  let link_between =
    Printf.sprintf
      (Util.ftransl conf "%d %s links between %s and %s")
      total_links link_word p1_name p2_name
  in
  let header = Printf.sprintf "<b>%s</b><br><br>" link_between in
  let body =
    if shortest_path.Relation.anc = [] then ""
    else
      let shortest = Util.transl conf "shortest path" |> Utf8.capitalize_fst in
      let shortest_path_text =
        Printf.sprintf "<b>%s%s</b><br>" shortest colon
      in
      let deg_icon = "<i class='fa-solid fa-elevator fa-fw'></i>" in
      let asc_icon =
        "<i class='fa-solid fa-person-arrow-up-from-line fa-fw'></i>"
      in
      let desc_icon =
        "<i class='fa-solid fa-person-arrow-down-to-line fa-flip-horizontal \
         fa-fw'></i>"
      in
      let kin_label =
        Util.transl_nth conf "degree of kinship"
          (if shortest_path.Relation.l1 + shortest_path.Relation.l2 = 1 then 0
           else 1)
      in
      let asc_idx = if shortest_path.Relation.l1 = 1 then 0 else 2 in
      let desc_idx = if shortest_path.Relation.l2 = 1 then 1 else 3 in
      let asc_label =
        Util.transl_nth conf "ascending/descending (degree)" asc_idx
      in
      let desc_label =
        Util.transl_nth conf "ascending/descending (degree)" desc_idx
      in
      let degres =
        Printf.sprintf
          "  %s <span class='rm-deg'>%d</span> %s<br>%s <span \
           class='rm-deg'>%d</span> %s<br>%s <span class='rm-deg'>%d</span> \
           %s<br>"
          asc_icon shortest_path.Relation.l1 asc_label desc_icon
          shortest_path.Relation.l2 desc_label deg_icon
          (shortest_path.Relation.l1 + shortest_path.Relation.l2)
          kin_label
      in
      let rel_pct_info =
        Printf.sprintf "<b>%s%s</b> %s<br><br>"
          (Util.transl_nth conf "relationship coefficient/percentage" 1
          |> Utf8.capitalize_fst)
          colon
          (Util.string_of_decimal_num conf (rel_coeff *. 100.0) ^ "&nbsp;%")
      in
      let anc_html =
        format_ancestors_with_couples base shortest_path.Relation.anc
      in
      let anc_label =
        Util.transl_nth conf "first common ancestor"
          (if List.length shortest_path.Relation.anc = 1 then 0 else 1)
        |> Utf8.capitalize_fst
      in
      let common_anc =
        Printf.sprintf "<br><b>%s%s</b><br>%s" anc_label colon anc_html
      in
      rel_pct_info ^ shortest_path_text ^ degres ^ common_anc
  in
  String.map (function '"' -> '\'' | c -> c) (header ^ body)

(* Conversion JSON *)
let anc_to_json anc =
  `Assoc
    [
      ("p", `String (Driver.Iper.to_string anc.Relation.p));
      ( "f",
        `List
          (List.map (fun f -> `String (Driver.Ifam.to_string f)) anc.Relation.f)
      );
      ("c", `Int anc.Relation.c);
    ]

let path_to_json path =
  `Assoc
    [
      ("l1", `Int path.Relation.l1);
      ("l2", `Int path.Relation.l2);
      ("anc", `List (List.map anc_to_json path.Relation.anc));
    ]

let cell_data_to_json cell =
  `Assoc
    [
      ("paths", `List (List.map path_to_json cell.paths));
      ("total", `String (Sosa.to_string cell.total));
      ("coeff", `Float cell.coeff);
    ]

(* Générer le JSON pour la partie supérieure de la matrice uniquement *)
let matrix_to_json base persons cell_storage n =
  let cells = ref [] in
  let used_ipers = ref IperSet.empty in
  (* Ne parcourir que la partie supérieure de la matrice *)
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      (* Ajouter les ipers des personnes de la matrice *)
      used_ipers :=
        IperSet.add
          (Driver.get_iper persons.(i))
          (IperSet.add (Driver.get_iper persons.(j)) !used_ipers);
      (* Récupérer les données depuis le cache *)
      match triangular_get cell_storage i j n with
      | None -> ()
      | Some cell_data ->
          (* Collecter les ipers des ancêtres *)
          List.iter
            (fun path ->
              List.iter
                (fun anc ->
                  used_ipers := IperSet.add anc.Relation.p !used_ipers)
                path.Relation.anc)
            cell_data.paths;
          let key = Printf.sprintf "%d_%d" i j in
          let cell_json =
            `Assoc
              [
                ( "i1",
                  `Assoc
                    [
                      ( "p",
                        `String
                          (Driver.Iper.to_string (Driver.get_iper persons.(i)))
                      );
                    ] );
                ( "i2",
                  `Assoc
                    [
                      ( "p",
                        `String
                          (Driver.Iper.to_string (Driver.get_iper persons.(j)))
                      );
                    ] );
                ("data", cell_data_to_json cell_data);
              ]
          in
          cells := (key, cell_json) :: !cells
    done
  done;
  (* Créer la table de mapping iper -> nom *)
  let names_table =
    IperSet.fold
      (fun iper acc ->
        let p = Driver.poi base iper in
        let name = get_person_simple_name base p in
        (Driver.Iper.to_string iper, `String name) :: acc)
      !used_ipers []
  in
  `Assoc [ ("cells", `Assoc !cells); ("names", `Assoc names_table) ]
