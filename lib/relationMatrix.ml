(* lib/relationMatrix.ml *)
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa
module IperSet = Driver.Iper.Set

type ancestor_item =
  [ `Couple of Driver.iper * int * Driver.iper * int * Driver.ifam
  | `Unique of Driver.iper * int ]

type kinship_degree = {
  len1 : int;
  len2 : int;
  sols : (Driver.iper * int) list;
  ancestors : (Driver.iper * Driver.ifam) list;
  all_paths : (int * int * (Driver.person * int) list) list;
}

type cell_data = {
  notation : string option;
  kd : kinship_degree option;
  total_sosa : Sosa.t;
  rel_coeff : float;
  relationship_exists : bool;
  all_ancestors_with_ifam : (Driver.iper * Driver.ifam) list;
}

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

(* Fonctions utilitaires de données *)
let group_ancestors_by_couples
    (ancestors_with_ifam : (Driver.iper * Driver.ifam) list)
    (sols : (Driver.iper * int) list) : ancestor_item list =
  let count_map =
    List.fold_left
      (fun acc (ip, cnt) -> Driver.Iper.Map.add ip cnt acc)
      Driver.Iper.Map.empty sols
  in
  let fam_groups =
    List.fold_left
      (fun acc (ip, ifam) ->
        let members =
          try Driver.Ifam.Map.find ifam acc with Not_found -> []
        in
        Driver.Ifam.Map.add ifam (ip :: members) acc)
      Driver.Ifam.Map.empty ancestors_with_ifam
  in
  let used = ref IperSet.empty in
  let items = ref [] in
  Driver.Ifam.Map.iter
    (fun ifam members ->
      match members with
      | [ a; b ]
        when a <> b && (not (IperSet.mem a !used)) && not (IperSet.mem b !used)
        ->
          let ca = try Driver.Iper.Map.find a count_map with Not_found -> 1 in
          let cb = try Driver.Iper.Map.find b count_map with Not_found -> 1 in
          items := `Couple (a, ca, b, cb, ifam) :: !items;
          used := IperSet.add a (IperSet.add b !used)
      | _ ->
          List.iter
            (fun ip ->
              if not (IperSet.mem ip !used) then (
                let c =
                  try Driver.Iper.Map.find ip count_map with Not_found -> 1
                in
                items := `Unique (ip, c) :: !items;
                used := IperSet.add ip !used))
            members)
    fam_groups;
  List.rev !items

let get_person_simple_name base person =
  let first_name = Driver.sou base (Driver.get_first_name person) in
  let surname = Driver.sou base (Driver.get_surname person) in
  Printf.sprintf "%s %s" first_name surname

(* Logique métier de calcul des relations *)
let extract_ancestors_for_ipers (ancestor_ipers : Driver.iper list)
    (all_ancestors : (Driver.iper * Driver.ifam) list) :
    (Driver.iper * Driver.ifam) list =
  let iper_set =
    List.fold_left
      (fun acc ip -> IperSet.add ip acc)
      IperSet.empty ancestor_ipers
  in
  List.filter (fun (ip, _) -> IperSet.mem ip iper_set) all_ancestors

let compute_relationship_cell conf base tstab p1 p2 :
    string option
    * kinship_degree option
    * Sosa.t
    * float
    * (Driver.iper * Driver.ifam) list =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  match Relation.compute_simple_relationship conf base tstab ip1 ip2 with
  | None -> (None, None, Sosa.zero, 0.0, [])
  | Some (rl, total, relationship, _reltab, ancestors_with_ifam) -> (
      match rl with
      | [] -> (None, None, total, relationship, ancestors_with_ifam)
      | (len1, len2, sols_person) :: _ ->
          let sols_iper =
            List.map (fun (p, cnt) -> (Driver.get_iper p, cnt)) sols_person
          in
          let anc_for_first_path =
            extract_ancestors_for_ipers (List.map fst sols_iper)
              ancestors_with_ifam
          in
          let kd =
            {
              len1;
              len2;
              sols = sols_iper;
              ancestors = anc_for_first_path;
              all_paths = rl;
            }
          in
          let notation =
            if len1 = 0 && len2 = 0 then "="
            else Printf.sprintf "%d/%d" len1 len2
          in
          (Some notation, Some kd, total, relationship, ancestors_with_ifam))

let get_best_inverted_path all_paths =
  let best = ref None in
  List.iter
    (fun (l1, l2, sols) ->
      let inverted = (l2, l1, sols) in
      match !best with
      | None -> best := Some inverted
      | Some (b_l1, b_l2, _) ->
          let sum = l2 + l1 in
          let b_sum = b_l1 + b_l2 in
          if sum < b_sum || (sum = b_sum && l2 < b_l1) then
            best := Some inverted)
    all_paths;
  !best

let create_inverted_cell_data (cell : cell_data) : cell_data =
  match cell.kd with
  | None -> cell
  | Some kd -> (
      match get_best_inverted_path kd.all_paths with
      | None -> cell
      | Some (new_len1, new_len2, new_sols) ->
          let new_sols_iper =
            List.map (fun (p, cnt) -> (Driver.get_iper p, cnt)) new_sols
          in
          let new_ancestors =
            extract_ancestors_for_ipers
              (List.map fst new_sols_iper)
              cell.all_ancestors_with_ifam
          in
          let new_kd =
            {
              len1 = new_len1;
              len2 = new_len2;
              sols = new_sols_iper;
              ancestors = new_ancestors;
              all_paths = kd.all_paths;
            }
          in
          let notation =
            if new_len1 = 0 && new_len2 = 0 then Some "="
            else Some (Printf.sprintf "%d/%d" new_len1 new_len2)
          in
          {
            notation;
            kd = Some new_kd;
            total_sosa = cell.total_sosa;
            rel_coeff = cell.rel_coeff;
            relationship_exists = cell.relationship_exists;
            all_ancestors_with_ifam = cell.all_ancestors_with_ifam;
          })
