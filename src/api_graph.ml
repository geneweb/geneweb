(* nocamlp5 *)

module M = Api_piqi
module Mext = Api_piqi_ext

module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext

open Config
open Gwdb
open Def
open Util
open Api_def
open Api_util



(**/**) (* TRY WITH COMPUTE RELATION *)
(*
module Iper2 =
  struct
    type t = (Adef.iper * int)
    let compare (i1, _) (i2, _) =
      Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2)
  end

module IperSet2 = Set.Make(Iper2) ;;


let close_persons conf base ip nb_gen_asc nb_gen_desc spouse_ascend =
  let _ = load_descends_array base in
  let _ = load_unions_array base in
  (* On construit la liste des plus vieux ancêtre pour obtenir *)
  (* toute la descendances correspondant aux proches.          *)
  let ascends = ref [] in
  let next_gen_ascend curr_gen =
    let rec loop curr_gen next_gen =
      match curr_gen with
      | [] -> next_gen
      | (ip, gen) :: l ->
          let asc = pget conf base ip in
          match get_parents asc with
          | Some ifam ->
              let cpl = foi base ifam in
              let father = (get_father cpl, (gen + 1)) in
              let mother = (get_mother cpl, (gen + 1)) in
              loop l (father :: mother :: next_gen)
          | None ->
              let _ = ascends := (ip, gen) :: !ascends in
              loop l next_gen
    in
    loop curr_gen []
  in
  let build_ascend l nb_gen =
    let rec loop l gen =
      if gen = nb_gen then List.append !ascends l
      else loop (next_gen_ascend l) (gen + 1)
    in loop l 0
  in
  (* Liste de la personne et conjoints dont on veut les proches *)
  let gen_0 =
    if spouse_ascend then
      let p = poi base ip in
      let spl =
        List.map
          (fun ifam ->
            let fam = foi base ifam in
            (spouse ip fam, 0) )
          (Array.to_list (get_family p))
      in
      build_ascend ((ip, 0) :: spl) nb_gen_asc
    else
      let p = poi base ip in
      let spl =
        List.map
          (fun ifam ->
            let fam = foi base ifam in
            (spouse ip fam, 0) )
          (Array.to_list (get_family p))
      in
      let spl = build_ascend spl 1 in
      let asc = build_ascend [(ip, 0)] nb_gen_asc in
      List.append asc spl
  in
  (* On construit la liste de tous les descendants, *)
  (* cela correspond à la liste des proches.        *)
  let close_persons = ref IperSet.empty in
  let mark_fam = Array.make (nb_of_families base) false in
  let next_gen_descend curr_gen =
    let rec loop curr_gen next_gen =
      match curr_gen with
      | [] -> IperSet2.elements !next_gen
      | (ip, gen) :: l ->
          close_persons := IperSet.add ip !close_persons;
          (* Si la génération suivante est trop 'basse', on ne l'ajoute pas *)
          if gen <= nb_gen_desc then loop l next_gen
          else
            begin
              let p = poi base ip in
              Array.iter
                (fun ifam ->
                  if mark_fam.(Adef.int_of_ifam ifam) then ()
                  else
                    begin
                      Array.set mark_fam (Adef.int_of_ifam ifam) true;
                      let fam = foi base ifam in
                      next_gen := IperSet2.add (spouse ip fam, gen) !next_gen;
                      Array.iter
                        (fun ip -> next_gen := IperSet2.add (ip, gen-1) !next_gen)
                        (get_children fam);
                    end)
                (get_family p);
              loop l next_gen
            end
    in
    loop curr_gen (ref IperSet2.empty)
  in
  let rec build_descend curr_gen =
    if curr_gen = [] then IperSet.elements !close_persons
    else build_descend (next_gen_descend curr_gen)
  in
  build_descend gen_0
;;

let print_close_person_relations2 conf base =
  let cpp = get_params conf Mext.parse_close_persons_params in
  let base_loop = has_base_loop conf base in
  let ref_person = cpp.M.Close_persons_params.person in
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  let asc =
    match cpp.M.Close_persons_params.nb_gen_asc with
    | Some n -> max 1 (Int32.to_int n)
    | None -> 2
  in
  let desc =
    match cpp.M.Close_persons_params.nb_gen_asc with
    | Some n -> (- (max 1 (Int32.to_int n)))
    | None -> (-3)
  in
  let spouse = cpp.M.Close_persons_params.spouse_ascend in
  let only_recent = cpp.M.Close_persons_params.only_recent in
  let list =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        close_person_relation conf base ip asc desc spouse
    | None -> []
  in
  let list =
    if only_recent then
      reduce_to_recent conf (List.map (fun ip -> poi base ip) list)
    else List.map (fun ip -> poi base ip) list
  in
  let list = M.List_person_relation.({person_relations = list}) in
  let data = Mext.gen_list_person_relation list in
  print_result conf data
;;

*)





(**/**)

module Iper3 =
  struct
    type t = (Adef.iper * int * M.relation_type)
    let compare (i1, _, _) (i2, _, _) =
      Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2)
  end

module IperSet3 = Set.Make(Iper3) ;;

let compute_related r_type level =
  match r_type with
  | Adoption ->
      (match level with
      | 1 -> `adoptive_parent
      | (-1) -> `adoptive_child
      | _ -> `no_relation)
  | Recognition ->
      (match level with
      | 1 -> `recognized_parent
      | (-1) -> `recognized_child
      | _ -> `no_relation)
  | CandidateParent ->
      (match level with
      | 1 -> `candidate_parent
      | (-1) -> `candidate_child
      | _ -> `no_relation)
  | GodParent ->
      (match level with
      | 1 -> `god_parent
      | (-1) -> `god_child
      | _ -> `no_relation)
  | FosterParent ->
      (match level with
      | 1 -> `foster_parent
      | (-1) -> `foster_child
      | _ -> `no_relation)
;;

let compute_rel_sp r_type =
  match r_type with
  | `self -> `spouse
  | `spouse -> `step_brother
  | `sibling -> `step_brother
  | `step_brother -> `step_brother
  | `parent -> `step_parent
  | `uncle -> `uncle_spouse
  | `cousin -> `cousin_spouse
  | `child -> `step_child
  | `grand_child -> `grand_child_spouse
  | `nephew -> `nephew_spouse
  | `nephew_spouse -> `nephew_spouse_spouse
  | `grand_nephew -> `grand_nephew_spouse
  | `grand_nephew_spouse -> `grand_nephew_spouse_spouse
  | `great_grand_nephew -> `great_grand_nephew_spouse
  | `great_grand_nephew_spouse -> `great_grand_nephew_spouse_spouse
  | `great_grand_child -> `great_grand_child_spouse
  | `child_cousin -> `child_cousin_spouse
  | `grand_child_cousin -> `grand_child_cousin_spouse
  | `great_grand_child_cousin -> `great_grand_child_cousin_spouse
  | _ -> `no_relation
;;

let compute_rel r_type level =
  match r_type with
  | `self ->
      (match level with
      | 0 -> `sibling
      | 1 -> `parent
      | (-1) -> `child
      | _ -> `no_relation)
  | `spouse ->
      (match level with
      | 0 -> `step_brother
      | 1 -> `step_parent
      | (-1) -> `great_grand_child_cousin
      | _ -> `no_relation)
  | `parent ->
      (match level with
      | 0 -> `uncle
      | 1 -> `grand_parent
      | _ -> `no_relation)
  | `uncle ->
      (match level with
      | (-1) -> `cousin
      | _ -> `no_relation)
  | `sibling ->
      (match level with
      | 0 -> `sibling
      | 1 -> `grand_parent
      | (-1) -> `nephew
      | _ -> `no_relation)
  | `child ->
      (match level with
      | (-1) -> `grand_child
      | _ -> `no_relation)
  | `grand_child ->
      (match level with
      | (-1) -> `great_grand_child
      | _ -> `no_relation)
  | `step_brother ->
      (match level with
      | (-1) -> `nephew_spouse
      | _ -> `no_relation)
  | `nephew_spouse ->
      (match level with
      | (-1) -> `grand_nephew_spouse
      | _ -> `no_relation)
  | `grand_nephew_spouse ->
      (match level with
      | (-1) -> `great_grand_nephew_spouse
      | _ -> `no_relation)
  | `cousin ->
      (match level with
      | (-1) -> `child_cousin
      | _ -> `no_relation)
  | `child_cousin ->
      (match level with
      | (-1) -> `grand_child_cousin
      | _ -> `no_relation)
  | `grand_child_cousin ->
      (match level with
      | (-1) -> `great_grand_child_cousin
      | _ -> `no_relation)
  | `nephew ->
      (match level with
      | (-1) -> `grand_nephew
      | _ -> `no_relation)
  | `grand_nephew ->
      (match level with
      | (-1) -> `great_grand_nephew
      | _ -> `no_relation)
  | _ -> `no_relation
;;


let close_person_relation conf base ip nb_gen_asc nb_gen_desc spouse_ascend only_recent base_loop =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
*)
  (* On construit la liste des plus vieux ancêtre pour obtenir *)
  (* toute la descendances correspondant aux proches.          *)
  let ref_close_person = poi base ip in
  let ascends = ref [] in
  let next_gen_ascend curr_gen =
    let rec loop curr_gen next_gen =
      match curr_gen with
      | [] -> next_gen
      | (ip, gen, r_type) :: l ->
          let asc = pget conf base ip in
          match get_parents asc with
          | Some ifam ->
              let cpl = foi base ifam in
              let father = (get_father cpl, (gen + 1), compute_rel r_type 1) in
              let mother = (get_mother cpl, (gen + 1), compute_rel r_type 1) in
              let child =
                List.map
                  (fun ip_c ->
                    if ip_c = ip then (ip, gen, r_type)
                    else (ip_c, gen, compute_rel r_type 0))
                  (Array.to_list (get_children cpl))
              in
              ascends := List.append !ascends child;
              loop l (father :: mother :: next_gen)
          | None ->
              ascends := (ip, gen, r_type) :: !ascends;
              loop l next_gen
    in
    loop curr_gen []
  in
  let build_ascend l nb_gen =
    let rec loop l gen =
      ascends := List.append !ascends l;
      if gen = nb_gen then !ascends
      else loop (next_gen_ascend l) (gen + 1)
    in loop l 0
  in
  (* Liste de la personne et conjoints dont on veut les proches *)
  let _ =
    (* on veut les ascendants de la personne et ses conjoints *)
    if spouse_ascend then
      let p = ref_close_person in
      let spl =
        List.map
          (fun ifam ->
            let fam = foi base ifam in
            (Gutil.spouse ip fam, 0, `spouse) )
          (Array.to_list (get_family p))
      in
      build_ascend ((ip, 0, `self) :: spl) nb_gen_asc
    (* on ne veut que les parents du conjoint *)
    else
      let p = ref_close_person in
      let spl =
        List.map
          (fun ifam ->
            let fam = foi base ifam in
            (Gutil.spouse ip fam, 0, `spouse) )
          (Array.to_list (get_family p))
      in
      let spl = build_ascend spl 1 in
      let asc = build_ascend [(ip, 0, `self)] nb_gen_asc in
      List.append asc spl
  in
  (* On construit la liste de tous les descendants, *)
  (* cela correspond à la liste des proches.        *)
  let close_persons = ref IperSet3.empty in
  let mark_fam = Array.make (nb_of_families base) false in
  let next_gen_descend curr_gen =
    let rec loop curr_gen next_gen =
      match curr_gen with
      | [] -> IperSet3.elements !next_gen
      | (ip, gen, r_type) :: l ->
          close_persons := IperSet3.add (ip, gen, r_type) !close_persons;
          (* Si la génération suivante est trop 'basse', on ne l'ajoute pas *)
          if gen <= nb_gen_desc then loop l next_gen
          else
            begin
              let p = poi base ip in
              Array.iter
                (fun ifam ->
                  if mark_fam.(Adef.int_of_ifam ifam) then ()
                  else
                    begin
                      Array.set mark_fam (Adef.int_of_ifam ifam) true;
                      let fam = foi base ifam in
                      next_gen :=
                        IperSet3.add
                          (Gutil.spouse ip fam, gen, compute_rel_sp r_type)
                          !next_gen;
                      (* On marque la famille du conjoint sinon, en cas de multi
                         marriage, on a la descendance de chaque conjoint *)
                      Array.iter
                        (fun ifam -> Array.set mark_fam (Adef.int_of_ifam ifam) true)
                        (get_family (poi base (Gutil.spouse ip fam)));
                      Array.iter
                        (fun ip ->
                          next_gen :=
                            IperSet3.add
                              (ip, gen-1, compute_rel r_type (-1))
                              !next_gen)
                        (get_children fam);
                    end)
                (get_family p);
              loop l next_gen
            end
    in
    loop curr_gen (ref IperSet3.empty)
  in
  let rec build_descend curr_gen =
    if curr_gen = [] then !close_persons
    else build_descend (next_gen_descend curr_gen)
  in
  let _ = build_descend (List.rev !ascends) in
  let _ =
    List.iter
      (fun r ->
        (match r.r_fath with
        | Some ip ->
            close_persons := IperSet3.add (ip, 0, compute_related r.r_type 1) !close_persons
        | None -> ());
        (match r.r_moth with
        | Some ip ->
            close_persons := IperSet3.add (ip, 0, compute_related r.r_type 1) !close_persons
        | None -> ()))
      (get_rparents ref_close_person)
  in
  let _ =
    List.iter
      (fun ip ->
        let p = poi base ip in
        List.iter
          (fun r ->
            (match r.r_fath with
            | Some _ ->
                close_persons := IperSet3.add (ip, 0, compute_related r.r_type (-1)) !close_persons
            | None -> ());
            (match r.r_moth with
            | Some _ ->
                close_persons := IperSet3.add (ip, 0, compute_related r.r_type (-1)) !close_persons
            | None -> ()))
          (get_rparents p))
      (get_related ref_close_person)
  in
  let _ =
    let ifam = get_family ref_close_person in
    List.iter
      (fun ifam ->
        let fam = foi base ifam in
        List.iter
          (fun ip ->
            close_persons := IperSet3.add (ip, 0, `witness) !close_persons)
          (Array.to_list (get_witnesses fam)))
      (Array.to_list (ifam))
  in
  IperSet3.elements !close_persons
;;


let print_close_person_relations conf base =
  let cpp = get_params conf Mext.parse_close_persons_params in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let ref_person = cpp.M.Close_persons_params.person in
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  let asc =
    match cpp.M.Close_persons_params.nb_gen_asc with
    | Some n -> max 1 (Int32.to_int n)
    | None -> 2
  in
  let desc =
    match cpp.M.Close_persons_params.nb_gen_asc with
    | Some n -> (- (max 1 (Int32.to_int n)))
    | None -> (-3)
  in
  let spouse = cpp.M.Close_persons_params.spouse_ascend in
  let only_recent = cpp.M.Close_persons_params.only_recent in
  let list =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        close_person_relation conf base ip asc desc spouse only_recent base_loop
    | None -> []
  in
  let () = Perso.build_sosa_ht conf base in
  let () = load_image_ht conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    begin
      let list =
        List.fold_left
          (fun accu (ip, _, r_type) ->
            let p = poi base ip in
            if apply_filters_p conf base filters Perso.get_sosa_person p then
              let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
              let p =
                M.Full_person_relation.({
                  person = p;
                  relation = r_type;
                })
              in
              p :: accu
            else accu )
          [] list
      in
      let data =
        if filters.nb_results then
          let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
          Mext.gen_internal_int32 len
        else
          begin
            let list = M.List_full_person_relation.({person_relations = list}) in
            Mext.gen_list_full_person_relation list
          end
      in
      print_result conf data
    end
  else
    begin
      let list =
        List.fold_left
          (fun accu (ip, _, r_type) ->
            let p = poi base ip in
            if apply_filters_p conf base filters Perso.get_sosa_person p then
              let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
              let p =
                M.Person_relation.({
                  person = p;
                  relation = r_type;
                })
              in
              p :: accu
            else accu )
          [] list
      in
      let data =
        if filters.nb_results then
          let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
          Mext.gen_internal_int32 len
        else
          begin
            let list = M.List_person_relation.({person_relations = list}) in
            Mext.gen_list_person_relation list
          end
      in
      print_result conf data
    end
;;


(* Graphe d'ascendance *)

let build_graph_asc conf base p max_gen base_loop =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let create_edge p_from p_to =
    M.Edge.({
      from_node = Int64.of_int (Adef.int_of_iper (get_key_index p_from));
      to_node = Int64.of_int (Adef.int_of_iper (get_key_index p_to));
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
  let rec loop l nodes edges families =
    match l with
    | [] ->
        (* On retourne la liste pour avoir les noeuds dans l'ordre *)
        (* la référence, suivi du père suivi, puis de la mère ...  *)
        (!nodes, List.rev !edges, List.rev !families)
    | (p, gen) :: l ->
      if gen >= max_gen then
        loop l nodes edges families
      else
        begin
          match get_parents p with
          | Some ifam ->
              let cpl = foi base ifam in
              let fath = poi base (get_father cpl) in
              let moth = poi base (get_mother cpl) in
              nodes := moth :: fath :: !nodes;
              edges := (create_edge p fath) :: !edges;
              edges := (create_edge p moth) :: !edges;
              create_family ifam families;
              loop ((fath, gen + 1) :: (moth, gen + 1) :: l) nodes edges families
          | None -> loop l nodes edges families
        end
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  nodes := p :: !nodes;
  loop [(p, 1)] nodes edges families
;;


let print_graph_asc conf base =
  let params = get_params conf Mext.parse_graph_params in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let ref_person = params.M.Graph_params.person in
  let (nodes, edges, families) =
    match piqi_ref_person_to_person base ref_person with
    | Some p ->
        let max_gen = 12 in
        let nb_gen =
          match params.M.Graph_params.generation with
          | Some n -> min max_gen (max (Int32.to_int n) 1)
          | None -> max_gen
        in
        build_graph_asc conf base p nb_gen base_loop
    | None -> ([], [], [])
  in
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length nodes)}) in
      Mext.gen_internal_int32 len
    else
      let nodes = person_node_map conf base nodes in
      match nodes with
      | PFull nodes ->
          let graph =
            M.Full_graph.({nodes = nodes; edges = edges; families = families})
          in
          Mext.gen_full_graph graph
      | PLight nodes ->
          let graph = M.Graph.({nodes = nodes; edges = edges;}) in
          Mext.gen_graph graph
  in
  print_result conf data
;;


(* Graphe d'ascendance lia *)

let build_graph_asc_lia conf base p max_gen base_loop =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge baseprefix_from p_from baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_int (Hashtbl.hash (baseprefix_from, get_key_index p_from))
    in
    let id_to =
      Int64.of_int (Hashtbl.hash (baseprefix_to, get_key_index p_to))
    in
    M.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p base_prefix =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_key_index p) in
    let id = Int64.of_int uniq_id in
    (id, p)
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
  let create_family_link (ifath, imoth) ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_link conf base (ifath, imoth) ifam fam) :: !families
  in
  let rec loop l nodes edges families =
    match l with
    | [] ->
        (* On retourne la liste pour avoir les noeuds dans l'ordre *)
        (* la référence, suivi du père suivi, puis de la mère ...  *)
        (!nodes, List.rev !edges, List.rev !families)
    | (p, gen) :: l ->
        try
          let _ = Hashtbl.find ht (get_key_index p) in
          loop l nodes edges families
        with Not_found ->
          begin
            if gen >= max_gen then loop l nodes edges families
            else
              begin
                Hashtbl.add ht (get_key_index p) true;
                match get_parents p with
                | Some ifam ->
                    let cpl = foi base ifam in
                    let fath = poi base (get_father cpl) in
                    let moth = poi base (get_mother cpl) in
                    nodes := create_node fath conf.command :: !nodes;
                    nodes := create_node moth conf.command :: !nodes;
                    edges := (create_edge conf.command p conf.command fath) :: !edges;
                    edges := (create_edge conf.command p conf.command moth) :: !edges;
                    create_family ifam families;
                    loop ((fath, gen + 1) :: (moth, gen + 1) :: l) nodes edges families
                | None ->
                    (* lien inter arbre *)
                    let ip = get_key_index p in
                    let () =
                      Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                    in
                    let () =
                      let ht = Hashtbl.create 42 in
                      let rec loop_parents l =
                        match l with
                        | [] -> ()
                        | (base_prefix, p, gen) :: l ->
                            try
                              let _ = Hashtbl.find ht (get_key_index p) in
                              loop_parents l
                            with Not_found ->
                              begin
                                if gen >= max_gen then loop_parents l
                                else
                                  let ip = get_key_index p in
                                  match Perso_link.get_parents_link base_prefix ip with
                                  | Some family ->
                                      begin
                                        let ifath = Adef.iper_of_int (Int32.to_int family.MLink.Family.ifath) in
                                        let imoth = Adef.iper_of_int (Int32.to_int family.MLink.Family.imoth) in
                                        let fam_base_prefix = family.MLink.Family.baseprefix in
                                        match
                                          (Perso_link.get_person_link fam_base_prefix ifath,
                                           Perso_link.get_person_link fam_base_prefix imoth,
                                           Perso_link.get_person_link base_prefix ip)
                                        with
                                        | (Some pfath, Some pmoth, Some c) ->
                                            let (fath, _) = Perso_link.make_ep_link conf base pfath in
                                            let (moth, _) = Perso_link.make_ep_link conf base pmoth in
                                            nodes := create_node fath pfath.MLink.Person.baseprefix :: !nodes;
                                            nodes := create_node moth pmoth.MLink.Person.baseprefix :: !nodes;
                                            edges := create_edge base_prefix p pfath.MLink.Person.baseprefix fath :: !edges;
                                            edges := create_edge base_prefix p pmoth.MLink.Person.baseprefix moth :: !edges;
                                            let ifath = get_key_index fath in
                                            let imoth = get_key_index moth in
                                            let (ifam, fam, _, _) = Perso_link.make_efam_link conf base ip family in
                                            create_family_link (ifath, imoth) ifam fam families;
                                            let l =
                                              ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                            in
                                            loop_parents l
                                        | _ -> loop_parents l
                                      end
                                  | None -> loop_parents l
                              end
                      in
                      loop_parents [(conf.bname, p, gen)]
                    in
                    loop l nodes edges families
              end
          end
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  nodes := create_node p conf.command :: !nodes;
  loop [(p, 1)] nodes edges families
;;


let print_graph_asc_lia conf base =
  let params = get_params conf Mext.parse_graph_params in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let ref_person = params.M.Graph_params.person in
  let (nodes, edges, families) =
    match piqi_ref_person_to_person base ref_person with
    | Some p ->
        let max_gen = 12 in
        let nb_gen =
          match params.M.Graph_params.generation with
          | Some n -> min max_gen (max (Int32.to_int n) 1)
          | None -> max_gen
        in
        build_graph_asc_lia conf base p nb_gen base_loop
    | None -> ([], [], [])
  in
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length nodes)}) in
      Mext.gen_internal_int32 len
    else
      let nodes = person_node_map_lia conf base nodes in
      match nodes with
      | PFull nodes ->
          let graph =
            M.Full_graph.({nodes = nodes; edges = edges; families = families})
          in
          Mext.gen_full_graph graph
      | PLight nodes ->
          let graph = M.Graph.({nodes = nodes; edges = edges;}) in
          Mext.gen_graph graph
  in
  print_result conf data
;;

(* Graphe de descendance *)

let build_graph_desc conf base p max_gen base_loop =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge p_from p_to =
    M.Edge.({
      from_node = Int64.of_int (Adef.int_of_iper (get_key_index p_from));
      to_node = Int64.of_int (Adef.int_of_iper (get_key_index p_to));
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
  let rec loop l nodes edges families =
    match l with
    | [] ->
        (* On retourne la liste pour avoir les noeuds dans l'ordre *)
        (* la référence, suivi du père suivi, puis de la mère ...  *)
        (!nodes, List.rev !edges, !families)
    | (p, gen) :: l ->
        try
          let _ = Hashtbl.find ht (get_key_index p) in
          loop l nodes edges families
        with Not_found ->
          begin
            if gen >= max_gen then
              loop l nodes edges families
            else
              begin
                Hashtbl.add ht (get_key_index p) true;
                let ifam = get_family p in
                let l =
                  (* fold_right pour le tri des mariages. *)
                  List.fold_right
                    (fun ifam accu ->
                      let fam = foi base ifam in
                      let sp = poi base (Gutil.spouse (get_key_index p) fam) in
                      let children =
                        List.map (poi base) (Array.to_list (get_children fam))
                      in
                      nodes := sp :: !nodes;
                      if gen <> max_gen then
                        begin
                          nodes := children @ !nodes;
                          List.iter
                            (fun c ->
                              edges := (create_edge p c) :: !edges;
                              edges := (create_edge sp c) :: !edges)
                            children;
                          create_family ifam families;
                          List.fold_right
                            (fun c accu -> (c, gen + 1) :: accu)
                            children accu
                        end
                      else accu)
                    (Array.to_list ifam) l
                in
                loop l nodes edges families
              end
          end
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  nodes := p :: !nodes;
  loop [(p, 1)] nodes edges families
;;


let print_graph_desc conf base =
  let params = get_params conf Mext.parse_graph_params in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let ref_person = params.M.Graph_params.person in
  let (nodes, edges, families) =
    match piqi_ref_person_to_person base ref_person with
    | Some p ->
        let max_gen = 12 in
        let nb_gen =
          match params.M.Graph_params.generation with
          | Some n -> min max_gen (max (Int32.to_int n) 1)
          | None -> max_gen
        in
        build_graph_desc conf base p nb_gen base_loop
    | None -> ([], [], [])
  in
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length nodes)}) in
      Mext.gen_internal_int32 len
    else
      let nodes = person_node_map conf base nodes in
      match nodes with
      | PFull nodes ->
          let graph =
            M.Full_graph.({nodes = nodes; edges = edges; families = families})
          in
          Mext.gen_full_graph graph
      | PLight nodes ->
          let graph = M.Graph.({nodes = nodes; edges = edges;}) in
          Mext.gen_graph graph
  in
  print_result conf data
;;


(* Calcul de relation entre deux personnes *)
let build_rel_graph conf base p1 p2 (pp1, pp2, (l1, l2, list), _) =
(*
  let base_loop = has_base_loop conf base in
  let () = Perso.build_sosa_ht conf base in
  let person_to_node p =
    let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
    let person =
      pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person
    in
    M.Node.({
      id = id;
      person = person;
    })
  in
*)
  let create_edge p_from p_to =
    M.Edge.({
      from_node = Int64.of_int (Adef.int_of_iper (get_key_index p_from));
      to_node = Int64.of_int (Adef.int_of_iper (get_key_index p_to));
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  let create_link a =
    let ip = get_key_index a in
    let p1 =
      match pp1 with
      | Some p1 -> p1
      | _ -> p1
    in
    let p2 =
      match pp2 with
      | Some p2 -> p2
      | _ -> p2
    in
    let ip1 = get_key_index p1 in
    let ip2 = get_key_index p2 in
    let dist = RelationLink.make_dist_tab conf base ip (max l1 l2 + 1) in
    let b1 = RelationLink.find_first_branch conf base dist ip l1 ip1 Neuter in
    let b2 = RelationLink.find_first_branch conf base dist ip l2 ip2 Neuter in
    match (b1, b2) with
    | (Some b1, Some b2) ->
(*        nodes := person_to_node a :: !nodes;*)
        nodes := a :: !nodes;
        let _ =
          List.fold_left
            (fun last_node (ip, _) ->
              let p = poi base ip in
              nodes := p :: !nodes;
              edges := (create_edge p last_node) :: !edges;
              p)
            a b1;
        in
        let _ =
          List.fold_left
            (fun last_node (ip, _) ->
              let p = poi base ip in
              nodes := p :: !nodes;
              edges := (create_edge p last_node) :: !edges;
              p)
            a b2;
        in
        List.iter
          (fun ifam -> create_family ifam families)
          (Array.to_list (get_family a))
    | _ -> ()
  in
  List.iter
    (fun (a, n) -> create_link a)
    list;
  (!nodes, !edges, !families)
;;


let print_graph_rel conf base =
  let params = get_params conf Mext.parse_graph_rel_params in
  let filters = get_filters conf in
  let ref_p1 = params.M.Graph_rel_params.person1 in
  let ref_p2 = params.M.Graph_rel_params.person2 in
  let (nodes, edges, families) =
    match (piqi_ref_person_to_person base ref_p1,
           piqi_ref_person_to_person base ref_p2) with
    | (Some p1, Some p2) ->
        let by_marr = true in
        let (rel, base_loop) =
          match
            try Left (Relation.compute_relationship conf base by_marr p1 p2)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> (rel, false)
          | Right p -> (None, true)
        in
        (match rel with
        | Some (rl, total, relation) ->
            (* rl contient la liste des personnes par *)
            (* lesquelles p1 et p2 sont en relation.  *)
            build_rel_graph conf base p1 p2 (List.hd rl)
        | None -> ([], [], []))
    | _ -> ([], [], [])
  in
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length nodes)}) in
      Mext.gen_internal_int32 len
    else
      let nodes = person_node_map conf base nodes in
      match nodes with
      | PFull nodes ->
          let graph =
            M.Full_graph.({nodes = nodes; edges = edges; families = families})
          in
          Mext.gen_full_graph graph
      | PLight nodes ->
          let graph = M.Graph.({nodes = nodes; edges = edges;}) in
          Mext.gen_graph graph
  in
  print_result conf data
(*
  let graph = M.Graph.({nodes = nodes; edges = edges}) in
  let data = Mext.gen_graph graph in
  print_result conf data
*)
;;


let print_cpl_relation conf base =
  let params = get_params conf Mext.parse_cpl_rel_params in
  let filters = get_filters conf in
  let ref_p1 = params.M.Cpl_rel_params.person1 in
  let ref_p2 = params.M.Cpl_rel_params.person2 in
(*  let base_loop = has_base_loop conf base in*)
  let list =
    match (piqi_ref_person_to_person base ref_p1,
           piqi_ref_person_to_person base ref_p2) with
    | (Some p1, Some p2) ->
        let by_marr = true in
        let (rel, base_loop) =
          match
            try Left (Relation.compute_relationship conf base by_marr p1 p2)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> (rel, false)
          | Right p -> (None, true)
        in
        (match rel with
        | Some (rl, total, relation) ->
            let list =
              (function (_, _, (_, _, list), _) -> list) (List.hd rl)
            in
            List.map (fun (p, _) -> p) (list)
        | None -> [])
    | _ -> []
  in
  let data = data_list_person conf base filters list in
  print_result conf data
;;
