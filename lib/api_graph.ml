#ifdef API

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

let close_person_relation conf base ips gen_desc =
  (* First, build the list of oldest ancestors of each branch *)
  let asc = Hashtbl.create 32 in
  (* Make sure that ref personne is first element of the list (because of implex) *)
  let rec loop_asc max_gen gen ip =
    if not @@ Hashtbl.mem asc ip then begin
      let p = pget conf base ip in
      match get_parents p with
      | Some ifam when gen < max_gen ->
        let cpl = foi base ifam in
        loop_asc max_gen (gen + 1) (get_father cpl) ;
        loop_asc max_gen (gen + 1) (get_mother cpl)
      | _ -> Hashtbl.add asc ip (gen, p)
    end
  in
  List.iter (fun (ip, max_gen) -> loop_asc max_gen 0 ip) ips ;
  let desc = Hashtbl.create 64 in
  let skip = Hashtbl.create 64 in
  let rec loop_desc gen ip =
    if not @@ Hashtbl.mem skip ip then begin
      let p = pget conf base ip in
      Hashtbl.add skip ip true ;
      Hashtbl.replace desc ip p ;
      Array.iter
        (fun ifam ->
           let sp = Gutil.spouse ip (foi base ifam) in
           Hashtbl.replace desc sp (pget conf base sp) )
        (get_family p) ;
      if gen > gen_desc then
        List.iter (loop_desc (gen - 1)) @@ ChangeChildren.select_children_of base p
    end
  in
  Hashtbl.iter (fun ip (gen, _p) -> loop_desc gen ip) asc ;
  Hashtbl.fold (fun _k v acc -> v :: acc) desc []

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
  let list =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
      let ips =
        let sp_lvl =
          if cpp.M.Close_persons_params.spouse_ascend
          then asc else 1
        in
        (ip, asc)
        :: Array.fold_left
          (fun acc f -> (Gutil.spouse ip (foi base f), sp_lvl) :: acc)
          [] (get_family @@ pget conf base ip)
      in
      close_person_relation conf base ips desc
    | None -> []
  in
  let () = Perso.build_sosa_ht conf base in
  let () = load_image_ht conf in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    begin
      let list =
        List.fold_left
          (fun accu p ->
            if apply_filters_p conf filters Perso.get_sosa_person p then
              let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
              p :: accu
            else accu)
          [] list
      in
      let data =
        if filters.nb_results then
          let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
          Mext.gen_internal_int32 len
        else
          begin
            let list = M.List_full_persons.({persons = list}) in
            Mext.gen_list_full_persons list
          end
      in
      print_result conf data
    end
  else
    begin
      let list =
        List.fold_left
          (fun accu p ->
            if apply_filters_p conf filters Perso.get_sosa_person p then
              let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
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
            let list = M.List_persons.({list_persons = list}) in
            Mext.gen_list_persons list
          end
      in
      print_result conf data
    end
;;


(* Graphe d'ascendance *)

let build_graph_asc conf base p max_gen =
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
        build_graph_asc conf base p nb_gen
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

let build_graph_asc_lia conf base p max_gen =
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
  let create_family_link ifath_imoth ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_link base ifath_imoth ifam fam) :: !families
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
                                        | (Some pfath, Some pmoth, Some _) ->
                                            let (fath, _) = Perso_link.make_ep_link base pfath in
                                            let (moth, _) = Perso_link.make_ep_link base pmoth in
                                            nodes := create_node fath pfath.MLink.Person.baseprefix :: !nodes;
                                            nodes := create_node moth pmoth.MLink.Person.baseprefix :: !nodes;
                                            edges := create_edge base_prefix p pfath.MLink.Person.baseprefix fath :: !edges;
                                            edges := create_edge base_prefix p pmoth.MLink.Person.baseprefix moth :: !edges;
                                            let ifath = get_key_index fath in
                                            let imoth = get_key_index moth in
                                            let (ifam, fam, _, _) = Perso_link.make_efam_link conf base family in
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
        build_graph_asc_lia conf base p nb_gen
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

let build_graph_desc conf base p max_gen =
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
        build_graph_desc conf base p nb_gen
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
  List.iter (fun (a, _) -> create_link a) list;
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
        let rel =
          match
            try Left (Relation.compute_relationship conf base by_marr p1 p2)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> rel
          | Right _ -> None
        in
        (match rel with
        | Some (rl, _, _) ->
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
        let rel =
          match
            try Left (Relation.compute_relationship conf base by_marr p1 p2)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> rel
          | Right _ -> None
        in
        (match rel with
        | Some (rl, _, _) ->
            let list =
              (function (_, _, (_, _, list), _) -> list) (List.hd rl)
            in
            List.map (fun (p, _) -> p) (list)
        | None -> [])
    | _ -> []
  in
  let data = data_list_person conf base filters list in
  print_result conf data

#endif
