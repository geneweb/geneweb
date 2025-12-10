open Config
open Dag2html
open Def
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Iperset = Driver.Iper.Set
module Ipermap = Driver.Iper.Map

let get_dag_elems conf base =
  let rec loop prev_person set i =
    let key = string_of_int i in
    let person_opt = Util.find_person_in_env conf base key in
    let person_opt = match person_opt with None -> prev_person | x -> x in
    let sosa_opt = Util.p_getenv conf.env ("s" ^ key) in
    match (person_opt, sosa_opt) with
    | Some p, Some s ->
        let set =
          match Util.branch_of_sosa conf base (Sosa.of_string s) p with
          | Some branch ->
              List.fold_left
                (fun set p -> Iperset.add (Driver.get_iper p) set)
                set branch
          | None -> set
        in
        loop person_opt set (i + 1)
    | _ -> set
  in
  Iperset.elements (loop None Iperset.empty 1)

type ('a, 'b) sum = ('a, 'b) Def.choice

let make_dag conf base ipers =
  let ipers_arr = Array.of_list ipers in
  let n = Array.length ipers_arr in
  let iper_to_idag =
    Array.fold_left
      (fun (map, i) ip -> (Ipermap.add ip (idag_of_int i) map, i + 1))
      (Ipermap.empty, 0) ipers_arr
    |> fst
  in
  let find_idag ip = Ipermap.find_opt ip iper_to_idag in
  let nodes =
    Array.map
      (fun ip ->
        let pare =
          match Driver.get_parents (pget conf base ip) with
          | Some ifam ->
              let fam = Driver.foi base ifam in
              List.filter_map find_idag
                [ Driver.get_father fam; Driver.get_mother fam ]
          | None -> []
        in
        let chil =
          let person = pget conf base ip in
          Driver.get_family person
          |> Array.fold_left
               (fun acc ifam ->
                 Driver.get_children (Driver.foi base ifam)
                 |> Array.fold_left
                      (fun acc child_ip ->
                        match find_idag child_ip with
                        | Some idag -> idag :: acc
                        | None -> acc)
                      acc)
               []
          |> List.rev
        in
        { pare; valu = Left ip; chil })
      ipers_arr
  in
  let nodes =
    let rec add_spouse_nodes nodes next_id i =
      if i >= Array.length nodes then nodes
      else
        match nodes.(i) with
        | { valu = Left ip; chil; _ } ->
            let ifams =
              Driver.get_family (pget conf base ip) |> Array.to_list
            in
            let nodes, next_id =
              List.fold_left
                (fun (nodes, next_id) ifam ->
                  let cpl = Driver.foi base ifam in
                  let spouse_ip = Gutil.spouse ip cpl in
                  match find_idag spouse_ip with
                  | Some spouse_idag ->
                      let j = int_of_idag spouse_idag in
                      if chil = [] && nodes.(j).chil = [] then (
                        let new_node =
                          {
                            pare = [ idag_of_int i; spouse_idag ];
                            valu = Right next_id;
                            chil = [];
                          }
                        in
                        let nodes = Array.append nodes [| new_node |] in
                        let new_idag = idag_of_int next_id in
                        nodes.(i) <- { (nodes.(i)) with chil = [ new_idag ] };
                        nodes.(j) <- { (nodes.(j)) with chil = [ new_idag ] };
                        (nodes, next_id + 1))
                      else if chil <> nodes.(j).chil then (
                        List.iter
                          (fun child_idag ->
                            if not (List.mem child_idag nodes.(j).chil) then (
                              let child_idx = int_of_idag child_idag in
                              nodes.(j) <-
                                {
                                  (nodes.(j)) with
                                  chil = child_idag :: nodes.(j).chil;
                                };
                              nodes.(child_idx) <-
                                {
                                  (nodes.(child_idx)) with
                                  pare = spouse_idag :: nodes.(child_idx).pare;
                                }))
                          chil;
                        List.iter
                          (fun child_idag ->
                            if not (List.mem child_idag chil) then (
                              let self_idag = idag_of_int i in
                              let child_idx = int_of_idag child_idag in
                              nodes.(i) <-
                                { (nodes.(i)) with chil = child_idag :: chil };
                              nodes.(child_idx) <-
                                {
                                  (nodes.(child_idx)) with
                                  pare = self_idag :: nodes.(child_idx).pare;
                                }))
                          nodes.(j).chil;
                        (nodes, next_id))
                      else (nodes, next_id)
                  | None -> (nodes, next_id))
                (nodes, next_id) ifams
            in
            add_spouse_nodes nodes next_id (i + 1)
        | _ -> add_spouse_nodes nodes next_id (i + 1)
    in
    add_spouse_nodes nodes n 0
  in
  { dag = nodes }
