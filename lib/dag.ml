open Config
open Def
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Iper = Driver.Iper

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
                (fun set p -> Iper.Set.add (Driver.get_iper p) set)
                set branch
          | None -> set
        in
        loop person_opt set (i + 1)
    | _ -> set
  in
  Iper.Set.elements (loop None Iper.Set.empty 1)

let make_dag conf base ipers =
  let ipers_arr = Array.of_list ipers in
  let n = Array.length ipers_arr in
  let iper_to_idag =
    Array.to_seqi ipers_arr
    |> Seq.map (fun (i, ip) -> (ip, Dag2html.idag_of_int i))
    |> Iper.Map.of_seq
  in
  let find_idag ip = Iper.Map.find_opt ip iper_to_idag in
  let nodes =
    Array.map
      (fun ip ->
        let pare =
          match Driver.get_parents (Util.pget conf base ip) with
          | Some ifam ->
              let fam = Driver.foi base ifam in
              List.filter_map find_idag
                [ Driver.get_father fam; Driver.get_mother fam ]
          | None -> []
        in
        let chil =
          let person = Util.pget conf base ip in
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
        { Dag2html.pare; valu = Left ip; chil })
      ipers_arr
  in
  let max_extra =
    Array.fold_left
      (fun acc -> function
        | { Dag2html.valu = Left ip; _ } ->
            acc + Array.length (Driver.get_family (Util.pget conf base ip))
        | _ -> acc)
      0 nodes
  in
  let nodes =
    if max_extra = 0 then nodes
    else
      let arr =
        Array.make (n + max_extra)
          { Dag2html.pare = []; valu = Right (-1); chil = [] }
      in
      Array.blit nodes 0 arr 0 n;
      arr
  in
  let nodes =
    let rec add_spouse_nodes next_id i =
      if i >= n then next_id
      else
        match nodes.(i) with
        | { valu = Left ip; chil; _ } ->
            let ifams =
              Driver.get_family (Util.pget conf base ip) |> Array.to_list
            in
            let next_id =
              List.fold_left
                (fun next_id ifam ->
                  let cpl = Driver.foi base ifam in
                  let spouse_ip = Gutil.spouse ip cpl in
                  match find_idag spouse_ip with
                  | Some spouse_idag ->
                      let j = Dag2html.int_of_idag spouse_idag in
                      if chil = [] && nodes.(j).chil = [] then (
                        let new_idag = Dag2html.idag_of_int next_id in
                        nodes.(next_id) <-
                          {
                            pare = [ Dag2html.idag_of_int i; spouse_idag ];
                            valu = Right next_id;
                            chil = [];
                          };
                        nodes.(i) <-
                          { (nodes.(i)) with chil = new_idag :: nodes.(i).chil };
                        nodes.(j) <-
                          { (nodes.(j)) with chil = new_idag :: nodes.(j).chil };
                        next_id + 1)
                      else if chil <> nodes.(j).chil then (
                        List.iter
                          (fun child_idag ->
                            if not (List.mem child_idag nodes.(j).chil) then (
                              let child_idx = Dag2html.int_of_idag child_idag in
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
                              let self_idag = Dag2html.idag_of_int i in
                              let child_idx = Dag2html.int_of_idag child_idag in
                              nodes.(i) <-
                                {
                                  (nodes.(i)) with
                                  chil = child_idag :: nodes.(i).chil;
                                };
                              nodes.(child_idx) <-
                                {
                                  (nodes.(child_idx)) with
                                  pare = self_idag :: nodes.(child_idx).pare;
                                }))
                          nodes.(j).chil;
                        next_id)
                      else next_id
                  | None -> next_id)
                next_id ifams
            in
            add_spouse_nodes next_id (i + 1)
        | _ -> add_spouse_nodes next_id (i + 1)
    in
    let final_next_id = add_spouse_nodes n 0 in
    if final_next_id = Array.length nodes then nodes
    else Array.sub nodes 0 final_next_id
  in
  { Dag2html.dag = nodes }
