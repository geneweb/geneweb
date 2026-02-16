module Pset = struct
  type t = Gwdb.iper list
  type elt = Gwdb.iper

  let add e s = if List.mem e s then s else e :: s
  let empty = []
  let elements s = List.rev s
  let mem = List.mem
end

(* input dag *)

let get_dag_elems conf base =
  let rec loop prev_po set i =
    let s = string_of_int i in
    let po = Util.find_person_in_env conf base s in
    let po = match po with None -> prev_po | x -> x in
    let so = Util.p_getenv conf.Config.env ("s" ^ s) in
    match (po, so) with
    | Some p, Some s ->
        let set =
          match
            Option.bind (Sosa.of_string s) (fun sosa ->
                Util.branch_of_sosa conf base sosa p)
          with
          | Some ipsl ->
              List.fold_left
                (fun set p -> Pset.add (Gwdb.get_iper p) set)
                set ipsl
          | None -> set
        in
        loop po set (i + 1)
    | Some _, None | None, Some _ | None, None -> set
  in
  loop None Pset.empty 1

let make_dag conf base set =
  let list = Pset.elements set in
  let nodes = Array.of_list list in
  let map =
    let rec loop map i =
      if i = Array.length nodes then map
      else loop (Gwdb.IperMap.add nodes.(i) (Dag2html.idag_of_int i) map) (i + 1)
    in
    loop Gwdb.IperMap.empty 0
  in
  let nodes =
    Array.map
      (fun ip ->
        let pare =
          match Gwdb.get_parents (Util.pget conf base ip) with
          | Some ifam ->
              let c = Gwdb.foi base ifam in
              let l =
                Option.fold
                  (Gwdb.IperMap.find_opt (Gwdb.get_mother c) map)
                  ~some:(fun mother -> [ mother ])
                  ~none:[]
              in
              Option.fold
                (Gwdb.IperMap.find_opt (Gwdb.get_father c) map)
                ~some:(fun father -> father :: l)
                ~none:l
          | None -> []
        in
        let chil =
          let u = Util.pget conf base ip in
          Array.fold_left
            (fun chil ifam ->
              let des = Gwdb.foi base ifam in
              Array.fold_left
                (fun chil ip ->
                  Option.fold
                    (Gwdb.IperMap.find_opt ip map)
                    ~some:(fun child -> child :: chil)
                    ~none:chil)
                chil (Gwdb.get_children des))
            [] (Gwdb.get_family u)
        in
        let chil = List.rev chil in
        { Dag2html.pare; valu = Def.Left ip; chil })
      nodes
  in
  let nodes =
    let rec loop nodes n i =
      if i = Array.length nodes then nodes
      else
        match nodes.(i) with
        | { Dag2html.valu = Def.Left ip; chil } ->
            let ifaml =
              Array.to_list (Gwdb.get_family (Util.pget conf base ip))
            in
            let nodes, n =
              let rec loop nodes = function
                | ifam :: ifaml -> (
                    let cpl = Gwdb.foi base ifam in
                    let isp = Gutil.spouse ip cpl in
                    let jdo = Gwdb.IperMap.find_opt isp map in
                    match jdo with
                    | Some jd ->
                        let j = Dag2html.int_of_idag jd in
                        if chil = [] && nodes.(j).Dag2html.chil = [] then (
                          let pare = [ Dag2html.idag_of_int i; jd ] in
                          let d =
                            { Dag2html.pare; valu = Def.Right n; chil = [] }
                          in
                          let nodes = Array.append nodes [| d |] in
                          let nd = Dag2html.idag_of_int n in
                          nodes.(i).chil <- [ nd ];
                          nodes.(j).chil <- [ nd ];
                          (nodes, n + 1))
                        else if chil <> nodes.(j).chil then (
                          List.iter
                            (fun nd ->
                              if List.mem nd nodes.(j).chil then ()
                              else
                                let n = Dag2html.int_of_idag nd in
                                nodes.(j).chil <- nd :: nodes.(j).chil;
                                nodes.(n).pare <- jd :: nodes.(n).pare)
                            chil;
                          List.iter
                            (fun nd ->
                              if List.mem nd chil then ()
                              else
                                let id = Dag2html.idag_of_int i in
                                let n = Dag2html.int_of_idag nd in
                                nodes.(i).chil <- nd :: chil;
                                nodes.(n).pare <- id :: nodes.(n).pare)
                            nodes.(j).chil;
                          loop nodes ifaml)
                        else loop nodes ifaml
                    | None -> loop nodes ifaml)
                | [] -> (nodes, n)
              in
              loop nodes ifaml
            in
            loop nodes n (i + 1)
        | _ -> loop nodes n (i + 1)
    in
    loop nodes (Array.length nodes) 0
  in
  { Dag2html.dag = nodes }
