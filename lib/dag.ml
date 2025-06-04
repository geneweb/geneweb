open Config
open Dag2html
open Def
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

(* TODO use a set.. *)
module Pset = struct
  type t = Driver.iper list
  type elt = Driver.iper

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
    let so = Util.p_getenv conf.env ("s" ^ s) in
    match (po, so) with
    | Some p, Some s ->
        let set =
          match Util.branch_of_sosa conf base (Sosa.of_string s) p with
          | Some ipsl ->
              List.fold_left
                (fun set p -> Pset.add (Driver.get_iper p) set)
                set ipsl
          | None -> set
        in
        loop po set (i + 1)
    | _ -> set
  in
  loop None Pset.empty 1

type ('a, 'b) sum = ('a, 'b) Def.choice

let make_dag conf base set =
  let list = Pset.elements set in
  let module O = struct
    type t = Driver.iper

    let compare = compare
  end in
  let module M = Map.Make (O) in
  let nodes = Array.of_list list in
  let map =
    let rec loop map i =
      if i = Array.length nodes then map
      else loop (M.add nodes.(i) (idag_of_int i) map) (i + 1)
    in
    loop M.empty 0
  in
  let nodes =
    Array.map
      (fun ip ->
        let pare =
          match Driver.get_parents (pget conf base ip) with
          | Some ifam -> (
              let c = Driver.foi base ifam in
              let l =
                try [ M.find (Driver.get_mother c) map ] with Not_found -> []
              in
              try M.find (Driver.get_father c) map :: l with Not_found -> l)
          | None -> []
        in
        let chil =
          let u = pget conf base ip in
          Array.fold_left
            (fun chil ifam ->
              let des = Driver.foi base ifam in
              Array.fold_left
                (fun chil ip ->
                  try M.find ip map :: chil with Not_found -> chil)
                chil (Driver.get_children des))
            [] (Driver.get_family u)
        in
        let chil = List.rev chil in
        { pare; valu = Left ip; chil })
      nodes
  in
  let nodes =
    let rec loop nodes n i =
      if i = Array.length nodes then nodes
      else
        match nodes.(i) with
        | { valu = Left ip; chil; _ } ->
            let ifaml = Array.to_list (Driver.get_family (pget conf base ip)) in
            let nodes, n =
              let rec loop nodes = function
                | ifam :: ifaml -> (
                    let cpl = Driver.foi base ifam in
                    let isp = Gutil.spouse ip cpl in
                    let jdo =
                      try Some (M.find isp map) with Not_found -> None
                    in
                    match jdo with
                    | Some jd ->
                        let j = int_of_idag jd in
                        if chil = [] && nodes.(j).chil = [] then (
                          let pare = [ idag_of_int i; jd ] in
                          let d = { pare; valu = Right n; chil = [] } in
                          let nodes = Array.append nodes [| d |] in
                          let nd = idag_of_int n in
                          nodes.(i).chil <- [ nd ];
                          nodes.(j).chil <- [ nd ];
                          (nodes, n + 1))
                        else if chil <> nodes.(j).chil then (
                          List.iter
                            (fun nd ->
                              if List.mem nd nodes.(j).chil then ()
                              else
                                let n = int_of_idag nd in
                                nodes.(j).chil <- nd :: nodes.(j).chil;
                                nodes.(n).pare <- jd :: nodes.(n).pare)
                            chil;
                          List.iter
                            (fun nd ->
                              if List.mem nd chil then ()
                              else
                                let id = idag_of_int i in
                                let n = int_of_idag nd in
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
  { dag = nodes }
