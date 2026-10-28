module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

type place = { istr : Driver.istr; raw : string; uses : int }

type context = {
  parents : string list;
  suburb : bool;
  places : place list;
  c_uses : int;
}

type entry = {
  term : string;
  key : string;
  cluster : string;
  contexts : context list;
  nb_places : int;
  uses : int;
}

let cluster_key term = Name.abbrev (Name.lower term)

let place_uses conf base =
  let counts : (Driver.istr, int ref) Hashtbl.t = Hashtbl.create 4096 in
  let count istrs =
    istrs
    |> List.filter (fun i -> not (Driver.Istr.is_empty i))
    |> List.sort_uniq compare
    |> List.iter (fun i ->
        match Hashtbl.find_opt counts i with
        | Some r -> incr r
        | None -> Hashtbl.add counts i (ref 1))
  in
  Collection.iter
    (fun ip ->
      let p = Util.pget conf base ip in
      count
        (Driver.get_birth_place p :: Driver.get_baptism_place p
       :: Driver.get_death_place p :: Driver.get_burial_place p
        :: List.map (fun e -> e.Def.epers_place) (Driver.get_pevents p)))
    (Driver.ipers base);
  Collection.iter
    (fun ifam ->
      let f = Driver.foi base ifam in
      count
        (Driver.get_marriage_place f
        :: List.map (fun e -> e.Def.efam_place) (Driver.get_fevents f)))
    (Driver.ifams base);
  Hashtbl.fold (fun i r acc -> (i, !r) :: acc) counts []

let compare_place p1 p2 = Place.compare_places p1.raw p2.raw

let compare_context c1 c2 =
  match Mutil.list_compare Gutil.alphabetic_order c1.parents c2.parents with
  | 0 -> compare c1.suburb c2.suburb
  | x -> x

let of_places inverted places =
  (* term -> (parents, suburb) -> places *)
  let terms : (string, (string list * bool, place list ref) Hashtbl.t) Hashtbl.t
      =
    Hashtbl.create 4096
  in
  let add term ctx p =
    let ctxs =
      match Hashtbl.find_opt terms term with
      | Some h -> h
      | None ->
          let h = Hashtbl.create 4 in
          Hashtbl.add terms term h;
          h
    in
    match Hashtbl.find_opt ctxs ctx with
    | Some l -> if not (List.memq p !l) then l := p :: !l
    | None -> Hashtbl.add ctxs ctx (ref [ p ])
  in
  List.iter
    (fun (istr, raw, uses) ->
      let p = { istr; raw; uses } in
      let rec loop parents = function
        | [] -> ()
        | (c : Place.component) :: rest ->
            if c.text <> "" then add c.text (List.rev parents, c.suburb) p;
            loop (if c.suburb then parents else c.text :: parents) rest
      in
      loop [] (Place.components inverted raw))
    places;
  Hashtbl.fold
    (fun term ctxs acc ->
      let contexts =
        Hashtbl.fold
          (fun (parents, suburb) l acc ->
            let places = List.sort compare_place !l in
            let c_uses =
              List.fold_left (fun n (p : place) -> n + p.uses) 0 places
            in
            { parents; suburb; places; c_uses } :: acc)
          ctxs []
        |> List.sort compare_context
      in
      let nb_places, uses =
        List.fold_left
          (fun (n, u) c -> (n + List.length c.places, u + c.c_uses))
          (0, 0) contexts
      in
      let key = Name.lower term in
      { term; key; cluster = cluster_key term; contexts; nb_places; uses }
      :: acc)
    terms []
  |> List.sort (fun e1 e2 ->
      match Gutil.alphabetic_order e1.term e2.term with
      | 0 -> compare e1.term e2.term
      | x -> x)

let build conf base =
  place_uses conf base
  |> List.map (fun (istr, uses) -> (istr, Driver.sou base istr, uses))
  |> of_places (Place.places_inverted conf)

let clusters entries =
  let h = Hashtbl.create 1024 in
  List.iter
    (fun e ->
      match Hashtbl.find_opt h e.cluster with
      | Some l -> l := e :: !l
      | None -> Hashtbl.add h e.cluster (ref [ e ]))
    entries;
  let uses l = List.fold_left (fun n e -> n + e.uses) 0 l in
  Hashtbl.fold
    (fun k l acc ->
      match !l with
      | [] | [ _ ] -> acc
      | l ->
          let l =
            List.sort (fun a b -> compare (b.uses, a.term) (a.uses, b.term)) l
          in
          (k, l) :: acc)
    h []
  |> List.sort (fun (k1, l1) (k2, l2) ->
      match compare (uses l2) (uses l1) with 0 -> compare k1 k2 | x -> x)
