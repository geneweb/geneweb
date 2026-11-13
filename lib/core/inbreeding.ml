module Driver = Geneweb_db.Driver
module Index = Geneweb_db.ConsangIndex
module Iper = Driver.Iper

type t = { coefficient : float option; implexes : int }

type graph = {
  ipers : Driver.iper array;
  father : int array;
  mother : int array;
  index : int Iper.Table.t;
}

exception Over_budget

let budget = 50_000_000

let is_unknown p =
  Driver.Istr.is_quest (Driver.get_first_name p)
  && Driver.Istr.is_quest (Driver.get_surname p)

let graph base root =
  let index = Iper.Table.create 4096 in
  let rec visit order ip =
    if Iper.Table.mem index ip then order
    else (
      Iper.Table.replace index ip (-1);
      match Driver.get_parents (Driver.poi base ip) with
      | Some ifam ->
          let fam = Driver.foi base ifam in
          let father = Driver.get_father fam in
          let mother = Driver.get_mother fam in
          (ip, Some (father, mother)) :: visit (visit order father) mother
      | None -> (ip, None) :: order)
  in
  let order = Array.of_list (visit [] root) in
  Array.iteri (fun i (ip, _) -> Iper.Table.replace index ip i) order;
  let parent pick =
    Array.map
      (fun (_, parents) ->
        match parents with
        | Some parents -> Iper.Table.find index (pick parents)
        | None -> -1)
      order
  in
  {
    ipers = Array.map fst order;
    father = parent fst;
    mother = parent snd;
    index;
  }

let value base entry ip =
  match entry.Index.consang with
  | Index.Known x -> Some x
  | Index.Outdated | Index.Deferred -> None
  | Index.Unset ->
      let c = Driver.get_consang (Driver.poi base ip) in
      if c = Adef.no_consang then None else Some (Adef.float_of_fix c)

let stored base ip = value base (Driver.consang_index base ip) ip

let solver base g =
  let n = Array.length g.ipers in
  let known = Array.make n nan in
  let w1 = Array.make n 0. in
  let w2 = Array.make n 0. in
  let shared = Array.make n 0. in
  let tag = Array.make n 0 in
  let stamp = ref 0 in
  let left = ref budget in
  let fresh = ref [] in
  let rec coefficient i =
    if Float.is_nan known.(i) then
      known.(i) <-
        (let ip = g.ipers.(i) in
         let entry = Driver.consang_index base ip in
         match value base entry ip with
         | Some x -> x
         | None when g.father.(i) < 0 -> 0.
         | None when entry.Index.consang = Index.Deferred -> raise Over_budget
         | None ->
             let x = kinship g.father.(i) g.mother.(i) in
             fresh := (ip, x) :: !fresh;
             x);
    known.(i)
  and kinship a b =
    let first = min a b in
    left := !left - (n - first);
    if !left < 0 then raise Over_budget;
    incr stamp;
    let current = !stamp in
    let touch i =
      if tag.(i) <> current then (
        tag.(i) <- current;
        w1.(i) <- 0.;
        w2.(i) <- 0.;
        shared.(i) <- 0.)
    in
    touch a;
    touch b;
    w1.(a) <- 1.;
    w2.(b) <- 1.;
    let total = ref 0. in
    let again = ref [] in
    for i = first to n - 1 do
      if tag.(i) = current then (
        total := !total +. (w1.(i) *. w2.(i));
        if shared.(i) <> 0. then again := (i, shared.(i)) :: !again;
        let p1 = 0.5 *. w1.(i) in
        let p2 = 0.5 *. w2.(i) in
        let up y =
          if y >= 0 then (
            touch y;
            w1.(y) <- w1.(y) +. p1;
            w2.(y) <- w2.(y) +. p2;
            shared.(y) <- shared.(y) +. (p1 *. p2))
        in
        up g.father.(i);
        up g.mother.(i))
    done;
    0.5
    *. List.fold_left
         (fun total (i, s) -> total -. (s *. (1. +. coefficient i)))
         !total !again
  in
  ((fun i -> try Some (coefficient i) with Over_budget -> None), fresh)

let count base g =
  let n = Array.length g.ipers in
  let met = Array.make n 0 in
  let total = ref 0 in
  met.(0) <- 1;
  for i = 0 to n - 1 do
    if met.(i) > 0 then
      List.iter
        (fun y ->
          if y >= 0 then
            if met.(y) = 0 then
              met.(y) <-
                (if is_unknown (Driver.poi base g.ipers.(y)) then -1 else 1)
            else if met.(y) = 1 then (
              met.(y) <- 2;
              incr total))
        [ g.father.(i); g.mother.(i) ]
  done;
  !total

let known (ip, x) = (ip, fun e -> { e with Index.consang = Index.Known x })

let store base updates =
  match updates with
  | [] -> ()
  | _ ->
      Driver.cache_consang_index base (fun get set ->
          List.iter (fun (ip, f) -> set ip (f (get ip))) updates)

let of_person base ip =
  let entry = Driver.consang_index base ip in
  let given_up = entry.Index.consang = Index.Deferred in
  match (value base entry ip, entry.Index.implexes) with
  | (Some _ as coefficient), Some implexes -> { coefficient; implexes }
  | None, Some implexes when given_up -> { coefficient = None; implexes }
  | coefficient, implexes ->
      let g = graph base ip in
      let solve, fresh = solver base g in
      let coefficient =
        if given_up || coefficient <> None then coefficient else solve 0
      in
      let updates = List.rev_map known !fresh in
      let updates =
        if given_up || coefficient <> None then updates
        else (ip, fun e -> { e with Index.consang = Index.Deferred }) :: updates
      in
      let implexes, updates =
        match implexes with
        | Some n -> (n, updates)
        | None ->
            let n = count base g in
            ( n,
              if Array.length g.ipers > 1 then
                (ip, fun e -> { e with Index.implexes = Some n }) :: updates
              else updates )
      in
      store base updates;
      { coefficient; implexes }

let of_ancestry base root k =
  let g = graph base root in
  let solve, fresh = solver base g in
  let result =
    k (fun ip -> Option.bind (Iper.Table.find_opt g.index ip) solve)
  in
  store base (List.rev_map known !fresh);
  result
