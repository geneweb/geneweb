type layout = {
  x : int array;
  rank : int array;
  paths : int array list;
  crossings : int;
  breadth : int;
  width : int;
}

let layout ?(iterations = 24) ?(separation = 16) ?slots ~widths ~ranks ~keys
    edges =
  let n = Array.length widths in
  let count = ref n in
  let dummies = ref [] in
  let fresh r k =
    dummies := (r, k) :: !dummies;
    incr count;
    !count - 1
  in
  let paths =
    List.map
      (fun (u, v) ->
        let rec chain r acc =
          if r >= ranks.(v) then Array.of_list (List.rev (v :: acc))
          else chain (r + 1) (fresh r keys.(u) :: acc)
        in
        chain (ranks.(u) + 1) [ u ])
      edges
  in
  let total = !count in
  let extra = Array.of_list (List.rev !dummies) in
  let rank =
    Array.init total (fun i -> if i < n then ranks.(i) else fst extra.(i - n))
  in
  let key =
    Array.init total (fun i -> if i < n then keys.(i) else snd extra.(i - n))
  in
  let width = Array.init total (fun i -> if i < n then widths.(i) else 0) in
  let up = Array.make total [] in
  let down = Array.make total [] in
  List.iter
    (fun p ->
      for i = 0 to Array.length p - 2 do
        down.(p.(i)) <- p.(i + 1) :: down.(p.(i));
        up.(p.(i + 1)) <- p.(i) :: up.(p.(i + 1))
      done)
    paths;
  let depth = 1 + Array.fold_left max 0 rank in
  let layers = Array.make depth [] in
  for i = total - 1 downto 0 do
    layers.(rank.(i)) <- i :: layers.(rank.(i))
  done;
  let layers =
    Array.map
      (fun l ->
        Array.of_list (List.stable_sort (fun a b -> compare key.(a) key.(b)) l))
      layers
  in
  let pos = Array.make total 0 in
  let place layer = Array.iteri (fun i v -> pos.(v) <- i) layer in
  Array.iter place layers;
  let between l =
    let lower = Array.length layers.(l + 1) in
    let es =
      Array.fold_left
        (fun acc u ->
          List.fold_left (fun acc v -> (pos.(u), pos.(v)) :: acc) acc down.(u))
        [] layers.(l)
      |> List.sort compare
    in
    let tree = Array.make (lower + 1) 0 in
    let rec add i =
      if i <= lower then (
        tree.(i) <- tree.(i) + 1;
        add (i + (i land -i)))
    in
    let rec sum i = if i > 0 then tree.(i) + sum (i - (i land -i)) else 0 in
    fst
      (List.fold_left
         (fun (c, seen) (_, v) ->
           let c = c + seen - sum (v + 1) in
           add (v + 1);
           (c, seen + 1))
         (0, 0) es)
  in
  let crossings () =
    let c = ref 0 in
    for l = 0 to depth - 2 do
      c := !c + between l
    done;
    !c
  in
  let median v nbrs =
    match List.sort compare (List.map (fun u -> pos.(u)) nbrs) with
    | [] -> float pos.(v)
    | l ->
        let a = Array.of_list l in
        let m = Array.length a in
        if m mod 2 = 1 then float a.(m / 2)
        else float (a.((m / 2) - 1) + a.(m / 2)) /. 2.
  in
  let sort_layer l nbrs =
    let order =
      Array.map (fun v -> (median v (nbrs v), pos.(v), v)) layers.(l)
    in
    Array.sort compare order;
    layers.(l) <- Array.map (fun (_, _, v) -> v) order;
    place layers.(l)
  in
  let cross u v =
    let count nu nv =
      List.fold_left
        (fun c a ->
          List.fold_left
            (fun c b -> if pos.(a) > pos.(b) then c + 1 else c)
            c nv)
        0 nu
    in
    count up.(u) up.(v) + count down.(u) down.(v)
  in
  let rec transpose k =
    let improved = ref false in
    Array.iter
      (fun layer ->
        for i = 0 to Array.length layer - 2 do
          let u = layer.(i) and v = layer.(i + 1) in
          if cross u v > cross v u then (
            layer.(i) <- v;
            layer.(i + 1) <- u;
            pos.(v) <- i;
            pos.(u) <- i + 1;
            improved := true)
        done)
      layers;
    if !improved && k > 1 then transpose (k - 1)
  in
  let best = ref (Array.map Array.copy layers) in
  let best_c = ref (crossings ()) in
  let it = ref 0 in
  while !best_c > 0 && !it < iterations do
    if !it mod 2 = 0 then
      for l = 1 to depth - 1 do
        sort_layer l (fun v -> up.(v))
      done
    else
      for l = depth - 2 downto 0 do
        sort_layer l (fun v -> down.(v))
      done;
    transpose 8;
    let c = crossings () in
    if c < !best_c then (
      best_c := c;
      best := Array.map Array.copy layers);
    incr it
  done;
  Array.iteri
    (fun l layer ->
      layers.(l) <- layer;
      place layer)
    !best;
  let x = Array.make total 0. in
  let gap a b = float (((width.(a) + width.(b)) / 2) + separation) in
  (match slots with
  | Some slotted ->
      (* Trial 2: equal slots on the slotted layers, the others follow. *)
      let most = ref 0 in
      Array.iteri
        (fun r layer ->
          if slotted r then most := max !most (Array.length layer))
        layers;
      let span = float ((separation + Array.fold_left max 0 width) * !most) in
      Array.iteri
        (fun r layer ->
          if slotted r then
            let k = float (Array.length layer) in
            Array.iteri
              (fun i v -> x.(v) <- span *. ((2. *. float i) +. 1.) /. (2. *. k))
              layer)
        layers;
      let mean l =
        List.fold_left (fun s u -> s +. x.(u)) 0. l /. float (List.length l)
      in
      Array.iteri
        (fun r layer ->
          if not (slotted r) then
            Array.iter
              (fun v ->
                match (up.(v), down.(v)) with
                | [], [] -> ()
                | [], l | l, _ -> x.(v) <- mean l)
              layer)
        layers
  | None ->
      (* Trial 1: pull towards the neighbours, crowds the root ancestry. *)
      Array.iter
        (fun layer ->
          Array.iteri
            (fun i v ->
              x.(v) <-
                (if i = 0 then float width.(v) /. 2.
                 else x.(layer.(i - 1)) +. gap layer.(i - 1) v))
            layer)
        layers;
      let balance layer nbrs =
        let m = Array.length layer in
        if m > 0 then (
          let desired =
            Array.map
              (fun v ->
                match nbrs v with
                | [] -> x.(v)
                | l ->
                    List.fold_left (fun s u -> s +. x.(u)) 0. l
                    /. float (List.length l))
              layer
          in
          let left = Array.copy desired in
          let right = Array.copy desired in
          for i = 1 to m - 1 do
            left.(i) <-
              Float.max desired.(i) (left.(i - 1) +. gap layer.(i - 1) layer.(i))
          done;
          for i = m - 2 downto 0 do
            right.(i) <-
              Float.min desired.(i)
                (right.(i + 1) -. gap layer.(i) layer.(i + 1))
          done;
          Array.iteri (fun i v -> x.(v) <- (left.(i) +. right.(i)) /. 2.) layer)
      in
      for k = 1 to 8 do
        if k mod 2 = 1 then
          for l = 1 to depth - 1 do
            balance layers.(l) (fun v -> up.(v))
          done
        else
          for l = depth - 2 downto 0 do
            balance layers.(l) (fun v -> down.(v))
          done
      done);
  let lo = ref infinity and hi = ref neg_infinity in
  Array.iteri
    (fun v xv ->
      let half = float width.(v) /. 2. in
      lo := Float.min !lo (xv -. half);
      hi := Float.max !hi (xv +. half))
    x;
  let lo = if total = 0 then 0. else !lo in
  {
    x = Array.map (fun xv -> int_of_float (Float.round (xv -. lo))) x;
    rank;
    paths;
    crossings = !best_c;
    breadth = Array.fold_left (fun m l -> max m (Array.length l)) 0 layers;
    width = (if total = 0 then 0 else int_of_float (Float.ceil (!hi -. lo)));
  }
