module Driver = Geneweb_db.Driver
module Iper = Driver.Iper
module Sosa = Geneweb_sosa

module SosaMap = Map.Make (struct
  type t = Sosa.t

  let compare = Sosa.compare
end)

type generation = {
  gen : int;
  paths : int;
  present : int;
  fresh : int;
  loops : int;
  period : (int * int) option;
}

type ancestor = {
  ip : Driver.iper;
  first_gen : int;
  multiplicity : int;
  weight : float;
}

type profile = { generations : generation list; ancestors : ancestor list }

type link = {
  ancestor : Driver.iper;
  partner : Driver.iper option;
  sosa1 : Sosa.t;
  sosa2 : Sosa.t;
  len1 : int;
  len2 : int;
  contribution : float;
}

type junction = {
  sosa : Sosa.t;
  person : Driver.iper;
  links : link list;
  direct : float;
  nested : Driver.iper list;
  enclosing : Sosa.t option;
  cluster : int;
}

type cluster = {
  id : int;
  members : Sosa.t list;
  common : Iper.Set.t;
  size : int;
}

type implex = {
  persons : Driver.iper list;
  occurrences : Sosa.t list;
  closing : Sosa.t list;
  above : (int * int) Lazy.t;
}

type structure = {
  implexes : implex list;
  junctions : junction list;
  clusters : cluster list;
}

type graph = {
  ipers : Driver.iper array;
  up : int array array;
  mark : int array;
  seen : int array;
  mutable stamp : int;
}

let is_unknown p =
  Driver.Istr.is_quest (Driver.get_first_name p)
  && Driver.Istr.is_quest (Driver.get_surname p)

let parents base ip =
  match Driver.get_parents (Driver.poi base ip) with
  | Some ifam ->
      let fam = Driver.foi base ifam in
      [ Driver.get_father fam; Driver.get_mother fam ]
  | None -> []

let year cd =
  match Date.od_of_cdate cd with
  | Some (Adef.Dgreg (d, _)) -> Some d.Adef.year
  | _ -> None

let span p =
  let pick a b = match a with Some _ -> a | None -> b in
  let early = pick (year (Driver.get_birth p)) (year (Driver.get_baptism p)) in
  let late =
    pick
      (match Driver.get_death p with Def.Death (_, cd) -> year cd | _ -> None)
      (match Driver.get_burial p with
      | Def.Buried cd | Def.Cremated cd -> year cd
      | Def.UnknownBurial -> None)
  in
  match (early, late) with
  | Some a, Some b -> Some (min a b, max a b)
  | Some a, None | None, Some a -> Some (a, a)
  | None, None -> None

let merge a b =
  match (a, b) with
  | Some (a1, a2), Some (b1, b2) -> Some (min a1 b1, max a2 b2)
  | x, None | None, x -> x

type tally = {
  t_gen : int;
  t_span : (int * int) option;
  mutable t_paths : int;
  mutable t_weight : float;
}

let profile base root =
  let seen = Iper.Table.create 4096 in
  let unknown = Iper.Table.create 64 in
  let register gen ip =
    match Iper.Table.find_opt seen ip with
    | Some t -> Some (t, false)
    | None when Iper.Table.mem unknown ip -> None
    | None ->
        let p = Driver.poi base ip in
        if is_unknown p then (
          Iper.Table.replace unknown ip ();
          None)
        else
          let t =
            { t_gen = gen; t_span = span p; t_paths = 0; t_weight = 0. }
          in
          Iper.Table.replace seen ip t;
          Some (t, true)
  in
  let rec loop gen curr fresh_prev acc =
    let next = Iper.Table.create (2 * Iper.Table.length curr) in
    let fresh = Iper.Table.create 64 in
    let paths = ref 0 in
    let edges = ref 0 in
    Iper.Table.iter
      (fun child n ->
        let from_fresh = Iper.Table.mem fresh_prev child in
        List.iter
          (fun ip ->
            match register gen ip with
            | None -> ()
            | Some (t, is_new) ->
                if is_new then Iper.Table.replace fresh ip ();
                if from_fresh then incr edges;
                paths := !paths + n;
                t.t_paths <- t.t_paths + n;
                t.t_weight <- t.t_weight +. ldexp (float n) (-gen);
                Iper.Table.replace next ip
                  (n + Option.value ~default:0 (Iper.Table.find_opt next ip)))
          (parents base child))
      curr;
    if Iper.Table.length next = 0 then List.rev acc
    else
      let period =
        Iper.Table.fold
          (fun ip _ span -> merge span (Iper.Table.find seen ip).t_span)
          next None
      in
      let fresh_n = Iper.Table.length fresh in
      let g =
        {
          gen;
          paths = !paths;
          present = Iper.Table.length next;
          fresh = fresh_n;
          loops = !edges - fresh_n;
          period;
        }
      in
      loop (gen + 1) next fresh (g :: acc)
  in
  let single v =
    let t = Iper.Table.create 1 in
    Iper.Table.replace t root v;
    t
  in
  let generations = loop 1 (single 1) (single ()) [] in
  let ancestors =
    Iper.Table.fold
      (fun ip t acc ->
        {
          ip;
          first_gen = t.t_gen;
          multiplicity = t.t_paths;
          weight = t.t_weight;
        }
        :: acc)
      seen []
  in
  { generations; ancestors }

let walk base root =
  let first = Iper.Table.create 4096 in
  let index = Iper.Table.create 4096 in
  let unknown = Iper.Table.create 64 in
  let ups = ref [] in
  let add ip s =
    Iper.Table.replace first ip s;
    Iper.Table.replace index ip (Iper.Table.length index)
  in
  add root Sosa.one;
  let visit (next, at, roots) s ip =
    match Iper.Table.find_opt first ip with
    | Some t -> (next, at, (ip, s, t) :: roots)
    | None ->
        if Iper.Table.mem unknown ip || is_unknown (Driver.poi base ip) then (
          Iper.Table.replace unknown ip ();
          (next, at, roots))
        else (
          add ip s;
          ((s, ip) :: next, SosaMap.add s ip at, roots))
  in
  let rec loop frontier at roots =
    match frontier with
    | [] -> (at, roots)
    | _ ->
        let next, at, roots =
          List.fold_left
            (fun acc (s, ip) ->
              match parents base ip with
              | [ father; mother ] as l ->
                  let s = Sosa.twice s in
                  let acc = visit (visit acc s father) (Sosa.inc s 1) mother in
                  ups := (ip, List.filter (Iper.Table.mem index) l) :: !ups;
                  acc
              | _ -> acc)
            ([], at, roots) frontier
        in
        loop (List.rev next) at roots
  in
  let at, roots =
    loop [ (Sosa.one, root) ] (SosaMap.singleton Sosa.one root) []
  in
  let n = Iper.Table.length index in
  let pos = Iper.Table.find index in
  let g =
    {
      ipers = Array.make n root;
      up = Array.make n [||];
      mark = Array.make n 0;
      seen = Array.make n 0;
      stamp = 0;
    }
  in
  Iper.Table.iter (fun ip i -> g.ipers.(i) <- ip) index;
  List.iter
    (fun (ip, l) -> g.up.(pos ip) <- Array.of_list (List.map pos l))
    !ups;
  (at, roots, unknown, g, pos)

let loop_count ?(limit = max_int) base root =
  let seen = Iper.Table.create 1024 in
  Iper.Table.replace seen root ();
  let rec loop n = function
    | [] -> n
    | _ when n >= limit -> n
    | frontier ->
        let n, next =
          List.fold_left
            (fun acc ip ->
              List.fold_left
                (fun (n, next) parent ->
                  if Iper.Table.mem seen parent then (n + 1, next)
                  else if is_unknown (Driver.poi base parent) then (n, next)
                  else (
                    Iper.Table.replace seen parent ();
                    (n, parent :: next)))
                acc (parents base ip))
            (n, []) frontier
        in
        loop n next
  in
  loop 0 [ root ]

let rec junction u v =
  let c = compare (Sosa.gen u) (Sosa.gen v) in
  if c > 0 then junction (Sosa.half u) v
  else if c < 0 then junction u (Sosa.half v)
  else if Sosa.eq u v then u
  else junction (Sosa.half u) (Sosa.half v)

let coefficient base ip =
  let c = Driver.get_consang (Driver.poi base ip) in
  if c = Adef.no_consang then 0. else Adef.float_of_fix c

let make_link base at unknown ip u v =
  let family s =
    Driver.get_parents (Driver.poi base (SosaMap.find (Sosa.half s) at))
  in
  let emit partner =
    let j = junction u v in
    let len s = Sosa.gen s - Sosa.gen j - 1 in
    let rec paternal s =
      if Sosa.gen s = Sosa.gen j + 1 then Sosa.even s
      else paternal (Sosa.half s)
    in
    let len1, len2 = if paternal u then (len u, len v) else (len v, len u) in
    let w_partner =
      match partner with Some b -> 1. +. coefficient base b | None -> 0.
    in
    let w = 1. +. coefficient base ip +. w_partner in
    let contribution = ldexp w (-(len1 + len2 + 1)) in
    Some
      ( j,
        {
          ancestor = ip;
          partner;
          sosa1 = u;
          sosa2 = v;
          len1;
          len2;
          contribution;
        } )
  in
  match (family u, family v) with
  | Some fu, Some fv when Driver.Ifam.equal fu fv ->
      let fam = Driver.foi base fu in
      let father = Driver.get_father fam in
      if Iper.equal ip father then emit (Some (Driver.get_mother fam))
      else if Iper.Table.mem unknown father then emit (Some father)
      else None
  | _ -> emit None

let rec fold_pairs f acc = function
  | [] -> acc
  | u :: l -> fold_pairs f (List.fold_left (fun acc v -> f acc u v) acc l) l

let stamp g =
  g.stamp <- g.stamp + 1;
  g.stamp

let rec visit g marks s f i =
  if marks.(i) <> s then (
    marks.(i) <- s;
    f i;
    Array.iter (visit g marks s f) g.up.(i))

let above g origin =
  let s = stamp g in
  let count = ref 0 in
  let edges = ref 0 in
  List.iter
    (visit g g.mark s (fun i ->
         incr count;
         edges := !edges + Array.length g.up.(i)))
    origin;
  let n = List.length origin in
  (!count - n, !edges + n - !count)

let common_ancestors g i =
  match g.up.(i) with
  | [| father; mother |] ->
      let s = stamp g in
      visit g g.mark s ignore father;
      let common = ref [] in
      visit g g.seen (stamp g)
        (fun x -> if g.mark.(x) = s then common := x :: !common)
        mother;
      let a = stamp g in
      List.iter
        (fun x -> Array.iter (fun y -> g.mark.(y) <- a) g.up.(x))
        !common;
      List.filter_map
        (fun x -> if g.mark.(x) = a then None else Some g.ipers.(x))
        !common
  | _ -> []

let linked links =
  List.fold_left
    (fun set l ->
      let set = Iper.Set.add l.ancestor set in
      Option.fold ~none:set ~some:(fun b -> Iper.Set.add b set) l.partner)
    Iper.Set.empty links

let total links = List.fold_left (fun s l -> s +. l.contribution) 0. links

let structure base root =
  let at, roots, unknown, graph, pos = walk base root in
  let occurrences = Iper.Table.create 256 in
  List.iter
    (fun (ip, s, t) ->
      let l =
        Option.value ~default:[ t ] (Iper.Table.find_opt occurrences ip)
      in
      Iper.Table.replace occurrences ip (s :: l))
    roots;
  Iper.Table.filter_map_inplace
    (fun _ l -> Some (List.sort Sosa.compare l))
    occurrences;
  let found =
    Iper.Table.fold
      (fun ip l acc ->
        fold_pairs
          (fun acc u v ->
            match make_link base at unknown ip u v with
            | Some (j, link) ->
                SosaMap.update j
                  (fun ls -> Some (link :: Option.value ~default:[] ls))
                  acc
            | None -> acc)
          acc l)
      occurrences SosaMap.empty
  in
  let found =
    SosaMap.union
      (fun _ links _ -> Some links)
      found
      (SosaMap.filter_map
         (fun _ ip -> if coefficient base ip > 0. then Some [] else None)
         at)
  in
  let nested =
    SosaMap.mapi
      (fun j links ->
        let jp = SosaMap.find j at in
        if coefficient base jp -. total links > 1e-6 then
          let direct = linked links in
          List.filter
            (fun x -> not (Iper.Set.mem x direct))
            (common_ancestors graph (pos jp))
        else [])
      found
  in
  let parent = Iper.Table.create 256 in
  let rec find ip =
    match Iper.Table.find_opt parent ip with
    | Some p when not (Iper.equal p ip) ->
        let r = find p in
        Iper.Table.replace parent ip r;
        r
    | _ -> ip
  in
  let union a b =
    let ra = find a and rb = find b in
    if not (Iper.equal ra rb) then Iper.Table.replace parent ra rb
  in
  SosaMap.iter
    (fun j links ->
      let jp = SosaMap.find j at in
      Iper.Set.iter (union jp) (linked links);
      List.iter (union jp) (SosaMap.find j nested))
    found;
  let groups = Iper.Table.create 64 in
  SosaMap.iter
    (fun j links ->
      let r = find (SosaMap.find j at) in
      let members, common, size =
        Option.value ~default:([], Iper.Set.empty, 0)
          (Iper.Table.find_opt groups r)
      in
      let common =
        List.fold_left
          (fun set x -> Iper.Set.add x set)
          (Iper.Set.union common (linked links))
          (SosaMap.find j nested)
      in
      Iper.Table.replace groups r
        (j :: members, common, size + List.length links))
    found;
  let ids = Iper.Table.create 64 in
  let clusters =
    Iper.Table.fold (fun r g acc -> (r, g) :: acc) groups []
    |> List.stable_sort (fun (_, (_, _, a)) (_, (_, _, b)) -> compare b a)
    |> List.mapi (fun i (r, (members, common, size)) ->
        Iper.Table.replace ids r (i + 1);
        { id = i + 1; members = List.rev members; common; size })
  in
  let rec enclosing s =
    if Sosa.eq s Sosa.one then None
    else
      let s = Sosa.half s in
      if SosaMap.mem s found then Some s else enclosing s
  in
  let by_sosa a b =
    match Sosa.compare a.sosa1 b.sosa1 with
    | 0 -> Sosa.compare a.sosa2 b.sosa2
    | c -> c
  in
  let junctions =
    SosaMap.fold
      (fun j links acc ->
        let person = SosaMap.find j at in
        {
          sosa = j;
          person;
          links = List.sort by_sosa links;
          direct = total links;
          nested = SosaMap.find j nested;
          enclosing = enclosing j;
          cluster = Iper.Table.find ids (find person);
        }
        :: acc)
      found []
    |> List.rev
  in
  let closing = Iper.Table.create 256 in
  SosaMap.iter
    (fun j links ->
      Iper.Set.iter
        (fun ip ->
          Iper.Table.replace closing ip
            (j :: Option.value ~default:[] (Iper.Table.find_opt closing ip)))
        (linked links))
    found;
  let make persons occurrences =
    let ip = List.hd persons in
    {
      persons;
      occurrences;
      closing =
        List.sort_uniq Sosa.compare
          (Option.value ~default:[] (Iper.Table.find_opt closing ip));
      above = lazy (above graph (List.map pos persons));
    }
  in
  let rec pair acc = function
    | (s, f, l) :: (s', m, l') :: rest
      when Sosa.even s
           && Sosa.eq s' (Sosa.inc s 1)
           && List.equal Sosa.eq l' (List.map (fun x -> Sosa.inc x 1) l) ->
        pair (make [ f; m ] l :: acc) rest
    | (_, ip, l) :: rest -> pair (make [ ip ] l :: acc) rest
    | [] -> List.rev acc
  in
  let implexes =
    Iper.Table.fold (fun ip l acc -> (List.hd l, ip, l) :: acc) occurrences []
    |> List.sort (fun (a, _, _) (b, _, _) -> Sosa.compare a b)
    |> pair []
  in
  { implexes; junctions; clusters }
