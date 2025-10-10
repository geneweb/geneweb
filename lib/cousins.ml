(* Copyright (c) 1998-2007 INRIA *)

open Def
open Util
module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

module IperSet = Set.Make (struct
  type t = Driver.iper

  let compare = Driver.Iper.compare
end)

module CoordMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

(* le cousin, liste des familles entre lui et l'ancêtre, l'ancêtre, level *)
(* TODO see if level is the same as List.length ifam list *)
type one_cousin =
  Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int

type cousins_i_j = one_cousin list

type cousins_sparse = {
  data : one_cousin list CoordMap.t;
  dates : (int * int) CoordMap.t;
  max_i : int;
  max_j : int;
}

let empty_sparse =
  { data = CoordMap.empty; dates = CoordMap.empty; max_i = 0; max_j = 0 }

let extract_level sparse level =
  let data = CoordMap.filter (fun (i, _) _ -> i = level) sparse.data in
  let dates = CoordMap.filter (fun (i, _) _ -> i = level) sparse.dates in
  let max_j = CoordMap.fold (fun (_, j) _ acc -> max j acc) data 0 in
  { data; dates; max_i = level; max_j }

let merge_sparse s1 s2 =
  let data = CoordMap.union (fun _ _ v2 -> Some v2) s1.data s2.data in
  let dates = CoordMap.union (fun _ _ v2 -> Some v2) s1.dates s2.dates in
  let max_i = max s1.max_i s2.max_i in
  let max_j = max s1.max_j s2.max_j in
  { data; dates; max_i; max_j }

let merge_sparse_list = List.fold_left merge_sparse empty_sparse

let get_cell sparse i j =
  CoordMap.find_opt (i, j) sparse.data |> Option.value ~default:[]

let get_dates sparse i j =
  CoordMap.find_opt (i, j) sparse.dates |> Option.value ~default:(0, 0)

let set_cell sparse i j value dates_val =
  {
    data = CoordMap.add (i, j) value sparse.data;
    dates = CoordMap.add (i, j) dates_val sparse.dates;
    max_i = max sparse.max_i i;
    max_j = max sparse.max_j j;
  }

let default_max_cnt = 2000

let max_cousin_level conf =
  let default_max_cousin_lvl = 6 in
  try int_of_string (List.assoc "max_cousins_level" conf.Config.base_env)
  with Not_found | Failure _ -> default_max_cousin_lvl

let children_of base u =
  Array.fold_right
    (fun ifam list ->
      let des = Driver.foi base ifam in
      Array.fold_right List.cons (Driver.get_children des) list)
    (Driver.get_family u) []

let children_of_fam base ifam =
  Array.to_list (Driver.get_children @@ Driver.foi base ifam)

let siblings_by conf base iparent ip =
  let list = children_of base (pget conf base iparent) in
  List.filter (( <> ) ip) list

let merge_siblings l1 l2 =
  let l =
    let rec rev_merge r = function
      | [] -> r
      | ((v, _) as x) :: l ->
          rev_merge (if List.mem_assoc v r then r else x :: r) l
    in
    rev_merge (List.rev l1) l2
  in
  List.rev l

let siblings conf base ip =
  match Driver.get_parents (pget conf base ip) with
  | None -> []
  | Some ifam ->
      let cpl = Driver.foi base ifam in
      let fath_sib =
        List.map
          (fun ip -> (ip, (Driver.get_father cpl, Male)))
          (siblings_by conf base (Driver.get_father cpl) ip)
      in
      let moth_sib =
        List.map
          (fun ip -> (ip, (Driver.get_mother cpl, Female)))
          (siblings_by conf base (Driver.get_mother cpl) ip)
      in
      merge_siblings fath_sib moth_sib

let rec has_desc_lev conf base lev u =
  if lev <= 1 then true
  else
    Array.exists
      (fun ifam ->
        let des = Driver.foi base ifam in
        Array.exists
          (fun ip -> has_desc_lev conf base (lev - 1) (pget conf base ip))
          (Driver.get_children des))
      (Driver.get_family u)

let br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1

(* Algorithms *)

let sibling_has_desc_lev conf base lev (ip, _) =
  has_desc_lev conf base lev (pget conf base ip)

(* begin cousins *)

let cousins_table = Array.make_matrix 1 1 []
let tm = Unix.localtime (Unix.time ())
let today_year = tm.Unix.tm_year + 1900
let cousins_t : cousins_sparse option ref = ref None
let mal = 12
let mdl = 12

let update_min_max (min, max) date =
  ((if date < min then date else min), if date > max then date else max)

(* find the max ancestor level for some individual *)
(* if max_anc_level in .gwf has no value, use supplied parameter *)
let max_ancestor_level conf base ip max_lvl =
  let max_lvl =
    match List.assoc_opt "max_anc_level" conf.Config.base_env with
    | Some v when v <> "" -> int_of_string v
    | _ -> max_lvl
  in
  let x = ref 0 in
  let mark =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
  in
  let rec loop level ip =
    (* Ne traite pas l'index s'il a déjà été traité. *)
    (* Pose surement probleme pour des implexes. *)
    if not @@ Collection.Marker.get mark ip then (
      (* Met à jour le tableau d'index pour indiquer que l'index est traité. *)
      Collection.Marker.set mark ip true;
      x := max !x level;
      if !x <> max_lvl then
        match Driver.get_parents (pget conf base ip) with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            loop (succ level) (Driver.get_father cpl);
            loop (succ level) (Driver.get_mother cpl)
        | _ -> x := max !x level)
  in
  loop 0 ip;
  !x

(* find the max descendant level for some individual *)
(* if max_desc_level in .gwf has no value, use supplied parameter *)
let max_descendant_level conf base ip max_lvl =
  let max_lvl =
    match List.assoc_opt "max_desc_level" conf.Config.base_env with
    | Some v when v <> "" -> int_of_string v
    | _ -> max_lvl
  in
  let childs_of_ip ip =
    let faml = Array.to_list (Driver.get_family (Driver.poi base ip)) in
    (* accumuler tous les enfants de ip *)
    let rec loop2 acc faml =
      match faml with
      | [] -> acc
      | ifam :: faml ->
          let children =
            Array.to_list (Driver.get_children (Driver.foi base ifam))
          in
          loop2 (children @ acc) faml
    in
    loop2 [] faml
  in
  let rec loop0 acc l lev =
    match l with
    | [] -> if lev < max_lvl then loop0 [] acc (lev + 1) else lev
    | ip :: l -> loop0 (childs_of_ip ip @ acc) l lev
  in
  loop0 [] [ ip ] 0

let get_min_max_dates base l =
  let rec loop (min, max) = function
    | [] -> (min, max)
    | one_cousin :: l -> (
        let ip, _, _, _ = one_cousin in
        let not_dead = Driver.get_death (Driver.poi base ip) = NotDead in
        let birth_date, death_date, _ =
          Gutil.get_birth_death_date (Driver.poi base ip)
        in
        match (birth_date, death_date) with
        | Some (Dgreg (b, _)), Some (Dgreg (d, _)) ->
            let birth =
              match b.prec with
              | After | Before | About | Maybe | OrYear _ | YearInt _ -> false
              | _ -> true
            in
            let death =
              match d.prec with
              | After | Before | About | Maybe | OrYear _ | YearInt _ -> false
              | _ -> true
            in
            if birth && death then
              let min, max = update_min_max (min, max) b.year in
              let min, max = update_min_max (min, max) d.year in
              loop (min, max) l
            else if birth && not death then
              loop (update_min_max (min, max) b.year) l
            else if (not birth) && death then
              loop (update_min_max (min, max) d.year) l
            else loop (min, max) l
        | Some (Dgreg (b, _)), _ -> (
            match b.prec with
            | After | Before | About | Maybe | OrYear _ | YearInt _ ->
                if not_dead then loop (update_min_max (min, max) today_year) l
                else loop (min, max) l
            | _ ->
                let min, max = update_min_max (min, max) b.year in
                if not_dead then loop (update_min_max (min, max) today_year) l
                else loop (min, max) l)
        | _, Some (Dgreg (d, _)) -> (
            match d.prec with
            | After | Before | About | Maybe | OrYear _ | YearInt _ ->
                loop (min, max) l
            | _ -> loop (update_min_max (min, max) d.year) l)
        | _, _ -> loop (min, max) l)
  in
  loop (10000, -10000) l

let rec ascendants base acc l =
  match l with
  | [] -> acc
  (* TODO type for this tuple?; why list of level? *)
  | (ip, _, _, lev) :: l -> (
      match Driver.get_parents (Driver.poi base ip) with
      | None -> ascendants base acc l
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          let ifath = Driver.get_father cpl in
          let imoth = Driver.get_mother cpl in
          let acc = (ifath, [], ifath, lev + 1) :: acc in
          let acc = (imoth, [], imoth, lev + 1) :: acc in
          ascendants base acc l)

(* descendants des ip de liste1 sauf ceux présents dans liste2 *)
let descendants_aux base liste1 liste2 =
  let excluded =
    List.fold_left
      (fun set (ip, _, _, _) -> IperSet.add ip set)
      IperSet.empty liste2
  in
  let rec loop0 acc = function
    | [] -> acc
    | one_cousin :: l ->
        let ip, ifaml, ipar0, lev = one_cousin in
        let fams = Driver.get_family (Driver.poi base ip) in
        let chlds =
          Array.fold_left
            (fun acc ifam ->
              let children = Driver.get_children (Driver.foi base ifam) in
              Array.fold_right
                (fun ipch acc ->
                  if IperSet.mem ipch excluded then acc
                  else (ipch, ifam :: ifaml, ipar0, lev - 1) :: acc)
                children acc)
            [] fams
        in
        loop0 (List.rev_append chlds acc) l
  in
  loop0 [] liste1

let descendants base sparse i j =
  let liste1 = get_cell sparse i (j - 1) in
  let liste2 = if i > 0 then get_cell sparse (i - 1) (j - 1) else [] in
  descendants_aux base liste1 liste2

let init_cousins_cnt conf base p =
  let max_a_l = max_ancestor_level conf base (Driver.get_iper p) mal in
  let max_a_l =
    match p_getenv conf.Config.env "v" with
    | Some v -> int_of_string v
    | None -> max_a_l
  in
  let build_level_0 () =
    let () = Driver.load_ascends_array base in
    let () = Driver.load_couples_array base in
    let initial_list =
      [ (Driver.get_iper p, [ Driver.Ifam.dummy ], Driver.Iper.dummy, 0) ]
    in
    let initial_dates = get_min_max_dates base initial_list in
    let sparse = set_cell empty_sparse 0 0 initial_list initial_dates in
    let rec loop0 j s =
      let liste = descendants base s 0 j in
      if liste = [] then s
      else
        let dates = get_min_max_dates base liste in
        loop0 (j + 1) (set_cell s 0 j liste dates)
    in
    loop0 1 sparse
  in

  let build_level_i cumul_sparse i =
    let rec loop2 j s =
      let cell_prev = get_cell s i (j - 1) in
      if cell_prev = [] then s
      else
        let liste = descendants base s i j in
        let dates = get_min_max_dates base liste in
        let s = set_cell s i j liste dates in
        if liste = [] then s else loop2 (j + 1) s
    in
    let cell = get_cell cumul_sparse (i - 1) 0 in
    if cell = [] then cumul_sparse
    else
      let liste = ascendants base [] cell in
      if liste = [] then cumul_sparse
      else
        let dates = get_min_max_dates base liste in
        let s = set_cell cumul_sparse i 0 liste dates in
        loop2 1 s
  in
  let fn = Name.strip_lower @@ Driver.sou base @@ Driver.get_surname p in
  let sn = Name.strip_lower @@ Driver.sou base @@ Driver.get_first_name p in
  let occ = Driver.get_occ p in
  let key = Format.sprintf "%s.%d.%s" fn occ sn in
  let cache_dir =
    Filename.concat
      (Filename.concat (!GWPARAM.bpath conf.bname) "caches")
      "cousins_levels"
  in

  let sparse =
    match List.assoc_opt "cache_cousins_tool" conf.Config.base_env with
    | Some "yes" ->
        flush stderr;
        if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o755;

        let rec load_levels acc level =
          if level > max_a_l then merge_sparse_list (List.rev acc)
          else
            let cache_file =
              Filename.concat cache_dir (Printf.sprintf "%s_level_%d" key level)
            in
            let level_sparse =
              try
                let cached_key, cached_lev, partial =
                  Mutil.read_or_create_value cache_file (fun () ->
                      let cumul = merge_sparse_list (List.rev acc) in
                      let full =
                        if level = 0 then build_level_0 ()
                        else build_level_i cumul level
                      in
                      let partial = extract_level full level in
                      (key, level, partial))
                in
                if cached_key = key && cached_lev = level then partial
                else (
                  Sys.remove cache_file;
                  let cumul = merge_sparse_list (List.rev acc) in
                  let full =
                    if level = 0 then build_level_0 ()
                    else build_level_i cumul level
                  in
                  let partial = extract_level full level in
                  ignore
                    (Mutil.read_or_create_value cache_file (fun () ->
                         (key, level, partial)));
                  partial)
              with _ ->
                if Sys.file_exists cache_file then Sys.remove cache_file;
                let cumul = merge_sparse_list (List.rev acc) in
                let full =
                  if level = 0 then build_level_0 ()
                  else build_level_i cumul level
                in
                extract_level full level
            in
            load_levels (level_sparse :: acc) (level + 1)
        in
        load_levels [] 0
    | _ ->
        flush stderr;
        let rec build_all i s =
          if i > max_a_l then s else build_all (i + 1) (build_level_i s i)
        in
        build_all 1 (build_level_0 ())
  in

  cousins_t := Some sparse;
  flush stderr;
  sparse

(* for cousins_dates.(l1).(l2) determine min or max date *)
let min_max_date conf base p min_max l1 l2 =
  let sparse =
    match !cousins_t with Some s -> s | None -> init_cousins_cnt conf base p
  in
  let i = try int_of_string l1 with Failure _ -> -1 in
  let j = try int_of_string l2 with Failure _ -> -1 in
  match (i, j) with
  | -1, _ | _, -1 -> None
  | _, _ ->
      let min, max = get_dates sparse i j in
      if min = 0 && max = 0 then None
      else if min_max then Some min
      else Some max

(* determine non empty max ancestor level (max_i)
   and non empty max descendant level
*)
let max_l1_l2 conf base p =
  let sparse =
    match !cousins_t with Some s -> s | None -> init_cousins_cnt conf base p
  in
  let max_a =
    let rec loop i =
      if i > sparse.max_i then i - 1
      else if get_cell sparse i 0 <> [] then loop (i + 1)
      else i - 1
    in
    max 0 (loop 0)
  in
  let max_d =
    CoordMap.fold (fun (i, j) _cell acc -> max acc (j - i)) sparse.data 0
  in
  (max_a, max_d)

let cousins_l1_l2_aux conf base l1 l2 p =
  let il1 = int_of_string l1 in
  let il2 = int_of_string l2 in
  let sparse =
    match !cousins_t with Some s -> s | None -> init_cousins_cnt conf base p
  in
  Some (get_cell sparse il1 il2)

(* create a new list of (ip, (ifamll, iancl, cnt), lev) from one_cousin list *)
let cousins_fold l =
  let _same_ifaml ifl1 ifl2 =
    List.for_all2 (fun if1 if2 -> if1 = if2) ifl1 ifl2
  in
  let l = List.sort compare l in
  let rec loop first acc (ip0, (ifaml0, iancl0, cnt0), lev0) = function
    | one_cousin :: l ->
        let ip, ifaml, ianc, lev = one_cousin in
        if ip = ip0 then
          loop false acc
            ( ip,
              ( ifaml :: ifaml0,
                (if List.mem ianc iancl0 then iancl0 else ianc :: iancl0),
                cnt0 + 1 ),
              lev :: lev0 )
            l
        else
          loop false
            (if first || cnt0 = 0 then acc
             else (ip0, (ifaml0, iancl0, cnt0), lev0) :: acc)
            (ip, ([ ifaml ], [ ianc ], 1), [ lev ])
            l
    | [] ->
        if first || cnt0 = 0 then acc
        else (ip0, (ifaml0, iancl0, cnt0), lev0) :: acc
  in
  loop false [] (Driver.Iper.dummy, ([], [], 0), [ 0 ]) l

let cousins_implex_cnt conf base l1 l2 p =
  let il1 = int_of_string l1 in
  let il2 = int_of_string l2 in
  let sparse =
    match !cousins_t with Some s -> s | None -> init_cousins_cnt conf base p
  in
  let cousl0 = cousins_fold (get_cell sparse il1 il2) in
  let rec loop0 cousl cnt =
    match cousl with
    | [] -> cnt
    | (ip, _, _) :: cousl ->
        loop0 cousl
          (let rec loop1 cnt j =
             if j = 0 then cnt
             else
               loop1
                 (let cousl_j = get_cell sparse il1 j in
                  let rec loop2 cousl_j cnt =
                    match cousl_j with
                    | [] -> cnt
                    | one_cousin :: cousl_j ->
                        let ipj, _, _, _ = one_cousin in
                        if ip = ipj then loop2 cousl_j (cnt + 1)
                        else loop2 cousl_j cnt
                  in
                  loop2 cousl_j cnt)
                 (j - 1)
           in
           loop1 cnt (il2 - 1))
  in
  loop0 cousl0 0

let asc_cnt_t = ref None
let desc_cnt_t = ref None

(* tableau des ascendants de p *)
let init_asc_cnt conf base p =
  let max_a_l = max_ancestor_level conf base (Driver.get_iper p) mal in
  match !asc_cnt_t with
  | Some t -> t
  | None ->
      let t' =
        let asc_cnt = Array.make (max_a_l + 2) [] in
        asc_cnt.(0) <-
          [ (Driver.get_iper p, [ Driver.Ifam.dummy ], Driver.Iper.dummy, 0) ];
        for i = 1 to max_a_l do
          asc_cnt.(i) <- ascendants base [] asc_cnt.(i - 1)
        done;
        asc_cnt
      in
      asc_cnt_t := Some t';
      t'

(* tableau des ascendants de p *)
let init_desc_cnt conf base p =
  let max_d_l = max_descendant_level conf base (Driver.get_iper p) mdl in
  match !desc_cnt_t with
  | Some t -> t
  | None ->
      let t' =
        let desc_cnt = Array.make (max_d_l + 2) [] in
        desc_cnt.(0) <-
          [ (Driver.get_iper p, [ Driver.Ifam.dummy ], Driver.Iper.dummy, 0) ];
        for i = 1 to min max_d_l (Array.length desc_cnt - 1) do
          desc_cnt.(i) <- descendants_aux base desc_cnt.(i - 1) []
        done;
        desc_cnt
      in
      desc_cnt_t := Some t';
      t'

let anc_cnt_aux conf base lev at_to p =
  let cous = Hashtbl.create 10000 in
  let asc_cnt =
    match !asc_cnt_t with Some t -> t | None -> init_asc_cnt conf base p
  in
  if at_to then if lev < Array.length asc_cnt then Some asc_cnt.(lev) else None
  else
    let rec loop i =
      if i > lev || i >= Array.length asc_cnt - 1 then
        Some (Hashtbl.fold (fun _k v acc -> v :: acc) cous [])
      else (
        (* several cousins records with same ip, different faml! *)
        List.iter
          (fun (ip, faml, ianc, lvl) ->
            Hashtbl.replace cous ip (ip, faml, ianc, lvl))
          asc_cnt.(i);
        loop (i + 1))
    in
    loop 1

let desc_cnt_aux conf base lev at_to p =
  let cous = Hashtbl.create 10000 in
  let desc_cnt =
    match !desc_cnt_t with Some t -> t | None -> init_desc_cnt conf base p
  in
  if at_to then
    if lev < Array.length desc_cnt then Some desc_cnt.(lev) else None
  else
    let rec loop i =
      if i > lev || i > Array.length desc_cnt - 1 then
        Some (Hashtbl.fold (fun _k v acc -> v :: acc) cous [])
      else (
        (* several cousins records with same ip, different faml! *)
        List.iter
          (fun (ip, faml, ianc, lvl) ->
            Hashtbl.replace cous ip (ip, faml, ianc, lvl))
          desc_cnt.(i);
        loop (i + 1))
    in
    loop 0

(* end cousins *)
