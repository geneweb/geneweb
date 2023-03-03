(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb
open Util

let default_max_cnt = 2000

let max_cousin_level conf base p =
  let default_max_cousin_lvl = 6 in
  let max_lvl =
    try int_of_string (List.assoc "max_cousins_level" conf.Config.base_env)
    with Not_found | Failure _ -> default_max_cousin_lvl
  in
  Util.max_ancestor_level conf base (get_iper p) max_lvl + 1

let children_of base u =
  Array.fold_right
    (fun ifam list ->
      let des = foi base ifam in
      Array.fold_right List.cons (get_children des) list)
    (get_family u) []

let children_of_fam base ifam = Array.to_list (get_children @@ foi base ifam)

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
  match get_parents (pget conf base ip) with
  | None -> []
  | Some ifam ->
      let cpl = foi base ifam in
      let fath_sib =
        List.map
          (fun ip -> (ip, (get_father cpl, Male)))
          (siblings_by conf base (get_father cpl) ip)
      in
      let moth_sib =
        List.map
          (fun ip -> (ip, (get_mother cpl, Female)))
          (siblings_by conf base (get_mother cpl) ip)
      in
      merge_siblings fath_sib moth_sib

let rec has_desc_lev conf base lev u =
  if lev <= 1 then true
  else
    Array.exists
      (fun ifam ->
        let des = foi base ifam in
        Array.exists
          (fun ip -> has_desc_lev conf base (lev - 1) (pget conf base ip))
          (get_children des))
      (get_family u)

let br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1

(* Algorithms *)

let sibling_has_desc_lev conf base lev (ip, _) =
  has_desc_lev conf base lev (pget conf base ip)

(* ---- *)

let get_descendants_at_level base p lev2 =
  match lev2 with
  | 0 -> []
  | n ->
      (* gather corresponding families in ifam_ht *)
      let ifam_ht = Hashtbl.create 1024 in
      let rec loop lev fam =
        Array.iter
          (fun ifam ->
            if lev < n then
              let children = get_children (foi base ifam) in
              Array.iter
                (fun ch -> loop (lev + 1) (get_family (poi base ch)))
                children
            else Hashtbl.replace ifam_ht ifam ())
          fam
      in
      loop 1 (get_family p);
      (* build the list of descendants from the families *)
      Hashtbl.fold
        (fun ifam () acc ->
          let childrens = get_children (foi base ifam) in
          Array.fold_left (fun acc ch -> ch :: acc) acc childrens)
        ifam_ht []

let cousins_t = ref None
let cousins_dates_t = ref None

let rec ascendants base acc l i =
  match l with
  | [] -> acc
  (* TODO type for this tuple?; why list of level? *)
  | (ip, _, _, lev :: _ll) :: l -> (
      match get_parents (poi base ip) with
      | None -> ascendants base acc l i
      | Some ifam ->
          let cpl = foi base ifam in
          let ifath = get_father cpl in
          let imoth = get_mother cpl in
          let acc = [ (ifath, [], ifath, [ lev + 1 ]) ] @ acc in
          let acc = [ (imoth, [], imoth, [ lev + 1 ]) ] @ acc in
          ascendants base acc l i)
  | _ :: l ->
      !GWPARAM.syslog `LOG_WARNING
        "Unexpected empty level list in ascend computation\n";
      ascendants base acc l i

(* descendants des ip de liste1 sauf ceux présents dans liste2 *)
let descendants_aux base liste1 liste2 =
  let liste2 = List.map (fun (ip, _, _, _) -> ip) liste2 in
  let rec loop0 acc = function
    | [] -> acc
    | (ip, ifaml, ipar0, lev :: _ll) :: l ->
        let fams = Array.to_list (get_family (poi base ip)) in
        let chlds =
          (* accumuler tous les enfants de ip *)
          let rec loop1 acc fams =
            (* iterer sur chaque famille *)
            match fams with
            | [] -> acc
            | ifam :: fams ->
                let children =
                  let rec loop2 acc2 children =
                    match children with
                    | [] -> acc2
                    | ipch :: children ->
                        loop2
                          ((ipch, ifam :: ifaml, ipar0, [ lev - 1 ]) :: acc2)
                          children
                  in
                  loop2 [] (Array.to_list (get_children (foi base ifam)))
                in
                loop1 (acc @ children) fams
          in
          loop1 [] fams
        in
        let chlds =
          List.fold_left (* on élimine les enfants présents dans l2 *)
            (fun acc (ip, ifaml, ipar, lev) ->
              if List.mem ip liste2 then acc else (ip, ifaml, ipar, lev) :: acc)
            [] chlds
        in
        loop0 (chlds @ acc) l
    | _ :: l ->
        !GWPARAM.syslog `LOG_WARNING
          "Unexpected empty level list in descend computation\n";
        loop0 acc l
  in
  loop0 [] liste1

let descendants base cousins_cnt i j =
  let liste1 = cousins_cnt.(i).(j - 1) in
  let liste2 = if i > 0 then cousins_cnt.(i - 1).(j - 1) else [] in
  descendants_aux base liste1 liste2

let get_min_max_dates conf base l =
  let dates =
    let rec loop acc = function
      | [] -> acc
      | (ip, _, _, _) :: l -> (
          let not_dead = get_death (poi base ip) = NotDead in
          let birth_date, death_date, _ =
            Gutil.get_birth_death_date (poi base ip)
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
              if birth && death then loop (b.year :: d.year :: acc) l
              else if birth && not death then loop (b.year :: acc) l
              else if (not birth) && death then loop (d.year :: acc) l
              else loop acc l
          | Some (Dgreg (b, _)), _ -> (
              match b.prec with
              | After | Before | About | Maybe | OrYear _ | YearInt _ ->
                  if not_dead then loop (conf.Config.today.year :: acc) l
                  else loop acc l
              | _ ->
                  if not_dead then loop (conf.today.year :: b.year :: acc) l
                  else loop (b.year :: acc) l)
          | _, Some (Dgreg (d, _)) -> (
              match d.prec with
              | After | Before | About | Maybe | OrYear _ | YearInt _ ->
                  loop acc l
              | _ -> loop (d.year :: acc) l)
          | _, _ ->
              if not_dead then loop (conf.today.year :: acc) l else loop acc l)
    in
    loop [] l
  in
  List.sort_uniq compare dates

(* TODO Cousins module should not have to handle `env`, it should be done in Perso *)
(* This function contaminate other making them dependent on `env` *)
let max_l1_l2_aux env =
  (* TODO duplicate of Perso.get_env *)
  let get_env v env =
    try match List.assoc v env with Vlazy l -> Lazy.force l | x -> x
    with Not_found -> Vnone
  in
  let max_l1 = match get_env "max_anc_level" env with Vint i -> i | _ -> 5 in
  let max_l2 = match get_env "max_desc_level" env with Vint i -> i | _ -> 5 in
  (max_l1 - 1, (2 * max_l1) + max_l2 + 20)

let init_cousins_cnt conf base env p =
  match (!cousins_t, !cousins_dates_t) with
  | Some t, Some d_t -> (t, d_t)
  | _, _ ->
      let t', d_t' =
        let max_l1, max_l2 = max_l1_l2_aux env in
        Printf.sprintf "******** Compute %d × %d table ********\n" max_l1 max_l2
        |> !GWPARAM.syslog `LOG_WARNING;
        (* TODO test for Sys.max_array_length *)
        let () = load_ascends_array base in
        let () = load_couples_array base in
        let cousins_cnt = Array.make_matrix (max_l2 + 1) (max_l2 + 1) [] in
        let cousins_dates = Array.make_matrix (max_l2 + 1) (max_l2 + 1) [] in
        cousins_cnt.(0).(0) <-
          [ (get_iper p, [ Gwdb.dummy_ifam ], Gwdb.dummy_iper, [ 0 ]) ];
        cousins_dates.(0).(0) <- get_min_max_dates conf base cousins_cnt.(0).(0);
        let rec loop0 j =
          (* initiate lists of direct descendants *)
          cousins_cnt.(0).(j) <- descendants base cousins_cnt 0 j;
          cousins_dates.(0).(j) <-
            get_min_max_dates conf base cousins_cnt.(0).(j);
          if j < max_l2 then loop0 (j + 1) else ()
        in
        loop0 1;
        let rec loop1 i =
          (* get ascendants *)
          cousins_cnt.(i).(0) <- ascendants base [] cousins_cnt.(i - 1).(0) i;
          cousins_dates.(i).(0) <-
            get_min_max_dates conf base cousins_cnt.(i).(0);
          let rec loop2 i j =
            (* get descendants of c1, except persons of previous level (c2) *)
            cousins_cnt.(i).(j) <- descendants base cousins_cnt i j;
            cousins_dates.(i).(j) <-
              get_min_max_dates conf base cousins_cnt.(i).(j);
            if j < max_l2 then loop2 i (j + 1)
            else if i < max_l1 then loop1 (i + 1)
            else ()
          in
          loop2 i 1
        in
        loop1 1;
        (cousins_cnt, cousins_dates)
      in
      cousins_t := Some t';
      cousins_dates_t := Some d_t';
      (t', d_t')

let max_l1_l2 conf base env p =
  let cousins_cnt, _cousins_dates =
    match (!cousins_t, !cousins_dates_t) with
    | Some t, Some d_t -> (t, d_t)
    | _, _ -> init_cousins_cnt conf base env p
  in
  let max_l1, max_l2 = max_l1_l2_aux env in
  let max_i =
    let rec loop0 i =
      if cousins_cnt.(i).(0) <> [] && i < max_l1 then loop0 (i + 1) else i
    in
    loop0 0
  in
  let rec loop i j =
    if cousins_cnt.(i).(j) <> [] then
      if j < max_l2 then loop i (j + 1) else (max_i, j - i)
    else if i < max_l1 && j < max_l2 then loop (i + 1) (j + 1)
    else (max_i, j - i)
  in
  loop 0 0

let cousins_l1_l2_aux conf base env l1 l2 p =
  let il1 = int_of_string l1 in
  let il2 = int_of_string l2 in
  let max_l1, max_l2 = max_l1_l2_aux env in
  if il1 <= max_l1 && il2 - il1 <= max_l2 then
    let cousins_cnt, _cousins_dates = init_cousins_cnt conf base env p in
    (* gros calcul *)
    Some cousins_cnt.(il1).(il2)
  else None

(* create a new l (ip, (ifaml, iancl, cnt), lev) from (ip, ifaml, ipar, lev) *)
let cousins_fold l =
  let same_ifaml ifl1 ifl2 =
    List.for_all2 (fun if1 if2 -> if1 = if2) ifl1 ifl2
  in
  let l = List.sort compare l in
  let rec loop first acc (ip0, (ifaml0, iancl0, cnt0), lev0) = function
    | (ip, ifaml, ianc, lev) :: l when ip = ip0 && same_ifaml ifaml ifaml0 ->
        loop false acc
          ( ip,
            ( ifaml,
              (if List.mem ianc iancl0 then iancl0 else ianc :: iancl0),
              cnt0 + 1 ),
            lev @ lev0 )
          l
    | (ip, ifaml, ianc, lev) :: l ->
        loop false
          (if first || cnt0 = 0 then acc
          else (ip0, (ifaml0, iancl0, cnt0), lev0) :: acc)
          (ip, (ifaml, [ ianc ], 1), lev)
          l
    | [] ->
        if first || cnt0 = 0 then acc
        else (ip0, (ifaml0, iancl0, cnt0), lev0) :: acc
  in
  loop false [] (Gwdb.dummy_iper, ([], [], 0), [ 0 ]) l

let asc_cnt_t = ref None
let desc_cnt_t = ref None
let max_anc_level_default = 5
let max_desc_level_default = 5

(* TODO should not depend on `env` *)
let init_asc_cnt base env p =
  match !asc_cnt_t with
  | Some t -> t
  | None ->
      let t' =
        let max_asc =
          match get_env "max_anc_level" env with
          | Vint i -> i
          | _ -> max_anc_level_default
        in
        let asc_cnt = Array.make (max_asc + 1) [] in
        asc_cnt.(0) <-
          [ (get_iper p, [ Gwdb.dummy_ifam ], Gwdb.dummy_iper, [ 0 ]) ];
        for i = 1 to min max_asc (Array.length asc_cnt - 1) do
          asc_cnt.(i) <- ascendants base [] asc_cnt.(i - 1) i
        done;
        asc_cnt
      in
      asc_cnt_t := Some t';
      t'

(* TODO should not depend on `env` *)
let init_asc_cnt base env p =
let init_desc_cnt base env p =
  match !desc_cnt_t with
  | Some t -> t
  | None ->
      let t' =
        let max_desc =
          match get_env "max_desc_level" env with
          | Vint i -> i
          | _ -> max_desc_level_default
        in
        let desc_cnt = Array.make (max_desc + 1) [] in
        desc_cnt.(0) <-
          [ (get_iper p, [ Gwdb.dummy_ifam ], Gwdb.dummy_iper, [ 0 ]) ];
        for i = 1 to min max_desc (Array.length desc_cnt - 1) do
          desc_cnt.(i) <- descendants_aux base desc_cnt.(i - 1) []
        done;
        desc_cnt
      in
      desc_cnt_t := Some t';
      t'

let anc_cnt_aux base env lev at_to p =
  let asc_cnt =
    match !asc_cnt_t with Some t -> t | None -> init_asc_cnt base env p
  in
  if at_to then if lev < Array.length asc_cnt then Some asc_cnt.(lev) else None
  else
    let rec loop acc i =
      if i > lev || i >= Array.length asc_cnt - 1 then Some acc
      else loop (asc_cnt.(i) @ acc) (i + 1)
    in
    loop [] 1

let desc_cnt_aux base env lev at_to p =
  let desc_cnt =
    match !desc_cnt_t with Some t -> t | None -> init_desc_cnt base env p
  in
  if at_to then
    if lev < Array.length desc_cnt then Some desc_cnt.(lev) else None
  else
    let rec loop acc i =
      if i > lev || i > Array.length desc_cnt - 1 then Some acc
      else loop (desc_cnt.(i) @ acc) (i + 1)
    in
    loop [] 0

let cous_paths_dates_aux conf base env p l1 l2 rev =
  let _cousins_cnt, cousins_dates =
    match (!cousins_t, !cousins_dates_t) with
    | Some t, Some d_t -> (t, d_t)
    | _, _ -> init_cousins_cnt conf base env p
  in
  match (int_of_string_opt l1, int_of_string_opt l2) with
  | Some i, Some j -> (
      let l =
        if rev then List.rev cousins_dates.(i).(j) else cousins_dates.(i).(j)
      in
      match l with
      | [] -> null_val (* this case is legit *)
      | lvl :: _l -> str_val (string_of_int lvl))
  | _, _ -> raise Not_found
