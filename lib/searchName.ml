(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Logs = Geneweb_logs.Logs
module Collection = Geneweb_db.Collection

(* FIXME is IperSet the same as Geneweb_db.Driver.Iper.Set *)
module IperSet = Set.Make (struct
  type t = Driver.iper

  let compare = compare
end)

type match_source = DirectMatch | FirstNameAlias of string

type search_result_with_info = {
  iper : Driver.Iper.t;
  match_source : match_source;
}

(* FIXME this set of options needs deeper review
   for semantic and implementation *)
type opts = {
  order : bool; (* first_names should be in same order as typed *)
  all : bool; (* all first_names typed should be present *)
  case : bool; (* maintain case and accents when comparing *)
  exact1 : bool; (* fuzzy match (for the time being starts_with *)
  all_in : bool; (* all first_names should match one of the typed first_names *)
}

type search_results = {
  exact : Driver.Iper.t list; (* résultats exacts *)
  partial : Driver.Iper.t list; (* résultats partiels *)
  spouse : Driver.Iper.t list; (* résultats avec nom d'époux *)
}

(* Generate all apostrophe variants of a string *)
let generate_apostrophe_variants s =
  let apostrophes =
    [
      "'";
      (* U+0027 APOSTROPHE *)
      "\xE2\x80\x99";
      (* U+2019 RIGHT SINGLE QUOTATION MARK *)
      "\xCA\xBC";
      (* U+02BC MODIFIER LETTER APOSTROPHE *)
      "\xCA\xBB";
      (* U+02BB MODIFIER LETTER TURNED COMMA *)
    ]
  in
  let rec find_apostrophe_pos s i =
    if i >= String.length s then None
    else
      match s.[i] with
      | '\'' -> Some (i, 1)
      | '\xE2'
        when i + 2 < String.length s && s.[i + 1] = '\x80' && s.[i + 2] = '\x99'
        ->
          Some (i, 3)
      | '\xCA'
        when i + 1 < String.length s
             && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB') ->
          Some (i, 2)
      | _ -> find_apostrophe_pos s (i + 1)
  in
  match find_apostrophe_pos s 0 with
  | None -> [ s ]
  | Some (pos, len) ->
      let variants =
        List.map
          (fun apo ->
            String.sub s 0 pos ^ apo
            ^ String.sub s (pos + len) (String.length s - pos - len))
          apostrophes
      in
      List.sort_uniq String.compare variants

let has_apostrophe s =
  String.contains s '\''
  || (try
        String.index s '\xE2' |> fun i ->
        i + 2 < String.length s && s.[i + 1] = '\x80' && s.[i + 2] = '\x99'
      with Not_found -> false)
  ||
  try
    String.index s '\xCA' |> fun i ->
    i + 1 < String.length s && (s.[i + 1] = '\xBC' || s.[i + 1] = '\xBB')
  with Not_found -> false

let empty_sn_or_fn base p =
  Driver.Istr.is_empty (Driver.get_surname p)
  || Driver.Istr.is_quest (Driver.get_surname p)
  || Driver.Istr.is_empty (Driver.get_first_name p)
  || Driver.Istr.is_quest (Driver.get_first_name p)
  || Name.lower (Driver.sou base (Driver.get_surname p)) = ""
  || Name.lower (Driver.sou base (Driver.get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if
    List.exists
      (fun n -> Name.strip n = k)
      (Driver.person_misc_names base p (nobtit conf base))
  then true
  else false

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Driver.p_first_name base p) in
  let sn = Name.strip_lower (Driver.p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

(* Select individuals matching key k and stock matching alias in AliasCache *)
let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      let iper = Driver.get_iper p in
      if person_is_approx_key base p k then (
        Some.AliasCache.add_direct iper;
        p :: pl)
      else
        let k_stripped = Name.strip_lower k in
        let aliases = Driver.get_aliases p in
        let matched_alias =
          List.find_opt
            (fun alias_istr ->
              let alias_str = Driver.sou base alias_istr in
              Name.strip_lower alias_str = k_stripped)
            aliases
        in
        match matched_alias with
        | Some alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            Some.AliasCache.add_alias iper alias_str;
            p :: pl
        | None ->
            if person_is_misc_name conf base p k then (
              Some.AliasCache.add_direct iper;
              p :: pl)
            else pl)
    pl []

let split_normalize case s =
  let s = Name.abbrev s in
  let s = if case then s else Name.lower s in
  cut_words s

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | None, _ | _, None -> None
  | Some p, Some n when n <> Sosa.zero -> (
      match
        Util.branch_of_sosa conf base n (pget conf base @@ Driver.get_iper p)
      with
      | Some (p :: _) -> Some p
      | _ -> None)
  | _ -> None

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index n1 ' ' with
  | exception Not_found -> []
  | i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      (* find bearers of sn *)
      let p_of_sn_l, _ =
        Some.persons_of_fsname conf base Driver.base_strings_of_surname
          (Driver.spi_find (Driver.persons_of_surname base))
          Driver.get_surname sn
      in
      (* filter those with fn *)
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              match Util.pget_opt conf base ip with
              | None -> pl
              | Some p ->
                  let fn1_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_first_name p))
                  in
                  let fn2_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_public_name p))
                  in
                  if List.mem fn fn1_l || List.mem fn fn2_l then p :: pl else pl)
            pl ipl)
        [] p_of_sn_l

let search_key_aux aux conf base an =
  let acc = Gutil.person_not_a_key_find_all base an in
  let an, acc =
    if acc = [] then
      match Util.name_with_roman_number an with
      | Some an1 ->
          let acc = Gutil.person_ht_find_all base an1 in
          if acc = [] then (an, []) else (an1, acc)
      | None -> (an, acc)
    else (an, acc)
  in
  let acc = Mutil.filter_map (fun i -> Util.pget_opt conf base i) acc in
  let acc = aux conf base acc an in
  Gutil.sort_uniq_person_list base acc

let search_approx_key = search_key_aux select_approx_key

let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | None -> None
  | Some ip -> Util.pget_opt conf base ip

(* Gestionnaire centralisé des duplicatas
module DuplicateManager = struct
  let create () = Hashtbl.create 100
  let add_if_new ht ip =
    if Hashtbl.mem ht ip then false
    else (
      Hashtbl.add ht ip ();
      true)
  let filter_new ht ips = List.filter (add_if_new ht) ips
end

*)

(* Cache pour éviter les appels répétés à Driver.sou *)
module StringCache = struct
  let cache = Hashtbl.create 2000
  let max_size = 5000
  let hits = ref 0
  let misses = ref 0

  let get_cached base istr =
    try
      incr hits;
      Hashtbl.find cache istr
    with Not_found ->
      incr misses;
      let s = Driver.sou base istr in
      if Hashtbl.length cache < max_size then Hashtbl.add cache istr s;
      s

  let clear_if_full () =
    if Hashtbl.length cache > max_size then (
      Logs.debug (fun k ->
          k "StringCache clearing cache: %d hits, %d misses" !hits !misses);
      Hashtbl.clear cache;
      hits := 0;
      misses := 0)

  let maintenance () = clear_if_full ()
end

(* Convertit une liste de persons en liste d’ipers *)
let persons_to_ipers pl = List.map (fun p -> Driver.get_iper p) pl

(* Transformer les options en listes *)
(* Recherche par Sosa optimisée *)
let search_sosa_opt conf base query =
  match search_by_sosa conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

(* Recherche par clé optimisée *)
let search_key_opt conf base query =
  match search_by_key conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

(* "exact" means complete word equality *)
let match_fn_lists fn_l fn1_l opts =
  let normalize s = if opts.case then s else Name.lower s in
  let word_matches query_word person_word =
    let q = normalize query_word in
    let p = normalize person_word in
    if opts.exact1 then
      (* Strict exact: complete equality *)
      q = p
    else
      (* Fuzzy: substring matching *)
      Mutil.contains p q
  in
  let passes_basic_test =
    if opts.all then
      (* Every query term must find a match in person names *)
      List.for_all
        (fun query_term -> List.exists (word_matches query_term) fn1_l)
        fn_l
    else
      (* At least one query term must find a match *)
      List.exists
        (fun query_term -> List.exists (word_matches query_term) fn1_l)
        fn_l
  in
  let passes_all_in_test =
    if opts.all_in then
      List.for_all
        (fun person_name ->
          List.exists
            (fun query_term -> word_matches query_term person_name)
            fn_l)
        fn1_l
    else true
  in
  let passes_order_test =
    if opts.order then
      let rec check_in_order remaining_queries remaining_persons =
        match remaining_queries with
        | [] -> true
        | q :: qs ->
            let rec find_match_and_continue persons =
              match persons with
              | [] -> false
              | p :: rest ->
                  if word_matches q p then check_in_order qs rest
                  else find_match_and_continue rest
            in
            find_match_and_continue remaining_persons
      in
      check_in_order fn_l fn1_l
    else true
  in
  passes_basic_test && passes_all_in_test && passes_order_test

let rec list_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: list_take (n - 1) xs

let rec list_drop n = function
  | xs when n <= 0 -> xs
  | [] -> []
  | _ :: xs -> list_drop (n - 1) xs

let search_for_multiple_fn conf base fn pl opts batch_size =
  Logs.debug (fun k -> k "        search_for_multiple_fn: %s" fn);
  let bool_to_string b = if b then "true" else "false" in
  Logs.debug (fun k ->
      k "          order: %s, all: %s, case: %s, exact: %s, all_in: %s"
        (bool_to_string opts.order)
        (bool_to_string opts.all) (bool_to_string opts.case)
        (bool_to_string opts.exact1)
        (bool_to_string opts.all_in));
  let fn_l = cut_words fn in
  let result =
    let rec process_batch acc remaining =
      match remaining with
      | [] -> acc
      | _ ->
          let batch, rest =
            if List.length remaining <= batch_size then (remaining, [])
            else (list_take batch_size remaining, list_drop batch_size remaining)
          in
          let batch_results =
            List.fold_left
              (fun acc_batch p ->
                if search_reject_p conf base p then acc_batch
                else
                  let fn1_istr = Driver.get_first_name p in
                  let fn1 = StringCache.get_cached base fn1_istr in
                  let fn1_l = split_normalize opts.case fn1 in
                  let fn2_istr = Driver.get_public_name p in
                  let fn2 = StringCache.get_cached base fn2_istr in
                  let fn2_l = split_normalize opts.case fn2 in
                  if
                    match_fn_lists fn_l fn1_l opts
                    || match_fn_lists fn_l fn2_l opts
                  then p :: acc_batch
                  else acc_batch)
              [] batch
          in
          process_batch (batch_results @ acc) rest
    in
    process_batch [] pl
  in
  Logs.debug (fun k -> k "          result: %d" (List.length result));
  result

let rec search_surname conf base x =
  Logs.debug (fun k -> k "      search_surname: %s" x);
  let has_apostrophe = has_apostrophe x in
  match has_apostrophe with
  | false ->
      let exact_results = search_exact conf base [ x ] in
      if exact_results <> [] then (
        Logs.debug (fun k ->
            k "        exact1: %d results" (List.length exact_results));
        (exact_results, []))
      else (
        Logs.debug (fun k -> k "        exact1: 0, trying phonetic search");
        ([], search_phonetic conf base x))
  | true ->
      Logs.debug (fun k ->
          k "          apostrophe detected, trying exact variants first");
      let variants = generate_apostrophe_variants x in
      let exact_results = search_exact conf base variants in
      if exact_results <> [] then (
        Logs.debug (fun k ->
            k "          exact: %d results" (List.length exact_results));
        (exact_results, []))
      else (
        Logs.debug (fun k -> k "          exact: 0, trying phonetic search");
        let fallback_query = List.hd variants in
        ([], search_phonetic conf base fallback_query))

and search_exact conf base variants =
  Logs.debug (fun k -> k "      search_exact: %s" (List.hd variants));
  let exact_iperl = ref IperSet.empty in
  let found_matches = ref [] in
  List.iter
    (fun variant ->
      try
        let list, _name_inj =
          Some.persons_of_fsname conf base Driver.base_strings_of_surname
            (Driver.spi_find (Driver.persons_of_surname base))
            Driver.get_surname variant
        in
        List.iter
          (fun (str, _, iperl) ->
            if Name.lower str = Name.lower variant then (
              found_matches := (str, List.length iperl) :: !found_matches;
              List.iter
                (fun ip -> exact_iperl := IperSet.add ip !exact_iperl)
                iperl))
          list
      with _ -> ())
    variants;
  if !found_matches <> [] then
    Logs.debug (fun k ->
        k "        matches: %s"
          (String.concat ", "
             (List.map
                (fun (str, count) -> Printf.sprintf "%s(%d)" str count)
                !found_matches)));
  IperSet.elements !exact_iperl

and search_phonetic conf base query =
  Logs.debug (fun k -> k "      search_phonetic: %s" query);
  try
    let list, _name_inj =
      Some.persons_of_fsname conf base Driver.base_strings_of_surname
        (Driver.spi_find (Driver.persons_of_surname base))
        Driver.get_surname query
    in
    let ddr_iperl = ref IperSet.empty in
    List.iter
      (fun (_, _, iperl) ->
        List.iter (fun ip -> ddr_iperl := IperSet.add ip !ddr_iperl) iperl)
      list;
    let results = IperSet.elements !ddr_iperl in
    Logs.debug (fun k -> k "        phonetic: %d results" (List.length results));
    results
  with _ ->
    Logs.debug (fun k -> k "        phonetic: failed");
    []

(* Recherche directe de prénom via l'index *)
let search_firstname_direct conf base query =
  let list, _name_inj =
    if query = "" then ([], fun x -> x)
    else
      Some.persons_of_fsname conf base Driver.base_strings_of_first_name
        (Driver.spi_find (Driver.persons_of_first_name base))
        Driver.get_first_name query
  in
  let result = ref [] in
  let seen = Hashtbl.create 1000 in
  List.iter
    (fun (_, _, iperl) ->
      List.iter
        (fun ip ->
          if not (Hashtbl.mem seen ip) then (
            Hashtbl.add seen ip ();
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if
              fn <> ""
              && (not (Driver.Istr.is_empty (Driver.get_first_name p)))
              && (not (Driver.Istr.is_empty (Driver.get_surname p)))
              && not
                   (Util.is_hide_names conf p
                   && not (Util.authorized_age conf base p))
            then result := ip :: !result))
        iperl)
    list;
  List.rev !result

let search_firstname_aliases conf base query =
  let query_lower = Name.lower query in
  let all_misc_matches = Gutil.person_not_a_key_find_all base query in
  List.fold_left
    (fun acc ip ->
      let p = Driver.poi base ip in
      if
        empty_sn_or_fn base p
        || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))
      then acc
      else
        let aliases = Driver.get_first_names_aliases p in
        match
          List.find_opt
            (fun alias_istr ->
              let alias_str = Driver.sou base alias_istr in
              Name.lower alias_str = query_lower)
            aliases
        with
        | Some alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            (ip, alias_str) :: acc
        | None -> acc)
    [] all_misc_matches

let search_firstname_with_aliases conf base query =
  let direct_results = search_firstname_direct conf base query in
  let alias_results = search_firstname_aliases conf base query in
  List.iter
    (fun (iper, alias) -> Some.AliasCache.add_alias iper alias)
    alias_results;
  List.iter (fun iper -> Some.AliasCache.add_direct iper) direct_results;
  let direct_with_info =
    List.map (fun iper -> { iper; match_source = DirectMatch }) direct_results
  in
  let alias_with_info =
    List.map
      (fun (iper, alias) -> { iper; match_source = FirstNameAlias alias })
      alias_results
  in
  direct_with_info @ alias_with_info

(* Recherche de prénom optimisée avec cache et accès direct à l'index
   Parameters:
   - query: firstname(s) to search (e.g. "Marie Anne")
   - opts.all: if true, search exact phrase; if false, search each word
   - opts.order: if true with all=true, also search all permutations
   - opts.exact1: if true, exact match; if false, fuzzy match (not implemented)

   Returns: (exact_ipers, partial_ipers, firstname_variants)
   - exact_ipers: persons with matching firstname (direct or via alias)
   - partial_ipers: persons with partial match (currently unused)
   - firstname_variants: set of actual firstname strings found

   Uses index for fast lookup:
   - Driver.persons_of_first_name: direct firstname index
   - Gutil.person_not_a_key_find_all: firstname aliases in names.inx

   Examples:
   - "Marie Anne" [all=true, order=false] → searches "Marie Anne" exactly
   - "Marie Anne" [all=true, order=true] → searches "Marie Anne" + "Anne Marie"
   - "Marie Anne" [all=false] → searches "Marie" OR "Anne" (union)
*)
let search_firstname_with_cache conf base query opts =
  let query_words = cut_words query in
  let all_results =
    if opts.all && List.length query_words > 1 then (
      (* Si all=true avec plusieurs mots *)
      let search_queries =
        if opts.order then
          (* Si order=on, générer toutes les permutations *)
          let rec permutations = function
            | [] -> [ [] ]
            | x :: xs ->
                let perms = permutations xs in
                List.flatten
                  (List.map
                     (fun p ->
                       let rec insert_everywhere e = function
                         | [] -> [ [ e ] ]
                         | h :: t ->
                             (e :: h :: t)
                             :: List.map
                                  (fun l -> h :: l)
                                  (insert_everywhere e t)
                       in
                       insert_everywhere x p)
                     perms)
          in
          let perms = permutations query_words in
          List.map (fun words -> String.concat " " words) perms
        else [ query ]
      in
      let all_found = ref [] in
      let seen = Hashtbl.create 1000 in
      List.iter
        (fun search_query ->
          let res = search_firstname_with_aliases conf base search_query in
          let total = List.length res in
          let direct =
            List.filter
              (fun r -> match r.match_source with DirectMatch -> true | _ -> false)
              res
            |> List.length
          in
          let alias = total - direct in
          Logs.debug (fun k ->
              k "    '%s': %d (%d direct, %d alias)" search_query total direct
                alias);
          List.iter
            (fun r ->
              if not (Hashtbl.mem seen r.iper) then (
                Hashtbl.add seen r.iper ();
                all_found := r :: !all_found))
            res)
        search_queries;
      List.rev !all_found)
    else
      (* all=false: chercher chaque mot séparément *)
      let results_per_word =
        List.map
          (fun word ->
            let res = search_firstname_with_aliases conf base word in
            let total = List.length res in
            let direct =
              List.filter
                (fun r ->
                  match r.match_source with DirectMatch -> true | _ -> false)
                res
              |> List.length
            in
            let alias = total - direct in
            Logs.debug (fun k ->
                k "    '%s': %d (%d direct, %d alias)" word total direct alias);
            res)
          query_words
      in
      let all_ips = List.flatten results_per_word in
      let seen = Hashtbl.create 1000 in
      List.filter
        (fun r ->
          if Hashtbl.mem seen r.iper then false
          else (
            Hashtbl.add seen r.iper ();
            true))
        all_ips
  in
  let firstname_variants = ref Mutil.StrSet.empty in
  if not opts.all then (
    List.iter
      (fun result ->
        match result.match_source with
        | DirectMatch ->
            let p = Driver.poi base result.iper in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then
              firstname_variants := Mutil.StrSet.add fn !firstname_variants
        | FirstNameAlias _ -> ())
      all_results;
    Logs.debug (fun k -> k "  → %d results (union)" (List.length all_results));
    (List.map (fun r -> r.iper) all_results, [], !firstname_variants))
  else
    let exact = ref [] in
    let direct_match_count = ref 0 in
    let alias_match_count = ref 0 in
    List.iter
      (fun result ->
        let ip = result.iper in
        exact := ip :: !exact;
        match result.match_source with
        | DirectMatch ->
            incr direct_match_count;
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then
              firstname_variants := Mutil.StrSet.add fn !firstname_variants
        | FirstNameAlias _ -> incr alias_match_count)
      all_results;
    Logs.debug (fun k ->
        k "  → %d results (%d direct, %d alias)" (List.length !exact)
          !direct_match_count !alias_match_count);
    (List.rev !exact, [], !firstname_variants)

let group_by_surname base ipers =
  let groups = Hashtbl.create 10 in
  List.iter
    (fun ip ->
      let p = Driver.poi base ip in
      let sn = Driver.sou base (Driver.get_surname p) in
      let current = try Hashtbl.find groups sn with Not_found -> [] in
      Hashtbl.replace groups sn (p :: current))
    ipers;
  Hashtbl.fold (fun sn persons acc -> (sn, List.rev persons) :: acc) groups []

let search_fullname conf base fn sn =
  let conf_sn =
    {
      conf with
      env =
        ("surname", Adef.encoded sn)
        :: ("exact_surname", Adef.encoded "on")
        :: conf.env;
    }
  in
  let persons, _ = AdvSearchOk.advanced_search conf_sn base max_int in
  Logs.debug (fun k ->
      k "      search_fullname: %d results" (List.length persons));
  match persons with
  | [] -> { exact = []; partial = []; spouse = [] }
  | [ p ] -> { exact = [ Driver.get_iper p ]; partial = []; spouse = [] }
  | pl ->
      let opts =
        {
          order = false;
          all = true;
          case = false;
          exact1 = true;
          all_in = true;
        }
      in
      let exact = search_for_multiple_fn conf base fn pl opts 1000 in
      let opts_partial = { opts with exact1 = false; all_in = false } in
      let partial = search_for_multiple_fn conf base fn pl opts_partial 1000 in
      Logs.debug (fun k -> k "        spouses:");
      let spouse =
        if List.assoc_opt "public_name_as_fn" conf.base_env <> Some "no" then
          let sn_bearers_exact, sn_bearers_phon = search_surname conf base sn in
          let spouses =
            List.fold_left
              (fun acc ip ->
                let p = Driver.poi base ip in
                Array.fold_left
                  (fun acc ifam ->
                    let f = Driver.foi base ifam in
                    let spouse_ip =
                      if ip = Driver.get_father f then Driver.get_mother f
                      else Driver.get_father f
                    in
                    Driver.poi base spouse_ip :: acc)
                  acc (Driver.get_family p))
              []
              (sn_bearers_exact @ sn_bearers_phon)
          in
          search_for_multiple_fn conf base fn spouses opts_partial 1000
        else []
      in
      {
        exact = persons_to_ipers exact;
        partial = persons_to_ipers partial;
        spouse = persons_to_ipers spouse;
      }

(* Recherche par clé partielle *)
let search_partial_key conf base query =
  let pl = search_by_name conf base query in
  Logs.debug (fun k ->
      k "      search_partial_key: %d results" (List.length pl));
  match pl with
  | [] ->
      let n1 = Name.abbrev (Name.lower query) in
      let fn, sn =
        match String.index_opt n1 ' ' with
        | Some i ->
            (String.sub n1 0 i, String.sub n1 (i + 1) (String.length n1 - i - 1))
        | _ -> ("", n1)
      in
      let conf = { conf with env = ("surname", Adef.encoded sn) :: conf.env } in
      let persons, _ = AdvSearchOk.advanced_search conf base max_int in
      if persons = [] then { exact = []; partial = []; spouse = [] }
      else
        let opts =
          {
            order = false;
            all = true;
            case = false;
            exact1 = false;
            all_in = true;
          }
        in
        let opts_exact = { opts with exact1 = true; all_in = true } in
        let opts_partial = { opts with all_in = false } in
        let exact =
          search_for_multiple_fn conf base fn persons opts_exact 1000
        in
        let partial =
          search_for_multiple_fn conf base fn persons opts_partial 1000
        in
        {
          exact = persons_to_ipers exact;
          partial = persons_to_ipers partial;
          spouse = [];
        }
  | [ p ] -> { exact = [ Driver.get_iper p ]; partial = []; spouse = [] }
  | pl -> { exact = persons_to_ipers pl; partial = []; spouse = [] }

module ApostropheCache = struct
  let cache = Hashtbl.create 100

  let get_variants s =
    try Hashtbl.find cache s
    with Not_found ->
      let variants = generate_apostrophe_variants s in
      Hashtbl.add cache s variants;
      variants
end

(* Refined types for better clarity *)
type search_case =
  | NoInput (* 0 - No parameters provided *)
  | PersonName of string (* 1, 3, 5, 7 - Various person name formats *)
  | SurnameOnly of string (* 2 - Surname only *)
  | FirstNameOnly of string (* 4 - First name only *)
  | FirstNameSurname of string * string (* 6 - Both first and surname *)
  | ParsedName of {
      (* 11-16 - Structured name parsing *)
      first_name : string option;
      surname : string option;
      oc : string option;
      original : string;
      format :
        [ `Space | `Slash | `Dot | `SlashSurname | `SlashFirstName | `DotOc ];
    }
  | InvalidFormat of string (* 17 - Unparseable format *)

type name_components = {
  first_name : string option;
  surname : string option;
  oc : string option;
  person_name : string option;
  case : search_case;
}

(* Extract name components from environment with cleaner logic *)
let rec extract_name_components conf =
  let get_param key =
    match p_getenv conf.env key with Some "" | None -> None | Some s -> Some s
  in
  let fn = get_param "p" in
  let sn = get_param "n" in
  let pn = get_param "pn" in
  match (fn, sn, pn) with
  | None, None, None ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = None;
        case = NoInput;
      }
  | None, None, Some pn -> parse_person_name pn
  | None, Some sn, None ->
      {
        first_name = None;
        surname = Some sn;
        oc = None;
        person_name = None;
        case = SurnameOnly sn;
      }
  | None, Some _, Some pn ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = Some pn;
        case = PersonName pn;
      }
  | Some fn, None, None ->
      {
        first_name = Some fn;
        surname = None;
        oc = None;
        person_name = None;
        case = FirstNameOnly fn;
      }
  | Some _, None, Some pn ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = Some pn;
        case = PersonName pn;
      }
  | Some fn, Some sn, None ->
      {
        first_name = Some fn;
        surname = Some sn;
        oc = None;
        person_name = None;
        case = FirstNameSurname (fn, sn);
      }
  | Some _, Some _, Some pn ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = Some pn;
        case = PersonName pn;
      }

(* Parse person name string into structured components *)
and parse_person_name pn =
  let find_char c = try Some (String.index pn c) with Not_found -> None in
  let find_last_char c =
    try Some (String.rindex pn c) with Not_found -> None
  in
  let slash_pos = find_char '/' in
  let dot_pos = find_char '.' in
  let space_pos = find_last_char ' ' in
  match (slash_pos, dot_pos, space_pos) with
  | None, None, None ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = Some pn;
        case = PersonName pn;
      }
  | None, None, Some k ->
      let fn = String.sub pn 0 k in
      let sn =
        String.sub pn (k + 1) (String.length pn - k - 1) |> String.trim
      in
      {
        first_name = Some fn;
        surname = Some sn;
        oc = None;
        person_name = None;
        case =
          ParsedName
            {
              first_name = Some fn;
              surname = Some sn;
              oc = None;
              original = pn;
              format = `Space;
            };
      }
  | Some i, None, _ -> parse_slash_separated pn i
  | None, Some j, _ -> parse_dot_separated pn j
  | _ ->
      {
        first_name = None;
        surname = None;
        oc = None;
        person_name = Some pn;
        case = InvalidFormat pn;
      }

(* Handle slash-separated names (fn/sn or /sn or fn/) *)
and parse_slash_separated pn slash_pos =
  let fn_part = String.sub pn 0 slash_pos in
  let sn_part =
    String.sub pn (slash_pos + 1) (String.length pn - slash_pos - 1)
    |> String.trim
  in
  match (fn_part, sn_part) with
  | "", sn ->
      {
        first_name = None;
        surname = Some sn;
        oc = None;
        person_name = None;
        case =
          ParsedName
            {
              first_name = None;
              surname = Some sn;
              oc = None;
              original = pn;
              format = `SlashSurname;
            };
      }
  | fn, "" ->
      {
        first_name = Some fn;
        surname = None;
        oc = None;
        person_name = None;
        case =
          ParsedName
            {
              first_name = Some fn;
              surname = None;
              oc = None;
              original = pn;
              format = `SlashFirstName;
            };
      }
  | fn, sn ->
      {
        first_name = Some fn;
        surname = Some sn;
        oc = None;
        person_name = None;
        case =
          ParsedName
            {
              first_name = Some fn;
              surname = Some sn;
              oc = None;
              original = pn;
              format = `Slash;
            };
      }

(* Handle dot-separated names with optional oc *)
and parse_dot_separated pn dot_pos =
  let fn_part = String.sub pn 0 dot_pos in
  let rest = String.sub pn (dot_pos + 1) (String.length pn - dot_pos - 1) in
  match String.index_opt rest ' ' with
  | Some space_pos ->
      let oc = String.sub rest 0 space_pos in
      let sn_part =
        String.sub rest (space_pos + 1) (String.length rest - space_pos - 1)
        |> String.trim
      in
      {
        first_name = Some fn_part;
        surname = Some sn_part;
        oc = Some oc;
        person_name = None;
        case =
          ParsedName
            {
              first_name = Some fn_part;
              surname = Some sn_part;
              oc = Some oc;
              original = pn;
              format = `DotOc;
            };
      }
  | None ->
      {
        first_name = Some fn_part;
        surname = Some rest;
        oc = None;
        person_name = None;
        case =
          ParsedName
            {
              first_name = Some fn_part;
              surname = Some rest;
              oc = None;
              original = pn;
              format = `Dot;
            };
      }

(* remove from partial list persons present in exact list *)
(* Using Set for all three lists (most efficient for large datasets) *)
let remove_duplicates results =
  let exact_set =
    List.fold_left
      (fun acc iper -> IperSet.add iper acc)
      IperSet.empty results.exact
  in
  let partial_filtered =
    List.filter (fun iper -> not (IperSet.mem iper exact_set)) results.partial
  in
  let partial_set =
    List.fold_left
      (fun acc iper -> IperSet.add iper acc)
      IperSet.empty partial_filtered
  in
  let spouse_filtered =
    List.filter
      (fun iper ->
        (not (IperSet.mem iper exact_set)) && not (IperSet.mem iper partial_set))
      results.spouse
  in
  {
    exact = results.exact;
    partial = partial_filtered;
    spouse = spouse_filtered;
  }

(* Simplified search method dispatcher *)
type search_method =
  | Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey

let execute_search_method conf base query method_ fn_options =
  match method_ with
  | Sosa ->
      let results = search_sosa_opt conf base query in
      Logs.debug (fun k ->
          k "    Method Sosa: %d results" (List.length results));
      { exact = results; partial = []; spouse = [] }
  | Key ->
      let results = search_key_opt conf base query in
      Logs.debug (fun k -> k "    Method Key: %d results" (List.length results));
      { exact = results; partial = []; spouse = [] }
  | Surname ->
      let exact, partial = search_surname conf base query in
      Logs.debug (fun k ->
          k "    Method Surname: %d + %d exact/partial" (List.length exact)
            (List.length partial));
      { exact; partial; spouse = [] }
  | FirstName ->
      let exact, partial, _variants =
        search_firstname_with_cache conf base query fn_options
      in
      { exact; partial; spouse = [] }
  | FullName ->
      let components = extract_name_components conf in
      let fn = Option.value components.first_name ~default:"" in
      let sn = Option.value components.surname ~default:query in
      let oc = Option.value components.oc ~default:"" in
      let results =
        search_fullname conf base (if oc <> "" then fn ^ "." ^ oc else fn) sn
      in
      Logs.debug (fun k ->
          k "    Method FullName: %d+%d+%d results"
            (List.length results.exact)
            (List.length results.partial)
            (List.length results.spouse));
      results
  | ApproxKey ->
      let persons = search_approx_key conf base query in
      let exact_matches, partial_matches =
        List.partition
          (fun p ->
            let iper = Driver.get_iper p in
            match Some.AliasCache.get_alias iper with
            | Some _ -> true
            | None -> false)
          persons
      in
      let exact_ipers = List.map Driver.get_iper exact_matches in
      let partial_ipers = List.map Driver.get_iper partial_matches in
      Logs.debug (fun k ->
          k "    Method ApproxKey: %d exact, %d partial"
            (List.length exact_ipers)
            (List.length partial_ipers));
      { exact = exact_ipers; partial = partial_ipers; spouse = [] }
  | PartialKey ->
      let results = search_partial_key conf base query in
      Logs.debug (fun k ->
          k "    Method PartialKey: %d+%d+%d results"
            (List.length results.exact)
            (List.length results.partial)
            (List.length results.spouse));
      results

(* Main search dispatcher with better separation of concerns *)
let dispatch_search_methods conf base query search_order fn_options =
  StringCache.maintenance ();
  let all_results = { exact = []; partial = []; spouse = [] } in
  let firstname_variants = ref Mutil.StrSet.empty in
  let combined_results =
    List.fold_left
      (fun acc method_ ->
        let results =
          execute_search_method conf base query method_ fn_options
        in
        {
          exact = acc.exact @ results.exact;
          partial = acc.partial @ results.partial;
          spouse = acc.spouse @ results.spouse;
        })
      all_results search_order
  in
  let results =
    {
      exact = List.sort_uniq compare combined_results.exact;
      partial = List.sort_uniq compare combined_results.partial;
      spouse = List.sort_uniq compare combined_results.spouse;
    }
  in
  let deduplicated = remove_duplicates results in
  (deduplicated, !firstname_variants)

let rec handle_search_results conf base query fn_options components specify
    results =
  let { exact; partial; spouse } = results in
  match exact with
  | [ single_exact ] ->
      record_visited conf single_exact;
      Perso.print conf base (Driver.poi base single_exact)
  | _ -> (
      let all_persons = exact @ partial @ spouse in
      match all_persons with
      | [] -> SrcfileDisplay.print_welcome conf base
      | [ single_person ] ->
          record_visited conf single_person;
          Perso.print conf base (Driver.poi base single_person)
      | _multiple_persons -> (
          let exact_persons = List.map (Driver.poi base) exact in
          let partial_persons = List.map (Driver.poi base) partial in
          let spouse_persons = List.map (Driver.poi base) spouse in
          match components.case with
          | FirstNameOnly fn ->
              display_firstname_results conf base query fn exact_persons
                partial_persons spouse_persons
          | SurnameOnly sn ->
              display_surname_results conf base query sn all_persons
          | FirstNameSurname (_fn, _sn) ->
              specify conf base query exact_persons partial_persons
                spouse_persons
          | ParsedName { format = `Space; _ }
            when fn_options.exact1 && fn_options.all
                 && List.length exact_persons = 1 ->
              let person = List.hd exact_persons in
              record_visited conf (Driver.get_iper person);
              Perso.print conf base person
          | _ ->
              specify conf base query exact_persons partial_persons
                spouse_persons))

(* Helper functions for displaying results *)
and display_firstname_results conf base query _firstname exact partial spouse =
  let firstname_set =
    List.fold_left
      (fun acc person ->
        let fn = Driver.sou base (Driver.get_first_name person) in
        Mutil.StrSet.add fn acc)
      Mutil.StrSet.empty exact
  in
  let sections =
    [
      ("", exact);
      ( (if partial <> [] then
           transl conf "other possibilities" |> Utf8.capitalize_fst
         else ""),
        partial );
      ( (if spouse <> [] then
           transl conf "with spouse name" |> Utf8.capitalize_fst
         else ""),
        spouse );
    ]
  in
  Some.first_name_print_list conf base query firstname_set sections

and display_surname_results conf base _query surname all_persons =
  let surname_groups = group_by_surname base all_persons in
  match surname_groups with
  | [ (single_surname, _) ] ->
      Some.search_surname_print conf base (fun _conf _x -> ()) single_surname
  | multiple_surnames -> (
      match p_getenv conf.env "m" with
      | Some "SN" ->
          Some.print_surname_details conf base surname multiple_surnames
      | _ ->
          Some.print_several_possible_surnames surname conf base
            ([], multiple_surnames))

(* Main search entry point *)
let search conf base query search_order fn_options specify =
  Some.AliasCache.clear ();
  (* Handle apostrophe variants if needed *)
  let variants =
    if List.mem FirstName search_order && has_apostrophe query then
      ApostropheCache.get_variants query
    else [ query ]
  in
  if List.length variants > 1 then
    Logs.debug (fun k ->
        k "  %d apostrophe variants: %s" (List.length variants)
          (String.concat ", " variants));
  (* Process all variants and combine results *)
  let final_results =
    List.fold_left
      (fun acc variant ->
        let results, _fn_variants =
          dispatch_search_methods conf base variant search_order fn_options
        in
        {
          exact = acc.exact @ results.exact;
          partial = acc.partial @ results.partial;
          spouse = acc.spouse @ results.spouse;
        })
      { exact = []; partial = []; spouse = [] }
      variants
  in
  (* Final deduplication *)
  let deduplicated = remove_duplicates final_results in
  let components = extract_name_components conf in
  handle_search_results conf base query fn_options components specify
    deduplicated

(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)

let format_str format =
  match format with
  | `Dot -> "Dot"
  | `DotOc -> "DotOc"
  | `Space -> "Space"
  | `Slash -> "Slash"
  | `SlashSurname -> "SlashSurname"
  | `SlashFirstName -> "SlashFirstName"

let case_str case =
  match case with
  | FirstNameSurname _ -> "FirstNameSurname"
  | PersonName _ -> "PersonName"
  | FirstNameOnly _ -> "FirstNameOnly"
  | SurnameOnly _ -> "SurnameOnly"
  | ParsedName _ -> "ParsedName"
  | NoInput -> "NoInput"
  | InvalidFormat _ -> "InvalidFormat"

(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
    trouver la meilleure combinaison de résultat pour afficher la réponse la
    plus probable. [Args] :
    - conf : configuration de la base
    - base : base [Retour] : Néant [Rem] : Exporté en clair hors de ce module.
*)
let print conf base specify =
  let components = extract_name_components conf in
  let fn_options =
    {
      order = p_getenv conf.env "p_order" = Some "on";
      all = p_getenv conf.env "p_all" <> Some "off";
      case = false;
      exact1 = p_getenv conf.env "p_exact" <> Some "off";
      all_in = false;
    }
  in
  let case = components.case in
  match case with
  | FirstNameSurname (fn, sn) ->
      Logs.debug (fun k -> k "Print case FirstNameSurname (%s, %s)" fn sn);
      let order = [ Key; FullName; ApproxKey; PartialKey; Surname ] in
      search conf base (fn ^ " " ^ sn) order fn_options specify
  | PersonName pn ->
      Logs.debug (fun k -> k "Print case PersonName (%s)" pn);
      let order = [ Key; FullName; ApproxKey; PartialKey; Surname ] in
      search conf base pn order fn_options specify
  | FirstNameOnly fn ->
      Logs.debug (fun k ->
          k "Search p=FirstNameOnly '%s' [all=%b, order=%b, exact=%b]" fn
            fn_options.all fn_options.order fn_options.exact1);
      let order = [ FirstName ] in
      search conf base fn order fn_options specify
  | SurnameOnly sn ->
      Logs.debug (fun k -> k "Print case SurnameOnly (%s)" sn);
      let order = [ Surname ] in
      search conf base sn order fn_options specify
  | ParsedName { first_name = fn; surname = sn; oc; format; _ } -> (
      match (fn, sn) with
      | Some fn, None when fn <> "" ->
          Logs.debug (fun k -> k "Print case ParsedName (fn = %s)" fn);
          let order = [ FirstName ] in
          search conf base fn order fn_options specify
      | None, Some sn when sn <> "" ->
          Logs.debug (fun k -> k "Print case ParsedName (sn = %s)" sn);
          let order = [ Surname; ApproxKey ] in
          search conf base sn order fn_options specify
      | _ -> (
          let order = [ Sosa; Key; FullName; ApproxKey; PartialKey; Surname ] in
          let fn = Option.value fn ~default:"" in
          let sn = Option.value sn ~default:"" in
          let oc = Option.value oc ~default:"" in
          match format with
          | `Dot ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              search conf base
                (Printf.sprintf "%s %s" fn sn)
                order fn_options specify
          | `DotOc ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              search conf base
                (Printf.sprintf "%s.%s %s" fn oc sn)
                order fn_options specify
          | `Space ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              search conf base
                (Printf.sprintf "%s %s" fn sn)
                order fn_options specify
          | `Slash ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              search conf base
                (Printf.sprintf "%s %s" fn sn)
                order fn_options specify
          | `SlashSurname ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              let order = [ Surname; ApproxKey ] in
              search conf base sn order fn_options specify
          | `SlashFirstName ->
              Logs.debug (fun k -> k "Print format %s" (format_str format));
              let order = [ FirstName ] in
              search conf base fn order fn_options specify))
  | _ ->
      Logs.debug (fun k -> k "Print case default (%s)" (case_str case));
      SrcfileDisplay.print_welcome conf base
