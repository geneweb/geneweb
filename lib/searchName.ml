(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Logs = Geneweb_logs.Logs
module Collection = Geneweb_db.Collection

(* ========================================================================= *)
(* Section 1: Types and Data Structures                                     *)
(* ========================================================================= *)

module IperSet = Set.Make (struct
  type t = Driver.iper

  let compare = compare
end)

type opts = { order : bool; exact1 : bool; incl_aliases : bool }

type search_results = {
  exact : Driver.Iper.t list;
  partial : Driver.Iper.t list;
  spouse : Driver.Iper.t list;
}

type search_case =
  | NoInput
  | PersonName of string
  | SurnameOnly of string
  | FirstNameOnly of string
  | FirstNameSurname of string * string
  | ParsedName of {
      first_name : string option;
      surname : string option;
      oc : string option;
      original : string;
      format :
        [ `Space | `Slash | `Dot | `SlashSurname | `SlashFirstName | `DotOc ];
    }
  | InvalidFormat of string

type name_components = {
  first_name : string option;
  surname : string option;
  oc : string option;
  person_name : string option;
  case : search_case;
}

type search_method =
  | Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey

type firstname_section = {
  persons : Driver.Iper.t list;
  variants : Mutil.StrSet.t;
}

type firstname_results = {
  direct : firstname_section;
  aliases : firstname_section;
  included : firstname_section;
  phonetic : firstname_section;
  permuted : firstname_section;
}

(* ========================================================================= *)
(* Section 2: Low-level Utilities                                           *)
(* ========================================================================= *)

let generate_apostrophe_variants s =
  let apostrophes = [ "'"; "\xE2\x80\x99"; "\xCA\xBC"; "\xCA\xBB" ] in
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

let split_normalize case s =
  let s = Name.abbrev s in
  let s = if case then s else Name.lower s in
  cut_words s

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let persons_to_ipers pl = List.map (fun p -> Driver.get_iper p) pl

let rec list_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: list_take (n - 1) xs

let rec list_drop n = function
  | xs when n <= 0 -> xs
  | [] -> []
  | _ :: xs -> list_drop (n - 1) xs

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

module ApostropheCache = struct
  let cache = Hashtbl.create 100

  let get_variants s =
    try Hashtbl.find cache s
    with Not_found ->
      let variants = generate_apostrophe_variants s in
      Hashtbl.add cache s variants;
      variants
end

(* ========================================================================= *)
(* Section 3: Core Search Functions                                         *)
(* ========================================================================= *)

let generate_permutations query =
  let normalized = Mutil.tr '-' ' ' query in
  let words =
    List.filter (fun w -> w <> "") (String.split_on_char ' ' normalized)
  in
  match List.length words with
  | 2 ->
      let w1 = List.nth words 0 in
      let w2 = List.nth words 1 in
      [ w2 ^ " " ^ w1 ]
  | 3 ->
      let w1 = List.nth words 0 in
      let w2 = List.nth words 1 in
      let w3 = List.nth words 2 in
      [
        w1 ^ " " ^ w3 ^ " " ^ w2;
        w2 ^ " " ^ w3 ^ " " ^ w1;
        w2 ^ " " ^ w1 ^ " " ^ w3;
        w3 ^ " " ^ w1 ^ " " ^ w2;
        w3 ^ " " ^ w2 ^ " " ^ w1;
      ]
  | 4 ->
      let w1 = List.nth words 0 in
      let w2 = List.nth words 1 in
      let w3 = List.nth words 2 in
      let w4 = List.nth words 3 in
      [
        w1 ^ " " ^ w2 ^ " " ^ w4 ^ " " ^ w3;
        w1 ^ " " ^ w3 ^ " " ^ w2 ^ " " ^ w4;
        w1 ^ " " ^ w3 ^ " " ^ w4 ^ " " ^ w2;
        w1 ^ " " ^ w4 ^ " " ^ w2 ^ " " ^ w3;
        w1 ^ " " ^ w4 ^ " " ^ w3 ^ " " ^ w2;
        w2 ^ " " ^ w1 ^ " " ^ w3 ^ " " ^ w4;
        w2 ^ " " ^ w1 ^ " " ^ w4 ^ " " ^ w3;
        w2 ^ " " ^ w3 ^ " " ^ w1 ^ " " ^ w4;
        w2 ^ " " ^ w3 ^ " " ^ w4 ^ " " ^ w1;
        w2 ^ " " ^ w4 ^ " " ^ w1 ^ " " ^ w3;
        w2 ^ " " ^ w4 ^ " " ^ w3 ^ " " ^ w1;
        w3 ^ " " ^ w1 ^ " " ^ w2 ^ " " ^ w4;
        w3 ^ " " ^ w1 ^ " " ^ w4 ^ " " ^ w2;
        w3 ^ " " ^ w2 ^ " " ^ w1 ^ " " ^ w4;
        w3 ^ " " ^ w2 ^ " " ^ w4 ^ " " ^ w1;
        w3 ^ " " ^ w4 ^ " " ^ w1 ^ " " ^ w2;
        w3 ^ " " ^ w4 ^ " " ^ w2 ^ " " ^ w1;
        w4 ^ " " ^ w1 ^ " " ^ w2 ^ " " ^ w3;
        w4 ^ " " ^ w1 ^ " " ^ w3 ^ " " ^ w2;
        w4 ^ " " ^ w2 ^ " " ^ w1 ^ " " ^ w3;
        w4 ^ " " ^ w2 ^ " " ^ w3 ^ " " ^ w1;
        w4 ^ " " ^ w3 ^ " " ^ w1 ^ " " ^ w2;
        w4 ^ " " ^ w3 ^ " " ^ w2 ^ " " ^ w1;
      ]
  | _ -> []

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

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index n1 ' ' with
  | exception Not_found -> []
  | i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let p_of_sn_l, _ =
        Some.persons_of_fsname conf base Driver.base_strings_of_surname
          (Driver.spi_find (Driver.persons_of_surname base))
          Driver.get_surname sn
      in
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

let search_sosa_opt conf base query =
  match search_by_sosa conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

let search_key_opt conf base query =
  match search_by_key conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

let match_fn_lists fn_l fn1_l opts =
  let normalize s = Name.lower s in
  let word_matches query_word person_word =
    let q = normalize query_word in
    let p = normalize person_word in
    if opts.exact1 then q = p else Mutil.contains p q
  in
  let passes_basic_test =
    List.for_all
      (fun query_term -> List.exists (word_matches query_term) fn1_l)
      fn_l
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
  passes_basic_test && passes_order_test

let search_for_multiple_fn conf base fn pl opts batch_size =
  Logs.debug (fun k -> k "        search_for_multiple_fn: %s" fn);
  let bool_to_string b = if b then "true" else "false" in
  Logs.debug (fun k ->
      k "          order: %s, exact: %s"
        (bool_to_string opts.order)
        (bool_to_string opts.exact1));
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
                  let fn1_l = split_normalize false fn1 in
                  let fn2_istr = Driver.get_public_name p in
                  let fn2 = StringCache.get_cached base fn2_istr in
                  let fn2_l = split_normalize false fn2 in
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
  let has_apostrophe = has_apostrophe x in
  match has_apostrophe with
  | false ->
      let exact_results = search_exact conf base [ x ] in
      if exact_results <> [] then (
        Logs.debug (fun k ->
            k "  → %d results (%d exact, 0 phonetic)"
              (List.length exact_results)
              (List.length exact_results));
        (exact_results, []))
      else
        let phonetic_results = search_phonetic conf base x in
        Logs.debug (fun k ->
            k "  → %d results (0 exact, %d phonetic)"
              (List.length phonetic_results)
              (List.length phonetic_results));
        ([], phonetic_results)
  | true ->
      let variants = generate_apostrophe_variants x in
      let exact_results = search_exact conf base variants in
      if exact_results <> [] then (
        Logs.debug (fun k ->
            k "  → %d results (%d exact, 0 phonetic)"
              (List.length exact_results)
              (List.length exact_results));
        (exact_results, []))
      else
        let fallback_query = List.hd variants in
        let phonetic_results = search_phonetic conf base fallback_query in
        Logs.debug (fun k ->
            k "  → %d results (0 exact, %d phonetic)"
              (List.length phonetic_results)
              (List.length phonetic_results));
        ([], phonetic_results)

and search_exact conf base variants =
  let exact_iperl = ref IperSet.empty in
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
            if Name.lower str = Name.lower variant then
              List.iter
                (fun ip -> exact_iperl := IperSet.add ip !exact_iperl)
                iperl)
          list
      with _ -> ())
    variants;
  IperSet.elements !exact_iperl

and search_phonetic_generic conf base query base_strings spi_find get_name =
  try
    let list, _name_inj =
      Some.persons_of_fsname conf base base_strings spi_find get_name query
    in
    let ddr_iperl = ref IperSet.empty in
    List.iter
      (fun (_, _, iperl) ->
        List.iter
          (fun ip ->
            if not (IperSet.mem ip !ddr_iperl) then
              let p = Driver.poi base ip in
              if
                (not (Driver.Istr.is_empty (Driver.get_first_name p)))
                && (not (Driver.Istr.is_empty (Driver.get_surname p)))
                && not
                     (Util.is_hide_names conf p
                     && not (Util.authorized_age conf base p))
              then ddr_iperl := IperSet.add ip !ddr_iperl)
          iperl)
      list;
    IperSet.elements !ddr_iperl
  with _ -> []

and search_phonetic conf base query =
  search_phonetic_generic conf base query Driver.base_strings_of_surname
    (Driver.spi_find (Driver.persons_of_surname base))
    Driver.get_surname

and search_firstname_phonetic conf base query =
  search_phonetic_generic conf base query Driver.base_strings_of_first_name
    (Driver.spi_find (Driver.persons_of_first_name base))
    Driver.get_first_name

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

let normalize_for_phonetic s =
  let buf = Buffer.create (String.length s) in
  String.iter (function ' ' | '-' -> () | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let search_firstname_phonetic_split conf base query =
  let query_lower = Name.lower query in
  let query_norm = normalize_for_phonetic query_lower in
  let query_words = cut_words query_lower in
  let phonetic_iperl_words =
    List.flatten
      (List.map
         (fun word ->
           let crushed = Name.crush_lower word in
           search_firstname_phonetic conf base crushed)
         query_words)
  in
  (* Cette partie n'est plus nécessaire avec l'index corrigé,
     mais ne fait pas de mal pour compatibilité *)
  let phonetic_iperl_hyphen =
    if List.length query_words >= 2 then
      let first = List.hd query_words in
      let second = List.nth query_words 1 in
      let hyphenated = first ^ "-" ^ second in
      let crushed = Name.crush_lower hyphenated in
      search_firstname_phonetic conf base crushed
    else []
  in
  let all_iperl = phonetic_iperl_words @ phonetic_iperl_hyphen in
  let seen = Hashtbl.create 1000 in
  let exact_matches = ref [] in
  let variant_matches = ref [] in
  let firstname_variants = ref Mutil.StrSet.empty in
  List.iter
    (fun iper ->
      if not (Hashtbl.mem seen iper) then (
        Hashtbl.add seen iper ();
        Some.AliasCache.add_direct iper;
        let p = Driver.poi base iper in
        let fn = Driver.sou base (Driver.get_first_name p) in
        let fn_lower = Name.lower fn in
        let fn_norm = normalize_for_phonetic fn_lower in
        let is_direct_match = Mutil.contains fn_norm query_norm in
        if is_direct_match then (
          exact_matches := iper :: !exact_matches;
          if fn <> "" then
            firstname_variants := Mutil.StrSet.add fn !firstname_variants)
        else
          let fn_norm_crushed = Name.crush fn_norm in
          let query_norm_crushed = Name.crush query_norm in
          if Mutil.contains fn_norm_crushed query_norm_crushed then
            variant_matches := iper :: !variant_matches))
    all_iperl;
  let exact = List.rev !exact_matches in
  let partial = List.rev !variant_matches in
  (exact, partial, !firstname_variants)

let search_with_ngrams_complement conf base _query query_words =
  let nb_words = List.length query_words in
  match nb_words with
  | 0 | 1 ->
      List.flatten
        (List.map
           (fun w -> search_firstname_phonetic conf base (Name.crush_lower w))
           query_words)
  | 2 ->
      let w1 = List.nth query_words 0 in
      let w2 = List.nth query_words 1 in
      let bigram = w1 ^ w2 in
      let results_words =
        List.flatten
          (List.map
             (fun w -> search_firstname_phonetic conf base (Name.crush_lower w))
             query_words)
      in
      let results_bigram =
        try
          let crushed = Name.crush bigram in
          search_firstname_phonetic conf base crushed
        with _ -> []
      in
      let all = results_words @ results_bigram in
      let seen = Hashtbl.create 1000 in
      List.filter
        (fun ip ->
          if Hashtbl.mem seen ip then false
          else (
            Hashtbl.add seen ip ();
            true))
        all
  | 3 ->
      let w1 = List.nth query_words 0 in
      let w2 = List.nth query_words 1 in
      let w3 = List.nth query_words 2 in
      let trigram = w1 ^ w2 ^ w3 in
      let results_words =
        List.flatten
          (List.map
             (fun w -> search_firstname_phonetic conf base (Name.crush_lower w))
             query_words)
      in
      let results_trigram =
        try
          let crushed = Name.crush trigram in
          search_firstname_phonetic conf base crushed
        with _ -> []
      in
      let all = results_words @ results_trigram in
      let seen = Hashtbl.create 1000 in
      List.filter
        (fun ip ->
          if Hashtbl.mem seen ip then false
          else (
            Hashtbl.add seen ip ();
            true))
        all
  | 4 ->
      let w1 = List.nth query_words 0 in
      let w2 = List.nth query_words 1 in
      let w3 = List.nth query_words 2 in
      let w4 = List.nth query_words 3 in
      let quadrigram = w1 ^ w2 ^ w3 ^ w4 in
      let results_words =
        List.flatten
          (List.map
             (fun w -> search_firstname_phonetic conf base (Name.crush_lower w))
             query_words)
      in
      let results_quadrigram =
        try
          let crushed = Name.crush quadrigram in
          search_firstname_phonetic conf base crushed
        with _ -> []
      in
      let all = results_words @ results_quadrigram in
      let seen = Hashtbl.create 1000 in
      List.filter
        (fun ip ->
          if Hashtbl.mem seen ip then false
          else (
            Hashtbl.add seen ip ();
            true))
        all
  | _ ->
      List.flatten
        (List.map
           (fun w -> search_firstname_phonetic conf base (Name.crush_lower w))
           query_words)

let search_firstname_with_aliases_and_ngrams conf base query =
  let query_lower = Name.lower query in
  let query_norm = normalize_for_phonetic query_lower in
  let query_norm_crushed = Name.crush query_norm in
  let query_words = cut_words query_lower in
  let alias_results = search_firstname_aliases conf base query in
  List.iter
    (fun (ip, alias) -> Some.AliasCache.add_alias ip alias)
    alias_results;
  let phonetic_iper =
    search_with_ngrams_complement conf base query query_words
  in
  let phonetic_results = ref [] in
  let seen = Hashtbl.create 1000 in
  List.iter
    (fun ip ->
      if not (Hashtbl.mem seen ip) then (
        Hashtbl.add seen ip ();
        let p = Driver.poi base ip in
        let fn = Driver.sou base (Driver.get_first_name p) in
        let fn_lower = Name.lower fn in
        let fn_norm = normalize_for_phonetic fn_lower in
        let fn_norm_crushed = Name.crush fn_norm in
        if Mutil.contains fn_norm_crushed query_norm_crushed then (
          Some.AliasCache.add_direct ip;
          phonetic_results := ip :: !phonetic_results)))
    phonetic_iper;
  Logs.debug (fun k ->
      k
        "  Phonetic + ngrams: %d candidates → %d after dedup (will be split \
         into included/phonetic)"
        (List.length phonetic_iper)
        (List.length !phonetic_results));
  (alias_results, !phonetic_results)

let search_firstname_with_cache conf base query opts =
  let query_lower = Name.lower query in
  let query_norm = normalize_for_phonetic query_lower in
  let query_words = cut_words query in
  let nb_words = List.length query_words in
  Logs.debug (fun k ->
      k "Search p=%s FirstNameOnly [aliases=%b, order=%b, exact=%b]" query
        opts.incl_aliases opts.order opts.exact1);
  let direct_results = search_firstname_direct conf base query in
  List.iter (fun ip -> Some.AliasCache.add_direct ip) direct_results;
  let direct_exact = ref [] in
  let direct_included = ref [] in
  List.iter
    (fun ip ->
      let p = Driver.poi base ip in
      let fn = Driver.sou base (Driver.get_first_name p) in
      let fn_lower = Name.lower fn in
      let fn_norm = normalize_for_phonetic fn_lower in
      if fn_norm = query_norm then direct_exact := ip :: !direct_exact
      else direct_included := ip :: !direct_included)
    direct_results;
  let direct_exact_results = List.rev !direct_exact in
  let exact_variants =
    List.fold_left
      (fun acc ip ->
        let p = Driver.poi base ip in
        let fn = Driver.sou base (Driver.get_first_name p) in
        if fn <> "" then Mutil.StrSet.add fn acc else acc)
      Mutil.StrSet.empty direct_exact_results
  in
  let alias_results, alias_variants =
    if opts.incl_aliases then (
      let aliases = search_firstname_aliases conf base query in
      List.iter (fun (ip, alias) -> Some.AliasCache.add_alias ip alias) aliases;
      let variants =
        List.fold_left
          (fun acc (ip, _alias) ->
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then Mutil.StrSet.add fn acc else acc)
          Mutil.StrSet.empty aliases
      in
      (List.map fst aliases, variants))
    else ([], Mutil.StrSet.empty)
  in
  let permuted_persons, permuted_variants =
    if opts.order && nb_words > 1 && nb_words < 5 then
      let permutations = generate_permutations query in
      List.fold_left
        (fun (acc_persons, acc_vars) perm_query ->
          let perm_direct = search_firstname_direct conf base perm_query in
          List.iter (fun ip -> Some.AliasCache.add_direct ip) perm_direct;
          let perm_alias =
            if opts.incl_aliases then
              search_firstname_aliases conf base perm_query
            else []
          in
          List.iter
            (fun (ip, alias) -> Some.AliasCache.add_alias ip alias)
            perm_alias;
          let perm_all = perm_direct @ List.map fst perm_alias in
          let perm_vars =
            List.fold_left
              (fun acc ip ->
                let p = Driver.poi base ip in
                let fn = Driver.sou base (Driver.get_first_name p) in
                if fn <> "" then Mutil.StrSet.add fn acc else acc)
              acc_vars perm_all
          in
          (acc_persons @ perm_all, perm_vars))
        ([], Mutil.StrSet.empty) permutations
    else ([], Mutil.StrSet.empty)
  in
  let included_iper, included_variants, phonetic_iper, phonetic_variants =
    if not opts.exact1 then (
      let direct_included_results = List.rev !direct_included in
      let phonetic_ngrams =
        if opts.incl_aliases then
          snd (search_firstname_with_aliases_and_ngrams conf base query)
        else []
      in
      let exact_phonetic, partial_phonetic, _ =
        if not opts.incl_aliases then
          search_firstname_phonetic_split conf base query
        else ([], [], Mutil.StrSet.empty)
      in
      let all_phonetic = phonetic_ngrams @ exact_phonetic in
      let contains_query = ref [] in
      let other_phonetic = ref [] in
      let seen = Hashtbl.create 1000 in
      List.iter (fun ip -> Hashtbl.add seen ip ()) direct_exact_results;
      List.iter (fun ip -> Hashtbl.add seen ip ()) alias_results;
      List.iter
        (fun ip ->
          if not (Hashtbl.mem seen ip) then (
            Hashtbl.add seen ip ();
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            let fn_lower = Name.lower fn in
            let fn_norm = normalize_for_phonetic fn_lower in
            if Mutil.contains fn_norm query_norm then
              contains_query := ip :: !contains_query
            else other_phonetic := ip :: !other_phonetic))
        all_phonetic;
      List.iter
        (fun ip ->
          if not (Hashtbl.mem seen ip) then (
            Hashtbl.add seen ip ();
            other_phonetic := ip :: !other_phonetic))
        partial_phonetic;
      let included_iper = direct_included_results @ List.rev !contains_query in
      let included_variants =
        List.fold_left
          (fun acc ip ->
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then Mutil.StrSet.add fn acc else acc)
          Mutil.StrSet.empty included_iper
      in
      let phonetic_iper = List.rev !other_phonetic in
      let phonetic_variants =
        List.fold_left
          (fun acc ip ->
            let p = Driver.poi base ip in
            let fn = Driver.sou base (Driver.get_first_name p) in
            if fn <> "" then Mutil.StrSet.add fn acc else acc)
          Mutil.StrSet.empty phonetic_iper
      in
      (included_iper, included_variants, phonetic_iper, phonetic_variants))
    else ([], Mutil.StrSet.empty, [], Mutil.StrSet.empty)
  in
  Logs.debug (fun k ->
      k
        "  → %d results (%d exact, %d alias, %d permuted, %d included, %d \
         phonetic)"
        (List.length direct_exact_results
        + List.length alias_results
        + List.length permuted_persons
        + List.length included_iper + List.length phonetic_iper)
        (List.length direct_exact_results)
        (List.length alias_results)
        (List.length permuted_persons)
        (List.length included_iper)
        (List.length phonetic_iper));
  {
    direct = { persons = direct_exact_results; variants = exact_variants };
    aliases = { persons = alias_results; variants = alias_variants };
    included = { persons = included_iper; variants = included_variants };
    phonetic = { persons = phonetic_iper; variants = phonetic_variants };
    permuted = { persons = permuted_persons; variants = permuted_variants };
  }

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
  let fn = String.map (fun c -> if c = '-' then ' ' else c) fn in
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
      let opts = { order = false; exact1 = true; incl_aliases = false } in
      let exact = search_for_multiple_fn conf base fn pl opts 1000 in
      let opts_partial = { opts with exact1 = false } in
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
        let opts = { order = false; exact1 = false; incl_aliases = false } in
        let opts_exact = { opts with exact1 = true } in
        let opts_partial = { opts with exact1 = false } in
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

(* ========================================================================= *)
(* Section 4: Name Parsing and Component Extraction                         *)
(* ========================================================================= *)

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

and make_parsed_component ?first_name ?surname ?oc original format =
  let fn_opt = if first_name = Some "" then None else first_name in
  let sn_opt = if surname = Some "" then None else surname in
  {
    first_name = fn_opt;
    surname = sn_opt;
    oc;
    person_name = None;
    case =
      ParsedName { first_name = fn_opt; surname = sn_opt; oc; original; format };
  }

and parse_slash_separated pn slash_pos =
  let fn_part = String.sub pn 0 slash_pos in
  let sn_part =
    String.sub pn (slash_pos + 1) (String.length pn - slash_pos - 1)
    |> String.trim
  in
  match (fn_part, sn_part) with
  | "", sn -> make_parsed_component ~surname:sn pn `SlashSurname
  | fn, "" -> make_parsed_component ~first_name:fn pn `SlashFirstName
  | fn, sn -> make_parsed_component ~first_name:fn ~surname:sn pn `Slash

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
      make_parsed_component ~first_name:fn_part ~surname:sn_part ~oc pn `DotOc
  | None -> make_parsed_component ~first_name:fn_part ~surname:rest pn `Dot

(* ========================================================================= *)
(* Section 5: Search Orchestration                                          *)
(* ========================================================================= *)

let remove_duplicates (results : search_results) =
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
      { exact; partial; spouse = [] }
  | FirstName ->
      let fn_results = search_firstname_with_cache conf base query fn_options in
      {
        exact =
          fn_results.direct.persons @ fn_results.aliases.persons
          @ fn_results.included.persons;
        partial = fn_results.phonetic.persons @ fn_results.permuted.persons;
        spouse = [];
      }
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

let dispatch_search_methods conf base query search_order fn_options =
  StringCache.maintenance ();
  let all_results = { exact = []; partial = []; spouse = [] } in
  let firstname_variants = ref Mutil.StrSet.empty in
  let combined_results =
    List.fold_left
      (fun acc method_ ->
        let (results : search_results) =
          execute_search_method conf base query method_ fn_options
        in
        ({
           exact = acc.exact @ results.exact;
           partial = acc.partial @ results.partial;
           spouse = acc.spouse @ results.spouse;
         }
          : search_results))
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

(* ========================================================================= *)
(* Section 6: Result Handling and Display                                   *)
(* ========================================================================= *)

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
          | SurnameOnly sn ->
              display_surname_results conf base query sn all_persons
          | ParsedName { first_name = None; surname = Some sn; _ } ->
              display_surname_results conf base query sn all_persons
          | ParsedName { first_name = Some fn; surname = None; _ } ->
              display_firstname_results conf base fn fn_options
                (search_firstname_with_cache conf base fn fn_options)
          | FirstNameSurname (_fn, _sn) ->
              specify conf base query exact_persons partial_persons
                spouse_persons
          | ParsedName { format = `Space; _ }
            when fn_options.exact1 && List.length exact_persons = 1 ->
              let person = List.hd exact_persons in
              record_visited conf (Driver.get_iper person);
              Perso.print conf base person
          | _ ->
              specify conf base query exact_persons partial_persons
                spouse_persons))

and display_firstname_results conf base query fn_options results =
  let include_aliases = fn_options.incl_aliases in
  let is_partial = not fn_options.exact1 in
  let sections_exact =
    if results.direct.persons <> [] then
      [ ("", List.map (Driver.poi base) results.direct.persons) ]
    else []
  in
  let sections_aliases =
    if include_aliases then
      [ ("", List.map (Driver.poi base) results.aliases.persons) ]
    else []
  in
  let sections_included =
    if results.included.persons <> [] then
      [ ("", List.map (Driver.poi base) results.included.persons) ]
    else []
  in
  let sections_partial =
    if results.phonetic.persons <> [] then
      [ ("", List.map (Driver.poi base) results.phonetic.persons) ]
    else []
  in
  let sections_permuted =
    if results.permuted.persons <> [] then
      [ ("", List.map (Driver.poi base) results.permuted.persons) ]
    else []
  in
  let sections_groups =
    [
      (0, sections_exact, false, results.direct.variants);
      (1, sections_aliases, false, results.aliases.variants);
      (4, sections_permuted, false, results.permuted.variants);
      (2, sections_included, false, results.included.variants);
      (3, sections_partial, true, results.phonetic.variants);
    ]
    |> List.filter (fun (i, secs, _, _) ->
           match i with
           | 0 -> secs <> []
           | 1 -> include_aliases
           | 4 -> secs <> []
           | 2 | 3 -> is_partial && secs <> []
           | _ -> false)
  in
  Some.first_name_print_list_multi conf base query sections_groups

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

let search conf base query search_order fn_options specify =
  Some.AliasCache.clear ();
  let variants =
    if List.mem FirstName search_order && has_apostrophe query then
      ApostropheCache.get_variants query
    else [ query ]
  in
  if List.length variants > 1 then
    Logs.debug (fun k ->
        k "  %d apostrophe variants: %s" (List.length variants)
          (String.concat ", " variants));
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
  let deduplicated = remove_duplicates final_results in
  let components = extract_name_components conf in
  handle_search_results conf base query fn_options components specify
    deduplicated

(* ========================================================================= *)
(* Section 7: Main Entry Point                                              *)
(* ========================================================================= *)

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

let print conf base specify =
  let components = extract_name_components conf in
  let fn_options =
    {
      order = p_getenv conf.env "p_order" = Some "on";
      exact1 = p_getenv conf.env "p_exact" <> Some "off";
      incl_aliases = p_getenv conf.env "fna" <> None;
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
      let results = search_firstname_with_cache conf base fn fn_options in
      display_firstname_results conf base fn fn_options results
  | SurnameOnly sn ->
      Logs.debug (fun k -> k "Search n=SurnameOnly '%s'" sn);
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
          let order = [ Surname ] in
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
