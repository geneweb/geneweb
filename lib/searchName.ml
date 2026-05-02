(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Iper = Driver.Iper

let src = Logs.Src.create ~doc:"SearchName" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)

(* ========================================================================= *)
(* Section 1: Types and Data Structures                                     *)
(* ========================================================================= *)

type opts = {
  order : bool;
  exact1 : bool;
  incl_aliases : bool;
  absolute : bool;
}

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

type normalized_string = {
  original : string;
  lower : string;
  normalized : string;
  words : string list option;
}

(* ========================================================================= *)
(* Section 2: Low-level Utilities                                           *)
(* ========================================================================= *)

let normalize_for_phonetic s =
  let buf = Buffer.create (String.length s) in
  String.iter (function ' ' | '-' -> () | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let normalize_query q =
  let q_normalized = Mutil.tr '-' ' ' q in
  let lower = Name.lower q in
  let normalized = normalize_for_phonetic lower in
  { original = q; lower; normalized; words = Some (cut_words q_normalized) }

let normalize_name n =
  let lower = Name.lower n in
  let normalized = normalize_for_phonetic lower in
  { original = n; lower; normalized; words = None }

let find_apostrophe_opt s =
  let rec aux i =
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
      | _ -> aux (i + 1)
  in
  aux 0

let generate_apostrophe_variants s =
  let apostrophes = [ "'"; "\xE2\x80\x99"; "\xCA\xBC"; "\xCA\xBB" ] in
  match find_apostrophe_opt s with
  | None -> [ s ]
  | Some (pos, len) ->
      List.map
        (fun apo ->
          String.sub s 0 pos ^ apo
          ^ String.sub s (pos + len) (String.length s - pos - len))
        apostrophes
      |> List.sort_uniq String.compare

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

let persons_to_ipers = List.map Driver.get_iper

let list_take n l =
  let rec aux acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n l

let list_drop n l =
  let rec aux n = function
    | xs when n <= 0 -> xs
    | [] -> []
    | _ :: xs -> aux (n - 1) xs
  in
  aux n l

(* String cache scoped to a single search request to avoid cross-base
   pollution when gwd serves multiple bases.  The cache is created at the
   top of [search] and threaded explicitly through the call chain; there is
   no global mutable state. *)
module StringCache = struct
  let create () : (Driver.istr, string) Hashtbl.t = Hashtbl.create 2000

  let get_cached cache base istr =
    match Hashtbl.find_opt cache istr with
    | Some s -> s
    | None ->
        let s = Driver.sou base istr in
        Hashtbl.add cache istr s;
        s
end

(* ========================================================================= *)
(* Section 3: Core Search Functions                                         *)
(* ========================================================================= *)

let generate_permutations query =
  let normalized = Mutil.tr '-' ' ' query in
  let words =
    List.filter (fun w -> w <> "") (String.split_on_char ' ' normalized)
  in
  let join = String.concat " " in
  match words with
  | [ w1; w2 ] -> [ join [ w2; w1 ] ]
  | [ w1; w2; w3 ] ->
      [
        join [ w1; w3; w2 ];
        join [ w2; w3; w1 ];
        join [ w2; w1; w3 ];
        join [ w3; w1; w2 ];
        join [ w3; w2; w1 ];
      ]
  | [ w1; w2; w3; w4 ] ->
      [
        join [ w1; w2; w4; w3 ];
        join [ w1; w3; w2; w4 ];
        join [ w1; w3; w4; w2 ];
        join [ w1; w4; w2; w3 ];
        join [ w1; w4; w3; w2 ];
        join [ w2; w1; w3; w4 ];
        join [ w2; w1; w4; w3 ];
        join [ w2; w3; w1; w4 ];
        join [ w2; w3; w4; w1 ];
        join [ w2; w4; w1; w3 ];
        join [ w2; w4; w3; w1 ];
        join [ w3; w1; w2; w4 ];
        join [ w3; w1; w4; w2 ];
        join [ w3; w2; w1; w4 ];
        join [ w3; w2; w4; w1 ];
        join [ w3; w4; w1; w2 ];
        join [ w3; w4; w2; w1 ];
        join [ w4; w1; w2; w3 ];
        join [ w4; w1; w3; w2 ];
        join [ w4; w2; w1; w3 ];
        join [ w4; w2; w3; w1 ];
        join [ w4; w3; w1; w2 ];
        join [ w4; w3; w2; w1 ];
      ]
  | _ -> []

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  List.exists
    (fun n -> Name.strip n = k)
    (Driver.person_misc_names base p (nobtit conf base))

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Driver.p_first_name base p) in
  let sn = Name.strip_lower (Driver.p_surname base p) in
  k = fn ^ sn && fn <> "" && sn <> ""

let select_approx_key alias_cache conf base pl k =
  List.fold_right
    (fun p pl ->
      let iper = Driver.get_iper p in
      if person_is_approx_key base p k then (
        Some.AliasCache.add_direct alias_cache iper;
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
            Some.AliasCache.add_alias alias_cache iper alias_str;
            p :: pl
        | None ->
            if person_is_misc_name conf base p k then (
              Some.AliasCache.add_direct alias_cache iper;
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

(* Search by partial key: split on first space to get fn and sn, try all
   apostrophe variants of sn. *)
let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
  | None -> []
  | Some i ->
      let fn = String.sub n1 0 i in
      let fn_variants = generate_apostrophe_variants fn in
      let sn_raw = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let sn_variants = generate_apostrophe_variants sn_raw in
      List.concat_map
        (fun sn ->
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
                        split_normalize false
                          (Driver.sou base (Driver.get_first_name p))
                      in
                      let fn2_l =
                        split_normalize false
                          (Driver.sou base (Driver.get_public_name p))
                      in
                      if
                        List.exists
                          (fun fnv -> List.mem fnv fn1_l || List.mem fnv fn2_l)
                          fn_variants
                      then p :: pl
                      else pl)
                pl ipl)
            [] p_of_sn_l)
        sn_variants
      |> List.sort_uniq (fun a b ->
          compare (Driver.get_iper a) (Driver.get_iper b))

let search_key_aux aux conf base an =
  let variants_an = generate_apostrophe_variants an in
  let result =
    List.fold_left
      (fun acc1 an ->
        let found = Gutil.person_not_a_key_find_all base an in
        let an, found =
          if found = [] then
            match Util.name_with_roman_number an with
            | Some an1 ->
                let found = Gutil.person_ht_find_all base an1 in
                if found = [] then (an, []) else (an1, found)
            | None -> (an, found)
          else (an, found)
        in
        let found =
          Mutil.filter_map (fun i -> Util.pget_opt conf base i) found
        in
        let found = aux conf base found an in
        List.rev_append found acc1)
      [] variants_an
  in
  Gutil.sort_uniq_person_list base result

let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | None -> None
  | Some ip -> Util.pget_opt conf base ip

let search_key_opt conf base query =
  match search_by_key conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

let search_sosa_opt conf base query =
  match search_by_sosa conf base query with
  | None -> []
  | Some p -> [ Driver.get_iper p ]

let match_fn_lists fn_l fn1_l opts =
  let qlist = List.map normalize_query fn_l in
  let nlist = List.map normalize_name fn1_l in
  let passes_basic_test =
    if opts.exact1 then
      let qsorted =
        List.sort compare (List.map (fun q -> q.normalized) qlist)
      in
      let nsorted =
        List.sort compare (List.map (fun n -> n.normalized) nlist)
      in
      qsorted = nsorted
    else
      List.for_all
        (fun q ->
          List.exists (fun n -> Mutil.contains n.normalized q.normalized) nlist)
        qlist
  in
  let passes_order_test =
    if opts.order then
      if opts.exact1 then
        let rec in_order qs ns =
          match qs with
          | [] -> true
          | q :: qs' ->
              let rec advance rest =
                match rest with
                | [] -> false
                | n :: ns' ->
                    if n.normalized = q.normalized then in_order qs' ns'
                    else advance ns'
              in
              advance ns
        in
        in_order qlist nlist
      else
        let rec in_order qs ns =
          match qs with
          | [] -> true
          | q :: qs' ->
              let rec find_next rest =
                match rest with
                | [] -> false
                | n :: ns' ->
                    if Mutil.contains n.normalized q.normalized then
                      in_order qs' ns'
                    else find_next ns'
              in
              find_next ns
        in
        in_order qlist nlist
    else true
  in
  passes_basic_test && passes_order_test

(* Filter a person list by first name using StringCache for fast istr lookup.
   The [cache] argument is a per-request Hashtbl created at the top of
   [search] and threaded down to avoid any global mutable state.
   Batching removed: all processing is in-memory so batching only added
   complexity without benefit. *)
let search_for_multiple_fn cache conf base fn pl opts =
  Log.debug (fun k -> k "        search_for_multiple_fn: %s" fn);
  Log.debug (fun k -> k "order: %b, exact: %b" opts.order opts.exact1);
  let fn_l = cut_words fn in
  let result =
    List.fold_left
      (fun acc p ->
        if search_reject_p conf base p then acc
        else
          let fn1 =
            StringCache.get_cached cache base (Driver.get_first_name p)
          in
          let fn1_l = split_normalize false fn1 in
          let fn2 =
            StringCache.get_cached cache base (Driver.get_public_name p)
          in
          let fn2_l = split_normalize false fn2 in
          if match_fn_lists fn_l fn1_l opts || match_fn_lists fn_l fn2_l opts
          then p :: acc
          else acc)
      [] pl
  in
  Log.debug (fun k -> k "          result: %d" (List.length result));
  result

let iper_set_of_lists lists =
  List.fold_left
    (List.fold_left (fun s p -> Iper.Set.add (Driver.get_iper p) s))
    Iper.Set.empty lists

(* Partition a person list into (exact, partial) in a single pass:
   exact   = persons matching with opts.exact1 = true
   partial = persons matching with opts.exact1 = false (which includes
             everything in exact, by construction of match_fn_lists).
   Equivalent in result to two consecutive search_for_multiple_fn calls
   (one with exact1=true, one with exact1=false) but traverses pl once
   and looks up StringCache once per person. *)
let partition_for_multiple_fn cache conf base fn pl opts =
  let fn_l = cut_words fn in
  let opts_partial = { opts with exact1 = false } in
  let exact, partial =
    List.fold_left
      (fun (ex, pa) p ->
        if search_reject_p conf base p then (ex, pa)
        else
          let fn1 =
            StringCache.get_cached cache base (Driver.get_first_name p)
          in
          let fn1_l = split_normalize false fn1 in
          let fn2 =
            StringCache.get_cached cache base (Driver.get_public_name p)
          in
          let fn2_l = split_normalize false fn2 in
          let m_partial =
            match_fn_lists fn_l fn1_l opts_partial
            || match_fn_lists fn_l fn2_l opts_partial
          in
          if not m_partial then (ex, pa)
          else
            let m_exact =
              opts.exact1
              && (match_fn_lists fn_l fn1_l opts
                 || match_fn_lists fn_l fn2_l opts)
            in
            if m_exact then (p :: ex, pa) else (ex, p :: pa))
      ([], []) pl
  in
  Log.debug (fun k ->
      k "        partition_for_multiple_fn: %d exact, %d partial"
        (List.length exact) (List.length partial));
  (exact, partial)

(* Iterate partition_for_multiple_fn over a list of fn variants,
   union the results, dedupe by iper, and restore the
   exact ⊂ partial invariant across variants: an iper that
   passes exact1=true for any one fn variant is classified as
   exact globally, even if it would only pass exact1=false for
   another variant. *)
let partition_for_multiple_fn_variants cache conf base variants_fn pl opts =
  let exact, partial =
    List.fold_left
      (fun (ex_acc, pa_acc) fn_v ->
        let ex, pa = partition_for_multiple_fn cache conf base fn_v pl opts in
        (List.rev_append ex ex_acc, List.rev_append pa pa_acc))
      ([], []) variants_fn
  in
  let cmp a b = compare (Driver.get_iper a) (Driver.get_iper b) in
  let exact = List.sort_uniq cmp exact in
  let exact_ipers = iper_set_of_lists [ exact ] in
  let partial =
    List.sort_uniq cmp partial
    |> List.filter (fun p -> not (Iper.Set.mem (Driver.get_iper p) exact_ipers))
  in
  (exact, partial)

let rec search_surname conf base x =
  let variants = generate_apostrophe_variants x in
  let exact_results = search_exact conf base variants in
  if exact_results <> [] then (
    Log.debug (fun k ->
        k "  -> %d results (%d exact, 0 phonetic)"
          (List.length exact_results)
          (List.length exact_results));
    (exact_results, []))
  else
    let fallback_query = List.hd variants in
    let phonetic_results = search_phonetic conf base fallback_query in
    Log.debug (fun k ->
        k "  -> %d results (0 exact, %d phonetic)"
          (List.length phonetic_results)
          (List.length phonetic_results));
    ([], phonetic_results)

and search_exact conf base variants =
  let exact_iperl = ref Iper.Set.empty in
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
                (fun ip -> exact_iperl := Iper.Set.add ip !exact_iperl)
                iperl)
          list
      with _ -> ())
    variants;
  Iper.Set.elements !exact_iperl

(* Find persons whose surname *contains* [word] as a whitespace-delimited
   token, even when it is not the full surname.  For example, a query of
   "vivier" will match "de Lagoutte du Vivier" because "vivier" is one of
   the words of that compound surname.

   Strategy: use the surname index entry for [word] (same entry point as
   search_exact / search_phonetic) which yields a list of (stored_string,
   _, iperl) triples.  Keep only those where Name.lower stored_string
   contains [word_lower] as a *word* (not just a substring) — this avoids
   matching "Levivier" or "Viviereau" while correctly matching "de
   Lagoutte du Vivier".

   This is intentionally a post-filter on the index result set, not a
   full table scan, so it stays efficient on large bases. *)
and search_word_in_surname conf base word =
  let word_lower = Name.lower word in
  (* Skip very short tokens: a query like "Le" or "du" would otherwise
     match every compound surname containing those particles ("Le Bel",
     "du Vivier", etc.). Three characters and up are specific enough.
     This shortcut also returns [[]] when the user enters an empty query. *)
  if String.length word_lower <= 2 then []
  else
    let is_word_in str =
      List.exists (( = ) word_lower) (cut_words (Name.lower str))
    in
    let found = ref Iper.Set.empty in
    (try
       let list, _name_inj =
         Some.persons_of_fsname conf base Driver.base_strings_of_surname
           (Driver.spi_find (Driver.persons_of_surname base))
           Driver.get_surname word
       in
       List.iter
         (fun (str, _, iperl) ->
           if is_word_in str then
             List.iter (fun ip -> found := Iper.Set.add ip !found) iperl)
         list
     with _ -> ());
    Iper.Set.elements !found

and search_phonetic_generic conf base query base_strings spi_find _get_name =
  try
    let istrl = base_strings base query in
    let ddr_iperl = ref Iper.Set.empty in
    List.iter
      (fun istr ->
        let iperl = spi_find istr in
        List.iter
          (fun ip ->
            if not (Iper.Set.mem ip !ddr_iperl) then
              let p = Driver.poi base ip in
              if
                (not (Driver.Istr.is_empty (Driver.get_first_name p)))
                && (not (Driver.Istr.is_empty (Driver.get_surname p)))
                && not
                     (Util.is_hide_names conf p
                     && not (Util.authorized_age conf base p))
              then ddr_iperl := Iper.Set.add ip !ddr_iperl)
          iperl)
      istrl;
    Iper.Set.elements !ddr_iperl
  with _ -> []

and search_phonetic conf base query =
  let query_crushed = Name.crush_lower query in
  let all_results =
    search_phonetic_generic conf base query Driver.base_strings_of_surname
      (Driver.spi_find (Driver.persons_of_surname base))
      Driver.get_surname
  in
  List.filter
    (fun ip ->
      let p = Driver.poi base ip in
      let sn = Driver.sou base (Driver.get_surname p) in
      let sn_crushed = Name.crush_lower sn in
      (* Short crush codes (<=2 chars) require exact equality to avoid
         e.g. "Le" (-> "l") matching every name containing an "l" phoneme.
         Longer codes are specific enough that substring containment is safe. *)
      if String.length query_crushed <= 2 then sn_crushed = query_crushed
      else Mutil.contains sn_crushed query_crushed)
    all_results

and search_firstname_phonetic conf base query =
  let query_crushed = Name.crush_lower query in
  let all_results =
    search_phonetic_generic conf base query Driver.base_strings_of_first_name
      (Driver.spi_find (Driver.persons_of_first_name base))
      Driver.get_first_name
  in
  List.filter
    (fun ip ->
      let p = Driver.poi base ip in
      let fn = Driver.sou base (Driver.get_first_name p) in
      let fn_crushed = Name.crush_lower fn in
      (* Short crush codes (<=2 chars) require exact equality to avoid
         "lea" (-> "l") matching "Louis" (-> "ls"), "Jean Louis" (-> "jnl"),
         etc.  Longer codes are specific enough for substring containment. *)
      if String.length query_crushed <= 2 then fn_crushed = query_crushed
      else Mutil.contains fn_crushed query_crushed)
    all_results

let deduplicate_collect ipers f =
  let seen = Hashtbl.create 1000 in
  let result = ref [] in
  List.iter
    (fun ip ->
      if not (Hashtbl.mem seen ip) then (
        Hashtbl.add seen ip ();
        match f ip with Some x -> result := x :: !result | None -> ()))
    ipers;
  List.rev !result

let deduplicate_partition ipers classify =
  let seen = Hashtbl.create 1000 in
  let matched = ref [] in
  let unmatched = ref [] in
  List.iter
    (fun ip ->
      if not (Hashtbl.mem seen ip) then (
        Hashtbl.add seen ip ();
        if classify ip then matched := ip :: !matched
        else unmatched := ip :: !unmatched))
    ipers;
  (List.rev !matched, List.rev !unmatched)

(* Search first name direct index, trying all apostrophe variants of the
   query so that e.g. "O'Brien" finds "O'Brien" regardless of apostrophe
   encoding. *)
let search_firstname_direct conf base query =
  let all_ipers =
    if query = "" then []
    else
      let variants = generate_apostrophe_variants query in
      List.concat_map
        (fun variant ->
          try
            let istrl = Driver.base_strings_of_first_name base variant in
            let spi_find =
              Driver.spi_find (Driver.persons_of_first_name base)
            in
            List.concat_map spi_find istrl
          with _ -> [])
        variants
  in
  deduplicate_collect all_ipers (fun ip ->
      let p = Driver.poi base ip in
      let fn = Driver.sou base (Driver.get_first_name p) in
      if
        fn <> ""
        && (not (Driver.Istr.is_empty (Driver.get_first_name p)))
        && (not (Driver.Istr.is_empty (Driver.get_surname p)))
        && not
             (Util.is_hide_names conf p && not (Util.authorized_age conf base p))
      then Some ip
      else None)

(* Search first-name aliases, trying all apostrophe variants of both the
   query and the stored alias strings. *)
let search_firstname_aliases conf base query =
  let query_variants = generate_apostrophe_variants query in
  let query_lower_variants = List.map Name.lower query_variants in
  let all_misc_matches =
    List.concat_map
      (fun variant -> Gutil.person_not_a_key_find_all base variant)
      query_variants
    |> List.sort_uniq compare
  in
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
              let alias_str = Name.lower (Driver.sou base alias_istr) in
              List.mem alias_str query_lower_variants)
            aliases
        with
        | Some alias_istr ->
            let alias_str = Driver.sou base alias_istr in
            (ip, alias_str) :: acc
        | None -> acc)
    [] all_misc_matches

let search_firstname_phonetic_split alias_cache conf base query =
  let query_lower = Name.lower query in
  let query_norm = normalize_for_phonetic query_lower in
  let query_words = cut_words query_lower in
  let nb_words = List.length query_words in
  let phonetic_iperl =
    if nb_words <= 1 then
      let crushed = Name.crush_lower query_lower in
      search_firstname_phonetic conf base crushed
    else
      let word_results =
        List.map
          (fun w ->
            let crushed = Name.crush_lower w in
            (w, search_firstname_phonetic conf base crushed))
          query_words
      in
      let all_candidates = List.flatten (List.map snd word_results) in
      deduplicate_collect all_candidates (fun ip ->
          let p = Driver.poi base ip in
          let fn = Driver.sou base (Driver.get_first_name p) in
          let fn_norm = normalize_for_phonetic (Name.lower fn) in
          let matches_all_words =
            List.for_all
              (fun (word, _) ->
                let word_norm = normalize_for_phonetic word in
                Mutil.contains fn_norm word_norm)
              word_results
          in
          if matches_all_words then Some ip else None)
  in
  let firstname_variants = ref Mutil.StrSet.empty in
  let classify ip =
    Some.AliasCache.add_direct alias_cache ip;
    let p = Driver.poi base ip in
    let fn = Driver.sou base (Driver.get_first_name p) in
    let fn_norm = normalize_for_phonetic (Name.lower fn) in
    let is_match = Mutil.contains fn_norm query_norm in
    if is_match then (
      if fn <> "" then
        firstname_variants := Mutil.StrSet.add fn !firstname_variants;
      true)
    else false
  in
  let exact, partial = deduplicate_partition phonetic_iperl classify in
  (exact, partial, !firstname_variants)

let search_with_ngrams_complement conf base _query query_words =
  let nb_words = List.length query_words in
  if nb_words = 0 || nb_words = 1 || nb_words > 4 then []
  else
    let ngram = String.concat "" query_words in
    try
      let crushed = Name.crush ngram in
      search_firstname_phonetic conf base crushed
    with _ -> []

(* Phonetic ngrams complement: searches the first-name index using
   crush(concat(query_words)) as a single-token n-gram, then keeps only
   results whose normalized crushed first name contains the query's
   normalized crush.  Caller is responsible for any alias handling — this
   function does not invoke search_firstname_aliases. *)
let search_firstname_phonetic_ngrams alias_cache conf base query =
  let query_lower = Name.lower query in
  let query_norm = normalize_for_phonetic query_lower in
  let query_norm_crushed = Name.crush query_norm in
  let query_words = cut_words query_lower in
  let phonetic_iper =
    search_with_ngrams_complement conf base query query_words
  in
  let phonetic_results =
    deduplicate_collect phonetic_iper (fun ip ->
        let p = Driver.poi base ip in
        let fn = Driver.sou base (Driver.get_first_name p) in
        let fn_lower = Name.lower fn in
        let fn_norm = normalize_for_phonetic fn_lower in
        let fn_norm_crushed = Name.crush fn_norm in
        if Mutil.contains fn_norm_crushed query_norm_crushed then (
          Some.AliasCache.add_direct alias_cache ip;
          Some ip)
        else None)
  in
  Log.debug (fun k ->
      k "  Phonetic ngrams: %d candidates -> %d after dedup"
        (List.length phonetic_iper)
        (List.length phonetic_results));
  phonetic_results

(* Search first names with full apostrophe-variant support.
   Renamed from search_firstname_with_cache: no caching was ever performed
   here, so the old name was misleading. *)
let search_firstname alias_cache conf base query opts =
  if opts.absolute then
    let istrl = Driver.base_strings_of_first_name base query in
    let exact_ips =
      List.fold_left
        (fun acc istr ->
          let str = Driver.sou base istr in
          if str = query then
            acc @ Driver.spi_find (Driver.persons_of_first_name base) istr
          else acc)
        [] istrl
    in
    {
      direct = { persons = exact_ips; variants = Mutil.StrSet.empty };
      aliases = { persons = []; variants = Mutil.StrSet.empty };
      permuted = { persons = []; variants = Mutil.StrSet.empty };
      included = { persons = []; variants = Mutil.StrSet.empty };
      phonetic = { persons = []; variants = Mutil.StrSet.empty };
    }
  else
    let nq = normalize_query query in
    let nb_words = match nq.words with Some w -> List.length w | None -> 0 in
    Log.debug (fun k ->
        k "Search p=%s FirstNameOnly [aliases=%b, order=%b, exact=%b]" query
          opts.incl_aliases opts.order opts.exact1);
    let query_variants = generate_apostrophe_variants query in
    let query_lower_variants = List.map Name.lower query_variants in
    let istrl =
      List.concat_map
        (fun variant -> Driver.base_strings_of_first_name base variant)
        query_variants
      |> List.sort_uniq compare
    in
    let exact_istrl, other_istrl =
      List.partition
        (fun istr ->
          let str = Name.lower (Driver.sou base istr) in
          List.mem str query_lower_variants)
        istrl
    in
    let exact_ips =
      List.concat_map
        (fun istr -> Driver.spi_find (Driver.persons_of_first_name base) istr)
        exact_istrl
    in
    List.iter (fun ip -> Some.AliasCache.add_direct alias_cache ip) exact_ips;
    let other_ips =
      List.concat_map
        (fun istr -> Driver.spi_find (Driver.persons_of_first_name base) istr)
        other_istrl
    in
    List.iter (fun ip -> Some.AliasCache.add_direct alias_cache ip) other_ips;
    let normalize_person ip =
      let p = Driver.poi base ip in
      let fn = Driver.sou base (Driver.get_first_name p) in
      (normalize_name fn, ip)
    in
    let direct_exact =
      List.map
        (fun ip ->
          ( normalize_name
              (Driver.sou base (Driver.get_first_name (Driver.poi base ip))),
            ip ))
        exact_ips
    in
    let direct_included = List.map normalize_person other_ips in
    let exact_variants =
      List.fold_left
        (fun acc (n, _) ->
          if n.original <> "" then Mutil.StrSet.add n.original acc else acc)
        Mutil.StrSet.empty direct_exact
    in
    let alias_results, alias_variants =
      if opts.incl_aliases then (
        let aliases = search_firstname_aliases conf base query in
        List.iter
          (fun (ip, alias) -> Some.AliasCache.add_alias alias_cache ip alias)
          aliases;
        let norm_alias =
          List.map (fun (ip, _) -> normalize_person ip) aliases
        in
        ( List.map snd norm_alias,
          List.fold_left
            (fun acc (n, _) ->
              if n.original <> "" then Mutil.StrSet.add n.original acc else acc)
            Mutil.StrSet.empty norm_alias ))
      else ([], Mutil.StrSet.empty)
    in
    (* Permutations for order-insensitive first-name search, up to 4 words. *)
    let permuted_persons, permuted_variants =
      if opts.order && nb_words > 1 && nb_words < 5 then (
        let perms = generate_permutations query in
        let seen_perm = Hashtbl.create 100 in
        List.iter
          (fun ip -> Hashtbl.add seen_perm ip ())
          (List.map snd direct_exact);
        List.iter (fun ip -> Hashtbl.add seen_perm ip ()) alias_results;
        List.fold_left
          (fun (acc_persons, acc_vars) perm_query ->
            let perm_query_norm =
              normalize_for_phonetic (Name.lower perm_query)
            in
            let perm_query_crushed = Name.crush_lower perm_query in
            let perm_direct = search_firstname_direct conf base perm_query in
            List.iter
              (fun ip -> Some.AliasCache.add_direct alias_cache ip)
              perm_direct;
            let perm_alias =
              if opts.incl_aliases then
                search_firstname_aliases conf base perm_query
              else []
            in
            List.iter
              (fun (ip, alias) ->
                Some.AliasCache.add_alias alias_cache ip alias)
              perm_alias;
            let perm_all = perm_direct @ List.map fst perm_alias in
            let filtered_perm =
              List.filter
                (fun ip ->
                  if Hashtbl.mem seen_perm ip then false
                  else
                    let p = Driver.poi base ip in
                    let fn = Driver.sou base (Driver.get_first_name p) in
                    let fn_norm = normalize_for_phonetic (Name.lower fn) in
                    let fn_crushed = Name.crush_lower fn in
                    let matches =
                      Mutil.contains fn_crushed perm_query_crushed
                      && Mutil.contains fn_norm perm_query_norm
                    in
                    if matches then Hashtbl.add seen_perm ip ();
                    matches)
                perm_all
            in
            let perm_vars =
              List.fold_left
                (fun acc ip ->
                  let n, _ = normalize_person ip in
                  if n.original <> "" then Mutil.StrSet.add n.original acc
                  else acc)
                acc_vars filtered_perm
            in
            (acc_persons @ filtered_perm, perm_vars))
          ([], Mutil.StrSet.empty) perms)
      else ([], Mutil.StrSet.empty)
    in
    let included_iper, included_variants, phonetic_iper, phonetic_variants =
      if not opts.exact1 then (
        let query_crushed = Name.crush_lower nq.lower in
        let direct_included_results = List.map snd direct_included in
        let phonetic_ngrams =
          if opts.incl_aliases then
            search_firstname_phonetic_ngrams alias_cache conf base query
          else []
        in
        let exact_phonetic, partial_phonetic, _ =
          search_firstname_phonetic_split alias_cache conf base query
        in
        let all_phonetic = phonetic_ngrams @ exact_phonetic in
        let contains_query = ref [] in
        let other_phonetic = ref [] in
        let seen = Hashtbl.create 1000 in
        List.iter (fun ip -> Hashtbl.add seen ip ()) (List.map snd direct_exact);
        List.iter (fun ip -> Hashtbl.add seen ip ()) alias_results;
        List.iter (fun ip -> Hashtbl.add seen ip ()) permuted_persons;
        let process_list lst f =
          List.iter
            (fun ip ->
              if not (Hashtbl.mem seen ip) then (
                Hashtbl.add seen ip ();
                f ip))
            lst
        in
        (* When the phonetic crush code is very short (<=2 chars), substring
           containment is too broad: "lea"->"l" would match "Louis"->"ls",
           "Jean Louis"->"jnl", etc.  Require exact crush equality instead. *)
        let phonetic_matches fn_crushed =
          if String.length query_crushed <= 2 then fn_crushed = query_crushed
          else Mutil.contains fn_crushed query_crushed
        in
        process_list direct_included_results (fun ip ->
            let n, _ = normalize_person ip in
            if Mutil.contains n.lower nq.lower then
              contains_query := ip :: !contains_query
            else
              let fn_crushed = Name.crush_lower n.lower in
              if phonetic_matches fn_crushed then
                other_phonetic := ip :: !other_phonetic);
        process_list all_phonetic (fun ip ->
            let n, _ = normalize_person ip in
            if Mutil.contains n.lower nq.lower then
              contains_query := ip :: !contains_query
            else
              let fn_crushed = Name.crush_lower n.lower in
              if phonetic_matches fn_crushed then
                other_phonetic := ip :: !other_phonetic);
        process_list partial_phonetic (fun ip ->
            let n, _ = normalize_person ip in
            let fn_crushed = Name.crush_lower n.lower in
            if phonetic_matches fn_crushed then
              other_phonetic := ip :: !other_phonetic);
        let included_iper = List.rev !contains_query in
        let included_variants =
          List.fold_left
            (fun acc ip ->
              let n, _ = normalize_person ip in
              if n.original <> "" then Mutil.StrSet.add n.original acc else acc)
            Mutil.StrSet.empty included_iper
        in
        let phonetic_iper = List.rev !other_phonetic in
        let phonetic_variants =
          List.fold_left
            (fun acc ip ->
              let n, _ = normalize_person ip in
              if n.original <> "" then Mutil.StrSet.add n.original acc else acc)
            Mutil.StrSet.empty phonetic_iper
        in
        (included_iper, included_variants, phonetic_iper, phonetic_variants))
      else ([], Mutil.StrSet.empty, [], Mutil.StrSet.empty)
    in
    Log.debug (fun k ->
        k
          "  -> %d results (%d exact, %d alias, %d permuted, %d included, %d \
           phonetic)"
          (List.length direct_exact + List.length alias_results
          + List.length permuted_persons
          + List.length included_iper + List.length phonetic_iper)
          (List.length direct_exact)
          (List.length alias_results)
          (List.length permuted_persons)
          (List.length included_iper)
          (List.length phonetic_iper));
    {
      direct =
        { persons = List.map snd direct_exact; variants = exact_variants };
      aliases = { persons = alias_results; variants = alias_variants };
      permuted = { persons = permuted_persons; variants = permuted_variants };
      included = { persons = included_iper; variants = included_variants };
      phonetic = { persons = phonetic_iper; variants = phonetic_variants };
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

let search_fullname cache conf base variants_fn variants_sn =
  let variants_sn = List.sort_uniq compare variants_sn in
  let variants_fn =
    List.map (String.map (fun c -> if c = '-' then ' ' else c)) variants_fn
    |> List.sort_uniq compare
  in
  Log.debug (fun k ->
      k "      search_fullname: fn variants: %s; sn variants: %s"
        (String.concat ", " variants_fn)
        (String.concat ", " variants_sn));
  (* Gather surname-matching persons via search_surname (exact then phonetic
     fallback) instead of AdvSearchOk with exact_surname=on.  This allows
     phonetic matching when the query spelling differs from the stored
     surname (e.g. "dapzol" finds "Dazpol").

     Note: AdvSearchOk.advanced_search reads many keys from conf.env (place,
     dates, occupation, sex, etc.) — search_surname only consumes the query
     string.  This is the correct behaviour for the m=S route, which is the
     simple search dispatcher; the advanced-search filters belong to m=AS_OK
     which routes to AdvSearchOk.print directly and never reaches this code
     path.  If the simple-search URL ever needs to honour additional filters
     in the future, the right fix is to wrap search_surname results in a
     post-filter rather than reintroducing AdvSearchOk here. *)
  let all_iper =
    List.fold_left
      (fun acc sn ->
        let exact_b, phon_b = search_surname conf base sn in
        (* Also find persons whose compound surname contains [sn] as a
           whitespace-delimited word, e.g. "de Lagoutte du Vivier" for
           query "vivier".  Without this, only exact and phonetic matches
           on the full surname would be considered. *)
        let word_b = search_word_in_surname conf base sn in
        exact_b @ phon_b @ word_b @ acc)
      [] variants_sn
    |> List.sort_uniq compare
  in
  let persons =
    List.filter_map (fun ip -> Util.pget_opt conf base ip) all_iper
  in
  Log.debug (fun k ->
      k "      search_fullname: %d results" (List.length persons));
  match persons with
  | [] -> { exact = []; partial = []; spouse = [] }
  | pl ->
      let opts =
        { order = false; exact1 = true; incl_aliases = false; absolute = false }
      in
      let opts_partial = { opts with exact1 = false } in
      let exact, partial =
        partition_for_multiple_fn_variants cache conf base variants_fn pl opts
      in
      (* Phonetic crush fallback for fn: catches typos / double-letter
         differences like "fereol" vs "Ferreol" where substring matching
         fails but Name.crush_lower matches. Only applied to persons not
         already captured by the exact or partial passes above. *)
      let partial =
        let fn_crushed_variants =
          List.map Name.crush_lower variants_fn
          |> List.filter (fun s -> s <> "")
          |> List.sort_uniq String.compare
        in
        if fn_crushed_variants = [] then partial
        else
          let already = iper_set_of_lists [ exact; partial ] in
          let phonetic_extra =
            List.filter
              (fun p ->
                let ip = Driver.get_iper p in
                if Iper.Set.mem ip already then false
                else
                  let fn1 =
                    StringCache.get_cached cache base (Driver.get_first_name p)
                  in
                  let fn1_crushed = Name.crush_lower fn1 in
                  List.exists
                    (fun q_crush ->
                      if String.length q_crush <= 2 then fn1_crushed = q_crush
                      else Mutil.contains fn1_crushed q_crush)
                    fn_crushed_variants)
              pl
          in
          List.rev_append phonetic_extra partial
      in
      (* Reuse all_iper (already gathered above) for the spouse search to
         avoid redundant search_surname calls and duplicate results. *)
      let spouse =
        if List.assoc_opt "public_name_as_fn" conf.base_env <> Some "no" then
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
              [] all_iper
          in
          let spouse_substr =
            List.fold_left
              (fun acc fn_v ->
                List.rev_append
                  (search_for_multiple_fn cache conf base fn_v spouses
                     opts_partial)
                  acc)
              [] variants_fn
            |> List.sort_uniq (fun a b ->
                compare (Driver.get_iper a) (Driver.get_iper b))
          in
          (* Same phonetic crush fallback as for direct persons: catches
             first-name near-misses like "margerite" vs "Marguerite". *)
          let fn_crushed_variants =
            List.map Name.crush_lower variants_fn
            |> List.filter (fun s -> s <> "")
            |> List.sort_uniq String.compare
          in
          if fn_crushed_variants = [] then spouse_substr
          else
            let already = iper_set_of_lists [ exact; partial; spouse_substr ] in
            let phonetic_extra =
              List.filter
                (fun p ->
                  let ip = Driver.get_iper p in
                  if Iper.Set.mem ip already || search_reject_p conf base p then
                    false
                  else
                    let fn1 =
                      StringCache.get_cached cache base
                        (Driver.get_first_name p)
                    in
                    let fn1_crushed = Name.crush_lower fn1 in
                    List.exists
                      (fun q_crush ->
                        if String.length q_crush <= 2 then fn1_crushed = q_crush
                        else Mutil.contains fn1_crushed q_crush)
                      fn_crushed_variants)
                spouses
            in
            List.rev_append phonetic_extra spouse_substr
        else []
      in
      {
        exact = persons_to_ipers exact;
        partial = persons_to_ipers partial;
        spouse = persons_to_ipers spouse;
      }

(* Partial key search.  Both result paths now go through search_for_multiple_fn
   to verify the first name actually matches, avoiding false positives from
   the former shortcut branches that returned results without checking fn. *)
let search_partial_key cache conf base query =
  let pl = search_by_name conf base query in
  Log.debug (fun k -> k "      search_partial_key: %d results" (List.length pl));
  let n1 = Name.abbrev (Name.lower query) in
  let fn, sn =
    match String.index_opt n1 ' ' with
    | Some i ->
        (String.sub n1 0 i, String.sub n1 (i + 1) (String.length n1 - i - 1))
    | _ -> ("", n1)
  in
  let persons =
    if pl <> [] then pl
    else
      let conf_sn =
        { conf with env = ("surname", Adef.encoded sn) :: conf.env }
      in
      let persons, _ = AdvSearchOk.advanced_search conf_sn base max_int in
      persons
  in
  if persons = [] then { exact = []; partial = []; spouse = [] }
  else
    let opts =
      { order = false; exact1 = true; incl_aliases = false; absolute = false }
    in
    let variants_fn = generate_apostrophe_variants fn in
    let exact, partial =
      partition_for_multiple_fn_variants cache conf base variants_fn persons
        opts
    in
    {
      exact = persons_to_ipers exact;
      partial = persons_to_ipers partial;
      spouse = [];
    }

(* ========================================================================= *)
(* Section 4: Name Parsing and Component Extraction                         *)
(* ========================================================================= *)

let rec extract_name_components conf base =
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
  | None, None, Some pn -> parse_person_name base pn
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

(* Insert a '/' before the first surname particle found in pn, so that
   "henry de foresta" becomes "henry/de foresta" and the existing slash
   parser can correctly split fn from sn.  Apostrophe variants of each
   word-suffix are tried against the compiled particle regexp so that
   "renaud d'harcourt" is detected as having a particle regardless of
   the apostrophe encoding used in the input.

   The original suffix string is preserved verbatim after the slash:
   downstream apostrophe variant generation (generate_apostrophe_variants
   in search_fullname, search_by_name, etc.) handles the typographic form
   matching against the stored data, so the input form must not be
   canonicalised here. *)
and insert_slash_before_particle base pn =
  let re = Driver.base_particles base in
  let words = String.split_on_char ' ' pn in
  let n = List.length words in
  if n < 2 then pn
  else
    let rec aux i =
      if i >= n then pn
      else
        let suffix = String.concat " " (list_drop i words) in
        let variants = generate_apostrophe_variants suffix in
        let has_particle =
          List.exists (fun v -> Mutil.get_particle re v <> "") variants
        in
        if has_particle then
          let fn_part = String.concat " " (list_take i words) in
          fn_part ^ "/" ^ suffix
        else aux (i + 1)
    in
    aux 1

and parse_person_name base pn =
  let original_pn = pn in
  let pn = insert_slash_before_particle base pn in
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
              original = original_pn;
              format = `Space;
            };
      }
  | Some i, None, _ -> parse_slash_separated original_pn pn i
  | None, Some j, _ -> parse_dot_separated original_pn pn j
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

and parse_slash_separated original_pn pn slash_pos =
  let fn_part = String.sub pn 0 slash_pos in
  let sn_part =
    String.sub pn (slash_pos + 1) (String.length pn - slash_pos - 1)
    |> String.trim
  in
  match (fn_part, sn_part) with
  | "", sn -> make_parsed_component ~surname:sn original_pn `SlashSurname
  | fn, "" -> make_parsed_component ~first_name:fn original_pn `SlashFirstName
  | fn, sn ->
      make_parsed_component ~first_name:fn ~surname:sn original_pn `Slash

and parse_dot_separated original_pn pn dot_pos =
  let fn_part = String.sub pn 0 dot_pos in
  let rest = String.sub pn (dot_pos + 1) (String.length pn - dot_pos - 1) in
  match String.index_opt rest ' ' with
  | Some space_pos ->
      let oc = String.sub rest 0 space_pos in
      let sn_part =
        String.sub rest (space_pos + 1) (String.length rest - space_pos - 1)
        |> String.trim
      in
      make_parsed_component ~first_name:fn_part ~surname:sn_part ~oc original_pn
        `DotOc
  | None ->
      make_parsed_component ~first_name:fn_part ~surname:rest original_pn `Dot

(* ========================================================================= *)
(* Section 5: Search Orchestration                                          *)
(* ========================================================================= *)

(* Priority order when an iper appears in multiple result piles:
   exact > spouse > partial.

   Exact wins for direct query matches.  Spouse wins over partial because
   a person found as a spouse-by-surname match (via FullName) carries the
   information "this person is here because of their spouse", which is
   the right context to display them in.  If [remove_duplicates] kept
   them in partial instead, they would lose that context — appearing in
   the main list as if matched directly. *)
let remove_duplicates (results : search_results) =
  let seen =
    Hashtbl.create (List.length results.exact + List.length results.spouse)
  in
  List.iter (fun ip -> Hashtbl.add seen ip ()) results.exact;
  let spouse_filtered =
    List.filter
      (fun ip ->
        if Hashtbl.mem seen ip then false
        else (
          Hashtbl.add seen ip ();
          true))
      results.spouse
  in
  let partial_filtered =
    List.filter (fun ip -> not (Hashtbl.mem seen ip)) results.partial
  in
  {
    exact = results.exact;
    partial = partial_filtered;
    spouse = spouse_filtered;
  }

let execute_search_method cache alias_cache conf base components query method_
    fn_options =
  match method_ with
  | Sosa ->
      let results = search_sosa_opt conf base query in
      Log.debug (fun k -> k "    Method Sosa: %d results" (List.length results));
      { exact = results; partial = []; spouse = [] }
  | Key ->
      let results =
        generate_apostrophe_variants query
        |> List.concat_map (fun v -> search_key_opt conf base v)
        |> List.sort_uniq compare
      in
      Log.debug (fun k -> k "    Method Key: %d results" (List.length results));
      { exact = results; partial = []; spouse = [] }
  | Surname ->
      let exact, partial = search_surname conf base query in
      { exact; partial; spouse = [] }
  | FirstName ->
      let fn_results =
        search_firstname alias_cache conf base query fn_options
      in
      {
        exact =
          fn_results.direct.persons @ fn_results.aliases.persons
          @ fn_results.included.persons;
        partial = fn_results.phonetic.persons @ fn_results.permuted.persons;
        spouse = [];
      }
  | FullName ->
      let fn = Option.value components.first_name ~default:"" in
      let sn = Option.value components.surname ~default:query in
      let oc = Option.value components.oc ~default:"" in
      if fn = "" then { exact = []; partial = []; spouse = [] }
      else
        let variants_sn = generate_apostrophe_variants sn in
        let variants_fn =
          let base_variants = generate_apostrophe_variants fn in
          if oc = "" then base_variants
          else List.map (fun v -> v ^ "." ^ oc) base_variants
        in
        let results = search_fullname cache conf base variants_fn variants_sn in
        Log.debug (fun k ->
            k "    Method FullName: %d+%d+%d results"
              (List.length results.exact)
              (List.length results.partial)
              (List.length results.spouse));
        results
  | ApproxKey ->
      let persons =
        search_key_aux (select_approx_key alias_cache) conf base query
      in
      let exact_matches, partial_matches =
        List.partition
          (fun p ->
            let iper = Driver.get_iper p in
            match Some.AliasCache.get_alias alias_cache iper with
            | Some _ -> true
            | None -> false)
          persons
      in
      let exact_ipers = List.map Driver.get_iper exact_matches in
      let partial_ipers = List.map Driver.get_iper partial_matches in
      Log.debug (fun k ->
          k "    Method ApproxKey: %d exact, %d partial"
            (List.length exact_ipers)
            (List.length partial_ipers));
      { exact = exact_ipers; partial = partial_ipers; spouse = [] }
  | PartialKey ->
      let results = search_partial_key cache conf base query in
      Log.debug (fun k ->
          k "    Method PartialKey: %d+%d+%d results"
            (List.length results.exact)
            (List.length results.partial)
            (List.length results.spouse));
      results

let dispatch_search_methods cache alias_cache conf base components query
    search_order fn_options =
  let all_results = { exact = []; partial = []; spouse = [] } in
  let combined_results =
    List.fold_left
      (fun acc method_ ->
        let (results : search_results) =
          execute_search_method cache alias_cache conf base components query
            method_ fn_options
        in
        ({
           exact = List.rev_append acc.exact results.exact;
           partial = List.rev_append acc.partial results.partial;
           spouse = List.rev_append acc.spouse results.spouse;
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
  remove_duplicates results

(* ========================================================================= *)
(* Section 6: Result Handling and Display                                   *)
(* ========================================================================= *)

let rec handle_search_results alias_cache conf base query fn_options components
    specify results =
  let redirect_to_person ip =
    record_visited conf ip;
    let p = Driver.poi base ip in
    Geneweb_http.Server.http_redirect_temporarily
      (Adef.(Util.commd conf ^^^ Util.acces conf base p) :> string)
  in
  let { exact; partial; spouse } = results in
  match exact with
  | [ single_exact ] -> redirect_to_person single_exact
  | _ -> (
      let all_persons = exact @ partial @ spouse in
      match all_persons with
      | [] -> SrcfileDisplay.print_welcome conf base
      | [ single_person ] -> redirect_to_person single_person
      | _multiple_persons -> (
          let exact_persons = List.map (Driver.poi base) exact in
          let partial_persons = List.map (Driver.poi base) partial in
          let spouse_persons = List.map (Driver.poi base) spouse in
          match components.case with
          | SurnameOnly sn ->
              display_surname_results conf base alias_cache query sn all_persons
          | ParsedName { first_name = None; surname = Some sn; _ } ->
              display_surname_results conf base alias_cache query sn all_persons
          | ParsedName { first_name = Some fn; surname = None; _ } ->
              display_firstname_results conf base alias_cache fn fn_options
                (search_firstname alias_cache conf base fn fn_options)
          | FirstNameSurname (_fn, _sn) ->
              specify conf base alias_cache query exact_persons partial_persons
                spouse_persons
          | ParsedName { format = `Space; _ }
            when fn_options.exact1 && List.length exact_persons = 1 ->
              redirect_to_person (Driver.get_iper (List.hd exact_persons))
          | _ ->
              specify conf base alias_cache query exact_persons partial_persons
                spouse_persons))

and display_firstname_results conf base alias_cache query fn_options results =
  let include_aliases = fn_options.incl_aliases in
  let is_partial = not fn_options.exact1 in
  let make_section ipers =
    if ipers = [] then [] else [ ("", List.map (Driver.poi base) ipers) ]
  in
  let sections_exact = make_section results.direct.persons in
  let sections_aliases =
    if include_aliases then make_section results.aliases.persons else []
  in
  let sections_included = make_section results.included.persons in
  let sections_partial = make_section results.phonetic.persons in
  let sections_permuted = make_section results.permuted.persons in
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
  Some.first_name_print_list_multi conf base alias_cache query sections_groups

and display_surname_results conf base alias_cache _query surname all_persons =
  let surname_groups = group_by_surname base all_persons in
  match surname_groups with
  | [ (single_surname, _) ] ->
      Some.search_surname_print conf base alias_cache
        (fun _conf _x -> ())
        single_surname
  | multiple_surnames -> (
      match p_getenv conf.env "m" with
      | Some "SN" ->
          Some.print_surname_details conf base alias_cache surname
            multiple_surnames
      | _ ->
          Some.print_several_possible_surnames surname conf base alias_cache
            ([], multiple_surnames))

(* Top-level search entry point.  Fresh StringCache and Some.AliasCache
   instances are created here and threaded through the call chain so each
   request starts with a clean cache scoped to the current base, with no
   global mutable state.  All search methods handle apostrophe variants
   internally (search_by_key included); no per-variant iteration is needed
   at this level. *)
let search conf base query search_order fn_options specify =
  let cache = StringCache.create () in
  let alias_cache = Some.AliasCache.create () in
  let components = extract_name_components conf base in
  let results =
    dispatch_search_methods cache alias_cache conf base components query
      search_order fn_options
  in
  handle_search_results alias_cache conf base query fn_options components
    specify results

(* ========================================================================= *)
(* Section 7: Main Entry Point                                              *)
(* ========================================================================= *)

module Debug = struct
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
end

let print conf base specify =
  let components = extract_name_components conf base in
  let fn_options =
    {
      order = p_getenv conf.env "p_order" = Some "on";
      exact1 = p_getenv conf.env "p_exact" <> Some "off";
      incl_aliases = p_getenv conf.env "fna" <> None;
      absolute = p_getenv conf.env "t" = Some "A";
    }
  in
  let case = components.case in
  let log_case msg = Log.debug (fun k -> k "Print case %s" msg) in
  let log_format fmt =
    Log.debug (fun k -> k "Print format %s" (Debug.format_str fmt))
  in
  let search_with query order =
    search conf base query order fn_options specify
  in
  let full_order = [ Sosa; Key; FullName; ApproxKey; PartialKey; Surname ] in
  let name_order = [ Key; FullName; ApproxKey; PartialKey; Surname ] in
  let surname_order = [ Surname ] in
  let firstname_order = [ FirstName ] in
  match case with
  | FirstNameSurname (fn, sn) ->
      log_case (Printf.sprintf "FirstNameSurname (%s, %s)" fn sn);
      search_with (fn ^ " " ^ sn) name_order
  | PersonName pn ->
      log_case (Printf.sprintf "PersonName (%s)" pn);
      search_with pn name_order
  | FirstNameOnly fn ->
      let alias_cache = Some.AliasCache.create () in
      let results = search_firstname alias_cache conf base fn fn_options in
      display_firstname_results conf base alias_cache fn fn_options results
  | SurnameOnly sn ->
      log_case (Printf.sprintf "SurnameOnly '%s'" sn);
      search_with sn surname_order
  | ParsedName { first_name = fn; surname = sn; oc; format; _ } -> (
      match (fn, sn) with
      | Some fn, None when fn <> "" ->
          log_case (Printf.sprintf "ParsedName (fn = %s)" fn);
          search_with fn firstname_order
      | None, Some sn when sn <> "" ->
          log_case (Printf.sprintf "ParsedName (sn = %s)" sn);
          search_with sn surname_order
      | _ ->
          let fn = Option.value fn ~default:"" in
          let sn = Option.value sn ~default:"" in
          let oc = Option.value oc ~default:"" in
          log_format format;
          let query =
            match format with
            | `DotOc -> Printf.sprintf "%s.%s %s" fn oc sn
            | `SlashSurname -> sn
            | `SlashFirstName -> fn
            | _ -> Printf.sprintf "%s %s" fn sn
          in
          let order =
            match format with
            | `SlashSurname -> [ Surname; ApproxKey ]
            | `SlashFirstName -> firstname_order
            | _ -> full_order
          in
          search_with query order)
  | _ ->
      log_case (Debug.case_str case);
      SrcfileDisplay.print_welcome conf base
