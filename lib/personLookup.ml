(* Copyright (c) 1998-2007 INRIA *)

open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

(* Tests whether [input] is a recognized identifier for [p]: either
   "firstname surname" (modulo Name.strip_lower) or one of the person's
   misc names (titles, public names, qualifiers, aliases). *)
let person_matches_input conf base p input =
  let input = Name.strip_lower input in
  input
  = Name.strip_lower (Driver.p_first_name base p ^ " " ^ Driver.p_surname base p)
  || List.exists
       (fun n -> Name.strip n = input)
       (Driver.person_misc_names base p (nobtit conf base))

(* Resolves [input] string to a list of persons, used by free-form-input
   routes (m=R, m=NG).  Returns [(persons, sosa_acc)] where [sosa_acc]
   is true when the result came via the Sosa path — callers use that
   flag to decide whether to redirect directly or render a specify page.

   Strategies tried in order:
   - Sosa number against the base's sosa-reference;
   - exact unique key via SearchName.search_by_key;
   - approximate match via SearchName.search_key_aux, preferring
     strict input matches but falling back to search_by_name. *)
let lookup_person_by_input conf base input =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string input) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | Some p, Some n when n <> Sosa.zero -> (
      match Util.branch_of_sosa conf base n p with
      | Some (p :: _) -> ([ p ], true)
      | _ -> ([], false))
  | _ -> (
      match SearchName.search_by_key conf base input with
      | Some p -> ([ p ], false)
      | None ->
          let aux conf base acc input =
            let strict =
              List.filter (fun p -> person_matches_input conf base p input) acc
            in
            if strict <> [] then strict
            else if acc <> [] then acc
            else SearchName.search_by_name conf base input
          in
          (SearchName.search_key_aux aux conf base input, false))

(* Resolve [input] via {!lookup_person_by_input} and dispatch the result:
   - empty result → search_surname_print with the [not_found] fallback;
   - single hit reachable as a key/Sosa/canonical name → redirect via
     [redirect_to_person];
   - single hit otherwise, or multiple hits → render the specify page.
   Shared between the m=R and m=NG legacy routes. *)
let redirect_or_specify conf base ~not_found ~redirect_to_person input =
  let alias_cache = Some.AliasCache.create () in
  let pl, sosa_acc = lookup_person_by_input conf base input in
  match pl with
  | [] -> Some.search_surname_print conf base alias_cache not_found input
  | [ p ] ->
      if
        sosa_acc
        || Gutil.person_of_string_key base input <> None
        || person_matches_input conf base p input
      then redirect_to_person conf base p
      else Some.specify conf base alias_cache input pl [] []
  | pl -> Some.specify conf base alias_cache input pl [] []
