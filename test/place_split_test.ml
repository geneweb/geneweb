(* PlaceSplit: the terms ending with a frequent term, the comma forgotten. *)

open Alcotest
open Geneweb

let test_split_at () =
  let frequent = Hashtbl.create 4 in
  Hashtbl.replace frequent (PlaceTerms.cluster_key "Canada") "Canada";
  Hashtbl.replace frequent (PlaceTerms.cluster_key "Saint-Malo") "Saint-Malo";
  let t = option (triple string string string) in
  (check t) "missing comma"
    (Some ("Montreal", "Canada", "canada"))
    (PlaceSplit.split_at frequent "Montreal Canada");
  (check t) "lowercase end"
    (Some ("Trois Rivieres", "canada", "canada"))
    (PlaceSplit.split_at frequent "Trois Rivieres canada");
  (check t) "the longest end"
    (Some ("Paramé", "St Malo", "st malo"))
    (PlaceSplit.split_at frequent "Paramé St Malo");
  (check t) "particle" None
    (PlaceSplit.split_at frequent "Saint-Pierre de Canada");
  (check t) "particle dropped by the key" None
    (PlaceSplit.split_at frequent "Pont of Canada");
  (check t) "elided particle" None
    (PlaceSplit.split_at frequent "Port d' Canada");
  (check t) "no space" None (PlaceSplit.split_at frequent "Canada");
  (check t) "no frequent end" None
    (PlaceSplit.split_at frequent "Montreal Quebec")

let test_proposals () =
  let istr = Geneweb_db.Driver.Istr.of_string in
  let entries =
    PlaceTerms.of_places false
      [
        (istr "1", "Montreal, Canada", 5);
        (istr "2", "Quebec, Canada", 5);
        (istr "3", "Montreal Canada", 2);
        (istr "4", "Gaspe Canada", 1);
        (istr "5", "Laval Canada, Canada", 1);
      ]
  in
  let show =
    List.map
      (fun (p : PlaceSplit.proposal) ->
        (PlaceSplit.new_term p, p.known, List.length p.occurrences))
      (PlaceSplit.proposals ~min_uses:10 entries)
  in
  (* "Laval Canada" is already under "Canada": not proposed *)
  (check (list (triple string bool int)))
    "proposals"
    [ ("Montreal, Canada", true, 1); ("Gaspe, Canada", false, 1) ]
    show

let v =
  [
    ( "place-split",
      [
        test_case "split_at" `Quick test_split_at;
        test_case "proposals" `Quick test_proposals;
      ] );
  ]
