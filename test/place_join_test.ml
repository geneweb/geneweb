(* PlaceJoin: two terms written as one frequent term, and the numbering of
   the ordinals. *)

open Alcotest
open Geneweb

let test_key () =
  List.iter
    (fun (a, b) ->
      (check string) (a ^ " ~ " ^ b) (PlaceJoin.key a) (PlaceJoin.key b))
    [
      ("Paris 14e", "Paris XIVe");
      ("Paris 14e", "Paris 14\195\168me");
      ("Paris 14e", "Paris 14");
      ("Paris 1er", "Paris Ier");
      ("Paris 1er", "Paris 1re");
    ];
  (check string) "key" "paris 14e" (PlaceJoin.key "Paris XIVe");
  (check bool) "15e / 14e" false
    (PlaceJoin.key "Paris 15e" = PlaceJoin.key "Paris 14e")

let test_proposals () =
  let istr = Geneweb_db.Driver.Istr.of_string in
  let places =
    [
      (istr "1", "Paris 1er, Seine, France", 5);
      (istr "2", "Paris 14e, Seine, France", 5);
      (istr "3", "Paris, 1er, Seine", 2);
      (istr "4", "Paris XIVe, Seine", 1);
      (istr "5", "Paris (1er)", 1);
      (istr "6", "Bordeaux Gironde), France", 9);
      (istr "7", "Bordeaux, Gironde, France", 1);
    ]
  in
  let entries = PlaceTerms.of_places false places in
  let show =
    List.map
      (fun (p : PlaceJoin.proposal) ->
        (p.written, p.target, List.length p.places))
      (PlaceJoin.proposals ~min_uses:5 false entries places)
  in
  (* "Paris (1er)": the two terms are not separated by a comma; "Bordeaux,
     Gironde": not numbered, not joined into the erroneous "Bordeaux
     Gironde)" *)
  (check (list (triple string string int)))
    "proposals"
    [ ("Paris, 1er", "Paris 1er", 1); ("Paris XIVe", "Paris 14e", 1) ]
    show

let v =
  [
    ( "place-join",
      [
        test_case "key" `Quick test_key;
        test_case "proposals" `Quick test_proposals;
      ] );
  ]
