(* OccuTerms: the occupations of a comma-separated list and their clusters of
   spellings. *)

open Alcotest
open Geneweb

let spans s =
  List.map (fun (sp : OccuTerms.span) -> (sp.text, sp.pos)) (OccuTerms.spans s)

let test_spans () =
  let t = list (pair string int) in
  (check t) "two occupations"
    [ ("laboureur", 0); ("marchand", 11) ]
    (spans "laboureur, marchand");
  (check t) "spaces, empty item, comma between parentheses"
    [ ("a ", 1); ("b", 4); ("c (d, e) ", 8) ]
    (spans " a ,b,, c (d, e) ");
  (check t) "comma between brackets"
    [ ("notaire [royal, apostolique]", 0) ]
    (spans "notaire [royal, apostolique]");
  (check t) "empty" [] (spans "");
  (check t) "only separators" [] (spans " , ,")

let test_clusters () =
  let d =
    List.mapi
      (fun i (raw, uses) ->
        (Geneweb_db.Driver.Istr.of_string (string_of_int i), raw, uses))
      [
        ("laboureur, marchand", 3);
        ("Laboureur", 2);
        ("laboureur , Marchand (de vin, de grains)", 1);
      ]
  in
  let show =
    List.map
      (fun (c : OccuTerms.cluster) ->
        ( c.ckey,
          List.map
            (fun (sp : SourceTerms.spelling) -> (sp.text, sp.uses))
            c.spellings,
          c.total ))
      (OccuTerms.clusters d)
  in
  (check (list (triple string (list (pair string int)) int)))
    "clusters"
    [
      ("laboureur", [ ("laboureur", 3); ("Laboureur", 2); ("laboureur ", 1) ], 6);
    ]
    show;
  (check (list (pair string (list string))))
    "samples"
    [ ("laboureur ", [ "laboureur , Marchand (de vin, de grains)" ]) ]
    (List.map
       (fun (t, l) -> (t, List.map fst l))
       (OccuTerms.samples d [ "laboureur " ] 5))

(* a single spelling is a cluster when it has spaces at both ends or
   invisible characters, to clean it *)
let test_unclean () =
  let d =
    List.mapi
      (fun i raw ->
        (Geneweb_db.Driver.Istr.of_string (string_of_int i), raw, 1))
      [ "notaire "; "cur\u{200B}\u{e9}"; "meunier" ]
  in
  (check (list (list string)))
    "unclean singles"
    [ [ "cur\u{200B}\u{e9}" ]; [ "notaire " ] ]
    (List.map
       (fun (c : OccuTerms.cluster) ->
         List.map (fun (sp : SourceTerms.spelling) -> sp.text) c.spellings)
       (OccuTerms.clusters d))

let v =
  [
    ( "occu-terms",
      [
        test_case "spans" `Quick test_spans;
        test_case "clusters" `Quick test_clusters;
        test_case "unclean singles" `Quick test_unclean;
      ] );
  ]
