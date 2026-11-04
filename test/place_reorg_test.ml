(* PlaceReorg: "town (rest) (department)" written "rest, town, department",
   the codes of the French departments written with their names. *)

open Alcotest
open Geneweb

let commas = PlaceReorg.reorganize PlaceReorg.Commas
let parens = PlaceReorg.reorganize PlaceReorg.Parens

let test_reorganize () =
  let t = option string in
  List.iter
    (fun (raw, expected) -> (check t) raw expected (commas raw))
    [
      ( "Nantes (2ème canton) (Loire-Atlantique)",
        Some "2ème canton, Nantes, Loire-Atlantique" );
      ( "Nantes (44) (Rue du Bocage)",
        Some "Rue du Bocage, Nantes, Loire-Atlantique" );
      ( "Nantes (44)  Saint Nicolas",
        Some "Saint Nicolas, Nantes, Loire-Atlantique" );
      ( "Nantes (2ème canton), Loire-Atlantique",
        Some "2ème canton, Nantes, Loire-Atlantique" );
      ( "Nantes (2<sup>e</sup> division) (Loire-Atlantique)",
        Some "2e division, Nantes, Loire-Atlantique" );
      ("Nantes (44)", Some "Nantes, Loire-Atlantique");
      ("Nantes (44) (Loire-Atlantique)", Some "Nantes, Loire-Atlantique");
      ("Nantes (44), France", Some "Nantes, Loire-Atlantique, France");
      ("Nantes (Loire-Inférieure)", Some "Nantes, Loire-Inférieure");
      (* left out *)
      ("Nantes, Loire-Atlantique", None);
      ("Nantes (Fraternité-Agriculture)", None);
      ("Nantes (44) (Gironde)", None);
      ("Paris (16)", None);
      ("[Boileau] - Nantes (44)", None);
      ("Nantes (44", None);
    ];
  List.iter
    (fun (raw, expected) -> (check t) ("parens: " ^ raw) expected (parens raw))
    [
      ( "Nantes (2ème canton) (Loire-Atlantique)",
        Some "2ème canton, Nantes (Loire-Atlantique)" );
      ("Nantes (44)", Some "Nantes (Loire-Atlantique)");
      ("Nantes (Loire-Atlantique)", None);
    ]

let test_departments () =
  let t = option string in
  (check t) "44" (Some "Loire-Atlantique") (FrDepartments.name_of_code "44");
  (check t) "2A" (Some "Corse-du-Sud") (FrDepartments.name_of_code "2a");
  (check t) "974" (Some "La Réunion") (FrDepartments.name_of_code "974");
  (check t) "16e" None (FrDepartments.name_of_code "16e");
  (check t) "4" None (FrDepartments.name_of_code "4");
  (check bool) "spelling" true (FrDepartments.is_name "loire atlantique");
  (check bool) "not a department" false (FrDepartments.is_name "Nantes")

let v =
  [
    ( "place-reorg",
      [
        test_case "reorganize" `Quick test_reorganize;
        test_case "departments" `Quick test_departments;
      ] );
  ]
