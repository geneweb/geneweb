open Geneweb
open Alcotest

let normalize () =
  (check string) "" "foo-bar, boobar (baz)"
    (Place.normalize "[foo-bar] - boobar (baz)");
  (check string) "" "[foo-bar - boobar (baz)"
    (Place.normalize "[foo-bar - boobar (baz)");
  (check string) "" "[foo-bar] boobar (baz)"
    (Place.normalize "[foo-bar] boobar (baz)");
  ()

let split_suburb () =
  (check (pair string string))
    ""
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] - boobar (baz)");
  (check (pair string string))
    "test split suburb emdash - 93"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] – boobar (baz)");
  (check (pair string string))
    "test split suburb endash - 94"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] — boobar (baz)");
  (check (pair string string))
    "" ("", "boobar (baz)")
    (Place.split_suburb "boobar (baz)");
  ()

let only_suburb () =
  (check string) "" "foo-bar" (Place.only_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "" (Place.only_suburb "boobar (baz)");
  ()

let without_suburb () =
  (check string) "" "boobar (baz)"
    (Place.without_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "boobar (baz)" (Place.without_suburb "boobar (baz)");
  ()

let compare_places () =
  (check int) "" 0 (Place.compare_places "boobar (baz)" "boobar (baz)");
  (check int) "" (-1) (Place.compare_places "baz (boobar)" "boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[bar-foo] - baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz (boobar)" "[bar-foo] - boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - ebaz (boobar)"
       "[bar-foo] - éboobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz, boobar, barboo"
       "[foo-bar] - baz, boobar, barboo, bam");
  ()

let normalize_place () =
  (* Canonical leaf-first form; must byte-match m=L p0/p%d and *_place_norm. *)
  (check string) "berlin false" "Berlin, Allemagne"
    (Place.normalize_place false "Berlin, Allemagne");
  (check string) "berlin true" "Allemagne, Berlin"
    (Place.normalize_place true "Berlin, Allemagne");
  (check string) "paren false" "Paris, 75"
    (Place.normalize_place false "Paris (75)");
  (check string) "paren true" "75, Paris"
    (Place.normalize_place true "Paris (75)");
  (check string) "suburb false" "Paris 16e"
    (Place.normalize_place false "[Hameau Boileau] - Paris 16e");
  (check string) "suburb true" "Paris 16e"
    (Place.normalize_place true "[Hameau Boileau] - Paris 16e");
  (check string) "three false" "Paris, \195\142le-de-France, France"
    (Place.normalize_place false "Paris, \195\142le-de-France, France");
  (check string) "three true" "France, \195\142le-de-France, Paris"
    (Place.normalize_place true "Paris, \195\142le-de-France, France");
  ()

let fold_place_long () =
  (check (pair (list string) string))
    "fold non-inverted"
    ([ "France"; "Paris" ], "")
    (Place.fold_place_long false "Paris, France");
  (check (pair (list string) string))
    "fold inverted"
    ([ "Paris"; "France" ], "")
    (Place.fold_place_long true "Paris, France");
  (check (pair (list string) string))
    "fold paren"
    ([ "75"; "Paris" ], "")
    (Place.fold_place_long false "Paris (75)");
  (check (pair (list string) string))
    "fold suburb"
    ([ "Paris 16e" ], "Hameau Boileau")
    (Place.fold_place_long false "[Hameau Boileau] - Paris 16e");
  ()

let components () =
  let texts inv s =
    List.map
      (fun (c : Place.component) ->
        (* the span must point at the text in the raw string *)
        assert (String.sub s c.pos c.len = c.text);
        if c.suburb then "[" ^ c.text ^ "]" else c.text)
      (Place.components inv s)
  in
  let same_as_fold inv s =
    let pl, sub = Place.fold_place_long inv s in
    (check (list string))
      ("components = fold_place_long: " ^ s)
      (pl @ if sub = "" then [] else [ "[" ^ sub ^ "]" ])
      (texts inv s)
  in
  (check (list string))
    "suburb last, root first"
    [ "75"; "Paris"; "[Boileau]" ]
    (texts false "[Boileau] - Paris (75)");
  (check (list string))
    "inverted" [ "Paris"; "France" ]
    (texts true "Paris, France");
  List.iter
    (fun s ->
      same_as_fold false s;
      same_as_fold true s)
    [
      "Paris, France";
      "Paris (75)";
      "[Hameau Boileau] - Paris 16e";
      "[Boileau] \226\128\147 Saint-Martin, Manche, France";
      "a (b) , c (d)";
      "Paris , 75 ,France";
      " Paris";
      "x ()";
      "[a(b)] - c";
      "[a] - c, d (e f)";
      "a,,b";
    ];
  ()

let of_places () =
  let istr = Geneweb_db.Driver.Istr.of_string in
  let entries =
    PlaceTerms.of_places false
      [
        (istr "1", "Saint-Martin, Manche, France", 3);
        (istr "2", "[Le Bourg] - Saint-Martin, Manche, France", 2);
        (istr "3", "Saint-Martin, Calvados, France", 1);
      ]
  in
  let find t = List.find (fun (e : PlaceTerms.entry) -> e.term = t) entries in
  let sm = find "Saint-Martin" in
  (check int) "Saint-Martin contexts" 2 (List.length sm.contexts);
  (check int) "Saint-Martin places" 3 sm.nb_places;
  (check int) "Saint-Martin uses" 6 sm.uses;
  (check (list (list string)))
    "Saint-Martin parents, root first"
    [ [ "France"; "Calvados" ]; [ "France"; "Manche" ] ]
    (List.map (fun (c : PlaceTerms.context) -> c.parents) sm.contexts);
  let fr = find "France" in
  (check int) "France one top-level context" 1 (List.length fr.contexts);
  (check int) "France uses" 6 fr.uses;
  let bourg = find "Le Bourg" in
  (check bool) "suburb flagged" true (List.hd bourg.contexts).suburb;
  (check (list string))
    "suburb parents = full chain"
    [ "France"; "Manche"; "Saint-Martin" ]
    (List.hd bourg.contexts).parents;
  ()

let rename_rewrite () =
  let rw inv t nt raw ds =
    PlaceRename.rewrite inv ~terms:[ t ] ~new_term:nt raw ds
  in
  (check (option string))
    "leaf" (Some "Saint-L\195\180, Manche, France")
    (rw false "St-L\195\180" "Saint-L\195\180" "St-L\195\180, Manche, France"
       [ Some 2 ]);
  (check (option string))
    "trailing parenthesis kept" (Some "St-L\195\180 (Manche)")
    (rw false "50" "Manche" "St-L\195\180 (50)" [ Some 0 ]);
  (check (option string))
    "only the chosen depth" (Some "Paris, Lut\195\168ce, France")
    (rw false "Paris" "Lut\195\168ce" "Paris, Paris, France" [ Some 1 ]);
  (check (option string))
    "two depths" (Some "Lut\195\168ce, Lut\195\168ce, France")
    (rw false "Paris" "Lut\195\168ce" "Paris, Paris, France" [ Some 1; Some 2 ]);
  (check (option string))
    "suburb, separator kept" (Some "[Bourg] \226\128\147 Saint-Martin")
    (rw false "Le Bourg" "Bourg" "[Le Bourg] \226\128\147 Saint-Martin" [ None ]);
  (check (option string))
    "inverted" (Some "FR, Manche, Saint-L\195\180")
    (rw true "France" "FR" "France, Manche, Saint-L\195\180" [ Some 0 ]);
  (check (option string))
    "term no longer there" None
    (rw false "Manche" "Calvados" "Saint-Lo, Orne, France" [ Some 1 ]);
  (check bool) "new term ok" true (PlaceRename.valid_new_term "Saint-L\195\180");
  (check bool) "comma allowed" true (PlaceRename.valid_new_term "a, b");
  (check bool) "empty refused" false (PlaceRename.valid_new_term "  ");
  (check bool) "parenthesis refused" false (PlaceRename.valid_new_term "a (b)");
  ()

let rename_occurrence_key () =
  let i = Geneweb_db.Driver.Istr.of_string "123" in
  List.iter
    (fun o ->
      (check bool) "round trip" true
        (PlaceRename.occurrence_of_key (PlaceRename.occurrence_key o) = Some o))
    [ (i, Some 0); (i, Some 4); (i, None) ];
  (check bool) "other key" true (PlaceRename.occurrence_of_key "oc" = None);
  (check bool) "bad depth" true (PlaceRename.occurrence_of_key "o12.x" = None);
  ()

let clusters () =
  let k = PlaceTerms.cluster_key in
  List.iter
    (fun (a, b) -> (check string) (a ^ " ~ " ^ b) (k a) (k b))
    [
      ("Saint-Malo", "St Malo");
      ("Saint-Malo", "Saint Malo");
      ("Saint-Malo", "saint-malo");
      ("Saint-L\195\180", "St-L\195\180");
      ("Saint-L\195\180", "Saint Lo");
      ("Sainte-M\195\168re-\195\137glise", "Ste M\195\168re Eglise");
      ("Granville", "Granville ");
    ];
  (check bool) "Saint-Martin / Saint-Malo" false
    (k "Saint-Martin" = k "Saint-Malo");
  let istr = Geneweb_db.Driver.Istr.of_string in
  let entries =
    PlaceTerms.of_places false
      [
        (istr "1", "Saint-Malo, Ille-et-Vilaine, France", 3);
        (istr "2", "St Malo, Ille-et-Vilaine, France", 1);
        (istr "3", "saint-malo, Ille-et-Vilaine, France", 1);
        (istr "4", "Granville, Manche, France", 1);
      ]
  in
  (check (list (pair string (list string))))
    "clusters, most used first"
    [ ("st malo", [ "Saint-Malo"; "St Malo"; "saint-malo" ]) ]
    (List.map
       (fun (key, l) ->
         (key, List.map (fun (e : PlaceTerms.entry) -> e.term) l))
       (PlaceTerms.clusters entries));
  (check (option string))
    "rewrite several spellings" (Some "Saint-Malo, Saint-Malo, France")
    (PlaceRename.rewrite false
       ~terms:[ "St Malo"; "saint-malo" ]
       ~new_term:"Saint-Malo" "saint-malo, St Malo, France" [ Some 1; Some 2 ]);
  ()

let groups () =
  let istr i = Geneweb_db.Driver.Istr.of_string (string_of_int i) in
  let groups =
    PlaceGroups.of_places false
      (List.mapi
         (fun i raw -> (istr (i + 1), raw, 1))
         [
           "Avranches";
           "Avranches (Manche)";
           "Avranches, Manche";
           "Avranches, Manche, France";
           "Saint-Martin";
           "Saint-Martin, Manche, France";
           "Saint-Martin, Calvados, France";
           "[Le Bourg] - Saint-Martin, Manche, France";
           "Manche, France";
         ])
  in
  let raws l = List.map (fun (p : PlaceTerms.place) -> p.raw) l in
  (check (list (pair (list string) (list string))))
    "groups"
    [
      ( [ "Avranches, Manche, France" ],
        [ "Avranches"; "Avranches, Manche"; "Avranches (Manche)" ] );
      ([ "Saint-Martin, Calvados, France" ], [ "Saint-Martin" ]);
      ([ "Saint-Martin, Manche, France" ], [ "Saint-Martin" ]);
    ]
    (List.map
       (fun (g : PlaceGroups.group) -> (raws g.targets, raws g.members))
       groups);
  (check int) "Saint-Martin is ambiguous" 2
    (PlaceGroups.count_groups groups (istr 5));
  (check int) "Avranches is not" 1 (PlaceGroups.count_groups groups (istr 1));
  (* same terms: "aaaa (bbbb)" and "aaaa, bbbb" are variants of each other *)
  (check (list (pair (list string) (list string))))
    "variants"
    [ ([ "Avranches (Manche)"; "Avranches, Manche" ], []) ]
    (List.map
       (fun (g : PlaceGroups.group) -> (raws g.targets, raws g.members))
       (PlaceGroups.of_places false
          [
            (istr 1, "Avranches, Manche", 1); (istr 2, "Avranches (Manche)", 2);
          ]));
  ()

let v =
  [
    ("place-normalize", [ test_case "Place normalize" `Quick normalize ]);
    ( "place-normalize-place",
      [ test_case "Place normalize_place ordering" `Quick normalize_place ] );
    ( "place-fold-long",
      [ test_case "Place fold_place_long ordering" `Quick fold_place_long ] );
    ( "place-split-suburb",
      [ test_case "Place split suburb" `Quick split_suburb ] );
    ("place-only-suburb", [ test_case "Place only suburb" `Quick only_suburb ]);
    ( "place-without-suburb",
      [ test_case "Place without suburb" `Quick without_suburb ] );
    ("place-compare", [ test_case "Place compare" `Quick compare_places ]);
    ( "place-components",
      [ test_case "Place components spans" `Quick components ] );
    ("place-terms", [ test_case "PlaceTerms of_places" `Quick of_places ]);
    ("place-groups", [ test_case "PlaceGroups of_places" `Quick groups ]);
    ( "place-clusters",
      [ test_case "PlaceTerms cluster_key and clusters" `Quick clusters ] );
    ( "place-rename",
      [
        test_case "PlaceRename rewrite" `Quick rename_rewrite;
        test_case "PlaceRename occurrence keys" `Quick rename_occurrence_key;
      ] );
  ]
