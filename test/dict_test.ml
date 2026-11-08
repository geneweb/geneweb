open Alcotest
open Def
module Dict = Geneweb_db.Dict

let particles = Mutil.compile_particles [ "de_"; "d'"; "du_"; "van_der_" ]
let key = Dict.key particles

let names () =
  List.iter
    (fun d ->
      let s = Dict.to_string d in
      (check (option string))
        s (Some s)
        (Option.map Dict.to_string (Dict.of_string s)))
    Dict.all;
  (check bool) "unknown" true (Dict.of_string "x" = None)

let surname_order () =
  let l =
    [
      "de La Tour";
      "La Tour";
      "Dupont";
      "d'Arc";
      "Arc";
      "du Bois";
      "Bois";
      "La";
      "van der Berg";
      "Berg";
      "Arcy";
      "de Arcy";
    ]
  in
  (check (list string))
    "order of compare_after_particle"
    (List.sort (Mutil.compare_after_particle particles) l)
    (List.sort
       (fun a b -> String.compare (key Dict.Surname a) (key Dict.Surname b))
       l);
  (check string) "surname" "La Tour\000de " (key Dict.Surname "de La Tour");
  (check string) "domain" "Arc\000d'" (key Dict.Domain "d'Arc");
  (check string) "first name" "de La Tour" (key Dict.First_name "de La Tour")

let place_keys () =
  let l =
    [
      "Paris, France";
      "[Montmartre] - Paris";
      "Paris";
      "[Belleville] – Paris";
      "Pari";
    ]
  in
  List.iter
    (fun s ->
      (check bool) ("prefix " ^ s) true
        (String.starts_with ~prefix:(Dict.without_suburb s) (key Dict.Place s)))
    l;
  (check (list string))
    "place before its suburbs"
    [
      "Pari";
      "Paris";
      "[Belleville] – Paris";
      "[Montmartre] - Paris";
      "Paris, France";
    ]
    (List.sort
       (fun a b -> String.compare (key Dict.Place a) (key Dict.Place b))
       l)

let suburb () =
  let bounds = check (option (pair int int)) in
  bounds "hyphen" (Some (3, 7)) (Dict.suburb "[ab] - c");
  bounds "em dash" (Some (3, 9)) (Dict.suburb "[ab] — c");
  bounds "no dash" None (Dict.suburb "[ab] c");
  bounds "truncated dash" None (Dict.suburb "[ab] \xE2");
  (check string) "without" "c" (Dict.without_suburb "[ab] – c")

let fields () =
  let p =
    {
      (Mutil.empty_person 0 0) with
      birth_place = 1;
      death_place = 2;
      occupation = 3;
      titles =
        [
          {
            t_name = Tnone;
            t_ident = 4;
            t_place = 5;
            t_date_start = Date.cdate_None;
            t_date_end = Date.cdate_None;
            t_nth = 0;
          };
        ];
    }
  in
  let f = { (Mutil.empty_family 0) with marriage_place = 6; fsources = 7 } in
  let sorted l = List.sort_uniq Int.compare l in
  let strings d = sorted (Dict.person_strings d p) in
  (check (list int)) "places" [ 0; 1; 2 ] (strings Dict.Place);
  (check (list int)) "occupation" [ 3 ] (strings Dict.Occupation);
  (check (list int)) "title" [ 4 ] (strings Dict.Title);
  (check (list int)) "domain" [ 5 ] (strings Dict.Domain);
  (check (list int))
    "family places" [ 6 ]
    (sorted (Dict.family_strings Dict.Place f));
  (check (list int))
    "family sources" [ 0; 7 ]
    (sorted (Dict.family_strings Dict.Source f));
  (check (list int))
    "no family occupation" []
    (Dict.family_strings Dict.Occupation f)

let v =
  [
    ( "dict",
      [
        test_case "Dict names" `Quick names;
        test_case "Dict surname order" `Quick surname_order;
        test_case "Dict place keys" `Quick place_keys;
        test_case "Dict suburb" `Quick suburb;
        test_case "Dict fields" `Quick fields;
      ] );
  ]
