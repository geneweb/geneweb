(* SourceTerms: the parsing of source strings (examples from Roglo) and the
   clusters of wizards and sources. *)

open Alcotest
open Geneweb

let raws =
  [
    "[[w:P.Colin]] (AD Vosges, RP paroisse Sainte-Croix, Saint-Di\195\169) 7 v \
     2013, [[w:P.Colin]] (AD Vosges, RP Paroisse Saint-Croix, \
     Saint-Di\195\169) 8 v 2013";
    "BdeRauglaudre, RP Stainville 1717-1788 28/403, 4xii2018";
    "BdeRauglaudre (EC Chalon-sur-Sa\195\180ne) 17v2021";
    "D.Gaultier (Le Matin du 2/10/1933) 7/8/11";
    "E.Polti ( Bottin Mondain ) 20.12.2005, [[w:J-N.Pommier]] (Archives de la \
     Corr\195\168ze) 19 iii 2012, R.de Tessieres (\195\169tat civil) 7 iii \
     2020";
    "R.de Tessieres (RP) 16 xi 2016";
    "AD 87, f\194\176 109/180";
    "J-L. Dega (geneanet jcrc2 et Bruno Muratet) 27 iv 2022";
    "G. Fa\195\191 (carnet du Figaro, du 6 septembre 2019) 27.ix.2019";
    "J. Walraet (site luchago) 28 avril 07";
    "Nicolas Soulard 10.02.2009 (AD72 vue 103)";
    "AD59 (vue 12)";
  ]

let known = SourceTerms.known_of_strings ~threshold:1 raws

let opt (sp : SourceTerms.span option) =
  Option.map (fun (sp : SourceTerms.span) -> sp.text) sp

let parsed s =
  List.map
    (fun (seg : SourceTerms.segment) ->
      (opt seg.wizard, opt seg.source, opt seg.date))
    (SourceTerms.parse known s)

let seg_t = list (triple (option string) (option string) (option string))

let test_parse () =
  let chk n expected =
    (check seg_t) (string_of_int n) expected (parsed (List.nth raws n))
  in
  chk 0
    [
      ( Some "[[w:P.Colin]]",
        Some "AD Vosges, RP paroisse Sainte-Croix, Saint-Di\195\169",
        Some "7 v 2013" );
      ( Some "[[w:P.Colin]]",
        Some "AD Vosges, RP Paroisse Saint-Croix, Saint-Di\195\169",
        Some "8 v 2013" );
    ];
  (* a wizard followed by a comma, commas inside the source *)
  chk 1
    [
      ( Some "BdeRauglaudre",
        Some "RP Stainville 1717-1788 28/403",
        Some "4xii2018" );
    ];
  chk 3 [ (Some "D.Gaultier", Some "Le Matin du 2/10/1933", Some "7/8/11") ];
  chk 4
    [
      (Some "E.Polti", Some "Bottin Mondain", Some "20.12.2005");
      ( Some "[[w:J-N.Pommier]]",
        Some "Archives de la Corr\195\168ze",
        Some "19 iii 2012" );
      (Some "R.de Tessieres", Some "\195\169tat civil", Some "7 iii 2020");
    ];
  (* no wizard: a single source, not cut at its comma *)
  chk 6 [ (None, Some "AD 87, f\194\176 109/180", None) ];
  chk 8
    [
      ( Some "G. Fa\195\191",
        Some "carnet du Figaro, du 6 septembre 2019",
        Some "27.ix.2019" );
    ];
  chk 9 [ (Some "J. Walraet", Some "site luchago", Some "28 avril 07") ];
  (* "wizard date (source)" *)
  chk 10 [ (Some "Nicolas Soulard", Some "AD72 vue 103", Some "10.02.2009") ];
  (* a digit: not taken for a wizard *)
  chk 11 [ (None, Some "AD59 (vue 12)", None) ];
  (* the spans point at the text *)
  List.iter
    (fun s ->
      List.iter
        (fun (seg : SourceTerms.segment) ->
          List.iter
            (fun (sp : SourceTerms.span) ->
              (check string) "span" sp.text (String.sub s sp.pos sp.len))
            (List.filter_map Fun.id [ seg.wizard; seg.source; seg.date ]))
        (SourceTerms.parse known s))
    raws

let test_keys () =
  let w = SourceTerms.wizard_key and s = SourceTerms.source_key in
  List.iter
    (fun (a, b) -> (check string) (a ^ " ~ " ^ b) (w a) (w b))
    [
      ("Gilles Francez", "[[w:G.Francez]]");
      ("G.Francez", "G. Francez");
      ("PRactMadoux", "P Ract Madoux");
      ("C.Denanc\195\169", "[[w: C.Denance]]");
      ("N de Meyrignac", "[[w:N.de Meyrignac]]");
      ("Beno\195\174t Ravinet", "B.Ravinet");
      ("G.Francez", " G.Francez\u{200B} ");
    ];
  (check bool) "P.Colin / Y.Colin" false (w "P.Colin" = w "Y.Colin");
  List.iter
    (fun (a, b) -> (check string) (a ^ " ~ " ^ b) (s a) (s b))
    [
      ("AD 75", "AD75");
      ("AD75", "ad-75");
      ("\195\169tat civil", "Etat-Civil");
      ("AD75", "\u{FEFF}AD 75 ");
    ]

let test_clusters () =
  let strings =
    List.mapi
      (fun i (raw, uses) ->
        (Geneweb_db.Driver.Istr.of_string (string_of_int i), raw, uses))
      [
        ("Gilles Francez (AD 75) 1 i 2020", 2);
        ("[[w:G.Francez]] (AD75) 2 ii 2021", 1);
        ("G.Francez (ad-75) 3 iii 2022", 1);
      ]
  in
  let d =
    {
      SourceTerms.known =
        SourceTerms.known_of_strings ~threshold:1
          (List.map (fun (_, s, _) -> s) strings);
      strings;
    }
  in
  let show part =
    List.map
      (fun (c : SourceTerms.cluster) ->
        ( c.ckey,
          List.map
            (fun (sp : SourceTerms.spelling) -> (sp.text, sp.uses))
            c.spellings ))
      (SourceTerms.clusters d part)
  in
  let t = list (pair string (list (pair string int))) in
  (check t) "wizards"
    [
      ( "gfrancez",
        [ ("Gilles Francez", 2); ("G.Francez", 1); ("[[w:G.Francez]]", 1) ] );
    ]
    (show SourceTerms.Wizard);
  (check t) "sources"
    [ ("ad75", [ ("AD 75", 2); ("AD75", 1); ("ad-75", 1) ]) ]
    (show SourceTerms.Source)

let v =
  [
    ( "source-terms",
      [
        test_case "parse" `Quick test_parse;
        test_case "keys" `Quick test_keys;
        test_case "clusters" `Quick test_clusters;
      ] );
  ]
