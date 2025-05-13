open Alcotest
open Geneweb
open NotesLinks

let f s =
  let len = String.length s in
  let rec loop acc i =
    if i = len then List.rev acc
    else
      match misc_notes_link s i with
      | (WLpage (j, _, _, _, _) | WLperson (j, _, _, _) | WLwizard (j, _, _)) as
        x ->
          loop (x :: acc) j
      | WLnone (j, _text) as wn -> loop (wn :: acc) j
  in
  loop [] 0

let expect_failure name speed f =
  Alcotest.test_case name speed (fun () ->
      try
        f ();
        Alcotest.fail "Expected this test to fail, but it passed"
      with _ -> ())

let l =
  [
    ([ WLpage (13, ([], "aaa"), "aaa", "", "bbb") ], "[[[aaa/bbb]]]");
    ([ WLperson (11, ("ccc", "ddd", 0), Some "ccc ddd", None) ], "[[ccc/ddd]]");
    ( [ WLperson (17, ("ccc", "ddd", 0), Some "Texte", None) ],
      "[[ccc/ddd/Texte]]" );
    ( [ WLperson (21, ("ccc", "ddd", 1), Some "Ccc Ddd", None) ],
      "[[ccc/ddd/1/Ccc Ddd]]" );
    ( [ WLperson (25, ("ccc", "ddd", 0), Some "Texte", Some "Texte 2") ],
      "[[ccc/ddd/Texte;Texte 2]]" );
    ([ WLnone (6, "[[[]]]") ], "[[[]]]");
    ([ WLnone (4, "[[]]") ], "[[]]");
    ([ WLnone (3, "[[w") ], "[[w");
    ( [ WLpage (34, ([], "d_azincourt"), "d_azincourt", "", "d&#039;Azincourt") ],
      "[[[d_azincourt/d&#039;Azincourt]]]" );
    ( [
        WLnone (1, "[");
        WLperson (12, ("aaa", "bbb", 0), Some "aaa bbb", None);
        WLnone (14, ", ");
        WLperson (33, ("ccc", "ddd", 0), Some "Ccc Ddd", None);
        WLnone (58, ", http://site.com/eee#fff");
      ],
      "[[[aaa/bbb]], [[ccc/ddd/Ccc Ddd]], http://site.com/eee#fff" );
    ([ WLnone (1, "["); WLnone (7, "[[aaa/") ], "[[[aaa/");
    ([ WLnone (1, "["); WLnone (4, "[[w") ], "[[[w");
    ([ WLwizard (8, "hg", "") ], "[[w:hg]]");
    ([ WLwizard (14, "hg", "henri") ], "[[w:hg/henri]]");
    ( [ WLnone (1, "["); WLnone (2, "["); WLwizard (16, "hg", "henri") ],
      "[[[[w:hg/henri]]" );
  ]

let pp_token ppf tk =
  match tk with
  | WLpage (pos, p, fname, anchor, text) ->
      Fmt.pf ppf "WLpage (%d, %a, %s, %s, %s)" pos
        Fmt.(
          parens @@ pair ~sep:comma (brackets @@ list ~sep:semi string) string)
        p fname anchor text
  | WLperson (pos, (fn, sn, oc), name, text) ->
      Fmt.pf ppf "WLperson (%d, (%s, %s, %d), %a, %a)" pos fn sn oc
        Fmt.(option string)
        name
        Fmt.(option string)
        text
  | WLwizard (pos, wiz, name) -> Fmt.pf ppf "WLwizard (%d, %s, %s)" pos wiz name
  | WLnone (pos, s) -> Fmt.pf ppf "WLnone (%d, %s)" pos s

let testable_wiki = testable pp_token ( = )

let test expected s () =
  (check (list testable_wiki)) "" expected (f s);
  ()

let bold_italic_syntax _ =
  let test a r = (check string) a r (Wiki.bold_italic_syntax a) in
  test "" "";
  test "abc ''def'' ghi" "abc <i>def</i> ghi";
  test "abc '''def''' ghi" "abc <b>def</b> ghi";
  test "abc '''''def''''' ghi" "abc <b><i>def</i></b> ghi"

let v =
  [
    ( "misc-notes-link",
      (* todo List.map here or in test? *)
      List.map
        (fun (expected, s) -> test_case "Wiki links" `Quick (test expected s))
        l );
    ( "bold-italic-syntax",
      [ test_case "Wiki.bold_italic_syntax" `Quick bold_italic_syntax ] );
  ]
