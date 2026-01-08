open Geneweb.NotesLinks

let f s =
  let len = String.length s in
  let rec loop acc i =
    if i = len then List.rev acc
    else
      match misc_notes_link s i with
      | (WLpage (j, _, _, _, _) | WLperson (j, _, _, _, _) | WLwizard (j, _, _))
        as x ->
          loop (x :: acc) j
      | WLnone (j, _text) as wn -> loop (wn :: acc) j
  in
  loop [] 0

let expect_failure name speed f =
  Alcotest.test_case name speed (fun () ->
      try
        f ();
        Alcotest.fail "Expected failure"
      with _ -> ())

let l =
  [
    ([ WLpage (13, ([], "aaa"), "aaa", "", "bbb") ], "[[[aaa/bbb]]]");
    ( [ WLperson (11, ("ccc", "ddd", 0), Some "ccc ddd", None, None) ],
      "[[ccc/ddd]]" );
    ( [ WLperson (17, ("ccc", "ddd", 0), Some "Texte", None, None) ],
      "[[ccc/ddd/Texte]]" );
    ( [ WLperson (21, ("ccc", "ddd", 1), Some "Ccc Ddd", None, None) ],
      "[[ccc/ddd/1/Ccc Ddd]]" );
    ( [ WLperson (25, ("ccc", "ddd", 0), Some "Texte", Some "Texte 2", None) ],
      "[[ccc/ddd/Texte;Texte 2]]" );
    ( [ WLperson (13, ("ccc", "ddd", 0), Some "ccc ddd", None, Some 1) ],
      "[[ccc/ddd]]#1" );
    ( [ WLperson (14, ("ccc", "ddd", 0), Some "ccc ddd", None, Some 42) ],
      "[[ccc/ddd]]#42" );
    ( [ WLperson (17, ("ccc", "ddd", 0), Some "ccc ddd", Some "txt", Some 5) ],
      "[[ccc/ddd;txt]]#5" );
    ([ WLnone (6, "[[[]]]") ], "[[[]]]");
    ([ WLnone (4, "[[]]") ], "[[]]");
    ([ WLnone (3, "[[w") ], "[[w");
    ( [ WLpage (34, ([], "d_azincourt"), "d_azincourt", "", "d&#039;Azincourt") ],
      "[[[d_azincourt/d&#039;Azincourt]]]" );
    ( [
        WLnone (1, "[");
        WLperson (12, ("aaa", "bbb", 0), Some "aaa bbb", None, None);
        WLnone (14, ", ");
        WLperson (33, ("ccc", "ddd", 0), Some "Ccc Ddd", None, None);
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
  | WLpage (pos, (p, fname), full, anchor, text) ->
      Fmt.pf ppf "WLpage (%d, ([%s], %s), %s, %s, %s)" pos
        (String.concat "; " p) fname full anchor text
  | WLperson (pos, (fn, sn, oc), name, text, fam_marker) ->
      Fmt.pf ppf "WLperson (%d, (%s, %s, %d), %a, %a, %a)" pos fn sn oc
        Fmt.(option ~none:(any "None") string)
        name
        Fmt.(option ~none:(any "None") string)
        text
        Fmt.(option ~none:(any "None") int)
        fam_marker
  | WLwizard (pos, wiz, name) -> Fmt.pf ppf "WLwizard (%d, %s, %s)" pos wiz name
  | WLnone (pos, s) -> Fmt.pf ppf "WLnone (%d, %s)" pos s

let eq_token = ( = )
let token_t = Alcotest.testable pp_token eq_token

let tests s (expected, input) () =
  Alcotest.(check (list token_t)) s expected (f input)

let test_cases =
  List.mapi
    (fun i (exp, inp) ->
      Alcotest.test_case
        (Printf.sprintf "case %d: %s" i inp)
        `Quick
        (tests inp (exp, inp)))
    l

let v = [ ("wiki", test_cases) ]
