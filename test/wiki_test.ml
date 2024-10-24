let f s =
  let len = String.length s in
  let rec loop acc i =
    if i = len then List.rev acc
    else
      match Geneweb.NotesLinks.misc_notes_link s i with
      | ( Geneweb.NotesLinks.WLpage (j, _, _, _, _)
        | Geneweb.NotesLinks.WLperson (j, _, _, _)
        | Geneweb.NotesLinks.WLwizard (j, _, _) ) as x ->
          loop (x :: acc) j
      | Geneweb.NotesLinks.WLnone -> (
          match acc with
          | [] -> loop (Geneweb.NotesLinks.WLnone :: acc) (i + 1)
          | hd :: _ ->
              if hd <> Geneweb.NotesLinks.WLnone then
                loop (Geneweb.NotesLinks.WLnone :: acc) (i + 1)
              else loop acc (i + 1))
  in
  loop [] 0

let l =
  [
    ( [
        Geneweb.NotesLinks.WLpage (13, ([], "aaa"), "aaa", "", "bbb");
        Geneweb.NotesLinks.WLnone;
        Geneweb.NotesLinks.WLperson (26, ("ccc", "ddd", 0), "ccc ddd", None);
        Geneweb.NotesLinks.WLnone;
      ],
      "[[[aaa/bbb]]], [[ccc/ddd]], http://site.com/eee#fff" );
    ( [
        Geneweb.NotesLinks.WLnone;
        Geneweb.NotesLinks.WLperson (12, ("aaa", "bbb", 0), "aaa bbb", None);
        Geneweb.NotesLinks.WLnone;
        Geneweb.NotesLinks.WLperson (25, ("ccc", "ddd", 0), "ccc ddd", None);
        Geneweb.NotesLinks.WLnone;
      ],
      "[[[aaa/bbb]], [[ccc/ddd]], http://site.com/eee#fff" );
    ([ Geneweb.NotesLinks.WLnone ], "[[[aaa/");
    ([ Geneweb.NotesLinks.WLnone ], "[[[]]]");
    ([ Geneweb.NotesLinks.WLnone ], "[[[w");
    ([ Geneweb.NotesLinks.WLnone ], "[[]]");
    ([ Geneweb.NotesLinks.WLnone ], "[[w");
    ( [
        Geneweb.NotesLinks.WLpage
          (34, ([], "d_azincourt"), "d_azincourt", "", "d&#039;Azincourt");
      ],
      "[[[d_azincourt/d&#039;Azincourt]]]" );
  ]

(* todo fix Fmt *)
let testable_wiki = Alcotest.testable Fmt.nop ( = )

let test expected s () =
  (Alcotest.check (Alcotest.list testable_wiki)) "" expected (f s);
  ()

let v =
  [
    ( "misc-notes-link",
      (* todo List.map here or in test? *)
      List.map
        (fun (expected, s) ->
          Alcotest.test_case "Wiki links" `Quick (test expected s))
        l );
  ]
