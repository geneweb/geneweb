(*
let pp_wiki_link = function
  | WLpage (a, b, c, d, e) ->
      "WLpage "
      ^ [%show: int * (string list * string) * string * string * string]
          (a, b, c, d, e)
  | WLperson (a, b, c, d) ->
      "WLperson"
      ^ [%show: int * (string * string * int) * string option * string option]
          (a, Obj.magic b, c, d)
  | WLwizard (a, b, c) -> "WLwizard" ^ [%show: int * string * string] (a, b, c)
  | WLnone (a, b)-> "WLnone" ^ [%show: int * string] (a, b)
  *)

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

(* todo fix Fmt *)
let testable_wiki =
  testable
    (fun fmt x ->
      match x with
      | WLpage (pos, _b, fname, anchor, text) ->
          Fmt.pf fmt "WLpage   (%d,_,%S,%S,%S)" pos fname anchor text
      | WLperson (pos, key, name, text) ->
          let fn, sn, oc = key in
          Fmt.pf fmt "WLperson (%d, (%S,%S,%d),%a,%a)" pos fn sn oc
            (Fmt.option Fmt.string) name (Fmt.option Fmt.string) text
      | WLwizard (pos, wiz, name) ->
          Fmt.pf fmt "WLwizard (%d,%S,%S)" pos wiz name
      | WLnone (pos, s) -> Fmt.pf fmt "WLnone   (%d,%S)" pos s)
    ( = )

let test expected s () =
  (check (list testable_wiki)) "" expected (f s);
  ()

let v =
  [
    ( "misc-notes-link",
      (* todo List.map here or in test? *)
      List.map
        (fun (expected, s) -> test_case "Wiki links" `Quick (test expected s))
        l );
  ]
