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
    ( [
        WLpage (13, ([], "aaa"), "aaa", "", "bbb");
        WLnone (15, ", ");
        WLperson (26, ("ccc", "ddd", 0), None, None);
        WLnone (51, ", http://site.com/eee#fff");
      ],
      "[[[aaa/bbb]]], [[ccc/ddd]], http://site.com/eee#fff" );
    ([ WLnone (6, "[[[]]]") ], "[[[]]]");
    ([ WLnone (4, "[[]]") ], "[[]]");
    ([ WLnone (3, "[[w") ], "[[w");
    ( [ WLpage (34, ([], "d_azincourt"), "d_azincourt", "", "d&#039;Azincourt") ],
      "[[[d_azincourt/d&#039;Azincourt]]]" );
    (*
    TODO tests suppressed pending fix in wiki.ml fir wiki_syntax
    following Keryan's modification for automatic pnoc update
    ( [
        WLnone (1, "[");
        WLperson (12, ("aaa", "bbb", 0), None, None);
        WLnone (14, ", ");
        WLperson (25, ("ccc", "ddd", 0), Some "Ccc Ddd", None);
        WLnone (50, ", http://site.com/eee#fff");
      ],
      "[[[aaa/bbb]], [[ccc/ddd/Ccc Ddd]], http://site.com/eee#fff" );
    ([ WLnone (7, "[[[aaa/") ], "[[[aaa/");
    ([ WLnone (4, "[[[w") ], "[[[w");
    *)
  ]

(* todo fix Fmt *)
let testable_wiki = testable Fmt.nop ( = )

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
