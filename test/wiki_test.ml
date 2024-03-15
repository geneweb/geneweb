(*
let pp_wiki_link = function
  | WLpage (a, b, c, d, e) ->
      "WLpage "
      ^ [%show: int * (string list * string) * string * string * string]
          (a, b, c, d, e)
  | WLperson (a, b, c, d) ->
      "WLperson"
      ^ [%show: int * (string * string * int) * string * string option]
          (a, Obj.magic b, c, d)
  | WLwizard (a, b, c) -> "WLwizard" ^ [%show: int * string * string] (a, b, c)
  | WLnone -> "WLnone"
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
      | WLnone -> (
          match acc with
          | [] -> loop (WLnone :: acc) (i + 1)
          | hd :: _ ->
              if hd <> WLnone then loop (WLnone :: acc) (i + 1)
              else loop acc (i + 1))
  in
  loop [] 0

let l =
  [
    ( [
        WLpage (13, ([], "aaa"), "aaa", "", "bbb");
        WLnone;
        WLperson (26, ("ccc", "ddd", 0), "ccc ddd", None);
        WLnone;
      ],
      "[[[aaa/bbb]]], [[ccc/ddd]], http://site.com/eee#fff" );
    ( [
        WLnone;
        WLperson (12, ("aaa", "bbb", 0), "aaa bbb", None);
        WLnone;
        WLperson (25, ("ccc", "ddd", 0), "ccc ddd", None);
        WLnone;
      ],
      "[[[aaa/bbb]], [[ccc/ddd]], http://site.com/eee#fff" );
    ([ WLnone ], "[[[aaa/");
    ([ WLnone ], "[[[]]]");
    ([ WLnone ], "[[[w");
    ([ WLnone ], "[[]]");
    ([ WLnone ], "[[w");
    ( [ WLpage (34, ([], "d_azincourt"), "d_azincourt", "", "d&#039;Azincourt") ],
      "[[[d_azincourt/d&#039;Azincourt]]]" );
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
