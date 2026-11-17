open Alcotest
open Geneweb

let pages () =
  let conf = { Config.empty with base_env = [ ("book_max_results", "10") ] } in
  let pages l =
    UpdateData.book_pages conf (List.map (fun (g, n) -> (g, n, [])) l)
    |> List.map (List.map (fun (g, _, _) -> g))
  in
  (check (list (list string)))
    "groups share a page under the maximum"
    [ [ "a"; "b" ]; [ "c"; "d" ] ]
    (pages [ ("a", 3); ("b", 4); ("c", 3); ("d", 1) ]);
  (check (list (list string)))
    "a big group is a page of its own"
    [ [ "B" ]; [ "a"; "c" ] ]
    (pages [ ("a", 3); ("B", 20); ("c", 2) ])

let v = [ ("book", [ test_case "pages" `Quick pages ]) ]
