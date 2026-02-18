module A = Alcotest
module Q = QCheck

module H = Geneweb_search.Heap.Make (struct
  type t = int

  let dummy = 0
  let compare = Int.compare
end)

let heap_sort l =
  let hp = H.create @@ List.length l in
  List.iter (H.insert hp) l;
  let rec loop acc =
    match H.delete_min hp with
    | exception H.Empty -> List.rev acc
    | v -> loop (v :: acc)
  in
  loop []

let heap_sort_test =
  let gen = Q.Gen.(list_size nat int) in
  let qtst =
    Q.Test.make ~count:1000 ~name:"random heap sort" (Q.make gen) @@ fun l ->
    let l1 = heap_sort l in
    let l2 = List.sort Int.compare l in
    Geneweb_compat.List.equal Int.equal l1 l2
  in
  QCheck_alcotest.to_alcotest qtst

let () = A.run __FILE__ [ ("heap tests", [ heap_sort_test ]) ]
