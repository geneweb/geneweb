open Alcotest
module D = Geneweb.Dag2html

(* === DAG builders for fixtures === *)

let mk_dag arr : int D.dag = { D.dag = arr }

let mk_node ~pare ~chil v =
  {
    D.pare = List.map D.idag_of_int pare;
    valu = v;
    chil = List.map D.idag_of_int chil;
  }

let dag_empty () : int D.dag = mk_dag [||]
let dag_single () = mk_dag [| mk_node ~pare:[] ~chil:[] 0 |]

let dag_isolated_pair () =
  mk_dag [| mk_node ~pare:[] ~chil:[] 0; mk_node ~pare:[] ~chil:[] 1 |]

let dag_linear_chain () =
  (* 0 -> 1 -> 2 *)
  mk_dag
    [|
      mk_node ~pare:[] ~chil:[ 1 ] 0;
      mk_node ~pare:[ 0 ] ~chil:[ 2 ] 1;
      mk_node ~pare:[ 1 ] ~chil:[] 2;
    |]

let dag_diamond () =
  (* 0 -> {1, 2} -> 3 *)
  mk_dag
    [|
      mk_node ~pare:[] ~chil:[ 1; 2 ] 0;
      mk_node ~pare:[ 0 ] ~chil:[ 3 ] 1;
      mk_node ~pare:[ 0 ] ~chil:[ 3 ] 2;
      mk_node ~pare:[ 1; 2 ] ~chil:[] 3;
    |]

(* === Output inspection helpers === *)

let no_phony _ = false
let all_phony _ = true

let elem_ids_of_table t =
  let acc = ref [] in
  Array.iter
    (fun row ->
      Array.iter
        (fun cell ->
          match cell.D.elem with
          | D.Elem id -> acc := D.int_of_idag id :: !acc
          | D.Ghost _ | D.Nothing -> ())
        row)
    t.D.table;
  List.sort_uniq Int.compare !acc

let same_kind e1 e2 =
  match (e1, e2) with
  | D.Elem a, D.Elem b -> D.int_of_idag a = D.int_of_idag b
  | D.Ghost _, D.Ghost _ -> true
  | D.Nothing, D.Nothing -> true
  | _ -> false

let layout d = D.table_of_dag no_phony false false false d

(* === Tests === *)

let test_idag_roundtrip () =
  for i = 0 to 100 do
    (check int) "round-trip" i (D.int_of_idag (D.idag_of_int i))
  done

let test_empty_dag () =
  let t = layout (dag_empty ()) in
  (check int) "empty input yields empty table" 0 (Array.length t.D.table);
  (check (list int)) "no Elem cells" [] (elem_ids_of_table t)

let test_single_node () =
  let t = layout (dag_single ()) in
  (check bool) "at least one row" true (Array.length t.D.table >= 1);
  (check (list int)) "single node preserved" [ 0 ] (elem_ids_of_table t)

let test_isolated_pair () =
  let t = layout (dag_isolated_pair ()) in
  (check (list int)) "both nodes preserved" [ 0; 1 ] (elem_ids_of_table t)

let test_linear_chain () =
  let t = layout (dag_linear_chain ()) in
  (check (list int)) "all 3 nodes preserved" [ 0; 1; 2 ] (elem_ids_of_table t);
  (check bool) "chain spans at least 3 rows" true (Array.length t.D.table >= 3)

let test_diamond () =
  let t = layout (dag_diamond ()) in
  (check (list int))
    "all 4 nodes preserved" [ 0; 1; 2; 3 ] (elem_ids_of_table t);
  (check bool) "diamond spans at least 3 rows" true (Array.length t.D.table >= 3)

let test_determinism () =
  let t1 = layout (dag_diamond ()) in
  let t2 = layout (dag_diamond ()) in
  let rows1 = Array.length t1.D.table in
  let rows2 = Array.length t2.D.table in
  (check int) "same row count" rows1 rows2;
  let n = min rows1 rows2 in
  for i = 0 to n - 1 do
    let r1 = t1.D.table.(i) in
    let r2 = t2.D.table.(i) in
    (check int)
      (Printf.sprintf "row %d width" i)
      (Array.length r1) (Array.length r2);
    let m = min (Array.length r1) (Array.length r2) in
    for j = 0 to m - 1 do
      (check bool)
        (Printf.sprintf "cell (%d,%d) same kind" i j)
        true
        (same_kind r1.(j).D.elem r2.(j).D.elem)
    done
  done

let check_same_elem_set label d =
  let normal = D.table_of_dag no_phony false false false d in
  let variants =
    [
      ("no_optim", D.table_of_dag no_phony true false false d);
      ("invert", D.table_of_dag no_phony false true false d);
      ("no_group", D.table_of_dag no_phony false false true d);
      ("all_phony", D.table_of_dag all_phony false false false d);
    ]
  in
  let expected = elem_ids_of_table normal in
  List.iter
    (fun (name, t) ->
      (check (list int))
        (Printf.sprintf "%s: %s preserves Elem set" label name)
        expected (elem_ids_of_table t))
    variants

let test_boolean_knobs_diamond () =
  check_same_elem_set "diamond" (dag_diamond ())

let test_boolean_knobs_chain () =
  check_same_elem_set "chain" (dag_linear_chain ())

let v =
  [
    ("dag2html-idag", [ test_case "idag round-trip" `Quick test_idag_roundtrip ]);
    ("dag2html-empty", [ test_case "empty DAG" `Quick test_empty_dag ]);
    ("dag2html-single", [ test_case "single node" `Quick test_single_node ]);
    ("dag2html-pair", [ test_case "isolated pair" `Quick test_isolated_pair ]);
    ("dag2html-chain", [ test_case "linear chain" `Quick test_linear_chain ]);
    ("dag2html-diamond", [ test_case "diamond" `Quick test_diamond ]);
    ( "dag2html-determinism",
      [ test_case "deterministic shape" `Quick test_determinism ] );
    ( "dag2html-knobs",
      [
        test_case "boolean knobs preserve nodes (diamond)" `Quick
          test_boolean_knobs_diamond;
        test_case "boolean knobs preserve nodes (chain)" `Quick
          test_boolean_knobs_chain;
      ] );
  ]
