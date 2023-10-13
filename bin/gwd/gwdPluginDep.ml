(* https://github.com/dmbaturin/ocaml-tsort *)
(* Authors: Daniil Baturin (2019), Martin Jambon (2020). *)
(* See LICENSE at the end of the file *)

(* Adapted to GeneWeb by Julien Sagot *)

type 'a sort_result = Sorted of 'a list | ErrorCycle of 'a list

(* Finds "isolated" nodes,
   that is, nodes that have no dependencies *)
let find_isolated_nodes hash =
  let aux id deps acc = match deps with [] -> id :: acc | _ -> acc in
  Hashtbl.fold aux hash []

(* Takes a node name list and removes all those nodes from a hash *)
let remove_nodes nodes hash = List.iter (Hashtbl.remove hash) nodes

(* Walks through a node:dependencies hash and removes a dependency
   from all nodes that have it in their dependency lists *)
let remove_dependency hash dep =
  let aux dep hash id =
    let deps = Hashtbl.find hash id in
    let deps = List.filter (( <> ) dep) deps in
    Hashtbl.remove hash id;
    Hashtbl.add hash id deps
  in
  let ids = Hashtbl.fold (fun k _ a -> k :: a) hash [] in
  List.iter (aux dep hash) ids

(* Deduplicate list items. *)
let deduplicate l =
  let tbl = Hashtbl.create (List.length l) in
  List.fold_left
    (fun acc x ->
      if Hashtbl.mem tbl x then acc
      else (
        Hashtbl.add tbl x ();
        x :: acc))
    [] l
  |> List.rev

(*
   Append missing nodes to the graph, in the order in which they were
   encountered. This particular order doesn't have to be guaranteed by the
   API but seems nice to have.
*)
let add_missing_nodes graph_l graph =
  let missing =
    List.fold_left
      (fun acc (_, vl) ->
        List.fold_left
          (fun acc v ->
            if not (Hashtbl.mem graph v) then (v, []) :: acc else acc)
          acc vl)
      [] graph_l
    |> List.rev
  in
  List.iter (fun (v, vl) -> Hashtbl.replace graph v vl) missing;
  graph_l @ missing

(* The Kahn's algorithm:
    1. Find nodes that have no dependencies ("isolated") and remove them from
       the graph hash.
       Add them to the initial sorted nodes list and the list of isolated
       nodes for the first sorting pass.
    2. For every isolated node, walk through the remaining nodes and
       remove it from their dependency list.
       Nodes that only depended on it now have empty dependency lists.
    3. Find all nodes with empty dependency lists and append them to the sorted
       nodes list _and_ the list of isolated nodes to use for the next step
    4. Repeat until the list of isolated nodes is empty
    5. If the graph hash is still not empty, it means there is a cycle.
*)
let sort nodes =
  let rec sorting_loop deps hash acc =
    match deps with
    | [] -> acc
    | dep :: deps ->
        let () = remove_dependency hash dep in
        let isolated_nodes = find_isolated_nodes hash in
        let () = remove_nodes isolated_nodes hash in
        sorting_loop
          (List.append deps isolated_nodes)
          hash
          (List.append acc isolated_nodes)
  in
  let nodes_hash =
    let tbl = Hashtbl.create 32 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) nodes;
    tbl
  in
  let _nodes = add_missing_nodes nodes nodes_hash in
  let base_nodes = find_isolated_nodes nodes_hash in
  let () = remove_nodes base_nodes nodes_hash in
  let sorted_node_ids = sorting_loop base_nodes nodes_hash [] in
  let sorted_node_ids = List.append base_nodes sorted_node_ids in
  let remaining_ids = Hashtbl.fold (fun k _ a -> k :: a) nodes_hash [] in
  match remaining_ids with
  | [] -> Sorted sorted_node_ids
  | _ -> ErrorCycle remaining_ids

(* MIT License *)

(* Copyright (c) 2019 Daniil Baturin *)

(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)

(* The above copyright notice and this permission notice shall be included in all *)
(* copies or substantial portions of the Software. *)

(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE *)
(* SOFTWARE. *)
