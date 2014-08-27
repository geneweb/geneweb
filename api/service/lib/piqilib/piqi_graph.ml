(*
   Copyright 2009, 2010, 2011 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


(*
 * This module implements various operations on graphs
 *)


let default_visitor _ = ()


(* depth-first graph traversal *)
let dfs
    ?(pre_visit = default_visitor)
    ?(cycle_visit = default_visitor)
    ?(post_visit = default_visitor)
    (nodes: 'a list)
    (get_adjacent_vertixes: ('a -> 'a list)) =
  let black = ref [] in (* visited nodes, i.e. after last_visit *)
  let grey = ref [] in (* nodes between first_visit and last_visit *)
  let set_color node = function
    | `black -> black := node::!black
    | `grey -> grey := node::!grey
  in
  let get_color node =
    if List.memq node !black
    then `black
    else if List.memq node !grey
    then `grey
    else `white
  in
  let rec aux node =
    match get_color node with
      | `black -> () (* already processed -- nothing to do *)
      | `grey -> (* found a cycle -- run a handler and return *)
          cycle_visit node
      | `white ->
          set_color node `grey;
          pre_visit node; (* run a pre-visit handler *)

          List.iter aux (get_adjacent_vertixes node);

          set_color node `black;
          post_visit node (* run a post-visit handler *)
  in
  List.iter aux nodes


(* topological sort of a graph *)
let tsort
    ?(cycle_visit = (fun _ -> failwith "found a cycle!"))
    (nodes: 'a list)
    (get_adjacent_vertixes: ('a -> 'a list)) : 'a list =
  let stack = ref [] in
  let post_visit node =
    stack := node::!stack
  in
  dfs nodes get_adjacent_vertixes ~post_visit ~cycle_visit;
  List.rev !stack

