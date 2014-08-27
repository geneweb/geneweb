(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
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


open Piqi_common 


let _ = Piqilib.init ()


(* old method for pretty-printing:
let prettyprint_ast ast =
  let code = Piq_gen.print_ast ast in
  Iolist.to_channel ch code
*)


let prettyprint_ast ch ast =
  Piq_gen.to_channel ch ast


let rec prettyprint_list ch ast_list =
  let rec aux = function
    | [] -> ()
    | [x] -> prettyprint_ast ch x
    | h::t ->
          prettyprint_ast ch h;
          output_char ch '\n';
          aux t
  in aux ast_list


let prettyprint_piqi_ast ch ast =
  match ast with
    | `list l -> prettyprint_list ch l
    | _ -> assert false


let transform_ast path f (ast:T.ast) =
  let rec aux p = function
    | `list l when p = [] -> (* leaf node *)
        (* f replaces, removes element, or splices elements of the list *)
        let res = flatmap f l in
        `list res
    | x when p = [] -> (* leaf node *)
        (* expecting f to replace the existing value, no other modifications
         * such as removal or splicing is allowed in this context *)
        (match f x with [res] -> res | _ -> assert false)
    | `list l ->
        (* haven't reached the leaf node => continue tree traversal *)
        let res = List.map (aux p) l in
        `list res
    | `named {T.Named.name = n; T.Named.value = v} when List.hd p = n ->
        (* found path element => continue tree traversal *)
        let res = T.Named#{name = n; value = aux (List.tl p) v} in
        `named res
    | x -> x
  in
  aux path ast


(* simplify piqi ast: *)
let simplify_piqi_ast (ast:T.ast) =
  let tr = transform_ast in
  (* map piqdef.x -> x *)
  let rm_piqdef =
    tr [] (
      function
        | `named {T.Named.name = "piqdef"; T.Named.value = v} -> [v]
        | x -> [x]
    )
  (* del .../mode.required *)
  (* map .../field/mode x -> x *)
  and tr_field_mode path =
    tr path (
      function
        | `named {T.Named.name = "mode"; T.Named.value = `name "required"} -> []
        | `named {T.Named.name = "mode"; T.Named.value = (`name _) as x} -> [x]
        | x -> [x]
    )
  (* map extend/.piq-any x -> x *)
  and tr_extend_piq_any =
    tr ["extend"] (
      function
        | `named {T.Named.name = "piq-any"; T.Named.value = v} -> [v]
        | x -> [x]
    )
  (* .../type.name x -> type.x *)
  and tr_type_name_common path =
    tr path (
      function
        | `named ({T.Named.name = "type"; T.Named.value = `named {T.Named.name = "name"; T.Named.value = v}} as x) ->
            let res = `named T.Named#{x with value = v} in
            [res]
        | x -> [x]
    )
  (* map ../anonymous-record.x -> x *)
  (* map ../name.x -> x *)
  and rm_param_extra path =
    tr path (
      function
        | `named {T.Named.name = "record"; T.Named.value = v} -> [v]
        | `named {T.Named.name = "name"; T.Named.value = v} -> [v]
        | x -> [x]
    )
  in
  let (|>) a f = f a in
  let simplify_function_param param ast =
    ast
    |> rm_param_extra ["function"; param]
    |> tr_field_mode ["function"; param; "field"]
    |> tr_type_name_common ["function"; param; "field"]
    |> tr_type_name_common ["function"; param; "variant"; "option"]
    |> tr_type_name_common ["function"; param; "enum"; "option"]
    |> tr_type_name_common ["function"; param; "list"]
  in
  ast
  |> rm_piqdef
  |> tr_field_mode ["record"; "field"]
  |> tr_extend_piq_any
  (* .../type.name x -> type.x *)
  |> tr_type_name_common ["record"; "field"]
  |> tr_type_name_common ["variant"; "option"]
  |> tr_type_name_common ["enum"; "option"]
  |> tr_type_name_common ["alias"]
  |> tr_type_name_common ["list"]
  (* functions *)
  |> simplify_function_param "input"
  |> simplify_function_param"output"
  |> simplify_function_param "error"


let compare_piqi_items a b =
  let name_of = function
    | `name x -> x
    | `named x -> x.T.Named#name
    | `typename x -> x
    | `typed x -> x.T.Typed#typename
    | _ -> assert false
  in
  let rank x =
    match name_of x with
      | "module" -> 0
      | "proto-package" -> 1
      | "include" -> 2
      | "import" -> 3
      (* skipping custom-field, see below
       * | "custom-field" -> 4
       *)
      | "piqdef" -> 5
      | "extend" -> 6
      | _ -> 100
  in
  rank a - rank b


let sort_piqi_items (ast:T.ast) =
  match ast with
    | `list l ->
        let l = List.stable_sort compare_piqi_items l in
        `list l
    | _ -> assert false


let piqi_to_ast ?(simplify=false) piqi =
  (* removing custom-field specifications as Piqi doesn't contain any
   * custom-fields at this stage *)
  let piqi = P#{piqi with custom_field = []} in
  (* XXX, TODO: move this call to Piqi.gen_piqi? *)
  let ast = Piqi.mlobj_to_ast !Piqi.piqi_lang_def T.gen__piqi piqi in
  let ast = sort_piqi_items ast in
  if simplify
  then simplify_piqi_ast ast
  else ast


let prettyprint_piqi ch (piqi:T.piqi) =
  let ast = piqi_to_ast piqi ~simplify:true in
  prettyprint_piqi_ast ch ast

