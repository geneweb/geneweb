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
(* open Piqi_json_common *)


(*
 * set json- names if not specified by user
 *)


(* json name of piqi name *)
let json_name' n =
  dashes_to_underscores n


let json_name n =
  Some (json_name' n)


(* check name validity *)
let check_json_name s =
  let error () = error s "invalid json-name" in

  if s = ""
  then error ();

  (match s.[0] with
    | 'a'..'z' | 'A'..'Z' | '_' -> ()
    | _ -> error ()
  );

  for i = 1 to String.length s - 1
  do
    match s.[i] with
      | 'a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '_' -> ()
      | _ -> error ()
  done


(* XXX: use name instead of json_name for foreign types? *)
let piqdef_json_name = function
  | `record t -> t.R#json_name
  | `variant t -> t.V#json_name
  | `enum t -> t.E#json_name
  | `alias t -> t.A#json_name
  | `list t -> t.L#json_name
  | _ ->
      (* this function will be called only for named types (i.e. piqdefs) *)
      assert false


let json_name_of name typeref =
  match name, typeref with
    | Some n, _ -> json_name n
    | None, Some t -> piqdef_json_name t
    | _ -> assert false


let json_name_field x =
  let open Field in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.typeref
    | Some n -> check_json_name n


let json_name_record x =
  let open Record in
  (match x.json_name with
     | None -> x.json_name <- json_name x.name
     | Some n -> check_json_name n
  )


let json_name_option x =
  let open Option in
  match x.json_name with
    | None -> x.json_name <- json_name_of x.name x.typeref
    | Some n -> check_json_name n


let json_name_variant x =
  let open Variant in
  (match x.json_name with
     | None -> x.json_name <- json_name x.name
     | Some n -> check_json_name n
  )


let json_name_alias x =
  let open Alias in
  match x.json_name with
    | None -> x.json_name <- json_name x.name
    | Some n -> check_json_name n


let json_name_list x =
  let open L in
  match x.json_name with
    | None -> x.json_name <- json_name x.name
    | Some n -> check_json_name n


let json_name_piqdef = function
  | `record x -> json_name_record x
  | `variant x | `enum x -> json_name_variant x
  | `alias x -> json_name_alias x
  | `list x -> json_name_list x


(* name fields and options *)
let json_name_record' x =
   List.iter json_name_field x.R#field

let json_name_variant' x =
   List.iter json_name_option x.V#option

let json_name_piqdef' = function
  | `record x -> json_name_record' x
  | `variant x | `enum x -> json_name_variant' x
  | _ -> ()


let json_name_defs defs =
    (* name data structures *)
    List.iter json_name_piqdef defs;
    (* name fields and options *)
    List.iter json_name_piqdef' defs


let json_name_piqi _idtable (piqi:T.piqi) =
  let open P in
  json_name_defs piqi.resolved_piqdef


(* NOTE: this function is called only in case if a JSON-related operation is
 * performed. We don't need this startup overhead otherwise *)
let init () =
  trace "init JSON\n";
  (* create JSON names in embedded Piqi self-specification *)
  (* AND add/check JSON names when loading any other Piqi modules *)
  Piqi.register_processing_hook json_name_piqi


(**)


let open_json fname =
  let ch = Piqi_main.open_input fname in
  let json_parser = Piqi_json_parser.init_from_channel ~fname ch in
  json_parser


let read_json_obj json_parser =
  let res = Piqi_json_parser.read_next json_parser in
  res


(* for internal use only: read one parsed JSON value from its string
 * representation *)
let json_of_string s :Piqi_json_common.json =
  let json_parser = Piqi_json_parser.init_from_string s in
  match read_json_obj json_parser with
    | Some ast -> ast
    | None -> assert false

