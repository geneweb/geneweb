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


module C = Piqi_common
open C


let _ = Piqilib.init ()


module R = Piqobj.Record
module F = Piqobj.Field
module V = Piqobj.Variant
module E = Piqobj.Variant
module O = Piqobj.Option
module A = Piqobj.Alias
module Any = Piqobj.Any
module L = Piqobj.List


type xml = Piqi_xml.xml
type xml_elem = Piqi_xml.xml_elem


let uint64_to_string x =
  Printf.sprintf "%Lu" x


let xml_string_of_float x =
  (* Using JavaScript notation for NaN and Infinity *)
  match Pervasives.classify_float x with
    | FP_nan -> "NaN"
    | FP_infinite -> if x > 0. then "Infinity" else "-Infinity"
    | _ -> Piq_gen.string_of_float x


let escape_xml_text x =
  (* TODO, XXX: escape control characters, '\r', leading and trailing whitespace
   *
   * NOTE: Xmlm library escapes only '<', '>', '&' characters *)
  x


let gen_scalar to_string_f x =
  let s = to_string_f x in
  [`Data s]


let make_element name contents =
  `Elem (name, contents)


let rec gen_obj (x:Piqobj.obj) :xml list =
  match x with
    (* built-in types *)
    | `int x -> gen_scalar Int64.to_string x
    | `uint x -> gen_scalar uint64_to_string x
    | `float x -> gen_scalar xml_string_of_float x
    | `bool x -> gen_scalar Pervasives.string_of_bool x (* "true" | "false" *)
    | `binary x -> gen_scalar Piqi_base64.encode x
    | `string x | `word x | `text x  -> gen_scalar escape_xml_text x
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x


and gen_any x =
  let open Any in
  (* NOTE: converting only typed and fully resolved piq objects to xml *)
  match x.obj with
    | None -> [`Elem ("undefined", [])] (* XXX: will it be always present? *)
    | Some obj -> gen_obj obj


and gen_record x =
  let open R in
  let field_types = x.piqtype.T.Record.field in
  (* generate fields and order them according to the order of fields in the
   * original Piqi record specification *)
  flatmap (gen_field x.field) field_types


and gen_field fields t =
  let open F in
  let name = C.name_of_field t in
  (* find all fields of the given type *)
  let fields = List.find_all (fun f -> f.piqtype == t) fields in
  (* generate fields *)
  List.map (fun f -> gen_obj_element name f.obj) fields


and gen_obj_element name = function
  | None -> make_element name [] (* empty element *)
  | Some obj -> make_element name (gen_obj obj)


and gen_variant x =
  let open V in
  let o = gen_option x.option in
  [o]


and gen_option x =
  let open O in
  let name = C.name_of_option x.piqtype in
  gen_obj_element name x.obj


and gen_enum x =
  let open V in
  gen_scalar gen_enum_option x.option


and gen_enum_option x =
  let open O in
  let name = C.name_of_option x.piqtype in
  name


and gen_list x = 
  let open L in
  List.map (fun x -> make_element "item" (gen_obj x)) x.obj


and gen_alias x =
  let open A in
  match x.obj with
    | `alias x -> gen_alias x
    | x -> gen_obj x


(* gen top-level XML element *)
let gen_obj (obj: Piqobj.obj) :xml =
  let piqtype = Piqobj_common.type_of obj in
  let name = C.piqi_typename piqtype in
  make_element name (gen_obj obj)

