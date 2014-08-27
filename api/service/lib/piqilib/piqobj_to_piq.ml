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


module R = Piqobj.Record
module F = Piqobj.Field
module V = Piqobj.Variant
module E = Piqobj.Variant
module O = Piqobj.Option
module A = Piqobj.Alias
module Any = Piqobj.Any
module L = Piqobj.List


(* NOTE: loosing precision here, in future we will support encoding floats as
 * string literals containing binary representation of 64-bit IEEE float *)
let gen_float x = `float x


let is_ascii_string s =
  let len = String.length s in
  let rec aux i =
    if i >= len
    then true
    else
      if Char.code s.[i] <= 127
      then aux (i + 1)
      else false
  in
  aux 0


let gen_string s =
  if is_ascii_string s
  then
    `ascii_string s
  else
    `utf8_string s


let gen_binary s =
  if is_ascii_string s
  then
    `ascii_string s
  else
    `binary s


let make_named name value =
  let open T in
  `named Named#{name = name; value = value}


let make_name name =
  `name name


(* (re-)order fields according to their positions in the original piqi spec *)
let order_record_fields t piqobj_fields =
  let find_fields ft l =
    List.partition (fun x -> x.F.piqtype == ft) l
  in
  let res, _rem =
    List.fold_left
      (fun (accu, rem) x -> (* folder *)
        let res, rem' = find_fields x rem in
        (List.rev_append res accu, rem'))

      ([], piqobj_fields) (* accu *)

      t.T.Record#field (* list to fold *)
  in
  List.rev res


let rec gen_obj0 (x:Piqobj.obj) :T.ast =
  match x with
    (* built-in types *)
    | `int x -> `int x
    | `uint x -> `uint x
    | `float x -> gen_float x
    | `bool x -> `bool x
    | `string x -> gen_string x
    | `binary x -> gen_binary x
    | `word x -> `word x
    | `text x -> `text x
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x


(* TODO: provide more precise locations for fields, options, etc *)
and gen_obj x = Piq_parser.piq_reference gen_obj0 x


and gen_typed_obj x =
  let name = Piqobj_common.full_typename x in
  let any = T.Any#{T.default_any() with ast = Some (gen_obj x)} in
  `typed T.Typed#{typename = name; value = any }


and gen_any x =
  let open Any in
  match x.any.T.Any.ast with
    | Some ast -> ast
    | None ->
        (match x.any.T.Any#binobj, x.any.T.Any#typename with
          | Some x, Some n ->
              (* generate the ast representation from the binary object if the
               * type is known *)
              (match Piqi_db.try_find_piqtype n with
                | Some t ->
                    Piqloc.pause ();
                    let piqobj = Piqobj_of_wire.parse_binobj t x in
                    let res = gen_obj piqobj in
                    Piqloc.resume ();
                    res
                | None ->
                    assert false
              )
          | _ ->
              (* XXX: are they always defined? *)
              assert false
        )


and gen_record x =
  let open R in
  (* TODO, XXX: doing ordering at every generation step is inefficient *)
  let fields = order_record_fields x.piqtype x.field in
  `list (List.map gen_field fields)


and gen_field x =
  let open F in
  let name = name_of_field x.piqtype in
  let res =
    match x.obj with
      | None -> make_name name
      | Some obj -> make_named name (gen_obj obj)
  in Piq_parser.piq_addrefret x res


and gen_variant x =
  let open V in
  gen_option x.option


and gen_option x =
  let open O in
  let name = name_of_option x.piqtype in
  let res =
    match x.obj with
      | None -> make_name name
      | Some obj -> make_named name (gen_obj obj)
  in Piq_parser.piq_addrefret x res


and gen_enum x = gen_variant x


and gen_list x = 
  let open L in
  `list (List.map gen_obj x.obj)


and gen_alias x =
  let open A in
  match x.obj with
    | `alias x -> gen_alias x
    | x -> gen_obj x

