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


(*
 * check and assign wire codes for fields and options
 *)

(*

From Protobuf Language Guide
(http://code.google.com/apis/protocolbuffers/docs/proto.html)

The smallest tag number you can specify is 1, and the largest is 2^29 - 1, or
536,870,911. You also cannot use the numbers 19000 though 19999
(FieldDescriptor::kFirstReservedNumber through
FieldDescriptor::kLastReservedNumber), as they are reserved for the Protocol
Buffers implementation - the protocol buffer compiler will complain if you use
one of these reserved numbers in your .proto. 

Enumerator constants must be in the range of a 32-bit integer. Since enum
values use varint encoding on the wire, negative values are inefficient and
thus not recommended.

*)

open Piqi_common


let default_wire_type (t:T.piqtype) =
  match t with
    | `int -> `zigzag_varint
    | `float -> `fixed64
    | `bool -> `varint
    | _ -> `block


let wire_type_name (wt:T.wire_type) = 
  match wt with
    | `varint -> "varint"
    | `zigzag_varint -> "zigzag_varint"
    | `fixed32 -> "fixed32"
    | `fixed64 -> "fixed64"
    | `signed_varint -> "signed_varint"
    | `signed_fixed32 -> "signed_fixed32"
    | `signed_fixed64 -> "signed_fixed64"
    | `block -> "block"


let get_wire_type (t:T.piqtype) (wt:T.wire_type option) =
  match wt with
    | None -> default_wire_type t
    | Some wt -> wt


let get_wire_type_name t wt =
  let wt = get_wire_type t wt in
  wire_type_name wt


(* get wire type width in bits, if it is known to be fixed *)
let get_wire_type_width t wt =
  let wt = get_wire_type t wt in
  match wt with
    | `fixed32 | `signed_fixed32 -> Some 32
    | `fixed64 | `signed_fixed64 -> Some 64
    | _ -> None


(*
 * add wire codes if not specified by user
 *) 


let invalid_max_code = Int32.shift_left 1l 29 (* 2 ^ 29 *)


let incr i =
  i := Int32.succ !i;
  if !i = 19000l
  then i := 20000l
  else
    if !i = invalid_max_code
    then
      piqi_error "auto-generatated wire code exceeds allowed range (1..2^29-1)"
    else ()


let addcodes_field code f =
  let open T.Field in
  match f.code with
    | None -> (* assign previously unassigned code *)
        f.code <- Some !code; incr code
    | Some _ -> assert false


let addcodes_option code o =
  let open T.Option in
  match o.code with
    | None -> (* assign previously unassigned code *)
        o.code <- Some !code; incr code
    | Some _ -> assert false


let addcodes_enum_option code o =
  let open T.Option in
  match o.code with
    | None -> (* assign previously unassigned code *)
        o.code <- Some !code; code := Int32.succ !code
    | Some _ -> assert false


let check_code i =
  let (<) a b = Int32.compare a b < 0 in
  let (>=) a b = Int32.compare a b >= 0 in
  if i < 1l || i >= invalid_max_code || (i >= 19000l && i < 20000l)
  then error i "wire code is out of allowed range"


let check_codes codes =
  List.iter check_code codes;
  match find_dups codes with
    | None -> ()
    | Some (code, _prev) ->
        error code ("duplicate wire code: " ^ Int32.to_string code)


let check_enum_codes codes =
  match find_dups codes with
    | None -> ()
    | Some (code, _prev) ->
        warning code ("duplicate enum wire code: " ^ Int32.to_string code)


(* order fields by their field's codes *)
let order_fields fields =
  List.sort
    (fun a b ->
      match a.F#code, b.F#code with
        | Some a, Some b -> Int32.to_int (Int32.sub a b) (* a - b *)
        | _ -> assert false) fields


let addcodes_record r =
  let open T.Record in
  let fields = r.field in
  if List.exists (fun x -> x.T.Field#code <> None) fields
  then (
    if List.exists (fun x -> x.T.Field#code = None) fields
    then error r "codes must be defined for either all or none fields"
    else
      (* all field codes are assigned *)
      let codes = List.map (fun x -> some_of x.T.Field#code) fields in
      check_codes codes;

      (* pre-order fields by their codes to speed-up further processing *)
      r.wire_field <- order_fields fields
  )
  else (
    let code = ref 1l in (* assign codes *)
    List.iter (addcodes_field code) fields;
    r.wire_field <- fields (* the order of fields remains the same *)
  )


let addcodes_variant v =
  let open T.Variant in
  let options = v.option in
  if List.exists (fun x -> x.T.Option#code <> None) options
  then (
    if List.exists (fun x -> x.T.Option#code = None) options
    then error v "codes must be defined for either all or none variant options"
    else
      (* all option codes are assigned *)
      let codes = List.map (fun x -> some_of x.T.Option#code) options in
      check_codes codes
  )
  else (
    let code = ref 1l in (* assign codes *)
    List.iter (addcodes_option code) options
  )


let addcodes_enum v =
  let open T.Variant in
  let options = v.option in
  if List.exists (fun x -> x.T.Option#code <> None) options
  then (
    if List.exists (fun x -> x.T.Option#code = None) options
    then error v "codes must be defined for either all or none enum options"
    else
      (* all option codes are assigned *)
      let codes = List.map (fun x -> some_of x.T.Option#code) options in
      check_enum_codes codes
  )
  else (
    (* XXX: assign enum constant values starting from 0? *)
    let code = ref 1l in (* assign codes *)
    List.iter (addcodes_enum_option code) options
  )


(* only primitive numeric types can be packed
 *
 * NOTE: enum is also counted as primitive numeric type *)
let can_be_packed t =
  match unalias t with
    | `int | `float | `bool -> true
    | `enum _ -> true
    | _ -> false


let check_packed_type obj t =
  if not (can_be_packed (piqtype t))
  then
    error obj "packed representation can be used only for numeric, bool and enum types"


let check_packed_field x =
  let open F in
  if x.wire_packed
  then (
    if x.mode <> `repeated
    then error x "packed representation can be used only for repeated fields";

    check_packed_type x (some_of x.typeref)
  )


let check_packed_list x =
  let open L in
  if x.wire_packed
  then check_packed_type x x.typeref


let process_def = function
  | `record x ->
      List.iter check_packed_field x.R#field;
      addcodes_record x
  | `variant x -> addcodes_variant x
  | `enum x -> addcodes_enum x
  | `list x -> check_packed_list x
  | _ -> ()


let process_defs (defs: T.piqdef list) =
  List.iter process_def defs


(*
 * Add codes for fields and options based on field/option name hash codes.
 *)


let rec hashcode name =
  let accu = ref 0 in
  for i = 0 to String.length name - 1 do
    accu := 223 * !accu + Char.code name.[i]
  done;
  (* reduce to 29 bits *)
  let res = !accu land (1 lsl 29 - 1) in
  if (res >= 19000 && res < 20000) || res = 0 (* invalid code values *)
  then
    (* recalculate hashcode on (name ^ "@") *)
    hashcode (name ^ "@")
  else res


let hashcode' x =
  let res = Int32.of_int (hashcode x) in
  Some res


let add_hashcodes_field f =
  let open T.Field in
  f.code <- hashcode' (name_of_field f)


let add_hashcodes_option o =
  let open T.Option in
  o.code <- hashcode' (name_of_option o)


let add_hashcodes_record r =
  let open T.Record in
  (* NOTE: silently overriding existing codes *)
  List.iter add_hashcodes_field r.field


let add_hashcodes_variant v =
  let open T.Variant in
  (* NOTE: silently overriding existing codes *)
  List.iter add_hashcodes_option v.option


let add_hashcodes_def def =
  match def with
    | `record x -> add_hashcodes_record x
    | `variant x | `enum x ->
        (* NOTE: we don't bother creating hashcodes for enum options separately,
         * althought the range for enum options is wider -- 32 bits as opposed to
         * 29 bits for field and enum options' codes *)
        add_hashcodes_variant x
    | _ -> ()


(* Add hash-based field and option codes instead of auto-enumerated ones.
 *
 * NOTE: Assigned hashcodes will be checked for hash conflicts at a later stage
 * by calling Piqi_wire.add_codes() from Piqi module as usual. This will ensure
 * we didn't get any hash conflicts and, also, reorder record fields by codes.
 *)
let add_hashcodes (defs: T.piqdef list) =
  List.iter add_hashcodes_def defs

