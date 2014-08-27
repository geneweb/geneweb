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


open Piqi_json_common


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


let error_duplicate obj name =
  error obj ("duplicate field: " ^ quote name)


let handle_unknown_field ((n, _) as x) =
  warning x ("unknown field: " ^ quote n)


let parse_int (obj:json) = match obj with
  | `Int x -> `int x
  | `Uint x -> `uint x
  | o -> error o "int constant expected"


let parse_float (x:json) = match x with
  | `Int x -> Int64.to_float x
  | `Uint x -> Piqobj_of_piq.uint64_to_float x
  | `Float x -> x
  | `String "NaN" -> Pervasives.nan
  | `String "Infinity" -> Pervasives.infinity
  | `String "-Infinity" -> Pervasives.neg_infinity
  | o -> error o "float constant expected"


let parse_bool (x:json) = match x with
  | `Bool x -> x
  | o -> error o "bool constant expected"


let parse_string (x:json) = match x with
  | `String x -> x
  | o -> error o "string constant expected"


let parse_binary (x:json) = match x with
  | `String x ->
      (try Piqi_base64.decode x
      with Invalid_argument _ ->
        error x "invalid base64-encoded string"
      )
  | o -> error o "string constant expected"


let rec parse_obj (t:T.piqtype) (x:json) :Piqobj.obj =
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (parse_float x)
    | `bool -> `bool (parse_bool x)
    | `string -> `string (parse_string x)
    | `binary -> `binary (parse_binary x)
    | `word -> `word (parse_string x)
    | `text -> `text (parse_string x)
    | `any -> `any (parse_any x)
    (* custom types *)
    | `record t -> `record (parse_record t x)
    | `variant t -> `variant (parse_variant t x)
    | `enum t -> `enum (parse_enum t x)
    | `list t -> `list (parse_list t x)
    | `alias t -> `alias (parse_alias t x)


and parse_any x =
  (* store JSON parse tree in the object store; it will be retrieved later when
   * needed by the referece *)
  let ref = Piqi_objstore.put x in
  let piq_any = T.Any#{T.default_any() with ref = Some ref} in
  Any#{ any = piq_any; obj = None }


and parse_record t = function
  | (`Assoc l) as x ->
      (* NOTE: passing locating information as a separate parameter since empty
       * list is unboxed and doesn't provide correct location information *)
      let loc = x in
      do_parse_record loc t l
  | o ->
      error o "object expected"


and do_parse_record loc t l =
  debug "do_parse_record: %s\n" t.T.Record#name;
  let fields_spec = t.T.Record#field in
  let fields, rem =
    List.fold_left (parse_field loc) ([], l) fields_spec in
  (* issue warnings on unparsed fields *)
  List.iter handle_unknown_field rem;
  (* put required fields back at the top *)
  R#{ piqtype = t; field = List.rev fields}


and parse_field loc (accu, rem) t =
  let fields, rem =
    match t.T.Field#typeref with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field loc t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let name = some_of t.json_name in
  debug "do_parse_flag: %s\n" name;
  let res, rem = find_flags name l in
  match res with
    | [] -> [], rem
    | [x] ->
        let res = F#{ piqtype = t; obj = None } in
        [res], rem
    | _::o::_ -> error_duplicate o name


and do_parse_field loc t l =
  let open T.Field in
  let name = some_of t.json_name in
  debug "do_parse_field: %s\n" name;
  let field_type = piqtype (some_of t.typeref) in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field loc name field_type l in
          [x], rem
      | `optional ->
          let x, rem = parse_optional_field name field_type t.default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          parse_repeated_field name field_type l
  in
  let fields =
    List.map (fun x -> F#{ piqtype = t; obj = Some x }) values
  in
  fields, rem
  

and parse_required_field loc name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> error loc ("missing field " ^ quote name)
    | [x] -> parse_obj field_type x, rem
    | _::o::_ -> error_duplicate o name


(* find field by name, return found fields and remaining fields *)
and find_fields (name:string) (l:(string*json) list) :(json list * (string*json) list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, v)::t when n = name -> aux (v::accu) rem t
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (l:(string*json) list) :(string list * (string*json) list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, `Bool true)::t when n = name -> aux (n::accu) rem t
    | (n, `Null ())::t when n = name -> aux accu rem t (* skipping *)
    | (n, _)::t when n = name ->
        error n ("value can not be specified for flag " ^ quote n)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and parse_optional_field name field_type default l =
  let res, rem = find_fields name l in
  match res with
    | [] -> Piqobj_of_wire.parse_default field_type default, rem
    | [`Null ()] -> None, rem
    | [x] -> Some (parse_obj field_type x), rem
    | _::o::_ -> error_duplicate o name


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> [], rem (* XXX: allowing repeated field to be acutally missing *)
    | [`List l] ->
        let res = List.map (parse_obj field_type) l in
        res, rem
    | [x] -> error x "array expected"
    | _::o::_ -> error_duplicate o name


and parse_variant t x =
  debug "parse_variant: %s\n" t.T.Variant#name;
  match x with
    | `Assoc [name, value] ->
        let options = t.T.Variant#option in
        let option =
          try
            let o =
              List.find (fun o ->
                some_of o.T.Option.json_name = name) options
            in
            parse_option o value
          with Not_found ->
            error x ("unknown variant option: " ^ quote name)
        in
        V#{ piqtype = t; option = option }
    | `Assoc l ->
        let l = List.filter (fun (n, v) -> v <> `Null ()) l in
        (match l with
          | [_] -> parse_variant t (`Assoc l)
          | _ -> error x "exactly one non-null option field expected"
        )
    | _ ->
        error x "object expected"


and parse_option t x =
  let open T.Option in
  match t.typeref, x with
    | None, `Bool true ->
        O#{ piqtype = t; obj = None }
    | None, _ ->
        error x "true value expected"
    | Some typeref, _ ->
        let option_type = piqtype typeref in
        let obj = parse_obj option_type x in
        O#{ piqtype = t; obj = Some obj }


and parse_enum t x =
  debug "parse_enum: %s\n" t.T.Variant#name;
  match x with
    | `String name ->
        let options = t.T.Variant#option in
        let option =
          try
            let o = List.find (fun o -> some_of o.T.Option#name = name) options in
            O#{ piqtype = o; obj = None }
          with Not_found ->
            error x ("unknown enum option: " ^ quote name)
        in
        V#{ piqtype = t; option = option }
    | _ ->
        error x "string enum value expected"


and parse_list t x =
  match x with
    | `List l ->
        debug "parse_list: %s\n" t.T.Piqlist#name;
        let obj_type = piqtype t.T.Piqlist#typeref in
        let contents = List.map (parse_obj obj_type) l in
        L#{ piqtype = t; obj = contents }
    | _ ->
        error x "array expected"


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t x =
  let open T.Alias in
  let obj_type = piqtype t.typeref in
  debug "parse_alias: %s\n" t.T.Alias#name;
  let obj = parse_obj obj_type x in
  A#{ piqtype = t; obj = obj }

