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


let error_duplicate obj name =
  error obj ("duplicate field: " ^ quote name)


let handle_unknown_field ((n, _) as x) =
  warning x ("unknown field: " ^ quote n)


let check_duplicate name tail =
  match tail with
    | [] -> ()
    | l ->
        List.iter (fun obj ->
          warning obj ("duplicate field " ^ quote name)) l


let parse_scalar xml_elem err_string =
  let _name, l = xml_elem in
  match l with
    | [`Data s] -> s
    | _ -> error xml_elem err_string


let parse_string_scalar xml_elem err_string =
  let _name, l = xml_elem in
  match l with
    | [] -> (* empty element content means empty string *)
        let res = "" in
        Piqloc.addrefret xml_elem res
    | [`Data s] -> s
    | _ -> error xml_elem err_string


let parse_int xml_elem = 
  let s = parse_scalar xml_elem "int constant expected" in
  let i =
    try 
      (* XXX: move this function to a common module *)
      Piqi_json_parser.parse_int64 s
    with Failure _ ->
      (* NOTE: actually, there can be two errors here: invalid integer literal
       * and integer overflow *)
      error xml_elem "invalid int constant"
  in
  match i with
    | `Int x -> `int x
    | `Uint x -> `uint x


let parse_float xml_elem =
  let s = parse_scalar xml_elem "float constant expected" in
  match s with
    | "NaN" -> Pervasives.nan
    | "Infinity" -> Pervasives.infinity
    | "-Infinity" -> Pervasives.neg_infinity
    | _ ->
        try float_of_string s
        with Failure _ ->
          error xml_elem "invalid float constant"


let parse_bool xml_elem =
  let err = "bool constant expected" in
  let s = parse_scalar xml_elem err in
  match s with
    | "true" -> true
    | "false" -> false
    | _ ->
        error xml_elem err


let parse_string xml_elem =
  parse_string_scalar xml_elem "string constant expected"


let parse_binary xml_elem =
  let s = parse_string_scalar xml_elem "binary constant expected" in
  try Piqi_base64.decode s
  with Invalid_argument _ ->
    error xml_elem "invalid base64-encoded string"


let rec parse_obj (t: T.piqtype) (x: xml_elem) :Piqobj.obj =
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
  let ref = Piqi_objstore.put (`Elem x) in
  let piq_any = T.Any#{T.default_any() with ref = Some ref} in
  Any#{ any = piq_any; obj = None }


and parse_record t xml_elem =
  debug "do_parse_record: %s\n" t.T.Record#name;
  (* get the list of XML elements from the node *)
  let _name, l = xml_elem in
  let l = List.map (fun xml ->
    match xml with
      | `Elem x -> x
      | `Data s ->
          error xml "XML element is expected as a record field") l
  in

  (* NOTE: passing locating information as a separate parameter since empty
   * list is unboxed and doesn't provide correct location information *)
  let loc = xml_elem in

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
  let name = some_of t.name in (* flag name is always defined *)
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
  let name = C.name_of_field t in
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
and find_fields (name:string) (l:xml_elem list) :(xml_elem list * xml_elem list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | ((n, _) as h)::t when n = name -> aux (h::accu) rem t
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (l:xml_elem list) :(string list * xml_elem list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, [])::t when n = name -> aux (n::accu) rem t
    | (n, _)::t when n = name ->
        error n ("value can not be specified for flag " ^ quote n)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and parse_optional_field name field_type default l =
  let res, rem = find_fields name l in
  match res with
    | [] -> Piqobj_of_wire.parse_default field_type default, rem
    | [x] -> Some (parse_obj field_type x), rem
    | _::o::_ -> error_duplicate o name


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> [], rem (* XXX: allowing repeated field to be acutally missing *)
    | l ->
        let res = List.map (parse_obj field_type) l in
        res, rem


and parse_variant t xml_elem =
  debug "parse_variant: %s\n" t.T.Variant#name;
  let _name, l = xml_elem in
  match l with
    | [`Elem ((name, _) as xml_elem)] ->
        let options = t.T.Variant#option in
        let option =
          try
            let o = List.find (fun o -> name = C.name_of_option o) options in
            parse_option o xml_elem
          with Not_found ->
            error xml_elem ("unknown variant option: " ^ quote name)
        in
        V#{ piqtype = t; option = option }
    | _ ->
        error xml_elem "exactly one XML element expected as a variant value"


and parse_option t xml_elem =
  let open T.Option in
  let name, l = xml_elem in
  match t.typeref, l with
    | None, [] ->
        O#{ piqtype = t; obj = None }
    | None, _ ->
        error name ("no value expected for option flag " ^ quote name)
    | Some typeref, _ ->
        let option_type = piqtype typeref in
        let obj = parse_obj option_type xml_elem in
        O#{ piqtype = t; obj = Some obj }


and parse_enum t xml_elem =
  debug "parse_enum: %s\n" t.T.Variant#name;
  let name =
    parse_scalar xml_elem "exactly one XML CDATA expected as an enum value"
  in
  let options = t.T.Variant#option in
  let option =
    try
      let o = List.find (fun o -> some_of o.T.Option#name = name) options in
      O#{ piqtype = o; obj = None }
    with Not_found ->
      error name ("unknown enum option: " ^ quote name)
  in
  V#{ piqtype = t; option = option }


and parse_list t xml_elem =
  debug "parse_list: %s\n" t.T.Piqlist#name;
  let obj_type = piqtype t.T.Piqlist#typeref in
  let _name, l = xml_elem in
  let contents = List.map (parse_list_item obj_type) l in
  L#{ piqtype = t; obj = contents }


and parse_list_item obj_type xml =
  debug "parse_list_item\n";
  match xml with
    | `Elem (("item", l) as xml_elem) ->
        parse_obj obj_type xml_elem
    | _ ->
        error xml "<item> XML element expected as a list item value"


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t x =
  let open T.Alias in
  let obj_type = piqtype t.typeref in
  debug "parse_alias: %s\n" t.T.Alias#name;
  let obj = parse_obj obj_type x in
  A#{ piqtype = t; obj = obj }


(* parse top-level Piq object formatted as XML *)
let parse_obj t xml =
  (* XXX: don't bother checking the name of the root element -- it's quite
   * annoying in practice
   *
   * XXX: provide an optional "name" parameter for checking the root element's
   * name *)
  (*
  let name = C.piqi_typename t in
  match xml with
    | `Elem ((n, _) as xml_elem) when n = name ->
        parse_obj t xml_elem
    | _ ->
        error xml ("<" ^ name ^ "> XML root element expected")
  *)
  match xml with
    | `Elem xml_elem ->
        parse_obj t xml_elem
    | _ ->
        error xml "XML root element expected"

