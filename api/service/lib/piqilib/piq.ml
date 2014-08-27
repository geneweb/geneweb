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

(* Piq stream *)


module C = Piqi_common  
open C


let _ = Piqilib.init ()


exception EOF

(* piq stream object *)
type obj =
  | Piqtype of string
  | Typed_piqobj of Piqobj.obj
  | Piqobj of Piqobj.obj
  | Piqi of T.piqi


let open_piq fname =
  trace "opening .piq file: %s\n" fname;
  let ch = Piqi_main.open_input fname in
  let piq_parser = Piq_parser.init_from_channel fname ch in
  piq_parser


let read_piq_ast piq_parser :T.ast = 
  let res = Piq_parser.read_next piq_parser in
  match res with
    | Some ast -> ast
    | None -> raise EOF


let default_piqtype = ref None


let check_piqtype n =
  if not (Piqi_name.is_valid_typename n)
  then error n ("invalid type name: " ^ quote n)
  else ()


let find_piqtype ?(check=false) typename =
  if check
  then check_piqtype typename;

  try Piqi_db.find_piqtype typename
  with Not_found ->
    error typename ("unknown type: " ^ typename)


let process_default_piqtype ?check typename =
  let piqtype = find_piqtype ?check typename in
  (* NOTE: silently overriding previous value *)
  default_piqtype := Some piqtype


(* default piqtype taken from the stream overrides the user-specified
 * one *)
let get_current_piqtype user_piqtype locref =
  match !default_piqtype, user_piqtype with
  | Some x, _ -> x
  | None, Some x -> x
  | None, None ->
      error locref "type of object is unknown"


let piqi_of_piq fname ast =
  let piqi = Piqi.parse_piqi ast in
  Piqi.process_piqi piqi ~fname; (* NOTE: caching the loaded module *)
  piqi


let load_piq_obj (user_piqtype: T.piqtype option) piq_parser :obj =
  let ast = read_piq_ast piq_parser in
  let fname, _ = piq_parser in (* TODO: improve getting a filename from parser *)
  match ast with
    | `typed {T.Typed.typename = "piqtype";
              T.Typed.value = {T.Any.ast = Some (`word typename)}} ->
        (* :piqtype <typename> *)
        process_default_piqtype typename;
        Piqtype typename
    | `typed {T.Typed.typename = "piqtype"} ->
        error ast "invalid piqtype specification"
    | `typed {T.Typed.typename = "piqi";
              T.Typed.value = {T.Any.ast = Some ((`list _) as ast)}} ->
        (* :piqi <piqi-spec> *)
        let piqi = piqi_of_piq fname ast in
        Piqi piqi
    | `typed {T.Typed.typename = "piqi"} ->
        error ast "invalid piqi specification"
    | `typename x ->
        error x "invalid piq object"
    | `typed _ ->
        let obj = Piqobj_of_piq.parse_typed_obj ast in
        Typed_piqobj obj
    | _ ->
        let piqtype = get_current_piqtype user_piqtype ast in
        let obj = Piqobj_of_piq.parse_obj piqtype ast in
        Piqobj obj


let make_piqtype typename =
  `typed {
    T.Typed.typename = "piqtype";
    T.Typed.value = {
      T.default_any () with
      T.Any.ast = Some (`word typename);
    }
  }


let original_piqi piqi =
  let orig_piqi = some_of piqi.P#original_piqi in
  (* make sure that the module's name is set *)
  P#{orig_piqi with modname = piqi.P#modname}


let piqi_to_piq piqi =
  let piqi_ast = Piqi_pp.piqi_to_ast (original_piqi piqi) ~simplify:true in
  `typed {
    T.Typed.typename = "piqi";
    T.Typed.value = {
      T.default_any () with
      T.Any.ast = Some piqi_ast;
    }
  }


let gen_piq (obj :obj) =
  match obj with
    | Piqtype typename ->
        make_piqtype typename
    | Piqi piqi ->
        piqi_to_piq piqi
    | Typed_piqobj obj ->
        Piqobj_to_piq.gen_typed_obj obj
    | Piqobj obj ->
        Piqobj_to_piq.gen_obj obj


let write_piq ch (obj:obj) =
  let ast = gen_piq obj in
  Piq_gen.to_channel ch ast;
  (* XXX: add one extra newline for better readability *)
  Pervasives.output_char ch '\n'


let open_wire fname =
  trace "opening .wire file: %s\n" fname;
  let ch = Piqi_main.open_input fname in
  let buf = Piqirun.IBuf.of_channel ch in
  buf


let read_wire_field buf =
  (* TODO: handle runtime wire read errors *)
  match Piqirun.parse_field buf with
    | Some x -> x
    | None -> raise EOF


let piqtypes = ref []

let add_piqtype code piqtype =
  if code = 1 (* default piqtype *)
  then
    (* NOTE: silently overriding previous value *)
    default_piqtype := Some piqtype
  else
    let code = (code+1)/2 in
    piqtypes := (code, piqtype) :: !piqtypes


let find_piqtype_by_code code =
  try
    let (_,piqtype) =
      List.find
        (function (code',_) when code = code' -> true | _ -> false)
        !piqtypes
    in piqtype
  with
    Not_found ->
      (* TODO: add stream position info *)
      piqi_error
        ("invalid field code when reading .wire: " ^ string_of_int code)


(* remove everything but "binobj" and "ast" in a field's default value *)
let reset_field_defaults f =
  let open F in
  match f.default with
    | None -> ()
    | Some x ->
        let any = Any#{T.default_any () with ast = x.ast; binobj = x.binobj} in
        f.default <- Some any


let reset_record_defaults = function
  | `record x ->
      List.iter reset_field_defaults x.R#field
  | _ -> ()


let reset_defaults defs =
  List.iter reset_record_defaults defs


let expand_piqi piqi =
  let expanded_piqi = Piqi_ext.expand_piqi piqi in

  (* make sure we include all automatically assigned hash-based wire code for
   * fiels and options *)
  if C.is_self_spec piqi
  then Piqi_wire.add_hashcodes expanded_piqi.P#piqdef;

  (* make sure that the module's name is set *)
  P#{expanded_piqi with modname = piqi.P#modname}


let piqi_to_piqobj piqi =
  let piqi = expand_piqi piqi in

  let piqtype = !Piqi.piqi_spec_def in
  let wire_generator = T.gen__piqi in
  Piqi.mlobj_to_piqobj piqtype wire_generator piqi


(* using max code value as a wire code for Piqi
 *
 * XXX: alternatively, we could use an invalid value like 0, or lowest possible
 * code, i.e. 1 *)
let piqi_spec_wire_code = (1 lsl 29) - 1


let piqi_to_pb_common piqi ~code =
  (* TODO: fix that ugliness by providing an "external mode" in Piqi_of/to_wire
   * that respects the external definition of the "any" record *)
  reset_defaults piqi.P#extended_piqdef;

  let piqobj = piqi_to_piqobj piqi in

  Piqloc.pause ();
  let pb = Piqobj_to_wire.gen_obj code piqobj in
  Piqloc.resume ();
  pb


let piqi_to_pb piqi =
  piqi_to_pb_common piqi ~code:-1 (* -1 means don't generate wire code *)


let piqi_to_wire piqi =
  piqi_to_pb_common piqi ~code:piqi_spec_wire_code


let piqi_of_wire bin ~cache =
  (* don't store location references as we're loading from the binary object *)
  Piqloc.pause ();

  (* TODO: use a safer method using the Piqi.piqi_spec_def, i.e. Piqi
   * self-specificaion rather that the language-impl *)
  let piqi = T.parse_piqi bin in
  Piqloc.resume ();

  Piqi.process_piqi piqi ~cache;
  piqi


let piqobj_of_wire piqtype buf =
  (* don't store location references as we're loading from the binary object *)
  Piqloc.pause ();
  let obj = Piqobj_of_wire.parse_obj piqtype buf in
  Piqloc.resume ();
  obj


let process_piqtype code typename =
  let piqtype =
    try Piqi_db.find_piqtype typename
    with Not_found ->
      (* TODO: add stream position info *)
      piqi_error ("unknown type: " ^ typename)
  in
  add_piqtype code piqtype


let rec load_wire_obj (user_piqtype :T.piqtype option) buf :obj =
  let field_code, field_obj = read_wire_field buf in
  match field_code with
    | c when c = piqi_spec_wire_code -> (* embedded Piqi spec *)
        (* NOTE: caching the loaded module *)
        let piqi = piqi_of_wire field_obj ~cache:true in
        Piqi piqi
    | c when c mod 2 = 1 ->
        let typename = Piqirun.parse_string_field field_obj in
        process_piqtype c typename;
        if c = 1
        then
          (* :piqtype <typename> *)
          Piqtype typename
        else
          (* we've just read type-code binding information;
             proceed to the next stream object *)
          load_wire_obj user_piqtype buf
    | 2 ->
        let piqtype =
          try get_current_piqtype user_piqtype `fake
          with _ ->
            (* TODO: add stream position info *)
            piqi_error "default type for piq wire object is unknown"
        in
        let obj = piqobj_of_wire piqtype field_obj in
        Piqobj obj
    | c -> (* the code is even which means typed piqobj *)
        let piqtype = find_piqtype_by_code (c/2) in
        let obj = piqobj_of_wire piqtype field_obj in
        Typed_piqobj obj


let out_piqtypes = ref []
let next_out_code = ref 2


let gen_piqtype code typename =
  Piqirun.gen_string_field code typename


let find_add_piqtype_code name =
  try 
    let (_, code) =
      List.find
        (function (name',_) when name = name' -> true | _ -> false)
        !out_piqtypes
    in None, code
  with Not_found ->
    let code = !next_out_code * 2 in
    incr next_out_code;
    out_piqtypes := (name, code)::!out_piqtypes;
    let piqtype = gen_piqtype (code-1) name in
    Some piqtype, code


let gen_wire (obj :obj) =
  match obj with
    | Piqi piqi ->
        piqi_to_wire piqi
    | Piqtype typename ->
        gen_piqtype 1 typename
    | Piqobj obj ->
        Piqobj_to_wire.gen_obj 2 obj
    | Typed_piqobj obj ->
        let typename = Piqobj_common.full_typename obj in
        let piqtype, code = find_add_piqtype_code typename in
        let data = Piqobj_to_wire.gen_obj code obj in
        match piqtype with
          | None -> data
          | Some x ->
              (* add the piqtype entry before the data *)
              Piqirun.OBuf.iol [ x; data]

 
let write_wire ch (obj :obj) =
  let data = gen_wire obj in
  Piqirun.to_channel ch data


let open_pb fname =
  trace "opening .pb file: %s\n" fname;
  let ch = Piqi_main.open_input fname in
  let buf = Piqirun.init_from_channel ch in
  buf


(* NOTE: this function will be called exactly once *)
let load_pb (piqtype:T.piqtype) wireobj :obj =
  (* TODO: handle runtime wire read errors *)
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = piqi_of_wire wireobj ~cache:true in
    Piqi piqi
  else
    let obj = piqobj_of_wire piqtype wireobj in
    Typed_piqobj obj


let gen_pb (obj :obj) =
  match obj with
    | Piqi piqi ->
        piqi_to_pb piqi
    | Typed_piqobj obj | Piqobj obj ->
        (* -1 is a special code meaning that key and length for blocks should
         * not be generated. The resulting code is the same as generated by
         * Piqi_to_wire.gen_binobj, but this way it is returned as an output
         * buffer instead of a string in order to avoid extra memory copying *)
        Piqobj_to_wire.gen_obj (-1) obj
    | Piqtype _ ->
        (* ignore default type names *)
        Piqirun.OBuf.iol [] (* == empty output *)


let write_pb ch (obj :obj) =
  let buf = gen_pb obj in
  Piqirun.to_channel ch buf


(*
 * JSON reading and writing
 *)

let piqobj_of_json piqtype json :Piqobj.obj =
  Piqobj_of_json.parse_obj piqtype json


let piqobj_of_json_ref piqtype ref =
  let json = Piqi_objstore.get ref in
  piqobj_of_json piqtype json


let piqi_of_json json ~cache =
  let piqtype = !Piqi.piqi_spec_def in
  let wire_parser = T.parse_piqi in

  (* don't resolve defaults when reading Json *)
  let piqobj =
    C.with_resolve_defaults false (Piqobj_of_json.parse_obj piqtype) json
  in
  let piqi = Piqi.mlobj_of_piqobj wire_parser piqobj in

  (* set the default field resolver to json *)
  Piqi.piqobj_of_ref := piqobj_of_json_ref;

  Piqi.process_piqi piqi ~cache;
  piqi


let piqi_to_json piqi =
  let piqobj = piqi_to_piqobj piqi in
  Piqobj_to_json.gen_obj piqobj


let write_json_obj ch json =
  Piqi_json_gen.pretty_to_channel ch json;
  (* XXX: add a newline for better readability *)
  Pervasives.output_char ch '\n'


let gen_json_common (piqobj : Piqobj.obj) =
  let ast = Piqobj_to_json.gen_obj piqobj in
  let piqtype = Piqobj_common.type_of piqobj in
  (* generating an associative array wrapper for primitive types because JSON
   * doesn't support them as top-level objects, according to RFC 4627 that says:
   * "A JSON text is a serialized object or array" *)
  if C.is_primitive_piqtype piqtype
  then `Assoc ["_", ast]
  else ast


let gen_piq_json (obj :obj) =
  match obj with
    | Piqi piqi -> (* embedded Piqi spec *)
        let json = piqi_to_json piqi in
        `Assoc [ "_piqi", json ]
    | Piqtype typename ->
        `Assoc [ "_piqtype", `String typename ]
    | Typed_piqobj obj ->
        Piqobj_to_json.gen_typed_obj obj
    | Piqobj obj ->
        gen_json_common obj


let write_piq_json ch (obj:obj) =
  let json = gen_piq_json obj in
  write_json_obj ch json


let gen_json (obj :obj) =
  match obj with
    | Typed_piqobj obj | Piqobj obj ->
        gen_json_common obj
    | Piqi piqi ->
        (* output Piqi spec itself if we are converting .piqi *)
        piqi_to_json piqi
    | Piqtype _ ->
        (* XXX *)
        assert false (* type hints are not supported by Json encoding *)


let write_json ch (obj:obj) =
  let json = gen_json obj in
  write_json_obj ch json


let read_json_ast json_parser :Piqi_json_common.json =
  let res = Piqi_json.read_json_obj json_parser in
  match res with
    | Some ast -> ast
    | None -> raise EOF


let load_json_common piqtype ast =
  let ast =
    if C.is_primitive_piqtype piqtype
    then
    (* expecting primitive types to be wrapped in associative array because JSON
     * doesn't support them as top-level objects, according to RFC 4627 that
     * says: "A JSON text is a serialized object or array" *)
      match ast with
        | `Assoc [ "_", ast ] -> ast
        | _ ->
            error ast
              "invalid toplevel value for primitive type: {\"_\": ...} expected"
    else ast
  in
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = piqi_of_json ast ~cache:true in
    Piqi piqi
  else
    let obj = piqobj_of_json piqtype ast in
    match !default_piqtype with
      | Some x when x == piqtype ->
          (* return as Piqobj when default_piqtype is used *)
          Piqobj obj
      | _ ->
          Typed_piqobj obj


let load_piq_json_obj (user_piqtype: T.piqtype option) json_parser :obj =
  let ast = read_json_ast json_parser in
  (* check typenames, as Json parser doesn't do it unlike the Piq parser *)
  let check = true in
  match ast with
    | `Assoc [ "_piqtype", `String typename ] ->
        (* :piqtype <typename> *)
        process_default_piqtype typename ~check;
        Piqtype typename
    | `Assoc [ "_piqtype", _ ] ->
        error ast "invalid piqtype specification"
    | `Assoc [ "_piqi", ((`Assoc _) as json_ast) ] ->
        (* :piqi <typename> *)
        (* NOTE: caching the loaded module *)
        let piqi = piqi_of_json json_ast ~cache:true in
        Piqi piqi
    | `Assoc [ "_piqi", _ ] ->
        error ast "invalid piqi specification"
    | `Assoc [ "_piqtype", `String typename;
               "_piqobj", ast ] ->
        let piqtype = find_piqtype typename ~check in
        let obj = piqobj_of_json piqtype ast in
        Typed_piqobj obj
    | `Assoc (("_piqtype", _ )::_) ->
        error ast "invalid type object specification"
    | _ ->
        let piqtype = get_current_piqtype user_piqtype ast in
        load_json_common piqtype ast


let load_json_obj (piqtype: T.piqtype) json_parser :obj =
  let ast = read_json_ast json_parser in
  load_json_common piqtype ast


(*
 * XML reading and writing
 *)

let piqobj_of_xml_ref piqtype ref =
  let xml = Piqi_objstore.get ref in
  Piqobj_of_xml.parse_obj piqtype xml


let piqi_of_xml xml =
  let piqtype = !Piqi.piqi_spec_def in
  let wire_parser = T.parse_piqi in

  (* don't resolve defaults when reading xml *)
  let piqobj =
    C.with_resolve_defaults false (Piqobj_of_xml.parse_obj piqtype) xml
  in
  let piqi = Piqi.mlobj_of_piqobj wire_parser piqobj in

  (* set the default field resolver to xml *)
  Piqi.piqobj_of_ref := piqobj_of_xml_ref;

  Piqi.process_piqi piqi ~cache:true;
  piqi


let piqi_to_xml piqi =
  let piqobj = piqi_to_piqobj piqi in
  Piqobj_to_xml.gen_obj piqobj


let gen_xml (obj :obj) :Piqi_xml.xml =
  match obj with
    | Typed_piqobj obj | Piqobj obj ->
        Piqobj_to_xml.gen_obj obj
    | Piqi piqi ->
        (* output Piqi spec itself if we are converting .piqi *)
        piqi_to_xml piqi
    | Piqtype _ ->
        (* XXX *)
        assert false (* type hints are not supported by xml encoding *)


let write_xml ch (obj:obj) =
  let xml = gen_xml obj in
  Piqi_xml.xml_to_channel ch xml;
  (* XXX: add a newline for better readability *)
  Pervasives.output_char ch '\n'


let read_xml_ast xml_parser :Piqi_xml.xml =
  let res = Piqi_xml.read_xml_obj xml_parser in
  match res with
    | Some ast -> ast
    | None -> raise EOF


let load_xml_obj (piqtype: T.piqtype) xml_parser :obj =
  let ast = read_xml_ast xml_parser in
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = piqi_of_xml ast in
    Piqi piqi
  else
    let obj = Piqobj_of_xml.parse_obj piqtype ast in
    Typed_piqobj obj

