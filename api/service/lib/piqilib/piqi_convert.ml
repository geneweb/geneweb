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
 * XML - JSON - Protocol Buffers data conversion
 *)

module C = Piqi_common  
open C


let _ = Piqilib.init ()


let init () =
  (* XXX: this is necessary when we convert to/from json, but now calling it
   * regardless of whether we actually need it *)
  Piqi_json.init ()


let find_piqtype typename =
  if not (Piqi_name.is_valid_typename typename)
  then
    piqi_error ("invalid type name: " ^ typename);

  if typename = "piqi" (* special case *)
  then !Piqi.piqi_lang_def (* return Piqi type from embedded self-definition *)
  else
    try Piqi_db.find_piqtype typename
    with Not_found ->
      piqi_error ("unknown type: " ^ typename)


(*
 * The converter:
 *)

let parse_piq_common get_next ~is_piqi_input =
  let rec aux () =
    let obj = get_next () in
    match obj with
      | Piq.Piqtype _ -> aux () (* skip default type *)
      | Piq.Piqi _ when not is_piqi_input -> aux () (* skip embedded piqi *)
      | Piq.Piqobj obj -> Piq.Typed_piqobj obj
      | _ -> obj (* Typed_piqobj or Piqi *)
  in aux ()


(* NOTE: parsers always return either Piqi or Typed_piqobj *)

let fname = "input" (* XXX *)


let parse_piq piqtype s ~is_piqi_input =
  let piq_parser = Piq_parser.init_from_string fname s in
  let get_next () = Piq.load_piq_obj (Some piqtype) piq_parser in
  let obj = parse_piq_common get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let gen_piq obj =
  let ast = Piq.gen_piq obj in
  Piq_gen.to_string ast


let parse_wire piqtype s ~is_piqi_input =
  let buf = Piqirun.IBuf.of_string s in
  let get_next () = Piq.load_wire_obj (Some piqtype) buf in
  let obj = parse_piq_common get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let gen_wire obj =
  let buf = Piq.gen_wire obj in
  Piqirun.to_string buf


let parse_json piqtype s =
  let json_parser = Piqi_json_parser.init_from_string ~fname s in
  let obj = Piq.load_json_obj piqtype json_parser in
  (* XXX: check eof? *)
  obj


let gen_json ?(pretty_print=true) obj =
  let json = Piq.gen_json obj in
  if pretty_print
  then
    Piqi_json_gen.pretty_to_string json
  else
    Piqi_json_gen.to_string json


let parse_pb piqtype s =
  let buf = Piqirun.init_from_string s in
  let obj = Piq.load_pb piqtype buf in
  (* XXX: check eof? *)
  obj


let gen_pb obj =
  let buf = Piq.gen_pb obj in
  Piqirun.to_string buf


let parse_xml piqtype s =
  let xml_parser = Piqi_xml.init_from_string ~fname s in
  let obj = Piq.load_xml_obj piqtype xml_parser in
  (* XXX: check eof? *)
  obj


let gen_xml ?pretty_print obj =
  let xml = Piq.gen_xml obj in
  Piqi_xml.xml_to_string xml ?pretty_print


let parse_obj piqtype input_format data =
  (* XXX *)
  let is_piqi_input = (piqtype == !Piqi.piqi_lang_def) in
  let piqobj =
    match input_format with
      | `piq  -> parse_piq piqtype data ~is_piqi_input
      | `json -> parse_json piqtype data
      | `pb -> parse_pb piqtype data
      | `xml -> parse_xml piqtype data
      (* XXX *)
      | `wire -> parse_wire piqtype data ~is_piqi_input
  in piqobj


let gen_obj ~pretty_print output_format piqobj =
  match output_format with
    | `piq  -> gen_piq piqobj
    | `json -> gen_json piqobj ~pretty_print
    | `pb -> gen_pb piqobj
    | `xml -> gen_xml piqobj ~pretty_print
    (* XXX *)
    | `wire -> gen_wire piqobj


type options =
  {
    mutable json_omit_null_fields : bool;
    mutable pretty_print : bool;
  }


let make_options
        ?(pretty_print=true)
        ?(json_omit_null_fields=true)
        () =
  {
    json_omit_null_fields = json_omit_null_fields;
    pretty_print = pretty_print;
  }


let set_options opts =
  Piqobj_to_json.omit_null_fields := opts.json_omit_null_fields;
  ()


let convert_piqtype ~opts piqtype input_format output_format data =

  (* apply some of the settings *)
  set_options opts;

  (* perform the conversion *)
  let piqobj =
    (* XXX: We need to resolve all defaults before converting to JSON or XML *)
    C.with_resolve_defaults
      (output_format = `json || output_format = `xml)
      (fun () -> parse_obj piqtype input_format data)
      ()
  in
  gen_obj output_format piqobj ~pretty_print:opts.pretty_print

