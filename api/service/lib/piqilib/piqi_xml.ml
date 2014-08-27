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


(* XML format parsing and generation using Xmlm library:
 *
 *      http://erratique.ch/software/xmlm
 *
 * Details about how Xmlm parses XML are available here:
 *
 *      http://erratique.ch/software/xmlm/doc/Xmlm
 *)


module C = Piqi_common
open C


type xml =
  [ `Elem of xml_elem
  | `Data of string
  ]
and xml_elem = string * xml list (* (name, [xml]) *)


type xml_parser =
  {
    input : Xmlm.input;
    fname : string; (* name of the file *)
  }


let init_xml_parser ?(fname = "input") source :xml_parser =
  (* don't strip whitespace in CDATA and expect UTF-8 input (no other encodings
   * are supported by Piqi)
   *
   * NOTE: according to Xmlm documentation, even when we specify ~strip:false,
   * "all kinds of line ends are translated to the newline character (U+000A)"
   *
   * NOTE: we use a custom whitespace stripper below that doesn't strip leading
   * and trailing whitespace in text nodes.
   *)
  let input = Xmlm.make_input source ~enc:(Some `UTF_8) ~strip:false in
  {
    input = input;
    fname = fname;
  }


let init_from_channel ?fname ch =
  let source = `Channel ch in
  init_xml_parser source ?fname


let init_from_string ?fname s =
  let source = `String (0, s) in
  init_xml_parser source ?fname


(**)

let open_xml fname =
  let ch = Piqi_main.open_input fname in
  init_from_channel ch ~fname


(* XML input *)

(* custom whitespace stripper, that srips only formatting whitespace and leaves
 * text nodes untouched *)
let strip_whitespace (l :xml list) =
  match l with
    | [(`Data _)] -> l
    | _ ->
      (* there is at least one element in the list; stripping all the data around
       * and between the elements *)
      List.filter (function `Elem _ -> true | `Data _ -> false) l


let do_read_xml_obj xml_parser :xml =
  let make_loc (line, col) =
    (xml_parser.fname, line, col)
  in
  (* below are cusomized versions of Xmlm.input_tree and Xmlm.input_doc_tree
   * functions that capture accurate information about location of elements and
   * data in the input stream *)
  let input_tree ~el ~data i =
    let rec aux tags context =
      let pos = Xmlm.pos i in
      match Xmlm.input i with
        | `El_start tag ->
            aux ((pos, tag) :: tags) ([] :: context)
        | `El_end -> 
            begin match tags, context with
            | (pos, tag) :: tags', childs :: context' ->
                let el = el pos tag (List.rev childs) in 
                begin match context' with
                | parent :: context'' -> aux tags' ((el :: parent) :: context'')
                | [] -> el
                end
            | _ -> assert false
            end
        | `Data d ->
            begin match context with
            | childs :: context' -> aux tags (((data pos d) :: childs) :: context')
            | [] -> assert false
            end
        | `Dtd _ -> assert false
    in 
    aux [] []
  in
  let input_doc_tree ~el ~data i =
    let pos = Xmlm.pos i in
    match Xmlm.input i with
     | `Dtd d -> d, input_tree ~el ~data i
     | _ ->
         error_at (make_loc pos) "invalid XML header"
  in
  let el pos tag contents =
    let (ns, name), attr = tag in
    let contents = strip_whitespace contents in
    let loc = make_loc pos in

    (* check that there is no namespace and no attributes *)
    if ns <> ""
    then error_at loc "namespaces are not allowed in XML element names";

    if attr <> []
    then error_at loc "attributes are not allowed in XML elements";

    let xml_elem = (name, contents) in
    let res = `Elem xml_elem in

    (* add information about term locations to the location database *)
    Piqloc.addloc loc name;
    Piqloc.addloc loc xml_elem;
    Piqloc.addloc loc res;

    res
  in
  let data pos d =
    let res = `Data d in
    (* add information about term locations to the location database *)
    let loc = make_loc pos in
    Piqloc.addloc loc d;
    Piqloc.addloc loc res;

    res
  in
  try
    let _dtd, xml = input_doc_tree ~el ~data xml_parser.input in
    xml
  with
    Xmlm.Error (pos, err) ->
      let loc = make_loc pos in
      let errstr = Xmlm.error_message err in
      error_at loc errstr


let read_xml_obj (xml_parser :xml_parser) :xml option =
  let is_eoi =
    try Xmlm.eoi xml_parser.input
    with
      | Xmlm.Error (_pos, `Unexpected_eoi) ->
          (* raised on a completely empty input *)
          true
      | Xmlm.Error ((line, col), err) ->
          let loc = xml_parser.fname, line, col in
          let errstr = Xmlm.error_message err in
          error_at loc errstr
  in
  if is_eoi
  then None
  else 
    let xml = do_read_xml_obj xml_parser in
    Some xml


(* XML output *)

let make_output ?(pretty_print=true) dest =
  (* Use 2-space indentation and output a newline character after the root
   * element.
   *
   * NOTE: we use a modified version of Xmlm library that doesn't insert
   * indentation around text nodes, because indentation leads to extra
   * whitespace and this is not what we want *)
  let indent =
    if pretty_print
    then Some 2
    else None
  in
  Xmlm.make_output dest ~indent ~nl:true


let gen_xml ?pretty_print dest (xml :xml) =
  let frag = function (* xml to Xmlm.frag converter *)
    | `Data x -> `Data x
    | `Elem (name, contents) ->
        let tag = ("", name), [] in (* no namespace, no attributes *)
        `El (tag, contents)
  in
  let output = make_output ?pretty_print dest in
  let dtd = None in
  Xmlm.output_doc_tree frag output (dtd, xml)


let xml_to_buffer ?pretty_print buf xml =
  let dest = `Buffer buf in
  gen_xml ?pretty_print dest xml


let xml_to_channel ?pretty_print ch xml =
  let dest = `Channel ch in
  gen_xml ?pretty_print dest xml


let xml_to_string ?pretty_print xml =
  let buf = Buffer.create 256 in
  xml_to_buffer ?pretty_print buf xml;
  Buffer.contents buf


let xml_of_string s :xml =
  let xml_parser = init_from_string s in
  match read_xml_obj xml_parser with
    | Some ast -> ast
    | None -> assert false

