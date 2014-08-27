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
open Iolist 


(* split (utf8) string into individual lines treating '\n' as a separator *)
let split_text s =
  let rec aux len i accu =
    if i < 0
    then
      let res = String.sub s 0 len in
      res::accu
    else
      if s.[i] = '\n'
      then
        let res = String.sub s (i + 1) len in
        aux 0 (i - 1) (res::accu)
      else
        aux (len + 1) (i - 1) accu
  in
  aux 0 (String.length s - 1) []


let make_text_line s =
  if s = ""
  then ios "#"
  else ios "# " ^^ ios s


(* NOTE: list is not empty *)
let print_text l =
  let l = List.fold_left
    (fun accu x -> eol :: (make_text_line x) :: accu) [] l
  in
  iol (List.rev l)


let rec is_multiline = function
  | Ios s -> String.contains s '\n'
  | Iol l -> List.fold_left (fun accu x -> accu || is_multiline x) false l
  | Iob '\n' -> true
  | Indent | Unindent | Eol -> true
  | _ -> false


let uint64_to_string x =
  (* XXX: printing large unsigned uint values in hex *)
  if Int64.compare x 0L >= 0
  then Printf.sprintf "%Lu" x
  else Printf.sprintf "0x%Lx" x


(* This method for printing floats is borrowed from Martin Jambon's Yojson
 * library, see piqi_json_gen.ml for details. *)
(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
	  '0'..'9' | '-' -> ()
	| _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to 
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)

let write_float ob x =
  let s1 = Printf.sprintf "%.16g" x in
  let s =
    if float_of_string s1 = x then s1
    else Printf.sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

(*
let write_float_fast ob x =
  let s = Printf.sprintf "%.17g" x in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float = write_float_fast
*)

let string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


(* XXX: providing custom version since Pervasives.string_of_float will add
 * trailing "." to the literal *)
let format_float x =
  match Pervasives.classify_float x with
    | FP_nan -> "0.nan"
    | FP_infinite  ->   (** Number is positive or negative infinity *)
        if x = Pervasives.infinity
        then "0.inf"
        else "-0.inf"
    | FP_normal         (** Normal number, none of the below *)
    | FP_zero           (** Number is 0.0 or -0.0 *)
    | FP_subnormal ->   (** Number very close to 0.0, has reduced precision *)
        string_of_float x


(* Old version without using pretty-printing library:
let print_ast (x:T.ast) =
  let rec aux = function
    | `int x -> ios (Int64.to_string x)
    | `uint x -> ios (uint64_to_string x)
    | `float x -> ios (format_float x)
    | `bool true -> ios "true"
    | `bool false -> ios "false"
    | `utf8_string s when !Config.pp_mode ->
        (* in pretty-print mode the literal represents the original string *)
        ioq s
    | `ascii_string s | `utf8_string s ->
        ioq (Piq_lexer.escape_string s)
    | `binary s ->
        ioq (Piq_lexer.escape_binary s)
    | `word s -> ios s
    | `text s -> print_text (split_text s)
    | `name s -> ios "." ^^ ios s
    | `typename s -> ios ":" ^^ ios s
    | `named {T.Named.name = n; T.Named.value = v} ->
        iol [
          ios "." ^^ ios n;
          gen_inner_ast v;
        ]
    | `typed {T.Typed.typename = n; T.Typed.value = v} ->
        iol [
          ios ":" ^^ ios n;
          gen_inner_ast (some_of v.T.Any#ast);
        ]
    | `list l ->
        let l = iod "\n" (List.map aux l) in
        if is_multiline l
        then
          iol [
            ios "["; indent;
            l; unindent; eol;
            ios "]";
          ]
        else 
          iol [ ios "[ "; l; ios " ]" ]
    | `control l ->
        iol [
          ios "("; indent;
          iod "\n" (List.map aux l); unindent; eol;
          ios ")";
        ]
  and gen_inner_ast x =
    match x with
      | `named _ | `name _ | `typed _ ->
          aux x
          (*
          (* wrap inner pair in parenthesis *)
          iol [ios "("; aux x; ios ")"]
          *)
      | _ -> ios " " ^^ aux x
  in
  aux x ^^ eol ^^ eol
*)


(*
 * Pretty-printing
 *)

module Fmt = Easy_format


let common_list =
  Fmt#{
    list with
    indent_body = 4;
  }

let atom_list =
  Fmt#{
    common_list with
    wrap_body = `Always_wrap;
  }

let single_elem_list =
  Fmt#{
    common_list with
    wrap_body = `Always_wrap;
  }

let multiple_elem_list =
  Fmt#{
    common_list with
    wrap_body = `Force_breaks;
  }

let control_list =
  Fmt#{
    common_list with
    space_after_opening = false;
    space_before_closing = false;
  }

let raw_list =
  Fmt#{
    common_list with
    wrap_body = `Force_breaks;
  }

let raw_top_list =
  Fmt#{
    raw_list with
    indent_body = 0;
  }


let make_atom x =
  Fmt.Atom (x, Fmt.atom)


let is_atom = function
  | Fmt.Atom _ -> true
  | _ -> false


let rec has_list = function
  | Fmt.List _ -> true
  | Fmt.Label ((label, _), node) ->
      if has_list label
      then true
      else has_list node
  | _ -> false


let make_list l =
  let fmt =
    match l with
      | [] ->
          single_elem_list
      | [x] ->
          if has_list x
          then multiple_elem_list
          else single_elem_list
      | _ ->
          if List.for_all is_atom l
          then atom_list
          else multiple_elem_list
  in
  Fmt.List (("[", "", "]", fmt), l)


let make_control x =
  Fmt.List (("(", "", ")", control_list), x)


let make_raw_list ~top x =
  (* no opening, closing; break after each item *)
  let list_fmt =
    if top
    then raw_top_list
    else raw_list
  in
  Fmt.List (("", "", "", list_fmt), x)


let make_divided_list x =
  (* no opening, closing; break after each item *)
  Fmt.List (("", "\n", "", raw_list), x)


let make_label label node =
  Fmt.Label ((label, Fmt.label), node)


let quote s = "\"" ^ s ^ "\""


let format_text_line s =
  let line =
    if s = ""
    then "#"
    else "# " ^ s
  in make_atom line


(* TODO: this method of printing produces extra empty lines -- before and
 * after the text *)
(* NOTE: l is not empty *)
let format_text ?(top=false) l =
  make_raw_list (List.map format_text_line l) ~top


let format_ast (x:T.ast) =
  let rec aux ?(label="") ?(top=false) = function
    | `int x -> make_atom (Int64.to_string x)
    | `uint x -> make_atom (uint64_to_string x)
    | `float x -> make_atom (format_float x)
    | `bool true -> make_atom "true"
    | `bool false -> make_atom "false"
    | `ascii_string s | `utf8_string s ->
        make_atom (quote (Piq_lexer.escape_string s))
    | `binary s ->
        make_atom (quote (Piq_lexer.escape_binary s))
    | `raw_binary s when !Config.pp_mode ->
        (* in pretty-print mode, the literal represents the original string *)
        make_atom (quote s)
    | `raw_binary s ->
        (* This literal can't be read back reliably after printing, and it
         * doesn't come from Piq, but we still need to print it somehow -- in
         * case if it is present. *)
        (* XXX: printing it is as binary for now, but may try to print it as
         * utf8 string if it does represet a valid string. *)
        make_atom (quote (Piq_lexer.escape_binary s))
    | `raw_word s (* used in pretty-printing mode and in some other cases *)
    | `word s -> make_atom s
    | `text s -> format_text (split_text s) ~top
    | `name s -> make_atom (label ^ "." ^ s)
    | `typename s -> make_atom (label ^ ":" ^ s)
    | `named {T.Named.name = n; T.Named.value = v} ->
        let label = label ^ "." ^ n in
        format_inner_ast label v
    | `typed {T.Typed.typename = n; T.Typed.value = v} ->
        let label = label ^ ":" ^ n in
        format_inner_ast label (some_of v.T.Any#ast)
    | `list [] ->
        make_atom "[]"
    | `list l ->
        make_list (List.map aux l)
    | `control l ->
        make_control (List.map aux l)
  and format_inner_ast label x =
    match x with
      | `named _ | `name _ | `typed _ ->
          (* continue building label *)
          aux ~label x
      | _ ->
          (* finshed building label *)
          make_label (make_atom label) (aux x)
  in
  aux x ~top:true


let to_buffer ?(nl = true) buf x =
  Fmt.Pretty.to_buffer buf (format_ast x);
  if nl then Buffer.add_char buf '\n'
    
    
let to_string ?(nl = true) x =
  let buf = Buffer.create 256 in
  to_buffer ~nl buf x;
  Buffer.contents buf


let to_channel ch x =
  Fmt.Pretty.to_channel ch (format_ast x);
  output_char ch '\n' (* make sure that text file ends with a newline *)

