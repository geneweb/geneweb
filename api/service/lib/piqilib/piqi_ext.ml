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


(* This module contains functionality that Piqi module would contain, but if it
 * did, it would break Piqicc bootstrap that doesn't support advanced Piqi
 * functionality such as functions. *)

(* XXX: include this module from Piqi? *)


open Piqi_common
open Piqi


let get_functions modules =
  flatmap (fun x -> x.P#func) modules


(* expand all includes and, optionally, extensions and produce an expanded
 * version of the Piqi module *)
let expand_piqi ?(includes_only=false) piqi =
  let open P in
  let all_piqi = piqi.included_piqi in
  let orig_piqi = some_of piqi.original_piqi in

  (* create a new piqi module from the original piqi module *)
  let res_piqi =
    {
      (* XXX
      T.default_piqi () with

      modname = orig_piqi.modname;
      proto_package = orig_piqi.proto_package;
      *)
      orig_piqi with

      custom_field = [];
      includ = [];

      (* copy all definitions to the resulting module *)
      piqdef =
        if includes_only
        then get_piqdefs all_piqi
        else piqi.extended_piqdef;

      (* copy all extensions to the resulting module *)
      extend =
        if includes_only
        then get_extensions all_piqi
        else [];

      (* copy all imports to the resulting module *)
      import = get_imports all_piqi;

      (* copy all functions to the resulting module *)
      func = get_functions all_piqi;
    }
  in
  res_piqi

