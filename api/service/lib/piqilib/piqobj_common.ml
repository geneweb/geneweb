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


let type_of (x:Piqobj.obj) :T.piqtype =
  (* XXX: built-in types should not be used at this point *)
  match x with
    | `int _ -> `int
    | `uint _ -> `int
    | `float _ -> `float
    | `bool _ -> `bool
    | `string _ -> `string
    | `binary _ -> `binary
    | `text _ -> `text
    | `word _ -> `word
    | `any _ -> `any
    (* custom types *)
    | `record x -> `record x.R#piqtype
    | `variant x -> `variant x.V#piqtype
    | `enum x -> `enum x.E#piqtype
    | `list x -> `list x.L#piqtype
    | `alias x -> `alias x.A#piqtype


let full_typename x =
  C.full_piqi_typename (type_of x)


