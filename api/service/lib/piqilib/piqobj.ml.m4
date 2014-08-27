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

(* XXX: replace this handwritten file by "piqobj.piqi" after boostrap stage. *)

(*
m4 macro definitions:

changequote(<<,>>)dnl

define(DEFTYPES, <<
  sig
    $1
  end =
  struct
    $1
  end
>>)

define(DEFRECORD, <<$1:
  sig
    type t =
      {$2
      }
  end =
  struct
    type t =
      {$2
      }
  end
>>)

*)


module rec Types: DEFTYPES(
  <<
    type record = Record.t
    type variant = Variant.t
    type enum = Variant.t
    type alias = Alias.t
    type list = List.t

    type field = Field.t
    type option = Option.t
    type any = Any.t

    type piqdef =
      [ `record of record
      | `variant of variant
      | `enum of enum
      | `alias of alias
      | `list of list ]

    type obj =
      [ piqdef
        (* built-in types *)
      | `int of int64 (* XXX: use big_int for internal representation? *)
      | `uint of int64
      | `float of float
      | `bool of bool
      | `string of string
      | `binary of string
      | `text of string

      | `word of string
      | `any of any ]
  >>)
and DEFRECORD(Record,
  <<
    mutable piqtype : Piqi_piqi.record;
    mutable field : Types.field list;
  >>)
and DEFRECORD(Field,
  <<
    mutable piqtype : Piqi_piqi.field;
    mutable obj: Types.obj option;
  >>)
and DEFRECORD(Variant,
  <<
    mutable piqtype : Piqi_piqi.variant;
    mutable option : Types.option;
  >>)
and DEFRECORD(Option,
  <<
    mutable piqtype : Piqi_piqi.option;
    mutable obj: Types.obj option; (* None for named options, i.e. constants *)
  >>)
and DEFRECORD(List,
  <<
    mutable piqtype : Piqi_piqi.piqlist;
    mutable obj: Types.obj list;
  >>)
and DEFRECORD(Alias,
  <<
    mutable piqtype : Piqi_piqi.alias;
    mutable obj: Types.obj;
  >>)
and DEFRECORD(Any,
  <<
    mutable any : Piqi_piqi.any;
    (* it is not possible to convert "any" to object at any given time, some
       typing hints may be required to do that which causes the delay, hence
       defining it as "optional" *)
    mutable obj: Types.obj option;
  >>)

include Types

(*
vim:ft=ocaml
*)
