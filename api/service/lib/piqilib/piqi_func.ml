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


let make_param_name func param_name =
  (* construct the type name as a concatentation of the function name and
   * -input|output|error *)
  let func_name = func.T.Func#name in
  let type_name = func_name ^ "-" ^ param_name in
  (* create a location reference for the newly constructed type name *)
  Piqloc.addrefret func_name type_name


let make_param_alias name x =
  let res =
    A#{
      T.default_alias () with

      name = name;
      typeref = `name x;
      is_func_param = true; (* mark the new alias as function parameter *)
    }
  in
  Piqloc.addrefret x res


let make_param_record name x =
  let res =
    R#{
      T.default_record () with

      name = name;
      field = x.T.Anonymous_record.field;
    }
  in
  let res = Piqi.copy_record res in (* preserve the original fields *)
  Piqloc.addrefret x res


let make_param_variant name x =
  let res =
    V#{
      T.default_variant () with

      name = name;
      option = x.T.Anonymous_variant.option;
    }
  in
  let res = Piqi.copy_variant res in (* preserve the original options *)
  Piqloc.addrefret x res


let make_param_list name x =
  let res =
    L#{
      T.default_piqlist () with

      name = name;
      typeref = x.T.Anonymous_list.typeref;
      (* XXX: what about wire-packed property? -- it is not defined for
       * anonymous list:
       * wire_packed = x.T.Anonymous_list.wire_packed;
       *)
    }
  in
  Piqloc.addrefret x res


(* convert function parameter to a type:
 *  - if the function parameter is a name, convert it to correspondent alias
 *  - if the function parameter is an anonymous record, convert it to
 *    correspondent record
 *  - do the same for anonymous variants, enums and lists
 *)
let resolve_param func param_name param =
  let type_name = make_param_name func param_name in
  let def =
    match param with
      | `name x ->
          (* make an alias from name reference *)
          `alias (make_param_alias type_name x)
      | `record x ->
          `record (make_param_record type_name x)
      | `variant x ->
          `variant (make_param_variant type_name x)
      | `enum x ->
          `enum (make_param_variant type_name x)
      | `list x ->
          `list (make_param_list type_name x)
  in
  Piqloc.addref param def;
  def


(* ughh. this is ugly *)
let resolve_param func param_name param =
  match param with
    | None -> None
    | Some param ->
        let res = resolve_param func param_name param in
        Some res


let process_func f =
  let open T.Func in
  begin
    Piqi.check_name f.name;
    let i = resolve_param f "input" f.input
    and o = resolve_param f "output" f.output
    and e = resolve_param f "error" f.error
    in T.Func#{
      f with
      resolved_input = i;
      resolved_output = o;
      resolved_error = e;
    }
  end


let check_dup_names funs =
  let open P in
  let names = List.map (fun x -> x.T.Func#name) funs in
  Piqi.check_dup_names "function" names;
  ()


let get_func_defs f =
  let open T.Func in
  let get_param = function
    | None -> []
    | Some x -> [ (x :> T.piqdef) ]
  in
  List.concat [
    get_param f.resolved_input;
    get_param f.resolved_output;
    get_param f.resolved_error;
  ]


let get_function_defs (piqi :T.piqi) =
  let open P in
  (* get all functions from this module and included modules *)
  let funs = Piqi_ext.get_functions piqi.included_piqi in

  (* check for duplicate function names *)
  check_dup_names funs;

  (* process functions and create a local copy of them *)
  let resolved_funs = List.map process_func funs in

  (* add function type definitions to Piqi resolved defs *)
  piqi.resolved_func <- resolved_funs;

  (* returned definitions derived from function parameters *)
  let defs = flatmap get_func_defs resolved_funs in
  defs


(* boot code *)
let init () =
  Piqi.get_function_defs := get_function_defs

