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


(* idtable implemented as map: string -> 'a *)
module Idtable =
  struct
    module M = Map.Make(String)

    type 'a t = 'a M.t

    let empty = M.empty

    let add idtable name entry =
      M.add name entry idtable

    let find idtable name =
      M.find name idtable 

    let remove idtable name =
      M.remove name idtable

    let mem idtable name =
      M.mem name idtable 
  end


(* the map of loaded piqi modules: modname -> piqi *)
module Piqitable = Idtable
let loaded_map = ref Piqitable.empty


let add_piqi piqi =
  let modname = some_of piqi.P#modname in
  trace "piqi_db: caching piqi module \"%s\"\n" modname;

  (* check for name override/conflict *)
  (* XXX: prohibit override? *)
  (
    try
      let prev_piqi = Piqitable.find !loaded_map modname in
      warning piqi ("redefinition of module " ^ quote modname);
      warning prev_piqi "previous definition is here";
    with Not_found -> ()
  );

  loaded_map := Piqitable.add !loaded_map modname piqi


let remove_piqi modname =
  loaded_map := Piqitable.remove !loaded_map modname


(* find already loaded module by name *)
let find_piqi modname :T.piqi =
  Piqitable.find !loaded_map modname


let find_local_piqdef piqi name =
  List.find (fun x -> name = piqdef_name x) piqi.P#resolved_piqdef


(* To be set up later to Piqi.load_piqi_file; we do this since OCaml doesn't
 * support recursive toplevel modules *)
let piqi_loader :(?modname:string -> string -> T.piqi) option ref = ref None


let load_piqi_module modname =
  trace "piqi_db: loading module: %s\n" modname;
  trace_enter ();
  let fname = Piqi_file.find_piqi modname in (* can raise Not_found *)
  (* XXX
  (* reset Config.noboot option, we can't use this option for
   * dependent types and modules *)
  let saved_noboot = !Config.noboot in
  Config.noboot := false;
  *)
  let load_piqi_file = some_of !piqi_loader in
  let piqi = load_piqi_file ~modname fname in
  (*
  Config.noboot := saved_noboot;
  *)
  trace_leave ();
  piqi


let find_load_piqi_module ~auto_load_piqi modname =
  match modname with
    | None -> (* built-in or local type *)
        (* NOTE: local types are not supported yet *)
        some_of !boot_piqi
    | Some modname ->
        (* check if the module is already loaded, and return it right away *)
        try find_piqi modname
        with Not_found when auto_load_piqi ->
          (* XXX: handle load errors *)
          load_piqi_module modname


let find_piqdef ?(auto_load_piqi=true) name =
  (* XXX: check global type name before loading? *)
  trace "looking for type: %s\n" name;
  let modname, typename = Piqi_name.split_name name in
  let piqi = find_load_piqi_module modname ~auto_load_piqi in
  find_local_piqdef piqi typename


let find_piqtype name =
  let def = find_piqdef name in
  (def: T.piqdef :> T.piqtype)


let try_find_piqtype name =
  try
    let def = find_piqdef name ~auto_load_piqi:false in
    Some (def: T.piqdef :> T.piqtype)
  with
    Not_found -> None

