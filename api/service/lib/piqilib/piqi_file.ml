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


let chop_extension fname =
  try Filename.chop_extension fname
  with _ -> fname


let chop_all_extensions fname =
  let start =
    try (String.rindex fname '/') + 1
    with Not_found -> 0
  in
  try 
    let i = String.index_from fname start '.' in
    String.sub fname 0 i
  with Not_found -> fname



(* basename + chop all extensions *)
let basename filename =
  let basename = Filename.basename filename in
  chop_all_extensions basename


let dirname = Filename.dirname


let concat = Filename.concat


let get_extension s =
  try
    let pos = String.rindex s '.' in
    String.sub s (pos + 1) (String.length s - pos - 1)
  with
    Not_found -> ""


(* reverts slashes on Windows *)
let make_os_path name =
  match Sys.os_type with
    | "Win32" ->
        string_subst_char name '/' '\\'
    | _ -> name


(* find piqi file in search paths given its (relative) splitted name *)
let find_piqi_file modname =
  let name = make_os_path modname in (* revert slashes on Windows *)
  let found_dir = ref "" and found_name = ref "" in
  let check_file dir ext =
    let name = name ^ ext in
    let file_name = Filename.concat dir name in
    let res = Sys.file_exists file_name in
    if res then (found_dir := dir; found_name := name);
    res
  in
  if List.exists (fun dir ->
      if check_file dir ".piqi"
      then true
      else check_file dir ".proto.piqi") !Piqi_config.paths
  then
    !found_dir, !found_name
  else
    (trace "piqi file is not found in path: %s\n" (quote name);
     raise Not_found
    )


let find_piqi modname =
  (* NOTE: now supporting only local namespace *)
  let dir, fname = find_piqi_file modname in
  Filename.concat dir fname

