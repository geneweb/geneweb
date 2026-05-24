(* place_cache.ml *)

open Def
open Geneweb_db.Driver

let magic = "GwPl0002"

type t = {
  persons : (string, (istr gen_pers_event_name * iper) list) Hashtbl.t;
  families : (string, (istr gen_fam_event_name * ifam) list) Hashtbl.t;
}

let cache_path bdir =
  let dir = Filename.concat bdir "caches" in
  let dir = Filename.concat dir "place_pps" in
  Filename.concat dir "all.cache"

let meta_path bdir =
  let dir = Filename.concat bdir "caches" in
  let dir = Filename.concat dir "place_pps" in
  Filename.concat dir "all.meta"

let cache_is_valid bdir _base =
  let path = cache_path bdir in
  if not (Sys.file_exists path) then false
  else
    let patches = Filename.concat bdir "patches" in
    if not (Sys.file_exists patches) then true
    else
      let cache_mtime = (Unix.stat path).Unix.st_mtime in
      let patches_mtime = (Unix.stat patches).Unix.st_mtime in
      cache_mtime > patches_mtime

let re_parens = Str.regexp " ?(\\([^)]*\\))"

let normalize_place_parens s =
  if String.contains s '(' then Str.global_replace re_parens ", \\1" s else s

(* --- Construction --- *)

let build _conf base =
  let nb = nb_of_persons base in
  let nf = nb_of_families base in
  let persons = Hashtbl.create (max 1024 (nb / 10)) in
  let families = Hashtbl.create (max 1024 (nf / 10)) in
  Geneweb_db.Collection.iter
    (fun iper ->
      let p = poi base iper in
      List.iter
        (fun evt ->
          let place = sou base evt.epers_place in
          if place <> "" then begin
            let place = normalize_place_parens place in
            let lst = try Hashtbl.find persons place with Not_found -> [] in
            Hashtbl.replace persons place ((evt.epers_name, iper) :: lst)
          end)
        (get_pevents p))
    (ipers base);
  Geneweb_db.Collection.iter
    (fun ifam ->
      let f = foi base ifam in
      List.iter
        (fun evt ->
          let place = sou base evt.efam_place in
          if place <> "" then begin
            let place = normalize_place_parens place in
            let lst = try Hashtbl.find families place with Not_found -> [] in
            Hashtbl.replace families place ((evt.efam_name, ifam) :: lst)
          end)
        (get_fevents f))
    (ifams base);
  { persons; families }

(* --- Persistence --- *)

let write bdir cache =
  let path = cache_path bdir in
  Mutil.mkdir_p (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = Secure.open_out_bin tmp in
  output_string oc magic;
  Marshal.to_channel oc cache [];
  close_out oc;
  (* atomic rename: remove first for Windows compatibility *)
  (try Sys.remove path with Sys_error _ -> ());
  Sys.rename tmp path

let read bdir =
  let path = cache_path bdir in
  let ic = Secure.open_in_bin path in
  let buf = Bytes.create (String.length magic) in
  really_input ic buf 0 (String.length magic);
  if Bytes.to_string buf <> magic then begin
    close_in ic;
    failwith "place_cache: magic mismatch"
  end;
  let cache : t = Marshal.from_channel ic in
  close_in ic;
  cache

(* --- Synchro_patches cursor --- *)

let read_meta bdir =
  let path = meta_path bdir in
  if not (Sys.file_exists path) then ""
  else
    let ic = Secure.open_in_bin path in
    let ts = try input_line ic with End_of_file -> "" in
    close_in ic;
    ts

let write_meta bdir ts =
  let path = meta_path bdir in
  let oc = Secure.open_out_bin path in
  output_string oc ts;
  close_out oc

let latest_synchro_ts _base_dir base =
  let sp = input_synchro (bdir base) in
  match sp.synch_list with (ts, _, _) :: _ -> ts | [] -> ""

let pending_entries base_dir base =
  let last_ts = read_meta base_dir in
  let sp = input_synchro (bdir base) in
  List.filter (fun (ts, _, _) -> ts > last_ts) sp.synch_list

let update_iper cache base iper =
  (* Remove all existing contributions for this iper *)
  Hashtbl.iter
    (fun place lst ->
      let lst' = List.filter (fun (_, i) -> not (Iper.equal i iper)) lst in
      if lst' = [] then Hashtbl.remove cache.persons place
      else Hashtbl.replace cache.persons place lst')
    cache.persons;
  (* Re-index only if iper exists in base (not deleted) *)
  if iper_exists base iper then
    let p = poi base iper in
    List.iter
      (fun evt ->
        let place = sou base evt.epers_place in
        if place <> "" then begin
          let place = normalize_place_parens place in
          let lst =
            try Hashtbl.find cache.persons place with Not_found -> []
          in
          Hashtbl.replace cache.persons place ((evt.epers_name, iper) :: lst)
        end)
      (get_pevents p)

let update_ifam cache base ifam =
  (* Remove all existing contributions for this ifam *)
  Hashtbl.iter
    (fun place lst ->
      let lst' = List.filter (fun (_, i) -> not (Ifam.equal i ifam)) lst in
      if lst' = [] then Hashtbl.remove cache.families place
      else Hashtbl.replace cache.families place lst')
    cache.families;
  (* Re-index only if ifam exists in base (not deleted) *)
  if ifam_exists base ifam then
    let f = foi base ifam in
    List.iter
      (fun evt ->
        let place = sou base evt.efam_place in
        if place <> "" then begin
          let place = normalize_place_parens place in
          let lst =
            try Hashtbl.find cache.families place with Not_found -> []
          in
          Hashtbl.replace cache.families place ((evt.efam_name, ifam) :: lst)
        end)
      (get_fevents f)

let apply_delta cache base entries =
  let latest_ts = ref "" in
  List.iter
    (fun (ts, ipers, ifams) ->
      if ts > !latest_ts then latest_ts := ts;
      List.iter
        (fun i -> update_iper cache base (Iper.of_string (string_of_int i)))
        ipers;
      List.iter
        (fun i -> update_ifam cache base (Ifam.of_string (string_of_int i)))
        ifams)
    entries;
  !latest_ts

(* --- Entry point --- *)

let get_or_build bdir conf base =
  if cache_is_valid bdir base then begin
    let cache = read bdir in
    let entries = pending_entries bdir base in
    if entries = [] then cache
    else begin
      let latest_ts = apply_delta cache base entries in
      write bdir cache;
      write_meta bdir latest_ts;
      cache
    end
  end
  else begin
    let cache = build conf base in
    write bdir cache;
    write_meta bdir (latest_synchro_ts bdir base);
    cache
  end
