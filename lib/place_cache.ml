(* place_cache.ml *)

open Def
open Geneweb_db.Driver

let magic = "GwPl0003"

type t = {
  persons : (string, (istr gen_pers_event_name * iper) list) Hashtbl.t;
  families : (string, (istr gen_fam_event_name * ifam) list) Hashtbl.t;
  iper_places : (iper, string list) Hashtbl.t;
      (* reverse index: the distinct places [iper] currently contributes to *)
  ifam_places : (ifam, string list) Hashtbl.t;
      (* reverse index: the distinct places [ifam] currently contributes to *)
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

(* --- Indexing primitives ---

   Each add/remove keeps the forward index ([persons]/[families]) and the
   reverse index ([iper_places]/[ifam_places]) in sync. The reverse index lets
   [update_iper]/[update_ifam] strip an id in time proportional to that id's own
   footprint, instead of scanning the whole place table. *)

let add_iper cache base iper =
  let p = poi base iper in
  let places =
    List.fold_left
      (fun places evt ->
        let place = sou base evt.epers_place in
        if place = "" then places
        else begin
          let place = normalize_place_parens place in
          let lst =
            try Hashtbl.find cache.persons place with Not_found -> []
          in
          Hashtbl.replace cache.persons place ((evt.epers_name, iper) :: lst);
          if List.mem place places then places else place :: places
        end)
      [] (get_pevents p)
  in
  if places <> [] then Hashtbl.replace cache.iper_places iper places

let remove_iper cache iper =
  match Hashtbl.find_opt cache.iper_places iper with
  | None -> ()
  | Some places ->
      List.iter
        (fun place ->
          match Hashtbl.find_opt cache.persons place with
          | None -> ()
          | Some lst ->
              let lst' =
                List.filter (fun (_, i) -> not (Iper.equal i iper)) lst
              in
              if lst' = [] then Hashtbl.remove cache.persons place
              else Hashtbl.replace cache.persons place lst')
        places;
      Hashtbl.remove cache.iper_places iper

let add_ifam cache base ifam =
  let f = foi base ifam in
  let places =
    List.fold_left
      (fun places evt ->
        let place = sou base evt.efam_place in
        if place = "" then places
        else begin
          let place = normalize_place_parens place in
          let lst =
            try Hashtbl.find cache.families place with Not_found -> []
          in
          Hashtbl.replace cache.families place ((evt.efam_name, ifam) :: lst);
          if List.mem place places then places else place :: places
        end)
      [] (get_fevents f)
  in
  if places <> [] then Hashtbl.replace cache.ifam_places ifam places

let remove_ifam cache ifam =
  match Hashtbl.find_opt cache.ifam_places ifam with
  | None -> ()
  | Some places ->
      List.iter
        (fun place ->
          match Hashtbl.find_opt cache.families place with
          | None -> ()
          | Some lst ->
              let lst' =
                List.filter (fun (_, i) -> not (Ifam.equal i ifam)) lst
              in
              if lst' = [] then Hashtbl.remove cache.families place
              else Hashtbl.replace cache.families place lst')
        places;
      Hashtbl.remove cache.ifam_places ifam

(* --- Construction --- *)

let build _conf base =
  let nb = nb_of_persons base in
  let nf = nb_of_families base in
  let cache =
    {
      persons = Hashtbl.create (max 1024 (nb / 10));
      families = Hashtbl.create (max 1024 (nf / 10));
      iper_places = Hashtbl.create (max 1024 (nb / 10));
      ifam_places = Hashtbl.create (max 1024 (nf / 10));
    }
  in
  Geneweb_db.Collection.iter (fun iper -> add_iper cache base iper) (ipers base);
  Geneweb_db.Collection.iter (fun ifam -> add_ifam cache base ifam) (ifams base);
  cache

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
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let buf = Bytes.create (String.length magic) in
      really_input ic buf 0 (String.length magic);
      if Bytes.to_string buf <> magic then
        failwith "place_cache: magic mismatch";
      (Marshal.from_channel ic : t))

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
  (* Strip old contributions via the reverse index, then re-index if the person
     still exists (i.e. was not deleted). *)
  remove_iper cache iper;
  if iper_exists base iper then add_iper cache base iper

let update_ifam cache base ifam =
  remove_ifam cache ifam;
  if ifam_exists base ifam then add_ifam cache base ifam

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

let rebuild bdir conf base =
  let cache = build conf base in
  write bdir cache;
  write_meta bdir (latest_synchro_ts bdir base);
  cache

let get_or_build bdir conf base =
  (* A cache file whose [magic] no longer matches (format bump) or that fails to
     deserialise (corruption/truncation) must trigger a rebuild, not a crash.
     [cache_is_valid] only checks mtime, so we guard the read itself and fall
     back to [rebuild] on any failure. *)
  if not (cache_is_valid bdir base) then rebuild bdir conf base
  else
    match read bdir with
    | exception _ -> rebuild bdir conf base
    | cache ->
        let entries = pending_entries bdir base in
        if entries = [] then cache
        else
          let latest_ts = apply_delta cache base entries in
          write bdir cache;
          write_meta bdir latest_ts;
          cache
