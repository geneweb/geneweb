type t = {
  cache : SosaCache.sosa_t;
  ht : (Gwdb.iper, (Sosa.t * Gwdb.person) option) Hashtbl.t;
}

let build_sosa_cache ~conf ~base = assert false
let output_sosa_cache ~conf ~base ~cache = ()
let sosa_cache : t option ref = ref None

let set_sosa_cache cache_o =
  Option.iter
    (fun cache -> sosa_cache := Some { cache; ht = Hashtbl.create 100 })
    cache_o

let is_default_sosa_ref conf base sosa_ref =
  let dft_sosa_ref = Util.default_sosa_ref conf base in
  Option.bind dft_sosa_ref (fun dft_sosa_ref ->
      Some (Gwdb.get_iper sosa_ref = Gwdb.get_iper dft_sosa_ref))
  |> Option.value ~default:false

let is_sosa_cache_valid conf base =
  let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
  let patch_file = Filename.concat base_dir "patches" in
  print_endline patch_file;
  not (Sys.file_exists patch_file)

let input_sosa_cache conf base : t option =
  (* TODO *)
  let sosa_ref = Util.find_sosa_ref conf base in
  let cache = SosaCache.init_sosa_t conf base (Option.get sosa_ref) in
  set_sosa_cache cache;
  !sosa_cache

let get_sosa_cache ~conf ~base : t option =
  (*print_endline "================GET SOSA CACHE========================";*)
  match !sosa_cache with
  | Some _cache as cache -> cache
  | None -> (
      let sosa_ref = Util.find_sosa_ref conf base in
      match sosa_ref with
      | Some sosa_ref
        when is_default_sosa_ref conf base sosa_ref
             && is_sosa_cache_valid conf base ->
          print_endline
            "============================================================use \
             static sosa cache";
          input_sosa_cache conf base
      | Some sosa_ref ->
          print_endline
            "============================================================use \
             dyn sosa cache";
          let cache = SosaCache.init_sosa_t conf base sosa_ref in
          set_sosa_cache cache;
          !sosa_cache
      | None ->
          (*print_endline "============================================================no sosa ref";*)
          None)

let add_to_l cache sosa iper person =
  Hashtbl.add cache.ht iper (Some (sosa, person))

let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
  if Gwdb.compare_iper iper (Gwdb.get_iper sosa_ref) = 0 then Some Sosa.one
  else
    try Option.map (fun (sosa, person) -> sosa) (Hashtbl.find cache.ht iper)
    with Not_found -> (
      match
        SosaCache.find_sosa conf base (Gwdb.poi base iper) (Some sosa_ref)
          cache.cache
      with
      | Some (sosa, _) ->
          add_to_l cache sosa iper (Gwdb.poi base iper);
          Some sosa
      | None ->
          Hashtbl.add cache.ht iper None;
          None)

(*let get_sosa ~conf ~base ~cache ~iper ~sosa_ref = Some Sosa.one*)

let next_sosa cache sosa = None
let previous_sosa cache sosa = None
