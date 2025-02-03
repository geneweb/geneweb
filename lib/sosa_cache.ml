type dynamic_cache = {
  cache : SosaCache.sosa_t;
  ht : (Gwdb.iper, (Sosa.t * Gwdb.person) option) Hashtbl.t;
}

type static_cache =
  | St of (Gwdb.iper, Sosa.t option) Gwdb.Marker.t
  | Stf of Sosa.t option array

type t = DynamicCache of dynamic_cache | StaticCache of static_cache

let compute_all_sosas ~base ~sosa_ref =
  print_endline "=================BUILD STATIC CACHE=======================";
  let ipers = Gwdb.ipers base in
  let iper_marker = Gwdb.iper_marker ipers None in
  let ancestor_queue = Queue.create () in
  Queue.add (sosa_ref, Sosa.one) ancestor_queue;
  Gwdb.Marker.set iper_marker sosa_ref (Some Sosa.one);
  let add_in_queue iper sosa =
    let v = Gwdb.Marker.get iper_marker iper in
    match v with
    | None ->
        Gwdb.Marker.set iper_marker iper (Some sosa);
        Queue.push (iper, sosa) ancestor_queue
    | Some _sosa -> ()
  in
  let rec aux () =
    if Queue.is_empty ancestor_queue then ()
    else
      let ancestor, sosa = Queue.pop ancestor_queue in
      match Gwdb.get_parents (Gwdb.poi base ancestor) with
      | Some ifam ->
          let fam = Gwdb.foi base ifam in
          let ifath = Gwdb.get_father fam in
          let imoth = Gwdb.get_mother fam in
          let sosa_fath = Sosa.twice sosa in
          let sosa_moth = Sosa.inc sosa_fath 1 in
          add_in_queue ifath sosa_fath;
          add_in_queue imoth sosa_moth;
          aux ()
      | None -> aux ()
  in
  aux ();
  St iper_marker

let build_sosa_cache ~conf ~base =
  let sosa_ref = Util.default_sosa_ref conf base in
  Option.bind sosa_ref (fun sosa_ref ->
      Some (compute_all_sosas ~base ~sosa_ref:(Gwdb.get_iper sosa_ref)))

let output_sosa_cache ~conf ~base ~cache =
  print_endline "======================OUTPUT CACHE==================";
  match cache with
  | St cache ->
      let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
      let cache_file = Filename.concat base_dir "cache_static_sosa" in
      let nb_persons = Gwdb.nb_of_persons base in
      let arr = Array.make nb_persons None in
      let oc = open_out cache_file in
      Gwdb.Collection.iter
        (fun iper ->
          let sosa_v = Gwdb.Marker.get cache iper in
          let i = Gwdb.string_of_iper iper |> int_of_string in
          arr.(i) <- sosa_v)
        (Gwdb.ipers base);
      Marshal.to_channel oc arr [];
      close_out oc
  | _ -> assert false

let sosa_cache : t option ref = ref None

let set_dynamic_cache cache_o =
  Option.iter
    (fun cache ->
      sosa_cache := Some (DynamicCache { cache; ht = Hashtbl.create 100 }))
    cache_o

let set_static_cache cache_o =
  Option.iter (fun cache -> sosa_cache := Some (StaticCache cache)) cache_o

let build_dynamic_cache conf base sosa_ref =
  let cache = SosaCache.init_sosa_t conf base sosa_ref in
  set_dynamic_cache cache;
  !sosa_cache

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

let input_sosa_cache conf base sosa_ref : t option =
  let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
  let cache_file = Filename.concat base_dir "cache_static_sosa" in
  if Sys.file_exists cache_file then (
    print_endline "===================INPUT CACHE FOUND=====================";
    let ic = open_in cache_file in
    let cache : Sosa.t option array = Marshal.from_channel ic in
    close_in ic;
    set_static_cache (Some (Stf cache));
    !sosa_cache)
  else (
    (* TODO no build *)
    print_endline
      "=================INPUT CACHE NOT FOUND=======================";
    let cache = build_sosa_cache ~conf ~base in
    set_static_cache cache;
    Option.iter (fun cache -> output_sosa_cache ~conf ~base ~cache) cache;
    Option.map (fun c -> StaticCache c) cache)

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
          input_sosa_cache conf base sosa_ref
      | Some sosa_ref ->
          print_endline
            "============================================================use \
             dyn sosa cache";
          build_dynamic_cache conf base sosa_ref
      | None ->
          (*print_endline "============================================================no sosa ref";*)
          None)

let add_to_l cache sosa iper person =
  Hashtbl.add cache.ht iper (Some (sosa, person))

let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
  if Gwdb.compare_iper iper (Gwdb.get_iper sosa_ref) = 0 then Some Sosa.one
  else
    match cache with
    | DynamicCache cache -> (
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
              None))
    | StaticCache (St cache) -> Gwdb.Marker.get cache iper
    | StaticCache (Stf cache) ->
        cache.(Gwdb.string_of_iper iper |> int_of_string)

(*let get_sosa ~conf ~base ~cache ~iper ~sosa_ref = Some Sosa.one*)

let next_sosa cache sosa = None
let previous_sosa cache sosa = None
