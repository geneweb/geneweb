(*module DynamicCache : sig
    type t

    val make : SosaCache.sosa_t -> t

    val build :
      conf:Config.config -> base:Gwdb.base -> sosa_ref:Gwdb.person -> t option

    val get_sosa :
      conf:Config.config ->
      base:Gwdb.base ->
      cache:t ->
      iper:Gwdb.iper ->
      sosa_ref:Gwdb.person ->
      Sosa.t option
  end = struct
    type t = {
      cache : SosaCache.sosa_t;
      ht : (Gwdb.iper, (Sosa.t * Gwdb.person) option) Hashtbl.t;
    }

    let make cache = { cache; ht = Hashtbl.create 100 }

    let build ~conf ~base ~sosa_ref =
      let cache = SosaCache.init_sosa_t conf base sosa_ref in
      Option.map make cache

    let add_to_ht cache sosa iper person =
      Hashtbl.add cache.ht iper (Some (sosa, person))

    let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
      try Option.map (fun (sosa, person) -> sosa) (Hashtbl.find cache.ht iper)
      with Not_found -> (
        match
          SosaCache.find_sosa conf base (Gwdb.poi base iper) (Some sosa_ref)
            cache.cache
        with
        | Some (sosa, _) ->
            add_to_ht cache sosa iper (Gwdb.poi base iper);
            Some sosa
        | None ->
            Hashtbl.add cache.ht iper None;
            None)
  end
*)
module DynamicCache : sig
  type t

  val make : base:Gwdb.base -> sosa_ref:Gwdb.iper -> t

  val get_sosa :
    conf:Config.config ->
    base:Gwdb.base ->
    cache:t ->
    iper:Gwdb.iper ->
    sosa_ref:Gwdb.iper ->
    Sosa.t option
end = struct
  type t = {
    cache : (Gwdb.iper, Sosa.t option) Gwdb.Marker.t;
    ancestor_queue : (Gwdb.iper * Sosa.t) Queue.t;
  }

  let compute_sosa ~base ~cache ~iper ~sosa_ref =
    match Gwdb.Marker.get cache.cache iper with
    | Some sosa -> Some sosa
    | None ->
        let add_in_queue iper sosa =
          let v = Gwdb.Marker.get cache.cache iper in
          match v with
          | None ->
              Gwdb.Marker.set cache.cache iper (Some sosa);
              Queue.push (iper, sosa) cache.ancestor_queue
          | Some _sosa -> ()
        in

        let add_parents iper sosa =
          match Gwdb.get_parents (Gwdb.poi base iper) with
          | Some ifam ->
              let fam = Gwdb.foi base ifam in
              let ifath = Gwdb.get_father fam in
              let imoth = Gwdb.get_mother fam in
              let sosa_fath = Sosa.twice sosa in
              let sosa_moth = Sosa.inc sosa_fath 1 in
              add_in_queue ifath sosa_fath;
              add_in_queue imoth sosa_moth
          | None -> ()
        in
        let rec aux () =
          if Queue.is_empty cache.ancestor_queue then None
          else
            let ancestor, sosa = Queue.pop cache.ancestor_queue in
            add_parents ancestor sosa;
            if Gwdb.compare_iper ancestor iper = 0 then Some sosa else aux ()
        in
        aux ()

  let make ~base ~sosa_ref =
    let cache = Gwdb.iper_marker (Gwdb.ipers base) None in
    let ancestor_queue = Queue.create () in
    Queue.push (sosa_ref, Sosa.one) ancestor_queue;
    { cache; ancestor_queue }

  let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
    compute_sosa ~base ~cache ~iper ~sosa_ref
end

module StaticCache : sig
  type t

  val build : conf:Config.config -> base:Gwdb.base -> t option
  val output : base:Gwdb.base -> cache:t -> unit
  val input : conf:Config.config -> base:Gwdb.base -> t option

  val get_sosa :
    conf:Config.config ->
    base:Gwdb.base ->
    cache:t ->
    iper:Gwdb.iper ->
    Sosa.t option
end = struct
  type t = Sosa.t option array

  let compute_all_sosas ~base ~sosa_ref =
    print_endline "=================BUILD STATIC CACHE=======================";
    let arr = Array.make (Gwdb.nb_of_persons base) None in
    let ancestor_queue = Queue.create () in
    Queue.add (sosa_ref, Sosa.one) ancestor_queue;
    arr.(Obj.magic sosa_ref) <- Some Sosa.one;
    let add_in_queue iper sosa =
      let v = arr.(Obj.magic iper) in
      match v with
      | None ->
          arr.(Obj.magic iper) <- Some sosa;
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
    arr

  let build ~conf ~base =
    let sosa_ref = Util.default_sosa_ref conf base in
    Option.bind sosa_ref (fun sosa_ref ->
        Some (compute_all_sosas ~base ~sosa_ref:(Gwdb.get_iper sosa_ref)))

  let output ~base ~cache =
    let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
    let cache_file = Filename.concat base_dir "cache_static_sosa" in
    let tmp_cache_file = cache_file ^ "~" in
    let oc = open_out tmp_cache_file in
    Marshal.to_channel oc cache [];
    close_out oc;
    Files.mv tmp_cache_file cache_file

  let input ~conf ~base : t option =
    let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
    let cache_file = Filename.concat base_dir "cache_static_sosa" in
    if Sys.file_exists cache_file then (
      print_endline "===================INPUT CACHE FOUND=====================";
      let ic = open_in cache_file in
      let cache : Sosa.t option array = Marshal.from_channel ic in
      close_in ic;
      Some cache)
    else (
      print_endline
        "=================INPUT CACHE NOT FOUND=======================";
      None)

  let get_sosa ~conf ~base ~cache ~iper = cache.(Obj.magic iper)
end

type t = DynamicCache of DynamicCache.t | StaticCache of StaticCache.t

let dynamic_cache cache = DynamicCache cache
let static_cache cache = StaticCache cache

let output_sosa_cache ~conf ~base ~cache =
  print_endline "======================OUTPUT CACHE==================";
  StaticCache.output ~base ~cache

let sosa_cache : t option ref = ref None

let set_dynamic_cache cache_o =
  Option.iter (fun cache -> sosa_cache := Some (dynamic_cache cache)) cache_o

let set_static_cache cache_o =
  Option.iter (fun cache -> sosa_cache := Some (static_cache cache)) cache_o

let is_default_sosa_ref conf base sosa_ref =
  let dft_sosa_ref = Util.default_sosa_ref conf base in
  Option.bind dft_sosa_ref (fun dft_sosa_ref ->
      Some (Gwdb.get_iper sosa_ref = Gwdb.get_iper dft_sosa_ref))
  |> Option.value ~default:false

let is_sosa_cache_valid conf base =
  let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
  let patch_file = Filename.concat base_dir "patches" in
  let cache_file = Filename.concat base_dir "cache_static_sosa" in
  print_endline patch_file;
  (not (Sys.file_exists patch_file))
  || Sys.file_exists cache_file
     && (Unix.stat patch_file).st_mtime < (Unix.stat cache_file).st_mtime

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
          let cache = StaticCache.input ~conf ~base in
          set_static_cache cache;
          Option.map static_cache cache
      | Some sosa_ref ->
          print_endline
            "============================================================use \
             dyn sosa cache";
          let cache =
            DynamicCache.make ~base ~sosa_ref:(Gwdb.get_iper sosa_ref)
          in
          set_dynamic_cache (Some cache);
          Some (dynamic_cache cache)
      | None ->
          (*print_endline "============================================================no sosa ref";*)
          None)

let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
  if Gwdb.compare_iper iper (Gwdb.get_iper sosa_ref) = 0 then Some Sosa.one
  else
    match cache with
    | DynamicCache cache ->
        DynamicCache.get_sosa ~conf ~base ~cache ~iper
          ~sosa_ref:(Gwdb.get_iper sosa_ref)
    | StaticCache cache -> StaticCache.get_sosa ~conf ~base ~cache ~iper

let next_sosa cache sosa = None
let previous_sosa cache sosa = None
