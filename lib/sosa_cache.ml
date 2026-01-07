module type CacheData_S = sig
  type t

  val set : t -> Gwdb.iper -> Sosa.t option -> unit
  val get : t -> Gwdb.iper -> Sosa.t option
  val make : Gwdb.base -> t
end

module Make (D : CacheData_S) : sig
  type t

  val pop : t -> Gwdb.iper * Sosa.t
  val is_empty : t -> bool
  val create : Gwdb.base -> Gwdb.iper -> t
  val get : t -> Gwdb.iper -> Sosa.t option
  val get_data : t -> D.t

  val add_parents :
    base:Gwdb.base -> iper:Gwdb.iper -> sosa:Sosa.t -> cache:t -> unit
end = struct
  type t = { queue : (Gwdb.iper * Sosa.t) Queue.t; data : D.t }

  let pop cache = Queue.pop cache.queue
  let is_empty cache = Queue.is_empty cache.queue

  let create base sosa_ref =
    let () = Gwdb.load_ascends_array base in
    let () = Gwdb.load_couples_array base in
    let data = D.make base in
    let queue = Queue.create () in
    Queue.push (sosa_ref, Sosa.one) queue;
    D.set data sosa_ref (Some Sosa.one);
    { queue; data }

  let add_in_queue cache iper sosa =
    match D.get cache.data iper with
    | None ->
        D.set cache.data iper (Some sosa);
        Queue.push (iper, sosa) cache.queue
    | Some _sosa -> ()

  let add_parents ~base ~iper ~sosa ~cache =
    Option.iter
      (fun ifam ->
        let fam = Gwdb.foi base ifam in
        let ifath = Gwdb.get_father fam in
        let imoth = Gwdb.get_mother fam in
        let sosa_fath = Sosa.twice sosa in
        let sosa_moth = Sosa.inc sosa_fath 1 in
        add_in_queue cache ifath sosa_fath;
        add_in_queue cache imoth sosa_moth)
      (Gwdb.get_parents (Gwdb.poi base iper))

  let get cache = D.get cache.data
  let get_data cache = cache.data
end

module DynamicCache : sig
  type t

  val make : base:Gwdb.base -> sosa_ref:Gwdb.iper -> t
  val get_sosa : base:Gwdb.base -> cache:t -> iper:Gwdb.iper -> Sosa.t option
end = struct
  module Cache = Make (struct
    type t = (Gwdb.iper, Sosa.t option) Gwdb.Marker.t

    let set = Gwdb.Marker.set
    let get = Gwdb.Marker.get
    let make base = Gwdb.iper_marker (Gwdb.ipers base) None
  end)

  type t = Cache.t

  let compute_sosa ~base ~cache ~iper =
    let rec aux () =
      if Cache.is_empty cache then None
      else
        let ancestor, sosa = Cache.pop cache in
        Cache.add_parents ~base ~iper:ancestor ~sosa ~cache;
        if Gwdb.eq_iper ancestor iper then Some sosa else aux ()
    in
    match Cache.get cache iper with Some sosa -> Some sosa | None -> aux ()

  let make ~base ~sosa_ref = Cache.create base sosa_ref
  let get_sosa = compute_sosa
end

module StaticCache : sig
  type t

  val build : conf:Config.config -> base:Gwdb.base -> t option
  val output : base:Gwdb.base -> cache:t -> unit
  val input : base:Gwdb.base -> t option
  val get_sosa : cache:t -> iper:Gwdb.iper -> Sosa.t option
  val get_sosa_ref : cache:t -> Gwdb.iper
  val erase_cache_file : Gwdb.base -> unit
end = struct
  module CacheData = struct
    type t = Sosa.t option array

    let set a iper = Array.set a (Gwdb.int_of_iper iper)
    let get a iper = Array.get a (Gwdb.int_of_iper iper)
    let make base = Array.make (Gwdb.nb_of_persons base) None
  end

  module Cache = Make (CacheData)

  type t = { sosa_ref : Gwdb.iper; data : CacheData.t }

  let compute_all_sosas ~base ~sosa_ref =
    let cache = Cache.create base sosa_ref in
    let rec aux () =
      if Cache.is_empty cache then ()
      else
        let ancestor, sosa = Cache.pop cache in
        Cache.add_parents ~base ~iper:ancestor ~sosa ~cache;
        aux ()
    in
    aux ();
    cache

  let build ~conf ~base =
    let sosa_ref = Util.default_sosa_ref conf base in
    Option.map
      (fun sosa_ref ->
        {
          sosa_ref = Gwdb.get_iper sosa_ref;
          data =
            Cache.get_data
              (compute_all_sosas ~base ~sosa_ref:(Gwdb.get_iper sosa_ref));
        })
      sosa_ref

  let cache_filename base =
    let base_dir = GWPARAM.bpath (Gwdb.bname base ^ ".gwb") in
    Filename.concat base_dir "cache_static_sosa"

  let erase_cache_file base = Files.rm (cache_filename base)

  let output ~base ~cache =
    let cache_file = cache_filename base in
    let tmp_cache_file = cache_file ^ "~" in
    let oc = Secure.open_out tmp_cache_file in
    Marshal.to_channel oc cache [];
    close_out oc;
    Files.mv tmp_cache_file cache_file

  let input ~base : t option =
    let base_dir = GWPARAM.bpath (Gwdb.bname base ^ ".gwb") in
    let cache_file = Filename.concat base_dir "cache_static_sosa" in
    if Sys.file_exists cache_file then (
      let ic = Secure.open_in cache_file in
      let cache : t = Marshal.from_channel ic in
      close_in ic;
      Some cache)
    else None

  let get_sosa ~cache ~iper = CacheData.get cache.data iper
  let get_sosa_ref ~cache = cache.sosa_ref
end

type t = DynamicCache of DynamicCache.t | StaticCache of StaticCache.t

let dynamic_cache cache = DynamicCache cache
let static_cache cache = StaticCache cache
let sosa_cache : t option ref = ref None
let sosa_ref : Gwdb.person option option ref = ref None

let find_sosa_ref conf base =
  match !sosa_ref with
  | Some sosa_ref -> sosa_ref
  | None ->
      let sosa_r = Util.find_sosa_ref conf base in
      sosa_ref := Some sosa_r;
      sosa_r

let set_dynamic_cache cache_o =
  Option.iter (fun cache -> sosa_cache := Some (dynamic_cache cache)) cache_o

let set_static_cache cache_o =
  Option.iter (fun cache -> sosa_cache := Some (static_cache cache)) cache_o

let is_default_sosa_ref conf base sosa_ref =
  let dft_sosa_ref = Util.default_sosa_ref conf base in
  Option.map
    (Gwdb.eq_iper (Gwdb.get_iper sosa_ref))
    (Option.map Gwdb.get_iper dft_sosa_ref)
  |> Option.value ~default:false

let is_sosa_cache_valid base =
  let base_dir = GWPARAM.bpath (Gwdb.bname base ^ ".gwb") in
  let patch_file = Filename.concat base_dir "patches" in
  let base_file = Filename.concat base_dir "base" in
  let cache_file = Filename.concat base_dir "cache_static_sosa" in
  let has_cache_file = Files.exists cache_file in
  let has_patch_file = Files.exists patch_file in
  has_cache_file && (not has_patch_file)
  && (Unix.stat base_file).st_mtime <= (Unix.stat cache_file).st_mtime
  || has_cache_file && has_patch_file
     && (Unix.stat patch_file).st_mtime < (Unix.stat cache_file).st_mtime

let get_dynamic_cache base sosa_ref =
  let cache = DynamicCache.make ~base ~sosa_ref:(Gwdb.get_iper sosa_ref) in
  set_dynamic_cache (Some cache);
  Some (dynamic_cache cache)

let get_sosa_cache ~conf ~base : t option =
  match !sosa_cache with
  | Some _cache as cache -> cache
  | None -> (
      let sosa_ref = find_sosa_ref conf base in
      match sosa_ref with
      | Some sosa_ref
        when is_default_sosa_ref conf base sosa_ref && is_sosa_cache_valid base
        -> (
          let cache = StaticCache.input ~base in
          (* default sosa ref may have changed since cache generation *)
          match cache with
          | Some cache
            when Gwdb.eq_iper
                   (StaticCache.get_sosa_ref ~cache)
                   (Gwdb.get_iper sosa_ref) ->
              set_static_cache (Some cache);
              Some (static_cache cache)
          | Some _ | None -> get_dynamic_cache base sosa_ref)
      | Some sosa_ref -> get_dynamic_cache base sosa_ref
      | None -> None)

let get_sosa ~base ~cache ~iper =
  match cache with
  | DynamicCache cache -> DynamicCache.get_sosa ~base ~cache ~iper
  | StaticCache cache -> StaticCache.get_sosa ~cache ~iper

let write_static_sosa_cache ~conf ~base =
  let cache = StaticCache.build ~conf ~base in
  match cache with
  | Some cache -> StaticCache.output ~base ~cache
  | None -> StaticCache.erase_cache_file base

let get_sosa_person ~conf ~base ~person =
  Option.bind (get_sosa_cache ~conf ~base) (fun cache ->
      get_sosa ~base ~cache ~iper:(Gwdb.get_iper person))
  |> Option.value ~default:Sosa.zero

(* ************************************************************************ *)
(*  [Fonc] print_sosa : config -> base -> person -> bool -> unit            *)

(* ************************************************************************ *)

(** [Description] : Affiche le picto sosa ainsi que le lien de calcul de
    relation entre la personne et le sosa 1 (si l'option cancel_link n'est pas
    activée). [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p : la personne que l'on veut afficher
    - link : ce booléen permet d'afficher ou non le lien sur le picto sosa. Il
      n'est pas nécessaire de mettre le lien si on a déjà affiché cette
      personne. [Retour] :
    - unit [Rem] : Exporté en clair hors de ce module. *)
let print_sosa ~conf ~base ~person ~link =
  let sosa_num = get_sosa_person ~conf ~base ~person in
  if Sosa.gt sosa_num Sosa.zero then
    match find_sosa_ref conf base with
    | Some r ->
        (if not link then ()
         else
           let sosa_link =
             let i1 = Gwdb.string_of_iper (Gwdb.get_iper person) in
             let i2 = Gwdb.string_of_iper (Gwdb.get_iper r) in
             let b2 = Sosa.to_string sosa_num in
             "m=RL&i1=" ^ i1 ^ "&i2=" ^ i2 ^ "&b1=1&b2=" ^ b2
           in
           Output.print_sstring conf {|<a href="|};
           Output.print_string conf (Util.commd conf);
           Output.print_string conf (sosa_link |> Adef.safe);
           Output.print_sstring conf {|"> |});
        let title =
          if Util.is_hide_names conf r && not (Person.is_visible conf base r)
          then ""
          else
            let direct_ancestor =
              Name.strip_c (Gwdb.p_first_name base r) '"'
              ^ " "
              ^ Name.strip_c (Gwdb.p_surname base r) '"'
            in
            Printf.sprintf
              (Util.fcapitale (Util.ftransl conf "direct ancestor of %s"))
              direct_ancestor
            ^ Printf.sprintf ", Sosa: %s"
                (Sosa.to_string_sep
                   (Util.transl conf "(thousand separator)")
                   sosa_num)
        in
        Output.print_sstring conf {|<img src="|};
        Output.print_string conf (Image.prefix conf);
        Output.print_sstring conf {|/sosa.png" alt="sosa" title="|};
        Output.print_string conf (title |> Adef.safe);
        Output.print_sstring conf {|"> |};
        if not link then () else Output.print_sstring conf "</a> "
    | None -> ()
