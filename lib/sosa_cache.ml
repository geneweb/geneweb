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
    let () = Gwdb.load_ascends_array base in
    let () = Gwdb.load_couples_array base in
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
    let () = Gwdb.load_ascends_array base in
    let () = Gwdb.load_couples_array base in
    let arr = Array.make (Gwdb.nb_of_persons base) None in
    let ancestor_queue = Queue.create () in
    Queue.add (sosa_ref, Sosa.one) ancestor_queue;
    arr.(Gwdb.int_of_iper sosa_ref) <- Some Sosa.one;
    let add_in_queue iper sosa =
      let v = arr.(Gwdb.int_of_iper iper) in
      match v with
      | None ->
          arr.(Gwdb.int_of_iper iper) <- Some sosa;
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
    let oc = Secure.open_out tmp_cache_file in
    Marshal.to_channel oc cache [];
    close_out oc;
    Files.mv tmp_cache_file cache_file

  let input ~conf ~base : t option =
    let base_dir = Util.bpath (Gwdb.bname base ^ ".gwb") in
    let cache_file = Filename.concat base_dir "cache_static_sosa" in
    if Sys.file_exists cache_file then (
      let ic = Secure.open_in cache_file in
      let cache : Sosa.t option array = Marshal.from_channel ic in
      close_in ic;
      Some cache)
    else None

  let get_sosa ~conf ~base ~cache ~iper = cache.(Gwdb.int_of_iper iper)
end

type t = DynamicCache of DynamicCache.t | StaticCache of StaticCache.t

let dynamic_cache cache = DynamicCache cache
let static_cache cache = StaticCache cache
let output_sosa_cache ~conf ~base ~cache = StaticCache.output ~base ~cache
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
  (not (Files.exists patch_file))
  || Files.exists cache_file
     && (Unix.stat patch_file).st_mtime < (Unix.stat cache_file).st_mtime

let get_sosa_cache ~conf ~base : t option =
  match !sosa_cache with
  | Some _cache as cache -> cache
  | None -> (
      let sosa_ref = Util.find_sosa_ref conf base in
      match sosa_ref with
      | Some sosa_ref
        when is_default_sosa_ref conf base sosa_ref
             && is_sosa_cache_valid conf base ->
          let cache = StaticCache.input ~conf ~base in
          set_static_cache cache;
          Option.map static_cache cache
      | Some sosa_ref ->
          let cache =
            DynamicCache.make ~base ~sosa_ref:(Gwdb.get_iper sosa_ref)
          in
          set_dynamic_cache (Some cache);
          Some (dynamic_cache cache)
      | None -> None)

let get_sosa ~conf ~base ~cache ~iper ~sosa_ref =
  if Gwdb.compare_iper iper (Gwdb.get_iper sosa_ref) = 0 then Some Sosa.one
  else
    match cache with
    | DynamicCache cache ->
        DynamicCache.get_sosa ~conf ~base ~cache ~iper
          ~sosa_ref:(Gwdb.get_iper sosa_ref)
    | StaticCache cache -> StaticCache.get_sosa ~conf ~base ~cache ~iper

let build_static_sosa_cache ~conf ~base =
  let cache = StaticCache.build ~conf ~base in
  Option.map static_cache cache

let output_static_sosa_cache ~base ~cache =
  match cache with
  | StaticCache cache -> StaticCache.output ~base ~cache
  | DynamicCache cache ->
      failwith "output_static_sosa_cache called with dynamic cache"

let write_static_sosa_cache ~conf ~base =
  let cache = build_static_sosa_cache ~conf ~base in
  Option.iter (fun cache -> output_static_sosa_cache ~base ~cache) cache

let get_sosa_person ~conf ~base ~person =
  Option.bind (get_sosa_cache ~conf ~base) (fun cache ->
      Option.bind (Util.find_sosa_ref conf base) (fun sosa_ref ->
          get_sosa ~conf ~base ~cache ~iper:(Gwdb.get_iper person) ~sosa_ref))
  |> Option.value ~default:Sosa.zero

(* ************************************************************************ *)
(*  [Fonc] print_sosa : config -> base -> person -> bool -> unit            *)

(* ************************************************************************ *)

(** [Description] : Affiche le picto sosa ainsi que le lien de calcul de
      relation entre la personne et le sosa 1 (si l'option cancel_link
      n'est pas activée).
    [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p    : la personne que l'on veut afficher
    - link : ce booléen permet d'afficher ou non le lien sur le picto
               sosa. Il n'est pas nécessaire de mettre le lien si on a
               déjà affiché cette personne.
      [Retour] :
    - unit
      [Rem] : Exporté en clair hors de ce module.                             *)
let print_sosa ~conf ~base ~person ~link =
  let sosa_num = get_sosa_person ~conf ~base ~person in
  if Sosa.gt sosa_num Sosa.zero then
    match Util.find_sosa_ref conf base with
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
          if Util.is_hide_names conf r && not (Util.authorized_age conf base r)
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
