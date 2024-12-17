let bname = ref ""
let trace = ref false
let fnames = ref false
let snames = ref false
let aliases = ref false
let pub_names = ref false
let fname_aliases = ref false
let sname_aliases = ref false
let places = ref false
let estates = ref false
let titles = ref false
let occupations = ref false
let qualifiers = ref false
let sources = ref false
let all = ref false
let prog = ref false
let width = ref 50
let cache_dir = ref ""
let ( // ) = Filename.concat

(* Attention: cache files are reorg independant *)
let set_cache_dir bname =
  let cache_dir = Secure.base_dir () // "etc" // bname // "cache" in
  File.create_dir ~parent:true cache_dir;
  cache_dir

let write_cache_file bname fname l =
  let filename = bname ^ "_" ^ fname ^ ".cache" in
  let file = !cache_dir // filename in
  let gz_file = file ^ ".gz" in
  let oc = Gzip.open_out gz_file in
  let finally () = try Gzip.close_out oc with Sys_error _ -> () in
  Fun.protect ~finally @@ fun () ->
  List.iter
    (fun s ->
      let s = s ^ "\n" in
      Gzip.output_substring oc s 0 (String.length s))
    l

let with_timer f =
  let start = Unix.gettimeofday () in
  let r = f () in
  let stop = Unix.gettimeofday () in
  (r, stop -. start)

module HT = struct
  include Hashtbl.Make (struct
    type t = Gwdb_driver.istr

    let equal = Gwdb_driver.eq_istr
    let hash = Gwdb_driver.hash_istr
  end)

  let replace s i v = if not @@ Gwdb_driver.is_empty_string i then replace s i v
end

let fullname bname fname = !cache_dir // (bname ^ "_" ^ fname ^ ".cache.gz")

let iteri_places f base =
  let ipers = Gwdb.ipers base in
  let n_pers = Gwdb.nb_of_persons base in
  let ifams = Gwdb.ifams base in
  Gwdb.Collection.iteri
    (fun i iper ->
      let per = Gwdb.poi base iper in
      f i (Gwdb.get_birth_place per);
      f i (Gwdb.get_baptism_place per);
      f i (Gwdb.get_death_place per);
      f i (Gwdb.get_burial_place per))
    ipers;
  Gwdb.Collection.iteri
    (fun i ifam ->
      let fam = Gwdb.foi base ifam in
      f (n_pers + i) (Gwdb.get_marriage_place fam))
    ifams

let iteri_pers f base =
  Gwdb.Collection.iteri
    (fun i iper -> f i (Gwdb.poi base iper))
    (Gwdb.ipers base)

let collect_places base bar =
  let len = Gwdb.nb_of_persons base + Gwdb.nb_of_families base in
  let set : unit HT.t = HT.create 2048 in
  iteri_places
    (fun i istr ->
      if !prog then ProgrBar.progress bar i len;
      HT.replace set istr ())
    base;
  set

let iter_field base p f = function
  | `Fnames with_aliases ->
      f (Gwdb.get_first_name p);
      if with_aliases then List.iter f (Gwdb.get_first_names_aliases p)
  | `Snames with_aliases ->
      f (Gwdb.get_surname p);
      if with_aliases then List.iter f (Gwdb.get_surnames_aliases p)
  | `Aliases -> List.iter f (Gwdb.get_aliases p)
  | `Occupations -> f (Gwdb.get_occupation p)
  | `Qualifiers -> List.iter f (Gwdb.get_qualifiers p)
  | `Pub_names -> f (Gwdb.get_public_name p)
  | `Estates -> List.iter (fun t -> f t.Def.t_place) (Gwdb.get_titles p)
  | `Titles -> List.iter (fun t -> f t.Def.t_ident) (Gwdb.get_titles p)
  | `Sources ->
      f (Gwdb.get_psources p);
      List.iter (fun t -> f t.Def.epers_src) (Gwdb.get_pevents p);
      Array.iter
        (fun ifam ->
          List.iter
            (fun evt -> f evt.Def.efam_src)
            (Gwdb.get_fevents (Gwdb.foi base ifam)))
        (Gwdb.get_family p)

let field_to_string = function
  | `Fnames _ -> "fnames"
  | `Snames _ -> "snames"
  | `Aliases -> "aliases"
  | `Occupations -> "occupations"
  | `Qualifiers -> "qualifiers"
  | `Pub_names -> "pub_names"
  | `Estates -> "estates"
  | `Titles -> "titles"
  | `Sources -> "sources"

let collect_names base field bar =
  let len = Gwdb.nb_of_persons base in
  let set : unit HT.t = HT.create 17 in
  iteri_pers
    (fun i p ->
      if !prog then ProgrBar.progress bar i len;
      iter_field base p (fun istr -> HT.replace set istr ()) field)
    base;
  set

let process_data base set =
  HT.fold (fun k () acc -> Gwdb.sou base k :: acc) set []
  |> List.sort String.compare

let speclist =
  [
    ( "-bd",
      Arg.String Secure.set_base_dir,
      "<DIR> Specify where the 'bases' directory with databases is installed \
       (default if empty is '.')" );
    ("-fn", Arg.Set fnames, " first names");
    ("-sn", Arg.Set snames, " surnames");
    ("-al", Arg.Set aliases, " aliases");
    ("-pu", Arg.Set pub_names, " public names");
    ("-qu", Arg.Set qualifiers, " qualifiers");
    ("-pl", Arg.Set places, " places");
    ("-fna", Arg.Set fname_aliases, " add first name aliases");
    ("-sna", Arg.Set sname_aliases, " add surnames aliases");
    ("-es", Arg.Set estates, " estates");
    ("-ti", Arg.Set titles, " titles");
    ("-oc", Arg.Set occupations, " occupations");
    ("-so", Arg.Set sources, " sources");
    ("-all", Arg.Set all, " all");
    ("-prog", Arg.Set prog, " show progress bar");
  ]
  |> List.sort compare |> Arg.align

let anonfun i = bname := i
let usage = "Usage: cache_files [options] base\n where [options] are:"

let () =
  Arg.parse speclist anonfun usage;
  if not (Array.mem "-bd" Sys.argv) then Secure.set_base_dir ".";
  let bname = Filename.remove_extension (Filename.basename !bname) in
  let base = Gwdb.open_base (Secure.base_dir () // bname) in
  cache_dir := set_cache_dir bname;

  Printf.printf "Generating cache(s) compressed with gzip\n";
  width := String.length (!cache_dir // bname) + 23;

  let total_duration =
    if !all || !places then (
      if !prog then Format.printf "Generating places cache...@.";
      let n, duration =
        with_timer @@ fun () ->
        ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter
        @@ fun bar ->
        let l = collect_places base bar |> process_data base in
        write_cache_file bname "places" l;
        List.length l
      in
      Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width
        (fullname bname "places") n "places" duration;
      duration)
    else 0.
  in

  let fds = [] in
  let fds = if !all || !sources then `Sources :: fds else fds in
  let fds = if !all || !qualifiers then `Qualifiers :: fds else fds in
  let fds = if !all || !occupations then `Occupations :: fds else fds in
  let fds = if !all || !titles then `Titles :: fds else fds in
  let fds = if !all || !estates then `Estates :: fds else fds in
  let fds = if !all || !pub_names then `Pub_names :: fds else fds in
  let fds = if !all || !aliases then `Aliases :: fds else fds in
  let fds =
    if !all then `Snames true :: fds
    else if !snames then `Snames !sname_aliases :: fds
    else fds
  in
  let fds =
    if !all then `Fnames true :: fds
    else if !fnames then `Fnames !fname_aliases :: fds
    else fds
  in

  let total_duration =
    List.fold_left
      (fun total_duration field ->
        let fname = field_to_string field in
        if !prog then Format.printf "Generating %s cache...@." fname;
        let n, duration =
          with_timer @@ fun () ->
          ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter
          @@ fun bar ->
          let l = collect_names base field bar |> process_data base in
          write_cache_file bname fname l;
          List.length l
        in
        Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2fs@]@." !width
          (fullname bname fname) n fname duration;
        total_duration +. duration)
      total_duration fds
  in
  let min, sec =
    let d = Float.to_int total_duration in
    (d / 60, mod_float total_duration 60.)
  in
  Format.printf "Total duration: %d min %6.2f s@." min sec
