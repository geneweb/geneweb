module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type checkdata_entry = Driver.istr * string

let bname = ref ""
let fnames = ref false
let snames = ref false
let aliases = ref false
let pub_names = ref false
let fname_aliases = ref false
let sname_aliases = ref false
let merge_aliases = ref false
let places = ref false
let estates = ref false
let titles = ref false
let occupations = ref false
let qualifiers = ref false
let sources = ref false
let all = ref false
let prog = ref false
let checkdata = ref false
let datalist = ref false
let width = ref 50
let cache_dir = ref ""
let ( // ) = Filename.concat

let set_cache_dir bname =
  let dir = Secure.base_dir () // "etc" // bname // "cache" in
  Filesystem.create_dir ~parent:true dir;
  dir

let write_cache_file bname fname data =
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
    data

let write_checkdata_cache bname fname entries =
  let filename = bname ^ "_" ^ fname ^ "_checkdata.cache" in
  let file = !cache_dir // filename in
  let oc = Secure.open_out_bin file in
  let finally () = try close_out oc with Sys_error _ -> () in
  Fun.protect ~finally @@ fun () -> Marshal.to_channel oc entries []

let read_checkdata_cache bname fname =
  let filename = bname ^ "_" ^ fname ^ "_checkdata.cache" in
  let file = !cache_dir // filename in
  if Sys.file_exists file then
    let ic = Secure.open_in_bin file in
    let finally () = try close_in ic with Sys_error _ -> () in
    Fun.protect ~finally @@ fun () ->
    try
      let entries = (Marshal.from_channel ic : checkdata_entry list) in
      Some (List.map snd entries)
    with _ -> None
  else None

let should_gen_datalist () =
  match (!checkdata, !datalist) with
  | false, false | false, true | true, true -> true
  | true, false -> false

let should_gen_checkdata () =
  match (!checkdata, !datalist) with
  | false, false | true, false | true, true -> true
  | false, true -> false

let with_timer f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  (result, stop -. start)

let iteri_places f base =
  let ipers = Driver.ipers base in
  let n_pers = Driver.nb_of_persons base in
  let ifams = Driver.ifams base in
  Collection.iteri
    (fun i iper ->
      let p = Driver.poi base iper in
      f i (Driver.get_birth_place p);
      f i (Driver.get_baptism_place p);
      f i (Driver.get_death_place p);
      f i (Driver.get_burial_place p))
    ipers;
  Collection.iteri
    (fun i ifam ->
      let fam = Driver.foi base ifam in
      f (n_pers + i) (Driver.get_marriage_place fam))
    ifams

let iteri_pers f base =
  Collection.iteri
    (fun i iper -> f i (Driver.poi base iper))
    (Driver.ipers base)

let iter_field base p f = function
  | `Fnames -> f (Driver.get_first_name p)
  | `Fnames_alias -> List.iter f (Driver.get_first_names_aliases p)
  | `Snames -> f (Driver.get_surname p)
  | `Snames_alias -> List.iter f (Driver.get_surnames_aliases p)
  | `Aliases -> List.iter f (Driver.get_aliases p)
  | `Occupations -> f (Driver.get_occupation p)
  | `Qualifiers -> List.iter f (Driver.get_qualifiers p)
  | `Pub_names -> f (Driver.get_public_name p)
  | `Estates -> List.iter (fun t -> f t.Def.t_place) (Driver.get_titles p)
  | `Titles -> List.iter (fun t -> f t.Def.t_ident) (Driver.get_titles p)
  | `Sources ->
      f (Driver.get_psources p);
      List.iter (fun t -> f t.Def.epers_src) (Driver.get_pevents p);
      Array.iter
        (fun ifam ->
          List.iter
            (fun evt -> f evt.Def.efam_src)
            (Driver.get_fevents (Driver.foi base ifam)))
        (Driver.get_family p)

let field_name = function
  | `Fnames -> "fnames"
  | `Fnames_alias -> "fnames_alias"
  | `Snames -> "snames"
  | `Snames_alias -> "snames_alias"
  | `Aliases -> "aliases"
  | `Occupations -> "occupations"
  | `Qualifiers -> "qualifiers"
  | `Pub_names -> "pub_names"
  | `Estates -> "estates"
  | `Titles -> "titles"
  | `Sources -> "sources"

let collect_checkdata_places base bar =
  let len = Driver.nb_of_persons base + Driver.nb_of_families base in
  let tbl : string Driver.Istr.Table.t = Driver.Istr.Table.create 2048 in
  let add_istr istr =
    if not (Driver.Istr.is_empty istr) then
      let val_ = Driver.sou base istr in
      if val_ <> "" then Driver.Istr.Table.replace tbl istr val_
  in
  iteri_places
    (fun i istr ->
      if !prog then ProgrBar.progress bar i len;
      add_istr istr)
    base;
  Driver.Istr.Table.fold (fun istr val_ acc -> (istr, val_) :: acc) tbl []
  |> List.sort (fun (_, s1) (_, s2) -> String.compare s1 s2)

let collect_checkdata_names base field bar =
  let len = Driver.nb_of_persons base in
  let tbl : string Driver.Istr.Table.t = Driver.Istr.Table.create 17 in
  let add_istr istr =
    if not (Driver.Istr.is_empty istr) then
      let val_ = Driver.sou base istr in
      if val_ <> "" then Driver.Istr.Table.replace tbl istr val_
  in
  iteri_pers
    (fun i p ->
      if !prog then ProgrBar.progress bar i len;
      iter_field base p add_istr field)
    base;
  Driver.Istr.Table.fold (fun istr val_ acc -> (istr, val_) :: acc) tbl []
  |> List.sort (fun (_, s1) (_, s2) -> String.compare s1 s2)

let gen_checkdata_cache bname fname collect_fn =
  let entries, duration =
    with_timer @@ fun () ->
    ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter collect_fn
  in
  write_checkdata_cache bname fname entries;
  let path = !cache_dir // (bname ^ "_" ^ fname ^ "_checkdata.cache") in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length entries) fname duration;
  (entries, duration)

let gen_datalist_from_entries bname fname entries =
  let data, duration =
    with_timer @@ fun () -> List.map snd entries |> List.sort String.compare
  in
  write_cache_file bname fname data;
  let path = !cache_dir // (bname ^ "_" ^ fname ^ ".cache.gz") in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length data) fname duration;
  duration

let gen_datalist_merged bname fname main_fname alias_fname =
  let data, duration =
    with_timer @@ fun () ->
    let main_data =
      match read_checkdata_cache bname main_fname with
      | Some data -> data
      | None -> []
    in
    let alias_data =
      match read_checkdata_cache bname alias_fname with
      | Some data -> data
      | None -> []
    in
    List.sort_uniq String.compare (main_data @ alias_data)
  in
  write_cache_file bname fname data;
  let path = !cache_dir // (bname ^ "_" ^ fname ^ ".cache.gz") in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length data) (fname ^ "+alias") duration;
  duration

let gen_both_caches bname fname collect_fn =
  if !prog then Format.printf "Generating %s checkdata cache...@." fname;
  let entries, dur1 = gen_checkdata_cache bname fname collect_fn in
  if !prog then Format.printf "Extracting %s text cache...@." fname;
  let dur2 = gen_datalist_from_entries bname fname entries in
  dur1 +. dur2

let gen_checkdata_only bname fname collect_fn =
  if !prog then Format.printf "Generating %s checkdata cache...@." fname;
  let _, duration = gen_checkdata_cache bname fname collect_fn in
  duration

let gen_datalist_only bname fname collect_fn =
  if !prog then Format.printf "Generating %s cache...@." fname;
  let entries, _ =
    with_timer @@ fun () ->
    ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter collect_fn
  in
  gen_datalist_from_entries bname fname entries

let gen_cache bname fname collect_fn =
  let gen_dl = should_gen_datalist () in
  let gen_cd = should_gen_checkdata () in
  match (gen_dl, gen_cd) with
  | true, true -> gen_both_caches bname fname collect_fn
  | false, true -> gen_checkdata_only bname fname collect_fn
  | true, false -> gen_datalist_only bname fname collect_fn
  | false, false -> assert false

let validate_options () =
  if !merge_aliases && not !datalist then (
    Printf.eprintf "Error: -merge can only be used with -datalist\n";
    exit 1);

  if (!fname_aliases || !sname_aliases) && not !checkdata then (
    Printf.eprintf "Error: -fna and -sna can only be used with -checkdata\n";
    exit 1)

let speclist =
  [
    ( "-bd",
      Arg.String Secure.set_base_dir,
      "<DIR> Specify where the 'bases' directory is installed (default '.')" );
    ("", Arg.Unit (fun () -> ()), "");
    ("-fn", Arg.Set fnames, " first names");
    ("-fna", Arg.Set fname_aliases, " first name aliases (only with -checkdata)");
    ("-sn", Arg.Set snames, " surnames");
    ("-sna", Arg.Set sname_aliases, " surname aliases (only with -checkdata)");
    ( "-merge",
      Arg.Set merge_aliases,
      " merge fn/sn aliases with fn/sn (only with -datalist)" );
    ("-al", Arg.Set aliases, " aliases");
    ("-pu", Arg.Set pub_names, " public names");
    ("-qu", Arg.Set qualifiers, " qualifiers");
    ("-pl", Arg.Set places, " places");
    ("-oc", Arg.Set occupations, " occupations");
    ("-ti", Arg.Set titles, " titles");
    ("-es", Arg.Set estates, " estates");
    ("-so", Arg.Set sources, " sources");
    ("", Arg.Unit (fun () -> ()), "");
    ("-all", Arg.Set all, " build all type of cache files");
    ("", Arg.Unit (fun () -> ()), "");
    ( "-checkdata",
      Arg.Set checkdata,
      " generate binary caches only (for typographic verification)" );
    ( "-datalist",
      Arg.Set datalist,
      " generate text caches only (for browser input suggestion)" );
    ("", Arg.Unit (fun () -> ()), "");
    ("-prog", Arg.Set prog, " show progress bar");
  ]
  |> List.filter (fun (opt, _, _) -> opt <> "") (* retire les séparateurs *)
  |> Arg.align

let anonfun i = bname := i
let usage = "Usage: cache_files [options] base\nwhere [options] are:"

let () =
  Arg.parse speclist anonfun usage;
  if not (Array.mem "-bd" Sys.argv) then Secure.set_base_dir ".";
  validate_options ();
  let bname = Filename.remove_extension (Filename.basename !bname) in

  Driver.with_database (Secure.base_dir () // bname) @@ fun base ->
  cache_dir := set_cache_dir bname;

  let gen_dl = should_gen_datalist () in
  let gen_cd = should_gen_checkdata () in

  width := String.length (!cache_dir // bname) + 30;

  (match (gen_dl, gen_cd) with
  | true, true -> Printf.printf "Generating datalist and checkdata caches\n"
  | false, true -> Printf.printf "Generating CheckData binary caches\n"
  | true, false -> Printf.printf "Generating datalist compressed caches\n"
  | false, false -> assert false);

  let total = ref 0. in

  if !all || !places then
    total := !total +. gen_cache bname "places" (collect_checkdata_places base);

  let fields = [] in
  let fields = if !all || !sources then `Sources :: fields else fields in
  let fields = if !all || !qualifiers then `Qualifiers :: fields else fields in
  let fields =
    if !all || !occupations then `Occupations :: fields else fields
  in
  let fields = if !all || !titles then `Titles :: fields else fields in
  let fields = if !all || !estates then `Estates :: fields else fields in
  let fields = if !all || !pub_names then `Pub_names :: fields else fields in
  let fields = if !all || !aliases then `Aliases :: fields else fields in

  (* Gestion spéciale fn/sn : d'abord checkdata puis datalist merged si besoin *)
  let should_merge = !merge_aliases || !all in

  if !all || !fnames then (
    if gen_cd then (
      (* Génère toujours le cache fnames checkdata *)
      total :=
        !total
        +. gen_checkdata_only bname "fnames"
             (collect_checkdata_names base `Fnames);
      (* Si -all, génère aussi fnames_alias checkdata *)
      if !all then
        total :=
          !total
          +. gen_checkdata_only bname "fnames_alias"
               (collect_checkdata_names base `Fnames_alias));
    if gen_dl then
      if should_merge then (
        (* Combine fnames + fnames_alias pour datalist *)
        if !prog then Format.printf "Generating fnames cache (merged)...@.";
        total :=
          !total +. gen_datalist_merged bname "fnames" "fnames" "fnames_alias")
      else
        (* Datalist normal sans alias *)
        total :=
          !total
          +. gen_datalist_only bname "fnames"
               (collect_checkdata_names base `Fnames));

  if !all || !snames then (
    if gen_cd then (
      (* Génère toujours le cache snames checkdata *)
      total :=
        !total
        +. gen_checkdata_only bname "snames"
             (collect_checkdata_names base `Snames);
      (* Si -all, génère aussi snames_alias checkdata *)
      if !all then
        total :=
          !total
          +. gen_checkdata_only bname "snames_alias"
               (collect_checkdata_names base `Snames_alias));
    if gen_dl then
      if should_merge then (
        (* Combine snames + snames_alias pour datalist *)
        if !prog then Format.printf "Generating snames cache (merged)...@.";
        total :=
          !total +. gen_datalist_merged bname "snames" "snames" "snames_alias")
      else
        (* Datalist normal sans alias *)
        total :=
          !total
          +. gen_datalist_only bname "snames"
               (collect_checkdata_names base `Snames));

  (* Caches alias séparés pour checkdata uniquement *)
  if !fname_aliases then
    total :=
      !total
      +. gen_cache bname "fnames_alias"
           (collect_checkdata_names base `Fnames_alias);

  if !sname_aliases then
    total :=
      !total
      +. gen_cache bname "snames_alias"
           (collect_checkdata_names base `Snames_alias);

  List.iter
    (fun field ->
      let fname = field_name field in
      total :=
        !total +. gen_cache bname fname (collect_checkdata_names base field))
    fields;

  let min, sec = (Float.to_int !total / 60, mod_float !total 60.) in
  Format.printf "Total duration: %d min %6.2f s@." min sec
