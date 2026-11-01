module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Dutil = Geneweb_db.Dutil
module Dirs = Geneweb_dirs

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
let notes = ref false
let ngrams = ref false
let all = ref false
let prog = ref false
let binary = ref false
let datalist = ref false
let width = ref 50
let text_dir = ref ""
let ( // ) = Filename.concat
let text_file bname fname = !text_dir // (bname ^ "_" ^ fname ^ ".cache.gz")

let write_cache_file bname fname data =
  let gz_file = text_file bname fname in
  let buf = Buffer.create 65536 in
  List.iter
    (fun s ->
      Buffer.add_string buf s;
      Buffer.add_char buf '\n')
    data;
  let compressed = My_gzip.gzip_string ~level:9 (Buffer.contents buf) in
  Secure.with_open_out_bin gz_file (fun oc -> output_string oc compressed)

let entries_strings bname fname collect_fn =
  match Geneweb.Cache.read bname fname with
  | Some entries -> List.map snd entries
  | None ->
      ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter collect_fn
      |> List.map snd

let should_gen_datalist () =
  match (!binary, !datalist) with
  | false, false | false, true | true, true -> true
  | true, false -> false

let should_gen_checkdata () =
  match (!binary, !datalist) with
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
      f i (Driver.get_burial_place p);
      List.iter (fun e -> f i e.Def.epers_place) (Driver.get_pevents p))
    ipers;
  Collection.iteri
    (fun i ifam ->
      let fam = Driver.foi base ifam in
      f (n_pers + i) (Driver.get_marriage_place fam);
      List.iter
        (fun e -> f (n_pers + i) e.Def.efam_place)
        (Driver.get_fevents fam))
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
      f (Driver.get_birth_src p);
      f (Driver.get_baptism_src p);
      f (Driver.get_death_src p);
      f (Driver.get_burial_src p);
      f (Driver.get_psources p);
      List.iter (fun t -> f t.Def.epers_src) (Driver.get_pevents p);
      Array.iter
        (fun ifam ->
          let fam = Driver.foi base ifam in
          f (Driver.get_marriage_src fam);
          f (Driver.get_fsources fam);
          List.iter (fun evt -> f evt.Def.efam_src) (Driver.get_fevents fam))
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
  Geneweb.Cache.write bname fname entries;
  let path = Geneweb.Cache.file bname fname in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length entries) fname duration;
  (entries, duration)

let gen_datalist_from_entries bname fname entries =
  let data, duration =
    with_timer @@ fun () -> List.map snd entries |> List.sort String.compare
  in
  write_cache_file bname fname data;
  let path = text_file bname fname in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length data) fname duration;
  duration

let gen_datalist_merged bname fname fetch_main fetch_alias =
  let data, duration =
    with_timer @@ fun () ->
    List.sort_uniq String.compare
      (List.rev_append (fetch_main ()) (fetch_alias ()))
  in
  write_cache_file bname fname data;
  let path = text_file bname fname in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width path
    (List.length data) (fname ^ "+alias") duration;
  duration

let gen_both_caches bname fname collect_fn =
  if !prog then Format.printf "Generating %s binary cache...@." fname;
  let entries, dur1 = gen_checkdata_cache bname fname collect_fn in
  if !prog then Format.printf "Extracting %s text cache...@." fname;
  let dur2 = gen_datalist_from_entries bname fname entries in
  dur1 +. dur2

let gen_checkdata_only bname fname collect_fn =
  if !prog then Format.printf "Generating %s binary cache...@." fname;
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

(* Corpus of person/family/event notes searched by m=NOTES_SEARCH, written
   atomically so that a running gwd keeps reading the previous one meanwhile.
   Changes committed later are read live by gwd: rebuild only after the base
   is regenerated. *)
let gen_notes_corpus bname base =
  let n, duration =
    with_timer (fun () ->
        ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter (fun bar ->
            Geneweb.NotesCorpus.build ~progress:(ProgrBar.progress bar) base))
  in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@." !width
    (Geneweb.NotesCorpus.file bname)
    n "notes" duration;
  duration

type first_name = { name : string; mutable persons : int }

let ngram_thresholds =
  [ 1; 2; 3; 5; 10; 20; 50; 100; 200; 500; 1000; 2000; 5000; 10000 ]

let ngrams_of n words =
  let a = Array.of_list words in
  List.init
    (max 0 (Array.length a - n + 1))
    (fun j -> String.concat "" (Array.to_list (Array.sub a j n)))

let hyphens_to_spaces = String.map (fun c -> if c = '-' then ' ' else c)

let gen_ngram_stats base =
  let names = Driver.Istr.Table.create 4096 in
  let nb_pers = Driver.nb_of_persons base in
  let (counts, added, index_entries), duration =
    with_timer @@ fun () ->
    ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter (fun bar ->
        iteri_pers
          (fun i p ->
            if !prog then ProgrBar.progress bar i nb_pers;
            let istr = Driver.get_first_name p in
            if not (Driver.Istr.is_empty istr || Driver.Istr.is_quest istr) then
              match Driver.Istr.Table.find_opt names istr with
              | Some f -> f.persons <- f.persons + 1
              | None ->
                  Driver.Istr.Table.replace names istr
                    {
                      name = hyphens_to_spaces (Driver.sou base istr);
                      persons = 1;
                    })
          base);
    let counts = Array.init 3 (fun _ -> Hashtbl.create 100_000) in
    Driver.Istr.Table.iter
      (fun _ f ->
        let words = Name.split_fname f.name in
        Array.iteri
          (fun k tbl ->
            List.iter
              (fun g ->
                Hashtbl.replace tbl g
                  (f.persons + Option.value ~default:0 (Hashtbl.find_opt tbl g)))
              (ngrams_of (k + 2) words))
          counts)
      names;
    let added = Array.make 3 [] in
    let index_entries = ref 0 in
    Driver.Istr.Table.iter
      (fun _ f ->
        let words = Name.split_fname f.name in
        let keys =
          List.sort_uniq Int.compare
            (List.map Dutil.name_index (f.name :: words))
        in
        index_entries := !index_entries + List.length keys;
        Array.iteri
          (fun k tbl ->
            List.fold_left
              (fun acc g ->
                let b = Dutil.name_index g in
                let c = Hashtbl.find tbl g in
                if List.mem b keys then acc
                else
                  match List.assoc_opt b acc with
                  | Some c' when c' >= c -> acc
                  | _ -> (b, c) :: List.remove_assoc b acc)
              []
              (ngrams_of (k + 2) words)
            |> List.iter (fun (_, c) -> added.(k) <- c :: added.(k)))
          counts)
      names;
    (counts, added, !index_entries)
  in
  let kb n = ((5 * n) + 512) / 1024 in
  Printf.printf
    "First name n-grams, counted on persons as gwc -ngrams does\n\
    \  %d distinct first names on %d persons\n\
    \  first name index without n-grams: %d entries, about %d kB\n"
    (Driver.Istr.Table.length names)
    (Driver.Istr.Table.fold (fun _ f n -> n + f.persons) names 0)
    index_entries (kb index_entries);
  Array.iteri
    (fun k tbl ->
      Printf.printf
        "  %s: %d distinct\n    threshold      kept   entries  growth\n"
        [| "bigrams"; "trigrams"; "quadrigrams" |].(k)
        (Hashtbl.length tbl);
      List.iter
        (fun t ->
          let kept =
            Hashtbl.fold (fun _ c n -> if c >= t then n + 1 else n) tbl 0
          in
          let entries =
            List.fold_left (fun n c -> if c >= t then n + 1 else n) 0 added.(k)
          in
          if kept > 0 then
            Printf.printf "    %9d %9d %9d %+6.1f%% (+%d kB)\n" t kept entries
              (100. *. float entries /. float (max 1 index_entries))
              (kb entries))
        ngram_thresholds;
      Hashtbl.fold (fun g c acc -> (g, c) :: acc) tbl []
      |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
      |> List.filteri (fun i _ -> i < 10)
      |> List.map (fun (g, c) -> Printf.sprintf "%s %d" g c)
      |> String.concat ", "
      |> Printf.printf "    most frequent: %s\n")
    counts;
  print_endline
    "  entries: (first name, crushed key) pairs added to the index, about 5 \
     bytes each; a key shared by n-grams of different lengths of a same first \
     name is counted once per length";
  duration

let validate_options () =
  if !bname = "" then (
    Printf.eprintf "Error: missing database name, see cache_files -help\n";
    exit 1);
  if !merge_aliases && not !datalist then (
    Printf.eprintf "Error: -merge can only be used with -datalist\n";
    exit 1);

  if (!fname_aliases || !sname_aliases) && not !binary then (
    Printf.eprintf "Error: -fna and -sna can only be used with -binary\n";
    exit 1)

let speclist =
  [
    ( "-bd",
      Arg.String Secure.set_base_dir,
      Fmt.str
        "<DIR> Specify where the 'bases' directory is installed (default %S)"
        (Dirs.name Secure.default_base_dir) );
    ("", Arg.Unit (fun () -> ()), "");
    ("-fn", Arg.Set fnames, " first names");
    ("-fna", Arg.Set fname_aliases, " first name aliases (only with -binary)");
    ("-sn", Arg.Set snames, " surnames");
    ("-sna", Arg.Set sname_aliases, " surname aliases (only with -binary)");
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
    ( "-notes",
      Arg.Set notes,
      " person, family and event notes, for m=NOTES_SEARCH (rebuild after gwc)"
    );
    ( "-ngrams",
      Arg.Set ngrams,
      " first name n-gram statistics, to choose the thresholds of gwc -ngrams"
    );
    ("", Arg.Unit (fun () -> ()), "");
    ("-all", Arg.Set all, " build all type of cache files");
    ("", Arg.Unit (fun () -> ()), "");
    ( "-binary",
      Arg.Set binary,
      " generate binary caches only, in <bases>/<base>.gwb/caches (for tools)"
    );
    ( "-datalist",
      Arg.Set datalist,
      " generate text caches only, in <bases>/etc/<base>/cache (for datalists)"
    );
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
  let gen_dl = should_gen_datalist () in
  let gen_cd = should_gen_checkdata () in
  text_dir := Secure.base_dir () // "etc" // bname // "cache";
  if gen_dl then Filesystem.create_dir ~parent:true !text_dir;

  width := String.length (Geneweb.GWPARAM.cache_d bname // bname) + 30;

  let no_dict_cache =
    (!notes || !ngrams)
    && not
         (!all || !fnames || !snames || !aliases || !pub_names || !fname_aliases
        || !sname_aliases || !places || !estates || !titles || !occupations
        || !qualifiers || !sources)
  in
  (if not no_dict_cache then
     match (gen_dl, gen_cd) with
     | true, true ->
         Printf.printf
           "Generating binary caches for tools and text caches for datalists\n"
     | false, true -> Printf.printf "Generating binary caches for tools\n"
     | true, false -> Printf.printf "Generating text caches for datalists\n"
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
          !total
          +. gen_datalist_merged bname "fnames"
               (fun () ->
                 entries_strings bname "fnames"
                   (collect_checkdata_names base `Fnames))
               (fun () ->
                 entries_strings bname "fnames_alias"
                   (collect_checkdata_names base `Fnames_alias)))
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
          !total
          +. gen_datalist_merged bname "snames"
               (fun () ->
                 entries_strings bname "snames"
                   (collect_checkdata_names base `Snames))
               (fun () ->
                 entries_strings bname "snames_alias"
                   (collect_checkdata_names base `Snames_alias)))
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

  if !all || !notes then total := !total +. gen_notes_corpus bname base;
  if !ngrams then total := !total +. gen_ngram_stats base;

  let min, sec = (Float.to_int !total / 60, mod_float !total 60.) in
  Format.printf "Total duration: %d min %6.2f s@." min sec
