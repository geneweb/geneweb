module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Database = Geneweb_db.Database

type kind = Person | Family | Person_event | Family_event
type entry = { kind : kind; id : string; evt : int; text : string }

type t = {
  version : int;
  fingerprint : string;
  built : float;
  synchro_len : int;
  entries : entry array;
}

(* bumped whenever [t] or [entry] change, so that an old file is rebuilt
   rather than misread *)
let current_version = 1
let ( // ) = Filename.concat
let gwb_dir bname = Secure.base_dir () // (bname ^ ".gwb")

(* The "base" file of a .gwb is only rewritten by a full rebuild (gwc,
   gwfixbase...), which also renumbers persons and families; commits from
   gwd go to "patches" and are followed with [changes]. *)
let fingerprint bname =
  match Unix.stat (gwb_dir bname // "base") with
  | st -> Printf.sprintf "%.0f-%d" st.Unix.st_mtime st.Unix.st_size
  | exception Unix.Unix_error _ -> ""

let synchro bname = (Database.input_synchro (gwb_dir bname)).synch_list

let entry kind id evt note acc =
  let text = NotesSearch.plain_text note in
  if text = "" then acc else { kind; id; evt; text } :: acc

let person_entries base p =
  let id = Driver.Iper.to_string (Driver.get_iper p) in
  let acc = entry Person id (-1) (Driver.sou base (Driver.get_notes p)) [] in
  let _, acc =
    List.fold_left
      (fun (k, acc) e ->
        (k + 1, entry Person_event id k (Driver.sou base e.Def.epers_note) acc))
      (0, acc) (Driver.get_pevents p)
  in
  List.rev acc

let family_entries base ifam fam =
  let id = Driver.Ifam.to_string ifam in
  let acc = entry Family id (-1) (Driver.sou base (Driver.get_comment fam)) [] in
  let _, acc =
    List.fold_left
      (fun (k, acc) e ->
        (k + 1, entry Family_event id k (Driver.sou base e.Def.efam_note) acc))
      (0, acc) (Driver.get_fevents fam)
  in
  List.rev acc

let create bname entries =
  {
    version = current_version;
    fingerprint = fingerprint bname;
    built = Unix.time ();
    synchro_len = List.length (synchro bname);
    entries = Array.of_list entries;
  }

let build ?(progress = fun _ _ -> ()) base =
  let bname = Driver.bname base in
  let ipers = Driver.ipers base in
  let ifams = Driver.ifams base in
  let n_pers = Collection.length ipers in
  let total = n_pers + Collection.length ifams in
  (* entries are collected in reverse, then reversed once *)
  let acc =
    Collection.fold
      (fun (i, acc) iper ->
        progress i total;
        let es = person_entries base (Driver.poi base iper) in
        (i + 1, List.rev_append es acc))
      (0, []) ipers
    |> snd
  in
  let acc =
    Collection.fold
      (fun (i, acc) ifam ->
        progress i total;
        let es = family_entries base ifam (Driver.foi base ifam) in
        (i + 1, List.rev_append es acc))
      (n_pers, acc) ifams
    |> snd
  in
  create bname (List.rev acc)

let file bname =
  Secure.base_dir () // "etc" // bname // "cache"
  // (bname ^ "_notes_search.cache")

let write bname corpus =
  let f = file bname in
  Filesystem.create_dir ~parent:true (Filename.dirname f);
  let tmp = f ^ ".tmp" in
  Secure.with_open_out_bin tmp (fun oc ->
      Marshal.to_channel oc (corpus : t) [ Marshal.No_sharing ]);
  Sys.rename tmp f

type status = Missing | Outdated | Available of t

let read bname =
  match
    Secure.with_open_in_bin (file bname) (fun ic ->
        (Marshal.from_channel ic : t))
  with
  | exception (Sys_error _ | End_of_file | Failure _) -> Missing
  | corpus ->
      if
        corpus.version <> current_version
        || corpus.fingerprint <> fingerprint bname
        || corpus.synchro_len > List.length (synchro bname)
      then Outdated
      else Available corpus

let changes bname corpus =
  let l = synchro bname in
  (* newest commits first: the ones after the build are the first
     [length - synchro_len] *)
  let rec take n l acc =
    match l with
    | (_, ipl, ifl) :: rest when n > 0 ->
        let ps, fs = acc in
        let ps =
          List.fold_left
            (fun s i -> Driver.Iper.Set.add (Driver.Iper.of_string (string_of_int i)) s)
            ps ipl
        in
        let fs =
          List.fold_left
            (fun s i -> Driver.Ifam.Set.add (Driver.Ifam.of_string (string_of_int i)) s)
            fs ifl
        in
        take (n - 1) rest (ps, fs)
    | _ :: _ | [] -> acc
  in
  take
    (List.length l - corpus.synchro_len)
    l
    (Driver.Iper.Set.empty, Driver.Ifam.Set.empty)
