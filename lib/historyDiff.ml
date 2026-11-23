(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

type gen_record = {
  date : Adef.safe_string;
  wizard : Adef.safe_string;
  gen_p : (Driver.iper, Driver.iper, string) gen_person;
  gen_f : (Driver.iper, Driver.ifam, string) gen_family list;
  gen_c : Driver.iper array list;
  gen_s : Driver.iper list;
}

(* A person as stored in a record: its key, and its index at the time of the
   record. Indexes change when the base is rebuilt, keys do not: a record is
   read back through the key, the index only serves for the persons whose key
   is shared ("?" names), and only if it still holds the same key. *)
type person_ref = {
  r_fn : string;
  r_sn : string;
  r_oc : int;
  r_ip : Driver.iper;
}

(* Records written since GnHd0003: persons by key, families by spouse. *)
type stored_record = {
  s_date : Adef.safe_string;
  s_wizard : Adef.safe_string;
  s_p : (unit, person_ref, string) gen_person;
  s_f : (person_ref, unit, string) gen_family list;
  s_c : person_ref array list;
  s_s : person_ref list;
}

(* Records GnHd0002: persons and families by index. *)
type gen_record_0002 = {
  date_0002 : Adef.safe_string;
  wizard_0002 : Adef.safe_string;
  gen_p_0002 : (Driver.iper, Driver.iper, string) gen_person;
  gen_f_0002 : (Driver.iper, Driver.ifam, string) gen_family list;
  gen_c_0002 : Driver.iper array list;
}

(* Records written before witness notes (GnWb0025): witnesses are pairs. A
   history file can hold all kinds, older records first. Each newer record
   is preceded by a marshaled marker, whose bytes cannot be those of an older
   record. *)
type gen_record_0024 = {
  date_0024 : Adef.safe_string;
  wizard_0024 : Adef.safe_string;
  gen_p_0024 : (Driver.iper, Driver.iper, string) Def_gnwb0024.gen_person;
  gen_f_0024 : (Driver.iper, Driver.ifam, string) Def_gnwb0024.gen_family list;
  gen_c_0024 : Driver.iper array list;
}

let record_marker = "GnHd0003"
let record_marker_0002 = "GnHd0002"
let marker_bytes = Marshal.to_bytes record_marker []
let marker_bytes_0002 = Marshal.to_bytes record_marker_0002 []

(* A record as read from a file, still marshaled. *)
type raw = V0003 of bytes | V0002 of bytes | V0024 of bytes

type entry = {
  e_raw : raw;
  e_date : Adef.safe_string;
  e_wizard : Adef.safe_string;
}

let entry_date e = e.e_date
let entry_wizard e = e.e_wizard
let unique_key fn sn = Name.lower fn <> "" && Name.lower sn <> ""

let person_ref base ip =
  if Driver.iper_exists base ip then
    let p = Driver.poi base ip in
    {
      r_fn = Driver.sou base (Driver.get_first_name p);
      r_sn = Driver.sou base (Driver.get_surname p);
      r_oc = Driver.get_occ p;
      r_ip = ip;
    }
  else { r_fn = ""; r_sn = ""; r_oc = 0; r_ip = Driver.Iper.dummy }

let stored_of_record base gr =
  let r = person_ref base in
  {
    s_date = gr.date;
    s_wizard = gr.wizard;
    s_p = { (Futil.map_person_ps r Fun.id gr.gen_p) with key_index = () };
    s_f =
      List.map
        (fun f ->
          { (Futil.map_family_ps r Fun.id Fun.id f) with fam_index = () })
        gr.gen_f;
    s_c = List.map (Array.map r) gr.gen_c;
    s_s = List.map r gr.gen_s;
  }

let output_record base oc gr =
  output_value oc record_marker;
  output_value oc (stored_of_record base gr)

(* The next marshaled value of [ic], as bytes. *)
let input_marshaled ic =
  let header = Bytes.create Marshal.header_size in
  really_input ic header 0 Marshal.header_size;
  let size = Marshal.data_size header 0 in
  let b = Bytes.create (Marshal.header_size + size) in
  Bytes.blit header 0 b 0 Marshal.header_size;
  really_input ic b Marshal.header_size size;
  b

let input_raw ic =
  let b = input_marshaled ic in
  if Bytes.equal b marker_bytes then V0003 (input_marshaled ic)
  else if Bytes.equal b marker_bytes_0002 then V0002 (input_marshaled ic)
  else V0024 b

let output_raw oc raw =
  match raw with
  | V0003 b ->
      output_value oc record_marker;
      output_bytes oc b
  | V0002 b ->
      output_value oc record_marker_0002;
      output_bytes oc b
  | V0024 b -> output_bytes oc b

let entry_of_raw raw =
  let e_date, e_wizard =
    match raw with
    | V0003 b ->
        let r : stored_record = Marshal.from_bytes b 0 in
        (r.s_date, r.s_wizard)
    | V0002 b ->
        let r : gen_record_0002 = Marshal.from_bytes b 0 in
        (r.date_0002, r.wizard_0002)
    | V0024 b ->
        let r : gen_record_0024 = Marshal.from_bytes b 0 in
        (r.date_0024, r.wizard_0024)
  in
  { e_raw = raw; e_date; e_wizard }

let known ip = not (Driver.Iper.is_dummy ip)

let find_key base fn sn oc =
  Option.value ~default:Driver.Iper.dummy (Driver.person_of_key base fn sn oc)

let iper_of_ref ?(among = fun _ -> true) base r =
  if unique_key r.r_fn r.r_sn then find_key base r.r_fn r.r_sn r.r_oc
  else if
    known r.r_ip
    && Driver.iper_exists base r.r_ip
    && among r.r_ip
    && person_ref base r.r_ip = r
  then r.r_ip
  else Driver.Iper.dummy

let filter_witnesses a =
  Array.of_seq (Seq.filter (fun (ip, _, _) -> known ip) (Array.to_seq a))

let record_of_stored base ki s =
  let ip = iper_of_ref base in
  let keep ip = if known ip then Some ip else None in
  let p = Futil.map_person_ps ip Fun.id s.s_p in
  let gen_p =
    {
      p with
      key_index = ki;
      rparents =
        List.map
          (fun r ->
            {
              r with
              r_fath = Option.bind r.r_fath keep;
              r_moth = Option.bind r.r_moth keep;
            })
          p.rparents;
      related = List.filter known p.related;
      pevents =
        List.map
          (fun e ->
            { e with epers_witnesses = filter_witnesses e.epers_witnesses })
          p.pevents;
    }
  in
  let gen_s = List.map ip s.s_s in
  let fams =
    if known ki then Driver.get_family (Driver.poi base ki) else [||]
  in
  let fam_of sp =
    if not (known sp) then Driver.Ifam.dummy
    else
      Option.value ~default:Driver.Ifam.dummy
        (Array.find_opt
           (fun ifam ->
             Driver.Iper.equal sp (Gutil.spouse ki (Driver.foi base ifam)))
           fams)
  in
  let gen_f =
    List.map2
      (fun f sp ->
        let f = Futil.map_family_ps ip (fun () -> fam_of sp) Fun.id f in
        {
          f with
          witnesses = Array.of_seq (Seq.filter known (Array.to_seq f.witnesses));
          fevents =
            List.map
              (fun e ->
                { e with efam_witnesses = filter_witnesses e.efam_witnesses })
              f.fevents;
        })
      s.s_f gen_s
  in
  let gen_c =
    List.map2
      (fun f c ->
        let children = Driver.get_children (Driver.foi base f.fam_index) in
        let among ip = Array.exists (Driver.Iper.equal ip) children in
        Array.of_seq
          (Seq.filter known
             (Seq.map (iper_of_ref ~among base) (Array.to_seq c))))
      gen_f s.s_c
  in
  { date = s.s_date; wizard = s.s_wizard; gen_p; gen_f; gen_c; gen_s }

let legacy_spouses base ki gen_f =
  List.map
    (fun f ->
      if Driver.ifam_exists base f.fam_index then
        Gutil.spouse ki (Driver.foi base f.fam_index)
      else Driver.Iper.dummy)
    gen_f

let record_of_raw base ki raw =
  match raw with
  | V0003 b -> record_of_stored base ki (Marshal.from_bytes b 0)
  | V0002 b ->
      let r : gen_record_0002 = Marshal.from_bytes b 0 in
      {
        date = r.date_0002;
        wizard = r.wizard_0002;
        gen_p = r.gen_p_0002;
        gen_f = r.gen_f_0002;
        gen_c = r.gen_c_0002;
        gen_s = legacy_spouses base r.gen_p_0002.key_index r.gen_f_0002;
      }
  | V0024 b ->
      let r : gen_record_0024 = Marshal.from_bytes b 0 in
      let gen_p = Def_gnwb0024.person ~empty:"" r.gen_p_0024 in
      let gen_f = List.map (Def_gnwb0024.family ~empty:"") r.gen_f_0024 in
      {
        date = r.date_0024;
        wizard = r.wizard_0024;
        gen_p;
        gen_f;
        gen_c = r.gen_c_0024;
        gen_s = legacy_spouses base gen_p.key_index gen_f;
      }

(* Le nom du fichier historique (à partir de la clé personne). *)
let history_file fn sn occ =
  let space_to_unders = Mutil.tr ' ' '_' in
  let f = space_to_unders (Name.lower fn) in
  let s = space_to_unders (Name.lower sn) in
  f ^ "." ^ string_of_int occ ^ "." ^ s

(* history directory path *)
let history_d conf =
  let path =
    match List.assoc_opt "history_path" conf.base_env with
    | Some path when path <> "" -> path
    | _ -> "history_d"
  in
  if Filename.is_relative path then
    Filename.concat (GWPARAM.bpath conf.bname) path
  else path

(* Folder name for the character of [fname] starting at byte [i], and the
   index of the next character. A multi-byte UTF-8 character is kept whole:
   a lone byte of it is an invalid name, refused by some file systems
   ("o’brien" must not give a "\xE2" folder). ASCII characters are kept as
   they are, so that existing records are still found, except path
   separators and control characters, which become '_', as does an invalid
   UTF-8 sequence. *)
let dir_char fname i =
  let d = String.get_utf_8_uchar fname i in
  let n = Uchar.utf_decode_length d in
  if not (Uchar.utf_decode_is_valid d) then ("_", i + n)
  else if n > 1 then (String.sub fname i n, i + n)
  else
    let c = fname.[i] in
    let name =
      if c >= ' ' && c <> '\x7F' && c <> '/' && c <> '\\' then String.make 1 c
      else "_"
    in
    (name, i + 1)

(* The two levels of folders holding [fname]: its first two characters. *)
let history_subdirs fname =
  if String.length fname >= 6 then
    let c1, i = dir_char fname 0 in
    let c2, _ = dir_char fname i in
    [ c1; c2 ]
  else []

let path_in dir fname =
  Filename.concat
    (List.fold_left Filename.concat dir (history_subdirs fname))
    fname

(* Le chemin du fichier historique dans le dossier history_d. *)
let history_path conf fname = path_in (history_d conf) fname

(* Créé tous les dossiers intermédiaires. *)
let create_history_dirs conf fname =
  match history_subdirs fname with
  | [] -> ()
  | subdirs ->
      Filesystem.create_dir ~parent:true
        (List.fold_left Filename.concat (history_d conf) subdirs)

let archive_deleted conf (p : (_, _, string) gen_person) =
  if unique_key p.first_name p.surname then
    let person_file = history_file p.first_name p.surname p.occ in
    let src = history_path conf person_file in
    if Sys.file_exists src then
      let dst =
        path_in (Filename.concat (history_d conf) "_deleted") person_file
      in
      try
        Filesystem.create_dir ~parent:true (Filename.dirname dst);
        if Sys.file_exists dst then (
          let s = Secure.with_open_in_bin src In_channel.input_all in
          Secure.with_open_out_gen [ Open_wronly; Open_append; Open_binary ]
            0o644 dst (fun oc -> output_string oc s);
          Sys.remove src)
        else Sys.rename src dst
      with Sys_error _ | Unix.Unix_error _ | Filesystem.File_error _ -> ()

(* ************************************************************************ *)
(*  [Fonc] write_history_file : config -> string -> gen_record -> unit      *)

(* ************************************************************************ *)

(** [Description] : Enregistre la personne dans son fichier historique. [Args] :
    - fname : le chemin du fichier
    - gr : le contenu de la personne [Retour] : Néant [Rem] : Non exporté en
      clair hors de ce module. *)
let write_history_file conf base person_file fname gr =
  (* On créé toujours les dossiers nécessaires (changement de clé ...). *)
  let () = create_history_dirs conf person_file in
  let ext_flags =
    [ Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock ]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname)
    with Sys_error _ -> None
  with
  | Some oc ->
      output_record base oc gr;
      close_out oc
  | None -> ()

(* ************************************************************************ *)
(* [Fonc] make_gen_record :
            config -> base -> bool -> gen_person -> gen_record *)

(* ************************************************************************ *)

(** [Description] : Crée un gen_record à partir d'une personne. [Args] :
    - conf : configuratino de la base
    - base : base de donnée
    - first : booléen pour savoir si c'est la première entrée de l'historique.
      Si c'est le cas, on ne connait pas la date de modification, donc on met
      "environ" une seconde avant.
    - gen_p : gen_person [Retour] :
    - gen_record [Rem] : Non exporté en clair hors de ce module. *)
let make_gen_record conf base first gen_p =
  let date =
    let conf =
      (* On évite les calculs savant pour la date (ss - 1 avec une date *)
      (* autour de minuit ...). C'est simplement une indication.        *)
      if first then
        let hh, mm, ss = conf.time in
        { conf with time = (hh, mm, min 0 ss) }
      else conf
    in
    Util.sprintf_today conf
  in
  let p = Driver.poi base gen_p.key_index in
  let fam = Driver.get_family p in
  (* On fait en sorte qu'il y a une 'bijection' *)
  (* entre les familles et les enfants.         *)
  let gen_f, gen_c, gen_s =
    Array.fold_right
      (fun ifam (accu_fam, accu_child, accu_spouse) ->
        let fam = Driver.foi base ifam in
        let children = Driver.get_children fam in
        let gen_f = Driver.gen_family_of_family fam in
        ( Util.string_gen_family base gen_f :: accu_fam,
          children :: accu_child,
          Gutil.spouse gen_p.key_index fam :: accu_spouse ))
      fam ([], [], [])
  in
  {
    date;
    wizard = (Util.escape_html conf.user :> Adef.safe_string);
    gen_p;
    gen_f;
    gen_c;
    gen_s;
  }

let records conf base changed o p =
  let gr = make_gen_record conf base false p in
  let o_gr = make_gen_record conf base true o in
  let restore f o_f =
    let gen_f, rest =
      List.combine o_gr.gen_f (List.combine o_gr.gen_c o_gr.gen_s)
      |> List.filter_map (fun ((g, cs) as x) ->
          if Driver.Ifam.equal g.fam_index f.fam_index then
            Option.map (fun o_f -> (o_f, cs)) o_f
          else Some x)
      |> List.split
    in
    let gen_c, gen_s = List.split rest in
    { o_gr with gen_f; gen_c; gen_s }
  in
  let o_gr =
    match changed with
    | U_Add_family (_, f) | U_Add_parent (_, f) -> restore f None
    | U_Modify_family (_, o_f, f) | U_Merge_family (_, o_f, _, f) ->
        restore f (Some o_f)
    | _ -> o_gr
  in
  ((if { o_gr with date = gr.date } = gr then None else Some o_gr), gr)

let change_records conf base changed =
  let pair o p =
    let o_gr, gr = records conf base changed o p in
    Option.map (fun o_gr -> (o_gr, gr)) o_gr
  in
  match changed with
  | U_Modify_person (o, p) | U_Merge_person (o, _, p) | U_Multi (o, p) ->
      pair o p
  | U_Add_family (p, _)
  | U_Modify_family (p, _, _)
  | U_Merge_family (p, _, _, _)
  | U_Add_parent (p, _) ->
      pair p p
  | _ -> None

(* ************************************************************************ *)
(*  [Fonc] record_diff : config -> base -> base_changed -> unit             *)

(* ************************************************************************ *)

(** [Description] : Met à jour le fichier historique d'une personne. [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - changed : le type de modification (voir def.mli) [Retour] : Néant [Rem] :
      Exporté en clair hors de ce module. *)

let record_diff conf base changed =
  match List.assoc_opt "history_diff" conf.base_env with
  | Some "yes" when not conf.manitou -> (
      let write_change person_file fname o p =
        let o_gr, gr = records conf base changed o p in
        (match o_gr with
        | Some o_gr when not (Sys.file_exists fname) ->
            write_history_file conf base person_file fname o_gr
        | _ -> ());
        write_history_file conf base person_file fname gr
      in
      let print_ind_add p =
        let person_file = history_file p.first_name p.surname p.occ in
        let fname = history_path conf person_file in
        let gr = make_gen_record conf base false p in
        write_history_file conf base person_file fname gr
      in
      let print_ind_mod o p =
        let o_person_file = history_file o.first_name o.surname o.occ in
        let person_file = history_file p.first_name p.surname p.occ in
        let ofname = history_path conf o_person_file in
        let fname = history_path conf person_file in
        (* La clé a changé, on reprend l'ancien historique. *)
        (if
           o_person_file <> person_file
           && unique_key o.first_name o.surname
           && Sys.file_exists ofname
         then
           try
             let () = create_history_dirs conf person_file in
             Sys.rename ofname fname
           with Sys_error _ -> ());
        write_change person_file fname o p
      in
      match changed with
      | U_Add_person p -> print_ind_add p
      | U_Modify_person (o, p) | U_Merge_person (o, _, p) | U_Multi (o, p) ->
          print_ind_mod o p
      | U_Delete_person _ -> ()
      | U_Delete_family (_p, _f) -> ()
      | U_Add_family (p, f)
      | U_Modify_family (p, _, f)
      | U_Merge_family (p, _, _, f)
      | U_Add_parent (p, f) ->
          let p_file = history_file p.first_name p.surname p.occ in
          let p_fname = history_path conf p_file in
          let cpl = Driver.foi base f.fam_index in
          let isp = Gutil.spouse p.key_index cpl in
          let sp = Driver.poi base isp in
          let sp_file =
            history_file
              (Driver.sou base (Driver.get_first_name sp))
              (Driver.sou base (Driver.get_surname sp))
              (Driver.get_occ sp)
          in
          let sp_fname = history_path conf sp_file in
          let gen_sp = Driver.gen_person_of_person sp in
          let gen_sp = Util.string_gen_person base gen_sp in
          write_change p_file p_fname p p;
          write_change sp_file sp_fname gen_sp gen_sp;
          (* Création des fichiers pour les enfants ajoutés. *)
          Array.iter
            (fun ip ->
              let p = Driver.poi base ip in
              let person_file =
                history_file
                  (Driver.sou base (Driver.get_first_name p))
                  (Driver.sou base (Driver.get_surname p))
                  (Driver.get_occ p)
              in
              let fname = history_path conf person_file in
              if Sys.file_exists fname then ()
              else
                let gen_p = Driver.gen_person_of_person p in
                let gen_p = Util.string_gen_person base gen_p in
                let gr = make_gen_record conf base false gen_p in
                write_history_file conf base person_file fname gr)
            (Driver.get_children cpl)
      | U_Change_children_name (_, list) ->
          List.iter
            (fun ((ofn, osn, oocc, _oip), (fn, sn, occ, ip)) ->
              let o_person_file = history_file ofn osn oocc in
              let person_file = history_file fn sn occ in
              if o_person_file <> person_file then (
                let ofname = history_path conf o_person_file in
                let fname = history_path conf person_file in
                (if unique_key ofn osn then
                   try Sys.rename ofname fname with Sys_error _ -> ());
                let p = Driver.poi base ip in
                let p =
                  Futil.map_person_ps
                    (fun p -> p)
                    (Driver.sou base)
                    (Driver.gen_person_of_person p)
                in
                write_change person_file fname
                  { p with first_name = ofn; surname = osn; occ = oocc }
                  p))
            list
      | _ -> ())
  | _ -> ()

let read_entries path =
  let rec loop ic acc =
    match input_raw ic with
    | raw -> loop ic (entry_of_raw raw :: acc)
    | exception End_of_file -> acc
  in
  try Secure.with_open_in_bin path (fun ic -> loop ic [])
  with Sys_error _ -> []

let load_entries conf fname = read_entries (history_path conf fname)

let rewrite_entries path entries =
  if entries = [] then Mutil.rm path
  else
    try
      Secure.with_open_out_gen
        [ Open_wronly; Open_trunc; Open_creat; Open_binary; Open_nonblock ]
        0o644 path (fun oc ->
          List.iter (fun e -> output_raw oc e.e_raw) entries)
    with Sys_error _ -> ()

let write_entries conf fname entries =
  rewrite_entries (history_path conf fname) entries

let record_of_entry base entries e =
  let ki =
    match entries with
    | { e_raw = V0003 b; _ } :: _ ->
        let s : stored_record = Marshal.from_bytes b 0 in
        if unique_key s.s_p.first_name s.s_p.surname then
          find_key base s.s_p.first_name s.s_p.surname s.s_p.occ
        else Driver.Iper.dummy
    | _ -> Driver.Iper.dummy
  in
  record_of_raw base ki e.e_raw

let convert ~dry_run conf base =
  let legacy e =
    match e.e_raw with V0003 _ -> false | V0002 _ | V0024 _ -> true
  in
  let upgrade e =
    if legacy e then
      let gr = record_of_raw base Driver.Iper.dummy e.e_raw in
      { e with e_raw = V0003 (Marshal.to_bytes (stored_of_record base gr) []) }
    else e
  in
  let dir = history_d conf in
  if not (Sys.file_exists dir) then 0
  else
    Filesystem.walk_folder ~recursive:true
      (fun entry n ->
        match entry with
        | Filesystem.File path -> (
            match read_entries path with
            | exception Failure _ -> n
            | entries when List.exists legacy entries ->
                if not dry_run then
                  rewrite_entries path (List.rev_map upgrade entries);
                n + 1
            | _ -> n)
        | Filesystem.Dir _ | Filesystem.Exn _ -> n)
      dir 0
