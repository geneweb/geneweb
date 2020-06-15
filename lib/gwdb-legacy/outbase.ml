(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk

let load_ascends_array base = base.data.ascends.load_array ()
let load_unions_array base = base.data.unions.load_array ()
let load_couples_array base = base.data.couples.load_array ()
let load_descends_array base = base.data.descends.load_array ()
let load_strings_array base = base.data.strings.load_array ()
let close_base base = base.func.cleanup ()

let save_mem = ref false
let verbose = Mutil.verbose

let trace s =
  if !verbose then begin Printf.eprintf "*** %s\n" s; flush stderr end

let count_error computed found =
  Printf.eprintf "Count error. Computed %d. Found %d.\n" computed found;
  flush stderr;
  exit 2

let make_name_index base =
  let t = Array.make Dutil.table_size [| |] in
  let add_name key valu =
    let key = Name.crush (Name.abbrev key) in
    let i = Hashtbl.hash key mod Array.length t in
    if Array.mem valu t.(i) then ()
    else t.(i) <- Array.append [| valu |] t.(i)
  in
  let rec add_names ip =
    function
      [] -> ()
    | n :: nl -> add_name n ip; add_names ip nl
  in
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    let first_name = Dutil.p_first_name base p in
    let surname = Dutil.p_surname base p in
    if first_name <> "?" && surname <> "?" then
      let names =
        Name.lower (first_name ^ " " ^ surname) ::
        Dutil.dsk_person_misc_names base p (fun p -> p.titles)
      in
      add_names p.key_index names
  done;
  t

let create_name_index oc_inx oc_inx_acc base =
  let ni = make_name_index base in
  let bpos = pos_out oc_inx in
  Dutil.output_value_no_sharing oc_inx (ni : Dutil.name_index_data);
  let epos =
    Iovalue.output_array_access oc_inx_acc (Array.get ni) (Array.length ni)
      bpos
  in
  if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)

let add_name t key valu =
  let key = Name.crush_lower key in
  let i = Hashtbl.hash key mod Array.length t in
  if Array.mem valu t.(i) then () else t.(i) <- Array.append [| valu |] t.(i)

let make_strings_of_fsname base =
  let t = Array.make Dutil.table_size [| |] in
  for i = 0 to base.data.persons.len - 1 do
    let p = Dutil.poi base i in
    let first_name = Dutil.p_first_name base p in
    let surname = Dutil.p_surname base p in
    if first_name <> "?" then add_name t first_name p.first_name;
    if surname <> "?" then
      begin
        add_name t surname p.surname;
        List.iter (fun sp -> add_name t sp p.surname)
          (Mutil.surnames_pieces surname)
      end
  done;
  t

let create_strings_of_fsname oc_inx oc_inx_acc base =
  let t = make_strings_of_fsname base in
  let bpos = pos_out oc_inx in
  Dutil.output_value_no_sharing oc_inx (t : Dutil.strings_of_fsname);
  let epos =
    Iovalue.output_array_access oc_inx_acc (Array.get t) (Array.length t) bpos
  in
  if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)

let is_prime a =
  let rec loop b =
    if a / b < b then true else if a mod b = 0 then false else loop (b + 1)
  in
  loop 2

let rec prime_after n = if is_prime n then n else prime_after (n + 1)

let output_strings_hash oc2 base =
  let () = base.data.strings.load_array () in
  let strings_array = base.data.strings in
  let taba =
    Array.make
      (min Sys.max_array_length
         (prime_after (max 2 (10 * strings_array.len))))
      (-1)
  in
  let tabl = Array.make strings_array.len (-1) in
  for i = 0 to strings_array.len - 1 do
    let ia = Hashtbl.hash (base.data.strings.get i) mod Array.length taba in
    tabl.(i) <- taba.(ia); taba.(ia) <- i
  done;
  output_binary_int oc2 (Array.length taba);
  output_binary_int oc2 0;
  output_binary_int oc2 0;
  for i = 0 to Array.length taba - 1 do output_binary_int oc2 taba.(i) done;
  for i = 0 to Array.length tabl - 1 do output_binary_int oc2 tabl.(i) done

let output_name_index_aux get _oc base names_inx names_dat =
  let ht = Dutil.IntHT.create 0 in
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    let k = get p in
    match Dutil.IntHT.find_opt ht k with
    | Some list -> Dutil.IntHT.replace ht k (p.key_index :: list)
    | None -> Dutil.IntHT.add ht k [ p.key_index ]
  done ;
  let a = Array.make (Dutil.IntHT.length ht) (0, []) in
  ignore @@ Dutil.IntHT.fold (fun k v i -> Array.set a i (k, v) ; succ i) ht 0 ;
  Array.sort (fun (k, _) (k', _) -> Dutil.compare_istr_fun base.data k k') a ;
  let oc_n_dat = Secure.open_out_bin names_dat in
  let bt2 =
    Array.map begin fun (i, ipl) ->
      let off = pos_out oc_n_dat in
      output_binary_int oc_n_dat (List.length ipl) ;
      List.iter (output_binary_int oc_n_dat) ipl ;
      (i, off)
    end a
  in
  close_out oc_n_dat ;
  let oc_n_inx = Secure.open_out_bin names_inx in
  Dutil.output_value_no_sharing oc_n_inx (bt2 : (int * int) array) ;
  close_out oc_n_inx

let output_surname_index oc2 base tmp_snames_inx tmp_snames_dat =
  output_name_index_aux (fun p -> p.surname) oc2 base tmp_snames_inx tmp_snames_dat

let output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat =
  output_name_index_aux (fun p -> p.first_name) oc2 base tmp_fnames_inx tmp_fnames_dat

let output_particles_file particles fname =
  let oc = open_out fname in
  List.iter begin fun s ->
    Printf.fprintf oc "%s\n" (Mutil.tr ' ' '_' s)
  end particles ;
  close_out oc

let output base =
  let bname = base.data.bdir in
  if not (Sys.file_exists bname) then Unix.mkdir bname 0o755 ;
  let tmp_particles = Filename.concat bname "1particles.txt" in
  let tmp_base = Filename.concat bname "1base" in
  let tmp_base_acc = Filename.concat bname "1base.acc" in
  let tmp_names_inx = Filename.concat bname "1names.inx" in
  let tmp_names_acc = Filename.concat bname "1names.acc" in
  let tmp_snames_inx = Filename.concat bname "1snames.inx" in
  let tmp_snames_dat = Filename.concat bname "1snames.dat" in
  let tmp_fnames_inx = Filename.concat bname "1fnames.inx" in
  let tmp_fnames_dat = Filename.concat bname "1fnames.dat" in
  let tmp_strings_inx = Filename.concat bname "1strings.inx" in
  let tmp_notes = Filename.concat bname "1notes" in
  let tmp_notes_d = Filename.concat bname "1notes_d" in
  load_ascends_array base;
  load_unions_array base;
  load_couples_array base;
  load_descends_array base;
  load_strings_array base;
  let oc = Secure.open_out_bin tmp_base in
  let oc_acc = Secure.open_out_bin tmp_base_acc in
  let output_array arrname arr =
    let bpos = pos_out oc in
    if !verbose then Printf.eprintf "*** saving %s array\n" arrname;
    flush stderr;
    arr.output_array oc;
    let epos = Iovalue.output_array_access oc_acc arr.get arr.len bpos in
    if epos <> pos_out oc then count_error epos (pos_out oc)
  in
  begin try
      output_string oc Dutil.magic_GnWb0021;
      output_binary_int oc base.data.persons.len;
      output_binary_int oc base.data.families.len;
      output_binary_int oc base.data.strings.len;
      let array_start_indexes = pos_out oc in
      output_binary_int oc 0;
      output_binary_int oc 0;
      output_binary_int oc 0;
      output_binary_int oc 0;
      output_binary_int oc 0;
      output_binary_int oc 0;
      output_binary_int oc 0;
      Dutil.output_value_no_sharing oc (base.data.bnotes.Def.norigin_file : string);
      let persons_array_pos = pos_out oc in
      output_array "persons" base.data.persons;
      let ascends_array_pos = pos_out oc in
      output_array "ascends" base.data.ascends;
      let unions_array_pos = pos_out oc in
      output_array "unions" base.data.unions;
      let families_array_pos = pos_out oc in
      output_array "families" base.data.families;
      let couples_array_pos = pos_out oc in
      output_array "couples" base.data.couples;
      let descends_array_pos = pos_out oc in
      output_array "descends" base.data.descends;
      let strings_array_pos = pos_out oc in
      output_array "strings" base.data.strings;
      seek_out oc array_start_indexes;
      output_binary_int oc persons_array_pos;
      output_binary_int oc ascends_array_pos;
      output_binary_int oc unions_array_pos;
      output_binary_int oc families_array_pos;
      output_binary_int oc couples_array_pos;
      output_binary_int oc descends_array_pos;
      output_binary_int oc strings_array_pos;
      base.data.families.clear_array ();
      base.data.descends.clear_array ();
      close_out oc;
      close_out oc_acc;
      begin let oc_inx = Secure.open_out_bin tmp_names_inx in
        let oc_inx_acc = Secure.open_out_bin tmp_names_acc in
        let oc2 = Secure.open_out_bin tmp_strings_inx in
        try
          trace "create name index";
          output_binary_int oc_inx 0;
          create_name_index oc_inx oc_inx_acc base;
          base.data.ascends.clear_array ();
          base.data.unions.clear_array ();
          base.data.couples.clear_array ();
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let surname_or_first_name_pos = pos_out oc_inx in
          trace "create strings of fsname";
          create_strings_of_fsname oc_inx oc_inx_acc base;
          seek_out oc_inx 0;
          output_binary_int oc_inx surname_or_first_name_pos;
          close_out oc_inx;
          close_out oc_inx_acc;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          trace "create string index";
          output_strings_hash oc2 base;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let surname_pos = pos_out oc2 in
          trace "create surname index";
          output_surname_index oc2 base tmp_snames_inx tmp_snames_dat;
          if !save_mem then begin trace "compacting"; Gc.compact () end;
          let first_name_pos = pos_out oc2 in
          trace "create first name index";
          output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat;
          seek_out oc2 Dutil.int_size;
          output_binary_int oc2 surname_pos;
          output_binary_int oc2 first_name_pos;
          let s = base.data.bnotes.Def.nread "" Def.RnAll in
          if s = "" then ()
          else
            begin let oc_not = Secure.open_out tmp_notes in
              output_string oc_not s; close_out oc_not
            end;
          close_out oc2;
          List.iter
            (fun f ->
               let s = base.data.bnotes.Def.nread f Def.RnAll in
               let fname = Filename.concat tmp_notes_d (f ^ ".txt") in
               Mutil.mkdir_p (Filename.dirname fname);
               let oc = open_out fname in output_string oc s; close_out oc)
            (List.rev (base.data.bnotes.Def.efiles ()));
          output_particles_file base.data.particles tmp_particles
        with e ->
          (try close_out oc_inx with _ -> ());
          (try close_out oc_inx_acc with _ -> ());
          (try close_out oc2 with _ -> ());
          raise e
      end;
      trace "ok" ;
      let nbp =
        let rec loop i acc =
          if i = base.data.persons.len then acc
          else
            let p = base.data.persons.get i in
            let acc =
              if p.key_index = -1
              || ( (0 = p.surname || 1 = p.surname)
                   && (0 = p.first_name || 1 = p.first_name) )
              then acc
              else acc + 1
            in
            loop (i + 1) acc
        in loop 0 0
      in
      let oc = Secure.open_out_bin @@ Filename.concat bname "nb_persons" in
      output_value oc nbp ;
      close_out oc ;
    with e ->
      (try close_out oc with _ -> ());
      (try close_out oc_acc with _ -> ());
      Mutil.rm tmp_base;
      Mutil.rm tmp_base_acc;
      begin
        Mutil.rm tmp_names_inx;
        Mutil.rm tmp_names_acc;
        Mutil.rm tmp_strings_inx;
        Mutil.remove_dir tmp_notes_d
      end;
      raise e
  end;
  close_base base;
  Mutil.rm (Filename.concat bname "base");
  Sys.rename tmp_base (Filename.concat bname "base");
  Mutil.rm (Filename.concat bname "base.acc");
  Sys.rename tmp_base_acc (Filename.concat bname "base.acc");
  Mutil.rm (Filename.concat bname "names.inx");
  Sys.rename tmp_names_inx (Filename.concat bname "names.inx");
  Mutil.rm (Filename.concat bname "names.acc");
  Sys.rename tmp_names_acc (Filename.concat bname "names.acc");
  Mutil.rm (Filename.concat bname "snames.dat");
  Sys.rename tmp_snames_dat (Filename.concat bname "snames.dat");
  Mutil.rm (Filename.concat bname "snames.inx");
  Sys.rename tmp_snames_inx (Filename.concat bname "snames.inx");
  Mutil.rm (Filename.concat bname "fnames.dat");
  Sys.rename tmp_fnames_dat (Filename.concat bname "fnames.dat");
  Mutil.rm (Filename.concat bname "fnames.inx");
  Sys.rename tmp_fnames_inx (Filename.concat bname "fnames.inx");
  Mutil.rm (Filename.concat bname "strings.inx");
  Sys.rename tmp_strings_inx (Filename.concat bname "strings.inx");
  Sys.rename tmp_particles (Filename.concat bname "particles.txt") ;
  Mutil.rm (Filename.concat bname "notes");
  if Sys.file_exists tmp_notes then
    Sys.rename tmp_notes (Filename.concat bname "notes");
  if Sys.file_exists tmp_notes_d then
    begin let notes_d = Filename.concat bname "notes_d" in
      Mutil.remove_dir notes_d; Sys.rename tmp_notes_d notes_d
    end;
  Mutil.rm (Filename.concat bname "patches");
  Mutil.rm (Filename.concat bname "patches~");
  Mutil.rm (Filename.concat bname "restrict") ;
  Mutil.rm (Filename.concat bname "tstab_visitor");
  Mutil.rm (Filename.concat bname "nb_persons");
  (* FIXME: should not be present in this part of the code? *)
  Mutil.rm (Filename.concat bname "tstab");
  Mutil.rm (Filename.concat bname "tstab_visitor")
