(* $Id: outbase.ml,v 5.21 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Def

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

let just_copy bname what oc oc_acc =
  if !verbose then Printf.eprintf "*** copying %s\n" what;
  flush stderr;
  let ic =
    let ic = Secure.open_in_bin (Path.path_from_bname bname).Path.file_base in
    Dutil.check_magic ic; ic
  in
  let ic_acc = Secure.open_in_bin (Path.path_from_bname bname).Path.file_base_acc in
  let persons_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let unions_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let descends_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let _ = input_value ic in
  let (beg_pos, end_pos, beg_acc_pos, array_len) =
    match what with
      "persons" ->
        let pos = 0 in persons_array_pos, ascends_array_pos, pos, persons_len
    | "ascends" ->
        let pos = persons_len * Iovalue.sizeof_long in
        ascends_array_pos, unions_array_pos, pos, persons_len
    | "unions" ->
        let pos = 2 * persons_len * Iovalue.sizeof_long in
        unions_array_pos, families_array_pos, pos, persons_len
    | "families" ->
        let pos = 3 * persons_len * Iovalue.sizeof_long in
        families_array_pos, couples_array_pos, pos, families_len
    | "couples" ->
        let pos = (3 * persons_len + families_len) * Iovalue.sizeof_long in
        couples_array_pos, descends_array_pos, pos, families_len
    | "descends" ->
        let pos =
          (3 * persons_len + 2 * families_len) * Iovalue.sizeof_long
        in
        descends_array_pos, strings_array_pos, pos, families_len
    | "strings" ->
        let pos =
          (3 * persons_len + 3 * families_len) * Iovalue.sizeof_long
        in
        strings_array_pos, in_channel_length ic, pos, strings_len
    | _ -> failwith ("just copy " ^ what)
  in
  let shift = pos_out oc - beg_pos in
  seek_in ic beg_pos;
  let rec loop pos =
    if pos = end_pos then close_in ic
    else begin output_char oc (input_char ic); loop (pos + 1) end
  in
  loop beg_pos;
  seek_in ic_acc beg_acc_pos;
  let rec loop len =
    if len = array_len then close_in ic_acc
    else
      begin
        output_binary_int oc_acc (input_binary_int ic_acc + shift);
        loop (len + 1)
      end
  in
  loop 0

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
  Mutil.output_value_no_sharing oc_inx (ni : Dutil.name_index_data);
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
    let p = Dutil.poi base (Adef.iper_of_int i) in
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
  Mutil.output_value_no_sharing oc_inx (t : Dutil.strings_of_fsname);
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

let output_surname_index oc2 base tmp_snames_inx tmp_snames_dat =
  let module IstrTree =
    Btree.Make
      (struct
        type t = dsk_istr
        let compare = Dutil.compare_istr_fun base.data
      end)
  in
  let bt = ref IstrTree.empty in
  for i = 0 to base.data.persons.len - 1 do
    let p = Dutil.poi base (Adef.iper_of_int i) in
    let a = try IstrTree.find p.surname !bt with Not_found -> [] in
    bt := IstrTree.add p.surname (p.key_index :: a) !bt
  done;
  (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
     i.e. the created database can be still read by these versions but this
     table will not be used in versions >= 4.10 *)
  Mutil.output_value_no_sharing oc2 (!bt : iper list IstrTree.t);
  (* new table created from version >= 4.10 *)
  let oc_sn_dat = tmp_snames_dat in
  let bt2 =
    IstrTree.map
      (fun ipl ->
         let i = pos_out oc_sn_dat in
         output_binary_int oc_sn_dat (List.length ipl);
         List.iter
           (fun ip -> output_binary_int oc_sn_dat (Adef.int_of_iper ip)) ipl;
         i)
      !bt
  in
  close_out oc_sn_dat;
  let oc_sn_inx = tmp_snames_inx in
  Mutil.output_value_no_sharing oc_sn_inx (bt2 : int IstrTree.t);
  close_out oc_sn_inx

let output_first_name_index oc2 base oc_fn_inx oc_fn_dat =
  let module IstrTree =
    Btree.Make
      (struct
        type t = dsk_istr
        let compare = Dutil.compare_istr_fun base.data
      end)
  in
  let bt = ref IstrTree.empty in
  for i = 0 to base.data.persons.len - 1 do
    let p = Dutil.poi base (Adef.iper_of_int i) in
    let a = try IstrTree.find p.first_name !bt with Not_found -> [] in
    bt := IstrTree.add p.first_name (p.key_index :: a) !bt
  done;
  (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
     i.e. the created database can be still read by these versions but this
     table will not be used in versions >= 4.10 *)
  Mutil.output_value_no_sharing oc2 (!bt : iper list IstrTree.t);
  (* new table created from version >= 4.10 *)
  let bt2 =
    IstrTree.map
      (fun ipl ->
         let i = pos_out oc_fn_dat in
         output_binary_int oc_fn_dat (List.length ipl);
         List.iter
           (fun ip -> output_binary_int oc_fn_dat (Adef.int_of_iper ip)) ipl;
         i)
      !bt
  in
  close_out oc_fn_dat;
  Mutil.output_value_no_sharing oc_fn_inx (bt2 : int IstrTree.t);
  close_out oc_fn_inx

let gen_output no_patches bname base =
  let open Path in
  let tmp fname = Filename.open_temp_file ~mode:[ Open_binary ] ~perms:0o644 fname "" in
  let path = path_from_bname bname in
  Mutil.mkdir_p path.dir_root ;
  let tmp_base_fn, tmp_base_oc =  tmp "base" in
  let tmp_base_acc_fn, tmp_base_acc_oc = tmp "base.acc" in
  let tmp_names_inx_fn, tmp_names_inx_oc = tmp "names.inx" in
  let tmp_names_acc_fn, tmp_names_acc_oc = tmp "names.acc" in
  let tmp_snames_inx_fn, tmp_snames_inx_oc = tmp "snames.inx" in
  let tmp_snames_dat_fn, tmp_snames_dat_oc = tmp "snames.dat" in
  let tmp_fnames_inx_fn, tmp_fnames_inx_oc = tmp "fnames.inx" in
  let tmp_fnames_dat_fn, tmp_fnames_dat_oc = tmp "fnames.dat" in
  let tmp_strings_inx_fn, tmp_strings_inx_oc = tmp "strings.inx" in
  let tmp_notes = Filename.concat path.dir_my_base "notes.tmp" in
  let tmp_notes_d = Filename.concat path.dir_my_base "notes_d.tmp" in
  if not no_patches then
    begin
      load_ascends_array base;
      load_unions_array base;
      load_couples_array base;
      load_descends_array base;
      load_strings_array base
    end;
  let output_array arrname arr =
    let bpos = pos_out tmp_base_oc in
    if !verbose then Printf.eprintf "*** saving %s array\n" arrname;
    flush stderr;
    arr.output_array tmp_base_oc;
    let epos = Iovalue.output_array_access tmp_base_acc_oc arr.get arr.len bpos in
    if epos <> pos_out tmp_base_oc then count_error epos (pos_out tmp_base_oc)
  in
  begin try
    output_string tmp_base_oc Dutil.magic_gwb;
    output_binary_int tmp_base_oc base.data.persons.len;
    output_binary_int tmp_base_oc base.data.families.len;
    output_binary_int tmp_base_oc base.data.strings.len;
    let array_start_indexes = pos_out tmp_base_oc in
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    output_binary_int tmp_base_oc 0;
    Mutil.output_value_no_sharing tmp_base_oc (base.data.bnotes.norigin_file : string);
    let persons_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "persons" base.data.persons
    else just_copy bname "persons" tmp_base_oc tmp_base_acc_oc;
    let ascends_array_pos = pos_out tmp_base_oc in
    if not no_patches then () else trace "saving ascends";
    output_array "ascends" base.data.ascends;
    let unions_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "unions" base.data.unions
    else just_copy bname "unions" tmp_base_oc tmp_base_acc_oc;
    let families_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "families" base.data.families
    else just_copy bname "families" tmp_base_oc tmp_base_acc_oc;
    let couples_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "couples" base.data.couples
    else just_copy bname "couples" tmp_base_oc tmp_base_acc_oc;
    let descends_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "descends" base.data.descends
    else just_copy bname "descends" tmp_base_oc tmp_base_acc_oc;
    let strings_array_pos = pos_out tmp_base_oc in
    if not no_patches then output_array "strings" base.data.strings
    else just_copy bname "strings" tmp_base_oc tmp_base_acc_oc;
    seek_out tmp_base_oc array_start_indexes;
    output_binary_int tmp_base_oc persons_array_pos;
    output_binary_int tmp_base_oc ascends_array_pos;
    output_binary_int tmp_base_oc unions_array_pos;
    output_binary_int tmp_base_oc families_array_pos;
    output_binary_int tmp_base_oc couples_array_pos;
    output_binary_int tmp_base_oc descends_array_pos;
    output_binary_int tmp_base_oc strings_array_pos;
    base.data.families.clear_array ();
    base.data.descends.clear_array ();
    close_out tmp_base_oc;
    close_out tmp_base_acc_oc;
    if not no_patches then begin try
        trace "create name index";
        output_binary_int tmp_names_inx_oc 0;
        create_name_index tmp_names_inx_oc tmp_names_acc_oc base;
        base.data.ascends.clear_array ();
        base.data.unions.clear_array ();
        base.data.couples.clear_array ();
        if !save_mem then begin trace "compacting"; Gc.compact () end;
        let surname_or_first_name_pos = pos_out tmp_names_inx_oc in
        trace "create strings of fsname";
        create_strings_of_fsname tmp_names_inx_oc tmp_names_acc_oc base;
        seek_out tmp_names_inx_oc 0;
        output_binary_int tmp_names_inx_oc surname_or_first_name_pos;
        close_out tmp_names_inx_oc;
        close_out tmp_names_acc_oc;
        if !save_mem then begin trace "compacting"; Gc.compact () end;
        trace "create string index";
        output_strings_hash tmp_strings_inx_oc base;
        if !save_mem then begin trace "compacting"; Gc.compact () end;
        let surname_pos = pos_out tmp_strings_inx_oc in
        trace "create surname index";
        output_surname_index tmp_strings_inx_oc base tmp_snames_inx_oc tmp_snames_dat_oc;
        if !save_mem then begin trace "compacting"; Gc.compact () end;
        let first_name_pos = pos_out tmp_strings_inx_oc in
        trace "create first name index";
        output_first_name_index tmp_strings_inx_oc base tmp_fnames_inx_oc tmp_fnames_dat_oc;
        seek_out tmp_strings_inx_oc Mutil.int_size;
        output_binary_int tmp_strings_inx_oc surname_pos;
        output_binary_int tmp_strings_inx_oc first_name_pos;
        (*  REORG *)
        List.iter
          (fun f ->
             let s = base.data.bnotes.nread f RnAll in
             let fname = Filename.concat tmp_notes_d (f ^ ".txt") in
             Mutil.mkdir_p (Filename.dirname fname);
             let oc = open_out fname in output_string oc s; close_out oc)
          (List.rev (base.data.bnotes.efiles ())) ;
        let s = base.data.bnotes.nread "" RnAll in
        if s = "" then ()
        else
        begin let oc_not = Secure.open_out tmp_notes in
        output_string oc_not s; close_out oc_not
        end ;
        close_out tmp_strings_inx_oc ;
      with e ->
        (try close_out tmp_names_inx_oc with _ -> ()) ;
        (try close_out tmp_names_acc_oc with _ -> ()) ;
        (try close_out tmp_strings_inx_oc with _ -> ()) ;
        raise e
    end;
    trace "ok"
  with e ->
    (try close_out tmp_base_oc with _ -> ()) ;
    (try close_out tmp_base_acc_oc with _ -> ()) ;
    Mutil.rm tmp_base_fn ;
    Mutil.rm tmp_base_acc_fn ;
    Mutil.rm tmp_names_inx_fn ;
    Mutil.rm tmp_names_acc_fn ;
    Mutil.rm tmp_strings_inx_fn ;
    Mutil.rm_rf tmp_notes_d ;
    raise e
  end;
  close_base base;
  Mutil.rm path.file_base ;
  Mutil.rn tmp_base_fn path.file_base ;
  Mutil.rm path.file_base_acc;
  Mutil.rn tmp_base_acc_fn path.file_base_acc;
  if not no_patches then
    begin
      let mv src dst = Mutil.rm dst ; Sys.rename src dst in
      mv tmp_names_inx_fn path.file_names_inx ;
      mv tmp_names_acc_fn path.file_names_acc ;
      mv tmp_snames_dat_fn path.file_snames_dat ;
      mv tmp_snames_inx_fn path.file_snames_inx ;
      mv tmp_fnames_dat_fn path.file_fnames_dat ;
      mv tmp_fnames_inx_fn path.file_fnames_inx ;
      mv tmp_strings_inx_fn path.file_strings_inx ;
      (* REORG *)
      if Sys.file_exists tmp_notes_d then
        begin let notes_d = path.dir_notes in
          Mutil.rm_rf notes_d ;
          Mutil.rn tmp_notes_d notes_d
        end ;
      if Sys.file_exists tmp_notes then
        begin 
          Mutil.rm path.file_notes ;
          Mutil.rn tmp_notes path.file_notes
        end ;
      Mutil.rm path.file_patches ;
      Mutil.rm path.file_ts ;
      Mutil.rm path.file_ts_visitor ;
      Mutil.rm path.file_restrict
    end

let output = gen_output false
