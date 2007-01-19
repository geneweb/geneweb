(* $Id: outbase.ml,v 5.21 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk;
open Dutil;
open Def;
open Mutil;

value load_ascends_array base = base.data.ascends.load_array ();
value load_unions_array base = base.data.unions.load_array ();
value load_couples_array base = base.data.couples.load_array ();
value load_descends_array base = base.data.descends.load_array ();
value load_strings_array base = base.data.strings.load_array ();
value close_base base = base.func.cleanup ();

value save_mem = ref False;

value trace s =
  if verbose.val then do { Printf.eprintf "*** %s\n" s; flush stderr }
  else ()
;

value count_error computed found =
  do {
    Printf.eprintf "Count error. Computed %d. Found %d.\n" computed found;
    flush stderr;
    exit 2
  }
;

value just_copy bname what oc oc_acc =
  do {
    Printf.eprintf "*** copying %s\n" what;
    flush stderr;
    let ic =
      let ic = Secure.open_in_bin (Filename.concat bname "base") in
      do { check_magic ic; ic }
    in
    let ic_acc = Secure.open_in_bin (Filename.concat bname "base.acc") in
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
    let _ (*norigin_file*) = input_value ic in
    let (beg_pos, end_pos, beg_acc_pos, array_len) =
      match what with
      [ "persons" ->
          let pos = 0 in
          (persons_array_pos, ascends_array_pos, pos, persons_len)
      | "ascends" ->
          let pos = persons_len * Iovalue.sizeof_long in
          (ascends_array_pos, unions_array_pos, pos, persons_len)
      | "unions" ->
          let pos = 2 * persons_len * Iovalue.sizeof_long in
          (unions_array_pos, families_array_pos, pos, persons_len)
      | "families" ->
          let pos = 3 * persons_len * Iovalue.sizeof_long in
          (families_array_pos, couples_array_pos, pos, families_len)
      | "couples" ->
          let pos = (3 * persons_len + families_len) * Iovalue.sizeof_long in
          (couples_array_pos, descends_array_pos, pos, families_len)
      | "descends" ->
          let pos =
            (3 * persons_len + 2 * families_len) * Iovalue.sizeof_long
          in
          (descends_array_pos, strings_array_pos, pos, families_len)
      | "strings" ->
          let pos =
            (3 * persons_len + 3 * families_len) * Iovalue.sizeof_long
          in
          (strings_array_pos, in_channel_length ic, pos, strings_len)
      | _ -> failwith ("just copy " ^ what) ]
    in
    let shift = pos_out oc - beg_pos in
    seek_in ic beg_pos;
    let rec loop pos =
      if pos = end_pos then close_in ic
      else do { output_char oc (input_char ic); loop (pos + 1) }
    in
    loop beg_pos;
    seek_in ic_acc beg_acc_pos;
    let rec loop len =
      if len = array_len then close_in ic_acc
      else do {
        output_binary_int oc_acc (input_binary_int ic_acc + shift);
        loop (len + 1)
      }
    in
    loop 0;
  }
;

value make_name_index base =
  let t = Array.create table_size [| |] in
  let add_name key valu =
    let key = Name.crush (Name.abbrev key) in
    let i = Hashtbl.hash key mod Array.length t in
    if array_mem valu t.(i) then ()
    else t.(i) := Array.append [| valu |] t.(i)
  in
  let rec add_names ip =
    fun
    [ [] -> ()
    | [n :: nl] -> do { add_name n ip; add_names ip nl } ]
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" && surname <> "?" then
        let names =
          [Name.lower (first_name ^ " " ^ surname) ::
           Dutil.dsk_person_misc_names base p (fun p -> p.titles)]
        in
        add_names p.key_index names
      else ();
    };
    t
  }
;

value create_name_index oc_inx oc_inx_acc base =
  let ni = make_name_index base in
  let bpos = pos_out oc_inx in
  do {
    output_value_no_sharing oc_inx (ni : name_index_data);
    let epos =
      Iovalue.output_array_access oc_inx_acc (Array.get ni) (Array.length ni)
        bpos
    in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value add_name t key valu =
  let key = Name.crush_lower key in
  let i = Hashtbl.hash key mod Array.length t in
  if array_mem valu t.(i) then ()
  else t.(i) := Array.append [| valu |] t.(i)
;

value make_strings_of_fsname base =
  let t = Array.create table_size [| |] in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" then add_name t first_name p.first_name else ();
      if surname <> "?" then do {
        add_name t surname p.surname;
        List.iter (fun sp -> add_name t sp p.surname)
          (surnames_pieces surname);
      }
      else ();
    };
    t
  }
;

value create_strings_of_fsname oc_inx oc_inx_acc base =
  let t = make_strings_of_fsname base in
  let bpos = pos_out oc_inx in
  do {
    output_value_no_sharing oc_inx (t : strings_of_fsname);
    let epos =
      Iovalue.output_array_access oc_inx_acc (Array.get t) (Array.length t)
        bpos
    in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value is_prime a =
  loop 2 where rec loop b =
    if a / b < b then True else if a mod b = 0 then False else loop (b + 1)
;

value rec prime_after n = if is_prime n then n else prime_after (n + 1);

value output_strings_hash oc2 base =
  let () = base.data.strings.load_array () in
  let strings_array = base.data.strings in
  let taba =
    Array.create
      (min Sys.max_array_length
        (prime_after (max 2 (10 * strings_array.len))))
      (-1)
  in
  let tabl = Array.create strings_array.len (-1) in
  do {
    for i = 0 to strings_array.len - 1 do {
      let ia = Hashtbl.hash (base.data.strings.get i) mod Array.length taba in
      tabl.(i) := taba.(ia);
      taba.(ia) := i;
    };
    output_binary_int oc2 (Array.length taba);
    output_binary_int oc2 0;
    output_binary_int oc2 0;
    for i = 0 to Array.length taba - 1 do {
      output_binary_int oc2 taba.(i)
    };
    for i = 0 to Array.length tabl - 1 do {
      output_binary_int oc2 tabl.(i)
    };
  }
;

value output_surname_index oc2 base tmp_snames_inx tmp_snames_dat =
  let module IstrTree =
    Btree.Make
      (struct
         type t = dsk_istr;
         value compare = compare_istr_fun base.data;
       end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let a =
        try IstrTree.find p.surname bt.val with [ Not_found -> [] ]
      in
      bt.val := IstrTree.add p.surname [p.key_index :: a] bt.val
    };
    (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
       i.e. the created database can be still read by these versions but this
       table will not be used in versions >= 4.10 *)
    output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
    (* new table created from version >= 4.10 *)
    let oc_sn_dat = Secure.open_out_bin tmp_snames_dat in
    let bt2 =
      IstrTree.map
        (fun ipl ->
           let i = pos_out oc_sn_dat in
           do {
             output_binary_int oc_sn_dat (List.length ipl);
             List.iter
               (fun ip -> output_binary_int oc_sn_dat (Adef.int_of_iper ip))
               ipl;
             i
           })
        bt.val
    in
    close_out oc_sn_dat;
    let oc_sn_inx = Secure.open_out_bin tmp_snames_inx in
    output_value_no_sharing oc_sn_inx (bt2 : IstrTree.t int);
    close_out oc_sn_inx;
  }
;

value output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat =
  let module IstrTree =
    Btree.Make
      (struct
         type t = dsk_istr;
         value compare = compare_istr_fun base.data;
       end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let a =
        try IstrTree.find p.first_name bt.val with [ Not_found -> [] ]
      in
      bt.val := IstrTree.add p.first_name [p.key_index :: a] bt.val
    };
    (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
       i.e. the created database can be still read by these versions but this
       table will not be used in versions >= 4.10 *)
    output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
    (* new table created from version >= 4.10 *)
    let oc_fn_dat = Secure.open_out_bin tmp_fnames_dat in
    let bt2 =
      IstrTree.map
        (fun ipl ->
           let i = pos_out oc_fn_dat in
           do {
             output_binary_int oc_fn_dat (List.length ipl);
             List.iter
               (fun ip -> output_binary_int oc_fn_dat (Adef.int_of_iper ip))
               ipl;
             i
           })
        bt.val
    in
    close_out oc_fn_dat;
    let oc_fn_inx = Secure.open_out_bin tmp_fnames_inx in
    output_value_no_sharing oc_fn_inx (bt2 : IstrTree.t int);
    close_out oc_fn_inx;
  }
;

value gen_output no_patches bname base =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  do {
    try Unix.mkdir bname 0o755 with _ -> ();
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
    if not no_patches then do {
      load_ascends_array base;
      load_unions_array base;
      load_couples_array base;
      load_descends_array base;
      load_strings_array base;
    }
    else ();
    let oc = Secure.open_out_bin tmp_base in
    let oc_acc = Secure.open_out_bin tmp_base_acc in
    let output_array arrname arr =
      let bpos = pos_out oc in
      do {
        Printf.eprintf "*** saving %s array\n" arrname;
        flush stderr;
        arr.output_array oc;
        let epos = Iovalue.output_array_access oc_acc arr.get arr.len bpos in
        if epos <> pos_out oc then count_error epos (pos_out oc) else ()
      }
    in
    try
      do {
        output_string oc
          (if utf_8_db.val then magic_gwb else magic_gwb_iso_8859_1);
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
        output_value_no_sharing oc (base.data.bnotes.norigin_file : string);
        let persons_array_pos = pos_out oc in
        if not no_patches then output_array "persons" base.data.persons
        else just_copy bname "persons" oc oc_acc;
        let ascends_array_pos = pos_out oc in
        if not no_patches then () else trace "saving ascends";
        output_array "ascends" base.data.ascends;
        let unions_array_pos = pos_out oc in
        if not no_patches then output_array "unions" base.data.unions
        else just_copy bname "unions" oc oc_acc;
        let families_array_pos = pos_out oc in
        if not no_patches then output_array "families" base.data.families
        else just_copy bname "families" oc oc_acc;
        let couples_array_pos = pos_out oc in
        if not no_patches then output_array "couples" base.data.couples
        else just_copy bname "couples" oc oc_acc;
        let descends_array_pos = pos_out oc in
        if not no_patches then output_array "descends" base.data.descends
        else just_copy bname "descends" oc oc_acc;
        let strings_array_pos = pos_out oc in
        if not no_patches then output_array "strings" base.data.strings
        else just_copy bname "strings" oc oc_acc;
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
        if not no_patches then
          let oc_inx = Secure.open_out_bin tmp_names_inx in
          let oc_inx_acc = Secure.open_out_bin tmp_names_acc in
          let oc2 = Secure.open_out_bin tmp_strings_inx in
          try
            do {
              trace "create name index";
              output_binary_int oc_inx 0;
              create_name_index oc_inx oc_inx_acc base;
              base.data.ascends.clear_array ();
              base.data.unions.clear_array ();
              base.data.couples.clear_array ();
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              let surname_or_first_name_pos = pos_out oc_inx in
              trace "create strings of fsname";
              create_strings_of_fsname oc_inx oc_inx_acc base;
              seek_out oc_inx 0;
              output_binary_int oc_inx surname_or_first_name_pos;
              close_out oc_inx;
              close_out oc_inx_acc;
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              trace "create string index";
              output_strings_hash oc2 base;
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              let surname_pos = pos_out oc2 in
              trace "create surname index";
              output_surname_index oc2 base tmp_snames_inx tmp_snames_dat;
              if save_mem.val then do {
                trace "compacting"; Gc.compact ()
              }
              else ();
              let first_name_pos = pos_out oc2 in
              trace "create first name index";
              output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat;
              seek_out oc2 int_size;
              output_binary_int oc2 surname_pos;
              output_binary_int oc2 first_name_pos;
              let s = base.data.bnotes.nread "" RnAll in
              if s = "" then ()
              else do {
                let oc_not = Secure.open_out tmp_notes in
                output_string oc_not s;
                close_out oc_not;
              };
              close_out oc2;
              List.iter
                (fun f ->
                   let s = base.data.bnotes.nread f RnAll in
                   let fname = Filename.concat tmp_notes_d (f ^ ".txt") in
                   do {
                     mkdir_p (Filename.dirname fname);
                     let oc = open_out fname in
                     output_string oc s;
                     close_out oc;
                   })
                (List.rev (base.data.bnotes.efiles ()));
            }
          with e ->
            do {
              try close_out oc_inx with _ -> ();
              try close_out oc_inx_acc with _ -> ();
              try close_out oc2 with _ -> ();
              raise e
            }
        else ();
        trace "ok";
      }
    with e ->
      do {
        try close_out oc with _ -> ();
        try close_out oc_acc with _ -> ();
        remove_file tmp_base;
        remove_file tmp_base_acc;
        if not no_patches then do {
          remove_file tmp_names_inx;
          remove_file tmp_names_acc;
          remove_file tmp_strings_inx;
          remove_dir tmp_notes_d;
        }
        else ();
        raise e
      };
    close_base base;
    remove_file (Filename.concat bname "base");
    Sys.rename tmp_base (Filename.concat bname "base");
    remove_file (Filename.concat bname "base.acc");
    Sys.rename tmp_base_acc (Filename.concat bname "base.acc");
    if not no_patches then do {
      remove_file (Filename.concat bname "names.inx");
      Sys.rename tmp_names_inx (Filename.concat bname "names.inx");
      remove_file (Filename.concat bname "names.acc");
      Sys.rename tmp_names_acc (Filename.concat bname "names.acc");
      remove_file (Filename.concat bname "snames.dat");
      Sys.rename tmp_snames_dat (Filename.concat bname "snames.dat");
      remove_file (Filename.concat bname "snames.inx");
      Sys.rename tmp_snames_inx (Filename.concat bname "snames.inx");
      remove_file (Filename.concat bname "fnames.dat");
      Sys.rename tmp_fnames_dat (Filename.concat bname "fnames.dat");
      remove_file (Filename.concat bname "fnames.inx");
      Sys.rename tmp_fnames_inx (Filename.concat bname "fnames.inx");
      remove_file (Filename.concat bname "strings.inx");
      Sys.rename tmp_strings_inx (Filename.concat bname "strings.inx");
      remove_file (Filename.concat bname "notes");
      if Sys.file_exists tmp_notes then
        Sys.rename tmp_notes (Filename.concat bname "notes")
      else ();
      if Sys.file_exists tmp_notes_d then do {
        let notes_d = Filename.concat bname "notes_d" in
        remove_dir notes_d;
        Sys.rename tmp_notes_d notes_d;
      }
      else ();
      remove_file (Filename.concat bname "patches");
      remove_file (Filename.concat bname "patches~");
      remove_file (Filename.concat bname "tstab");
      remove_file (Filename.concat bname "tstab_visitor");
      remove_file (Filename.concat bname "restrict")
    }
    else ();
  }
;

value output = gen_output False;
