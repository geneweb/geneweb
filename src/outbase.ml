(* $Id: outbase.ml,v 5.9 2006-09-30 18:58:19 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Def;
open Gwdb;
open Mutil;

value save_mem = ref False;
value intext_magic_number = [| 0x84; 0x95; 0xA6; 0xBE |];

value trace s =
  if verbose.val then do { Printf.eprintf "*** %s\n" s; flush stderr }
  else ()
;

value output_array_no_sharing oc arr =
  do {
    let pos = pos_out oc in
    (* magic number *)
    for i = 0 to 3 do { output_byte oc intext_magic_number.(i); };

    (* room for block length *)
    output_binary_int oc 0;
    (* room for obj counter *)
    output_binary_int oc 0;
    (* room for size_32 *)
    output_binary_int oc 0;
    (* room for size_64 *)
    output_binary_int oc 0;

    let pos_start = pos_out oc in
    Iovalue.size_32.val := 0;
    Iovalue.size_64.val := 0;
    Iovalue.output_block_header oc 0 arr.len;
    for i = 0 to arr.len - 1 do {
      Iovalue.output oc (arr.get i);
    };
    let pos_end = pos_out oc in
    let size_32 = Iovalue.size_32.val in
    let size_64 = Iovalue.size_64.val in

    (* block_length *)
    seek_out oc (pos + 4);
    output_binary_int oc (pos_end - pos_start);
    (* obj counter is zero because no_sharing *)
    output_binary_int oc 0;
    (* size_32 *)
    output_binary_int oc size_32;
    (* size_64 *)
    output_binary_int oc size_64;
    seek_out oc pos_end;
(*
Printf.eprintf "check with marshal %d\n" arr.len;
flush stderr;
let array = Array.init arr.len arr.get in
let s = Marshal.to_string array [Marshal.No_sharing] in
let sign_extend_shift = (Iovalue.sizeof_long - 1) * 8 - 1 in
let sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift in
let glop i =
  Obj.magic ((sign_extend (Char.code s.[i])) lsl 24 + Char.code s.[i+1] lsl 16 + Char.code s.[i+2] lsl 8 + Char.code s.[i+3])
in
Printf.eprintf "block length %d %d\n" (glop 4) (pos_end - pos_start);
Printf.eprintf "obj counter %d %d\n" (glop 8) 0;
Printf.eprintf "size_32 %d %d\n" (glop 12) size_32;
Printf.eprintf "size_64 %d %d\n" (glop 16) size_64;
flush stderr;
*)
  }
;

value output_value_header_size = 20;
value array_header_size arr_len = if arr_len < 8 then 1 else 5;

value output_array_access oc arr_get arr_len pos =
  loop (pos + output_value_header_size + array_header_size arr_len) 0
  where rec loop pos i =
    if i == arr_len then pos
    else do {
      output_binary_int oc pos;
      loop (pos + Iovalue.size (arr_get i)) (i + 1)
    }
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
      do { Iobase.check_magic ic; ic }
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

value p_first_name base p = nominative (sou base (get_first_name p));
value p_surname base p = nominative (sou base (get_surname p));

value make_name_index base =
  let t = Array.create Iobase.table_size [| |] in
  let add_name key valu =
    let key = Name.crush (Name.abbrev key) in
    let i = Hashtbl.hash key mod Array.length t in
    if array_memq valu t.(i) then ()
    else t.(i) := Array.append [| valu |] t.(i)
  in
  let rec add_names ip =
    fun
    [ [] -> ()
    | [n :: nl] -> do { add_name n ip; add_names ip nl } ]
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" && surname <> "?" then
        let names =
          [Name.lower (first_name ^ " " ^ surname) ::
           Gwdb.person_misc_names base p get_titles]
        in
        add_names (get_key_index p) names
      else ();
    };
    t
  }
;

value create_name_index oc_inx oc_inx_acc base =
  let ni = make_name_index base in
  let bpos = pos_out oc_inx in
  do {
    output_value_no_sharing oc_inx (ni : Iobase.name_index_data);
    let epos =
      output_array_access oc_inx_acc (Array.get ni) (Array.length ni) bpos
    in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value add_name t key valu =
  let key = Name.crush_lower key in
  let i = Hashtbl.hash key mod Array.length t in
  if array_memq valu t.(i) then ()
  else t.(i) := Array.append [| valu |] t.(i)
;

value make_strings_of_fsname base =
  let t = Array.create Iobase.table_size [| |] in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" then add_name t first_name (get_first_name p)
      else ();
      if surname <> "?" then do {
        add_name t surname (get_surname p);
        List.iter (fun sp -> add_name t sp (get_surname p))
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
    output_value_no_sharing oc_inx (t : Iobase.strings_of_fsname);
    let epos =
      output_array_access oc_inx_acc (Array.get t) (Array.length t) bpos
    in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value is_prime a =
  loop 2 where rec loop b =
    if a / b < b then True else if a mod b == 0 then False else loop (b + 1)
;

value rec prime_after n = if is_prime n then n else prime_after (n + 1);

value output_strings_hash oc2 base =
  let strings_array = base.data.strings.array_obj () in
  let taba =
    Array.create
      (min Sys.max_array_length
        (prime_after (max 2 (10 * Array.length strings_array))))
      (-1)
  in
  let tabl = Array.create (Array.length strings_array) (-1) in
  do {
    for i = 0 to Array.length strings_array - 1 do {
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
         type t = istr;
         value compare = Iobase.compare_istr base;
       end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let a =
        try IstrTree.find (get_surname p) bt.val with [ Not_found -> [] ]
      in
      bt.val := IstrTree.add (get_surname p) [get_key_index p :: a] bt.val
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
         type t = istr;
         value compare = Iobase.compare_istr base;
       end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let a =
        try IstrTree.find (get_first_name p) bt.val with [ Not_found -> [] ]
      in
      bt.val := IstrTree.add (get_first_name p) [get_key_index p :: a] bt.val
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
        match try Some (arr.array_obj ()) with [ Failure _ -> None ] with
        [ Some a -> output_value_no_sharing oc a
        | None -> output_array_no_sharing oc arr ];
        let epos = output_array_access oc_acc arr.get arr.len bpos in
        if epos <> pos_out oc then count_error epos (pos_out oc) else ()
      }
    in
    try
      do {
        output_string oc
          (if utf_8_db.val then Iobase.magic_gwb
           else Iobase.magic_gwb_iso_8859_1);
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
        output_value_no_sharing oc base.data.bnotes.norigin_file;
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
    base_cleanup base;
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
