(* camlp5r ./pa_lock.cmo *)
(* $Id: mk_consang.ml,v 5.49 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Printf;

value fname = ref "";
value indexes = ref False;
value scratch = ref False;
value quiet = ref False;

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist =
  [("-q", Arg.Set quiet, ": quiet mode");
   ("-i", Arg.Set indexes, ": build the indexes again");
   ("-scratch", Arg.Set scratch, ": from scratch");
   ("-mem", Arg.Set Outbase.save_mem,
    ": Save memory, but slower when rewritting database");
   ("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database.")]
;
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several databases")
;

value rebuild_field_array db2 pad bdir compress f = do {
  if Mutil.verbose.val then do {
    eprintf "rebuilding %s..." (Filename.basename bdir);
    flush stderr;
  }
  else ();
  let oc_dat = open_out_bin (Filename.concat bdir "data") in
  let oc_acc = open_out_bin (Filename.concat bdir "access") in
  Db2out.output_value_array oc_dat pad compress (f oc_acc);
  close_out oc_acc;
  close_out oc_dat;
  if Mutil.verbose.val then do {
    eprintf "\n";
    flush stderr
  }
  else ()
};

type field_info 'index 'item =
  { fi_nb : int;
    fi_ht : Hashtbl.t 'index 'item;
    fi_index_of_int : int -> 'index;
    fi_dir : string }
;

value rebuild_any_field_array db2 fi pad compress (f2, get) = do {
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array db2 pad bdir compress
    (fun oc_acc output_item -> do {
       (* put pad as 1st elem; not necessary, just for beauty *)
       if compress then ignore (output_item pad : int) else ();
       for i = 0 to fi.fi_nb - 1 do {
         let x =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
           [ Not_found ->
               let pos = Db2disk.get_field_acc db2 i (f1, f2) in
               Db2disk.get_field_data db2 pos (f1, f2) "data" ]
         in
         let pos = output_item x in
         output_binary_int oc_acc pos;
       }
     })
};

value rebuild_option_field_array db2 fi pad (f2, get) = do {
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array db2 pad bdir True
    (fun oc_acc output_item ->
        for i = 0 to fi.fi_nb - 1 do {
          let x =
            try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
            [ Not_found ->
                let pos = Db2disk.get_field_acc db2 i (f1, f2) in
                if pos = -1 then None
                else Some (Db2disk.get_field_data db2 pos (f1, f2) "data") ]
          in
          match x with
          [ None -> output_binary_int oc_acc (-1)
          | Some x -> do {
              let pos = output_item x in
              output_binary_int oc_acc pos
            } ];
        })
};

value rebuild_list_field_array db2 fi (f2, get) = do {
  let f1 = fi.fi_dir in
  let f oc_acc oc_dat =
    for i = 0 to fi.fi_nb - 1 do {
      let x =
        try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
        [ Not_found ->
            let pos = Db2disk.get_field_acc db2 i (f1, f2) in
            if pos = -1 then []
            else Db2disk.get_field_data db2 pos (f1, f2) "data" ]
      in
      if x = [] then output_binary_int oc_acc (-1)
      else do {
        let pos = pos_out oc_dat in
        Iovalue.output oc_dat x;
        output_binary_int oc_acc pos
      }
    }
  in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;

  if Mutil.verbose.val then do {
    eprintf "rebuilding %s..." (Filename.basename bdir);
    flush stderr;
  }
  else ();
  let oc_dat = open_out_bin (Filename.concat bdir "data") in
  let oc_acc = open_out_bin (Filename.concat bdir "access") in
  f oc_acc oc_dat;
  close_out oc_acc;
  close_out oc_dat;
  if Mutil.verbose.val then do {
    eprintf "\n";
    flush stderr
  }
  else ()
};

value rebuild_list2_field_array db2 fi (f2, get) = do {
  let f1 = fi.fi_dir in
  let f oc_acc oc_dat =
    for i = 0 to fi.fi_nb - 1 do {
      let rxl =
        try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
        [ Not_found ->
            let pos = Db2disk.get_field_acc db2 i (f1, f2) in
            loop [] pos where rec loop list pos =
              if pos = -1 then list
              else
                let (x, pos) =
                  Db2disk.get_field_2_data db2 pos (f1, f2) "data"
                in
                loop [x :: list] pos ]
      in
      let pos =
        loop (-1) rxl where rec loop pos =
          fun
          [ [] -> pos
          | [x :: xl] -> do {
              let new_pos = pos_out oc_dat in
              Iovalue.output oc_dat x;
              Iovalue.output oc_dat pos;
              loop new_pos xl
            } ]
      in
      output_binary_int oc_acc pos;
    }
  in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;

  if Mutil.verbose.val then do {
    eprintf "rebuilding %s..." (Filename.basename bdir);
    flush stderr;
  }
  else ();
  let oc_dat = open_out_bin (Filename.concat bdir "data") in
  let oc_acc = open_out_bin (Filename.concat bdir "access") in
  f oc_acc oc_dat;
  close_out oc_acc;
  close_out oc_dat;
  if Mutil.verbose.val then do {
    eprintf "\n";
    flush stderr
  }
  else ()

};

value rebuild_string_field db2 fi (f2, get) = do {
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  rebuild_field_array db2 "" bdir True
    (fun oc_acc output_item -> do {
       let istr_empty = output_item "" in
       let istr_quest = output_item "?" in
       assert (istr_empty = Db2.empty_string_pos);
       assert (istr_quest = Db2.quest_string_pos);
       for i = 0 to fi.fi_nb - 1 do {
         let s =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
           [ Not_found ->
               let pos = Db2disk.get_field_acc db2 i (f1, f2) in
               Db2disk.string_of_istr2 db2 (f1, f2) pos ]
         in
         let pos = output_item s in
         output_binary_int oc_acc pos;
       };
     })
};

value rebuild_list_with_string_field_array g h db2 fi (f2, get) = do {
  let f1 = fi.fi_dir in
  let bdir =
    List.fold_left Filename.concat db2.Db2disk.bdir2 ["new_d"; f1; f2]
  in
  Mutil.mkdir_p bdir;
  let oc_ext = open_out_bin (Filename.concat bdir "data2.ext") in
  rebuild_field_array db2 "" bdir True
    (fun oc_acc output_item -> do {
       let istr_empty = output_item "" in
       let istr_quest = output_item "?" in
       assert (istr_empty = Db2.empty_string_pos);
       assert (istr_quest = Db2.quest_string_pos);
       for i = 0 to fi.fi_nb - 1 do {
         let sl =
           try get (Hashtbl.find fi.fi_ht (fi.fi_index_of_int i)) with
           [ Not_found ->
               let list : list 'a =
                 let pos = Db2disk.get_field_acc db2 i (f1, f2) in
                 if pos = -1 then []
                 else Db2disk.get_field_data db2 pos (f1, f2) "data2.ext"
               in
               List.map (g (Db2disk.string_of_istr2 db2 (f1, f2))) list ]
         in
         let pl = List.map (h output_item) sl in
         if pl = [] then output_binary_int oc_acc (-1)
         else do {
           output_binary_int oc_acc (pos_out oc_ext);
           let (s32, s64) = (Iovalue.size_32.val, Iovalue.size_64.val) in
           Iovalue.output oc_ext (pl : list 'a);
           Iovalue.size_32.val := s32;
           Iovalue.size_64.val := s64;
         }
       }
     });
  close_out oc_ext;
};

value unique_key_string (ht, scnt) s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find ht s with
  [ Not_found -> do {
      let istr = Adef.istr_of_int scnt.val in
      Hashtbl.add ht s istr;
      incr scnt;
      istr
    } ]
;

value make_key_index db2 nb_per bdir = do {
  if Mutil.verbose.val then do {
    eprintf "key index...";
    flush stderr;
  }
  else ();

  let person_of_key_d = Filename.concat bdir "person_of_key" in
  try Mutil.mkdir_p person_of_key_d with _ -> ();
  let ht_index_of_key = Hashtbl.create 1 in
  let ht_strings = (Hashtbl.create 1, ref 0) in

  let f1f2_fn = (Filename.concat "new_d" "person", "first_name") in
  let f1f2_sn = (Filename.concat "new_d" "person", "surname") in
  let f1f2_oc = (Filename.concat "new_d" "person", "occ") in
  for i = 0 to nb_per - 1 do {
    let fn =
      let pos = Db2disk.get_field_acc db2 i f1f2_fn in
      Db2disk.string_of_istr2 db2 f1f2_fn pos
    in
    let sn =
      let pos = Db2disk.get_field_acc db2 i f1f2_sn in
      Db2disk.string_of_istr2 db2 f1f2_sn pos
    in
    if fn = "?" || sn = "?" then ()
    else
      let fn = unique_key_string ht_strings fn in
      let sn = unique_key_string ht_strings sn in
      let oc = Db2disk.get_field db2 i f1f2_oc in
      Hashtbl.add ht_index_of_key (Db2.key2_of_key (fn, sn, oc))
        (Adef.iper_of_int i);
  };

  Db2out.output_hashtbl person_of_key_d "iper_of_key.ht"
    (ht_index_of_key : Hashtbl.t Db2.key2 Def.iper);
  Hashtbl.clear ht_index_of_key;

  Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
    (fst ht_strings : Hashtbl.t string Adef.istr);
  Hashtbl.clear (fst ht_strings);

  if Mutil.verbose.val then do {
    eprintf "\n";
    flush stderr
  }
  else ();
};

value rebuild_fields2 db2 = do {
  let fi_per =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_person;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  let fi_asc =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_ascend;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  let fi_uni =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_per;
     fi_ht = db2.Db2disk.patches.Db2disk.h_union;
     fi_index_of_int = Adef.iper_of_int; fi_dir = "person"}
  in
  List.iter (rebuild_string_field db2 fi_per)
    [("first_name", fun p -> p.Def.first_name);
     ("surname", fun p -> p.Def.surname);
     ("image", fun p -> p.Def.image);
     ("public_name", fun p -> p.Def.public_name);
     ("occupation", fun p -> p.Def.occupation);
     ("birth_place", fun p -> p.Def.birth_place);
     ("birth_src", fun p -> p.Def.birth_src);
     ("baptism_place", fun p -> p.Def.baptism_place);
     ("baptism_src", fun p -> p.Def.baptism_src);
     ("death_place", fun p -> p.Def.death_place);
     ("death_src", fun p -> p.Def.death_src);
     ("burial_place", fun p -> p.Def.burial_place);
     ("burial_src", fun p -> p.Def.burial_src);
     ("notes", fun p -> p.Def.notes);
     ("psources", fun p -> p.Def.psources)];
  rebuild_any_field_array db2 fi_per 0 True
    ("occ", fun p -> p.Def.occ);
  List.iter
    (rebuild_list_with_string_field_array (fun f -> f) (fun f -> f) db2
       fi_per)
    [("qualifiers", fun p -> p.Def.qualifiers);
     ("aliases", fun p -> p.Def.aliases);
     ("first_names_aliases", fun p -> p.Def.first_names_aliases);
     ("surnames_aliases", fun p -> p.Def.surnames_aliases)];
  rebuild_list_with_string_field_array Futil.map_title_strings
    Futil.map_title_strings db2 fi_per
    ("titles", fun p -> p.Def.titles);
  rebuild_list_field_array db2 fi_per ("rparents", fun p -> p.Def.rparents);
  rebuild_list2_field_array db2 fi_per ("related", fun p -> p.Def.related);
  rebuild_any_field_array db2 fi_per Def.Neuter True
    ("sex", fun p -> p.Def.sex);
  rebuild_any_field_array db2 fi_per Def.IfTitles True
    ("access", fun p -> p.Def.access);
  List.iter (rebuild_any_field_array db2 fi_per Adef.codate_None True)
    [("birth", fun p -> p.Def.birth);
     ("baptism", fun p -> p.Def.baptism)];
  rebuild_any_field_array db2 fi_per Def.NotDead True
    ("death", fun p -> p.Def.death);
  rebuild_any_field_array db2 fi_per Def.UnknownBurial True
    ("burial", fun p -> p.Def.burial);
  rebuild_option_field_array db2 fi_asc (Adef.ifam_of_int (-1))
    ("parents", fun p -> p.Def.parents);
  rebuild_any_field_array db2 fi_asc Adef.no_consang False
    ("consang", fun p -> p.Def.consang);
  rebuild_any_field_array db2 fi_uni [| |] False
    ("family", fun p -> p.Def.family);

  let fi_fam =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_family;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  let fi_cpl =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_couple;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  let fi_des =
    {fi_nb = db2.Db2disk.patches.Db2disk.nb_fam;
     fi_ht = db2.Db2disk.patches.Db2disk.h_descend;
     fi_index_of_int = Adef.ifam_of_int; fi_dir = "family"}
  in
  rebuild_any_field_array db2 fi_fam Adef.codate_None True
    ("marriage", fun f -> f.Def.marriage);
  List.iter (rebuild_string_field db2 fi_fam)
    [("marriage_place", fun f -> f.Def.marriage_place);
     ("marriage_src", fun f -> f.Def.marriage_src);
     ("comment", fun f -> f.Def.comment);
     ("origin_file", fun f -> f.Def.origin_file);
     ("fsources", fun f -> f.Def.fsources)];
  rebuild_any_field_array db2 fi_fam [| |] True
    ("witnesses", fun f -> f.Def.witnesses);
  rebuild_any_field_array db2 fi_fam Def.Married True
    ("relation", fun f -> f.Def.relation);
  rebuild_any_field_array db2 fi_fam Def.NotDivorced True
    ("divorce", fun f -> f.Def.divorce);
  List.iter (rebuild_any_field_array db2 fi_cpl (Adef.iper_of_int (-1)) True)
    [("father", fun f -> Adef.father f);
     ("mother", fun f -> Adef.mother f)];
  rebuild_any_field_array db2 fi_des [| |] False
    ("children", fun f -> f.Def.children);

  let nb_per = fi_per.fi_nb in

  let new_d = Filename.concat db2.Db2disk.bdir2 "new_d" in
  make_key_index db2 nb_per new_d;
  Gc.compact ();

  let particles =
    Mutil.input_particles (Filename.concat db2.Db2disk.bdir2 "particles.txt")
  in
  Db2out.make_indexes new_d nb_per particles;

  let old_d = Filename.concat db2.Db2disk.bdir2 "old_d" in
  Mutil.remove_dir old_d;
  Mutil.mkdir_p old_d;
  List.iter
    (fun f ->
       Sys.rename (Filename.concat db2.Db2disk.bdir2 f)
         (Filename.concat old_d f))
    ["family"; "person"; "person_of_key"; "person_of_name"; "patches"];
  List.iter
    (fun f ->
       Sys.rename (Filename.concat new_d f)
         (Filename.concat db2.Db2disk.bdir2 f))
    ["family"; "person"; "person_of_key"; "person_of_name"];
};

value simple_output bname base carray =
  match carray with
  [ Some tab ->
      Gwdb.apply_base2 base
        (fun db2 -> do {
           let bdir = db2.Db2disk.bdir2 in
           let dir =
             List.fold_left Filename.concat bdir ["person"; "consang"]
           in
           Mutil.mkdir_p dir;
           let oc = open_out_bin (Filename.concat dir "data") in
           output_value oc tab;
           close_out oc;
           let oc = open_out_bin (Filename.concat dir "access") in
           let _ : int =
             Iovalue.output_array_access oc (Array.get tab) (Array.length tab)
               0
           in
           close_out oc;
           let has_patches =
             Sys.file_exists (Filename.concat bdir "patches")
           in
           if has_patches then do {
             let list =
               Hashtbl.fold
                 (fun ip a list ->
                    let a =
                      {(a) with Def.consang = tab.(Adef.int_of_iper ip)}
                    in
                    [(ip, a) :: list])
                 db2.Db2disk.patches.Db2disk.h_ascend []
             in
             List.iter
               (fun (ip, a) ->
                  Hashtbl.replace db2.Db2disk.patches.Db2disk.h_ascend ip a)
               list;
             Db2disk.commit_patches2 db2;
             rebuild_fields2 db2;
           }
           else ();
         })
  | None ->
      Gwdb.apply_base1 base
        (fun base ->
           let bname = base.Dbdisk.data.Dbdisk.bdir in
           let no_patches =
             not (Sys.file_exists (Filename.concat bname "patches"))
           in
           Outbase.gen_output (no_patches && not indexes.val) bname base) ]
;

value designation base p =
  let first_name = Gwdb.p_first_name base p in
  let nom = Gwdb.p_surname base p in
  Mutil.iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (Gwdb.get_occ p) ^ " " ^ nom)
;

value main () = do {
  Argl.parse speclist anonfun errmsg;
  if fname.val = "" then do {
    eprintf "Missing file name\n";
    eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2;
  }
  else ();
  Secure.set_base_dir (Filename.dirname fname.val);
  let f () =
    let base = Gwdb.open_base fname.val in
    try do {
      Sys.catch_break True;
      let carray = ConsangAll.compute base scratch.val quiet.val in
      simple_output fname.val base carray;
    }
    with
    [ Consang.TopologicalSortError p -> do {
        printf "\nError: loop in database, %s is his/her own ancestor.\n"
          (designation base p);
        flush stdout;
        exit 2
      } ]
  in
  lock (Mutil.lock_file fname.val) with
  [ Accept -> f ()
  | Refuse -> do {
      eprintf "Base is locked. Waiting... ";
      flush stderr;
      lock_wait (Mutil.lock_file fname.val) with
      [ Accept -> do { eprintf "Ok\n"; flush stderr; f () }
      | Refuse -> do {
          printf "\nSorry. Impossible to lock base.\n";
          flush stdout;
          exit 2
        } ]
  } ];
};

Printexc.catch main ();
