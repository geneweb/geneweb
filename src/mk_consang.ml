(* camlp4r ./pa_lock.cmo *)
(* $Id: mk_consang.ml,v 5.28 2007-02-20 00:40:05 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

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

value rebuild_string_fields db2 patches_ht f1 nb item_of_int list =
  let bdir = Filename.concat db2.Db2disk.bdir2 f1 in
  List.iter
    (fun (f2, get_field) -> do {
       if Mutil.verbose.val then do {
         Printf.eprintf "rebuilding %s..." f2;
         flush stderr;
       }
       else ();
       let bdir = Filename.concat bdir f2 in
       let oc_dat = open_out_bin (Filename.concat bdir "1data") in
       let oc_acc = open_out_bin (Filename.concat bdir "1access") in
       Db2out.output_value_array_string oc_dat
         (fun output_item ->
            for i = 0 to nb - 1 do {
              let fn =
                try get_field (Hashtbl.find patches_ht (item_of_int i)) with
                [ Not_found ->
                    Db2disk.string_of_istr2 db2 (f1, f2)
                      (Db2disk.get_field_acc db2 i (f1, f2)) ]
              in
              let pos = output_item fn in
              output_binary_int oc_acc pos;
            });
       close_out oc_acc;
       close_out oc_dat;
       if Mutil.verbose.val then do {
         Printf.eprintf "\n";
         flush stderr
       }
       else ()
     })
    list
;

value rebuild_fields2 db2 = do {
  let patches = db2.Db2disk.patches in
  let nb_per = patches.Db2disk.nb_per in
  rebuild_string_fields db2 patches.Db2disk.h_person "person" nb_per
    Adef.iper_of_int
    [("baptism_place", fun p -> p.Def.baptism_place);
     ("baptism_src", fun p -> p.Def.baptism_src);
     ("birth_place", fun p -> p.Def.birth_place);
     ("birth_src", fun p -> p.Def.birth_src);
     ("burial_place", fun p -> p.Def.burial_place);
     ("burial_src", fun p -> p.Def.burial_src);
     ("death_place", fun p -> p.Def.death_place);
     ("death_src", fun p -> p.Def.death_src);
     ("first_name", fun p -> p.Def.first_name);
     ("image", fun p -> p.Def.image);
     ("notes", fun p -> p.Def.notes);
     ("occupation", fun p -> p.Def.occupation);
     ("psources", fun p -> p.Def.psources);
     ("public_name", fun p -> p.Def.public_name);
     ("surname", fun p -> p.Def.surname)];
  let nb_fam = patches.Db2disk.nb_fam in
  rebuild_string_fields db2 patches.Db2disk.h_family "family" nb_fam
    Adef.ifam_of_int
    [("comment", fun f -> f.Def.comment);
     ("fsources", fun f -> f.Def.fsources);
     ("marriage_place", fun f -> f.Def.marriage_place);
     ("marriage_src", fun f -> f.Def.marriage_src);
     ("origin_file", fun f -> f.Def.origin_file)]
};

value simple_output bname base carray =
  match carray with
  [ Some tab ->
      Gwdb.apply_base2 base
        (fun db2 -> do {
           let dir =
             List.fold_left Filename.concat db2.Db2disk.bdir2
               ["person"; "consang"]
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
           let bdir = db2.Db2disk.bdir2 in
           let has_patches =
             Sys.file_exists (Filename.concat bdir "patches")
           in
           Printf.eprintf "has_patches %b\n" has_patches;
           flush stderr;
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

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    if fname.val = "" then do {
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2;
    }
    else ();
    Secure.set_base_dir (Filename.dirname fname.val);
    let f () =
      let base = Gwdb.open_base fname.val in
      try
        do {
          Sys.catch_break True;
          let carray = ConsangAll.compute base scratch.val quiet.val in
          simple_output fname.val base carray;
        }
      with
      [ Consang.TopologicalSortError p ->
          do {
            Printf.printf
              "\nError: loop in database, %s is his/her own ancestor.\n"
              (designation base p);
            flush stdout;
            exit 2
          } ]
    in
    lock (Mutil.lock_file fname.val) with
    [ Accept -> f ()
    | Refuse ->
        do {
          Printf.eprintf "Base is locked. Waiting... ";
          flush stderr;
          lock_wait (Mutil.lock_file fname.val) with
          [ Accept -> do { Printf.eprintf "Ok\n"; flush stderr; f () }
          | Refuse ->
              do {
                Printf.printf "\nSorry. Impossible to lock base.\n";
                flush stdout;
                exit 2
              } ]
        } ]
  }
;

Printexc.catch main ();
