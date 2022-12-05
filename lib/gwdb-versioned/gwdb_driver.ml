
module Log = struct
  let oc : out_channel option ref = ref None

  let log fn =
    match !oc with
    | Some oc -> fn oc
    | None -> ()

  type level =
    [ `LOG_ALERT
    | `LOG_CRIT
    | `LOG_DEBUG
    | `LOG_EMERG
    | `LOG_ERR
    | `LOG_INFO
    | `LOG_NOTICE
    | `LOG_WARNING
    ]

  let syslog (level : level) msg =
    let flags = [`LOG_PERROR] in
    let log = Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log ;
    Printexc.print_backtrace stderr
(*  let log msg =
    let tm = Unix.(time () |> localtime) in
    let level = "DEBUG" in
    Printf.eprintf "[%s]: %s %s\n"
      (Mutil.sprintf_date tm : Adef.safe_string :> string) level msg
  (*let log = Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log ;
    if !debug then Printexc.print_backtrace stderr *)*)
end

(*let log msg = Log.syslog Log.(`LOG_DEBUG) msg*)
let log _ = ()

            
module Legacy_driver = struct

  include Gwdb_legacy.Gwdb_driver
  let versions = Version.([gnwb20;gnwb21;gnwb22;gnwb23;gnwb24])

  type pers_event = (iper, istr) Def.gen_pers_event

  type fam_event = (iper, istr) Def.gen_fam_event

  let compatibility_directory = "gnwb25"

  let compat_dir base =
    Filename.concat (bdir base) compatibility_directory
                              
  let compatibility_file = "witness_notes"
  let fcompatibility_file = "fwitness_notes"

  let compat_file base =
    Filename.concat (compat_dir base) compatibility_file

  let fcompat_file base =
    Filename.concat (compat_dir base) fcompatibility_file
    
  let compat_exists base =    
    Sys.file_exists (compat_dir base) && Sys.file_exists (compat_file base) && Sys.file_exists  (fcompat_file base)

  let create_compatibility_files base =
    let dir = bdir base in
    log @@ "CREATE COMPAT DIR:" ^ dir;
    Files.mkdir_p (compat_dir base)

  let witness_notes_tbl : (iper, istr array array) Hashtbl.t option ref =
    ref None
  let fwitness_notes_tbl : (ifam, istr array array) Hashtbl.t option ref =
    ref None

  type person = {
      person : Gwdb_legacy.Gwdb_driver.person;
      witness_notes : istr array array
      (*      base : Gwdb_legacy.Gwdb_driver.base*)
    }

  type family = {
      family : Gwdb_legacy.Gwdb_driver.family;
      witness_notes : istr array array
    }
              
  let write_witness_notes base tbl_opt = match tbl_opt with
    | None -> ()
    | Some tbl ->
       if not (compat_exists base) then create_compatibility_files base;
       let witfile = compat_file base in
       let witfile_tmp = witfile ^ "~" in
       if Sys.file_exists witfile_tmp then failwith "oups";
       let oc = Secure.open_out witfile_tmp in

       log "WRITE TBL";
       Hashtbl.iter (fun iper wnotes ->
           log @@ "WIPER:" ^ (string_of_int iper);
           log "WNOTES:";
           Array.iter (Array.iter (fun istr-> log @@ "ISTR" ^ (string_of_int istr))) wnotes
         ) tbl;
       
       Marshal.to_channel oc tbl [Marshal.No_sharing];
       close_out oc;
       Files.mv witfile_tmp witfile;
       Files.rm witfile_tmp

  let write_fwitness_notes base tbl_opt = match tbl_opt with
    | None -> ()
    | Some tbl ->
       if not (compat_exists base) then create_compatibility_files base;
       let witfile = fcompat_file base in
       let witfile_tmp = witfile ^ "~" in
       if Sys.file_exists witfile_tmp then failwith "oups";
       let oc = Secure.open_out witfile_tmp in

       log "WRITE FTBL";
       Hashtbl.iter (fun ifam wnotes ->
           log @@ "WIFAM:" ^ (string_of_int ifam);
           log "WNOTES:";
           Array.iter (Array.iter (fun istr-> log @@ "ISTR" ^ (string_of_int istr))) wnotes
         ) tbl;
       
       Marshal.to_channel oc tbl [Marshal.No_sharing];
       close_out oc;
       Files.mv witfile_tmp witfile;
       Files.rm witfile_tmp

       
  let write_witness_notes base =
    log "WRITE_WITNESS_NOTES";
    let tbl_opt = !witness_notes_tbl in
    write_witness_notes base tbl_opt

  let write_fwitness_notes base =
    log "WRITE_FWITNESS_NOTES";
    let tbl_opt = !fwitness_notes_tbl in
    write_fwitness_notes base tbl_opt

    
  let load_witness_notes base =
    log "LOAD WNOTES";
    let tbl =
      if compat_exists base then begin
          log "COMPAT EXISTS";
          let ic = Secure.open_in (compat_file base) in
          let tbl = (Marshal.from_channel ic : (iper, istr array array) Hashtbl.t) in
          close_in ic;
          log "PRINT NOTES";
          Hashtbl.iter (fun iper notes ->
              log ("IPER:" ^ string_of_int iper);
              Array.iter (Array.iter (fun n ->
                              log @@ "ISTR:" ^ string_of_int n;
                              log @@ "NOTE:" ^ sou base n)) notes
            ) tbl;
          witness_notes_tbl := Some tbl;
          tbl
        end
      else begin
          log "NO COMPAT FOUND"; Hashtbl.create 1
        end
    in
    witness_notes_tbl := Some tbl;
    tbl

  let load_fwitness_notes base =
    log "LOAD FWNOTES";
    let tbl =
      if compat_exists base then begin
          log "COMPAT EXISTS";
          let ic = Secure.open_in (fcompat_file base) in
          let tbl = (Marshal.from_channel ic : (ifam, istr array array) Hashtbl.t) in
          close_in ic;
          log "PRINT FNOTES";
          Hashtbl.iter (fun ifam notes ->
              log ("IFAM:" ^ string_of_int ifam);
              Array.iter (Array.iter (fun n ->
                              log @@ "ISTR:" ^ string_of_int n;
                              log @@ "NOTE:" ^ sou base n)) notes
            ) tbl;
          fwitness_notes_tbl := Some tbl;
          tbl
        end
      else begin
          log "NO COMPAT FOUND"; Hashtbl.create 1
        end
    in
    fwitness_notes_tbl := Some tbl;
    tbl

  let init_witness_notes_tbl () =
    witness_notes_tbl := Some (Hashtbl.create 1);
    fwitness_notes_tbl := Some (Hashtbl.create 1)

  let get_witness_notes_tbl () = !witness_notes_tbl

  let get_fwitness_notes_tbl () = !fwitness_notes_tbl

  let witness_notes_tbl base = match !witness_notes_tbl with
    | Some tbl -> tbl
    | None -> load_witness_notes base

  let fwitness_notes_tbl base = match !fwitness_notes_tbl with
    | Some tbl -> tbl
    | None -> load_fwitness_notes base
            
  let gen_person_of_person p =
    let gen_pers = gen_person_of_person p.person in
    let pevents =
      List.mapi (fun ie pe ->
          let pe = Translate.legacy_to_def_pevent empty_string pe in
          let epers_witnesses =
            Array.mapi (fun iw (ip, wk, _) ->
                ip, wk, p.witness_notes.(ie).(iw)) pe.epers_witnesses
          in
          {pe with epers_witnesses}
        ) gen_pers.pevents
    in
    let gen_pers = Translate.legacy_to_def_person empty_string gen_pers in
    {gen_pers with pevents}
                             
  let person_of_gen_person base (genpers, gen_ascend, gen_union) =
    let pevents = genpers.Def.pevents in
    let witness_notes =
      List.map (fun pe ->
          Array.map (fun (_,_,wnote) -> wnote) pe.Def.epers_witnesses
        ) pevents |> Array.of_list
    in
    let genpers = Translate.as_legacy_person genpers in
    let person = person_of_gen_person base (genpers, gen_ascend, gen_union) in
    {person; witness_notes}

  let no_person iper =
    let nop = no_person iper in
    Translate.legacy_to_def_person empty_string nop


  let test_on_person base genpers =
    let pers_events = genpers.Def.pevents in
    List.iter (fun pers_event ->
        let witnesses = pers_event.Def.epers_witnesses in
        let wnotes = Array.map (fun (_ip, _wk, wnote) -> sou base wnote) witnesses in
        Array.iter (log) wnotes
      ) pers_events


  let add_witness_notes tbl iper pevents =
    let a : 'a array = Array.make (List.length pevents) (Array.make 0 empty_string) in
    List.iteri (fun i pevent ->
        let witnotes : istr array =
          Array.map (fun (_, _, wnote) -> wnote) pevent.Def.epers_witnesses
        in
        a.(i) <- witnotes
      ) pevents;
    Hashtbl.replace tbl iper a

  let add_fwitness_notes tbl ifam fevents =
    let a : 'a array = Array.make (List.length fevents) (Array.make 0 empty_string) in
    List.iteri (fun i fevent ->
        let witnotes : istr array =
          Array.map (fun (_, _, wnote) -> wnote) fevent.Def.efam_witnesses
        in
        a.(i) <- witnotes
      ) fevents;
    Hashtbl.replace tbl ifam a
    
  let patch_person base iper genpers =
    log @@ "PATCH PERSON" ^ (string_of_int iper);
    test_on_person base genpers;
    log "LETS PATCH";
    let pevents = genpers.pevents in
    let genpers = Translate.as_legacy_person genpers in
    patch_person base iper genpers;
    let tbl = witness_notes_tbl base in
    add_witness_notes tbl iper pevents

  let insert_person base iper genpers =
    log "INSERT PERSON";
    test_on_person base genpers;
    log "LETS INSERT";
    let pevents = genpers.pevents in
    let genpers = Translate.as_legacy_person genpers in
    insert_person base iper genpers;
    let tbl = witness_notes_tbl base in
    add_witness_notes tbl iper pevents

  let commit_patches base =
    log "COMMIT LEGACY PATCHES";
    commit_patches base;
    log "COMMIT NOTES PATCHES";
    write_witness_notes base;
    write_fwitness_notes base

  let get_pevents p =
    let pevents = get_pevents p.person in
    let pevents =
      List.mapi (fun i pe ->
          let pe = Translate.legacy_to_def_pevent empty_string pe in
          let wnotes = p.witness_notes.(i) in
          let witnesses = Array.mapi (fun i (ip, wk, _) -> ip, wk, wnotes.(i)) pe.epers_witnesses in
          {pe with epers_witnesses = witnesses}
        ) pevents in
    pevents

  let get_fevents f =
    let fevents = get_fevents f.family in
    let fevents =
      List.mapi (fun i fe ->
          let fe = Translate.legacy_to_def_fevent empty_string fe in
          let wnotes = f.witness_notes.(i) in
          let witnesses = Array.mapi (fun i (ip, wk, _) -> ip, wk, wnotes.(i)) fe.efam_witnesses in
          {fe with efam_witnesses = witnesses}
        ) fevents in
    fevents
    
  let make bname particles ((persons, ascends, unions), (families, couples, descends), string_arrays, base_notes) =
    (*let persons = Array.map Translate.as_legacy_person persons in
      let families = Array.map Translate.as_legacy_family families in*)
    init_witness_notes_tbl ();
    let persons = Array.map (fun p ->
        let leg_person = Translate.as_legacy_person p in
        add_witness_notes (get_witness_notes_tbl () |> Option.get) p.key_index p.pevents;
        leg_person
      ) persons in
    let families = Array.map (fun f ->
        let leg_family = Translate.as_legacy_family f in
        add_fwitness_notes (get_fwitness_notes_tbl () |> Option.get) f.fam_index f.fevents;
        leg_family
      ) families in
    make bname particles ((persons, ascends, unions), (families, couples, descends), string_arrays, base_notes)


  let open_base bname =
    log @@ "BNAME:" ^ bname;
    let base = open_base bname in
    log @@ "Bdir:" ^ bdir base;
    base

  let close_base base =
    log "CLOSING THE BASE";
    close_base base

  let empty_person base iper =
    let p = empty_person base iper in
    {person = p; witness_notes = [||]}
    
  let get_access p = get_access p.person
  let get_aliases p = get_aliases p.person
  let get_baptism p = get_baptism p.person
  let get_baptism_note p = get_baptism_note p.person
  let get_baptism_place p = get_baptism_place p.person
  let get_baptism_src p = get_baptism_src p.person
  let get_birth p = get_birth p.person
  let get_birth_note p = get_birth_note p.person
  let get_birth_place p = get_birth_place p.person
  let get_birth_src p = get_birth_src p.person
  let get_death p = get_death p.person
  let get_death_note p = get_death_note p.person
  let get_death_place p = get_death_place p.person
  let get_death_src p = get_death_src p.person
  let get_burial p = get_burial p.person
  let get_burial_note p = get_burial_note p.person
  let get_burial_place p = get_burial_place p.person
  let get_burial_src p = get_burial_src p.person
  let get_consang p = get_consang p.person
  let get_family p = get_family p.person
  let get_first_name p = get_first_name p.person
  let get_first_names_aliases p = get_first_names_aliases p.person
  let get_image p = get_image p.person
  let get_iper p = get_iper p.person
  let get_notes p = get_notes p.person
  let get_occ p = get_occ p.person
  let get_occupation p = get_occupation p.person
  let get_parents p = get_parents p.person
  let get_psources p = get_psources p.person
  let get_public_name p =get_public_name p.person
  let get_qualifiers p = get_qualifiers p.person
  let get_related p = get_related p.person
  let get_rparents p =get_rparents p.person
  let get_sex p = get_sex p.person
  let get_surname p = get_surname p.person
  let get_surnames_aliases p =get_surnames_aliases p.person
  let get_titles p = get_titles p.person
  let gen_ascend_of_person p = gen_ascend_of_person p.person
  let gen_union_of_person p = gen_union_of_person p.person

  let witness_notes base iper =
    let tbl = witness_notes_tbl base in
    match Hashtbl.find_opt tbl iper with
      | Some notes -> notes
      | None ->
         let p = poi base iper in
         let genpers = Gwdb_legacy.Gwdb_driver.gen_person_of_person p in
         let pevents = genpers.Gwdb_legacy.Dbdisk.pevents in
         let witnesses_notes =
           List.map (fun pe ->
               let wits = pe.Gwdb_legacy.Dbdisk.epers_witnesses in
               Array.make (Array.length wits) empty_string) pevents
         |> Array.of_list
         in
         witnesses_notes

  let fwitness_notes base ifam =
    let tbl = fwitness_notes_tbl base in
    match Hashtbl.find_opt tbl ifam with
      | Some notes -> notes
      | None ->
         let f = foi base ifam in
         let genfam = Gwdb_legacy.Gwdb_driver.gen_family_of_family f in
         let fevents = genfam.Gwdb_legacy.Dbdisk.fevents in
         let witnesses_notes =
           List.map (fun fe ->
               let wits = fe.Gwdb_legacy.Dbdisk.efam_witnesses in
               Array.make (Array.length wits) empty_string) fevents
         |> Array.of_list
         in
         witnesses_notes

  let poi base iper =
    {person = poi base iper; witness_notes = witness_notes base iper}

  let base_visible_get base (f : person -> bool) iper =
    let f person =
      let witness_notes = witness_notes base (Gwdb_legacy.Gwdb_driver.get_iper person) in
      f {person; witness_notes} in
    base_visible_get base f iper

  let persons base =
    let coll = persons base in
    Collection.map (fun person ->
        let witness_notes = witness_notes base (Gwdb_legacy.Gwdb_driver.get_iper person) in
        {person; witness_notes} ) coll

  let empty_family base ifam =
    let f = empty_family base ifam in
    {family = f; witness_notes = [||]}

  let gen_family_of_family f =
    let gen_fam = gen_family_of_family f.family in
    let fevents =
      List.mapi (fun ie fe ->
          let fe = Translate.legacy_to_def_fevent empty_string fe in
          let efam_witnesses =
            Array.mapi (fun iw (ip, wk, _) ->
                ip, wk, f.witness_notes.(ie).(iw)) fe.efam_witnesses
          in
          {fe with efam_witnesses}
        ) gen_fam.fevents
    in
    let gen_fam = Translate.legacy_to_def_family empty_string gen_fam in
    {gen_fam with fevents}
    
  let family_of_gen_family base (genfam, gen_couple, gen_descend) =
    let fevents = genfam.Def.fevents in
    let witness_notes =
      List.map (fun fe ->
          Array.map (fun (_,_,wnote) -> wnote) fe.Def.efam_witnesses
        ) fevents |> Array.of_list
    in
    let genfam = Translate.as_legacy_family genfam in
    let family = family_of_gen_family base (genfam, gen_couple, gen_descend) in
    {family; witness_notes}

  let no_family ifam =
    let nof = no_family ifam in
    Translate.legacy_to_def_family empty_string nof
    
  let patch_family base ifam genfam =
    log @@ "PATCH FAMILY" ^ (string_of_int ifam);
    (* TODO HANDLE WNOTES *)
    log "LETS PATCH";
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    patch_family base ifam genfam;
    let tbl = fwitness_notes_tbl base in
    add_fwitness_notes tbl ifam fevents

  let insert_family base ifam genfam =
    log "INSERT FAMILY";
    log "LETS INSERT";
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    insert_family base ifam genfam;
    let tbl = fwitness_notes_tbl base in
    add_fwitness_notes tbl ifam fevents

  let get_children f = get_children f.family
  let get_comment f = get_comment f.family
  let get_divorce f = get_divorce f.family
  let get_father f = get_father f.family
  let get_fsources f = get_fsources f.family
  let get_ifam f = get_ifam f.family
  let get_marriage f = get_marriage f.family
  let get_marriage_note f = get_marriage_note f.family
  let get_marriage_place f = get_marriage_place f.family
  let get_marriage_src f = get_marriage_src f.family
  let get_mother f = get_mother f.family
  let get_origin_file f = get_origin_file f.family
  let get_parent_array f = get_parent_array f.family
  let get_relation f = get_relation f.family
  let get_witnesses f = get_witnesses f.family
  let gen_couple_of_family f = gen_couple_of_family f.family
  let gen_descend_of_family f = gen_descend_of_family f.family
  let foi base ifam =
    {family = foi base ifam; witness_notes = fwitness_notes base ifam}

  let families ?(select = fun _ -> true) base =
    let select f =
      select {family = f; witness_notes = [||]}
    in
    let coll = families ~select base in
    Collection.map (fun family ->
        let witness_notes = fwitness_notes base (Gwdb_legacy.Gwdb_driver.get_ifam family) in
        {family; witness_notes} ) coll

  let sync ?(scratch=false) ~save_mem base =
    sync ~scratch ~save_mem base;
    write_witness_notes base;
    write_fwitness_notes base

                         
end

module Driver = Compat.Make (Legacy_driver) (Legacy_driver)

include Driver
