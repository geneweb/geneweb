
module Legacy_driver = struct

  include Gwdb_legacy.Gwdb_driver
  let versions = Version.([gnwb20;gnwb21;gnwb22;gnwb23;gnwb24])

  type pers_event = (iper, istr) Def.gen_pers_event

  let compatibility_directory = "gnwb25"

  let compat_dir base =
    Filename.concat (bdir base) compatibility_directory
                              
  let compatibility_file = "witness_notes"

  let compat_file base =
    Filename.concat (compat_dir base) compatibility_file

  let compat_exists base =    
    Sys.file_exists (compat_dir base) && Sys.file_exists (compat_file base)

  let create_compatibility_files base =
    let dir = bdir base in
    print_endline @@ "CREATE COMPAT DIR:" ^ dir;
    Files.mkdir_p (compat_dir base)

  let witness_notes_tbl : (iper, istr array array) Hashtbl.t option ref =
    ref None

  type person = {
      person : Gwdb_legacy.Gwdb_driver.person;
      witness_notes : istr array array
      (*      base : Gwdb_legacy.Gwdb_driver.base*)
    }
    
  let write_witness_notes base tbl_opt = match tbl_opt with
    | None -> ()
    | Some tbl ->
       if not (compat_exists base) then create_compatibility_files base;
       let witfile = compat_file base in
       let witfile_tmp = witfile ^ "~" in
       if Sys.file_exists witfile_tmp then failwith "oups";
       let oc = Secure.open_out witfile_tmp in
       Marshal.to_channel oc tbl [Marshal.No_sharing];
       close_out oc;
       Files.mv witfile_tmp witfile;
       Files.rm witfile_tmp

  let write_witness_notes base =
    print_endline "WRITE_WITNESS_NOTES";
    let tbl_opt = !witness_notes_tbl in
    write_witness_notes base tbl_opt
       
  let load_witness_notes base =
    print_endline "LOAD WNOTES";
    let tbl =
      if compat_exists base then begin
          print_endline "COMPAT EXISTS";
          let ic = Secure.open_in (compat_file base) in
          let tbl = (Marshal.from_channel ic : (iper, istr array array) Hashtbl.t) in
          close_in ic;
          print_endline "PRINT NOTES";
          Hashtbl.iter (fun iper notes ->
              print_endline (string_of_int iper);
              Array.iter (Array.iter (fun n -> print_endline @@ sou base n)) notes
            ) tbl;
          witness_notes_tbl := Some tbl;
          tbl
        end
      else begin
          print_endline "NO COMPAT FOUND"; Hashtbl.create 1
        end
    in
    witness_notes_tbl := Some tbl;
    tbl
    
  let witness_notes_tbl base = match !witness_notes_tbl with
    | Some tbl -> tbl
    | None -> load_witness_notes base

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
        let wnotes = Array.map (fun (ip, wk, wnote) -> sou base wnote) witnesses in
        Array.iter print_endline wnotes
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
    
  let patch_person base iper genpers =
    print_endline "PATCH PERSON";
    test_on_person base genpers;
    print_endline "LETS PATCH";
    let pevents = genpers.pevents in
    let genpers = Translate.as_legacy_person genpers in
    patch_person base iper genpers;
    let tbl = witness_notes_tbl base in
    add_witness_notes tbl iper pevents

  let insert_person base iper genpers =
    print_endline "INSERT PERSON";
    test_on_person base genpers;
    print_endline "LETS INSERT";
    let pevents = genpers.pevents in
    let genpers = Translate.as_legacy_person genpers in
    insert_person base iper genpers;
    let tbl = witness_notes_tbl base in
    add_witness_notes tbl iper pevents

  let commit_patches base =
    print_endline "COMMIT LEGACY PATCHES";
    commit_patches base;
    print_endline "COMMIT NOTES PATCHES";
    write_witness_notes base

  let get_pevents p =
    let pevents = get_pevents p in
    List.map (Translate.legacy_to_def_pevent empty_string) pevents
    
  let make bname particles ((persons, ascends, unions), fam_arrays, string_arrays, base_notes) =
    let persons = Array.map Translate.as_legacy_person persons in
    make bname particles ((persons, ascends, unions), fam_arrays, string_arrays, base_notes)


  let open_base bname =
    print_endline @@ "BNAME:" ^ bname;
    let base = open_base bname in
    print_endline @@ "Bdir:" ^ bdir base;
    base

  let close_base base =
    print_endline @@ "CLOSING THE BASE";
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
  let get_pevents p = get_pevents p.person
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
end

module Driver = Compat.Make (Legacy_driver) (Legacy_driver)

include Driver
