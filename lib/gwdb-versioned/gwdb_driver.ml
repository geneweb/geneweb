module Log = struct
  let oc : out_channel option ref = ref None
  let _log fn = match !oc with Some oc -> fn oc | None -> ()

  type level =
    [ `LOG_ALERT
    | `LOG_CRIT
    | `LOG_DEBUG
    | `LOG_EMERG
    | `LOG_ERR
    | `LOG_INFO
    | `LOG_NOTICE
    | `LOG_WARNING ]

  let _syslog (level : level) msg =
    let flags = [ `LOG_PERROR ] in
    let log =
      Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name
    in
    Syslog.syslog log level msg;
    Syslog.closelog log;
    Printexc.print_backtrace stderr
  (* let log msg =
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

module type Data = sig
  type t
  type index = int
  type base

  val patch_file : base -> string
  val data_file : base -> string
  val directory : base -> string
end

module Store (D : Data) : sig
  val get : D.base -> D.index -> D.t option
  val set : D.base -> D.index -> D.t -> unit
  val unsafe_set : D.index -> D.t -> unit
  val write : D.base -> unit
  val sync : (D.base -> D.t array) -> D.base -> unit
  val empty : unit -> unit
  val close_data_file : unit -> unit
end = struct
  type t = (D.index, D.t) Hashtbl.t

  let patch_ht : (D.index, D.t) Hashtbl.t option ref = ref None

  let data_file_in_channel : in_channel option ref = ref None

  let cache_ht  : (D.index, D.t) Hashtbl.t option ref = ref None
  
  let open_data_file base = match !data_file_in_channel with
    | Some ic ->
      seek_in ic 0;
      ic
    | None ->
      let file = D.data_file base in
      let ic = Secure.open_in file in
      data_file_in_channel := Some ic;
      ic

  let close_data_file () = match !data_file_in_channel with
    | Some ic ->
      close_in ic;
      data_file_in_channel := None
    | None -> ()
  
  let patch_file_exists base = Sys.file_exists (D.patch_file base)
  let data_file_exists base = Sys.file_exists (D.data_file base)
  let directory_exists base = Sys.file_exists (D.directory base)
  let create_files base = Files.mkdir_p (D.directory base)

  let load_patch base =
    if patch_file_exists base then (
      let file = D.patch_file base in
      let ic = Secure.open_in file in
      let tbl = (Marshal.from_channel ic : t) in
      close_in ic;
      patch_ht := Some tbl;
      tbl)
    else
      let tbl = Hashtbl.create 1 in
      patch_ht := Some tbl;
      tbl

  let patch base = match !patch_ht with Some ht -> ht | None -> load_patch base

  let cache () = match !cache_ht with
    | Some ht -> ht
    | None ->
      let tbl = Hashtbl.create 10 in
      cache_ht := Some tbl;
      tbl
  
  let get_from_data_file base index =
    if data_file_exists base then (
      let ic = open_data_file base in
      let len = input_binary_int ic in
      assert (index < len);
      seek_in ic (4 + (index * 4));
      let pos_data = input_binary_int ic in
      seek_in ic pos_data;
      let data = (Marshal.from_channel ic : D.t) in
      let c = cache () in
      Hashtbl.replace c index data;
      Some data)
    else None

  let get_from_cache base index = match !cache_ht with
    | Some ht -> Hashtbl.find_opt ht index
    | None -> None
  
  let get base index =
    match Hashtbl.find_opt (patch base) index with
    | Some _v as value -> value
    | None -> match get_from_cache base index with
      | Some _v as value -> value
      | None -> get_from_data_file base index

  let set base index value =
    let tbl = patch base in
    Hashtbl.replace tbl index value

  let unsafe_set index value =
    let tbl = Option.get !patch_ht in
    Hashtbl.replace tbl index value

  let write base =
    let tbl = patch base in
    if not (directory_exists base) then create_files base;
    let patchfile = D.patch_file base in
    let patchfile_tmp = patchfile ^ "~" in
    if Sys.file_exists patchfile_tmp then failwith "oups";
    let oc = Secure.open_out patchfile_tmp in
    Marshal.to_channel oc tbl [ Marshal.No_sharing ];
    close_out oc;
    Files.mv patchfile_tmp patchfile;
    Files.rm patchfile_tmp

  let empty () = patch_ht := Some (Hashtbl.create 1)

  let load_data build_from_scratch base : D.t array =
    if not (data_file_exists base) then (
      (*      log "no data file";*)
      build_from_scratch base
    )
    else (
      (*      log "some data file found";*)
      let ic = open_data_file base in
      let len = input_binary_int ic in
      seek_in ic (4 + (4 * len));

      let rec loop i l =
        if i = 0 then l
        else
          let l = (Marshal.from_channel ic : D.t) :: l in
          loop (i - 1) l
      in

      let data = Array.of_list @@ List.rev (loop len []) in
      data)

  let sync build_from_scratch base =
    (*    log "SYNC";*)
    if not (directory_exists base) then create_files base;
    let tbl = patch base in

    (*    log "LOAD";*)

    let data = load_data build_from_scratch base in

    (*    log "POST LOAD";*)

    let dfile = D.data_file base in
    let dfile_tmp = dfile ^ "~" in
    let oc = Secure.open_out dfile_tmp in

    let syncdata = Hashtbl.create (Array.length data) in
    Array.iteri (Hashtbl.add syncdata) data;
    Hashtbl.iter (Hashtbl.replace syncdata) tbl;
    let len = Hashtbl.length syncdata in
    let accesses = Array.make len 0 in

    let l = Hashtbl.fold (fun k v l -> (k, v) :: l) syncdata [] in
    let a = Array.of_list l in
    Array.sort (fun (k, _) (k', _) -> k - k') a;
    let a = Array.map snd a in

    output_binary_int oc len;
    seek_out oc (4 + (len * 4));
    Array.iteri
      (fun i data ->
        let pos = pos_out oc in
        Marshal.to_channel oc data [ Marshal.No_sharing ];
        accesses.(i) <- pos)
      a;
    seek_out oc 4;
    Array.iter (output_binary_int oc) accesses;
    close_out oc;
    close_data_file ();
    Files.mv dfile_tmp dfile;
    Files.rm dfile_tmp;
    Files.rm (D.patch_file base)
    (*    log "END SYNC"*)
end

module Legacy_driver = struct
  include Gwdb_legacy.Gwdb_driver

  let compatibility_directory = "gnwb25-2"
  let compatibility_file = "witness_notes"
  let fcompatibility_file = "fwitness_notes"
  let data_file = "witness_notes.dat"
  let fdata_file = "fwitness_notes.dat"

  module PersonData = struct
    type t = istr array array
    type index = iper
    type base = Gwdb_legacy.Gwdb_driver.base

    let directory base = Filename.concat (bdir base) compatibility_directory
    let patch_file base = Filename.concat (directory base) compatibility_file
    let data_file base = Filename.concat (directory base) data_file
  end

  module PatchPer = Store (PersonData)

  module FamilyData = struct
    type t = istr array array
    type index = ifam
    type base = Gwdb_legacy.Gwdb_driver.base

    let directory base = Filename.concat (bdir base) compatibility_directory
    let patch_file base = Filename.concat (directory base) fcompatibility_file
    let data_file base = Filename.concat (directory base) fdata_file
  end

  module PatchFam = Store (FamilyData)

  let versions = Version.[ gnwb20; gnwb21; gnwb22; gnwb23; gnwb24 ]

  type pers_event = (iper, istr) Def.gen_pers_event
  type fam_event = (iper, istr) Def.gen_fam_event

  type person = {
    person : Gwdb_legacy.Gwdb_driver.person;
    base : Gwdb_legacy.Gwdb_driver.base;
    mutable witness_notes : istr array array option;
  }

  type family = {
    family : Gwdb_legacy.Gwdb_driver.family;
    base : Gwdb_legacy.Gwdb_driver.base;
    mutable witness_notes : istr array array option;
  }

  
  let get_pers_wit_notes (p : person) ie iw = match p.witness_notes with
    | Some a when Array.length a > 0 && Array.length a.(ie) > 0 -> a.(ie).(iw)
    | Some a -> empty_string
    | None ->
      let iper = Gwdb_legacy.Gwdb_driver.get_iper p.person in
      if iper = dummy_iper then begin
        p.witness_notes <- Some [||];
        empty_string
      end
      else
        let notes = PatchPer.get p.base iper in
        match notes with
        | Some wnotes ->
          p.witness_notes <- notes;
          if Array.length wnotes = 0 then empty_string
          else if Array.length wnotes.(ie) = 0 then empty_string
          else wnotes.(ie).(iw)
        | None ->
          p.witness_notes <- Some [||];
          empty_string

  let get_fam_wit_notes (f : family) ie iw = match f.witness_notes with
    | Some a when Array.length a > 0 && Array.length a.(ie) > 0 -> a.(ie).(iw)
    | Some a -> empty_string
    | None ->
      let ifam = Gwdb_legacy.Gwdb_driver.get_ifam f.family in
      if ifam = dummy_ifam then begin
        f.witness_notes <- Some [||];
        empty_string
      end
      else
        let notes = PatchFam.get f.base ifam in
        match notes with
        | Some wnotes ->
          f.witness_notes <- notes;
          if Array.length wnotes = 0 then empty_string
          else if Array.length wnotes.(ie) = 0 then empty_string
          else wnotes.(ie).(iw)
        | None ->
          f.witness_notes <- Some [||];
          empty_string
  


  let gen_person_of_person p =
    let gen_pers = gen_person_of_person p.person in
    let pevents =
      List.mapi
        (fun ie pe ->
          let pe = Translate.legacy_to_def_pevent empty_string pe in
          let epers_witnesses =
            Array.mapi
              (fun iw (ip, wk, _) -> (ip, wk, get_pers_wit_notes p ie iw))
              pe.epers_witnesses
          in
          { pe with epers_witnesses })
        gen_pers.pevents
    in
    let gen_pers = Translate.legacy_to_def_person empty_string gen_pers in
    { gen_pers with pevents }

  let person_of_gen_person base (genpers, gen_ascend, gen_union) =
    let pevents = genpers.Def.pevents in
    let witness_notes = 
      Some (List.map
        (fun pe ->
          Array.map (fun (_, _, wnote) -> wnote) pe.Def.epers_witnesses)
        pevents
      |> Array.of_list)
    in
    let genpers = Translate.as_legacy_person genpers in
    let person = person_of_gen_person base (genpers, gen_ascend, gen_union) in
    { person; base; witness_notes }

  let no_person iper =
    let nop = no_person iper in
    Translate.legacy_to_def_person empty_string nop

  let _test_on_person base genpers =
    let pers_events = genpers.Def.pevents in
    List.iter
      (fun pers_event ->
        let witnesses = pers_event.Def.epers_witnesses in
        let wnotes =
          Array.map (fun (_ip, _wk, wnote) -> sou base wnote) witnesses
        in
        Array.iter log wnotes)
      pers_events

  let witness_notes_of_events pevents : istr array array =
    let l = List.map
        (fun pe ->
           let a = Array.map (fun (_, _, wnote) -> wnote) pe.Def.epers_witnesses in
           if Array.exists (fun wnote -> not (is_empty_string wnote)) a then
             a
           else [||]
        )
        pevents
    in
    let has_data = List.exists (fun a -> Array.length a <> 0) l in
    if has_data then begin
      (*print_endline "has_data";*)
      Array.of_list l
    end else begin
        (*      print_endline "no data";*)
      [||]
    end
    
  let fwitness_notes_of_events fevents : istr array array =
    let l = List.map
        (fun fe ->
           let a = Array.map (fun (_, _, wnote) -> wnote) fe.Def.efam_witnesses in
           if Array.exists (fun wnote -> not (is_empty_string wnote)) a then
             a
           else [||]
        )
        fevents
    in
    let has_data = List.exists (fun a -> Array.length a <> 0) l in
    if has_data then Array.of_list l else [||]


  let patch_person base iper genpers =
(*    log @@ "PATCH PERSON" ^ string_of_int iper;
    test_on_person base genpers;
      log "LETS PATCH"; *)
    let pevents = genpers.Def.pevents in
    let genpers = Translate.as_legacy_person genpers in
    patch_person base iper genpers;
    let witnotes = witness_notes_of_events pevents in
    PatchPer.set base iper witnotes

  let insert_person base iper genpers =
(*    log "INSERT PERSON";
    test_on_person base genpers;
      log "LETS INSERT";*)
    let pevents = genpers.Def.pevents in
    let genpers = Translate.as_legacy_person genpers in
    insert_person base iper genpers;
    let witnotes = witness_notes_of_events pevents in
    PatchPer.set base iper witnotes

  let commit_patches base =
    (*    log "COMMIT LEGACY PATCHES";*)
    commit_patches base;
    (*    log "COMMIT NOTES PATCHES";*)
    PatchPer.write base;
    PatchFam.write base

  let get_pevents p =
    let pevents = get_pevents p.person in
    let pevents =
      List.mapi
        (fun i pe ->
          let pe = Translate.legacy_to_def_pevent empty_string pe in
          let wnotes_f = get_pers_wit_notes p i in
          let witnesses =
            Array.mapi
              (fun i (ip, wk, _) -> (ip, wk, wnotes_f i))
              pe.epers_witnesses
          in
          { pe with epers_witnesses = witnesses })
        pevents
    in
    pevents

  let get_fevents f =
    let fevents = get_fevents f.family in
    let fevents =
      List.mapi
        (fun i fe ->
          let fe = Translate.legacy_to_def_fevent empty_string fe in
          let wnotes_f = get_fam_wit_notes f i in
          let witnesses =
            Array.mapi
              (fun i (ip, wk, _) -> (ip, wk, wnotes_f i))
              fe.efam_witnesses
          in
          { fe with efam_witnesses = witnesses })
        fevents
    in
    fevents

  let build_from_scratch_pevents base =
    let persons = Gwdb_legacy.Gwdb_driver.persons base in
    let notes : istr array array list =
      List.rev
      @@ Gwdb_legacy.Gwdb_driver.Collection.fold
           (fun l p ->
             let pevents = Gwdb_legacy.Gwdb_driver.get_pevents p in
             let witness_array_list =
               List.map
                 (fun pe -> pe.Gwdb_legacy.Dbdisk.epers_witnesses)
                 pevents
             in
             let notes =
               Array.of_list
               @@ List.map
                    (Array.map (fun _ -> empty_string))
                    witness_array_list
             in
             notes :: l)
           [] persons
    in
    Array.of_list notes
  
  let build_from_scratch_fevents base =
    (*    log "BUILD FEVENTS";*)
    let families = Gwdb_legacy.Gwdb_driver.families base in
    (* log "BUILD FEVENTS2"; *)
    let notes : istr array array list =
      List.rev
      @@ Gwdb_legacy.Gwdb_driver.Collection.fold
           (fun l f ->
            (*log "SOME FEVENT";*)
             let fevents = Gwdb_legacy.Gwdb_driver.get_fevents f in
             let witness_array_list =
               List.map (fun fe -> fe.Gwdb_legacy.Dbdisk.efam_witnesses) fevents
             in
             let notes =
               Array.of_list
               @@ List.map
                    (Array.map (fun _ -> empty_string))
                    witness_array_list
             in
             notes :: l)
           [] families
    in
    (*    log "FEVENTS BUILT";*)
    Array.of_list notes
  
  let build_from_scratch_pevents base =
    let persons = Gwdb_legacy.Gwdb_driver.persons base in
    let max_index, data = Gwdb_legacy.Gwdb_driver.Collection.fold (fun (max_index, l) p ->
        (*print_endline @@ string_of_int (get_iper p);*)
        let iper = get_iper p in
        max max_index iper, ((iper, [||]) :: l)) (0, []) persons
    in
    let d = Array.make (max_index + 1) ([||]) in
    List.iter (fun (i, v) -> Array.unsafe_set d i v ) data;
    d
    
  let build_from_scratch_fevents base =
    let families = Gwdb_legacy.Gwdb_driver.families base in
    let max_index, data = Gwdb_legacy.Gwdb_driver.Collection.fold (fun (max_index, l) f ->
        (*print_endline @@ string_of_int (get_ifam f);*)
        let ifam = get_ifam f in
        max max_index ifam, ((ifam, [||]) :: l)) (0, []) families
    in
    let d = Array.make (max_index + 1) ([||]) in
    List.iter (fun (i, v) -> Array.unsafe_set d i v ) data;
    d
  
  (* TODO : properly sync *)
  let sync ?(scratch = false) ~save_mem base =

    sync ~scratch ~save_mem base;
    let bname = Gwdb_legacy.Gwdb_driver.bdir base in
    close_base base;
    let base = Gwdb_legacy.Gwdb_driver.open_base bname in
    (*PatchPer.write base;
      PatchFam.write base*)
    (*    log "PERS SYNC"; *)
    PatchPer.sync build_from_scratch_pevents base;
    (*    log "FAM SYNC";*)
    PatchFam.sync build_from_scratch_fevents base


  let make bname particles
      ( (persons, ascends, unions),
        (families, couples, descends),
        string_arrays,
        base_notes ) =
    (*let persons = Array.map Translate.as_legacy_person persons in
      let families = Array.map Translate.as_legacy_family families in*)
    PatchPer.empty ();
    PatchFam.empty ();
    let persons =
      Array.map
        (fun p ->
          let leg_person = Translate.as_legacy_person p in
          PatchPer.unsafe_set p.key_index (witness_notes_of_events p.pevents);
          leg_person)
        persons
    in
    let families =
      Array.map
        (fun f ->
          let leg_family = Translate.as_legacy_family f in
          PatchFam.unsafe_set f.fam_index (fwitness_notes_of_events f.fevents);
          leg_family)
        families
    in
    let base =
      make bname particles
        ( (persons, ascends, unions),
          (families, couples, descends),
          string_arrays,
          base_notes )
    in

    (* TODO : properly sync *)

    (*PatchPer.write base;
      PatchFam.write base;*)
    (*    log "PERS SYNC";*)
    PatchPer.sync build_from_scratch_pevents base;
    (*    log "FAM SYNC";*)
    PatchFam.sync build_from_scratch_fevents base;
    base

  let open_base bname =
    (*    log @@ "BNAME:" ^ bname;*)
    let base = open_base bname in
    (*    log @@ "Bdir:" ^ bdir base;*)
    base

  let close_base base =
    (*    log "CLOSING THE BASE";*)
    close_base base;
    PatchPer.close_data_file ();
    PatchFam.close_data_file ()

  let empty_person base iper =
    let p = empty_person base iper in
    { person = p; base; witness_notes = Some [||] }

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
  let get_public_name p = get_public_name p.person
  let get_qualifiers p = get_qualifiers p.person
  let get_related p = get_related p.person
  let get_rparents p = get_rparents p.person
  let get_sex p = get_sex p.person
  let get_surname p = get_surname p.person
  let get_surnames_aliases p = get_surnames_aliases p.person
  let get_titles p = get_titles p.person
  let gen_ascend_of_person p = gen_ascend_of_person p.person
  let gen_union_of_person p = gen_union_of_person p.person

(*  let witness_notes base iper =
    if iper = dummy_iper then [||]
    else
    match PatchPer.get base iper with
    | Some notes -> notes
    | None ->
        let p = poi base iper in
        let genpers = Gwdb_legacy.Gwdb_driver.gen_person_of_person p in
        let pevents = genpers.Gwdb_legacy.Dbdisk.pevents in
        let witnesses_notes =
          List.map
            (fun pe ->
              let wits = pe.Gwdb_legacy.Dbdisk.epers_witnesses in
              Array.make (Array.length wits) empty_string)
            pevents
          |> Array.of_list
        in
        witnesses_notes
*)
(*  let fwitness_notes base ifam =
    if ifam = dummy_ifam then [||]
    else
    match PatchFam.get base ifam with
    | Some notes -> notes
    | None ->
        let f = foi base ifam in
        let genfam = Gwdb_legacy.Gwdb_driver.gen_family_of_family f in
        let fevents = genfam.Gwdb_legacy.Dbdisk.fevents in
        let witnesses_notes =
          List.map
            (fun fe ->
              let wits = fe.Gwdb_legacy.Dbdisk.efam_witnesses in
              Array.make (Array.length wits) empty_string)
            fevents
          |> Array.of_list
        in
        witnesses_notes
*)
  let poi base iper =
    { person = poi base iper; base; witness_notes = None }

  let base_visible_get base (f : person -> bool) iper =
    let f person =
      f { person; base; witness_notes = None }
    in
    base_visible_get base f iper

  let persons base =
    let coll = persons base in
    Collection.map
      (fun person ->
        { person; base; witness_notes = None })
      coll

  let empty_family base ifam =
    let f = empty_family base ifam in
    { family = f; base; witness_notes = Some [||] }

  let gen_family_of_family f =
    let gen_fam = gen_family_of_family f.family in
    let fevents =
      List.mapi
        (fun ie fe ->
          let fe = Translate.legacy_to_def_fevent empty_string fe in
          let efam_witnesses =
            Array.mapi
              (fun iw (ip, wk, _) -> (ip, wk, get_fam_wit_notes f ie iw))
              fe.efam_witnesses
          in
          { fe with efam_witnesses })
        gen_fam.fevents
    in
    let gen_fam = Translate.legacy_to_def_family empty_string gen_fam in
    { gen_fam with fevents }

  let family_of_gen_family base (genfam, gen_couple, gen_descend) =
    let fevents = genfam.Def.fevents in
    let witness_notes =
      Some (List.map
        (fun fe -> Array.map (fun (_, _, wnote) -> wnote) fe.Def.efam_witnesses)
        fevents
      |> Array.of_list)
    in
    let genfam = Translate.as_legacy_family genfam in
    let family = family_of_gen_family base (genfam, gen_couple, gen_descend) in
    { family; base; witness_notes }

  let no_family ifam =
    let nof = no_family ifam in
    Translate.legacy_to_def_family empty_string nof

  let patch_family base ifam genfam =
    (*    log @@ "PATCH FAMILY" ^ string_of_int ifam;*)
    (* TODO HANDLE WNOTES *)
    (*    log "LETS PATCH";*)
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    patch_family base ifam genfam;
    let witnotes = fwitness_notes_of_events fevents in
    PatchFam.set base ifam witnotes

  let insert_family base ifam genfam =
(*    log "INSERT FAMILY";
      log "LETS INSERT";*)
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    insert_family base ifam genfam;
    let witnotes = fwitness_notes_of_events fevents in
    PatchFam.set base ifam witnotes

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
    { family = foi base ifam; base; witness_notes = None }

  let families ?(select = fun _ -> true) base =
    let select f = select { family = f; base; witness_notes = None } in
    let coll = families ~select base in
    Collection.map
      (fun family ->
        { family; base; witness_notes = None })
      coll
end

module Driver = Compat.Make (Legacy_driver) (Legacy_driver)
include Driver
