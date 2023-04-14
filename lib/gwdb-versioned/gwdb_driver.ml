

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
  val tmp_file : string -> string
end

module Store (D : Data) : sig
  val get : D.base -> D.index -> D.t option
  val set : D.base -> D.index -> D.t option -> unit
  val unsafe_set : D.index -> D.t option -> unit
  val write : D.base -> unit
  val sync : (D.base -> D.t option array) -> D.base -> unit
  val empty : unit -> unit
  val close_data_file : unit -> unit
  val move_data_file : D.base -> unit
  val move_patch_file : D.base -> unit
  val remove_patch_file : D.base -> unit
end = struct
  type t = (D.index, D.t option) Hashtbl.t

  let patch_ht : (D.index, D.t option) Hashtbl.t option ref = ref None

  let data_file_in_channel : in_channel option ref = ref None

  let cache_ht  : (D.index, D.t option) Hashtbl.t option ref = ref None
  
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

  let move_data_file base =
    let dataf = D.data_file base in
    let tmp = D.tmp_file dataf in
    Files.mv tmp dataf

  let move_patch_file base =
    let patchf = D.patch_file base in
    let tmp = D.tmp_file patchf in
    Files.mv tmp patchf

  let remove_patch_file base =
    let patchf = D.patch_file base in
    Files.rm patchf
  
  let load_patch base =
    if patch_file_exists base then (
      let file = D.patch_file base in
      let ic = Secure.open_in file in
      let tbl = (Marshal.from_channel ic : t) in
      close_in ic;
      patch_ht := Some tbl;
      tbl)
    else begin
      let tbl = Hashtbl.create 1 in
      patch_ht := Some tbl;
      tbl
    end

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
      if not (index < len) then None
      else begin
        assert (index < len);
        seek_in ic (4 + (index * 4));
        let pos_data = input_binary_int ic in
        if pos_data <> -1 then begin
          seek_in ic pos_data;
          let data = (Marshal.from_channel ic : D.t) in
          let c = cache () in
          Hashtbl.replace c index (Some data);
          Some data
        end
        else
          None
      end
    )
    else None

  let get_from_cache base index = match !cache_ht with
    | Some ht -> Hashtbl.find_opt ht index
    | None -> None
  
  let get base index =
    match Hashtbl.find_opt (patch base) index with
    | Some v -> v
    | None -> match get_from_cache base index with
      | Some v -> v
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
    let patchfile_tmp = D.tmp_file patchfile in
    if Sys.file_exists patchfile_tmp then failwith "Error while writing patch file : temporary file remained";
    let oc = Secure.open_out patchfile_tmp in
    Marshal.to_channel oc tbl [ Marshal.No_sharing ];
    close_out oc

  let empty () = patch_ht := Some (Hashtbl.create 1)

  let load_data build_from_scratch base : D.t option array =
    if not (data_file_exists base) then
      build_from_scratch base
    else (
      let ic = open_data_file base in
      let len = input_binary_int ic in

      let get_pos i =
        seek_in ic (4 + 4 * i);
        let pos = input_binary_int ic in
        pos
      in

      seek_in ic (4 + (4 * len));

      let rec loop i l =
        if i = 0 then l
        else
          let pos = get_pos (i - 1) in
          if pos = -1 then loop (i - 1) (None :: l)
          else begin
            seek_in ic pos;
            let l = Some (Marshal.from_channel ic : D.t) :: l in
            loop (i - 1) l
          end
      in

      let data = Array.of_list @@ loop len [] in
      data)

  let sync build_from_scratch base =

    if not (directory_exists base) then create_files base;

    let tbl = patch base in
    let data = load_data build_from_scratch base in

    let dfile = D.data_file base in
    let dfile_tmp = D.tmp_file dfile in
    if Sys.file_exists dfile_tmp then failwith "Error while writing data file : temporary file remained";

    let oc = Secure.open_out dfile_tmp in

    let syncdata = Hashtbl.create (Array.length data) in
    Array.iteri (Hashtbl.add syncdata) data;
    Hashtbl.iter (Hashtbl.replace syncdata) tbl;


    let max_index, l = Hashtbl.fold (fun k v (max_index, l) ->
        let max_index = max max_index k in max_index, (k, v) :: l)
        syncdata (-1,[])
    in

    let a = Array.of_list l in
    Array.sort (fun (k, _) (k', _) -> k - k') a;

    let len = max_index + 1 in
    let accesses = Array.make len (-1) in
    
    output_binary_int oc len;
    seek_out oc (4 + (len * 4));
    Array.iter
      (fun (index, data) ->
         match data with
         | Some data ->
           let pos = pos_out oc in
           Marshal.to_channel oc data [ Marshal.No_sharing ];
           accesses.(index) <- pos
         | None -> accesses.(index) <- -1)
      a;
    seek_out oc 4;
    Array.iter (output_binary_int oc) accesses;
    close_out oc;
    close_data_file ()
end

module Legacy_driver = struct
  include Gwdb_legacy.Gwdb_driver

  let compatibility_directory = "gnwb25-2"
  let compatibility_file = "witness_notes"
  let fcompatibility_file = "fwitness_notes"
  let data_file = "witness_notes.dat"
  let fdata_file = "fwitness_notes.dat"

  let directory base = Filename.concat (bdir base) compatibility_directory
  let patch_file base = Filename.concat (directory base) compatibility_file
  let data_file base = Filename.concat (directory base) data_file
  let fpatch_file base = Filename.concat (directory base) fcompatibility_file
  let fdata_file base = Filename.concat (directory base) fdata_file

  let tmp_file fname = fname ^ "~"
  
  module PersonData = struct
    type t = istr array array
    type index = iper
    type base = Gwdb_legacy.Gwdb_driver.base

    let directory = directory
    let patch_file = patch_file
    let data_file = data_file
    let tmp_file = tmp_file
  end

  module PatchPer = Store (PersonData)

  module FamilyData = struct
    type t = istr array array
    type index = ifam
    type base = Gwdb_legacy.Gwdb_driver.base

    let directory = directory
    let patch_file = fpatch_file
    let data_file = fdata_file
    let tmp_file = tmp_file
  end

  module PatchFam = Store (FamilyData)

  let versions = Version.[ gnwb20; gnwb21; gnwb22; gnwb23; gnwb24 ]

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
  
  type pers_event = {
    pevent : Gwdb_legacy.Gwdb_driver.pers_event;
    event_index : int;
    event_person : person;
    mutable pwitness_notes : istr array option;
    mutable witnesses : (iper * Def.witness_kind * istr) array option
  }

  type fam_event = {
    fevent : Gwdb_legacy.Gwdb_driver.fam_event;
    event_index : int;
    event_family : family;
    mutable fwitness_notes : istr array option;
    mutable witnesses : (iper * Def.witness_kind * istr) array option
  }

  let poi_ht : (iper, person) Hashtbl.t option ref = ref None
  let foi_ht : (ifam, family) Hashtbl.t option ref = ref None


  let reset_poi_ht () = match !poi_ht with
    | Some ht ->
      Hashtbl.clear ht;
      poi_ht := None
    | None -> ()    

  let reset_foi_ht () = match !foi_ht with
    | Some ht ->
      Hashtbl.clear ht;
      foi_ht := None
    | None -> ()    
  
  let cache_foi_poi = ref true

  let set_fpoi_cache base b =
    reset_poi_ht ();
    reset_foi_ht ();
    cache_foi_poi := b
  
  let find_poi iper =
    if not !cache_foi_poi then None
    else
      match !poi_ht with
      | Some ht -> Hashtbl.find_opt ht iper
      | None -> poi_ht := Some (Hashtbl.create 1); None

  let find_foi ifam =
    if not !cache_foi_poi then None
    else
    match !foi_ht with
    | Some ht -> Hashtbl.find_opt ht ifam
    | None -> foi_ht := Some (Hashtbl.create 1); None

  let set_poi iper data =
    if !cache_foi_poi then
      match !poi_ht with
      | Some ht -> Hashtbl.add ht iper data
      | _ -> ()

  let set_foi ifam data =
    if !cache_foi_poi then
      match !foi_ht with
      | Some ht -> Hashtbl.add ht ifam data
      | _ -> ()

  let clear_poi iper = match !poi_ht with
    | Some ht -> Hashtbl.remove ht iper
    | _ -> ()

  let clear_foi ifam = match !foi_ht with
    | Some ht -> Hashtbl.remove ht ifam
    | _ -> ()
  

  let get_pers_full_wit_notes (p : person) = match p.witness_notes with
    | Some a when Array.length a > 0 ->
      fun ie ->
        if Array.length a.(ie) > 0 then
          fun iw -> a.(ie).(iw)
        else
          fun _iw -> empty_string
    | Some a ->
      fun ie iw -> empty_string 
    | None ->
      let iper = Gwdb_legacy.Gwdb_driver.get_iper p.person in
      if iper = dummy_iper then begin
        p.witness_notes <- Some [||];
        fun _ie _iw -> empty_string
      end
      else
        let notes = PatchPer.get p.base iper in
        match notes with
        | Some wnotes ->
          p.witness_notes <- notes;
          fun ie ->
            if Array.length wnotes.(ie) = 0 then fun _iw -> empty_string
            else fun iw -> wnotes.(ie).(iw)
        | None ->
          p.witness_notes <- Some [||];
          fun _ie _iw -> empty_string
  
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
          if Array.length wnotes.(ie) = 0 then empty_string
          else wnotes.(ie).(iw)
        | None ->
          p.witness_notes <- Some [||];
          empty_string

  let get_fam_full_wit_notes f = match f.witness_notes with
    | Some a when Array.length a > 0 ->
      fun ie ->
        if Array.length a.(ie) > 0 then
          fun iw -> a.(ie).(iw)
        else
          fun _iw -> empty_string
    | Some a ->
      fun ie iw -> empty_string 
    | None ->
      let ifam = Gwdb_legacy.Gwdb_driver.get_ifam f.family in
      if ifam = dummy_ifam then begin
        f.witness_notes <- Some [||];
        fun _ie _iw -> empty_string
      end
      else
        let notes = PatchFam.get f.base ifam in
        match notes with
        | Some wnotes ->
          f.witness_notes <- notes;
          fun ie ->
            if Array.length wnotes.(ie) = 0 then fun _iw -> empty_string
            else fun iw -> wnotes.(ie).(iw)
        | None ->
          f.witness_notes <- Some [||];
          fun _ie _iw -> empty_string
  
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
          if Array.length wnotes.(ie) = 0 then empty_string
          else wnotes.(ie).(iw)
        | None ->
          f.witness_notes <- Some [||];
          empty_string
  


  let gen_person_of_person (p : person) =
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

  let witness_notes_of_events pevents : istr array array option =
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
    if has_data then
      Some (Array.of_list l)
    else None
    
  let fwitness_notes_of_events fevents : istr array array option =
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
    if has_data then Some (Array.of_list l) else None


  let patch_person base iper genpers =
    let pevents = genpers.Def.pevents in
    let genpers = Translate.as_legacy_person genpers in
    patch_person base iper genpers;
    let witnotes = witness_notes_of_events pevents in
    PatchPer.set base iper witnotes;
    clear_poi iper

  let insert_person base iper genpers =
    let pevents = genpers.Def.pevents in
    let genpers = Translate.as_legacy_person genpers in
    insert_person base iper genpers;
    let witnotes = witness_notes_of_events pevents in
    PatchPer.set base iper witnotes;
    clear_poi iper

  let delete_person base iper =
    Gwdb_legacy.Gwdb_driver.delete_person base iper;
    clear_poi iper

  let commit_patches base =
    PatchPer.write base;
    PatchFam.write base;
    commit_patches base;
    PatchPer.move_patch_file base;
    PatchFam.move_patch_file base

  let pwitness_notes_of_pevent (pe : pers_event) = match pe.pwitness_notes with
    | Some a -> a
    | None ->
      let wlen = Array.length pe.pevent.epers_witnesses in
      let a = Array.init wlen (get_pers_full_wit_notes pe.event_person pe.event_index) in
      pe.pwitness_notes <- Some a;
      a

  let fwitness_notes_of_fevent (fe : fam_event) = match fe.fwitness_notes with
    | Some a -> a
    | None ->
      let wlen = Array.length fe.fevent.efam_witnesses in
      let a = Array.init wlen (get_fam_full_wit_notes fe.event_family fe.event_index) in
      fe.fwitness_notes <- Some a;
      a
  
  let get_pevents (p : person) =
    let pevents = Gwdb_legacy.Gwdb_driver.get_pevents p.person in
    List.mapi (fun ie pevent ->
        { pevent; event_person = p; event_index = ie; pwitness_notes = None; witnesses = None })
      pevents
  
  let get_pevent_name pe = pe.pevent.epers_name

  let get_pevent_date pe = pe.pevent.epers_date

  let get_pevent_place pe = pe.pevent.epers_place
  let get_pevent_reason pe = pe.pevent.epers_reason
  let get_pevent_note pe = pe.pevent.epers_note
  let get_pevent_src pe = pe.pevent.epers_src
  let get_pevent_witnesses pe = pe.pevent.epers_witnesses
  let get_pevent_witness_notes pe = pwitness_notes_of_pevent pe

  let get_pevent_witnesses_and_notes (pe : pers_event) = match pe.witnesses with
    | Some a -> a
    | None ->
      let len = Array.length pe.pevent.epers_witnesses in
      let wnotes = pwitness_notes_of_pevent pe in
      let a = Array.init len (fun iw ->
          let ip, wk = pe.pevent.epers_witnesses.(iw) in
          let wnote = wnotes.(iw) in
          ip, wk, wnote
        ) in
      pe.witnesses <- Some a;
      a

  let gen_pevent_of_pers_event pe =
    let genpers = Translate.legacy_to_def_pevent empty_string pe.pevent in
    let len = Array.length pe.pevent.epers_witnesses in
    let wnotes = pwitness_notes_of_pevent pe in
    let epers_witnesses = Array.init len (fun i ->
        let ip, wk = pe.pevent.epers_witnesses.(i) in
        let wnote = wnotes.(i) in
        ip, wk, wnote
      ) in
    {genpers with epers_witnesses}
    
  let pers_event_of_gen_pevent base genpers = assert false

  let eq_pevent p1 p2 =
    p1.pevent = p2.pevent

  let eq_fevent f1 f2 =
    f1.fevent = f2.fevent
  
  let gen_fevent_of_fam_event fe =
    let genfam = Translate.legacy_to_def_fevent empty_string fe.fevent in
    let len = Array.length fe.fevent.efam_witnesses in
    let wnotes = fwitness_notes_of_fevent fe in
    let efam_witnesses = Array.init len (fun i ->
        let ip, wk = fe.fevent.efam_witnesses.(i) in
        let wnote = wnotes.(i) in
        ip, wk, wnote
      ) in
    {genfam with efam_witnesses}
    
  let fam_event_of_gen_fevent base genfam = assert false
    
  let get_fevents (f : family) =
    let fevents = Gwdb_legacy.Gwdb_driver.get_fevents f.family in
    List.mapi (fun ie fevent ->
        { fevent; fwitness_notes = None; event_index = ie;  witnesses = None; event_family = f})
      fevents

  let get_fevent_name fe = fe.fevent.efam_name

  let get_fevent_date fe = fe.fevent.efam_date

  let get_fevent_place fe = fe.fevent.efam_place
  let get_fevent_reason fe = fe.fevent.efam_reason
  let get_fevent_note fe = fe.fevent.efam_note
  let get_fevent_src fe = fe.fevent.efam_src
  let get_fevent_witnesses fe = fe.fevent.efam_witnesses
  let get_fevent_witness_notes fe = fwitness_notes_of_fevent fe

  let get_fevent_witnesses_and_notes fe =
    let len = Array.length fe.fevent.efam_witnesses in
    let wnotes = fwitness_notes_of_fevent fe in
    Array.init len (fun iw ->
        let ip, wk = fe.fevent.efam_witnesses.(iw) in
        let wnote = wnotes.(iw) in
        ip, wk, wnote
      )
  
  
  let build_from_scratch_pevents base =
    let persons = Gwdb_legacy.Gwdb_driver.persons base in
    let max_index, data = Gwdb_legacy.Gwdb_driver.Collection.fold (fun (max_index, l) p ->
        let iper = get_iper p in
        max max_index iper, ((iper, None) :: l)) (0, []) persons
    in
    let d = Array.make (max_index + 1) (None) in
    List.iter (fun (i, v) ->
        if i = -1 then ()
        else Array.unsafe_set d i v ) data;
    d
    
  let build_from_scratch_fevents base =
    let families = Gwdb_legacy.Gwdb_driver.families base in
    let max_index, data = Gwdb_legacy.Gwdb_driver.Collection.fold (fun (max_index, l) f ->
        let ifam = get_ifam f in
        max max_index ifam, ((ifam, None) :: l)) (0, []) families
    in
    let d = Array.make (max_index + 1) (None) in
    List.iter (fun (i, v) ->
        if i = -1 then ()
        else Array.unsafe_set d i v ) data;
    d

  let sync ?(scratch = false) ~save_mem base =
    let dir = Filename.concat (bdir base) compatibility_directory in
    if scratch && Sys.file_exists dir then Files.remove_dir dir;
    PatchPer.sync build_from_scratch_pevents base;
    PatchFam.sync build_from_scratch_fevents base;

    sync ~scratch ~save_mem base;

    PatchPer.move_data_file base;
    PatchPer.remove_patch_file base;
    PatchFam.move_data_file base;
    PatchFam.remove_patch_file base


  let make bname particles
      ( (persons, ascends, unions),
        (families, couples, descends),
        string_arrays,
        base_notes ) =
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


    let dir = Filename.concat (bdir base) compatibility_directory in
    if Sys.file_exists dir then Files.remove_dir dir;
    PatchPer.sync build_from_scratch_pevents base;

    PatchFam.sync build_from_scratch_fevents base;
    PatchPer.move_data_file base;
    PatchFam.move_data_file base;
    base

  let open_base = open_base

  let close_base base =
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
  
  let poi base iper =
    match find_poi iper with
    | Some p -> p
    | None ->
      let p = { person = poi base iper; base; witness_notes = None } in
      set_poi iper p;
      p

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
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    patch_family base ifam genfam;
    let witnotes = fwitness_notes_of_events fevents in
    PatchFam.set base ifam witnotes;
    clear_foi ifam

  let insert_family base ifam genfam =
    let fevents = genfam.Def.fevents in
    let genfam = Translate.as_legacy_family genfam in
    insert_family base ifam genfam;
    let witnotes = fwitness_notes_of_events fevents in
    PatchFam.set base ifam witnotes;
    clear_foi ifam

  let delete_family base ifam =
    Gwdb_legacy.Gwdb_driver.delete_family base ifam;
    clear_foi ifam

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
    match find_foi ifam with
    | Some f -> f
    | None ->
      let f = { family = foi base ifam; base; witness_notes = None } in
      set_foi ifam f;
      f

  let families ?(select = fun _ -> true) base =
    let select f = select { family = f; base; witness_notes = None } in
    let coll = families ~select base in
    Collection.map
      (fun family ->
        { family; base; witness_notes = None })
      coll
  
  let wrap_pid clear patch insert delete =
    let patch b i d = patch b i d; clear i in
    let insert b i d = insert b i d; clear i in
    let delete b i = delete b i; clear i in
    patch, insert, delete

  let wrap_iper_pid p i d = wrap_pid clear_poi p i d

  let wrap_ifam_pid p i d = wrap_pid clear_foi p i d

  let patch_ascend, insert_ascend, delete_ascend =
    wrap_iper_pid patch_ascend insert_ascend delete_ascend

  let patch_union, insert_union, delete_union =
    wrap_iper_pid patch_union insert_union delete_union

  let patch_descend, insert_descend, delete_descend =
    wrap_ifam_pid patch_descend insert_descend delete_descend

  let patch_couple, insert_couple, delete_couple =
    wrap_ifam_pid patch_couple insert_couple delete_couple

  let load_clear_array load clear =
    let load_array base =
      set_fpoi_cache base false;
      load base
    in
    let clear_array base =
      clear base;
      set_fpoi_cache base true
    in load_array, clear_array

  let load_ascends_array, clear_ascends_array =
    load_clear_array load_ascends_array clear_ascends_array

  let load_descends_array, clear_descends_array =
    load_clear_array load_descends_array clear_descends_array

  let load_unions_array, clear_unions_array =
    load_clear_array load_unions_array clear_unions_array

  let load_couples_array, clear_couples_array =
    load_clear_array load_couples_array clear_couples_array

end

module Driver = Compat.Make (Legacy_driver) (Legacy_driver)
include Driver
