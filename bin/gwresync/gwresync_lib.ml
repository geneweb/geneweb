module Privacy : sig
  val apply_cutoff_date :
    base:Gwdb.base -> cutoff_year:int -> verbosity:int -> bool

  val apply_list :
    base:Gwdb.base -> access:Def.access -> file:string -> verbosity:int -> bool
end = struct
  let nb_gen_by_century = 3
  let nb_years_by_gen = 30

  let year_of_cdate = function
    | Some (Date.Dgreg (d, _)) -> Some d.Date.year
    | _ -> None

  let oldest_year_of p =
    match year_of_cdate (Date.od_of_cdate (Gwdb.get_birth p)) with
    | Some _ as y -> y
    | None -> (
        match year_of_cdate (Date.od_of_cdate (Gwdb.get_baptism p)) with
        | Some _ as y -> y
        | None -> year_of_cdate (Date.date_of_death (Gwdb.get_death p)))

  let most_recent_year_of p =
    match year_of_cdate (Date.date_of_death (Gwdb.get_death p)) with
    | Some _ as y -> y
    | None -> (
        match year_of_cdate (Date.od_of_cdate (Gwdb.get_baptism p)) with
        | Some _ as y -> y
        | None -> year_of_cdate (Date.od_of_cdate (Gwdb.get_birth p)))

  let compute_ndgen cutoff_year y = (cutoff_year - y) * nb_gen_by_century / 100

  let mark_descendants base scanned old cutoff_year =
    let rec loop p ndgen =
      let p_key_index = Gwdb.get_iper p in
      if Gwdb.Marker.get scanned p_key_index < ndgen then
        let ndgen =
          match most_recent_year_of p with
          | Some y ->
              Gwdb.Marker.set scanned p_key_index max_int;
              compute_ndgen cutoff_year y
          | None ->
              Gwdb.Marker.set scanned p_key_index ndgen;
              ndgen
        in
        if ndgen > 0 then (
          let ndgen' = ndgen - 1 in
          Gwdb.Marker.set old p_key_index true;
          Array.iter
            (fun ifam ->
              let fam = Gwdb.foi base ifam in
              let sp = Gutil.spouse p_key_index fam in
              let ndgen'' =
                if sp <> Gwdb.dummy_iper && Gwdb.Marker.get scanned sp < ndgen
                then (
                  let ndgen_sp =
                    Option.fold ~none:ndgen
                      ~some:(compute_ndgen cutoff_year)
                      (most_recent_year_of (Gwdb.poi base sp))
                  in
                  if ndgen_sp > 0 then Gwdb.Marker.set old sp true;
                  ndgen_sp)
                else ndgen
              in
              Array.iter
                (fun c ->
                  loop (Gwdb.poi base c) (min ndgen' (max 0 (ndgen'' - 1))))
                (Gwdb.get_children fam))
            (Gwdb.get_family p))
    in
    loop

  let mark_ancestors base scanned cutoff_year changes p =
    let rec loop p =
      let i = Gwdb.get_iper p in
      if not (Gwdb.Marker.get scanned i) then (
        Gwdb.Marker.set scanned i true;
        (match oldest_year_of p with
        | Some y when y >= cutoff_year -> ()
        | _ ->
            if
              Gwdb.get_access p <> Def.Public
              && (not (Gwdb.is_quest_string (Gwdb.get_first_name p)))
              && not (Gwdb.is_quest_string (Gwdb.get_surname p))
            then (
              let gen_p =
                { (Gwdb.gen_person_of_person p) with Def.access = Def.Public }
              in
              Gwdb.patch_person base gen_p.Def.key_index gen_p;
              changes := true));
        Option.iter
          (fun ifam ->
            let cpl = Gwdb.foi base ifam in
            let fath = Gwdb.get_father cpl in
            let moth = Gwdb.get_mother cpl in
            if fath <> Gwdb.dummy_iper then loop (Gwdb.poi base fath);
            if moth <> Gwdb.dummy_iper then loop (Gwdb.poi base moth))
          (Gwdb.get_parents p))
    in
    loop p

  module DatesStore : sig
    type t

    val of_base : Gwdb.base -> verbosity:int -> t

    val fold :
      ('a -> iper:Gwdb.iper -> estimated_year:int -> 'a) -> 'a -> t -> 'a
  end = struct
    type computed_date = FoundDate of int | EstimatedDate of int | NoDate
    type computation = Result of computed_date | Ongoing | Todo

    let result date = Result date
    let estimated_date year = EstimatedDate year
    let date_of_result = function Result d -> Some d | Todo | Ongoing -> None

    module Store = struct
      type siblings = Gwdb.iper array

      type t = {
        iper_collection : Gwdb.iper Gwdb.Collection.t;
        iper_store : (Gwdb.iper, computation) Gwdb.Marker.t;
        ifam_store : (Gwdb.ifam, siblings option) Gwdb.Marker.t;
      }

      let get { iper_store; _ } iper = Gwdb.Marker.get iper_store iper
      let set { iper_store; _ } iper comp = Gwdb.Marker.set iper_store iper comp
      let get_siblings { ifam_store; _ } ifam = Gwdb.Marker.get ifam_store ifam

      let set_siblings { ifam_store; _ } ifam siblings =
        Gwdb.Marker.set ifam_store ifam siblings

      let create iper_collection ifam_collection =
        let iper_store = Gwdb.iper_marker iper_collection Todo in
        let ifam_store = Gwdb.ifam_marker ifam_collection None in
        { iper_collection; iper_store; ifam_store }

      let fold f acc t =
        Gwdb.Collection.fold
          (fun acc iper -> f acc iper (get t iper))
          acc t.iper_collection
    end

    type t = Store.t

    let fold f acc store =
      Store.fold
        (fun acc iper comp ->
          match comp with
          | Result (EstimatedDate estimated_year) -> f acc ~iper ~estimated_year
          | Result _ -> acc
          | Ongoing | Todo -> assert false)
        acc store

    let is_ongoing store iper = Store.get store iper = Ongoing

    let add_not_ongoing_to_queue store search_queue iper =
      if not (is_ongoing store iper) then Queue.push iper search_queue

    let valid_iper_opt ip = if ip = Gwdb.dummy_iper then None else Some ip

    let add_parents_to_queue search_queue store parents =
      let father =
        Option.bind (Option.map Gwdb.get_father parents) valid_iper_opt
      in
      let mother =
        Option.bind (Option.map Gwdb.get_mother parents) valid_iper_opt
      in
      Option.iter (add_not_ongoing_to_queue store search_queue) father;
      Option.iter (add_not_ongoing_to_queue store search_queue) mother

    let spouses_of_families iper families =
      Array.to_list families
      |> List.filter_map (fun fam -> valid_iper_opt (Gutil.spouse iper fam))
      |> Array.of_list

    let siblings_of_family store parents =
      let ifam = Gwdb.get_ifam parents in
      match Store.get_siblings store ifam with
      | Some siblings -> siblings
      | None ->
          let children = Gwdb.get_children parents in
          Store.set_siblings store ifam (Some children);
          children

    let add_one_gen_to_date = function
      | NoDate as d -> d
      | FoundDate d | EstimatedDate d -> estimated_date (d + nb_years_by_gen)

    let best_date_year_opt d1 d2 =
      match (d1, d2) with
      | (FoundDate d1 | EstimatedDate d1), (FoundDate d2 | EstimatedDate d2) ->
          if d1 > d2 then Some d1 else Some d2
      | (FoundDate d | EstimatedDate d), _ | _, (FoundDate d | EstimatedDate d)
        ->
          Some d
      | NoDate, NoDate -> None

    let year_opt_of_date = function
      | FoundDate d | EstimatedDate d -> Some d
      | NoDate -> None

    let best_estimated_date_of_years d1 d2 =
      match (d1, d2) with
      | Some d1, Some d2 -> Option.map estimated_date (best_date_year_opt d1 d2)
      | Some d, _ | _, Some d -> Option.map estimated_date (year_opt_of_date d)
      | None, None -> None

    let best_estimated_date_of_dates d1 d2 =
      let date_opt = Option.map estimated_date (best_date_year_opt d1 d2) in
      Option.value ~default:NoDate date_opt

    let best_date_of_parents store parents =
      let father =
        Option.bind (Option.map Gwdb.get_father parents) valid_iper_opt
      in
      let mother =
        Option.bind (Option.map Gwdb.get_mother parents) valid_iper_opt
      in
      let date_opt =
        match (father, mother) with
        | Some ifath, Some imoth ->
            let ifath_year_opt = date_of_result (Store.get store ifath) in
            let imoth_year_opt = date_of_result (Store.get store imoth) in
            let date_opt =
              best_estimated_date_of_years ifath_year_opt imoth_year_opt
            in
            Option.map add_one_gen_to_date date_opt
        | Some ifath, None ->
            let d = date_of_result (Store.get store ifath) in
            Option.map add_one_gen_to_date d
        | None, Some imoth ->
            let d = date_of_result (Store.get store imoth) in
            Option.map add_one_gen_to_date d
        | None, None -> None
      in
      Option.value ~default:NoDate date_opt

    let best_date_of_ipers_opt store ipers =
      Array.fold_left
        (fun best_date iper ->
          let computation = Store.get store iper in
          let date_opt = date_of_result computation in
          let best_year_opt =
            best_date_year_opt best_date (Option.value ~default:NoDate date_opt)
          in
          Option.value ~default:best_date
            (Option.map estimated_date best_year_opt))
        NoDate ipers

    let rec compute_stack' base store (ancestor_stack, to_compute_again)
        progress_was_made =
      if Stack.is_empty ancestor_stack then (
        List.iter (fun iper -> Stack.push iper ancestor_stack) to_compute_again;
        progress_was_made)
      else
        let iper = Stack.pop ancestor_stack in
        let person = Gwdb.poi base iper in
        let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
        let families = Array.map (Gwdb.foi base) (Gwdb.get_family person) in
        let spouses = spouses_of_families iper families in
        let siblings = Option.map (siblings_of_family store) parents in
        let date_parents = best_date_of_parents store parents in
        let date_spouses = best_date_of_ipers_opt store spouses in
        let date_siblings_opt =
          Option.map (best_date_of_ipers_opt store) siblings
        in
        let date_siblings = Option.value ~default:NoDate date_siblings_opt in
        let date =
          best_estimated_date_of_dates date_siblings
            (best_estimated_date_of_dates date_parents date_spouses)
        in
        let to_compute_again =
          if date = NoDate then iper :: to_compute_again
          else (
            Store.set store iper (result date);
            to_compute_again)
        in
        let progress_was_made = progress_was_made || date <> NoDate in
        compute_stack' base store
          (ancestor_stack, to_compute_again)
          progress_was_made

    let compute_stack base store ancestor_stack =
      compute_stack' base store (ancestor_stack, []) false

    let rec find_person_date base search_queue ancestor_stack_queue
        ancestor_stack (store : t) iper =
      Store.set store iper Ongoing;
      let person = Gwdb.poi base iper in
      match oldest_year_of person with
      | Some date ->
          Store.set store iper (result (FoundDate date));
          find_person_date_of_queue base search_queue ancestor_stack_queue
            ancestor_stack store
      | None ->
          let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
          add_parents_to_queue search_queue store parents;
          Stack.push iper ancestor_stack;
          find_person_date_of_queue base search_queue ancestor_stack_queue
            ancestor_stack store

    and find_person_date_of_queue base search_queue ancestor_stack_queue
        ancestor_stack store =
      if Queue.is_empty search_queue then
        Queue.push ancestor_stack ancestor_stack_queue
      else
        let iper = Queue.pop search_queue in
        match Store.get store iper with
        | Todo ->
            find_person_date base search_queue ancestor_stack_queue
              ancestor_stack store iper
        | Result _ | Ongoing ->
            find_person_date_of_queue base search_queue ancestor_stack_queue
              ancestor_stack store

    let find_person_date base store ancestor_stack_queue iper =
      let search_queue = Queue.create () in
      let ancestor_stack = Stack.create () in
      Queue.push iper search_queue;
      find_person_date_of_queue base search_queue ancestor_stack_queue
        ancestor_stack store

    let of_base base ~verbosity =
      let n = Gwdb.nb_of_persons base in
      let ipers_collection = Gwdb.ipers base in
      let ifams_collection = Gwdb.ifams base in
      let store = Store.create ipers_collection ifams_collection in
      let ancestor_stack_queue = Queue.create () in
      if verbosity >= 2 then ProgrBar.start ();
      Gwdb.Collection.iteri
        (fun i iper ->
          find_person_date base store ancestor_stack_queue iper;
          if verbosity >= 2 then ProgrBar.run i n)
        ipers_collection;
      let rec work_until_no_progress ancestor_stack_queue n_ancestor_stack_queue
          progress =
        if Queue.is_empty ancestor_stack_queue then
          if progress then
            work_until_no_progress n_ancestor_stack_queue (Queue.create ())
              false
          else
            Queue.iter
              (Stack.iter (fun iper -> Store.set store iper (result NoDate)))
              n_ancestor_stack_queue
        else
          let ancestor_stack = Queue.pop ancestor_stack_queue in
          let stack_progress = compute_stack base store ancestor_stack in
          if not (Stack.is_empty ancestor_stack) then
            Queue.push ancestor_stack n_ancestor_stack_queue;
          let progress = progress || stack_progress in
          work_until_no_progress ancestor_stack_queue n_ancestor_stack_queue
            progress
      in
      work_until_no_progress ancestor_stack_queue (Queue.create ()) false;
      if verbosity >= 2 then ProgrBar.finish ();
      store
  end

  let apply_cutoff_date ~base ~cutoff_year ~verbosity =
    let changes = ref false in
    let nb = Gwdb.nb_of_persons base in
    let ipers = Gwdb.ipers base in
    let old = Gwdb.iper_marker ipers false in
    let scanned = Gwdb.iper_marker ipers (-1) in
    if verbosity >= 2 then ProgrBar.start ();
    Gwdb.Collection.iteri
      (fun i ip ->
        if verbosity >= 2 then ProgrBar.run i nb;
        if Gwdb.Marker.get scanned ip < 0 then
          let p = Gwdb.poi base ip in
          mark_descendants base scanned old cutoff_year p 0)
      ipers;
    if verbosity >= 2 then ProgrBar.finish ();
    let scanned_anc = Gwdb.iper_marker ipers false in
    if verbosity >= 2 then ProgrBar.start ();
    Gwdb.Collection.iteri
      (fun i ip ->
        if verbosity >= 2 then ProgrBar.run i nb;
        if Gwdb.Marker.get old ip && not (Gwdb.Marker.get scanned_anc ip) then
          let p = Gwdb.poi base ip in
          mark_ancestors base scanned_anc cutoff_year changes p)
      ipers;
    if verbosity >= 2 then ProgrBar.finish ();
    let store = DatesStore.of_base base ~verbosity in
    let changed_by_dates =
      DatesStore.fold
        (fun ch ~iper ~estimated_year ->
          let p = Gwdb.poi base iper in
          match Gwdb.get_access p with
          | Def.IfTitles ->
              let access =
                if estimated_year > cutoff_year then Def.Private else Def.Public
              in
              let gen_p = { (Gwdb.gen_person_of_person p) with Def.access } in
              Gwdb.patch_person base gen_p.Def.key_index gen_p;
              true
          | Def.Private | Def.Public -> ch)
        false store
    in
    !changes || changed_by_dates

  let access_some_aux ~verbosity base access key changes =
    let ip_opt =
      match Gutil.person_ht_find_all base key with
      | [ ip ] -> Some ip
      | _ -> Gutil.person_of_string_dot_key base key
    in
    match ip_opt with
    | Some ip ->
        let p = Gwdb.poi base ip in
        if Gwdb.get_access p <> access then (
          let gen_p = { (Gwdb.gen_person_of_person p) with Def.access } in
          Gwdb.patch_person base gen_p.Def.key_index gen_p;
          changes := true)
    | None ->
        if verbosity >= 1 then (
          Printf.eprintf "Bad key %s\n" key;
          flush stderr)

  let apply_list ~base ~access ~file ~verbosity =
    if file = "" || not (Sys.file_exists file) then false
    else
      let changes = ref false in
      let ic = open_in file in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          try
            while true do
              let line = String.trim (input_line ic) in
              if line <> "" then
                access_some_aux ~verbosity base access line changes
            done;
            !changes
          with End_of_file -> !changes)
end

module Integrity : sig
  val perform_fixes : base:Gwdb.base -> verbosity:int -> int
end = struct
  let perform_fixes ~base ~verbosity =
    let person_fixes =
      [
        Geneweb.Fixbase.fix_invalid_occurrence_number;
        Geneweb.Fixbase.fix_person_parents;
        Geneweb.Fixbase.fix_nbds;
        Geneweb.Fixbase.fix_person_unions;
        Geneweb.Fixbase.fix_person_events_witnesses;
      ]
    in
    let family_fixes =
      [
        Geneweb.Fixbase.fix_family_parents;
        Geneweb.Fixbase.fix_family_children;
        Geneweb.Fixbase.fix_family_spouses;
        Geneweb.Fixbase.fix_family_events_witnesses;
        Geneweb.Fixbase.fix_family_divorce;
      ]
    in
    let progress = if verbosity >= 2 then ProgrBar.run else fun _ _ -> () in
    let report =
      if verbosity >= 2 then
        Some
          (fun s ->
            print_endline @@ "\t" ^ Geneweb.Fixbase.string_of_patch base s;
            flush stdout)
      else None
    in
    if verbosity >= 2 then ProgrBar.start ();
    let fix_count =
      Geneweb.Fixbase.perform_fixes ~report ~progress ~base ~person_fixes
        ~family_fixes
    in
    if verbosity >= 2 then ProgrBar.finish ();
    if verbosity >= 1 && fix_count > 0 then (
      Printf.printf "%d fixes performed\n" fix_count;
      flush stdout);
    fix_count
end

module CycleCheck : sig
  val detect_loop : base:Gwdb.base -> bool
end = struct
  let detect_loop ~base =
    let has_loop = ref false in
    Consang.check_noloop base (function
      | Def.OwnAncestor p ->
          Printf.printf
            "\nError: loop in database, %s is his/her own ancestor.\n"
            (Gutil.designation base p);
          flush stdout;
          has_loop := true
      | _ -> assert false);
    !has_loop
end

module Consanguinity : sig
  val compute : base:Gwdb.base -> scratch:bool -> verbosity:int -> bool
end = struct
  let compute ~base ~scratch ~verbosity =
    ConsangAll.compute ~verbosity base scratch
end

module NotesIndexing : sig
  val compute : base:Gwdb.base -> bdir:string -> verbosity:int -> unit
end = struct
  let notes_links s =
    if not (String.contains s '[') then ([], [])
    else
      let slen = String.length s in
      let rec loop list_nt list_ind pos i =
        if i >= slen then (list_nt, list_ind)
        else if s.[i] = '%' && i + 1 < slen then
          loop list_nt list_ind pos (i + 2)
        else if s.[i] = '[' then
          match Geneweb.NotesLinks.misc_notes_link s i with
          | Geneweb.NotesLinks.WLpage (j, _, lfname, _, _) ->
              let list_nt =
                if List.mem lfname list_nt then list_nt else lfname :: list_nt
              in
              loop list_nt list_ind pos j
          | Geneweb.NotesLinks.WLperson (j, key, _, text) ->
              let list_ind =
                let link = { Def.NLDB.lnTxt = text; lnPos = pos } in
                (key, link) :: list_ind
              in
              loop list_nt list_ind (pos + 1) j
          | Geneweb.NotesLinks.WLwizard (j, _, _) ->
              loop list_nt list_ind (pos + 1) j
          | Geneweb.NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
        else loop list_nt list_ind pos (i + 1)
      in
      loop [] [] 1 0

  let compute ~base ~bdir ~verbosity =
    let bdir =
      if Filename.check_suffix bdir ".gwb" then bdir else bdir ^ ".gwb"
    in
    let nb_ind = Gwdb.nb_of_persons base in
    let nb_fam = Gwdb.nb_of_families base in
    let db = ref [] in
    if verbosity >= 1 then (
      Printf.eprintf "--- database notes\n";
      flush stderr);
    let list = notes_links (Gwdb.base_notes_read base "") in
    if list <> ([], []) then
      db := Geneweb.NotesLinks.add_in_db !db Def.NLDB.PgNotes list;
    if verbosity >= 1 then (
      Printf.eprintf "--- wizard notes\n";
      flush stderr);
    (try
       let wiznotes_dir = Filename.concat bdir (Gwdb.base_wiznotes_dir base) in
       let files = Sys.readdir wiznotes_dir in
       for i = 0 to Array.length files - 1 do
         let file = files.(i) in
         if Filename.check_suffix file ".txt" then
           let wizid = Filename.chop_suffix file ".txt" in
           let wfile = Filename.concat wiznotes_dir file in
           let content = Mutil.read_file_content wfile in
           let list = notes_links content in
           if list <> ([], []) then
             db :=
               Geneweb.NotesLinks.add_in_db !db (Def.NLDB.PgWizard wizid) list
       done
     with Sys_error _ -> ());
    if verbosity >= 1 then (
      Printf.eprintf "--- misc notes\n";
      flush stderr);
    let ndir = Filename.concat bdir (Gwdb.base_notes_dir base) in
    let rec loop dir name =
      try
        let cdir = Filename.concat ndir dir in
        let files = Sys.readdir cdir in
        for i = 0 to Array.length files - 1 do
          let file = files.(i) in
          if Filename.check_suffix file ".txt" then (
            let fnotes = Filename.chop_suffix file ".txt" in
            let file_rel = Filename.concat dir fnotes in
            let list = notes_links (Gwdb.base_notes_read base file_rel) in
            if list <> ([], []) then
              let entry_name =
                if name = "" then fnotes
                else
                  Printf.sprintf "%s%c%s" name Geneweb.NotesLinks.char_dir_sep
                    fnotes
              in
              db :=
                Geneweb.NotesLinks.add_in_db !db (Def.NLDB.PgMisc entry_name)
                  list)
          else
            loop (Filename.concat dir file)
              (if name = "" then file
              else
                Printf.sprintf "%s%c%s" name Geneweb.NotesLinks.char_dir_sep
                  file)
        done
      with Sys_error _ -> ()
    in
    loop Filename.current_dir_name "";
    let buffer = Buffer.create 1024 in
    let add_string istr =
      if not (Gwdb.is_empty_string istr) then (
        Buffer.add_string buffer (Gwdb.sou base istr);
        Buffer.add_char buffer ' ')
    in
    if verbosity >= 2 then (
      ProgrBar.full := '*';
      Printf.eprintf "--- individual notes\n";
      flush stderr;
      ProgrBar.start ());
    Gwdb.Collection.iteri
      (fun i p ->
        if verbosity >= 2 then ProgrBar.run i nb_ind;
        Buffer.reset buffer;
        add_string (Gwdb.get_notes p);
        add_string (Gwdb.get_occupation p);
        add_string (Gwdb.get_birth_note p);
        add_string (Gwdb.get_birth_src p);
        add_string (Gwdb.get_baptism_note p);
        add_string (Gwdb.get_baptism_src p);
        add_string (Gwdb.get_death_note p);
        add_string (Gwdb.get_death_src p);
        add_string (Gwdb.get_burial_note p);
        add_string (Gwdb.get_burial_src p);
        add_string (Gwdb.get_psources p);
        List.iter
          (fun pe ->
            add_string (Gwdb.get_pevent_note pe);
            add_string (Gwdb.get_pevent_src pe))
          (Gwdb.get_pevents p);
        if Buffer.length buffer > 0 then
          match notes_links (Buffer.contents buffer) with
          | [], [] -> ()
          | list ->
              db :=
                Geneweb.NotesLinks.add_in_db !db
                  (Def.NLDB.PgInd (Gwdb.get_iper p))
                  list)
      (Gwdb.persons base);
    if verbosity >= 2 then (
      ProgrBar.finish ();
      Printf.eprintf "--- families notes\n";
      flush stderr;
      ProgrBar.start ());
    Gwdb.Collection.iteri
      (fun i fam ->
        if verbosity >= 2 then ProgrBar.run i nb_fam;
        Buffer.reset buffer;
        add_string (Gwdb.get_comment fam);
        add_string (Gwdb.get_fsources fam);
        add_string (Gwdb.get_marriage_note fam);
        add_string (Gwdb.get_marriage_src fam);
        List.iter
          (fun fe ->
            add_string (Gwdb.get_fevent_note fe);
            add_string (Gwdb.get_fevent_src fe))
          (Gwdb.get_fevents fam);
        if Buffer.length buffer > 0 then
          match notes_links (Buffer.contents buffer) with
          | [], [] -> ()
          | list ->
              db :=
                Geneweb.NotesLinks.add_in_db !db
                  (Def.NLDB.PgFam (Gwdb.get_ifam fam))
                  list)
      (Gwdb.families base);
    if verbosity >= 2 then ProgrBar.finish ();
    Gwdb.write_nldb base !db
end

let main () =
  let bname = ref "" in
  let fast = ref false in
  let cutoff_year = ref (None : int option) in
  let whitelist_file = ref "" in
  let blacklist_file = ref "" in
  let no_consang = ref false in
  let verbosity = ref 2 in
  let speclist =
    [
      ( "-y",
        Arg.Int (fun y -> cutoff_year := Some y),
        "<year> Cutoff year for public/private date estimation." );
      ( "--whitelist",
        Arg.String (fun s -> whitelist_file := s),
        "<file> File containing whitelist person keys." );
      ( "--blacklist",
        Arg.String (fun s -> blacklist_file := s),
        "<file> File containing blacklist person keys." );
      ("--no-consang", Arg.Set no_consang, " Skip consanguinity calculation.");
      ("-fast", Arg.Set fast, " Fast mode (load arrays in memory).");
      ("-q", Arg.Unit (fun () -> verbosity := 1), " Quiet mode.");
      ("-qq", Arg.Unit (fun () -> verbosity := 0), " Very quiet mode.");
      ("-nolock", Arg.Set Lock.no_lock_flag, " Do not lock database.");
    ]
  in
  let anonfun s =
    if !bname = "" then bname := s
    else raise (Arg.Bad "Cannot process multiple databases.")
  in
  let usage = "Usage: gwresync [options] <base>" in
  Arg.parse (Arg.align speclist) anonfun usage;
  if !bname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "%s\n" (Arg.usage_string speclist usage);
    flush stderr;
    exit 2);
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Files.lock_file !bname) ~onerror:Lock.print_error_and_exit
    (fun () ->
      let base = Gwdb.open_base !bname in
      if !verbosity >= 1 then (
        Printf.printf "Base opened: %s\n" (Gwdb.bname base);
        flush stdout);
      if !fast then (
        Gwdb.load_persons_array base;
        Gwdb.load_families_array base;
        Gwdb.load_ascends_array base;
        Gwdb.load_couples_array base;
        Gwdb.load_unions_array base;
        Gwdb.load_descends_array base;
        Gwdb.load_strings_array base);
      Fun.protect
        ~finally:(fun () ->
          if !fast then (
            Gwdb.clear_strings_array base;
            Gwdb.clear_persons_array base;
            Gwdb.clear_families_array base;
            Gwdb.clear_ascends_array base;
            Gwdb.clear_couples_array base;
            Gwdb.clear_unions_array base;
            Gwdb.clear_descends_array base);
          Gwdb.close_base base)
        (fun () ->
          let changes = ref false in
          Option.iter
            (fun y ->
              if !verbosity >= 1 then (
                Printf.printf "Applying cutoff date (%d)...\n" y;
                flush stdout);
              if
                Privacy.apply_cutoff_date ~base ~cutoff_year:y
                  ~verbosity:!verbosity
              then changes := true)
            !cutoff_year;
          if !whitelist_file <> "" then (
            if !verbosity >= 1 then (
              Printf.printf "Applying whitelist...\n";
              flush stdout);
            if
              Privacy.apply_list ~base ~access:Def.Public ~file:!whitelist_file
                ~verbosity:!verbosity
            then changes := true);
          if !blacklist_file <> "" then (
            if !verbosity >= 1 then (
              Printf.printf "Applying blacklist...\n";
              flush stdout);
            if
              Privacy.apply_list ~base ~access:Def.Private ~file:!blacklist_file
                ~verbosity:!verbosity
            then changes := true);
          if !verbosity >= 1 then (
            Printf.printf "Checking integrity and repairing...\n";
            flush stdout);
          let fix_count = Integrity.perform_fixes ~base ~verbosity:!verbosity in
          if fix_count > 0 then changes := true;
          let has_loop = CycleCheck.detect_loop ~base in
          if has_loop then (
            if !verbosity >= 1 then (
              Printf.printf
                "Loop detected, consanguinity calculation will be skipped.\n";
              flush stdout))
          else if not !no_consang then (
            if !verbosity >= 1 then (
              Printf.printf "Computing consanguinity...\n";
              flush stdout);
            let consang_patched =
              Consanguinity.compute ~base ~scratch:false ~verbosity:!verbosity
            in
            if consang_patched then changes := true);
          if !verbosity >= 1 then (
            Printf.printf "Indexing notes...\n";
            flush stdout);
          NotesIndexing.compute ~base ~bdir:!bname ~verbosity:!verbosity;
          if !changes then (
            if !verbosity >= 1 then (
              Printf.printf "Committing patches in memory...\n";
              flush stdout);
            Gwdb.commit_patches ~write_to_disk:false base);
          if !verbosity >= 1 then (
            Printf.printf "Rebuilding indexes and caches...\n";
            flush stdout);
          Gwdb.sync ~save_mem:false
            ~tasks:
              [
                (fun () -> Geneweb.Caches.write_caches base);
                (fun () ->
                  let conf =
                    Geneweb.Util.minimal_wiz_conf ~bname:(Gwdb.bname base)
                  in
                  Geneweb.Sosa_cache.write_static_sosa_cache ~conf ~base);
                (fun () ->
                  Geneweb.Util.rewrite_visited
                    (Geneweb.Util.minimal_wiz_conf ~bname:(Gwdb.bname base)));
              ]
            base;
          if !verbosity >= 1 then (
            Printf.printf "gwresync completed successfully.\n";
            flush stdout)))
