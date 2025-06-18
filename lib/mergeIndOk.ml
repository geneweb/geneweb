(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let rec merge_lists l1 = function
  | x2 :: l2 ->
      if List.mem x2 l1 then merge_lists l1 l2 else merge_lists (l1 @ [ x2 ]) l2
  | [] -> l1

let merge_strings base is1 sep is2 =
  let n1 = Driver.sou base is1 in
  let n2 = Driver.sou base is2 in
  if n1 = n2 then n1
  else if n1 = "" then n2
  else if n2 = "" then n1
  else n1 ^ sep ^ n2

let sorp base ip =
  let p = Driver.poi base ip in
  ( Driver.sou base (Driver.get_first_name p),
    Driver.sou base (Driver.get_surname p),
    Driver.get_occ p,
    Update.Link,
    "" )

let merge_event_witnesses wit1 wit2 =
  let list =
    Array.fold_right
      (fun wit list -> if List.mem wit list then list else wit :: list)
      wit1 (Array.to_list wit2)
  in
  Array.of_list list

let merge_events l1 l2 p =
  let list_mem e l =
    let found_birth = ref false in
    let found_baptism = ref false in
    let found_death = ref false in
    let found_burial = ref false in
    match e.epers_name with
    | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial | Epers_Cremation
      ->
        List.fold_right
          (fun e1 (mem, l1) ->
            match e1.epers_name with
            | Epers_Birth ->
                if !found_birth then (mem, e1 :: l1)
                else if e.epers_name = e1.epers_name then
                  let witnesses =
                    merge_event_witnesses e1.epers_witnesses e.epers_witnesses
                  in
                  let e1 =
                    {
                      e1 with
                      epers_date = p.birth;
                      epers_place = p.birth_place;
                      epers_note = p.birth_note;
                      epers_src = p.birth_src;
                      epers_witnesses = witnesses;
                    }
                  in
                  let _ = found_birth := true in
                  (true, e1 :: l1)
                else (mem, e1 :: l1)
            | Epers_Baptism ->
                if !found_baptism then (mem, e1 :: l1)
                else if e.epers_name = e1.epers_name then
                  let witnesses =
                    merge_event_witnesses e1.epers_witnesses e.epers_witnesses
                  in
                  let e1 =
                    {
                      e1 with
                      epers_date = p.baptism;
                      epers_place = p.baptism_place;
                      epers_note = p.baptism_note;
                      epers_src = p.baptism_src;
                      epers_witnesses = witnesses;
                    }
                  in
                  let _ = found_baptism := true in
                  (true, e1 :: l1)
                else (mem, e1 :: l1)
            | Epers_Death ->
                if !found_death then (mem, e1 :: l1)
                else if e.epers_name = e1.epers_name then
                  let is_dead, date =
                    match p.death with
                    | NotDead | DontKnowIfDead -> (false, Date.cdate_None)
                    | Death (_, cd) -> (true, cd)
                    | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
                        (true, Date.cdate_None)
                  in
                  let witnesses =
                    merge_event_witnesses e1.epers_witnesses e.epers_witnesses
                  in
                  let e1 =
                    {
                      e1 with
                      epers_date = date;
                      epers_place = p.death_place;
                      epers_note = p.death_note;
                      epers_src = p.death_src;
                      epers_witnesses = witnesses;
                    }
                  in
                  let _ = found_death := true in
                  if is_dead then (true, e1 :: l1) else (true, l1)
                else (mem, e1 :: l1)
            | Epers_Burial ->
                if !found_burial then (mem, e1 :: l1)
                else if e.epers_name = e1.epers_name then
                  match p.burial with
                  | UnknownBurial ->
                      let _ = found_burial := true in
                      (true, l1)
                  | Buried cd ->
                      let witnesses =
                        merge_event_witnesses e1.epers_witnesses
                          e.epers_witnesses
                      in
                      let e1 =
                        {
                          e1 with
                          epers_date = cd;
                          epers_place = p.burial_place;
                          epers_note = p.burial_note;
                          epers_src = p.burial_src;
                          epers_witnesses = witnesses;
                        }
                      in
                      let _ = found_burial := true in
                      (true, e1 :: l1)
                  | _ ->
                      let _ = found_burial := true in
                      (mem, e1 :: l1)
                else (mem, e1 :: l1)
            | Epers_Cremation ->
                if !found_burial then (mem, e1 :: l1)
                else if e.epers_name = e1.epers_name then
                  match p.burial with
                  | UnknownBurial ->
                      let _ = found_burial := true in
                      (true, l1)
                  | Cremated cd ->
                      let witnesses =
                        merge_event_witnesses e1.epers_witnesses
                          e.epers_witnesses
                      in
                      let e1 =
                        {
                          e1 with
                          epers_date = cd;
                          epers_place = p.burial_place;
                          epers_note = p.burial_note;
                          epers_src = p.burial_src;
                          epers_witnesses = witnesses;
                        }
                      in
                      let _ = found_burial := true in
                      (true, e1 :: l1)
                  | _ ->
                      let _ = found_burial := true in
                      (mem, e1 :: l1)
                else (mem, e1 :: l1)
            | _ -> (mem, e1 :: l1))
          l (false, [])
    | _ -> (false, l)
  in
  let rec merge_events_aux l1 l2 =
    match l2 with
    | [] -> l1
    | e2 :: l2 ->
        let mem, l1 = list_mem e2 l1 in
        if mem then merge_events_aux l1 l2
        else merge_events_aux (l1 @ [ e2 ]) l2
  in
  merge_events_aux l1 l2

let reconstitute conf base p1 p2 =
  let field name proj null =
    let x1 = proj p1 in
    let x2 = proj p2 in
    match p_getenv conf.env name with
    | Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1
  in
  let merge_field name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    match p_getenv conf.env name with
    | Some "1" -> x1
    | Some "2" -> x2
    | _ -> merge_strings base x1 "<br>\n" x2 |> Driver.insert_string base
  in
  let list conv proj =
    let l1 = List.map conv (proj p1) in
    let l2 = List.map conv (proj p2) in
    merge_lists l1 l2
  in
  let merge_primary_events conv proj p =
    let l1 = List.map conv (proj p1) in
    let l2 = List.map conv (proj p2) in
    merge_events l1 l2 p
  in
  let p =
    {
      first_name =
        field "first_name"
          (fun p -> Driver.p_first_name base p)
          (fun x -> x = "" || x = "?");
      surname =
        field "surname"
          (fun p -> Driver.p_surname base p)
          (fun x -> x = "" || x = "?");
      occ = field "number" Driver.get_occ (( = ) 0);
      image =
        field "image"
          (fun p ->
            match Image.get_portrait conf base p with
            | Some src -> Image.src_to_string src
            | None -> "")
          (( = ) "");
      public_name =
        field "public_name"
          (fun p -> Driver.sou base (Driver.get_public_name p))
          (( = ) "");
      qualifiers = list (Driver.sou base) Driver.get_qualifiers;
      aliases = list (Driver.sou base) Driver.get_aliases;
      first_names_aliases =
        list (Driver.sou base) Driver.get_first_names_aliases;
      surnames_aliases = list (Driver.sou base) Driver.get_surnames_aliases;
      titles =
        list (Futil.map_title_strings (Driver.sou base)) Driver.get_titles;
      rparents =
        list
          (Futil.map_relation_ps (sorp base) (Driver.sou base))
          Driver.get_rparents;
      related = [];
      occupation =
        field "occupation"
          (fun p -> Driver.sou base (Driver.get_occupation p))
          (( = ) "");
      sex = field "sex" Driver.get_sex (( = ) Neuter);
      access = field "access" Driver.get_access (( = ) IfTitles);
      birth = field "birth" Driver.get_birth (( = ) Date.cdate_None);
      birth_place =
        field "birth_place"
          (fun p -> Driver.sou base (Driver.get_birth_place p))
          (( = ) "");
      birth_note =
        merge_field "birth_note" (fun p -> Driver.get_birth_note p)
        |> Driver.sou base;
      birth_src =
        merge_field "birth_source" (fun p -> Driver.get_birth_src p)
        |> Driver.sou base;
      baptism = field "baptism" Driver.get_baptism (( = ) Date.cdate_None);
      baptism_place =
        field "baptism_place"
          (fun p -> Driver.sou base (Driver.get_baptism_place p))
          (( = ) "");
      baptism_note =
        merge_field "baptism_note" (fun p -> Driver.get_baptism_note p)
        |> Driver.sou base;
      baptism_src =
        merge_field "baptism_source" (fun p -> Driver.get_baptism_src p)
        |> Driver.sou base;
      death =
        field "death" Driver.get_death (fun x ->
            match x with DontKnowIfDead | OfCourseDead -> true | _ -> false);
      death_place =
        field "death_place"
          (fun p -> Driver.sou base (Driver.get_death_place p))
          (( = ) "");
      death_note =
        merge_field "death_note" (fun p -> Driver.get_death_note p)
        |> Driver.sou base;
      death_src =
        merge_field "death_src" (fun p -> Driver.get_death_src p)
        |> Driver.sou base;
      burial = field "burial" Driver.get_burial (( = ) UnknownBurial);
      burial_place =
        field "burial_place"
          (fun p -> Driver.sou base (Driver.get_burial_place p))
          (( = ) "");
      burial_note =
        merge_field "burial_note" (fun p -> Driver.get_burial_note p)
        |> Driver.sou base;
      burial_src =
        merge_field "burial_source" (fun p -> Driver.get_burial_src p)
        |> Driver.sou base;
      pevents =
        list
          (Futil.map_pers_event (sorp base) (Driver.sou base))
          Driver.get_pevents;
      notes =
        merge_field "notes" (fun p -> Driver.get_notes p) |> Driver.sou base;
      psources =
        merge_field "sources" (fun p -> Driver.get_psources p)
        |> Driver.sou base;
      key_index = Driver.get_iper p1;
    }
  in
  (* On fait la fusion des évènements à partir *)
  (* de la fusion des évènements principaux.   *)
  let pevents =
    merge_primary_events
      (Futil.map_pers_event (sorp base) (Driver.sou base))
      Driver.get_pevents p
  in
  { p with pevents }

let redirect_relations_of_added_related base p ip2 rel_chil =
  let p_related, mod_p =
    List.fold_right
      (fun ipc (p_related, mod_p) ->
        let pc = Driver.poi base ipc in
        let pc_rparents, _, p_related, mod_p =
          List.fold_right
            (fun r (pc_rparents, mod_pc, p_related, mod_p) ->
              let r, mod_pc, p_related, mod_p =
                match r.r_fath with
                | Some ip when ip = ip2 ->
                    let p_related, mod_p =
                      if List.mem ipc p_related then (p_related, mod_p)
                      else (ipc :: p_related, true)
                    in
                    let r = { r with r_fath = Some p.key_index } in
                    (r, true, p_related, mod_p)
                | _ -> (r, mod_pc, p_related, mod_p)
              in
              let r, mod_pc, p_related, mod_p =
                match r.r_moth with
                | Some ip when ip = ip2 ->
                    let p_related, mod_p =
                      if List.mem ipc p_related then (p_related, mod_p)
                      else (ipc :: p_related, true)
                    in
                    let r = { r with r_moth = Some p.key_index } in
                    (r, true, p_related, mod_p)
                | _ -> (r, mod_pc, p_related, mod_p)
              in
              (r :: pc_rparents, mod_pc, p_related, mod_p))
            (Driver.get_rparents pc)
            ([], false, p_related, mod_p)
        in
        let pc_pevents, mod_pc, p_related, mod_p =
          List.fold_right
            (fun e (pc_pevents, mod_pc, p_related, _) ->
              let e, mod_pc, p_related, mod_p =
                let witnesses, mod_p, p_related =
                  List.fold_right
                    (fun (ip, k) (witnesses, mod_p, p_related) ->
                      if ip = ip2 then
                        let p_related, mod_p =
                          if List.mem ipc p_related then (p_related, mod_p)
                          else (ipc :: p_related, true)
                        in
                        ((p.key_index, k) :: witnesses, mod_p, p_related)
                      else ((ip, k) :: witnesses, mod_p, p_related))
                    (Array.to_list e.epers_witnesses)
                    ([], mod_pc, p_related)
                in
                let e = { e with epers_witnesses = Array.of_list witnesses } in
                (e, true, p_related, mod_p)
              in
              (e :: pc_pevents, mod_pc, p_related, mod_p))
            (Driver.get_pevents pc)
            ([], false, p_related, mod_p)
        in
        (* TODO mod_pc = True tout le temps *)
        (if mod_pc then
           let pc = Driver.gen_person_of_person pc in
           let pc = { pc with rparents = pc_rparents; pevents = pc_pevents } in
           Driver.patch_person base ipc pc);
        let p_related, mod_p =
          let rec loop (p_related, mod_p) i =
            if i = Array.length (Driver.get_family pc) then (p_related, mod_p)
            else
              let ifam = (Driver.get_family pc).(i) in
              let fam = Driver.gen_family_of_family (Driver.foi base ifam) in
              let p_related, mod_p =
                if Array.mem ip2 fam.witnesses then (
                  let p_related, mod_p =
                    let rec loop (p_related, mod_p) j =
                      if j = Array.length fam.witnesses then (p_related, mod_p)
                      else
                        let p_related, mod_p =
                          if fam.witnesses.(j) = ip2 then (
                            fam.witnesses.(j) <- p.key_index;
                            if List.mem ipc p_related then (p_related, mod_p)
                            else (ipc :: p_related, true))
                          else (p_related, mod_p)
                        in
                        loop (p_related, mod_p) (j + 1)
                    in
                    loop (p_related, mod_p) 0
                  in
                  Driver.patch_family base ifam fam;
                  (p_related, mod_p))
                else (p_related, mod_p)
              in
              let pc_fevents, mod_pc, p_related, mod_p =
                List.fold_right
                  (fun e (pc_fevents, _, p_related, mod_p) ->
                    let e, mod_pc, p_related, mod_p =
                      let p_related, mod_p =
                        let rec loop (p_related, mod_p) j =
                          if j = Array.length e.efam_witnesses then
                            (p_related, mod_p)
                          else
                            let p_related, mod_p =
                              if fst e.efam_witnesses.(j) = ip2 then (
                                let _, wk = e.efam_witnesses.(j) in
                                e.efam_witnesses.(j) <- (p.key_index, wk);
                                if List.mem ipc p_related then (p_related, mod_p)
                                else (ipc :: p_related, true))
                              else (p_related, mod_p)
                            in
                            loop (p_related, mod_p) (j + 1)
                        in
                        loop (p_related, mod_p) 0
                      in
                      (e, true, p_related, mod_p)
                    in
                    (e :: pc_fevents, mod_pc, p_related, mod_p))
                  fam.fevents
                  ([], false, p_related, mod_p)
              in
              let () =
                (* TODO mod_pc = True tout le temps *)
                if mod_pc then
                  let fam = { fam with fevents = pc_fevents } in
                  Driver.patch_family base ifam fam
              in
              loop (p_related, mod_p) (i + 1)
          in
          loop (p_related, mod_p) 0
        in
        (p_related, mod_p))
      rel_chil (p.related, false)
  in
  if mod_p then { p with related = p_related } else p

let redirect_added_families base p ip2 p2_family =
  for i = 0 to Array.length p2_family - 1 do
    let ifam = p2_family.(i) in
    let fam = Driver.foi base ifam in
    let cpl =
      if ip2 = Driver.get_father fam then (
        Array.iter
          (fun ip ->
            let w = Driver.poi base ip in
            if not (List.mem p.key_index (Driver.get_related w)) then
              let w = Driver.gen_person_of_person w in
              let w = { w with related = p.key_index :: w.related } in
              Driver.patch_person base ip w)
          (Driver.get_witnesses fam);
        List.iter
          (fun evt ->
            Array.iter
              (fun (ip, _) ->
                let w = Driver.poi base ip in
                if not (List.mem p.key_index (Driver.get_related w)) then
                  let w = Driver.gen_person_of_person w in
                  let w = { w with related = p.key_index :: w.related } in
                  Driver.patch_person base ip w)
              evt.efam_witnesses)
          (Driver.get_fevents fam);
        Gutil.couple false p.key_index (Driver.get_mother fam))
      else if ip2 = Driver.get_mother fam then
        Gutil.couple false (Driver.get_father fam) p.key_index
      else assert false
    in
    Driver.patch_couple base ifam cpl
  done

let merge_carrousel conf base o_p1 o_p2 p =
  (* move files of dir2 into dir1 *)
  let rec move_files dir1 dir2 =
    Array.iter
      (fun entry ->
        let full_path1 = Filename.concat dir1 entry in
        let full_path2 = Filename.concat dir2 entry in
        if (Unix.stat full_path2).st_kind = Unix.S_REG then
          try
            Sys.rename full_path2
              (full_path1 ^ if full_path1 = full_path2 then "-copy" else "")
          with e ->
            Printf.eprintf "Error rename path2 (merge) %s\n"
              (Printexc.to_string e)
        else if
          (Unix.stat full_path2).st_kind = Unix.S_DIR
          && entry <> "." && entry <> ".."
        then
          if not (Sys.file_exists full_path1) then (
            try Unix.mkdir full_path1 0o755
            with e ->
              Printf.eprintf "Error create save1 (merge) %s\n"
                (Printexc.to_string e);
              move_files full_path1 full_path2)
          else ())
      (Sys.readdir dir2)
  in
  let full_dir file = Filename.concat (!GWPARAM.images_d conf.bname) file in
  let ofn = p.first_name in
  let osn = p.surname in
  let oocc = p.occ in
  let ofn1 = Driver.sou base (Driver.get_first_name o_p1) in
  let osn1 = Driver.sou base (Driver.get_surname o_p1) in
  let oocc1 = Driver.get_occ o_p1 in
  let ofn2 = Driver.sou base (Driver.get_first_name o_p2) in
  let osn2 = Driver.sou base (Driver.get_surname o_p2) in
  let oocc2 = Driver.get_occ o_p2 in
  let dir0 = Printf.sprintf "%s.%d.%s" ofn oocc osn in
  let dir1 = Printf.sprintf "%s.%d.%s" ofn1 oocc1 osn1 in
  let dir2 = Printf.sprintf "%s.%d.%s" ofn2 oocc2 osn2 in
  let dir0 = full_dir dir0 in
  let dir1 = full_dir dir1 in
  let dir2 = full_dir dir2 in
  match (Sys.file_exists dir1, Sys.file_exists dir2) with
  | true, true ->
      if dir0 = dir1 then (
        move_files dir0 dir2;
        try Mutil.rm_rf dir2
        with e ->
          Printf.eprintf "Error delete dir2 (merge) %s\n" (Printexc.to_string e));
      if dir0 = dir2 then (
        move_files dir0 dir1;
        try Mutil.rm_rf dir1
        with e ->
          Printf.eprintf "Error delete dir1 (merge) %s\n" (Printexc.to_string e))
  | true, false -> (
      if dir1 <> dir0 then
        try Sys.rename dir1 dir0
        with e ->
          Printf.eprintf "Error rename dir1 (merge) %s\n" (Printexc.to_string e)
      )
  | false, true -> (
      if dir2 <> dir0 then
        try Sys.rename dir2 dir0
        with e ->
          Printf.eprintf "Error rename dir2 (merge) %s\n" (Printexc.to_string e)
      )
  | false, false -> ()

let effective_mod_merge o_conf base o_p1 o_p2 sp print_mod_merge_ok =
  let conf = Update.update_conf o_conf in
  let p_family = Driver.get_family (Driver.poi base sp.key_index) in
  let p2_family = Driver.get_family (Driver.poi base o_p2.key_index) in
  let db = Driver.read_nldb base in
  let ofn1 = o_p1.first_name in
  let osn1 = o_p1.surname in
  let oocc1 = o_p1.occ in
  let key1 = (Name.lower ofn1, Name.lower osn1, oocc1) in
  let pgl1 = Notes.links_to_ind conf base db key1 None in
  let ofn2 = o_p2.first_name in
  let osn2 = o_p2.surname in
  let oocc2 = o_p2.occ in
  let key2 = (Name.lower ofn2, Name.lower osn2, oocc2) in
  let pgl2 = Notes.links_to_ind conf base db key2 None in
  let warning _ = () in
  MergeInd.reparent_ind base warning sp.key_index o_p2.key_index;
  let p =
    UpdateIndOk.effective_mod ~skip_conflict:o_p2.key_index conf base sp
  in
  let p =
    redirect_relations_of_added_related base p o_p2.key_index o_p2.related
  in
  redirect_added_families base p o_p2.key_index p2_family;
  UpdateIndOk.effective_del_no_commit base o_p2;
  Driver.patch_person base p.key_index p;
  let new_key =
    (Driver.sou base p.first_name, Driver.sou base p.surname, p.occ)
  in
  if
    (not (String.equal ofn1 sp.first_name && String.equal osn1 sp.surname))
    || oocc1 <> sp.occ
  then Notes.update_ind_key conf base pgl1 key1 new_key;
  Notes.update_ind_key conf base pgl2 key2 new_key;
  let u = { family = Array.append p_family p2_family } in
  if p2_family <> [||] then Driver.patch_union base p.key_index u;
  Consang.check_noloop_for_person_list base
    (Update.def_error conf base)
    [ p.key_index ];
  let wl =
    let a = Driver.poi base p.key_index in
    let a =
      { parents = Driver.get_parents a; consang = Driver.get_consang a }
    in
    UpdateIndOk.all_checks_person base p a u
  in
  Util.commit_patches conf base;
  History.record conf base
    (U_Merge_person (o_p1, o_p2, Util.string_gen_person base p))
    "fp";
  Notes.update_notes_links_db base (Def.NLDB.PgInd o_p2.key_index) "";
  (* TODO update_cache_linked_pages *)
  Update.delete_topological_sort conf base;
  print_mod_merge_ok conf base wl p pgl1 ofn1 osn1 oocc1 pgl2 ofn2 osn2 oocc2
