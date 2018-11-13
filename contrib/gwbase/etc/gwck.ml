open Geneweb
open Def
open Gwdb

let check_keys base nb_ind fix =
  Printf.printf "Check keys\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in
    if fn <> "?" && sn <> "?" then
      match Gwdb.person_of_key base fn sn occ with
        Some ip2 ->
          if ip2 <> ip then
            begin
              ProgrBar.suspend ();
              Printf.printf "*** key %s.%d %s is \"%s\"\n" fn occ sn
                (Gutil.designation base (poi base ip2));
              flush stdout;
              Gwdb.patch_key base ip fn sn occ;
              Printf.printf "*** fixed\n";
              flush stdout;
              fix := true;
              ProgrBar.restart i nb_ind
            end
      | None ->
          ProgrBar.suspend ();
          Printf.printf "*** key %s.%d %s = no anwser\n" fn occ sn;
          flush stdout;
          Gwdb.patch_key base ip fn sn occ;
          Printf.printf "*** fixed\n";
          flush stdout;
          fix := true;
          ProgrBar.restart i nb_ind
  done;
  ProgrBar.finish ()

let check_families_parents base nb_fam =
  Printf.printf "Check families' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else
      let a = get_parent_array fam in
      for j = 0 to Array.length a - 1 do
        let ip = a.(j) in
        if Array.mem ifam (get_family (poi base ip)) then ()
        else
          begin
            ProgrBar.suspend ();
            Printf.printf "*** no family for : %s\n"
              (Gutil.designation base (poi base ip));
            flush stdout;
            ProgrBar.restart i nb_fam
          end
      done
  done;
  ProgrBar.finish ()

let check_families_children base nb_fam fix =
  Printf.printf "Check families' children\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else
      let children = get_children fam in
      for j = 0 to Array.length children - 1 do
        let ip = children.(j) in
        let a = poi base ip in
        match get_parents a with
          Some ifam1 ->
            if ifam1 != ifam then
              begin
                Printf.printf "*** bad parents : %s\n"
                  (Gutil.designation base (poi base ip));
                flush stdout
              end
        | None ->
            ProgrBar.suspend ();
            Printf.printf "*** no parents : %s in family\n    %s & %s\n"
              (Gutil.designation base (poi base ip))
              (let ip = get_father fam in Gutil.designation base (poi base ip))
              (let ip = get_mother fam in Gutil.designation base (poi base ip));
            flush stdout;
            patch_ascend base ip
              {parents = Some ifam; consang = get_consang a};
            fix := true;
            ProgrBar.restart i nb_fam
      done
  done;
  ProgrBar.finish ()

let check_persons_parents base nb_ind fix =
  Printf.printf "Check persons' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let a = poi base ip in
    match get_parents a with
      Some ifam ->
        let fam = foi base ifam in
        if is_deleted_family fam then
          begin
            Printf.printf "*** parent family deleted: %s\n"
              (Gutil.designation base (poi base ip));
            flush stdout;
            patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
            fix := true
          end
        else
          let children = get_children fam in
          if Array.mem ip children then ()
          else
            begin
              Printf.printf "*** not in parent's family: %s\n"
                (Gutil.designation base (poi base ip));
              flush stdout;
              let children = Array.append children [| ip |] in
              patch_descend base ifam {children = children}; fix := true
            end
    | None -> ()
  done;
  ProgrBar.finish ()

let check_persons_families base nb_ind fix =
  Printf.printf "Check persons' families\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let u = poi base ip in
    let ifams = get_family u in
    for j = 0 to Array.length ifams - 1 do
      let ifam = ifams.(j) in
      let cpl = foi base ifam in
      if Array.mem ip (get_parent_array cpl) then ()
      else
        begin
          ProgrBar.suspend ();
          Printf.printf "*** not father or mother of hir family: %s\n"
            (Gutil.designation base (poi base ip));
          flush stdout;
          let ifams =
            Array.append (Array.sub ifams 0 j)
              (Array.sub ifams (j + 1) (Array.length ifams - j - 1))
          in
          patch_union base ip {family = ifams};
          Printf.printf "*** fixed\n";
          flush stdout;
          fix := true;
          ProgrBar.restart i nb_ind
        end
    done
  done;
  ProgrBar.finish ()

let check_witnesses base nb_fam fix =
  Printf.printf "Check witnesses\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let witn = get_witnesses fam in
    let ifath = get_father fam in
    for j = 0 to Array.length witn - 1 do
      let ip = witn.(j) in
      let p = poi base ip in
      if not (List.memq ifath (get_related p)) then
        begin
          ProgrBar.suspend ();
          let imoth = get_mother fam in
          Printf.printf "*** in marriage: %s & %s\n"
            (Gutil.designation base (poi base ifath))
            (Gutil.designation base (poi base imoth));
          Printf.printf "*** witness has no pointer to marriage: %s\n"
            (Gutil.designation base p);
          flush stdout;
          patch_person base ip
            {(gen_person_of_person p) with related = ifath :: get_related p};
          Printf.printf "*** fixed\n";
          fix := true;
          flush stdout;
          ProgrBar.restart i nb_fam
        end
    done
  done;
  ProgrBar.finish ()

let check bname =
  let base = Gwdb.open_base bname in
  let fix = ref false in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  check_keys base nb_ind fix;
  check_families_parents base nb_fam;
  check_families_children base nb_fam fix;
  check_persons_parents base nb_ind fix;
  check_persons_families base nb_ind fix;
  check_witnesses base nb_fam fix;
  if !fix then Gwdb.commit_patches base
  else begin Printf.printf "No change\n"; flush stdout end

let main () =
  let bname = Sys.argv.(1) in
  Lock.control (Mutil.lock_file bname) false (fun () -> check bname)
    ~onerror:Lock.print_try_again

let _ = main ()
