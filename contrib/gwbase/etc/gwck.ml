(* camlp5r *)
(* $Id: gwck.ml,v 4.13 2009-03-11 10:58:51 deraugla Exp $ *)

#load "pa_lock.cmo";

open Def;
open Gwdb;
open Printf;

value designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)
;

value check_keys base nb_ind fix = do {
  printf "Check keys\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do {
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in
    if fn <> "?" && sn <> "?" then
      match Gwdb.person_of_key base fn sn occ with
      [ Some ip2 ->
          if ip2 <> ip then do {
            ProgrBar.suspend ();
            printf "*** key %s.%d %s is \"%s\"\n" fn occ sn
              (designation base ip (poi base ip2));
            flush stdout;
            Gwdb.patch_key base ip fn sn occ;
            printf "*** fixed\n";
            flush stdout;
            fix.val := True;
            ProgrBar.restart i nb_ind;
          }
          else ()
      | None -> do {
          ProgrBar.suspend ();
          printf "*** key %s.%d %s = no anwser\n" fn occ sn;
          flush stdout;
          Gwdb.patch_key base ip fn sn occ;
          printf "*** fixed\n";
          flush stdout;
          fix.val := True;
          ProgrBar.restart i nb_ind;
        } ]
    else ();
  };
  ProgrBar.finish ();
};

value check_families_parents base nb_fam = do {
  printf "Check families' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do {
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else
      let a = get_parent_array fam in
      for j = 0 to Array.length a - 1 do {
        let ip = a.(j) in
        if Mutil.array_mem ifam (get_family (poi base ip)) then ()
        else do {
           ProgrBar.suspend ();
           printf "*** no family for : %s\n"
             (designation base ip (poi base ip));
           flush stdout;
           ProgrBar.restart i nb_fam;
        };
      }
  };
  ProgrBar.finish ();
};

value check_families_children base nb_fam fix = do {
  printf "Check families' children\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do {
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else
      let children = get_children fam in
      for j = 0 to Array.length children - 1 do {
        let ip = children.(j) in
        let a = poi base ip in
        match get_parents a with
        [ Some ifam1 ->
            if ifam1 != ifam then do {
              printf "*** bad parents : %s\n"
                (designation base ip (poi base ip));
              flush stdout;
            }
            else ()
        | None -> do {
            ProgrBar.suspend ();
            printf "*** no parents : %s in family\n    %s & %s\n"
              (designation base ip (poi base ip))
              (let ip = get_father fam in designation base ip (poi base ip))
              (let ip = get_mother fam in designation base ip (poi base ip));
            flush stdout;
            patch_ascend base ip
              {parents = Some ifam; consang = get_consang a};
            fix.val := True;
            ProgrBar.restart i nb_fam;
          } ];
      };
  };
  ProgrBar.finish ();
};

value check_persons_parents base nb_ind fix = do {
  printf "Check persons' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do {
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let a = poi base ip in
    match get_parents a with
    [ Some ifam ->
        let fam = foi base ifam in
        if is_deleted_family fam then do {
          printf "*** parent family deleted: %s\n"
            (designation base ip (poi base ip));
          flush stdout;
          patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
          fix.val := True;
        }
        else
          let children = get_children fam in
          if Mutil.array_mem ip children then ()
          else do {
            printf "*** not in parent's family: %s\n"
              (designation base ip (poi base ip));
            flush stdout;
            let children = Array.append children [| ip |] in
            patch_descend base ifam {children = children};
            fix.val := True;
          }
    | None -> () ];
  };
  ProgrBar.finish ();
};

value check_persons_families base nb_ind fix = do {
  printf "Check persons' families\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do {
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let u = poi base ip in
    let ifams = get_family u in
    for j = 0 to Array.length ifams - 1 do {
      let ifam = ifams.(j) in
      let cpl = foi base ifam in
      if Mutil.array_mem ip (get_parent_array cpl) then ()
      else do {
        ProgrBar.suspend ();
        printf "*** not father or mother of hir family: %s\n"
          (designation base ip (poi base ip));
        flush stdout;
        let ifams =
          Array.append (Array.sub ifams 0 j)
            (Array.sub ifams (j + 1) (Array.length ifams - j - 1))
        in
        patch_union base ip {family = ifams};
        printf "*** fixed\n";
        flush stdout;
        fix.val := True;
        ProgrBar.restart i nb_ind;
      }
    };
  };
  ProgrBar.finish ();
};

value check_witnesses base nb_fam fix = do {
  printf "Check witnesses\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do {
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let witn = get_witnesses fam in
    let ifath = get_father fam in
    for j = 0 to Array.length witn - 1 do {
      let ip = witn.(j) in
      let p = poi base ip in
      if not (List.memq ifath (get_related p)) then do {
        ProgrBar.suspend ();
        let imoth = get_mother fam in
        printf "*** in marriage: %s & %s\n"
          (designation base ifath (poi base ifath))
          (designation base ifath (poi base imoth));
        printf "*** witness has no pointer to marriage: %s\n"
          (designation base ip p);
        flush stdout;
        patch_person base ip
          {(gen_person_of_person p) with related = [ifath :: get_related p]};
        printf "*** fixed\n";
        fix.val := True;
        flush stdout;
        ProgrBar.restart i nb_fam;
      }
      else ();
    };
  };
  ProgrBar.finish ();
};

value check bname = do {
  let base = Gwdb.open_base bname in
  let fix = ref False in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  check_keys base nb_ind fix;
  check_families_parents base nb_fam;
  check_families_children base nb_fam fix;
  check_persons_parents base nb_ind fix;
  check_persons_families base nb_ind fix;
  check_witnesses base nb_fam fix;
  if fix.val then Gwdb.commit_patches base
  else do {
    printf "No change\n";
    flush stdout;
  }
};

value main () =
  let bname = Sys.argv.(1) in
  lock Mutil.lock_file bname with
  [ Accept -> check bname
  | Refuse -> do {
      eprintf "Cannot lock database. Try again.\n";
      flush stderr;
    } ]
;

main ();
