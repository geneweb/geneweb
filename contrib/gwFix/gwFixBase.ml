(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Def
open Gwdb
open Printf


let designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)

let check_keys base nb_ind fix =
  printf "Check keys\n";
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
              printf "*** key %s.%d %s is \"%s\"\n" fn occ sn
                (designation base ip (poi base ip2));
              flush stdout;
              Gwdb.patch_key base ip fn sn occ;
              printf "*** fixed\n";
              flush stdout;
              fix := true;
              ProgrBar.restart i nb_ind
            end
      | None ->
          ProgrBar.suspend ();
          printf "*** key %s.%d %s = no anwser\n" fn occ sn;
          flush stdout;
          Gwdb.patch_key base ip fn sn occ;
          printf "*** fixed\n";
          flush stdout;
          fix := true;
          ProgrBar.restart i nb_ind
  done;
  ProgrBar.finish ()

let check_families_parents base nb_fam =
  printf "Check families' parents\n";
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
            printf "*** no family for : %s\n"
              (designation base ip (poi base ip));
            flush stdout;
            ProgrBar.restart i nb_fam
          end
      done
  done;
  ProgrBar.finish ()

let check_families_children base nb_fam fix =
  printf "Check families' children\n";
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
                printf "*** bad parents : %s\n"
                  (designation base ip (poi base ip));
                flush stdout
              end
        | None ->
            ProgrBar.suspend ();
            printf "*** no parents : %s in family\n    %s & %s\n"
              (designation base ip (poi base ip))
              (let ip = get_father fam in designation base ip (poi base ip))
              (let ip = get_mother fam in designation base ip (poi base ip));
            flush stdout;
            patch_ascend base ip
              {parents = Some ifam; consang = get_consang a};
            fix := true;
            ProgrBar.restart i nb_fam
      done
  done;
  ProgrBar.finish ()

let check_persons_parents base nb_ind fix =
  printf "Check persons' parents\n";
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
            printf "*** parent family deleted: %s\n"
              (designation base ip (poi base ip));
            flush stdout;
            patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
            fix := true
          end
        else
          let children = get_children fam in
          if Array.mem ip children then ()
          else
            begin
              printf "*** not in parent's family: %s\n"
                (designation base ip (poi base ip));
              flush stdout;
              let children = Array.append children [| ip |] in
              patch_descend base ifam {children = children}; fix := true
            end
    | None -> ()
  done;
  ProgrBar.finish ()

let check_persons_families base nb_ind fix =
  printf "Check persons' families\n";
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
          fix := true;
          ProgrBar.restart i nb_ind
        end
    done
  done;
  ProgrBar.finish ()

let check_witnesses base nb_fam fix =
  printf "Check witnesses\n";
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
          printf "*** in marriage: %s & %s\n"
            (designation base ifath (poi base ifath))
            (designation base ifath (poi base imoth));
          printf "*** witness has no pointer to marriage: %s\n"
            (designation base ip p);
          flush stdout;
          patch_person base ip
            {(gen_person_of_person p) with related = ifath :: get_related p};
          printf "*** fixed\n";
          fix := true;
          flush stdout;
          ProgrBar.restart i nb_fam
        end
    done
  done;
  ProgrBar.finish ()

let check_pevents_witnesses base nb_ind fix =
  printf "Check persons' events witnesses\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    List.iter
      (fun evt ->
         let witn = List.map fst (Array.to_list evt.epers_witnesses) in
         let witn = Array.of_list witn in
         for j = 0 to Array.length witn - 1 do
           let ip2 = witn.(j) in
           let p2 = poi base ip2 in
           if not (List.memq ip (get_related p2)) then
             begin
               ProgrBar.suspend ();
               printf "*** in persons' event: %s\n"
                 (designation base ip2 (poi base ip2));
               printf "*** witness has no pointer to persons' event: %s\n"
                 (designation base ip p);
               flush stdout;
               patch_person base ip2
                 {(gen_person_of_person p2) with related =
                   ip :: get_related p2};
               printf "*** fixed\n";
               fix := true;
               flush stdout;
               ProgrBar.restart i nb_ind
             end
         done)
      (get_pevents p)
  done;
  ProgrBar.finish ()

let check_fevents_witnesses base nb_fam fix =
  printf "Check family events witnesses\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let ifath = get_father fam in
    List.iter
      (fun evt ->
         let witn = List.map fst (Array.to_list evt.efam_witnesses) in
         let witn = Array.of_list witn in
         for j = 0 to Array.length witn - 1 do
           let ip = witn.(j) in
           let p = poi base ip in
           if not (List.memq ifath (get_related p)) then
             begin
               ProgrBar.suspend ();
               let imoth = get_mother fam in
               printf "*** in family event: %s & %s\n"
                 (designation base ifath (poi base ifath))
                 (designation base ifath (poi base imoth));
               printf "*** witness has no pointer to family event: %s\n"
                 (designation base ip p);
               flush stdout;
               patch_person base ip
                 {(gen_person_of_person p) with related =
                   ifath :: get_related p};
               printf "*** fixed\n";
               fix := true;
               flush stdout;
               ProgrBar.restart i nb_fam
             end
         done)
      (get_fevents fam)
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
  check_pevents_witnesses base nb_ind fix;
  check_fevents_witnesses base nb_fam fix;
  if !fix then Gwdb.commit_patches base
  else begin printf "No change\n"; flush stdout end


(**/**)

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  match
    Lock.control (Mutil.lock_file !bname) false (fun () -> check !bname)
  with
    Some x -> x
  | None -> eprintf "Cannot lock database. Try again.\n"; flush stderr

let _ = main ()
