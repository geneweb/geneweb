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


let suspend_with msg = ProgrBar.suspend (); msg () ; flush stdout
let restart_with i n = printf "*** fixed\n"; flush stdout; ProgrBar.restart i n

(* TODO: -fast option to load array and unload at the end *)
let check_keys ~verbose ~fast base nb_ind fix =
  printf "Check keys\n";
  flush stdout;
  if fast then begin load_strings_array base ; load_persons_array base end ;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in
    if fn <> "?" && sn <> "?" then
      let patch ?msg base ip fn sn occ =
        Opt.iter suspend_with msg ;
        Gwdb.patch_key base ip fn sn occ;
        incr fix ;
        Opt.iter (fun _ -> restart_with i nb_ind) msg
      in
      match Gwdb.person_of_key base fn sn occ with
      | Some ip2 when ip2 <> ip ->
        let msg =
          if verbose
          then Some (fun () ->
              printf
                "*** key %s.%d %s is \"%s\"\n"
                fn occ sn (designation base ip (poi base ip2)) )
          else None
        in
        patch ?msg base ip fn sn occ
      | None ->
        let msg =
          if verbose
          then Some (fun () -> printf "*** key %s.%d %s = no anwser\n" fn occ sn)
          else None
        in
        patch ?msg base ip fn sn occ
      | _ -> ()
  done;
  if fast then begin clear_strings_array base ; clear_persons_array base end ;
  ProgrBar.finish ()

let check_families_parents base nb_fam =
  printf "Check families' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if not @@ is_deleted_family fam then
      let a = get_parent_array fam in
      for j = 0 to Array.length a - 1 do
        let ip = a.(j) in
        if not @@ Array.mem ifam (get_family (poi base ip)) then
          begin
            suspend_with (fun () ->
                printf
                  "*** no family for : %s\n"
                  (designation base ip (poi base ip)) ) ;
            restart_with i nb_fam
          end
      done
  done;
  ProgrBar.finish ()

let check_families_children verbose base nb_fam fix =
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
        | Some ifam1 when ifam1 != ifam ->
          printf "*** bad parents : %s\n"
            (designation base ip (poi base ip));
          flush stdout
        | None ->
          if verbose then begin
            ProgrBar.suspend ();
            printf "*** no parents : %s in family\n    %s & %s\n"
              (designation base ip (poi base ip))
              (let ip = get_father fam in designation base ip (poi base ip))
              (let ip = get_mother fam in designation base ip (poi base ip));
            flush stdout
          end ;
          patch_ascend base ip
            {parents = Some ifam; consang = get_consang a};
          incr fix ;
          if verbose then ProgrBar.restart i nb_fam ;
        | _ -> ()
      done
  done;
  ProgrBar.finish ()

let check_persons_parents verbose base nb_ind fix =
  printf "Check persons' parents\n";
  flush stdout;
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let ip = Adef.iper_of_int i in
    let a = poi base ip in
    get_parents a |> Opt.iter @@ fun ifam ->
    let fam = foi base ifam in
    if is_deleted_family fam then begin
      if verbose then begin
        printf
          "*** parent family deleted: %s\n"
          (designation base ip (poi base ip)) ;
        flush stdout
      end ;
      patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
      incr fix
    end
    else
      let children = get_children fam in
      if not @@ Array.mem ip children then
        begin
          if verbose then begin
            printf "*** not in parent's family: %s\n"
              (designation base ip (poi base ip));
            flush stdout
          end ;
          let children = Array.append children [| ip |] in
          patch_descend base ifam {children = children}; incr fix
        end
  done;
  ProgrBar.finish ()

let check_persons_families verbose base nb_ind fix =
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
      if not @@ Array.mem ip (get_parent_array cpl) then
        begin
          if verbose then
            suspend_with (fun () ->
                printf "*** not father or mother of hir family: %s\n"
              (designation base ip (poi base ip)) );
          let ifams =
            Array.append (Array.sub ifams 0 j)
              (Array.sub ifams (j + 1) (Array.length ifams - j - 1))
          in
          patch_union base ip {family = ifams};
          incr fix;
          if verbose then restart_with i nb_ind ;
        end
    done
  done;
  ProgrBar.finish ()

let check_witnesses verbose base nb_fam fix =
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
          if verbose then
            suspend_with (fun () ->
                let imoth = get_mother fam in
                printf "*** in marriage: %s & %s\n"
                  (designation base ifath (poi base ifath))
                  (designation base ifath (poi base imoth));
                printf "*** witness has no pointer to marriage: %s\n"
                  (designation base ip p) ) ;
          patch_person base ip
            {(gen_person_of_person p) with related = ifath :: get_related p};
          incr fix;
          if verbose then restart_with i nb_fam
        end
    done
  done;
  ProgrBar.finish ()

let check_pevents_witnesses verbose base nb_ind fix =
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
               if verbose then suspend_with (fun () ->
                   printf "*** in persons' event: %s\n"
                     (designation base ip2 (poi base ip2));
                   printf "*** witness has no pointer to persons' event: %s\n"
                     (designation base ip p) ) ;
               patch_person base ip2
                 {(gen_person_of_person p2)
                  with related = ip :: get_related p2};
               incr fix;
               if verbose then restart_with i nb_ind
             end
         done)
      (get_pevents p)
  done;
  ProgrBar.finish ()

let check_fevents_witnesses verbose base nb_fam fix =
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
               if verbose then
                 suspend_with (fun () ->
                     let imoth = get_mother fam in
                     printf "*** in family event: %s & %s\n"
                       (designation base ifath (poi base ifath))
                       (designation base ifath (poi base imoth));
                     printf "*** witness has no pointer to family event: %s\n"
                       (designation base ip p) ) ;
               patch_person base ip
                 {(gen_person_of_person p)
                  with related = ifath :: get_related p};
               incr fix;
               if verbose then restart_with i nb_fam
             end
         done)
      (get_fevents fam)
  done;
  ProgrBar.finish ()

let check
    ~verbose
    ~fast
    ~keys
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    bname =
  let verbose = !verbose in
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let fix = ref 0 in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  if !keys then check_keys ~verbose ~fast base nb_ind fix;
  if !f_parents then check_families_parents base nb_fam;
  if !f_children then check_families_children verbose base nb_fam fix;
  if !p_parents then check_persons_parents verbose base nb_ind fix;
  if !p_families then check_persons_families verbose base nb_ind fix;
  if !witnesses then check_witnesses verbose base nb_fam fix;
  if !pevents_witnesses then check_pevents_witnesses verbose base nb_ind fix;
  if !fevents_witnesses then check_fevents_witnesses verbose base nb_fam fix;
  if !fix <> 0 then begin
    Gwdb.commit_patches base ;
    printf "%n changes commited\n" !fix ;
    flush stdout
  end
  else begin
    printf "No change\n" ;
    flush stdout
  end

(**/**)

let bname = ref ""
let verbose = ref true
let fast = ref false
let keys = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let witnesses = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false

let speclist =
  [ ("-q", Arg.Clear verbose, " quiet mode")
  ; ("-fast", Arg.Set fast, " fast mode. Needs more memory.")
  ; ("-keys", Arg.Set keys, " missing doc")
  ; ("-families-parents", Arg.Set f_parents, " missing doc")
  ; ("-families-children", Arg.Set f_children, " missing doc")
  ; ("-persons-parents", Arg.Set p_parents, " missing doc")
  ; ("-persons-families", Arg.Set p_families, " missing doc")
  ; ("-witnesses", Arg.Set witnesses, " missing doc")
  ; ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc")
  ; ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc")
  ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again @@
  fun () ->
  if !keys
  || !f_parents
  || !f_children
  || !p_parents
  || !p_families
  || !witnesses
  || !pevents_witnesses
  || !fevents_witnesses
  then ()
  else begin
    keys := true ;
    f_parents := true ;
    f_children := true ;
    p_parents := true ;
    p_families := true ;
    witnesses := true ;
    pevents_witnesses := true ;
    fevents_witnesses := true
  end ;
  check
    ~fast
    ~verbose
    ~keys
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    !bname

let _ = main ()
