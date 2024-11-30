open Gwdb
open Def

type patch =
  | Fix_WrongUTF8Encoding of
      Gwdb.ifam option * Gwdb.iper option * (Gwdb.istr * Gwdb.istr) option
  | Fix_UpdatedOcc of iper * int * int

let mk_pevent name date place note src =
  {
    epers_name = name;
    epers_date = date;
    epers_place = place;
    epers_reason = empty_string;
    epers_note = note;
    epers_src = src;
    epers_witnesses = [||];
  }

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let invalid_utf8 = ref false
let p_key = ref false
let index = ref false
let dry_run = ref false
let server = ref "localhost"
let gwd_port = ref 2317
let utf8_key = ref false
let portraits_list = ref []
let of_pevent e = (e.epers_date, e.epers_place, e.epers_note, e.epers_src)

let find_pevent names pevents =
  List.find_opt (fun x -> List.mem x.epers_name names) pevents

(* check for malformed Utf8 characters
   Apply corrections
*)

let fix_utf8_sequence ?report progress base =
  let normalize_utf_8_date ifam iper s =
    let s' = Mutil.normalize_utf_8 s in
    (if s <> s' then
     match report with
     | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, None))
     | None -> ());
    s'
  in
  let normalize_utf_8 ifam iper i =
    let s = Gwdb.sou base i in
    let s' = Mutil.normalize_utf_8 s in
    let i' = Gwdb.insert_string base s' in
    (if i <> i' then
     match report with
     | Some fn -> fn (Fix_WrongUTF8Encoding (ifam, iper, Some (i, i')))
     | None -> ());
    i'
  in
  let nbf = nb_of_families base in
  let nbp = nb_of_persons base in
  let nb = nbp + nbf in
  let fp i = i in
  let ff i = i in
  let fs ifam iper i = normalize_utf_8 ifam iper i in
  let fd ifam iper = function
    | Dtext d -> Dtext (normalize_utf_8_date ifam iper d)
    | d -> d
  in
  Gwdb.Collection.iteri
    (fun i fam ->
      progress i nb;
      let ifam = Gwdb.get_ifam fam in
      let f = Gwdb.gen_family_of_family fam in
      let f' =
        Futil.map_family_ps ~fd:(fd (Some ifam) None) fp ff
          (fs (Some ifam) None) f
      in
      if f' <> f then Gwdb.patch_family base ifam f')
    (Gwdb.families base);
  Gwdb.Collection.iteri
    (fun i per ->
      progress (nbf + i) nb;
      let iper = Gwdb.get_iper per in
      let p = Gwdb.gen_person_of_person per in
      let p' =
        Futil.map_person_ps ~fd:(fd None (Some iper)) fp (fs None (Some iper)) p
      in
      if p' <> p then Gwdb.patch_person base iper p')
    (Gwdb.persons base)

(* Identify conflicting keys.
   Change occ to resolve
   scan_utf8_conflicts seems to be doing a better job
     (detects conflicting apostrophs)!!
*)

let fix_key ?report progress base =
  let nb_ind = nb_of_persons base in
  let ipers = Gwdb.ipers base in
  let skip = Gwdb.iper_marker ipers false in
  Gwdb.Collection.iteri
    (fun i ip ->
      progress i nb_ind;
      let p = poi base ip in
      let f = Gwdb.p_first_name base p in
      let s = Gwdb.p_surname base p in
      if f <> "?" && s <> "?" then
        let p_key = Name.concat f s in
        let ipers = Gwdb.persons_of_name base p_key in
        let f = Name.lower f in
        let s = Name.lower s in
        let list =
          let rec loop acc = function
            | ip :: tl ->
                let p = poi base ip in
                if
                  Name.lower @@ p_first_name base p = f
                  && Name.lower @@ p_surname base p = s
                then loop ((get_iper p, get_occ p) :: acc) tl
                else loop acc tl
            | [] -> acc
          in
          loop [] ipers
        in
        let rev_list = List.sort (fun a b -> compare b a) list in
        let cnt = ref 0 in
        let mem_occ occ acc list =
          List.exists (fun (_, o) -> o = occ) acc
          || List.exists (fun (_, o) -> o = occ) list
        in
        let rec new_occ acc list =
          if mem_occ !cnt acc list then (
            incr cnt;
            new_occ acc list)
          else !cnt
        in
        let rec loop acc list =
          match acc with
          | [] -> (
              match list with
              | [] -> failwith p_key
              | (ip, occ) :: tl ->
                  Gwdb.Marker.set skip ip true;
                  loop [ (ip, occ) ] tl)
          | acc -> (
              match list with
              | [] -> acc
              | (ip, occ) :: tl ->
                  if not @@ Gwdb.Marker.get skip ip then (
                    Gwdb.Marker.set skip ip true;
                    if mem_occ occ acc tl then (
                      let occ' = new_occ acc list in
                      Gwdb.patch_person base ip
                        {
                          (Gwdb.gen_person_of_person (poi base ip)) with
                          occ = occ';
                        };
                      (match report with
                      | Some fn -> fn (Fix_UpdatedOcc (ip, occ, occ'))
                      | None -> ());
                      loop ((ip, occ') :: acc) tl)
                    else loop ((ip, occ) :: acc) tl)
                  else loop ((ip, occ) :: acc) tl)
        in
        ignore @@ loop [] rev_list)
    ipers

let special_utf_8 s =
  let s =
    if
      String.length s = 3
      && Char.code s.[0] = 0xE2
      && Char.code s.[1] = 0x80
      && (Char.code s.[2] = 0x98
         || (* ’ apostrophes typo *)
         Char.code s.[2] = 0x99)
    then " "
    else s
  in
  if String.length s > 0 && Char.code s.[0] < 0x80 then
    match s.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' -> s
    | _ -> " "
  else s

let string_unaccent ?(special = false) lower s =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else
      let t, j = Name.unaccent_utf_8 lower s i in
      let t = if special then special_utf_8 t else t in
      copy j (Buff.mstore len t)
  in
  copy 0 0

(* Scan a base to identify potential conflicts arising when:
   - replacing ’ by ' in the lower function
   - properly treating supplementary Latin accented characters (for vietnameese)
   resolution typically consists in changing the occ number (+1)
*)

let scan_utf8_conflicts ?report progress base =
  let rec new_occ fn sn occ htoc =
    let oc = string_of_int occ in
    let v1 = string_unaccent ~special:true true (fn ^ "." ^ oc ^ " " ^ sn) in
    match
      Gwdb.person_of_key base
        (string_unaccent ~special:true true fn)
        (string_unaccent ~special:true true sn)
        occ
    with
    (* Fail : changes have not been commmited yet !! *)
    (* works only for first conflict *)
    | Some _ip ->
        let occ =
          match Hashtbl.find_opt htoc v1 with
          | Some occ' -> occ' + 1
          | None -> occ + 1
        in
        new_occ fn sn occ htoc
    | _ -> occ
  in
  let nb_ind = Gwdb.nb_of_persons base in
  let ht = Hashtbl.create nb_ind in
  let htoc = Hashtbl.create 100 in
  let ipers = Gwdb.ipers base in
  let bname1 = !bname in
  let conf = { Geneweb.Config.empty with wizard = true; bname = bname1 } in
  Gwdb.Collection.iteri
    (fun i ip ->
      progress i nb_ind;
      let p = poi base ip in
      let fn = Gwdb.sou base (Gwdb.get_first_name p) in
      let sn = Gwdb.sou base (Gwdb.get_surname p) in
      let occ = string_of_int (Gwdb.get_occ p) in
      let ip0 =
        match
          Gwdb.person_of_key base
            (string_unaccent ~special:true true fn)
            (string_unaccent ~special:true true sn)
            (int_of_string occ)
        with
        | Some ip -> ip
        | _ -> Gwdb.dummy_iper
      in
      let v = fn ^ "." ^ occ ^ " " ^ sn in
      let v1 = string_unaccent ~special:true true (fn ^ "." ^ occ ^ " " ^ sn) in
      if fn <> "?" && sn <> "?" then
        if not (Hashtbl.mem ht v1) then Hashtbl.add ht v1 v1
        else (
          Printf.printf "conflit %s (index: %d) avec %s (%s)...\n" v i
            (Hashtbl.find ht v1) (string_of_iper ip0);
          let occ' = new_occ fn sn (int_of_string occ) htoc in
          Hashtbl.add htoc v1 occ';
          (* record portrait filename *)
          let portrait =
            match Geneweb.Image.get_portrait_path conf base p with
            | Some (`Path pa) -> Some pa
            | _ -> None
          in
          portraits_list := (portrait, occ') :: !portraits_list;
          Gwdb.patch_person base ip
            { (Gwdb.gen_person_of_person p) with occ = occ' };
          match report with
          | Some fn -> fn (Fix_UpdatedOcc (ip, int_of_string occ, occ'))
          | None -> ()))
    ipers

let rename_portraits p_list dry_run =
  List.iter
    (fun (portrait, noc) ->
      (* TODO REORG  Blasons *)
      match portrait with
      | Some portrait ->
          let dir = Filename.dirname portrait in
          let fname = Filename.basename portrait in
          let parts = String.split_on_char '.' fname in
          if List.length parts = 4 then
            match parts with
            | [ fn; occ; sn; ext ] -> (
                let new_fname = Printf.sprintf "%s.%d.%s.%s" fn occ' sn ext in
                if dry_run then
                  Printf.printf "Will rename %s.%s.%s.%s (%s -> %d)\n" fn occ sn
                    ext occ occ'
                else
                  try
                    Sys.rename portrait (Filename.concat dir new_fname);
                    Printf.printf "Renamed %s.%s.%s.%s (%s -> %d)\n" fn occ sn
                      ext occ occ'
                  with Failure _ ->
                    Printf.printf "Failed to rename %s\n" portrait)
            | _ -> Printf.printf "Failed to rename %s\n" portrait
          else Printf.printf "Failed to rename %s\n" portrait
      | None -> ())
    p_list

let aux conf txt
    (fn : ?report:(patch -> unit) -> (int -> int -> unit) -> base -> unit) ~v1
    ~v2 base n cnt =
  let string_of_patch =
    let string_of_p i = Gutil.designation base (poi base i) in
    function
    | Fix_WrongUTF8Encoding (ifam_opt, iper_opt, opt) ->
        Printf.sprintf "Fixed invalid UTF-8 sequence (%s): %s"
          (match ifam_opt with
          | Some i -> "ifam " ^ string_of_ifam i
          | None -> (
              match iper_opt with
              | Some i -> "iper " ^ string_of_iper i
              | None -> assert false))
          (match opt with
          | Some (i, i') -> string_of_istr i ^ " -> " ^ string_of_istr i'
          | None -> "Dtext")
    | Fix_UpdatedOcc (iper, oocc, nocc) ->
        let ofn = sou base (get_first_name (poi base iper)) in
        let osn = sou base (get_surname (poi base iper)) in
        let okey = (Name.lower ofn, Name.lower osn, oocc) in
        let pgl =
          let db = Gwdb.read_nldb base in
          let db = Geneweb.Notes.merge_possible_aliases conf db in
          Geneweb.Perso.links_to_ind conf base db okey
        in
        if pgl <> [] then
          let dir = !Geneweb.GWPARAM.portraits_d bname in
          let portrait =
            Filename.concat dir (Format.sprintf "%s.%d.%s" ofn ooc osn)
          in
          let exists ext =
            let fname = portrait ^ ext in
            if Sys.file_exists fname then Some fname else None
          in
          let full_portrait =
            Mutil.array_find_map exists
              Geneweb.Image.authorized_image_file_extension
          in
          let portrait_str =
            match full_portrait with
            | Some f ->
                let portrait = Filename.basename f in
                rename_portraits conf bname [ (Some portrait, noc) ] !dry_run;
                Printf.sprintf "Uptated portrait %s<br>\n" portrait
            | None -> ""
          in
          let notes_list =
            Printf.sprintf {|<br><span style="color:#FF0000;">%s</span><br>%s|}
              (Geneweb.Util.transl conf "notes to be updated")
              (Geneweb.NotesDisplay.linked_list conf base pgl)
          in
          Printf.sprintf "%sUptated occ for %s: %d -> %d%s" portrait_str
            (string_of_p iper) ooc noc notes_list
        else ""
  in
  let i' = ref 0 in
  if v1 then (
    print_endline txt;
    flush stdout;
    ProgrBar.start ());
  let progress =
    if v2 then (fun i n ->
      ProgrBar.run i n;
      i' := i)
    else if v1 then ProgrBar.run
    else fun _ _ -> ()
  in
  let report =
    if v2 then
      Some
        (fun s ->
          incr cnt;
          ProgrBar.suspend ();
          print_endline @@ "\t" ^ string_of_patch s;
          flush stdout;
          ProgrBar.restart !i' n)
    else Some (fun _ -> incr cnt)
  in
  fn ?report progress base;
  if v1 then ProgrBar.finish ()

let check ~dry_run ~verbosity ~fast ~invalid_utf8 ~p_key ~utf8_key bname =
  let v1 = !verbosity >= 1 in
  let v2 = !verbosity >= 2 in
  if not v1 then Mutil.verbose := false;
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let conf = { Geneweb.Config.empty with bname } in
  let fix = ref 0 in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  if fast then (
    load_strings_array base;
    load_persons_array base);
  if !invalid_utf8 then
    aux conf bname "Fix invalid UTF-8 sequence" fix_utf8_sequence ~v1 ~v2 base
      nb_fam fix;
  if !p_key then
    aux conf bname "Fix duplicate keys" fix_key ~v1 ~v2 base nb_ind fix;
  if !utf8_key then
    aux conf bname "Scan for possible UTF-8 conflicts" scan_utf8_conflicts ~v1
      ~v2 base nb_ind fix;
  if fast then (
    clear_strings_array base;
    clear_persons_array base);
  if v1 then (
    Printf.printf "%n changes found\n" !fix;
    flush stdout);
  if not !dry_run then (
    if !fix <> 0 then (
      (* more than 0 fixes *)
      Gwdb.commit_patches base;
      if v1 then (
        Printf.printf "%n changes commited\n" !fix;
        flush stdout);
      if !portraits_list <> [] then
        rename_portraits conf bname !portraits_list false;
      if v1 then (
        Printf.printf "Portraits renamed\n";
        flush stdout))
    else if v1 then (
      Printf.printf "No change\n";
      flush stdout);
    if v1 then (
      Printf.printf "Rebuilding the indexes..\n";
      flush stdout);
    Gwdb.sync base)
  else (
    Printf.printf "No commits\n";
    rename_portraits !portraits_list true);
  if v1 then (
    Printf.printf "Done\n";
    flush stdout);
  if !fix <> 0 then (
    Printf.printf {|<span style="color:#FF0000;">WARNING WIP</span><br>|};
    Printf.printf
      "- Wizard access to the pages to be modified is not provided<br>")

(**/**)

let speclist =
  [
    ("-dry-run", Arg.Set dry_run, " do not commit changes (only print)");
    ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
    ("-fast", Arg.Set fast, " fast mode. Needs more memory.");
    ("-persons-key", Arg.Set p_key, " missing doc");
    ("-invalid-utf8", Arg.Set invalid_utf8, " missing doc");
    ("-utf8_key", Arg.Set utf8_key, " check potential utf8 p_key conflicts");
    ( "-index",
      Arg.Set index,
      " rebuild index. It is automatically enable by any other option." );
    ("-server", Arg.String (fun s -> server := s), " missing doc");
    ("-gwd_p", Arg.Int (fun x -> gwd_port := x), " missing doc");
  ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then (
    Printf.eprintf "Missing base name\n";
    exit 2);
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again
  @@ fun () ->
  if !invalid_utf8 || !p_key || !utf8_key || !index then ()
  else (
    invalid_utf8 := true;
    p_key := true;
    utf8_key := true);
  check ~dry_run ~fast ~verbosity ~invalid_utf8 ~p_key ~utf8_key !bname

let _ = main ()
