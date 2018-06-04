(* $Id: db2disk.ml,v 5.27 2012-01-27 16:57:07 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def
open Mutil
open Printf

let magic_patch = "GwPt0002"

type patches =
  { mutable nb_per : int;
    mutable nb_fam : int;
    nb_per_ini : int;
    nb_fam_ini : int;
    h_person : (iper, (iper, string) gen_person) Hashtbl.t;
    h_ascend : (iper, ifam gen_ascend) Hashtbl.t;
    h_union : (iper, ifam gen_union) Hashtbl.t;
    h_family : (ifam, (iper, string) gen_family) Hashtbl.t;
    h_couple : (ifam, iper gen_couple) Hashtbl.t;
    h_descend : (ifam, iper gen_descend) Hashtbl.t;
    h_key : (string * string * int, iper option) Hashtbl.t;
    h_name : (string, iper list) Hashtbl.t }

type db2 =
  { phony : unit -> unit;
    bdir2 : string;
    cache_chan : (string * string * string, in_channel) Hashtbl.t;
    patches : patches;
    mutable parents_array : ifam option array option;
    mutable consang_array : Adef.fix array option;
    mutable family_array : ifam array array option;
    mutable father_array : iper array option;
    mutable mother_array : iper array option;
    mutable children_array : iper array array option }

(* reading in files style database 2 *)

let fast_open_in_bin_and_seek db2 f1 f2 f pos =
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, f) with
      Not_found ->
        let ic =
          open_in_bin (List.fold_left Filename.concat db2.bdir2 [f1; f2; f])
        in
        Hashtbl.add db2.cache_chan (f1, f2, f) ic; ic
  in
  seek_in ic pos; ic

let field_exists db2 (f1, f2) =
  let fname = List.fold_left Filename.concat db2.bdir2 [f1; f2; "access"] in
  Sys.file_exists fname

let get_field_acc db2 i (f1, f2) =
  try
    let ic = fast_open_in_bin_and_seek db2 f1 f2 "access" (4 * i) in
    let r = input_binary_int ic in
    assert (r >= -1); assert (r <= 0x3fffffff); r
  with e ->
    eprintf "Error get_field_acc \"%s/%s/access\" i = %d\n" f1 f2 i;
    flush stderr;
    raise e

let get_field_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in Iovalue.input ic

let get_field_2_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in
  let r = Iovalue.input ic in let s = Iovalue.input ic in r, s

let get_field db2 i path =
  let pos = get_field_acc db2 i path in get_field_data db2 pos path "data"

let string_of_istr2 db2 f pos =
  if pos = -1 then "" else get_field_data db2 pos f "data"

(* hash tables in disk *)

type ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let rec hashtbl_find_rec key =
  function
    Empty -> raise Not_found
  | Cons (k, d, rest) ->
      if compare key k = 0 then d else hashtbl_find_rec key rest

let hashtbl_find dir file key =
  let ic_ht = open_in_bin (Filename.concat dir file) in
  let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
  let alen = input_binary_int ic_hta in
  let pos = int_size + Hashtbl.hash key mod alen * int_size in
  seek_in ic_hta pos;
  let pos = input_binary_int ic_hta in
  close_in ic_hta;
  seek_in ic_ht pos;
  let bl : (_, _) bucketlist = Iovalue.input ic_ht in
  close_in ic_ht; hashtbl_find_rec key bl

let hashtbl_find_all dir file key =
  let rec find_in_bucket =
    function
      Empty -> []
    | Cons (k, d, rest) ->
        if compare k key = 0 then d :: find_in_bucket rest
        else find_in_bucket rest
  in
  match
    try Some (open_in_bin (Filename.concat dir file)) with Sys_error _ -> None
  with
    Some ic_ht ->
      let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
      let alen = input_binary_int ic_hta in
      let pos = int_size + Hashtbl.hash key mod alen * int_size in
      seek_in ic_hta pos;
      let pos = input_binary_int ic_hta in
      close_in ic_hta;
      seek_in ic_ht pos;
      let bl : (_, _) bucketlist = Iovalue.input ic_ht in
      close_in ic_ht; find_in_bucket bl
  | None -> []

let key_hashtbl_find dir file k = hashtbl_find dir file (Db2.key2_of_key k)

(* string person index version 2 *)

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : (string * int) list;
    mutable ini : string;
    mutable curr_i : int;
    mutable curr_s : string }

let start_with s p =
  String.length p <= String.length s && String.sub s 0 (String.length p) = p

type string_person =
    Sp of int
  | SpNew of string

let sorted_patched_person_strings db2 is_first_name =
  let particles =
    Mutil.input_particles (Filename.concat db2.bdir2 "particles.txt")
  in
  let sl =
    Hashtbl.fold
      (fun ip p sl ->
         let s = if is_first_name then p.first_name else p.surname in s :: sl)
      db2.patches.h_person []
  in
  let sl = list_uniq (List.sort compare sl) in
  let sl =
    List.map
      (fun s ->
         let s_ord =
           try
             let part = List.find (start_with s) particles in
             let plen = String.length part in
             String.sub s plen (String.length s - plen) ^ " (" ^ part ^ ")"
           with Not_found -> s
         in
         s_ord, s)
      sl
  in
  List.sort compare sl

let spi2_first db2 spi (f1, f2) s =
  spi.ini <- s;
  let i_opt =
    (* to be faster, go directly to the first string starting with
       the same char *)
    if s = "" then Some 0
    else
      let nbc = Name.nbc s.[0] in
      let rec loop =
        function
          (s1, i1) :: list ->
            if s1 = "" then loop list
            else
              let nbc1 = Name.nbc s1.[0] in
              if nbc = nbc1 && nbc > 0 && nbc <= String.length s &&
                 nbc <= String.length s1 &&
                 String.sub s 0 nbc = String.sub s1 0 nbc
              then
                Some i1
              else loop list
        | [] -> None
      in
      loop spi.index_of_first_char
  in
  let first_in_disk =
    match i_opt with
      Some i ->
        let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (4 * i) in
        let pos = input_binary_int ic in
        let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos in
        begin try
          let rec loop i =
            let (s1, pos) = (Iovalue.input ic : string * int) in
            if start_with s1 s then Some (s1, pos, i) else loop (i + 1)
          in
          loop i
        with End_of_file -> None
        end
    | None -> None
  in
  let first_patched =
    let patched_sl = sorted_patched_person_strings db2 spi.is_first_name in
    let rec loop =
      function
        (s2_ord, s2) :: sl ->
          if s2_ord < s then loop sl
          else if start_with s2_ord s then Some (s2_ord, s2)
          else loop sl
      | [] -> None
    in
    loop patched_sl
  in
  match first_in_disk, first_patched with
    Some (s1, _, _), Some (s2_ord, s2) when s2_ord < s1 ->
      spi.curr_s <- s2_ord; SpNew s2
  | Some (s1, pos, i), _ -> spi.curr_i <- i; spi.curr_s <- s1; Sp pos
  | None, Some (s2_ord, s2) -> spi.curr_s <- s2_ord; SpNew s2
  | None, None -> raise Not_found

let spi2_next db2 spi (f1, f2) need_whole_list =
  let i_opt =
    if spi.ini = "" && not need_whole_list then
      let rec loop =
        function
          (_, i1) :: ((_, i2) :: _ as list) ->
            if spi.curr_i = i1 then Some i2 else loop list
        | [] | [_] -> None
      in
      loop spi.index_of_first_char
    else Some (spi.curr_i + 1)
  in
  let next_in_disk =
    match i_opt with
      Some i ->
        begin try
          let ic =
            let ic =
              fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (i * 4)
            in
            let pos = input_binary_int ic in
            fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos
          in
          let (s, pos) = (Iovalue.input ic : string * int) in
          let dlen = i - spi.curr_i in Some (i, s, pos, dlen)
        with End_of_file -> None
        end
    | None -> None
  in
  let next_patched =
    let patched_sl = sorted_patched_person_strings db2 spi.is_first_name in
    let rec loop =
      function
        (s2_ord, s2) :: sl ->
          if s2_ord <= spi.curr_s then loop sl else Some (s2_ord, s2)
      | [] -> None
    in
    loop patched_sl
  in
  match next_in_disk, next_patched with
    Some (_, s1, _, _), Some (s2_ord, s2) when s2_ord < s1 ->
      spi.curr_s <- s2_ord; SpNew s2, 1
  | Some (i, s1, pos, dlen), _ ->
      spi.curr_i <- i; spi.curr_s <- s1; Sp pos, dlen
  | None, Some (s2_ord, s2) -> spi.curr_s <- s2_ord; SpNew s2, 1
  | None, None -> raise Not_found

let spi2gen_add pl db2 spi s =
  let proj =
    if spi.is_first_name then fun p -> p.first_name else fun p -> p.surname
  in
  Hashtbl.fold
    (fun _ p iperl -> if proj p = s then p.key_index :: iperl else iperl)
    db2.patches.h_person pl

let spi2_find db2 spi (f1, f2) pos =
  let dir = List.fold_left Filename.concat db2.bdir2 [f1; f2] in
  let pl = hashtbl_find_all dir "person_of_string.ht" pos in
  let s = string_of_istr2 db2 (f1, f2) pos in spi2gen_add pl db2 spi s

let spi2gen_find = spi2gen_add []

(* *)

let disk_person2_of_key db2 fn sn oc =
  let person_of_key_d = Filename.concat db2.bdir2 "person_of_key" in
  try
    let ifn = hashtbl_find person_of_key_d "istr_of_string.ht" fn in
    let isn = hashtbl_find person_of_key_d "istr_of_string.ht" sn in
    let key = ifn, isn, oc in
    Some (key_hashtbl_find person_of_key_d "iper_of_key.ht" key : iper)
  with Not_found -> None

let person2_of_key db2 fn sn oc =
  let fn = Name.lower (nominative fn) in
  let sn = Name.lower (nominative sn) in
  try Hashtbl.find db2.patches.h_key (fn, sn, oc) with
    Not_found -> disk_person2_of_key db2 fn sn oc

let strings2_of_fsname db2 f s =
  let k = Name.crush_lower s in
  let dir = List.fold_left Filename.concat db2.bdir2 ["person"; f] in
  hashtbl_find_all dir "string_of_crush.ht" k

let persons2_of_name db2 s =
  let s = Name.crush_lower s in
  let dir = Filename.concat db2.bdir2 "person_of_name" in
  List.rev_append (try Hashtbl.find db2.patches.h_name s with Not_found -> [])
    (hashtbl_find_all dir "person_of_name.ht" s)

let persons_of_first_name_or_surname2 db2 is_first_name =
  let f1 = "person" in
  let f2 = if is_first_name then "first_name" else "surname" in
  let fdir = List.fold_left Filename.concat db2.bdir2 [f1; f2] in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let ic = open_in_bin index_ini_fname in
  let iofc : (string * int) list = input_value ic in
  close_in ic;
  {is_first_name = is_first_name; index_of_first_char = iofc; ini = "";
   curr_i = 0; curr_s = ""}

let load_array2 bdir nb_ini nb def f1 f2 get =
  if nb = 0 then [| |]
  else
    try
      let ic_acc =
        open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "access"])
      in
      let ic_dat =
        open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "data"])
      in
      let tab = Array.make nb def in
      for i = 0 to nb_ini - 1 do
        tab.(i) <- get ic_dat (input_binary_int ic_acc)
      done;
      close_in ic_dat;
      close_in ic_acc;
      tab
    with e ->
      eprintf "Error load_array2 %s/%s nb_ini %d nb %d\n" f1 f2 nb_ini nb;
      flush stderr;
      raise e

let load_couples_array2 db2 =
  eprintf "*** loading couples array\n";
  flush stderr;
  let nb = db2.patches.nb_fam in
  begin match db2.father_array with
    Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir2 db2.patches.nb_fam_ini nb (Adef.iper_of_int 0)
          "family" "father"
          (fun ic_dat pos -> seek_in ic_dat pos; Iovalue.input ic_dat)
      in
      Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) <- Adef.father c)
        db2.patches.h_couple;
      db2.father_array <- Some tab
  end;
  match db2.mother_array with
    Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir2 db2.patches.nb_fam_ini nb (Adef.iper_of_int 0)
          "family" "mother"
          (fun ic_dat pos -> seek_in ic_dat pos; Iovalue.input ic_dat)
      in
      Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) <- Adef.mother c)
        db2.patches.h_couple;
      db2.mother_array <- Some tab

let parents_array2 db2 nb_ini nb =
  let arr =
    if nb_ini = 0 then Array.make nb None
    else
      load_array2 db2.bdir2 nb_ini nb None "person" "parents"
        (fun ic_dat pos ->
           if pos = -1 then None
           else
             begin seek_in ic_dat pos; Some (Iovalue.input ic_dat : ifam) end)
  in
  Hashtbl.iter (fun i a -> arr.(Adef.int_of_iper i) <- a.parents)
    db2.patches.h_ascend;
  arr

let consang_array2 db2 nb =
  let arr =
    let cg_fname =
      List.fold_left Filename.concat db2.bdir2 ["person"; "consang"; "data"]
    in
    match try Some (open_in_bin cg_fname) with Sys_error _ -> None with
      Some ic ->
        let tab = input_value ic in
        close_in ic;
        if Array.length tab < db2.patches.nb_per_ini then
          failwith
            (sprintf "consang_array2 array length = %d < %d"
               (Array.length tab) db2.patches.nb_per_ini);
        if nb > Array.length tab then
          Array.append tab
            (Array.make (nb - Array.length tab) Adef.no_consang)
        else tab
    | None -> Array.make nb Adef.no_consang
  in
  Hashtbl.iter (fun i a -> arr.(Adef.int_of_iper i) <- a.consang)
    db2.patches.h_ascend;
  arr

let family_array2 db2 =
  let fname =
    List.fold_left Filename.concat db2.bdir2 ["person"; "family"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  if Array.length tab < db2.patches.nb_per_ini then
    failwith
      (sprintf "family_array2 array length = %d < %d" (Array.length tab)
         db2.patches.nb_per_ini);
  tab

let children_array2 db2 =
  let fname =
    List.fold_left Filename.concat db2.bdir2 ["family"; "children"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  if Array.length tab < db2.patches.nb_fam_ini then
    failwith
      (sprintf "children_array2 array length = %d < %d" (Array.length tab)
         db2.patches.nb_fam_ini);
  tab

let read_notes db2 fnotes rn_mode =
  let bdir = db2.bdir2 in
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  match
    try Some (Secure.open_in (Filename.concat bdir fname)) with
      Sys_error _ -> None
  with
    Some ic ->
      let str =
        match rn_mode with
          RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> (try input_line ic with End_of_file -> "")
        | RnAll ->
            let rec loop len =
              match try Some (input_char ic) with End_of_file -> None with
                Some c -> loop (Buff.store len c)
              | _ -> Buff.get len
            in
            loop 0
      in
      close_in ic; str
  | None -> ""

let check_magic ic magic id =
  let b = really_input_string ic (String.length magic) in
  if b <> magic then failwith (sprintf "bad %s magic number" id)

let commit_patches2 db2 =
  let fname = Filename.concat db2.bdir2 "patches" in
  let oc = open_out_bin (fname ^ "1") in
  output_string oc magic_patch;
  output_value_no_sharing oc db2.patches;
  close_out oc;
  remove_file (fname ^ "~");
  (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
  Sys.rename (fname ^ "1") fname

let commit_notes2 db2 fnotes s =
  let bdir = db2.bdir2 in
  if fnotes <> "" then
    (try Unix.mkdir (Filename.concat bdir "notes_d") 0o755 with _ -> ());
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  let fname = Filename.concat bdir fname in
  (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
  (try Sys.rename fname (fname ^ "~") with _ -> ());
  if s = "" then ()
  else let oc = Secure.open_out fname in output_string oc s; close_out oc

let base_of_base2 bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let bdir = Filename.concat bname "base_d" in
  let patches =
    let patch_fname = Filename.concat bdir "patches" in
    match try Some (open_in_bin patch_fname) with Sys_error _ -> None with
      Some ic ->
        check_magic ic magic_patch "patch";
        let p = input_value ic in close_in ic; flush stderr; p
    | None ->
        let nb_per =
          let fname =
            List.fold_left Filename.concat bdir ["person"; "sex"; "access"]
          in
          let st = Unix.lstat fname in st.Unix.st_size / 4
        in
        let nb_fam =
          let fname =
            List.fold_left Filename.concat bdir
              ["family"; "marriage"; "access"]
          in
          let st = Unix.lstat fname in st.Unix.st_size / 4
        in
        let empty_ht () = Hashtbl.create 1 in
        {nb_per = nb_per; nb_fam = nb_fam; nb_per_ini = nb_per;
         nb_fam_ini = nb_fam; h_person = empty_ht (); h_ascend = empty_ht ();
         h_union = empty_ht (); h_family = empty_ht ();
         h_couple = empty_ht (); h_descend = empty_ht (); h_key = empty_ht ();
         h_name = empty_ht ()}
  in
  {bdir2 = bdir; cache_chan = Hashtbl.create 1; patches = patches;
   parents_array = None; consang_array = None; family_array = None;
   father_array = None; mother_array = None; children_array = None;
   phony = fun () -> ()}

let iter_patched_keys db2 f = Hashtbl.iter f db2.patches.h_key
