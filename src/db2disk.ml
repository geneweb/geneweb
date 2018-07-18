(* $Id: db2disk.ml,v 5.27 2012-01-27 16:57:07 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def;
open Mutil;
open Printf;

value magic_patch = "GwPt0002";

type patches =
  { nb_per : mutable int;
    nb_fam : mutable int;
    nb_per_ini : int;
    nb_fam_ini : int;
    h_person : Hashtbl.t iper (gen_person iper string);
    h_ascend : Hashtbl.t iper (gen_ascend ifam);
    h_union : Hashtbl.t iper (gen_union ifam);
    h_family : Hashtbl.t ifam (gen_family iper string);
    h_couple : Hashtbl.t ifam (gen_couple iper);
    h_descend : Hashtbl.t ifam (gen_descend iper);
    h_key : Hashtbl.t (string * string * int) (option iper);
    h_name : Hashtbl.t string (list iper) }
;

type db2 =
  { phony : unit -> unit; (* to prevent usage of "=" in the program *)
    bdir2 : string;
    cache_chan : Hashtbl.t (string * string * string) in_channel;
    patches : patches;
    parents_array : mutable option (array (option ifam));
    consang_array : mutable option (array Adef.fix);
    family_array : mutable option (array (array ifam));
    father_array : mutable option (array iper);
    mother_array : mutable option (array iper);
    children_array : mutable option (array (array iper)) }
;

(* reading in files style database 2 *)

value fast_open_in_bin_and_seek db2 f1 f2 f pos = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, f) with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir2 [f1; f2; f])
        in
        Hashtbl.add db2.cache_chan (f1, f2, f) ic;
        ic
      } ]
  in
  seek_in ic pos;
  ic
};

value field_exists db2 (f1, f2) =
  let fname = List.fold_left Filename.concat db2.bdir2 [f1; f2; "access"] in
  Sys.file_exists fname
;

value get_field_acc db2 i (f1, f2) =
  try do {
    let ic = fast_open_in_bin_and_seek db2 f1 f2 "access" (4 * i) in
    let r = input_binary_int ic in
    assert (r >= -1);
    assert (r <= 0x3fffffff);
    r
  }
  with e -> do {
    eprintf "Error get_field_acc \"%s/%s/access\" i = %d\n" f1 f2 i;
    flush stderr;
    raise e;
  }
;

value get_field_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in
  Iovalue.input ic
;

value get_field_2_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in
  let r = Iovalue.input ic in
  let s = Iovalue.input ic in
  (r, s)
;

value get_field db2 i path =
  let pos = get_field_acc db2 i path in
  get_field_data db2 pos path "data"
;

value string_of_istr2 db2 f pos =
  if pos = -1 then "" else get_field_data db2 pos f "data"
;

(* hash tables in disk *)

type bucketlist 'a 'b =
  [ Empty
  | Cons of 'a and 'b and bucketlist 'a 'b ]
;

value rec hashtbl_find_rec key =
  fun
  [ Empty -> raise Not_found
  | Cons k d rest ->
      if compare key k = 0 then d else hashtbl_find_rec key rest ]
;

value hashtbl_find dir file key = do {
  let ic_ht = open_in_bin (Filename.concat dir file) in
  let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
  let alen = input_binary_int ic_hta in
  let pos = int_size + (Hashtbl.hash key) mod alen * int_size in
  seek_in ic_hta pos;
  let pos = input_binary_int ic_hta in
  close_in ic_hta;
  seek_in ic_ht pos;
  let bl : bucketlist _ _ = Iovalue.input ic_ht in
  close_in ic_ht;
  hashtbl_find_rec key bl
};

value hashtbl_find_all dir file key = do {
  let rec find_in_bucket =
    fun
    [ Empty -> []
    | Cons k d rest ->
        if compare k key = 0 then [d :: find_in_bucket rest]
        else find_in_bucket rest ]
  in
  match
    try Some (open_in_bin (Filename.concat dir file)) with
    [ Sys_error _ -> None ]
  with
  [ Some ic_ht -> do {
  let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
  let alen = input_binary_int ic_hta in
  let pos = int_size + (Hashtbl.hash key) mod alen * int_size in
  seek_in ic_hta pos;
  let pos = input_binary_int ic_hta in
  close_in ic_hta;
  seek_in ic_ht pos;
  let bl : bucketlist _ _ = Iovalue.input ic_ht in
  close_in ic_ht;
  find_in_bucket bl
    }
  | None -> [] ]
};

value key_hashtbl_find dir file k = hashtbl_find dir file (Db2.key2_of_key k);

(* string person index version 2 *)

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : list (string * int);
    ini : mutable string;
    curr_i : mutable int;
    curr_s : mutable string }
;

value start_with s p =
  String.length p <= String.length s &&
  String.sub s 0 (String.length p) = p
;

type string_person =
  [ Sp of int
  | SpNew of string ]
;

value sorted_patched_person_strings db2 is_first_name =
  let particles =
    Mutil.input_particles (Filename.concat db2.bdir2 "particles.txt")
  in
  let sl =
    Hashtbl.fold
      (fun ip p sl ->
         let s = if is_first_name then p.first_name else p.surname in
         [s :: sl])
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
             String.sub s plen (String.length s - plen) ^ " (" ^
             part ^ ")"
           with
           [ Not_found -> s ]
         in
         (s_ord, s))
       sl
  in
  List.sort compare sl
;

value spi2_first db2 spi (f1, f2) s = do {
  spi.ini := s;
  let i_opt =
    (* to be faster, go directly to the first string starting with
       the same char *)
    if s = "" then Some 0
    else
      let nbc = Name.nbc s.[0] in
      loop spi.index_of_first_char where rec loop =
        fun
        [ [(s1, i1) :: list] ->
            if s1 = "" then loop list
            else
              let nbc1 = Name.nbc s1.[0] in
              if nbc = nbc1 && nbc > 0 && nbc <= String.length s &&
                 nbc <= String.length s1 &&
                 String.sub s 0 nbc = String.sub s1 0 nbc
              then Some i1
              else loop list
        | [] -> None ]
  in
  let first_in_disk =
    match i_opt with
    [ Some i ->
        let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (4 * i) in
        let pos = input_binary_int ic in
        let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos in
        try
          loop i where rec loop i =
            let (s1, pos) : (string * int) = Iovalue.input ic in
            if start_with s1 s then Some (s1, pos, i) else loop (i + 1)
        with
        [ End_of_file -> None ]
    | None -> None ]
  in
  let first_patched =
    let patched_sl = sorted_patched_person_strings db2 spi.is_first_name in
    loop patched_sl where rec loop =
      fun
      [ [(s2_ord, s2) :: sl] ->
          if s2_ord < s then loop sl
          else if start_with s2_ord s then Some (s2_ord, s2)
          else loop sl
      | [] -> None ]
  in
  match (first_in_disk, first_patched) with
  [ (Some (s1, _, _), Some (s2_ord, s2)) when s2_ord < s1 -> do {
      spi.curr_s := s2_ord;
      SpNew s2
    }
  | (Some (s1, pos, i), _) -> do {
      spi.curr_i := i;
      spi.curr_s := s1;
      Sp pos;
    }
  | (None, Some (s2_ord, s2)) -> do {
      spi.curr_s := s2_ord;
      SpNew s2
    }
  | (None, None) -> raise Not_found ]
};

value spi2_next db2 spi (f1, f2) need_whole_list =
  let i_opt =
    if spi.ini = "" && not need_whole_list then
      loop spi.index_of_first_char where rec loop =
        fun
        [ [(_, i1) :: ([(_, i2) :: _] as list)] ->
            if spi.curr_i = i1 then Some i2 else loop list
        | [] | [_] -> None ]
    else Some (spi.curr_i + 1)
  in
  let next_in_disk =
    match i_opt with
    [ Some i ->
        try
          let ic =
            let ic =
              fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (i * 4)
            in
            let pos = input_binary_int ic in
            fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos
          in
          let (s, pos) : (string * int) = Iovalue.input ic in
          let dlen = i - spi.curr_i in
          Some (i, s, pos, dlen)
        with
        [ End_of_file -> None ]
    | None -> None ]
  in
  let next_patched =
    let patched_sl = sorted_patched_person_strings db2 spi.is_first_name in
    loop patched_sl where rec loop =
      fun
      [ [(s2_ord, s2) :: sl] ->
          if s2_ord <= spi.curr_s then loop sl else Some (s2_ord, s2)
      | [] -> None ]
  in
  match (next_in_disk, next_patched) with
  [ (Some (_, s1, _, _), Some (s2_ord, s2)) when s2_ord < s1 -> do {
      spi.curr_s := s2_ord;
      (SpNew s2, 1)
    }
  | (Some (i, s1, pos, dlen), _) -> do {
      spi.curr_i := i;
      spi.curr_s := s1;
      (Sp pos, dlen)
    }
  | (None, Some (s2_ord, s2)) -> do {
      spi.curr_s := s2_ord;
      (SpNew s2, 1)
    }
  | (None, None) -> raise Not_found ]
;

value spi2gen_add pl db2 spi s =
  let proj =
    if spi.is_first_name then fun p -> p.first_name
    else fun p -> p.surname
  in
  Hashtbl.fold
    (fun _ p iperl -> if proj p = s then [p.key_index :: iperl] else iperl)
    db2.patches.h_person pl
;

value spi2_find db2 spi (f1, f2) pos =
  let dir = List.fold_left Filename.concat db2.bdir2 [f1; f2] in
  let pl = hashtbl_find_all dir "person_of_string.ht" pos in
  let s = string_of_istr2 db2 (f1, f2) pos in
  spi2gen_add pl db2 spi s
;

value spi2gen_find = spi2gen_add [];

(* *)

value disk_person2_of_key db2 fn sn oc =
  let person_of_key_d = Filename.concat db2.bdir2 "person_of_key" in
  try do {
    let ifn = hashtbl_find person_of_key_d "istr_of_string.ht" fn in
    let isn = hashtbl_find person_of_key_d "istr_of_string.ht" sn in
    let key = (ifn, isn, oc) in
    Some (key_hashtbl_find person_of_key_d "iper_of_key.ht" key : iper)
  }
  with
  [ Not_found -> None ]
;

value person2_of_key db2 fn sn oc =
  let fn = Name.lower (nominative fn) in
  let sn = Name.lower (nominative sn) in
  try Hashtbl.find db2.patches.h_key (fn, sn, oc) with
  [ Not_found -> disk_person2_of_key db2 fn sn oc ]
;

value strings2_of_fsname db2 f s =
  let k = Name.crush_lower s in
  let dir = List.fold_left Filename.concat db2.bdir2 ["person"; f] in
  hashtbl_find_all dir "string_of_crush.ht" k
;

value persons2_of_name db2 s =
  let s = Name.crush_lower s in
  let dir = Filename.concat db2.bdir2 "person_of_name" in
  List.rev_append
    (try Hashtbl.find db2.patches.h_name s with [ Not_found -> [] ])
    (hashtbl_find_all dir "person_of_name.ht" s)
;

value persons_of_first_name_or_surname2 db2 is_first_name = do {
  let f1 = "person" in
  let f2 = if is_first_name then "first_name" else "surname" in
  let fdir = List.fold_left Filename.concat db2.bdir2 [f1; f2] in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let ic = open_in_bin index_ini_fname in
  let iofc : list (string * int) = input_value ic in
  close_in ic;
  {is_first_name = is_first_name; index_of_first_char = iofc; ini = "";
   curr_i = 0; curr_s = ""}
};

value load_array2 bdir nb_ini nb def f1 f2 get =
  if nb = 0 then [| |]
  else
    try do {
      let ic_acc =
        open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "access"])
      in
      let ic_dat =
        open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "data"])
      in
      let tab = Array.make nb def in
      for i = 0 to nb_ini - 1 do {
        tab.(i) := get ic_dat (input_binary_int ic_acc);
      };
      close_in ic_dat;
      close_in ic_acc;
      tab
    }
    with e -> do {
      eprintf "Error load_array2 %s/%s nb_ini %d nb %d\n" f1 f2 nb_ini nb;
      flush stderr;
      raise e;
    }
;

value load_couples_array2 db2 = do {
  eprintf "*** loading couples array\n";
  flush stderr;
  let nb = db2.patches.nb_fam in
  match db2.father_array with
  [ Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir2 db2.patches.nb_fam_ini nb (Adef.iper_of_int 0)
          "family" "father"
          (fun ic_dat pos ->
             do { seek_in ic_dat pos; Iovalue.input ic_dat })
      in
      do {
        Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := Adef.father c)
          db2.patches.h_couple;
        db2.father_array := Some tab
      } ];
  match db2.mother_array with
  [ Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir2 db2.patches.nb_fam_ini nb (Adef.iper_of_int 0)
          "family" "mother"
          (fun ic_dat pos ->
             do { seek_in ic_dat pos; Iovalue.input ic_dat })
      in
      do {
        Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := Adef.mother c)
          db2.patches.h_couple;
        db2.mother_array := Some tab
      } ]
};

value parents_array2 db2 nb_ini nb = do {
  let arr =
    if nb_ini = 0 then Array.make nb None
    else
      load_array2 db2.bdir2 nb_ini nb None "person" "parents"
        (fun ic_dat pos ->
           if pos = -1 then None
           else do {
             seek_in ic_dat pos;
             Some (Iovalue.input ic_dat : ifam)
           })
  in
  Hashtbl.iter (fun i a -> arr.(Adef.int_of_iper i) := a.parents)
    db2.patches.h_ascend;
  arr
};

value consang_array2 db2 nb = do {
  let arr =
    let cg_fname =
      List.fold_left Filename.concat db2.bdir2 ["person"; "consang"; "data"]
    in
    match try Some (open_in_bin cg_fname) with [ Sys_error _ -> None ] with
    [ Some ic -> do {
        let tab = input_value ic in
        close_in ic;
        if Array.length tab < db2.patches.nb_per_ini then
          failwith
            (sprintf "consang_array2 array length = %d < %d"
               (Array.length tab) db2.patches.nb_per_ini)
        else ();
        if nb > Array.length tab then
          Array.append tab
            (Array.make (nb - Array.length tab) Adef.no_consang)
        else tab
      }
    | None -> Array.make nb Adef.no_consang ]
  in
  Hashtbl.iter (fun i a -> arr.(Adef.int_of_iper i) := a.consang)
    db2.patches.h_ascend;
  arr
};

value family_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir2 ["person"; "family"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  if Array.length tab < db2.patches.nb_per_ini then
    failwith
      (sprintf "family_array2 array length = %d < %d"
         (Array.length tab) db2.patches.nb_per_ini)
  else ();
  tab
};

value children_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir2 ["family"; "children"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  if Array.length tab < db2.patches.nb_fam_ini then
    failwith
      (sprintf "children_array2 array length = %d < %d"
         (Array.length tab) db2.patches.nb_fam_ini)
  else ();
  tab
};

value read_notes db2 fnotes rn_mode =
  let bdir = db2.bdir2 in
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  match
    try Some (Secure.open_in (Filename.concat bdir fname)) with
    [ Sys_error _ -> None ]
  with
  [ Some ic -> do {
      let str =
        match rn_mode with
        [ RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> try input_line ic with [ End_of_file -> "" ]
        | RnAll ->
            loop 0 where rec loop len =
              match try Some (input_char ic) with [ End_of_file -> None ] with
              [ Some c -> loop (Buff.store len c)
              | _ -> Buff.get len ] ]
      in
      close_in ic;
      str
    }
  | None -> "" ]
;

value check_magic ic magic id = do {
  let b = really_input_string ic (String.length magic) in
  if b <> magic then failwith (sprintf "bad %s magic number" id)
  else ();
};

value commit_patches2 db2 = do {
  let fname = Filename.concat db2.bdir2 "patches" in
  let oc = open_out_bin (fname ^ "1") in
  output_string oc magic_patch;
  output_value_no_sharing oc db2.patches;
  close_out oc;
  remove_file (fname ^ "~");
  try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
  Sys.rename (fname ^ "1") fname
};

value commit_notes2 db2 fnotes s = do {
  let bdir = db2.bdir2 in
  if fnotes <> "" then
    try Unix.mkdir (Filename.concat bdir "notes_d") 0o755 with _ -> ()
  else ();
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  let fname = Filename.concat bdir fname in
  try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
  try Sys.rename fname (fname ^ "~") with _ -> ();
  if s = "" then ()
  else do {
    let oc = Secure.open_out fname in
    output_string oc s;
    close_out oc;
  }
};

value base_of_base2 bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let bdir = Filename.concat bname "base_d" in
  let patches =
    let patch_fname = Filename.concat bdir "patches" in
    match try Some (open_in_bin patch_fname) with [ Sys_error _ -> None ] with
    [ Some ic -> do {
        check_magic ic magic_patch "patch";
        let p = input_value ic in
        close_in ic;
        flush stderr;
        p
      }
    | None ->
        let nb_per =
          let fname =
            List.fold_left Filename.concat bdir
              ["person"; "sex"; "access"]
          in
          let st = Unix.lstat fname in
          st.Unix.st_size / 4
        in
        let nb_fam =
          let fname =
            List.fold_left Filename.concat bdir
              ["family"; "marriage"; "access"]
          in
          let st = Unix.lstat fname in
          st.Unix.st_size / 4
        in
        let empty_ht () = Hashtbl.create 1 in
        {nb_per = nb_per; nb_fam = nb_fam;
         nb_per_ini = nb_per; nb_fam_ini = nb_fam;
         h_person = empty_ht (); h_ascend = empty_ht ();
         h_union = empty_ht (); h_family = empty_ht ();
         h_couple = empty_ht (); h_descend = empty_ht ();
         h_key = empty_ht (); h_name = empty_ht ()} ]
  in
  {bdir2 = bdir; cache_chan = Hashtbl.create 1; patches = patches;
   parents_array = None; consang_array = None; family_array = None;
   father_array = None; mother_array = None; children_array = None;
   phony () = ()}
;

value iter_patched_keys db2 f = Hashtbl.iter f db2.patches.h_key;
