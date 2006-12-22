(* $Id: gwdb.ml,v 5.174 2006-12-22 17:05:00 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;
open Config;
open Dbdisk;
open Def;
open Futil;
open Mutil;
open Printf;

value magic_patch = "GwPt0001";

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
    h_key : Hashtbl.t (string * string * int) iper;
    h_name : Hashtbl.t string (list iper) }
;

type db2 =
  { phony : unit -> unit; (* to prevent usage of "=" in the program *)
    bdir : string;
    cache_chan : Hashtbl.t (string * string * string) in_channel;
    patches : patches;
    parents_array : mutable option (array (option ifam));
    consang_array : mutable option (array Adef.fix);
    family_array : mutable option (array (array ifam));
    father_array : mutable option (array iper);
    mother_array : mutable option (array iper);
    children_array : mutable option (array (array iper)) }
;

type istr =
  [ Istr of dsk_istr
  | Istr2 of db2 and (string * string) and int
  | Istr2New of db2 and string ]
;

type union =
  [ Union of dsk_union
  | Union2 of db2 and int
  | Union2Gen of db2 and gen_union ifam ]
;

type family =
  [ Family of dsk_family
  | Family2 of db2 and int
  | Family2Gen of db2 and gen_family iper string ]
;
type couple =
  [ Couple of dsk_couple
  | Couple2 of db2 and int
  | Couple2Gen of db2 and gen_couple iper ]
;
type descend =
  [ Descend of dsk_descend
  | Descend2 of db2 and int
  | Descend2Gen of db2 and gen_descend iper ]
;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type gen_string_person_index 'istr = Dbdisk.string_person_index 'istr ==
  { find : 'istr -> list iper;
    cursor : string -> 'istr;
    next : 'istr -> 'istr }
;

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : list (string * int);
    ini : mutable string;
    curr : mutable int }
;

type string_person_index =
  [ Spi of gen_string_person_index dsk_istr
  | Spi2 of db2 and string_person_index2 ]
;

value fast_open_in_bin_and_seek db2 f1 f2 f pos = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, f) with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir [f1; f2; f])
        in
        Hashtbl.add db2.cache_chan (f1, f2, f) ic;
        ic
      } ]
  in
  seek_in ic pos;
  ic
};

value get_field_acc db2 i (f1, f2) =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 "access" (4 * i) in
  input_binary_int ic
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
  if pos = -1 || pos = Db2.empty_string_pos then ""
  else get_field_data db2 pos f "data"
;

value eq_istr i1 i2 =
  match (i1, i2) with
  [ (Istr i1, Istr i2) -> Adef.int_of_istr i1 = Adef.int_of_istr i2
  | (Istr2 _ (f11, f12) i1, Istr2 _ (f21, f22) i2) ->
      i1 = i2 && f11 = f21 && f12 = f22
  | (Istr2New _ s1, Istr2New _ s2) -> s1 = s2
  | (Istr2 db2 f pos, Istr2New _ s2) -> False (*string_of_istr2 db2 f pos = s2*)
  | (Istr2New _ s1, Istr2 db2 f pos) -> False (*s1 = string_of_istr2 db2 f pos*)
  | _ -> failwith "eq_istr" ]
;

value make_istr2 db2 path i = Istr2 db2 path (get_field_acc db2 i path);

value is_empty_string =
  fun
  [ Istr istr -> Adef.int_of_istr istr = 0
  | Istr2 db2 path pos -> pos = Db2.empty_string_pos
  | Istr2New db2 s -> s = "" ]
;
value is_quest_string =
  fun
  [ Istr istr -> Adef.int_of_istr istr = 1
  | Istr2 db2 path pos -> failwith "not impl is_quest_string"
  | Istr2New db2 s -> s = "?" ]
;

value un_istr =
  fun
  [ Istr i -> i
  | Istr2 _ _ i -> failwith "un_istr"
  | Istr2New _ _ -> failwith "un_istr" ]
;

value un_istr2 =
  fun
  [ Istr _ -> failwith "un_istr2 1"
  | Istr2 _ _ _ -> failwith "un_istr2 2"
  | Istr2New _ s -> s ]
;

value no_consang = Adef.fix (-1);

value empty_person empty_string ip =
  {first_name = empty_string; surname = empty_string; occ = 0;
   image = empty_string; first_names_aliases = []; surnames_aliases = [];
   public_name = empty_string; qualifiers = []; titles = []; rparents = [];
   related = []; aliases = []; occupation = empty_string; sex = Neuter;
   access = Private; birth = Adef.codate_None; birth_place = empty_string;
   birth_src = empty_string; baptism = Adef.codate_None;
   baptism_place = empty_string; baptism_src = empty_string;
   death = DontKnowIfDead; death_place = empty_string;
   death_src = empty_string; burial = UnknownBurial;
   burial_place = empty_string; burial_src = empty_string;
   notes = empty_string; psources = empty_string; key_index = ip}
;

value get_family =
  fun
  [ Union u -> u.Def.family
  | Union2 db2 i ->
      match db2.family_array with
      [ Some tab -> tab.(i)
      | None -> get_field db2 i ("person", "family") ]
  | Union2Gen db2 u -> u.Def.family ]
;

value gen_union_of_union =
  fun
  [ Union u -> u
  | Union2 _ _ as u -> {family = get_family u}
  | Union2Gen db2 u -> u ]
;

value get_comment =
  fun
  [ Family f -> Istr f.Def.comment
  | Family2 db2 i -> make_istr2 db2 ("family", "comment") i
  | Family2Gen db2 f -> Istr2New db2 f.Def.comment ]
;
value get_divorce =
  fun
  [ Family f -> f.Def.divorce
  | Family2 db2 i -> get_field db2 i ("family", "divorce")
  | Family2Gen db2 f -> f.Def.divorce ]
;

value get_fsources =
  fun
  [ Family f -> Istr f.Def.fsources
  | Family2 db2 i -> make_istr2 db2 ("family", "fsources") i
  | Family2Gen db2 f -> Istr2New db2 f.Def.fsources ]
;
value get_marriage =
  fun
  [ Family f -> f.Def.marriage
  | Family2 db2 i -> get_field db2 i ("family", "marriage")
  | Family2Gen db2 f -> f.Def.marriage ]
;
value get_marriage_place =
  fun
  [ Family f -> Istr f.Def.marriage_place
  | Family2 db2 i -> make_istr2 db2 ("family", "marriage_place") i
  | Family2Gen db2 f -> Istr2New db2 f.Def.marriage_place ]
;
value get_marriage_src =
  fun
  [ Family f -> Istr f.Def.marriage_src
  | Family2 db2 i -> make_istr2 db2 ("family", "marriage_src") i
  | Family2Gen db2 f -> Istr2New db2 f.Def.marriage_src ]
;
value get_origin_file =
  fun
  [ Family f -> Istr f.Def.origin_file
  | Family2 db2 i -> make_istr2 db2 ("family", "origin_file") i
  | Family2Gen db2 f -> Istr2New db2 f.Def.origin_file ]
;
value get_relation =
  fun
  [ Family f -> f.Def.relation
  | Family2 db2 i -> get_field db2 i ("family", "relation")
  | Family2Gen db2 f -> f.Def.relation ]
;
value get_witnesses =
  fun
  [ Family f -> f.Def.witnesses
  | Family2 db2 i -> get_field db2 i ("family", "witnesses")
  | Family2Gen db2 f -> f.Def.witnesses ]
;

value family_with_origin_file f orf fi =
  match (f, orf) with
  [ (Family f, Istr orf) ->
      Family {(f) with origin_file = orf; fam_index = fi}
  | (Family2Gen db2 f, Istr2 _ path pos) ->
      let orf =
        if pos = -1 || pos = Db2.empty_string_pos then ""
        else get_field_data db2 pos path "data"
      in
      Family2Gen db2 {(f) with origin_file = orf; fam_index = fi}
  | (Family2Gen db2 f, Istr2New _ orf) ->
      Family2Gen db2 {(f) with origin_file = orf; fam_index = fi}
  | _ -> assert False ]
;

value gen_family_of_family =
  fun
  [ Family f -> map_family_ps (fun p -> p) (fun s -> Istr s) f
  | Family2 _ i as f ->
      {marriage = get_marriage f; marriage_place = get_marriage_place f;
       marriage_src = get_marriage_src f; witnesses = get_witnesses f;
       relation = get_relation f; divorce = get_divorce f;
       comment = get_comment f; origin_file = get_origin_file f;
       fsources = get_fsources f; fam_index = Adef.ifam_of_int i}
  | Family2Gen db2 f ->
      map_family_ps (fun p -> p) (fun s -> Istr2New db2 s) f ]
;

value get_father =
  fun
  [ Couple c -> Adef.father c
  | Couple2 db2 i ->
      match db2.father_array with
      [ Some tab -> tab.(i)
      | None -> get_field db2 i ("family", "father") ]
  | Couple2Gen db2 c -> Adef.father c ]
;

value get_mother =
  fun
  [ Couple c -> Adef.mother c
  | Couple2 db2 i ->
      match db2.mother_array with
      [ Some tab -> tab.(i)
      | None -> get_field db2 i ("family", "mother") ]
  | Couple2Gen db2 c -> Adef.mother c ]
;

value get_parent_array =
  fun
  [ Couple c -> Adef.parent_array c
  | Couple2 db2 i ->
      let p1 = get_field db2 i ("family", "father") in
      let p2 = get_field db2 i ("family", "mother") in
      [| p1; p2 |]
  | Couple2Gen db2 c -> Adef.parent_array c ]
;

value gen_couple_of_couple =
  fun
  [ Couple c -> c
  | Couple2 _ _ as c -> couple (get_father c) (get_mother c)
  | Couple2Gen db2 c -> c ]
;

value get_children =
  fun
  [ Descend d -> d.Def.children
  | Descend2 db2 i ->
      match db2.children_array with
      [ Some tab -> tab.(i)
      | None -> get_field db2 i ("family", "children") ]
  | Descend2Gen db2 d -> d.Def.children ]
;

value gen_descend_of_descend =
  fun
  [ Descend d -> d
  | Descend2 _ _ as d -> {children = get_children d}
  | Descend2Gen db2 d -> d ]
;

value sou2 i =
  match i with
  [ Istr2 db2 f pos -> string_of_istr2 db2 f pos
  | Istr2New db2 s -> s
  | _ -> assert False ]
;

value is_deleted_family =
  fun
  [ Family f -> f.Def.fam_index = Adef.ifam_of_int (-1)
  | Family2 _ i -> False (* not yet implemented *)
  | Family2Gen db2 f -> f.Def.fam_index = Adef.ifam_of_int (-1) ]
;

value ok_I_know = ref False;

type key =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

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
  find_in_bucket bl
};

value key_hashtbl_find dir file (fn, sn, oc) =
  let key = if oc = 0 then Key0 fn sn else Key fn sn oc in
  hashtbl_find dir file key
;

value person2_of_key db2 fn sn oc =
  let fn = Name.lower (nominative fn) in
  let sn = Name.lower (nominative sn) in
  try Some (Hashtbl.find db2.patches.h_key (fn, sn, oc)) with
  [ Not_found ->
      let person_of_key_d = Filename.concat db2.bdir "person_of_key" in
      try do {
        let ifn = hashtbl_find person_of_key_d "istr_of_string.ht" fn in
        let isn = hashtbl_find person_of_key_d "istr_of_string.ht" sn in
        let key = (ifn, isn, oc) in
        Some (key_hashtbl_find person_of_key_d "iper_of_key.ht" key : iper)
      }
      with
      [ Not_found -> None ] ]
;

value strings2_of_fsname db2 f s =
  let k = Name.crush_lower s in
  let dir = List.fold_left Filename.concat db2.bdir ["person"; f] in
  hashtbl_find_all dir "string_of_crush.ht" k
;

value persons2_of_name db2 s =
  let s = Name.crush_lower s in
  let dir = Filename.concat db2.bdir "person_of_name" in
  List.rev_append
    (try Hashtbl.find db2.patches.h_name s with [ Not_found -> [] ])
    (hashtbl_find_all dir "person_of_name.ht" s)
;

value start_with s p =
  String.length p < String.length s &&
  String.sub s 0 (String.length p) = p
;

value persons_of_first_name_or_surname2 db2 is_first_name = do {
  let f1 = "person" in
  let f2 = if is_first_name then "first_name" else "surname" in
  let fdir = List.fold_left Filename.concat db2.bdir [f1; f2] in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let ic = open_in_bin index_ini_fname in
  let iofc : list (string * int) = input_value ic in
  close_in ic;
  Spi2 db2
    {is_first_name = is_first_name; index_of_first_char = iofc; ini = "";
     curr = 0}
};

value spi_first spi s =
  match spi with
  [ Spi spi -> Istr (spi.cursor s)
  | Spi2 db2 spi -> do {
      let i =
        (* to be faster, go directly to the first string starting with
           the same char *)
        if s = "" then 0
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
                  then i1
                  else loop list
            | [] -> raise Not_found ]
      in
      let f1 = "person" in
      let f2 = if spi.is_first_name then "first_name" else "surname" in
      let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (4 * i) in
      let pos = input_binary_int ic in
      let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos in
      let (pos, i) =
        try
          loop i where rec loop i =
            let (s1, pos) : (string * int) = Iovalue.input ic in
            if start_with s1 s then (pos, i) else loop (i + 1)
        with
        [ End_of_file -> raise Not_found ]
      in
      spi.ini := s;
      spi.curr := i;
      Istr2 db2 (f1, f2) pos
    } ]
;

value spi_next spi istr need_whole_list =
  match (spi, istr) with
  [ (Spi spi, Istr s) -> (Istr (spi.next s), 1)
  | (Spi2 db2 spi, Istr2 _ (f1, f2) _) ->
      let i =
        if spi.ini = "" && not need_whole_list then
          loop spi.index_of_first_char where rec loop =
            fun
            [ [(_, i1) :: ([(_, i2) :: _] as list)] ->
                if spi.curr = i1 then i2 else loop list
            | [] | [_] -> raise Not_found ]
        else spi.curr + 1
      in
      try do {
        let ic =
          if i = spi.curr + 1 then
            Hashtbl.find db2.cache_chan (f1, f2, "index.dat")
          else
            let ic =
              fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (i * 4)
            in
            let pos = input_binary_int ic in
            fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos
        in
        let (s, pos) : (string * int) = Iovalue.input ic in
        let dlen = i - spi.curr in
        spi.curr := i;
        (Istr2 db2 (f1, f2) pos, dlen)
      }
      with
      [ End_of_file -> raise Not_found ]
  | _ -> failwith "not impl spi_next" ]
;

value spi_find spi s =
  match (spi, s) with
  [ (Spi spi, Istr s) -> spi.find s
  | (Spi2 _ _, Istr2 db2 (f1, f2) pos) -> do {
      let dir = List.fold_left Filename.concat db2.bdir [f1; f2] in
      hashtbl_find_all dir "person_of_string.ht" pos
    }
  | (Spi2 _ spi, Istr2New db2 s) ->
      let proj =
        if spi.is_first_name then fun p -> p.first_name
        else fun p -> p.surname
      in
      Hashtbl.fold
        (fun _ iper iperl ->
           try
             let p = Hashtbl.find db2.patches.h_person iper in
             if proj p = s then [iper :: iperl] else iperl
           with
           [ Not_found -> iperl ])
        db2.patches.h_key []
  | _ -> failwith "not impl spi_find" ]
;

value load_array2 bdir nb_ini nb f1 f2 get =
  if nb = 0 then [| |]
  else do {
    let ic_acc =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "access"])
    in
    let ic_dat =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "data"])
    in
    let tab = Array.create nb (get ic_dat (input_binary_int ic_acc)) in
    for i = 1 to nb_ini - 1 do {
      tab.(i) := get ic_dat (input_binary_int ic_acc);
    };
    close_in ic_dat;
    close_in ic_acc;
    tab
  }
;

value parents_array2 db2 nb_ini nb = do {
  let arr =
    load_array2 db2.bdir nb_ini nb "person" "parents"
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

value consang_array2 db2 nb =
  let cg_fname =
    List.fold_left Filename.concat db2.bdir ["person"; "consang"; "data"]
  in
  match try Some (open_in_bin cg_fname) with [ Sys_error _ -> None ] with
  [ Some ic -> do {
      let tab = input_value ic in
      close_in ic;
      tab
    }
  | None -> Array.make nb no_consang ]
;

value family_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir ["person"; "family"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  tab
};

value children_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir ["family"; "children"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  tab
};

value read_notes bname fnotes rn_mode =
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  let fname = Filename.concat "base_d" fname in
  match
    try Some (Secure.open_in (Filename.concat bname fname)) with
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
  let b = String.create (String.length magic) in
  really_input ic b 0 (String.length b);
  if b <> magic then failwith (sprintf "bad %s magic number" id)
  else ();
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
  {bdir = bdir; cache_chan = Hashtbl.create 1; patches = patches;
   parents_array = None; consang_array = None; family_array = None;
   father_array = None; mother_array = None; children_array = None;
   phony () = ()}
;

(* Implementation by emulated objects *)
(* This code works only with camlp4s *)

(*

#load "pa_pragma.cmo";
#load "pa_extend.cmo";
#load "q_MLast.cmo";

#pragma
  EXTEND
    GLOBAL: expr str_item;
    expr: LEVEL "apply"
      [ [ "heriter"; c = LIDENT; ":"; n = LIDENT ->
            <:expr< $uid:"C_" ^ c$ . $lid:n$ >> ] ]
    ;
    str_item:
      [ [ "classe"; "virtuelle"; n = LIDENT;
          rtpl = parametres_de_type_de_classe;
          (so, ldel) = fonction_objet_virtuel ->
            let td =
              let ldl =
                List.fold_right
                  (fun (loc, n, t_o, _) ldl ->
                     if t_o = None then ldl
                     else [(loc, n, False, match_with_some t_o) :: ldl])
                  ldel []
              in
              <:str_item< type $n$ $list:rtpl$ = { $list:ldl$ } >>
            in
            let lel =
              List.fold_right
                (fun (loc, n, _, eo) lel ->
                   if eo = None then lel
                   else [(loc, n, match_with_some eo) :: lel])
                ldel []
            in
            if lel = [] then td
            else
              let stl =
                List.map
                  (fun (loc, lab, (e, teo)) ->
                     if teo = None then
                       <:str_item< value $lid:lab$ = $e$ >>
                     else
                       let e =
                         if so = None then e
                         else
                           let s = match_with_some so in
                           <:expr< fun $lid:s$ -> $e$ >>
                       in
                       <:str_item< value $lid:lab$ = $e$ >>)
                  lel
              in
              let sgl =
                List.fold_right
                  (fun (loc, lab, (e, teo)) sgl ->
                     if teo = None then sgl
                     else
                       let sg =
                         let te = match_with_some teo in
                         let t =
                          if so = None then te else <:ctyp< $lid:n$ -> $te$ >>
                         in
                         <:sig_item< value $lid:lab$ : $t$ >>
                       in
                       [sg :: sgl])
                  lel []
              in
              let md =
                <:str_item<
                  module $uid:"C_" ^ n$ : sig $list:sgl$ end =
                    struct $list:stl$ end >>
              in
              <:str_item< declare $td$; $md$; end >>
        | "classe"; n = LIDENT; e = fonction_objet ->
            <:str_item< value $lid:n$ = $e$ >> ] ]
    ;
    parametres_de_type_de_classe:
      [ [ -> []
        | tpl = parametres_de_type_de_classe; "'"; tp = LIDENT ->
            [(tp, (False, False)) :: tpl] ] ]
    ;
    fonction_objet_virtuel:
      [ [ "="; "objet"; "("; soi = LIDENT; ")";
          ldel = LIST0 champ_d_objet_virtuel; "fin" ->
            (Some soi, ldel)
        | "="; "objet"; ldel = LIST0 champ_d_objet_virtuel; "fin" ->
            (None, ldel) ] ]
    ;
    fonction_objet:
      [ [ p = LIDENT; e = fonction_objet -> <:expr< fun $lid:p$ -> $e$ >>
        | "="; "objet"; "("; soi = LIDENT; ")"; lel = LIST0 champ_d_objet;
          "fin" ->
            let pel =
              List.fold_right
                (fun (p, priv, e) lel ->
                   if priv then lel else [(p, e) :: lel])
                lel []
            in
            let priv_pel =
              List.fold_right
                (fun (p, priv, e) ppel ->
                   if priv then [(p, e) :: ppel] else ppel)
                lel []
            in
            let e =
              <:expr< let rec $lid:soi$ = { $list:pel$ } in $lid:soi$ >>
            in
            if priv_pel = [] then e else <:expr< let $list:priv_pel$ in $e$ >>
        | "="; "objet"; lel = LIST0 champ_d_objet; "fin" ->
            let pel =
              List.fold_right
                (fun (p, priv, e) lel ->
                   if priv then lel else [(p, e) :: lel])
                lel []
            in
            <:expr< { $list:pel$ } >> ] ]
    ;
    champ_d_objet_virtuel:
      [ [ "methode"; "privee"; n = LIDENT; e = fonction_methode; ";" ->
            (loc, n, None, Some (e, None))
        | "methode"; n = LIDENT; pl = LIST0 LIDENT; ":"; t = ctyp;
          eo = OPT fonction_methode_virtuelle; ";" ->
            let eto =
              if eo = None then None
              else
                let e =
                  List.fold_right (fun p e -> <:expr< fun $lid:p$ -> $e$ >>)
                    pl (match_with_some eo)
                in
                Some (e, Some t)
            in
            (loc, n, Some t, eto) ] ]
    ;
    fonction_methode_virtuelle:
      [ [ "="; e = expr -> e ] ]
    ;
    champ_d_objet:
      [ [ "methode"; "privee"; n = LIDENT; e = fonction_methode; ";" ->
            (<:patt< $lid:n$ >>, True, e)
        | "methode"; n = LIDENT; e = fonction_methode; ";" ->
            (<:patt< $lid:n$ >>, False, e) ] ]
    ;
    fonction_methode:
      [ [ p = ipatt; e = fonction_methode -> <:expr< fun $p$ -> $e$ >>
        | "="; e = expr -> e ] ]
    ;
    ipatt:
      [ [ s = LIDENT -> <:patt< $lid:s$ >>
        | "_" -> <:patt< _ >>
        | "("; ")" -> <:patt< () >>
        | "("; p = ipatt; ","; pl = LIST1 ipatt SEP ","; ")" ->
            <:patt< ( $list:[p :: pl]$ ) >> ] ]
    ;
  END;

type person =
  [ Person of dsk_person
  | Person2 of db2 and int
  | Person2Gen of db2 and gen_person iper string ]
;

classe virtuelle person_fun 'a =
  objet
    methode get_access : 'a -> access;
    methode get_aliases : 'a -> list istr;
    methode get_baptism : 'a -> codate;
    methode get_baptism_place : 'a -> istr;
    methode get_baptism_src : 'a -> istr;
    methode get_birth : 'a -> codate;
    methode get_birth_place : 'a -> istr;
    methode get_birth_src : 'a -> istr;
    methode get_burial : 'a -> Def.burial;
    methode get_burial_place : 'a -> istr;
    methode get_burial_src : 'a -> istr;
    methode get_death : 'a -> Def.death;
    methode get_death_place : 'a -> istr;
    methode get_death_src : 'a -> istr;
    methode get_first_name : 'a -> istr;
    methode get_first_names_aliases : 'a -> list istr;
    methode get_image : 'a -> istr;
    methode get_key_index : 'a -> iper;
    methode get_notes : 'a -> istr;
    methode get_occ : 'a -> int;
    methode get_occupation : 'a -> istr;
    methode get_psources : 'a -> istr;
    methode get_public_name : 'a -> istr;
    methode get_qualifiers : 'a -> list istr;
    methode get_related : 'a -> list iper;
    methode get_rparents : 'a -> list relation;
    methode get_sex : 'a -> Def.sex;
    methode get_surname : 'a -> istr;
    methode get_surnames_aliases : 'a -> list istr;
    methode get_titles : 'a -> list title;
    methode person_with_key : 'a -> istr -> istr -> int -> person;
    methode person_with_related : 'a -> list iper -> person;
    methode person_with_rparents : 'a -> list relation -> person;
    methode person_with_sex : 'a -> Def.sex -> person;
    methode gen_person_of_person : 'a -> Def.gen_person iper istr;
    methode dsk_person_of_person : 'a -> Dbdisk.dsk_person;
  fin
;

classe person1_fun =
  objet
    methode get_access p = p.Def.access;
    methode get_aliases p = List.map (fun i -> Istr i) p.Def.aliases;
    methode get_baptism p = p.Def.baptism;
    methode get_baptism_place p = Istr p.Def.baptism_place;
    methode get_baptism_src p = Istr p.Def.baptism_src;
    methode get_birth p = p.Def.birth;
    methode get_birth_place p = Istr p.Def.birth_place;
    methode get_birth_src p = Istr p.Def.birth_src;
    methode get_burial p = p.Def.burial;
    methode get_burial_place p = Istr p.Def.burial_place;
    methode get_burial_src p = Istr p.Def.burial_src;
    methode get_death p = p.Def.death;
    methode get_death_place p = Istr p.Def.death_place;
    methode get_death_src p = Istr p.Def.death_src;
    methode get_first_name p = Istr p.Def.first_name;
    methode get_first_names_aliases p =
      List.map (fun i -> Istr i) p.Def.first_names_aliases;
    methode get_image p = Istr p.Def.image;
    methode get_key_index p = p.Def.key_index;
    methode get_notes p = Istr p.Def.notes;
    methode get_occ p = p.Def.occ;
    methode get_occupation p = Istr p.Def.occupation;
    methode get_psources p = Istr p.Def.psources;
    methode get_public_name p = Istr p.Def.public_name;
    methode get_qualifiers p = List.map (fun i -> Istr i) p.Def.qualifiers;
    methode get_related p = p.Def.related;
    methode get_rparents p =
      List.map (map_relation_ps (fun x -> x) (fun i -> Istr i))
        p.Def.rparents;
    methode get_sex p = p.Def.sex;
    methode get_surname p = Istr p.Def.surname;
    methode get_surnames_aliases p =
      List.map (fun i -> Istr i) p.Def.surnames_aliases;
    methode get_titles p =
      List.map (fun t -> map_title_strings (fun i -> Istr i) t) p.Def.titles;
    methode person_with_key p fn sn oc =
      match (fn, sn) with
      [ (Istr fn, Istr sn) ->
          Person {(p) with first_name = fn; surname = sn; occ = oc}
      | _ -> assert False ]
    ;
    methode person_with_related p r = Person {(p) with related = r};
    methode person_with_rparents p r =
      let r = List.map (map_relation_ps (fun p -> p) un_istr) r in
      Person {(p) with rparents = r}
    ;
    methode person_with_sex p s = Person {(p) with sex = s};
    methode gen_person_of_person p =
      map_person_ps (fun p -> p) (fun s -> Istr s) p;
    methode dsk_person_of_person p = p;
  fin
;

classe person2_fun =
  objet (self)
    methode get_access (db2, i) = get_field db2 i ("person", "access");
    methode get_aliases (db2, i) =
      let pos = get_field_acc db2 i ("person", "aliases") in
      if pos = -1 then []
      else
        let list = get_field_data db2 pos ("person", "aliases") "data2.ext" in
        List.map (fun pos -> Istr2 db2 ("person", "aliases") pos) list;
    methode get_baptism (db2, i) = get_field db2 i ("person", "baptism");
    methode get_baptism_place (db2, i) =
      make_istr2 db2 ("person", "baptism_place") i;
    methode get_baptism_src (db2, i) =
      make_istr2 db2 ("person", "baptism_src") i;
    methode get_birth (db2, i) = get_field db2 i ("person", "birth");
    methode get_birth_place (db2, i) =
      make_istr2 db2 ("person", "birth_place") i;
    methode get_birth_src (db2, i) =
      make_istr2 db2 ("person", "birth_src") i;
    methode get_burial (db2, i) = get_field db2 i ("person", "burial");
    methode get_burial_place (db2, i) =
      make_istr2 db2 ("person", "burial_place") i;
    methode get_burial_src (db2, i) =
      make_istr2 db2 ("person", "burial_src") i;
    methode get_death (db2, i) = get_field db2 i ("person", "death");
    methode get_death_place (db2, i) =
      make_istr2 db2 ("person", "death_place") i;
    methode get_death_src (db2, i) = make_istr2 db2 ("person", "death_src") i;
    methode get_first_name (db2, i) =
      make_istr2 db2 ("person", "first_name") i;
    methode get_first_names_aliases (db2, i) =
      let pos = get_field_acc db2 i ("person", "first_names_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "first_names_aliases") "data2.ext"
        in
        List.map (fun pos -> Istr2 db2 ("person", "first_names_aliases") pos)
          list
    ;
    methode get_image (db2, i) = make_istr2 db2 ("person", "image") i;
    methode get_key_index (db2, i) = Adef.iper_of_int i;
    methode get_notes (db2, i) = make_istr2 db2 ("person", "notes") i;
    methode get_occ (db2, i) = get_field db2 i ("person", "occ");
    methode get_occupation (db2, i) =
      make_istr2 db2 ("person", "occupation") i;
    methode get_psources (db2, i) =
      make_istr2 db2 ("person", "psources") i;
    methode get_public_name (db2, i) =
      make_istr2 db2 ("person", "public_name") i;
    methode get_qualifiers (db2, i) =
      let pos = get_field_acc db2 i ("person", "qualifiers") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "qualifiers") "data2.ext"
        in
        List.map (fun pos -> Istr2 db2 ("person", "qualifiers") pos) list
    ;
    methode get_related (db2, i) =
      let pos = get_field_acc db2 i ("person", "related") in
      loop [] pos where rec loop list pos =
        if pos = -1 then List.rev list
        else
          let (ip, pos) =
            get_field_2_data db2 pos ("person", "related") "data"
          in
          loop [ip :: list] pos
    ;
    methode get_rparents (db2, i) =
      let pos = get_field_acc db2 i ("person", "rparents") in
      if pos = -1 then []
      else
        let rl = get_field_data db2 pos ("person", "rparents") "data" in
        List.map
          (map_relation_ps (fun x -> x) (fun _ -> Istr2 db2 ("", "") (-1)))
          rl
    ;
    methode get_sex (db2, i) = get_field db2 i ("person", "sex");
    methode get_surname (db2, i) = make_istr2 db2 ("person", "surname") i;
    methode get_surnames_aliases (db2, i) =
      let pos = get_field_acc db2 i ("person", "surnames_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "surnames_aliases") "data2.ext"
        in
        List.map
          (fun pos -> Istr2 db2 ("person", "surnames_aliases") pos) list
    ;
    methode get_titles (db2, i) =
      let pos = get_field_acc db2 i ("person", "titles") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "titles") "data2.ext"
        in
        List.map
          (map_title_strings (fun pos -> Istr2 db2 ("person", "titles") pos))
          list
    ;
    methode person_with_key (db2, i) fn sn oc =
      match (fn, sn) with
      [ (Istr2 _ (f1, f2) ifn, Istr2 _ (f3, f4) isn) ->
          failwith "not impl person_with_key 1"
      | (Istr2New _ fn, Istr2New _ sn) ->
          let p = self.gen_person_of_person (db2, i) in
          let p = map_person_ps (fun ip -> ip) sou2 p in
          Person2Gen db2 {(p) with first_name = fn; surname = sn; occ = oc}
      | _ -> failwith "not impl person_with_key 2" ]
    ;
    methode person_with_related (db2, i) r =
      failwith "not impl person_with_related";
    methode person_with_rparents (db2, i) r =
      failwith "not impl person_with_rparents";
    methode person_with_sex (db2, i) s =
      failwith "not impl person_with_sex";
    methode gen_person_of_person pp =
      {first_name = self.get_first_name pp; surname = self.get_surname pp;
       occ = self.get_occ pp; image = self.get_image pp;
       public_name = self.get_public_name pp;
       qualifiers = self.get_qualifiers pp; aliases = self.get_aliases pp;
       first_names_aliases = self.get_first_names_aliases pp;
       surnames_aliases = self.get_surnames_aliases pp;
       titles = self.get_titles pp; rparents = self.get_rparents pp;
       related = self.get_related pp; occupation = self.get_occupation pp;
       sex = self.get_sex pp; access = self.get_access pp;
       birth = self.get_birth pp; birth_place = self.get_birth_place pp;
       birth_src = self.get_birth_src pp; baptism = self.get_baptism pp;
       baptism_place = self.get_baptism_place pp;
       baptism_src = self.get_baptism_src pp;
       death = self.get_death pp; death_place = self.get_death_place pp;
       death_src = self.get_death_src pp; burial = self.get_burial pp;
       burial_place = self.get_burial_place pp;
       burial_src = self.get_burial_src pp; notes = self.get_notes pp;
       psources = self.get_psources pp; key_index = self.get_key_index pp}
    ;
    methode dsk_person_of_person p =
      failwith "not impl dsk_person_of_person";
  fin
;

classe person2gen_fun =
  objet
    methode get_access (db2, p) = p.Def.access;
    methode get_aliases (db2, p) =
      List.map (fun s -> Istr2New db2 s) p.Def.aliases;
    methode get_baptism (db2, p) = p.Def.baptism;
    methode get_baptism_place (db2, p) = Istr2New db2 p.Def.baptism_place;
    methode get_baptism_src (db2, p) = Istr2New db2 p.Def.baptism_src;
    methode get_birth (db2, p) = p.Def.birth;
    methode get_birth_place (db2, p) = Istr2New db2 p.Def.birth_place;
    methode get_birth_src (db2, p) = Istr2New db2 p.Def.birth_src;
    methode get_burial (db2, p) = p.Def.burial;
    methode get_burial_place (db2, p) = Istr2New db2 p.Def.burial_place;
    methode get_burial_src (db2, p) = Istr2New db2 p.Def.burial_src;
    methode get_death (db2, p) = p.Def.death;
    methode get_death_place (db2, p) = Istr2New db2 p.Def.death_place;
    methode get_death_src (db2, p) = Istr2New db2 p.Def.death_src;
    methode get_first_name (db2, p) = Istr2New db2 p.Def.first_name;
    methode get_first_names_aliases (db2, p) =
      List.map (fun s -> Istr2New db2 s) p.Def.first_names_aliases;
    methode get_image (db2, p) = Istr2New db2 p.Def.image;
    methode get_key_index (db2, p) = p.Def.key_index;
    methode get_notes (db2, p) = Istr2New db2 p.Def.notes;
    methode get_occ (db2, p) = p.Def.occ;
    methode get_occupation (db2, p) = Istr2New db2 p.Def.occupation;
    methode get_psources (db2, p) = Istr2New db2 p.Def.psources;
    methode get_public_name (db2, p) = Istr2New db2 p.Def.public_name;
    methode get_qualifiers (db2, p) =
      List.map (fun s -> Istr2New db2 s) p.Def.qualifiers;
    methode get_related (db2, p) = p.Def.related;
    methode get_rparents (db2, p) =
      List.map (map_relation_ps (fun x -> x) (fun s -> Istr2New db2 s))
        p.Def.rparents;
    methode get_sex (db2, p) = p.Def.sex;
    methode get_surname (db2, p) = Istr2New db2 p.Def.surname;
    methode get_surnames_aliases (db2, p) =
      List.map (fun s -> Istr2New db2 s) p.Def.surnames_aliases;
    methode get_titles (db2, p) =
      List.map (fun t -> map_title_strings (fun s -> Istr2New db2 s) t)
        p.Def.titles
    ;
    methode person_with_key (db2, p) fn sn oc =
      match (fn, sn) with
      [ (Istr2New _ fn, Istr2New _ sn) ->
          Person2Gen db2 {(p) with first_name = fn; surname = sn; occ = oc}
      | _ -> failwith "not impl person_with_key 3" ]
    ;
    methode person_with_related (db2, p) r =
      failwith "not impl person_with_related (gen)";
    methode person_with_rparents (db2, p) r =
      failwith "not impl person_with_rparents (gen)";
    methode person_with_sex (db2, p) s = Person2Gen db2 {(p) with sex = s};
    methode gen_person_of_person (db2, p) =
      map_person_ps (fun p -> p) (fun s -> Istr2New db2 s) p;
    methode dsk_person_of_person (db2, p) =
      failwith "not impl dsk_person_of_person (gen)";
  fin
;

value wrap_per f g h =
  fun
  [ Person p -> f person1_fun p
  | Person2 db2 i -> g person2_fun (db2, i)
  | Person2Gen db2 p -> h person2gen_fun (db2, p) ]
;

value get_access = let f pf = pf.get_access in wrap_per f f f;
value get_aliases = let f pf = pf.get_aliases in wrap_per f f f;
value get_baptism = let f pf = pf.get_baptism in wrap_per f f f;
value get_baptism_place = let f pf = pf.get_baptism_place in wrap_per f f f;
value get_baptism_src = let f pf = pf.get_baptism_src in wrap_per f f f;
value get_birth = let f pf = pf.get_birth in wrap_per f f f;
value get_birth_place = let f pf = pf.get_birth_place in wrap_per f f f;
value get_birth_src = let f pf = pf.get_birth_src in wrap_per f f f;
value get_burial = let f pf = pf.get_burial in wrap_per f f f;
value get_burial_place = let f pf = pf.get_burial_place in wrap_per f f f;
value get_burial_src = let f pf = pf.get_burial_src in wrap_per f f f;
value get_death = let f pf = pf.get_death in wrap_per f f f;
value get_death_place = let f pf = pf.get_death_place in wrap_per f f f;
value get_death_src = let f pf = pf.get_death_src in wrap_per f f f;
value get_first_name = let f pf = pf.get_first_name in wrap_per f f f;
value get_first_names_aliases =
  let f pf = pf.get_first_names_aliases in
  wrap_per f f f
;
value get_image = let f pf = pf.get_image in wrap_per f f f;
value get_key_index = let f pf = pf.get_key_index in wrap_per f f f;
value get_notes = let f pf = pf.get_notes in wrap_per f f f;
value get_occ = let f pf = pf.get_occ in wrap_per f f f;
value get_occupation = let f pf = pf.get_occupation in wrap_per f f f;
value get_psources = let f pf = pf.get_psources in wrap_per f f f;
value get_public_name = let f pf = pf.get_public_name in wrap_per f f f;
value get_qualifiers = let f pf = pf.get_qualifiers in wrap_per f f f;
value get_related = let f pf = pf.get_related in wrap_per f f f;
value get_rparents = let f pf = pf.get_rparents in wrap_per f f f;
value get_sex = let f pf = pf.get_sex in wrap_per f f f;
value get_surname = let f pf = pf.get_surname in wrap_per f f f;
value get_surnames_aliases =
  let f pf = pf.get_surnames_aliases in
  wrap_per f f f
;
value get_titles = let f pf = pf.get_titles in wrap_per f f f;

value person_with_key = let f pf = pf.person_with_key in wrap_per f f f;
value person_with_related =
  let f pf = pf.person_with_related in
  wrap_per f f f
;
value person_with_rparents =
  let f pf = pf.person_with_rparents in
  wrap_per f f f
;
value person_with_sex = let f pf = pf.person_with_sex in wrap_per f f f;
value gen_person_of_person =
  let f pf = pf.gen_person_of_person in
  wrap_per f f f
;
value dsk_person_of_person =
  let f pf = pf.dsk_person_of_person in
  wrap_per f f f
;

classe virtuelle ascend_fun 'a =
  objet
    methode get_consang : 'a -> Adef.fix;
    methode get_parents : 'a -> option ifam;
  fin
;

classe ascend1_fun =
  objet
    methode get_consang a = a.Def.consang;
    methode get_parents a = a.Def.parents;
  fin
;

classe ascend2_fun =
  objet
    methode get_consang (db2, i) =
      match db2.consang_array with
      [ Some tab -> tab.(i)
      | None ->
          try get_field db2 i ("person", "consang") with
          [ Sys_error _ -> no_consang ] ]
    ;
    methode get_parents (db2, i) =
      match db2.parents_array with
      [ Some tab -> tab.(i)
      | None ->
          let pos = get_field_acc db2 i ("person", "parents") in
          if pos = -1 then None
          else Some (get_field_data db2 pos ("person", "parents") "data") ]
    ;
  fin
;

classe ascend2gen_fun =
  objet
    methode get_consang (db2, a) = a.Def.consang;
    methode get_parents (db2, a) = a.Def.parents;
  fin
;

type ascend =
  [ Ascend of dsk_ascend
  | Ascend2 of db2 and int
  | Ascend2Gen of db2 and gen_ascend ifam ]
;

value wrap_asc f g h =
  fun
  [ Ascend a -> f ascend1_fun a
  | Ascend2 db2 i -> g ascend2_fun (db2, i)
  | Ascend2Gen db2 a -> h ascend2gen_fun (db2, a) ]
;

value get_consang = let f pf = pf.get_consang in wrap_asc f f f;
value get_parents = let f pf = pf.get_parents in wrap_asc f f f;

classe virtuelle base =
  objet (self)
    methode privee husbands self p =
      let u = self.uoi (get_key_index p) in
      List.map
        (fun ifam ->
           let cpl = self.coi ifam in
           let husband = self.poi (get_father cpl) in
           let husband_surname = self.p_surname husband in
           let husband_surnames_aliases =
             List.map self.sou (get_surnames_aliases husband)
           in
           (husband_surname, husband_surnames_aliases))
        (Array.to_list (get_family u))
    ;
    methode privee father_titles_places self p nobtit =
      match get_parents (self.aoi (get_key_index p)) with
      [ Some ifam ->
          let cpl = self.coi ifam in
          let fath = self.poi (get_father cpl) in
          List.map (fun t -> self.sou t.t_place) (nobtit fath)
      | None -> [] ]
    ;
    methode close_base : unit -> unit;
    methode empty_person : iper -> person;
    methode person_of_gen_person : Def.gen_person iper istr -> person;
    methode ascend_of_gen_ascend : Def.gen_ascend ifam -> ascend;
    methode union_of_gen_union : Def.gen_union ifam -> union;
    methode family_of_gen_family : Def.gen_family iper istr -> family;
    methode couple_of_gen_couple : Def.gen_couple iper -> couple;
    methode descend_of_gen_descend : Def.gen_descend iper -> descend;
    methode poi : iper -> person;
    methode aoi : iper -> ascend;
    methode uoi : iper -> union;
    methode foi : ifam -> family;
    methode coi : ifam -> couple;
    methode doi : ifam -> descend;
    methode sou : istr -> string;
    methode nb_of_persons : unit -> int;
    methode nb_of_families : unit -> int;
    methode patch_person : iper -> person -> unit;
    methode patch_ascend : iper -> ascend -> unit;
    methode patch_union : iper -> union -> unit;
    methode patch_family : ifam -> family -> unit;
    methode patch_descend : ifam -> descend -> unit;
    methode patch_couple : ifam -> couple -> unit;
    methode patch_key : iper -> string -> string -> int -> unit;
    methode patch_name : string -> iper -> unit;
    methode insert_string : string -> istr;
    methode commit_patches : unit -> unit;
    methode commit_notes : string -> string -> unit;
    methode is_patched_person : iper -> bool;
    methode patched_ascends : unit -> list iper;
    methode output_consang_tab : array Adef.fix -> unit;
    methode delete_family ifam : ifam -> unit = do {
      let cpl =
        self.couple_of_gen_couple
          (couple (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)))
      in
      let fam =
        let empty = self.insert_string "" in
        self.family_of_gen_family
          {marriage = codate_None; marriage_place = empty; marriage_src = empty;
           relation = Married; divorce = NotDivorced; witnesses = [| |];
           comment = empty; origin_file = empty; fsources = empty;
           fam_index = Adef.ifam_of_int (-1)}
      in
      let des = self.descend_of_gen_descend {children = [| |]} in
      self.patch_family ifam fam;
      self.patch_couple ifam cpl;
      self.patch_descend ifam des
    };
    methode person_of_key : string -> string -> int -> option iper;
    methode persons_of_name : string -> list iper;
    methode persons_of_first_name : unit -> string_person_index;
    methode persons_of_surname : unit -> string_person_index;
    methode base_visible_get : (person -> bool) -> int -> bool;
    methode base_visible_write : unit -> unit;
    methode base_particles : unit -> list string;
    methode base_strings_of_first_name s : string -> list istr;
    methode base_strings_of_surname s : string -> list istr;
    methode load_ascends_array : unit -> unit;
    methode load_unions_array : unit -> unit;
    methode load_couples_array : unit -> unit;
    methode load_descends_array : unit -> unit;
    methode load_strings_array : unit -> unit;
    methode persons_array : unit -> (int -> person * int -> person -> unit);
    methode ascends_array :
      unit ->
        (int -> option ifam * int -> Adef.fix * int -> Adef.fix -> unit *
         option (array Adef.fix));
    methode base_notes_read : string -> string;
    methode base_notes_read_first_line : string -> string;
    methode base_notes_are_empty : string -> bool;
    methode base_notes_origin_file : unit -> string;
    methode base_notes_dir : unit -> string;
    methode base_wiznotes_dir : unit -> string;
    methode person_misc_names p tit :
      person -> (person -> list title) -> list string
    =
      let sou = self.sou in
      Futil.gen_person_misc_names (sou (get_first_name p))
        (sou (get_surname p)) (sou (get_public_name p))
        (List.map sou (get_qualifiers p)) (List.map sou (get_aliases p))
        (List.map sou (get_first_names_aliases p))
        (List.map sou (get_surnames_aliases p))
        (List.map (Futil.map_title_strings sou) (tit p))
        (if get_sex p = Female then husbands self p else [])
        (father_titles_places self p tit)
    ;
    methode nobtit conf p : config -> person -> list title =
      let list = get_titles p in
      match Lazy.force conf.allowed_titles with
      [ [] -> list
      | allowed_titles ->
          let list =
            List.fold_right
              (fun t l ->
                 let id = Name.lower (self.sou t.t_ident) in
                 let pl = Name.lower (self.sou t.t_place) in
                 if pl = "" then
                   if List.mem id allowed_titles then [t :: l] else l
                 else if
                   List.mem (id ^ "/" ^ pl) allowed_titles ||
                   List.mem (id ^ "/*") allowed_titles
                 then
                   [t :: l]
                 else l)
              list []
          in
          match Lazy.force conf.denied_titles with
          [ [] -> list
          | denied_titles ->
              List.filter
                (fun t ->
                   let id = Name.lower (self.sou t.t_ident) in
                   let pl = Name.lower (self.sou t.t_place) in
                   if List.mem (id ^ "/" ^ pl) denied_titles ||
                      List.mem ("*/" ^ pl) denied_titles
                   then False
                   else True)
                list ] ]
    ;
    methode p_first_name p : person -> string =
      nominative (self.sou (get_first_name p));
    methode p_surname p : person -> string =
      nominative (self.sou (get_surname p));
    methode date_of_last_change : string -> float;
    methode apply_as_dsk_base : (Dbdisk.dsk_base -> unit) -> unit;
  fin
;

classe base1 base =
  objet (self)
    methode privee base_strings_of_first_name_or_surname s =
      List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
    ;
    methode close_base = base.func.cleanup;
    methode empty_person ip = Person (empty_person (Adef.istr_of_int 0) ip);
    methode person_of_gen_person p =
      Person (map_person_ps (fun p -> p) un_istr p);
    methode ascend_of_gen_ascend a =
      Ascend a;
    methode union_of_gen_union u =
      Union u;
    methode family_of_gen_family f =
      Family (map_family_ps (fun p -> p) un_istr f);
    methode couple_of_gen_couple c =
      Couple c;
    methode descend_of_gen_descend d =
      Descend d;
    methode poi i = Person (base.data.persons.get (Adef.int_of_iper i));
    methode aoi i = Ascend (base.data.ascends.get (Adef.int_of_iper i));
    methode uoi i = Union (base.data.unions.get (Adef.int_of_iper i));
    methode foi i = Family (base.data.families.get (Adef.int_of_ifam i));
    methode coi i = Couple (base.data.couples.get (Adef.int_of_ifam i));
    methode doi i = Descend (base.data.descends.get (Adef.int_of_ifam i));
    methode sou i =
      match i with
      [ Istr i -> base.data.strings.get (Adef.int_of_istr i)
      | _ -> assert False ]
    ;
    methode nb_of_persons () = base.data.persons.len;
    methode nb_of_families () = base.data.families.len;
    methode patch_person ip p =
      match p with
      [ Person p -> base.func.Dbdisk.patch_person ip p
      | _ -> assert False ]
    ;
    methode patch_ascend ip a =
      match a with
      [ Ascend a -> base.func.Dbdisk.patch_ascend ip a
      | _ -> assert False ]
    ;
    methode patch_union ip u =
      match u with
      [ Union u -> base.func.Dbdisk.patch_union ip u
      | _ -> failwith "not impl patch_union" ]
    ;
    methode patch_family ifam f =
      match f with
      [ Family f -> base.func.Dbdisk.patch_family ifam f
      | _ -> failwith "not impl patch_family" ]
    ;
    methode patch_descend ifam d =
      match d with
      [ Descend d -> base.func.Dbdisk.patch_descend ifam d
      | _ -> failwith "not impl patch_descend" ]
    ;
    methode patch_couple ifam c =
      match c with
      [ Couple c -> base.func.Dbdisk.patch_couple ifam c
      | _ -> failwith "not impl patch_couple" ]
    ;
    methode patch_key ip fn sn occ = ();
    methode patch_name s ip = base.func.Dbdisk.patch_name s ip;
    methode insert_string s = Istr (base.func.Dbdisk.insert_string s);
    methode commit_patches = base.func.Dbdisk.commit_patches;
    methode commit_notes = base.func.Dbdisk.commit_notes;
    methode is_patched_person ip = base.func.Dbdisk.is_patched_person ip;
    methode patched_ascends = base.func.Dbdisk.patched_ascends;
    methode output_consang_tab tab = do {
      eprintf "error Gwdb.output_consang_tab\n";
      flush stdout
    };
    methode delete_family ifam = heriter base : delete_family self ifam;
    methode person_of_key = base.func.Dbdisk.person_of_key;
    methode persons_of_name = base.func.Dbdisk.persons_of_name;
    methode persons_of_first_name () =
      Spi base.func.Dbdisk.persons_of_first_name;
    methode persons_of_surname () =
      Spi base.func.Dbdisk.persons_of_surname;
    methode base_visible_get f =
      base.data.visible.v_get (fun p -> f (Person p));
    methode base_visible_write = base.data.visible.v_write;
    methode base_particles () = base.data.particles;
    methode base_strings_of_first_name =
      base_strings_of_first_name_or_surname
    ;
    methode base_strings_of_surname =
      base_strings_of_first_name_or_surname
    ;
    methode load_ascends_array = base.data.ascends.load_array;
    methode load_unions_array = base.data.unions.load_array;
    methode load_couples_array = base.data.couples.load_array;
    methode load_descends_array = base.data.descends.load_array;
    methode load_strings_array = base.data.strings.load_array;
    methode persons_array () =
      let get i = Person (base.data.persons.get i) in
      let set i =
        fun
        [ Person p -> base.data.persons.set i p
        | Person2 _ _ -> assert False
        | Person2Gen _ _ -> assert False ]
      in
      (get, set)
    ;
    methode ascends_array () =
      let fget i = (base.data.ascends.get i).parents in
      let cget i = (base.data.ascends.get i).consang in
      let cset i v =
        base.data.ascends.set i
          {(base.data.ascends.get i) with consang = v}
      in
      (fget, cget, cset, None)
    ;
    methode base_notes_read fnotes = base.data.bnotes.nread fnotes RnAll;
    methode base_notes_read_first_line fnotes =
      base.data.bnotes.nread fnotes Rn1Ln;
    methode base_notes_are_empty fnotes =
      base.data.bnotes.nread fnotes RnDeg = "";
    methode base_notes_origin_file () = base.data.bnotes.norigin_file;
    methode base_notes_dir () = "notes_d";
    methode base_wiznotes_dir () = "wiznotes";
    methode person_misc_names p tit =
      heriter base : person_misc_names self p tit;
    methode nobtit conf p = heriter base : nobtit self conf p;
    methode p_first_name p = heriter base : p_first_name self p;
    methode p_surname p = heriter base : p_surname self p;
    methode date_of_last_change bname =
      let bdir =
        if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
      in
      let s =
        try Unix.stat (Filename.concat bdir "patches") with
        [ Unix.Unix_error _ _ _ -> Unix.stat (Filename.concat bdir "base") ]
      in
      s.Unix.st_mtime
    ;
    methode apply_as_dsk_base f = f base;
  fin
;

classe base2 db2 =
  objet (self)
    methode privee base_strings_of_first_name_or_surname field proj s =
      let posl = strings2_of_fsname db2 field s in
      let istrl =
        List.map (fun pos -> Istr2 db2 ("person", field) pos) posl
      in
      let s = Name.crush_lower s in
      Hashtbl.fold
        (fun _ iper istrl ->
           try
             let p = Hashtbl.find db2.patches.h_person iper in
             if Name.crush_lower (proj p) = s then
               [Istr2New db2 (proj p) :: istrl]
             else istrl
           with
           [ Not_found -> istrl ])
        db2.patches.h_key istrl
    ;
    methode close_base () =
      Hashtbl.iter (fun (f1, f2, f) ic -> close_in ic) db2.cache_chan;
    methode empty_person ip = Person2Gen db2 (empty_person "" ip);
    methode person_of_gen_person p =
      Person2Gen db2 (map_person_ps (fun p -> p) un_istr2 p);
    methode ascend_of_gen_ascend a = Ascend2Gen db2 a;
    methode union_of_gen_union u = Union2Gen db2 u;
    methode family_of_gen_family f =
      Family2Gen db2 (map_family_ps (fun p -> p) un_istr2 f);
    methode couple_of_gen_couple c = Couple2Gen db2 c;
    methode descend_of_gen_descend d = Descend2Gen db2 d;
    methode poi i =
      try Person2Gen db2 (Hashtbl.find db2.patches.h_person i) with
      [ Not_found -> Person2 db2 (Adef.int_of_iper i) ]
    ;
    methode aoi i =
      try Ascend2Gen db2 (Hashtbl.find db2.patches.h_ascend i) with
      [ Not_found -> Ascend2 db2 (Adef.int_of_iper i) ]
    ;
    methode uoi i =
      try Union2Gen db2 (Hashtbl.find db2.patches.h_union i) with
      [ Not_found -> Union2 db2 (Adef.int_of_iper i) ]
    ;
    methode foi i =
      try Family2Gen db2 (Hashtbl.find db2.patches.h_family i) with
      [ Not_found -> Family2 db2 (Adef.int_of_ifam i) ]
    ;
    methode coi i =
      try Couple2Gen db2 (Hashtbl.find db2.patches.h_couple i) with
      [ Not_found -> Couple2 db2 (Adef.int_of_ifam i) ]
    ;
    methode doi i =
      try Descend2Gen db2 (Hashtbl.find db2.patches.h_descend i) with
      [ Not_found -> Descend2 db2 (Adef.int_of_ifam i) ]
    ;
    methode sou i =
      match i with
      [ Istr2 db2 f pos -> string_of_istr2 db2 f pos
      | Istr2New db2 s -> s
      | _ -> assert False ]
    ;
    methode nb_of_persons () = db2.patches.nb_per;
    methode nb_of_families () = db2.patches.nb_fam;
    methode patch_person ip p =
      match p with
      [ Person2Gen _ p -> do {
          Hashtbl.replace db2.patches.h_person ip p;
          db2.patches.nb_per :=
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
        }
      | _ -> assert False ]
    ;
    methode patch_ascend ip a =
      match a with
      [ Ascend2Gen _ a -> do {
          Hashtbl.replace db2.patches.h_ascend ip a;
          db2.patches.nb_per :=
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
        }
      | _ -> assert False ]
    ;
    methode patch_union ip u =
      match u with
      [ Union2Gen _ u -> do {
          Hashtbl.replace db2.patches.h_union ip u;
          db2.patches.nb_per :=
            max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
        }
      | _ -> failwith "not impl patch_union" ]
    ;
    methode patch_family ifam f =
      match f with
      [ Family2Gen _ f -> do {
          Hashtbl.replace db2.patches.h_family ifam f;
          db2.patches.nb_fam :=
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam;
        }
      | _ -> failwith "not impl patch_family" ]
    ;
    methode patch_descend ifam d =
      match d with
      [ Descend2Gen _ d -> do {
          Hashtbl.replace db2.patches.h_descend ifam d;
          db2.patches.nb_fam :=
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam;
        }
      | _ -> failwith "not impl patch_descend" ]
    ;
    methode patch_couple ifam c =
      match c with
      [ Couple2Gen _ c -> do {
          Hashtbl.replace db2.patches.h_couple ifam c;
          db2.patches.nb_fam :=
            max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam;
        }
      | _ -> failwith "not impl patch_couple" ]
    ;
    methode patch_key ip fn sn occ = do {
      Hashtbl.iter
        (fun key ip1 ->
           if ip = ip1 then Hashtbl.remove db2.patches.h_key key else ())
        db2.patches.h_key;
      let fn = Name.lower (nominative fn) in
      let sn = Name.lower (nominative sn) in
      Hashtbl.replace db2.patches.h_key (fn, sn, occ) ip
    };
    methode patch_name s ip =
      let s = Name.crush_lower s in
      let ht = db2.patches.h_name in
      try
        let ipl = Hashtbl.find ht s in
        if List.mem ip ipl then () else Hashtbl.replace ht s [ip :: ipl]
      with
      [ Not_found -> Hashtbl.add ht s [ip] ]
    ;
    methode insert_string s = Istr2New db2 s;
    methode commit_patches () = do {
      let fname = Filename.concat db2.bdir "patches" in
      let oc = open_out_bin (fname ^ "1") in
      output_string oc magic_patch;
      output_value oc db2.patches;
      close_out oc;
      remove_file (fname ^ "~");
      try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
      Sys.rename (fname ^ "1") fname
    };
    methode commit_notes _ = failwith "not impl commit_notes";
    methode is_patched_person ip =
      let _ =
        if ok_I_know.val then ()
        else do {
          ok_I_know.val := True;
          eprintf "not impl is_patched_person\n";
          flush stderr;
        }
      in
      False
    ;
    methode patched_ascends () =
      let _ = do { eprintf "not impl patched_ascends\n"; flush stderr; } in
      []
    ;
    methode output_consang_tab tab = do {
      let dir =
        List.fold_left Filename.concat db2.bdir ["person"; "consang"]
      in
      Mutil.mkdir_p dir;
      let oc = open_out_bin (Filename.concat dir "data") in
      output_value oc tab;
      close_out oc;
      let oc = open_out_bin (Filename.concat dir "access") in
      let _ : int =
        Iovalue.output_array_access oc (Array.get tab) (Array.length tab) 0
      in
      close_out oc;
    };
    methode delete_family ifam = heriter base : delete_family self ifam;
    methode person_of_key fn sn oc = person2_of_key db2 fn sn oc;
    methode persons_of_name s = persons2_of_name db2 s;
    methode persons_of_first_name () =
      persons_of_first_name_or_surname2 db2 True;
    methode persons_of_surname () =
      persons_of_first_name_or_surname2 db2 False;
    methode base_visible_get f = failwith "not impl visible_get";
    methode base_visible_write () = failwith "not impl visible_write";
    methode base_particles () =
      Mutil.input_particles (Filename.concat db2.bdir "particles.txt");
    methode base_strings_of_first_name s =
      base_strings_of_first_name_or_surname "first_name"
        (fun p -> p.first_name) s
    ;
    methode base_strings_of_surname s =
      base_strings_of_first_name_or_surname "surname"
        (fun p -> p.surname) s
    ;
    methode load_ascends_array () = do {
      eprintf "*** loading ascends array\n"; flush stderr;
      let nb = db2.patches.nb_per in
      let nb_ini = db2.patches.nb_per_ini in
      match db2.parents_array with
      [ Some _ -> ()
      | None -> db2.parents_array := Some (parents_array2 db2 nb_ini nb) ];
      match db2.consang_array with
      [ Some _ -> ()
      | None -> db2.consang_array := Some (consang_array2 db2 nb) ];
    };
    methode load_unions_array () =
      match db2.family_array with
      [ Some _ -> ()
      | None -> do {
          eprintf "*** loading unions array\n"; flush stderr;
          db2.family_array := Some (family_array2 db2)
        } ]
    ;
    methode load_couples_array () = do {
      eprintf "*** loading couples array\n"; flush stderr;
      let nb = db2.patches.nb_fam in
      match db2.father_array with
      [ Some _ -> ()
      | None -> do {
          let tab =
            load_array2 db2.bdir db2.patches.nb_fam_ini nb "family" "father"
              (fun ic_dat pos -> do {
                 seek_in ic_dat pos;
                 Iovalue.input ic_dat
               })
          in
          Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := father c)
            db2.patches.h_couple;
          db2.father_array := Some tab
        } ];
      match db2.mother_array with
      [ Some _ -> ()
      | None -> do {
          let tab =
            load_array2 db2.bdir db2.patches.nb_fam_ini nb "family" "mother"
              (fun ic_dat pos -> do {
                 seek_in ic_dat pos;
                 Iovalue.input ic_dat
               })
          in
          Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := mother c)
            db2.patches.h_couple;
          db2.mother_array := Some tab
        } ]
    };
    methode load_descends_array () =
      match db2.children_array with
      [ Some _ -> ()
      | None -> do {
          eprintf "*** loading descends array\n"; flush stderr;
          db2.children_array := Some (children_array2 db2)
        } ]
    ;
    methode load_strings_array () = ();
    methode persons_array () = failwith "not impl persons_array";
    methode ascends_array () =
      let nb = self.nb_of_persons () in
      let cg_tab =
        match db2.consang_array with
        [ Some tab -> tab
        | None -> consang_array2 db2 nb ]
      in
      let fget i = get_parents (Ascend2 db2 i) in
      let cget i = cg_tab.(i) in
      let cset i v = cg_tab.(i) := v in
      (fget, cget, cset, Some cg_tab)
    ;
    methode base_notes_read fnotes =
      read_notes (Filename.dirname db2.bdir) fnotes RnAll;
    methode base_notes_read_first_line fnotes =
      read_notes (Filename.dirname db2.bdir) fnotes Rn1Ln;
    methode base_notes_are_empty fnotes =
      read_notes (Filename.dirname db2.bdir) fnotes RnDeg = "";
    methode base_notes_origin_file () =
      let fname = Filename.concat db2.bdir "notes_of.txt" in
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic -> do {
          let r = input_line ic in
          close_in ic;
          r
        }
      | None -> "" ]
    ;
    methode base_notes_dir () = Filename.concat "base_d" "notes_d";
    methode base_wiznotes_dir () = Filename.concat "base_d" "wiznotes_d";
    methode person_misc_names p tit =
      heriter base : person_misc_names self p tit;
    methode nobtit conf p = heriter base : nobtit self conf p;
    methode p_first_name p = heriter base : p_first_name self p;
    methode p_surname p = heriter base : p_surname self p;
    methode date_of_last_change bname =
      let bdir =
        if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
      in
      let s =
        let bdir = Filename.concat bdir "base_d" in
        try Unix.stat (Filename.concat bdir "patches") with
        [ Unix.Unix_error _ _ _ -> Unix.stat bdir ]
      in
      s.Unix.st_mtime
    ;
    methode apply_as_dsk_base f = failwith "not impl apply_as_dsk_base";
  fin
;

value open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  if Sys.file_exists (Filename.concat bname "base_d") then do {
    Printf.eprintf "*** database new implementation\n";
    flush stderr; 
    base2 (base_of_base2 bname)
  }
  else base1 (Database.opendb bname)
;

value close_base b = b.close_base ();
value empty_person b = b.empty_person;
value person_of_gen_person b = b.person_of_gen_person;
value ascend_of_gen_ascend b = b.ascend_of_gen_ascend;
value union_of_gen_union b = b.union_of_gen_union;
value family_of_gen_family b = b.family_of_gen_family;
value couple_of_gen_couple b = b.couple_of_gen_couple;
value descend_of_gen_descend b = b.descend_of_gen_descend;
value poi b = b.poi;
value aoi b = b.aoi;
value uoi b = b.uoi;
value foi b = b.foi;
value coi b = b.coi;
value doi b = b.doi;
value sou b = b.sou;
value nb_of_persons b = b.nb_of_persons ();
value nb_of_families b = b.nb_of_families ();
value patch_person b = b.patch_person;
value patch_ascend b = b.patch_ascend;
value patch_union b = b.patch_union;
value patch_family b = b.patch_family;
value patch_descend b = b.patch_descend;
value patch_couple b = b.patch_couple;
value patch_key b = b.patch_key;
value patch_name b = b.patch_name;
value insert_string b = b.insert_string;
value commit_patches b = b.commit_patches ();
value commit_notes b = b.commit_notes;
value is_patched_person b = b.is_patched_person;
value patched_ascends b = b.patched_ascends ();
value output_consang_tab b = b.output_consang_tab;
value delete_family b = b.delete_family;
value person_of_key b = b.person_of_key;
value persons_of_name b = b.persons_of_name;
value persons_of_first_name b = b.persons_of_first_name ();
value persons_of_surname b = b.persons_of_surname ();
value base_visible_get b = b.base_visible_get;
value base_visible_write b = b.base_visible_write ();
value base_particles b = b.base_particles ();
value base_strings_of_first_name b = b.base_strings_of_first_name;
value base_strings_of_surname b = b.base_strings_of_surname;
value load_ascends_array b = b.load_ascends_array ();
value load_unions_array b = b.load_unions_array ();
value load_couples_array b = b.load_couples_array ();
value load_descends_array b = b.load_descends_array ();
value load_strings_array b = b.load_strings_array ();
value persons_array b = b.persons_array ();
value ascends_array b = b.ascends_array ();
value base_notes_read b = b.base_notes_read;
value base_notes_read_first_line b = b.base_notes_read_first_line;
value base_notes_are_empty b = b.base_notes_are_empty;
value base_notes_origin_file b = b.base_notes_origin_file ();
value base_notes_dir b = b.base_notes_dir ();
value base_wiznotes_dir b = b.base_wiznotes_dir ();
value person_misc_names b = b.person_misc_names;
value nobtit conf b = b.nobtit conf;
value p_first_name b = b.p_first_name;
value p_surname b = b.p_surname;
value date_of_last_change s b = b.date_of_last_change s;
value apply_as_dsk_base f b = b.apply_as_dsk_base f;
value base_of_dsk_base b = base1 b;

*)

(* This code is a pretty print of the code above from '#load "pragma.cmo"' *)

type person =
  [ Person of dsk_person
  | Person2 of db2 and int
  | Person2Gen of db2 and gen_person iper string ]
;

type person_fun 'a =
  { get_access : 'a -> access;
    get_aliases : 'a -> list istr;
    get_baptism : 'a -> codate;
    get_baptism_place : 'a -> istr;
    get_baptism_src : 'a -> istr;
    get_birth : 'a -> codate;
    get_birth_place : 'a -> istr;
    get_birth_src : 'a -> istr;
    get_burial : 'a -> Def.burial;
    get_burial_place : 'a -> istr;
    get_burial_src : 'a -> istr;
    get_death : 'a -> Def.death;
    get_death_place : 'a -> istr;
    get_death_src : 'a -> istr;
    get_first_name : 'a -> istr;
    get_first_names_aliases : 'a -> list istr;
    get_image : 'a -> istr;
    get_key_index : 'a -> iper;
    get_notes : 'a -> istr;
    get_occ : 'a -> int;
    get_occupation : 'a -> istr;
    get_psources : 'a -> istr;
    get_public_name : 'a -> istr;
    get_qualifiers : 'a -> list istr;
    get_related : 'a -> list iper;
    get_rparents : 'a -> list relation;
    get_sex : 'a -> Def.sex;
    get_surname : 'a -> istr;
    get_surnames_aliases : 'a -> list istr;
    get_titles : 'a -> list title;
    person_with_key : 'a -> istr -> istr -> int -> person;
    person_with_related : 'a -> list iper -> person;
    person_with_rparents : 'a -> list relation -> person;
    person_with_sex : 'a -> Def.sex -> person;
    gen_person_of_person : 'a -> Def.gen_person iper istr;
    dsk_person_of_person : 'a -> Dbdisk.dsk_person }
;

value person1_fun =
  {get_access p = p.Def.access;
   get_aliases p = List.map (fun i -> Istr i) p.Def.aliases;
   get_baptism p = p.Def.baptism;
   get_baptism_place p = Istr p.Def.baptism_place;
   get_baptism_src p = Istr p.Def.baptism_src; get_birth p = p.Def.birth;
   get_birth_place p = Istr p.Def.birth_place;
   get_birth_src p = Istr p.Def.birth_src; get_burial p = p.Def.burial;
   get_burial_place p = Istr p.Def.burial_place;
   get_burial_src p = Istr p.Def.burial_src; get_death p = p.Def.death;
   get_death_place p = Istr p.Def.death_place;
   get_death_src p = Istr p.Def.death_src;
   get_first_name p = Istr p.Def.first_name;
   get_first_names_aliases p =
     List.map (fun i -> Istr i) p.Def.first_names_aliases;
   get_image p = Istr p.Def.image; get_key_index p = p.Def.key_index;
   get_notes p = Istr p.Def.notes; get_occ p = p.Def.occ;
   get_occupation p = Istr p.Def.occupation;
   get_psources p = Istr p.Def.psources;
   get_public_name p = Istr p.Def.public_name;
   get_qualifiers p = List.map (fun i -> Istr i) p.Def.qualifiers;
   get_related p = p.Def.related;
   get_rparents p =
     List.map (map_relation_ps (fun x -> x) (fun i -> Istr i)) p.Def.rparents;
   get_sex p = p.Def.sex; get_surname p = Istr p.Def.surname;
   get_surnames_aliases p = List.map (fun i -> Istr i) p.Def.surnames_aliases;
   get_titles p =
     List.map (fun t -> map_title_strings (fun i -> Istr i) t) p.Def.titles;
   person_with_key p fn sn oc =
     match (fn, sn) with
     [ (Istr fn, Istr sn) ->
         Person {(p) with first_name = fn; surname = sn; occ = oc}
     | _ -> assert False ];
   person_with_related p r = Person {(p) with related = r};
   person_with_rparents p r =
     let r = List.map (map_relation_ps (fun p -> p) un_istr) r in
     Person {(p) with rparents = r};
   person_with_sex p s = Person {(p) with sex = s};
   gen_person_of_person p = map_person_ps (fun p -> p) (fun s -> Istr s) p;
   dsk_person_of_person p = p}
;

value person2_fun =
  let rec self =
    {get_access (db2, i) = get_field db2 i ("person", "access");
     get_aliases (db2, i) =
       let pos = get_field_acc db2 i ("person", "aliases") in
       if pos = -1 then []
       else
         let list =
           get_field_data db2 pos ("person", "aliases") "data2.ext"
         in
         List.map (fun pos -> Istr2 db2 ("person", "aliases") pos) list;
     get_baptism (db2, i) = get_field db2 i ("person", "baptism");
     get_baptism_place (db2, i) =
       make_istr2 db2 ("person", "baptism_place") i;
     get_baptism_src (db2, i) = make_istr2 db2 ("person", "baptism_src") i;
     get_birth (db2, i) = get_field db2 i ("person", "birth");
     get_birth_place (db2, i) = make_istr2 db2 ("person", "birth_place") i;
     get_birth_src (db2, i) = make_istr2 db2 ("person", "birth_src") i;
     get_burial (db2, i) = get_field db2 i ("person", "burial");
     get_burial_place (db2, i) = make_istr2 db2 ("person", "burial_place") i;
     get_burial_src (db2, i) = make_istr2 db2 ("person", "burial_src") i;
     get_death (db2, i) = get_field db2 i ("person", "death");
     get_death_place (db2, i) = make_istr2 db2 ("person", "death_place") i;
     get_death_src (db2, i) = make_istr2 db2 ("person", "death_src") i;
     get_first_name (db2, i) = make_istr2 db2 ("person", "first_name") i;
     get_first_names_aliases (db2, i) =
       let pos = get_field_acc db2 i ("person", "first_names_aliases") in
       if pos = -1 then []
       else
         let list =
           get_field_data db2 pos ("person", "first_names_aliases")
             "data2.ext"
         in
         List.map (fun pos -> Istr2 db2 ("person", "first_names_aliases") pos)
           list;
     get_image (db2, i) = make_istr2 db2 ("person", "image") i;
     get_key_index (db2, i) = Adef.iper_of_int i;
     get_notes (db2, i) = make_istr2 db2 ("person", "notes") i;
     get_occ (db2, i) = get_field db2 i ("person", "occ");
     get_occupation (db2, i) = make_istr2 db2 ("person", "occupation") i;
     get_psources (db2, i) = make_istr2 db2 ("person", "psources") i;
     get_public_name (db2, i) = make_istr2 db2 ("person", "public_name") i;
     get_qualifiers (db2, i) =
       let pos = get_field_acc db2 i ("person", "qualifiers") in
       if pos = -1 then []
       else
         let list =
           get_field_data db2 pos ("person", "qualifiers") "data2.ext"
         in
         List.map (fun pos -> Istr2 db2 ("person", "qualifiers") pos) list;
     get_related (db2, i) =
       let pos = get_field_acc db2 i ("person", "related") in
       let rec loop list pos =
         if pos = -1 then List.rev list
         else
           let (ip, pos) =
             get_field_2_data db2 pos ("person", "related") "data"
           in
           loop [ip :: list] pos
       in
       loop [] pos;
     get_rparents (db2, i) =
       let pos = get_field_acc db2 i ("person", "rparents") in
       if pos = -1 then []
       else
         let rl = get_field_data db2 pos ("person", "rparents") "data" in
         List.map
           (map_relation_ps (fun x -> x) (fun _ -> Istr2 db2 ("", "") (-1)))
           rl;
     get_sex (db2, i) = get_field db2 i ("person", "sex");
     get_surname (db2, i) = make_istr2 db2 ("person", "surname") i;
     get_surnames_aliases (db2, i) =
       let pos = get_field_acc db2 i ("person", "surnames_aliases") in
       if pos = -1 then []
       else
         let list =
           get_field_data db2 pos ("person", "surnames_aliases") "data2.ext"
         in
         List.map (fun pos -> Istr2 db2 ("person", "surnames_aliases") pos)
           list;
     get_titles (db2, i) =
       let pos = get_field_acc db2 i ("person", "titles") in
       if pos = -1 then []
       else
         let list = get_field_data db2 pos ("person", "titles") "data2.ext" in
         List.map
           (map_title_strings (fun pos -> Istr2 db2 ("person", "titles") pos))
           list;
     person_with_key (db2, i) fn sn oc =
       match (fn, sn) with
       [ (Istr2 _ (f1, f2) ifn, Istr2 _ (f3, f4) isn) ->
           failwith "not impl person_with_key 1"
       | (Istr2New _ fn, Istr2New _ sn) ->
           let p = self.gen_person_of_person (db2, i) in
           let p = map_person_ps (fun ip -> ip) sou2 p in
           Person2Gen db2 {(p) with first_name = fn; surname = sn; occ = oc}
       | _ -> failwith "not impl person_with_key 2" ];
     person_with_related (db2, i) r = failwith "not impl person_with_related";
     person_with_rparents (db2, i) r =
       failwith "not impl person_with_rparents";
     person_with_sex (db2, i) s = failwith "not impl person_with_sex";
     gen_person_of_person pp =
       {first_name = self.get_first_name pp; surname = self.get_surname pp;
        occ = self.get_occ pp; image = self.get_image pp;
        public_name = self.get_public_name pp;
        qualifiers = self.get_qualifiers pp; aliases = self.get_aliases pp;
        first_names_aliases = self.get_first_names_aliases pp;
        surnames_aliases = self.get_surnames_aliases pp;
        titles = self.get_titles pp; rparents = self.get_rparents pp;
        related = self.get_related pp; occupation = self.get_occupation pp;
        sex = self.get_sex pp; access = self.get_access pp;
        birth = self.get_birth pp; birth_place = self.get_birth_place pp;
        birth_src = self.get_birth_src pp; baptism = self.get_baptism pp;
        baptism_place = self.get_baptism_place pp;
        baptism_src = self.get_baptism_src pp; death = self.get_death pp;
        death_place = self.get_death_place pp;
        death_src = self.get_death_src pp; burial = self.get_burial pp;
        burial_place = self.get_burial_place pp;
        burial_src = self.get_burial_src pp; notes = self.get_notes pp;
        psources = self.get_psources pp; key_index = self.get_key_index pp};
     dsk_person_of_person p = failwith "not impl dsk_person_of_person"}
  in
  self
;

value person2gen_fun =
  {get_access (db2, p) = p.Def.access;
   get_aliases (db2, p) = List.map (fun s -> Istr2New db2 s) p.Def.aliases;
   get_baptism (db2, p) = p.Def.baptism;
   get_baptism_place (db2, p) = Istr2New db2 p.Def.baptism_place;
   get_baptism_src (db2, p) = Istr2New db2 p.Def.baptism_src;
   get_birth (db2, p) = p.Def.birth;
   get_birth_place (db2, p) = Istr2New db2 p.Def.birth_place;
   get_birth_src (db2, p) = Istr2New db2 p.Def.birth_src;
   get_burial (db2, p) = p.Def.burial;
   get_burial_place (db2, p) = Istr2New db2 p.Def.burial_place;
   get_burial_src (db2, p) = Istr2New db2 p.Def.burial_src;
   get_death (db2, p) = p.Def.death;
   get_death_place (db2, p) = Istr2New db2 p.Def.death_place;
   get_death_src (db2, p) = Istr2New db2 p.Def.death_src;
   get_first_name (db2, p) = Istr2New db2 p.Def.first_name;
   get_first_names_aliases (db2, p) =
     List.map (fun s -> Istr2New db2 s) p.Def.first_names_aliases;
   get_image (db2, p) = Istr2New db2 p.Def.image;
   get_key_index (db2, p) = p.Def.key_index;
   get_notes (db2, p) = Istr2New db2 p.Def.notes;
   get_occ (db2, p) = p.Def.occ;
   get_occupation (db2, p) = Istr2New db2 p.Def.occupation;
   get_psources (db2, p) = Istr2New db2 p.Def.psources;
   get_public_name (db2, p) = Istr2New db2 p.Def.public_name;
   get_qualifiers (db2, p) =
     List.map (fun s -> Istr2New db2 s) p.Def.qualifiers;
   get_related (db2, p) = p.Def.related;
   get_rparents (db2, p) =
     List.map (map_relation_ps (fun x -> x) (fun s -> Istr2New db2 s))
       p.Def.rparents;
   get_sex (db2, p) = p.Def.sex;
   get_surname (db2, p) = Istr2New db2 p.Def.surname;
   get_surnames_aliases (db2, p) =
     List.map (fun s -> Istr2New db2 s) p.Def.surnames_aliases;
   get_titles (db2, p) =
     List.map (fun t -> map_title_strings (fun s -> Istr2New db2 s) t)
       p.Def.titles;
   person_with_key (db2, p) fn sn oc =
     match (fn, sn) with
     [ (Istr2New _ fn, Istr2New _ sn) ->
         Person2Gen db2 {(p) with first_name = fn; surname = sn; occ = oc}
     | _ -> failwith "not impl person_with_key 3" ];
   person_with_related (db2, p) r =
     failwith "not impl person_with_related (gen)";
   person_with_rparents (db2, p) r =
     failwith "not impl person_with_rparents (gen)";
   person_with_sex (db2, p) s = Person2Gen db2 {(p) with sex = s};
   gen_person_of_person (db2, p) =
     map_person_ps (fun p -> p) (fun s -> Istr2New db2 s) p;
   dsk_person_of_person (db2, p) =
     failwith "not impl dsk_person_of_person (gen)"}
;

value wrap_per f g h =
  fun
  [ Person p -> f person1_fun p
  | Person2 db2 i -> g person2_fun (db2, i)
  | Person2Gen db2 p -> h person2gen_fun (db2, p) ]
;

value get_access =
  let f pf = pf.get_access in
  wrap_per f f f
;
value get_aliases =
  let f pf = pf.get_aliases in
  wrap_per f f f
;
value get_baptism =
  let f pf = pf.get_baptism in
  wrap_per f f f
;
value get_baptism_place =
  let f pf = pf.get_baptism_place in
  wrap_per f f f
;
value get_baptism_src =
  let f pf = pf.get_baptism_src in
  wrap_per f f f
;
value get_birth =
  let f pf = pf.get_birth in
  wrap_per f f f
;
value get_birth_place =
  let f pf = pf.get_birth_place in
  wrap_per f f f
;
value get_birth_src =
  let f pf = pf.get_birth_src in
  wrap_per f f f
;
value get_burial =
  let f pf = pf.get_burial in
  wrap_per f f f
;
value get_burial_place =
  let f pf = pf.get_burial_place in
  wrap_per f f f
;
value get_burial_src =
  let f pf = pf.get_burial_src in
  wrap_per f f f
;
value get_death =
  let f pf = pf.get_death in
  wrap_per f f f
;
value get_death_place =
  let f pf = pf.get_death_place in
  wrap_per f f f
;
value get_death_src =
  let f pf = pf.get_death_src in
  wrap_per f f f
;
value get_first_name =
  let f pf = pf.get_first_name in
  wrap_per f f f
;
value get_first_names_aliases =
  let f pf = pf.get_first_names_aliases in
  wrap_per f f f
;
value get_image =
  let f pf = pf.get_image in
  wrap_per f f f
;
value get_key_index =
  let f pf = pf.get_key_index in
  wrap_per f f f
;
value get_notes =
  let f pf = pf.get_notes in
  wrap_per f f f
;
value get_occ =
  let f pf = pf.get_occ in
  wrap_per f f f
;
value get_occupation =
  let f pf = pf.get_occupation in
  wrap_per f f f
;
value get_psources =
  let f pf = pf.get_psources in
  wrap_per f f f
;
value get_public_name =
  let f pf = pf.get_public_name in
  wrap_per f f f
;
value get_qualifiers =
  let f pf = pf.get_qualifiers in
  wrap_per f f f
;
value get_related =
  let f pf = pf.get_related in
  wrap_per f f f
;
value get_rparents =
  let f pf = pf.get_rparents in
  wrap_per f f f
;
value get_sex =
  let f pf = pf.get_sex in
  wrap_per f f f
;
value get_surname =
  let f pf = pf.get_surname in
  wrap_per f f f
;
value get_surnames_aliases =
  let f pf = pf.get_surnames_aliases in
  wrap_per f f f
;
value get_titles =
  let f pf = pf.get_titles in
  wrap_per f f f
;

value person_with_key =
  let f pf = pf.person_with_key in
  wrap_per f f f
;
value person_with_related =
  let f pf = pf.person_with_related in
  wrap_per f f f
;
value person_with_rparents =
  let f pf = pf.person_with_rparents in
  wrap_per f f f
;
value person_with_sex =
  let f pf = pf.person_with_sex in
  wrap_per f f f
;
value gen_person_of_person =
  let f pf = pf.gen_person_of_person in
  wrap_per f f f
;
value dsk_person_of_person =
  let f pf = pf.dsk_person_of_person in
  wrap_per f f f
;

type ascend_fun 'a =
  { get_consang : 'a -> Adef.fix; get_parents : 'a -> option ifam }
;

value ascend1_fun =
  {get_consang a = a.Def.consang; get_parents a = a.Def.parents}
;

value ascend2_fun =
  {get_consang (db2, i) =
     match db2.consang_array with
     [ Some tab -> tab.(i)
     | None ->
         try get_field db2 i ("person", "consang") with
         [ Sys_error _ -> no_consang ] ];
   get_parents (db2, i) =
     match db2.parents_array with
     [ Some tab -> tab.(i)
     | None ->
         let pos = get_field_acc db2 i ("person", "parents") in
         if pos = -1 then None
         else Some (get_field_data db2 pos ("person", "parents") "data") ]}
;

value ascend2gen_fun =
  {get_consang (db2, a) = a.Def.consang; get_parents (db2, a) = a.Def.parents}
;

type ascend =
  [ Ascend of dsk_ascend
  | Ascend2 of db2 and int
  | Ascend2Gen of db2 and gen_ascend ifam ]
;

value wrap_asc f g h =
  fun
  [ Ascend a -> f ascend1_fun a
  | Ascend2 db2 i -> g ascend2_fun (db2, i)
  | Ascend2Gen db2 a -> h ascend2gen_fun (db2, a) ]
;

value get_consang =
  let f pf = pf.get_consang in
  wrap_asc f f f
;
value get_parents =
  let f pf = pf.get_parents in
  wrap_asc f f f
;

type base =
  { close_base : unit -> unit;
    empty_person : iper -> person;
    person_of_gen_person : Def.gen_person iper istr -> person;
    ascend_of_gen_ascend : Def.gen_ascend ifam -> ascend;
    union_of_gen_union : Def.gen_union ifam -> union;
    family_of_gen_family : Def.gen_family iper istr -> family;
    couple_of_gen_couple : Def.gen_couple iper -> couple;
    descend_of_gen_descend : Def.gen_descend iper -> descend;
    poi : iper -> person;
    aoi : iper -> ascend;
    uoi : iper -> union;
    foi : ifam -> family;
    coi : ifam -> couple;
    doi : ifam -> descend;
    sou : istr -> string;
    nb_of_persons : unit -> int;
    nb_of_families : unit -> int;
    patch_person : iper -> person -> unit;
    patch_ascend : iper -> ascend -> unit;
    patch_union : iper -> union -> unit;
    patch_family : ifam -> family -> unit;
    patch_descend : ifam -> descend -> unit;
    patch_couple : ifam -> couple -> unit;
    patch_key : iper -> string -> string -> int -> unit;
    patch_name : string -> iper -> unit;
    insert_string : string -> istr;
    commit_patches : unit -> unit;
    commit_notes : string -> string -> unit;
    is_patched_person : iper -> bool;
    patched_ascends : unit -> list iper;
    output_consang_tab : array Adef.fix -> unit;
    delete_family : ifam -> unit;
    person_of_key : string -> string -> int -> option iper;
    persons_of_name : string -> list iper;
    persons_of_first_name : unit -> string_person_index;
    persons_of_surname : unit -> string_person_index;
    base_visible_get : (person -> bool) -> int -> bool;
    base_visible_write : unit -> unit;
    base_particles : unit -> list string;
    base_strings_of_first_name : string -> list istr;
    base_strings_of_surname : string -> list istr;
    load_ascends_array : unit -> unit;
    load_unions_array : unit -> unit;
    load_couples_array : unit -> unit;
    load_descends_array : unit -> unit;
    load_strings_array : unit -> unit;
    persons_array : unit -> (int -> person * int -> person -> unit);
    ascends_array :
      unit ->
        (int -> option ifam * int -> Adef.fix * int -> Adef.fix -> unit *
         option (array Adef.fix));
    base_notes_read : string -> string;
    base_notes_read_first_line : string -> string;
    base_notes_are_empty : string -> bool;
    base_notes_origin_file : unit -> string;
    base_notes_dir : unit -> string;
    base_wiznotes_dir : unit -> string;
    person_misc_names : person -> (person -> list title) -> list string;
    nobtit : config -> person -> list title;
    p_first_name : person -> string;
    p_surname : person -> string;
    date_of_last_change : string -> float;
    apply_as_dsk_base : (Dbdisk.dsk_base -> unit) -> unit }
;
module C_base :
  sig
    value delete_family : base -> ifam -> unit;
    value person_misc_names :
      base -> person -> (person -> list title) -> list string;
    value nobtit : base -> config -> person -> list title;
    value p_first_name : base -> person -> string;
    value p_surname : base -> person -> string;
  end =
  struct
    value husbands self p =
      let u = self.uoi (get_key_index p) in
      List.map
        (fun ifam ->
           let cpl = self.coi ifam in
           let husband = self.poi (get_father cpl) in
           let husband_surname = self.p_surname husband in
           let husband_surnames_aliases =
             List.map self.sou (get_surnames_aliases husband)
           in
           (husband_surname, husband_surnames_aliases))
        (Array.to_list (get_family u))
    ;
    value father_titles_places self p nobtit =
      match get_parents (self.aoi (get_key_index p)) with
      [ Some ifam ->
          let cpl = self.coi ifam in
          let fath = self.poi (get_father cpl) in
          List.map (fun t -> self.sou t.t_place) (nobtit fath)
      | None -> [] ]
    ;
    value delete_family self ifam =
      let cpl =
        self.couple_of_gen_couple
          (couple (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)))
      in
      let fam =
        let empty = self.insert_string "" in
        self.family_of_gen_family
          {marriage = codate_None; marriage_place = empty;
           marriage_src = empty; relation = Married; divorce = NotDivorced;
           witnesses = [| |]; comment = empty; origin_file = empty;
           fsources = empty; fam_index = Adef.ifam_of_int (-1)}
      in
      let des = self.descend_of_gen_descend {children = [| |]} in
      do {
        self.patch_family ifam fam;
        self.patch_couple ifam cpl;
        self.patch_descend ifam des
      }
    ;
    value person_misc_names self p tit =
      let sou = self.sou in
      Futil.gen_person_misc_names (sou (get_first_name p))
        (sou (get_surname p)) (sou (get_public_name p))
        (List.map sou (get_qualifiers p)) (List.map sou (get_aliases p))
        (List.map sou (get_first_names_aliases p))
        (List.map sou (get_surnames_aliases p))
        (List.map (Futil.map_title_strings sou) (tit p))
        (if get_sex p = Female then husbands self p else [])
        (father_titles_places self p tit)
    ;
    value nobtit self conf p =
      let list = get_titles p in
      match Lazy.force conf.allowed_titles with
      [ [] -> list
      | allowed_titles ->
          let list =
            List.fold_right
              (fun t l ->
                 let id = Name.lower (self.sou t.t_ident) in
                 let pl = Name.lower (self.sou t.t_place) in
                 if pl = "" then
                   if List.mem id allowed_titles then [t :: l] else l
                 else if
                   List.mem (id ^ "/" ^ pl) allowed_titles ||
                   List.mem (id ^ "/*") allowed_titles
                 then
                   [t :: l]
                 else l)
              list []
          in
          match Lazy.force conf.denied_titles with
          [ [] -> list
          | denied_titles ->
              List.filter
                (fun t ->
                   let id = Name.lower (self.sou t.t_ident) in
                   let pl = Name.lower (self.sou t.t_place) in
                   if List.mem (id ^ "/" ^ pl) denied_titles ||
                      List.mem ("*/" ^ pl) denied_titles
                   then
                     False
                   else True)
                list ] ]
    ;
    value p_first_name self p = nominative (self.sou (get_first_name p));
    value p_surname self p = nominative (self.sou (get_surname p));
  end
;

value base1 base =
  let base_strings_of_first_name_or_surname s =
    List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  in
  let rec self =
    {close_base = base.func.cleanup;
     empty_person ip = Person (empty_person (Adef.istr_of_int 0) ip);
     person_of_gen_person p = Person (map_person_ps (fun p -> p) un_istr p);
     ascend_of_gen_ascend a = Ascend a; union_of_gen_union u = Union u;
     family_of_gen_family f = Family (map_family_ps (fun p -> p) un_istr f);
     couple_of_gen_couple c = Couple c; descend_of_gen_descend d = Descend d;
     poi i = Person (base.data.persons.get (Adef.int_of_iper i));
     aoi i = Ascend (base.data.ascends.get (Adef.int_of_iper i));
     uoi i = Union (base.data.unions.get (Adef.int_of_iper i));
     foi i = Family (base.data.families.get (Adef.int_of_ifam i));
     coi i = Couple (base.data.couples.get (Adef.int_of_ifam i));
     doi i = Descend (base.data.descends.get (Adef.int_of_ifam i));
     sou i =
       match i with
       [ Istr i -> base.data.strings.get (Adef.int_of_istr i)
       | _ -> assert False ];
     nb_of_persons () = base.data.persons.len;
     nb_of_families () = base.data.families.len;
     patch_person ip p =
       match p with
       [ Person p -> base.func.Dbdisk.patch_person ip p
       | _ -> assert False ];
     patch_ascend ip a =
       match a with
       [ Ascend a -> base.func.Dbdisk.patch_ascend ip a
       | _ -> assert False ];
     patch_union ip u =
       match u with
       [ Union u -> base.func.Dbdisk.patch_union ip u
       | _ -> failwith "not impl patch_union" ];
     patch_family ifam f =
       match f with
       [ Family f -> base.func.Dbdisk.patch_family ifam f
       | _ -> failwith "not impl patch_family" ];
     patch_descend ifam d =
       match d with
       [ Descend d -> base.func.Dbdisk.patch_descend ifam d
       | _ -> failwith "not impl patch_descend" ];
     patch_couple ifam c =
       match c with
       [ Couple c -> base.func.Dbdisk.patch_couple ifam c
       | _ -> failwith "not impl patch_couple" ];
     patch_key ip fn sn occ = ();
     patch_name s ip = base.func.Dbdisk.patch_name s ip;
     insert_string s = Istr (base.func.Dbdisk.insert_string s);
     commit_patches = base.func.Dbdisk.commit_patches;
     commit_notes = base.func.Dbdisk.commit_notes;
     is_patched_person ip = base.func.Dbdisk.is_patched_person ip;
     patched_ascends = base.func.Dbdisk.patched_ascends;
     output_consang_tab tab =
       do { eprintf "error Gwdb.output_consang_tab\n"; flush stdout };
     delete_family ifam = C_base.delete_family self ifam;
     person_of_key = base.func.Dbdisk.person_of_key;
     persons_of_name = base.func.Dbdisk.persons_of_name;
     persons_of_first_name () = Spi base.func.Dbdisk.persons_of_first_name;
     persons_of_surname () = Spi base.func.Dbdisk.persons_of_surname;
     base_visible_get f = base.data.visible.v_get (fun p -> f (Person p));
     base_visible_write = base.data.visible.v_write;
     base_particles () = base.data.particles;
     base_strings_of_first_name = base_strings_of_first_name_or_surname;
     base_strings_of_surname = base_strings_of_first_name_or_surname;
     load_ascends_array = base.data.ascends.load_array;
     load_unions_array = base.data.unions.load_array;
     load_couples_array = base.data.couples.load_array;
     load_descends_array = base.data.descends.load_array;
     load_strings_array = base.data.strings.load_array;
     persons_array () =
       let get i = Person (base.data.persons.get i) in
       let set i =
         fun
         [ Person p -> base.data.persons.set i p
         | Person2 _ _ -> assert False
         | Person2Gen _ _ -> assert False ]
       in
       (get, set);
     ascends_array () =
       let fget i = (base.data.ascends.get i).parents in
       let cget i = (base.data.ascends.get i).consang in
       let cset i v =
         base.data.ascends.set i {(base.data.ascends.get i) with consang = v}
       in
       (fget, cget, cset, None);
     base_notes_read fnotes = base.data.bnotes.nread fnotes RnAll;
     base_notes_read_first_line fnotes = base.data.bnotes.nread fnotes Rn1Ln;
     base_notes_are_empty fnotes = base.data.bnotes.nread fnotes RnDeg = "";
     base_notes_origin_file () = base.data.bnotes.norigin_file;
     base_notes_dir () = "notes_d"; base_wiznotes_dir () = "wiznotes";
     person_misc_names p tit = C_base.person_misc_names self p tit;
     nobtit conf p = C_base.nobtit self conf p;
     p_first_name p = C_base.p_first_name self p;
     p_surname p = C_base.p_surname self p;
     date_of_last_change bname =
       let bdir =
         if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
       in
       let s =
         try Unix.stat (Filename.concat bdir "patches") with
         [ Unix.Unix_error _ _ _ -> Unix.stat (Filename.concat bdir "base") ]
       in
       s.Unix.st_mtime;
     apply_as_dsk_base f = f base}
  in
  self
;

value base2 db2 =
  let base_strings_of_first_name_or_surname field proj s =
    let posl = strings2_of_fsname db2 field s in
    let istrl = List.map (fun pos -> Istr2 db2 ("person", field) pos) posl in
    let s = Name.crush_lower s in
    Hashtbl.fold
      (fun _ iper istrl ->
         try
           let p = Hashtbl.find db2.patches.h_person iper in
           if Name.crush_lower (proj p) = s then
             [Istr2New db2 (proj p) :: istrl]
           else istrl
         with
         [ Not_found -> istrl ])
      db2.patches.h_key istrl
  in
  let rec self =
    {close_base () =
       Hashtbl.iter (fun (f1, f2, f) ic -> close_in ic) db2.cache_chan;
     empty_person ip = Person2Gen db2 (empty_person "" ip);
     person_of_gen_person p =
       Person2Gen db2 (map_person_ps (fun p -> p) un_istr2 p);
     ascend_of_gen_ascend a = Ascend2Gen db2 a;
     union_of_gen_union u = Union2Gen db2 u;
     family_of_gen_family f =
       Family2Gen db2 (map_family_ps (fun p -> p) un_istr2 f);
     couple_of_gen_couple c = Couple2Gen db2 c;
     descend_of_gen_descend d = Descend2Gen db2 d;
     poi i =
       try Person2Gen db2 (Hashtbl.find db2.patches.h_person i) with
       [ Not_found -> Person2 db2 (Adef.int_of_iper i) ];
     aoi i =
       try Ascend2Gen db2 (Hashtbl.find db2.patches.h_ascend i) with
       [ Not_found -> Ascend2 db2 (Adef.int_of_iper i) ];
     uoi i =
       try Union2Gen db2 (Hashtbl.find db2.patches.h_union i) with
       [ Not_found -> Union2 db2 (Adef.int_of_iper i) ];
     foi i =
       try Family2Gen db2 (Hashtbl.find db2.patches.h_family i) with
       [ Not_found -> Family2 db2 (Adef.int_of_ifam i) ];
     coi i =
       try Couple2Gen db2 (Hashtbl.find db2.patches.h_couple i) with
       [ Not_found -> Couple2 db2 (Adef.int_of_ifam i) ];
     doi i =
       try Descend2Gen db2 (Hashtbl.find db2.patches.h_descend i) with
       [ Not_found -> Descend2 db2 (Adef.int_of_ifam i) ];
     sou i =
       match i with
       [ Istr2 db2 f pos -> string_of_istr2 db2 f pos
       | Istr2New db2 s -> s
       | _ -> assert False ];
     nb_of_persons () = db2.patches.nb_per;
     nb_of_families () = db2.patches.nb_fam;
     patch_person ip p =
       match p with
       [ Person2Gen _ p ->
           do {
             Hashtbl.replace db2.patches.h_person ip p;
             db2.patches.nb_per :=
               max (Adef.int_of_iper ip + 1) db2.patches.nb_per
           }
       | _ -> assert False ];
     patch_ascend ip a =
       match a with
       [ Ascend2Gen _ a ->
           do {
             Hashtbl.replace db2.patches.h_ascend ip a;
             db2.patches.nb_per :=
               max (Adef.int_of_iper ip + 1) db2.patches.nb_per
           }
       | _ -> assert False ];
     patch_union ip u =
       match u with
       [ Union2Gen _ u ->
           do {
             Hashtbl.replace db2.patches.h_union ip u;
             db2.patches.nb_per :=
               max (Adef.int_of_iper ip + 1) db2.patches.nb_per
           }
       | _ -> failwith "not impl patch_union" ];
     patch_family ifam f =
       match f with
       [ Family2Gen _ f ->
           do {
             Hashtbl.replace db2.patches.h_family ifam f;
             db2.patches.nb_fam :=
               max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
           }
       | _ -> failwith "not impl patch_family" ];
     patch_descend ifam d =
       match d with
       [ Descend2Gen _ d ->
           do {
             Hashtbl.replace db2.patches.h_descend ifam d;
             db2.patches.nb_fam :=
               max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
           }
       | _ -> failwith "not impl patch_descend" ];
     patch_couple ifam c =
       match c with
       [ Couple2Gen _ c ->
           do {
             Hashtbl.replace db2.patches.h_couple ifam c;
             db2.patches.nb_fam :=
               max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
           }
       | _ -> failwith "not impl patch_couple" ];
     patch_key ip fn sn occ =
       do {
         Hashtbl.iter
           (fun key ip1 ->
              if ip = ip1 then Hashtbl.remove db2.patches.h_key key else ())
           db2.patches.h_key;
         let fn = Name.lower (nominative fn) in
         let sn = Name.lower (nominative sn) in
         Hashtbl.replace db2.patches.h_key (fn, sn, occ) ip
       };
     patch_name s ip =
       let s = Name.crush_lower s in
       let ht = db2.patches.h_name in
       try
         let ipl = Hashtbl.find ht s in
         if List.mem ip ipl then () else Hashtbl.replace ht s [ip :: ipl]
       with
       [ Not_found -> Hashtbl.add ht s [ip] ];
     insert_string s = Istr2New db2 s;
     commit_patches () =
       let fname = Filename.concat db2.bdir "patches" in
       let oc = open_out_bin (fname ^ "1") in
       do {
         output_string oc magic_patch;
         output_value oc db2.patches;
         close_out oc;
         remove_file (fname ^ "~");
         try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
         Sys.rename (fname ^ "1") fname
       };
     commit_notes _ = failwith "not impl commit_notes";
     is_patched_person ip =
       let _ =
         if ok_I_know.val then ()
         else do {
           ok_I_know.val := True;
           eprintf "not impl is_patched_person\n";
           flush stderr
         }
       in
       False;
     patched_ascends () =
       let _ = do { eprintf "not impl patched_ascends\n"; flush stderr } in
       [];
     output_consang_tab tab =
       let dir =
         List.fold_left Filename.concat db2.bdir ["person"; "consang"]
       in
       do {
         Mutil.mkdir_p dir;
         let oc = open_out_bin (Filename.concat dir "data") in
         output_value oc tab;
         close_out oc;
         let oc = open_out_bin (Filename.concat dir "access") in
         let _ : int =
           Iovalue.output_array_access oc (Array.get tab) (Array.length tab) 0
         in
         close_out oc
       };
     delete_family ifam = C_base.delete_family self ifam;
     person_of_key fn sn oc = person2_of_key db2 fn sn oc;
     persons_of_name s = persons2_of_name db2 s;
     persons_of_first_name () = persons_of_first_name_or_surname2 db2 True;
     persons_of_surname () = persons_of_first_name_or_surname2 db2 False;
     base_visible_get f = failwith "not impl visible_get";
     base_visible_write () = failwith "not impl visible_write";
     base_particles () =
       Mutil.input_particles (Filename.concat db2.bdir "particles.txt");
     base_strings_of_first_name s =
       base_strings_of_first_name_or_surname "first_name"
         (fun p -> p.first_name) s;
     base_strings_of_surname s =
       base_strings_of_first_name_or_surname "surname" (fun p -> p.surname) s;
     load_ascends_array () =
       do {
         eprintf "*** loading ascends array\n";
         flush stderr;
         let nb = db2.patches.nb_per in
         let nb_ini = db2.patches.nb_per_ini in
         match db2.parents_array with
         [ Some _ -> ()
         | None -> db2.parents_array := Some (parents_array2 db2 nb_ini nb) ];
         match db2.consang_array with
         [ Some _ -> ()
         | None -> db2.consang_array := Some (consang_array2 db2 nb) ]
       };
     load_unions_array () =
       match db2.family_array with
       [ Some _ -> ()
       | None ->
           do {
             eprintf "*** loading unions array\n";
             flush stderr;
             db2.family_array := Some (family_array2 db2)
           } ];
     load_couples_array () =
       do {
         eprintf "*** loading couples array\n";
         flush stderr;
         let nb = db2.patches.nb_fam in
         match db2.father_array with
         [ Some _ -> ()
         | None ->
             let tab =
               load_array2 db2.bdir db2.patches.nb_fam_ini nb "family"
                 "father"
                 (fun ic_dat pos ->
                    do { seek_in ic_dat pos; Iovalue.input ic_dat })
             in
             do {
               Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := father c)
                 db2.patches.h_couple;
               db2.father_array := Some tab
             } ];
         match db2.mother_array with
         [ Some _ -> ()
         | None ->
             let tab =
               load_array2 db2.bdir db2.patches.nb_fam_ini nb "family"
                 "mother"
                 (fun ic_dat pos ->
                    do { seek_in ic_dat pos; Iovalue.input ic_dat })
             in
             do {
               Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := mother c)
                 db2.patches.h_couple;
               db2.mother_array := Some tab
             } ]
       };
     load_descends_array () =
       match db2.children_array with
       [ Some _ -> ()
       | None ->
           do {
             eprintf "*** loading descends array\n";
             flush stderr;
             db2.children_array := Some (children_array2 db2)
           } ];
     load_strings_array () = ();
     persons_array () = failwith "not impl persons_array";
     ascends_array () =
       let nb = self.nb_of_persons () in
       let cg_tab =
         match db2.consang_array with
         [ Some tab -> tab
         | None -> consang_array2 db2 nb ]
       in
       let fget i = get_parents (Ascend2 db2 i) in
       let cget i = cg_tab.(i) in
       let cset i v = cg_tab.(i) := v in
       (fget, cget, cset, Some cg_tab);
     base_notes_read fnotes =
       read_notes (Filename.dirname db2.bdir) fnotes RnAll;
     base_notes_read_first_line fnotes =
       read_notes (Filename.dirname db2.bdir) fnotes Rn1Ln;
     base_notes_are_empty fnotes =
       read_notes (Filename.dirname db2.bdir) fnotes RnDeg = "";
     base_notes_origin_file () =
       let fname = Filename.concat db2.bdir "notes_of.txt" in
       match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
       [ Some ic ->
           let r = input_line ic in
           do { close_in ic; r }
       | None -> "" ];
     base_notes_dir () = Filename.concat "base_d" "notes_d";
     base_wiznotes_dir () = Filename.concat "base_d" "wiznotes_d";
     person_misc_names p tit = C_base.person_misc_names self p tit;
     nobtit conf p = C_base.nobtit self conf p;
     p_first_name p = C_base.p_first_name self p;
     p_surname p = C_base.p_surname self p;
     date_of_last_change bname =
       let bdir =
         if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
       in
       let s =
         let bdir = Filename.concat bdir "base_d" in
         try Unix.stat (Filename.concat bdir "patches") with
         [ Unix.Unix_error _ _ _ -> Unix.stat bdir ]
       in
       s.Unix.st_mtime;
     apply_as_dsk_base f = failwith "not impl apply_as_dsk_base"}
  in
  self
;

value open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  if Sys.file_exists (Filename.concat bname "base_d") then do {
    Printf.eprintf "*** database new implementation\n";
    flush stderr;
    base2 (base_of_base2 bname)
  }
  else base1 (Database.opendb bname)
;

value close_base b = b.close_base ();
value empty_person b = b.empty_person;
value person_of_gen_person b = b.person_of_gen_person;
value ascend_of_gen_ascend b = b.ascend_of_gen_ascend;
value union_of_gen_union b = b.union_of_gen_union;
value family_of_gen_family b = b.family_of_gen_family;
value couple_of_gen_couple b = b.couple_of_gen_couple;
value descend_of_gen_descend b = b.descend_of_gen_descend;
value poi b = b.poi;
value aoi b = b.aoi;
value uoi b = b.uoi;
value foi b = b.foi;
value coi b = b.coi;
value doi b = b.doi;
value sou b = b.sou;
value nb_of_persons b = b.nb_of_persons ();
value nb_of_families b = b.nb_of_families ();
value patch_person b = b.patch_person;
value patch_ascend b = b.patch_ascend;
value patch_union b = b.patch_union;
value patch_family b = b.patch_family;
value patch_descend b = b.patch_descend;
value patch_couple b = b.patch_couple;
value patch_key b = b.patch_key;
value patch_name b = b.patch_name;
value insert_string b = b.insert_string;
value commit_patches b = b.commit_patches ();
value commit_notes b = b.commit_notes;
value is_patched_person b = b.is_patched_person;
value patched_ascends b = b.patched_ascends ();
value output_consang_tab b = b.output_consang_tab;
value delete_family b = b.delete_family;
value person_of_key b = b.person_of_key;
value persons_of_name b = b.persons_of_name;
value persons_of_first_name b = b.persons_of_first_name ();
value persons_of_surname b = b.persons_of_surname ();
value base_visible_get b = b.base_visible_get;
value base_visible_write b = b.base_visible_write ();
value base_particles b = b.base_particles ();
value base_strings_of_first_name b = b.base_strings_of_first_name;
value base_strings_of_surname b = b.base_strings_of_surname;
value load_ascends_array b = b.load_ascends_array ();
value load_unions_array b = b.load_unions_array ();
value load_couples_array b = b.load_couples_array ();
value load_descends_array b = b.load_descends_array ();
value load_strings_array b = b.load_strings_array ();
value persons_array b = b.persons_array ();
value ascends_array b = b.ascends_array ();
value base_notes_read b = b.base_notes_read;
value base_notes_read_first_line b = b.base_notes_read_first_line;
value base_notes_are_empty b = b.base_notes_are_empty;
value base_notes_origin_file b = b.base_notes_origin_file ();
value base_notes_dir b = b.base_notes_dir ();
value base_wiznotes_dir b = b.base_wiznotes_dir ();
value person_misc_names b = b.person_misc_names;
value nobtit conf b = b.nobtit conf;
value p_first_name b = b.p_first_name;
value p_surname b = b.p_surname;
value date_of_last_change s b = b.date_of_last_change s;
value apply_as_dsk_base f b = b.apply_as_dsk_base f;
value base_of_dsk_base b = base1 b;

(* end of pretty printed code *)

(**)
