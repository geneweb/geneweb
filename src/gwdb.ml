(* $Id: gwdb.ml,v 5.57 2006-10-21 10:35:25 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;
open Config;
open Dbdisk;
open Def;
open Futil;
open Mutil;
open Printf;

type cache =
  { chan : mutable list ((string * string * string) * in_channel) }
;

type istr =
  [ Istr of dsk_istr
  | Istr2 of (string * cache) and (string * string) and int ]
;

type person =
  [ Person of dsk_person
  | Person2 of (string * cache) and int ]
;
type ascend =
  [ Ascend of dsk_ascend
  | Ascend2 of (string * cache) and int ]
;
type union =
  [ Union of dsk_union
  | Union2 of (string * cache) and int ]
;

type family =
  [ Family of dsk_family
  | Family2 of (string * cache) and int ]
;
type couple =
  [ Couple of dsk_couple
  | Couple2 of (string * cache) and int ]
;
type descend =
  [ Descend of dsk_descend
  | Descend2 of (string * cache) and int ]
;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

type gen_string_person_index 'istr = Dbdisk.string_person_index 'istr ==
  { find : 'istr -> list iper;
    cursor : string -> 'istr;
    next : 'istr -> 'istr }
;

type string_person_index =
  [ Spi of gen_string_person_index dsk_istr
  | Spi2 ]
;

type base =
  [ Base of Dbdisk.dsk_base
  | Base2 of (string * cache) ]
;

value get_field_acc (bn, cache) i (f1, f2) = do {
  let ic =
    try List.assoc (f1, f2, "access") cache.chan with
    [ Not_found -> do {
        let ic =
          open_in_bin (List.fold_left Filename.concat bn [f1; f2; "access"])
        in
        cache.chan := [((f1, f2, "access"), ic) :: cache.chan];
        ic
      } ]
  in
  seek_in ic (4 * i);
  let pos = input_binary_int ic in
  pos
};

value get_field_data (bn, cache) pos (f1, f2) data = do {
  let ic =
    try List.assoc (f1, f2, data) cache.chan with
    [ Not_found -> do {
        let ic =
          open_in_bin (List.fold_left Filename.concat bn [f1; f2; data])
        in
        cache.chan := [((f1, f2, data), ic) :: cache.chan];
        ic
      } ]
  in
  seek_in ic pos;
  let r = Iovalue.input ic in
  r
};

value get_field_2_data (bn, cache_chan) pos (f1, f2) data = do {
  let ic =
    open_in_bin (List.fold_left Filename.concat bn [f1; f2; data])
  in
  seek_in ic pos;
  let r = Iovalue.input ic in
  let s = Iovalue.input ic in
  close_in ic;
  (r, s)
};

value get_field bn i path =
  let pos = get_field_acc bn i path in
  get_field_data bn pos path "data"
;

value make_istr2 bnc path i = Istr2 bnc path (get_field_acc bnc i path);

value is_empty_string =
  fun
  [ Istr istr -> istr = Adef.istr_of_int 0
  | Istr2 bn path pos -> pos = 0 ]
;
value is_quest_string =
  fun
  [ Istr istr -> istr = Adef.istr_of_int 1
  | Istr2 bn path pos -> failwith "not impl is_quest_string" ]
;

value get_access =
  fun
  [ Person p -> p.Def.access
  | Person2 bn i -> get_field bn i ("person", "access") ]
;
value get_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.aliases
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "aliases") in
      if pos = -1 then []
      else
        let list = get_field_data bn pos ("person", "aliases") "data2.ext" in
        List.map (fun pos -> Istr2 bn ("person", "aliases") pos) list ]
;
value get_baptism =
  fun
  [ Person p -> p.Def.baptism
  | Person2 bn i -> get_field bn i ("person", "baptism") ]
;
value get_baptism_place =
  fun
  [ Person p -> Istr p.Def.baptism_place
  | Person2 bnc i -> make_istr2 bnc ("person", "baptism_place") i ]
;
value get_baptism_src =
  fun
  [ Person p -> Istr p.Def.baptism_src
  | Person2 bnc i -> make_istr2 bnc ("person", "baptism_src") i ]
;
value get_birth =
  fun
  [ Person p -> p.Def.birth
  | Person2 bn i -> get_field bn i ("person", "birth") ]
;
value get_birth_place =
  fun
  [ Person p -> Istr p.Def.birth_place
  | Person2 bnc i -> make_istr2 bnc ("person", "birth_place") i ]
;
value get_birth_src =
  fun
  [ Person p -> Istr p.Def.birth_src
  | Person2 bnc i -> make_istr2 bnc ("person", "birth_src") i ]
;
value get_burial =
  fun
  [ Person p -> p.Def.burial
  | Person2 bn i -> get_field bn i ("person", "burial") ]
;
value get_burial_place =
  fun
  [ Person p -> Istr p.Def.burial_place
  | Person2 bnc i -> make_istr2 bnc ("person", "burial_place") i ]
;
value get_burial_src =
  fun
  [ Person p -> Istr p.Def.burial_src
  | Person2 bnc i -> make_istr2 bnc ("person", "burial_src") i ]
;
value get_death =
  fun
  [ Person p -> p.Def.death
  | Person2 bn i -> get_field bn i ("person", "death") ]
;
value get_death_place =
  fun
  [ Person p -> Istr p.Def.death_place
  | Person2 bnc i -> make_istr2 bnc ("person", "death_place") i ]
;
value get_death_src =
  fun
  [ Person p -> Istr p.Def.death_src
  | Person2 bnc i -> make_istr2 bnc ("person", "death_src") i ]
;
value get_first_name =
  fun
  [ Person p -> Istr p.Def.first_name
  | Person2 bnc i -> make_istr2 bnc ("person", "first_name") i ]
;
value get_first_names_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.first_names_aliases
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "first_names_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data bn pos ("person", "first_names_aliases") "data2.ext"
        in
        List.map (fun pos -> Istr2 bn ("person", "first_names_aliases") pos)
          list ]
;
value get_image =
  fun
  [ Person p -> Istr p.Def.image
  | Person2 bnc i -> make_istr2 bnc ("person", "image") i ]
;
value get_key_index =
  fun
  [ Person p -> p.Def.key_index
  | Person2 _ i -> Adef.iper_of_int i ]
;
value get_notes =
  fun
  [ Person p -> Istr p.Def.notes
  | Person2 bnc i -> make_istr2 bnc ("person", "notes") i ]
;
value get_occ =
  fun
  [ Person p -> p.Def.occ
  | Person2 bn i -> get_field bn i ("person", "occ") ]
;
value get_occupation =
  fun
  [ Person p -> Istr p.Def.occupation
  | Person2 bnc i -> make_istr2 bnc ("person", "occupation") i ]
;
value get_psources =
  fun
  [ Person p -> Istr p.Def.psources
  | Person2 bnc i -> make_istr2 bnc ("person", "psources") i ]
;
value get_public_name =
  fun
  [ Person p -> Istr p.Def.public_name
  | Person2 bnc i -> make_istr2 bnc ("person", "public_name") i ]
;
value get_qualifiers =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.qualifiers
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "qualifiers") in
      if pos = -1 then []
      else
        let list =
          get_field_data bn pos ("person", "qualifiers") "data2.ext"
        in
        List.map (fun pos -> Istr2 bn ("person", "qualifiers") pos) list ]
;
value get_related =
  fun
  [ Person p -> p.Def.related
  | Person2 bn i -> get_field bn i ("person", "related") ]
;
value get_rparents =
  fun
  [ Person p ->
      List.map (fun r -> map_relation_ps (fun x -> x) (fun i -> Istr i) r)
        p.Def.rparents
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "rparents") in
      if pos = -1 then []
      else failwith "not impl get_rparents" ]
;
value get_sex =
  fun
  [ Person p -> p.Def.sex
  | Person2 bn i -> get_field bn i ("person", "sex") ]
;
value get_surname =
  fun
  [ Person p -> Istr p.Def.surname
  | Person2 bnc i -> make_istr2 bnc ("person", "surname") i ]
;
value get_surnames_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.surnames_aliases
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "surnames_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data bn pos ("person", "surnames_aliases") "data2.ext"
        in
        List.map
          (fun pos -> Istr2 bn ("person", "surnames_aliases") pos) list ]
;
value get_titles =
  fun
  [ Person p ->
      List.map (fun t -> map_title_strings (fun i -> Istr i) t)
        p.Def.titles
  | Person2 bn i ->
      let pos = get_field_acc bn i ("person", "titles") in
      if pos = -1 then []
      else
        let list =
          get_field_data bn pos ("person", "titles") "data2.ext"
        in
        List.map
          (map_title_strings (fun pos -> Istr2 bn ("person", "titles") pos))
          list ]
;

value person_with_key p fn sn oc =
  match (p, fn, sn) with
  [ (Person p, Istr fn, Istr sn) ->
      Person {(p) with first_name = fn; surname = sn; occ = oc}
  | _ -> failwith "not impl person_with_key" ]
;
value person_with_related p r =
  match p with
  [ Person p -> Person {(p) with related = r}
  | Person2 _ _ -> failwith "not impl person_with_key" ]
;

value un_istr =
  fun
  [ Istr i -> i
  | Istr2 _ _ i -> failwith "un_istr" ]
;

value person_with_rparents p r =
  match p with
  [ Person p ->
      let r = List.map (map_relation_ps (fun p -> p) un_istr) r in
      Person {(p) with rparents = r}
  | Person2 _ _ -> failwith "not impl person_with_rparents" ]
;
value person_with_sex p s =
  match p with
  [ Person p -> Person {(p) with sex = s}
  | Person2 _ _ -> failwith "not impl person_with_sex" ]
;
value person_of_gen_person p =
  let p = map_person_ps (fun p -> p) un_istr p in
  Person p
;
value gen_person_of_person =
  fun
  [ Person p -> map_person_ps (fun p -> p) (fun s -> Istr s) p
  | Person2 _ _ as p ->
      {first_name = get_first_name p; surname = get_surname p;
       occ = get_occ p; image = get_image p; public_name = get_public_name p;
       qualifiers = get_qualifiers p; aliases = get_aliases p;
       first_names_aliases = get_first_names_aliases p;
       surnames_aliases = get_surnames_aliases p; titles = get_titles p;
       rparents = get_rparents p; related = get_related p;
       occupation = get_occupation p; sex = get_sex p; access = get_access p;
       birth = get_birth p; birth_place = get_birth_place p;
       birth_src = get_birth_src p; baptism = get_baptism p;
       baptism_place = get_baptism_place p; baptism_src = get_baptism_src p;
       death = get_death p; death_place = get_death_place p;
       death_src = get_death_src p; burial = get_burial p;
       burial_place = get_burial_place p; burial_src = get_burial_src p;
       notes = get_notes p; psources = get_psources p;
       key_index = get_key_index p} ]
;

value get_consang =
  fun
  [ Ascend a -> a.Def.consang
  | Ascend2 bn i -> Adef.fix (-1) ]
;
value get_parents =
  fun
  [ Ascend a -> a.Def.parents
  | Ascend2 bn i ->
      let pos = get_field_acc bn i ("person", "parents") in
      if pos = -1 then None
      else Some (get_field_data bn pos ("person", "parents") "data") ]
;

value ascend_with_consang a c =
  match a with
  [ Ascend a -> Ascend {parents = a.parents; consang = c}
  | Ascend2 _ _ -> failwith "not impl ascend_with_consang" ]
;
value ascend_with_parents a p =
  match a with
  [ Ascend a -> Ascend {parents = p; consang = a.consang}
  | Ascend2 _ _ -> failwith "not impl ascend_with_consang" ]
;
value ascend_of_gen_ascend a = Ascend a;

value empty_person ip =
  let empty_string = Adef.istr_of_int 0 in
  let p =
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
  in
  Person p
;

value get_family =
  fun
  [ Union u -> u.Def.family
  | Union2 bn i ->
      let pos = get_field_acc bn i ("person", "family") in
      loop [] pos where rec loop list pos =
        if pos = -1 then Array.of_list list
        else
          let (ifam, pos) =
            get_field_2_data bn pos ("person", "family") "data"
          in
          loop [ifam :: list] pos ]
;

value union_of_gen_union u = Union u;

value get_comment =
  fun
  [ Family f -> Istr f.Def.comment
  | Family2 bnc i -> make_istr2 bnc ("family", "comment") i ]
;
value get_divorce =
  fun
  [ Family f -> f.Def.divorce
  | Family2 bn i -> get_field bn i ("family", "divorce") ]
;
value get_fam_index =
  fun
  [ Family f -> f.Def.fam_index
  | Family2 _ i -> Adef.ifam_of_int i ]
;
value get_fsources =
  fun
  [ Family f -> Istr f.Def.fsources
  | Family2 bnc i -> make_istr2 bnc ("family", "fsources") i ]
;
value get_marriage =
  fun
  [ Family f -> f.Def.marriage
  | Family2 bn i -> get_field bn i ("family", "marriage") ]
;
value get_marriage_place =
  fun
  [ Family f -> Istr f.Def.marriage_place
  | Family2 bnc i -> make_istr2 bnc ("family", "marriage_place") i ]
;
value get_marriage_src =
  fun
  [ Family f -> Istr f.Def.marriage_src
  | Family2 bnc i -> make_istr2 bnc ("family", "marriage_src") i ]
;
value get_origin_file =
  fun
  [ Family f -> Istr f.Def.origin_file
  | Family2 bnc i -> make_istr2 bnc ("family", "origin_file") i ]
;
value get_relation =
  fun
  [ Family f -> f.Def.relation
  | Family2 bn i -> get_field bn i ("family", "relation") ]
;
value get_witnesses =
  fun
  [ Family f -> f.Def.witnesses
  | Family2 bn i -> get_field bn i ("family", "witnesses") ]
;

value family_of_gen_family f =
  let f = map_family_ps (fun p -> p) un_istr f in
  Family f
;
value gen_family_of_family =
  fun
  [ Family f -> map_family_ps (fun p -> p) (fun s -> Istr s) f
  | Family2 _ _ as f ->
      {marriage = get_marriage f; marriage_place = get_marriage_place f;
       marriage_src = get_marriage_src f; witnesses = get_witnesses f;
       relation = get_relation f; divorce = get_divorce f;
       comment = get_comment f; origin_file = get_origin_file f;
       fsources = get_fsources f; fam_index = get_fam_index f} ]
;
value get_father =
  fun
  [ Couple c -> Adef.father c
  | Couple2 bn i -> get_field bn i ("family", "father") ]
;
value get_mother =
  fun
  [ Couple c -> Adef.mother c
  | Couple2 bn i -> get_field bn i ("family", "mother") ]
;
value get_parent_array =
  fun
  [ Couple c -> Adef.parent_array c
  | Couple2 bn i ->
      let p1 = get_field bn i ("family", "father") in
      let p2 = get_field bn i ("family", "mother") in
      [| p1; p2 |] ]
;

value couple_of_gen_couple c = Couple c;
value gen_couple_of_couple =
  fun
  [ Couple c -> c
  | Couple2 _ _ as c -> couple (get_father c) (get_mother c) ]
;

value get_children =
  fun
  [ Descend d -> d.Def.children
  | Descend2 bn i -> get_field bn i ("family", "children") ]
;

value descend_of_gen_descend d = Descend d;
value gen_descend_of_descend =
  fun
  [ Descend d -> d
  | Descend2 _ _ as d -> {children = get_children d} ]
;

value poi base i =
  match base with
  [ Base base -> Person (base.data.persons.get (Adef.int_of_iper i))
  | Base2 fn -> Person2 fn (Adef.int_of_iper i) ]
;
value aoi base i =
  match base with
  [ Base base -> Ascend (base.data.ascends.get (Adef.int_of_iper i))
  | Base2 fn -> Ascend2 fn (Adef.int_of_iper i) ]
;
value uoi base i =
  match base with
  [ Base base -> Union (base.data.unions.get (Adef.int_of_iper i))
  | Base2 fn -> Union2 fn (Adef.int_of_iper i) ]
;

value foi base i =
  match base with
  [ Base base -> Family (base.data.families.get (Adef.int_of_ifam i))
  | Base2 fn -> Family2 fn (Adef.int_of_ifam i) ]
;
value coi base i =
  match base with
  [ Base base -> Couple (base.data.couples.get (Adef.int_of_ifam i))
  | Base2 fn -> Couple2 fn (Adef.int_of_ifam i) ]
;
value doi base i =
  match base with
  [ Base base -> Descend (base.data.descends.get (Adef.int_of_ifam i))
  | Base2 fn -> Descend2 fn (Adef.int_of_ifam i) ]
;

value sou base i =
  match (base, i) with
  [ (Base base, Istr i) -> base.data.strings.get (Adef.int_of_istr i)
  | (Base2 _, Istr2 bn f pos) -> get_field_data bn pos f "data"
  | _ -> assert False ]
;

value nb_of_persons base =
  match base with
  [ Base base -> base.data.persons.len
  | Base2 (dir, _) ->
      let fname =
        List.fold_left Filename.concat dir ["person"; "sex"; "access"]
      in
      let st = Unix.lstat fname in
      st.Unix.st_size / 4 ]
;
value nb_of_families base =
  match base with
  [ Base base -> base.data.families.len
  | Base2 (dir, _) ->
      let fname =
        List.fold_left Filename.concat dir ["family"; "marriage"; "access"]
      in
      let st = Unix.lstat fname in
      st.Unix.st_size / 4 ]
;

value patch_person base ip p =
  match (base, p) with
  [ (Base base, Person p) -> base.func.patch_person ip p
  | (Base2 _, _) -> failwith "not impl patch_person"
  | _ -> assert False ]
;
value patch_ascend base ip a =
  match (base, a) with
  [ (Base base, Ascend a) -> base.func.patch_ascend ip a
  | (Base2 _, _) -> failwith "not impl patch_ascend"
  | _ -> assert False ]
;
value patch_union base ip u =
  match (base, u) with
  [ (Base base, Union u) -> base.func.patch_union ip u
  | _ -> failwith "not impl patch_union" ]
;
value patch_family base ifam f =
  match (base, f) with
  [ (Base base, Family f) -> base.func.patch_family ifam f
  | _ -> failwith "not impl patch_family" ]
;
value patch_descend base ifam d =
  match (base, d) with
  [ (Base base, Descend d) -> base.func.patch_descend ifam d
  | _ -> failwith "not impl patch_descend" ]
;
value patch_couple base ifam c =
  match (base, c) with
  [ (Base base, Couple c) -> base.func.patch_couple ifam c
  | _ -> failwith "not impl patch_couple" ]
;
value patch_name base =
  match base with
  [ Base base -> base.func.patch_name
  | Base2 _ -> failwith "not impl patch_name" ]
;
value insert_string base s =
  match base with
  [ Base base -> Istr (base.func.insert_string s)
  | Base2 _ -> failwith "not impl insert_string" ]
;
value commit_patches base =
  match base with
  [ Base base -> base.func.commit_patches ()
  | Base2 _ -> failwith "not impl commit_patches" ]
;
value commit_notes base =
  match base with
  [ Base base -> base.func.commit_notes
  | Base2 _ -> failwith "not impl commit_notes" ]
;
value is_patched_person base =
  match base with
  [ Base base -> base.func.is_patched_person
  | Base2 _ -> failwith "not impl is_patched_person" ]
;
value patched_ascends base =
  match base with
  [ Base base -> base.func.patched_ascends ()
  | Base2 _ -> failwith "not impl patched_ascends" ]
;

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

value person2_of_key (dir, _) fn sn oc =
  let person_of_key_d = Filename.concat dir "person_of_key" in
  try do {
    let ifn =
      let fn = Name.lower (nominative fn) in
      hashtbl_find person_of_key_d "istr_of_string.ht" fn
    in
    let isn =
      let sn = Name.lower (nominative sn) in
      hashtbl_find person_of_key_d "istr_of_string.ht" sn
    in
    let key = (ifn, isn, oc) in
    Some (key_hashtbl_find person_of_key_d "iper_of_key.ht" key : iper)
  }
  with
  [ Not_found -> None ]
;

value strings2_of_fsname (bn, _) f s =
  let k = Name.crush_lower s in
  let dir = List.fold_left Filename.concat bn ["person"; f] in
  hashtbl_find_all dir "string_of_crush.ht" k
;

value persons2_of_name (bn, _) s =
  let dir = Filename.concat bn "person_of_name" in
  hashtbl_find_all dir "person_of_name.ht" (Name.crush_lower s)
;

value person_of_key base =
  match base with
  [ Base base -> base.func.person_of_key
  | Base2 bn -> person2_of_key bn ]
;
value persons_of_name base =
  match base with
  [ Base base -> base.func.persons_of_name
  | Base2 bnc -> persons2_of_name bnc ]
;
value persons_of_first_name base =
  match base with
  [ Base base -> Spi base.func.persons_of_first_name
  | Base2 _ -> Spi2 ]
;
value persons_of_surname base =
  match base with
  [ Base base -> Spi base.func.persons_of_surname
  | Base2 _ -> Spi2 ]
;

value spi_cursor spi s =
  match spi with
  [ Spi spi -> Istr (spi.cursor s)
  | Spi2 -> failwith "not impl spi_cursor" ]
;
value spi_find spi s =
  match (spi, s) with
  [ (Spi spi, Istr s) -> spi.find s
  | (Spi2, Istr2 (bn, _) (f1, f2) pos) -> do {
      let dir = List.fold_left Filename.concat bn [f1; f2] in
      hashtbl_find_all dir "person_of_string.ht" pos
    }
  | _ -> failwith "not impl spi_find" ]
;
value spi_next spi s =
  match (spi, s) with
  [ (Spi spi, Istr s) -> Istr (spi.next s)
  | _ -> failwith "not impl spi_next" ]
;

value base_visible_get base f =
  match base with
  [ Base base -> base.data.visible.v_get (fun p -> f (Person p))
  | Base2 _ -> failwith "not impl visible_get" ]
;
value base_visible_write base =
  match base with
  [ Base base -> base.data.visible.v_write ()
  | Base2 _ -> failwith "not impl visible_write" ]
;
value base_particles base =
  match base with
  [ Base base -> base.data.particles
  | Base2 (bn, _) ->
      Mutil.input_particles (Filename.concat bn "../particles.txt") ]

;

value base_strings_of_first_name base s =
  match base with
  [ Base base -> List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  | Base2 bnc ->
      let posl = strings2_of_fsname bnc "first_name" s in
      List.map (fun pos -> Istr2 bnc ("person", "first_name") pos) posl ]
;
value base_strings_of_surname base s =
  match base with
  [ Base base -> List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  | Base2 bnc ->
      let posl = strings2_of_fsname bnc "surname" s in
      List.map (fun pos -> Istr2 bnc ("person", "surname") pos) posl ]
;
value base_cleanup base =
  match base with
  [ Base base -> base.func.cleanup ()
  | Base2 (bn, cache) ->
      List.iter (fun ((f1, f2, f), ic) -> close_in ic) cache.chan ]
;

value load_ascends_array base =
  match base with
  [ Base base -> base.data.ascends.load_array ()
  | Base2 _ -> failwith "not impl load_ascends_array" ]
;
value load_unions_array base =
  match base with
  [ Base base -> base.data.unions.load_array ()
  | Base2 _ -> failwith "not impl load_unions_array" ]
;
value load_couples_array base =
  match base with
  [ Base base -> base.data.couples.load_array ()
  | Base2 _ -> failwith "not impl load_couples_array" ]
;
value load_descends_array base =
  match base with
  [ Base base -> base.data.descends.load_array ()
  | Base2 _ -> failwith "not impl load_descends_array" ]
;
value load_strings_array base =
  match base with
  [ Base base -> base.data.strings.load_array ()
  | Base2 _ -> failwith "not impl load_string_array" ]
;

value persons_array base =
  match base with
  [ Base base ->
      let get i = Person (base.data.persons.get i) in
      let set i =
        fun
        [ Person p -> base.data.persons.set i p
        | Person2 _ _ -> assert False ]
      in
      (get, set)
  | Base2 _ -> failwith "not impl persons_array" ]
;
value ascends_array base =
  match base with
  [ Base base ->
      let get i = Ascend (base.data.ascends.get i) in
      let set i =
        fun
        [ Ascend a -> base.data.ascends.set i a
        | Ascend2 _ _ -> assert False ]
      in
      (get, set)
  | Base2 _ -> failwith "not impl ascends_array" ]
;

value read_notes bname fnotes rn_mode =
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
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
value base_notes_read base fn =
  match base with
  [ Base base -> base.data.bnotes.nread fn RnAll
  | Base2 (bn, _) -> read_notes (Filename.dirname bn) fn RnAll ]
;
value base_notes_read_first_line base fn =
  match base with
  [ Base base -> base.data.bnotes.nread fn Rn1Ln
  | Base2 (bn, _) -> read_notes (Filename.dirname bn) fn Rn1Ln ]
;
value base_notes_are_empty base fn =
  match base with
  [ Base base -> base.data.bnotes.nread fn RnDeg = ""
  | Base2 (bn, _) -> read_notes (Filename.dirname bn) fn RnDeg = "" ]
;
value base_notes_origin_file base =
  match base with
  [ Base base -> base.data.bnotes.norigin_file
  | Base2 _ -> failwith "not impl base_notes_origin_file" ]
;

value p_first_name base p = nominative (sou base (get_first_name p));
value p_surname base p = nominative (sou base (get_surname p));

value nobtit conf base p =
  let list = get_titles p in
  match Lazy.force conf.allowed_titles with
  [ [] -> list
  | allowed_titles ->
      List.fold_right
        (fun t l ->
           let id = sou base t.t_ident in
           let pl = sou base t.t_place in
           if List.mem (id ^ "/" ^ pl) allowed_titles then [t :: l] else l)
        list [] ]
;

value husbands base p =
  let u = uoi base (get_key_index p) in
  List.map
    (fun ifam ->
       let cpl = coi base ifam in
       let husband = poi base (get_father cpl) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) (get_surnames_aliases husband)
       in
       (husband_surname, husband_surnames_aliases))
    (Array.to_list (get_family u))
;

value father_titles_places base p nobtit =
  match get_parents (aoi base (get_key_index p)) with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (get_father cpl) in
      List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> [] ]
;

value person_misc_names base p tit =
  let sou = sou base in
  Futil.gen_person_misc_names (sou (get_first_name p))
    (sou (get_surname p)) (sou (get_public_name p))
    (List.map sou (get_qualifiers p)) (List.map sou (get_aliases p))
    (List.map sou (get_first_names_aliases p))
    (List.map sou (get_surnames_aliases p))
    (List.map (Futil.map_title_strings sou) (tit p))
    (if get_sex p = Female then husbands base p else [])
    (father_titles_places base p tit)
;

value base_of_dsk_base base = Base base;
value apply_as_dsk_base f base =
  match base with
  [ Base base -> f base
  | Base2 _ -> failwith "not impl apply_as_dsk_base" ]
;

value base_of_base2 bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  Base2 (Filename.concat bname "base_d", {chan = []})
;

value dsk_person_of_person =
  fun
  [ Person p -> p
  | Person2 _ _ -> failwith "not impl dsk_person_of_person" ]
;
