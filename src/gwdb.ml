(* $Id: gwdb.ml,v 5.90 2006-10-31 14:01:42 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Adef;
open Config;
open Dbdisk;
open Def;
open Futil;
open Mutil;
open Printf;

type patches =
  { max_per : mutable int;
    max_fam : mutable int;
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
  { bdir : string;
    cache_chan : Hashtbl.t (string * string * string) in_channel;
    patches : patches;
    parents_array : mutable option (array (option ifam));
    consang_array : mutable option (array Adef.fix);
    father_array : mutable option (array iper);
    mother_array : mutable option (array iper) }
;

type istr =
  [ Istr of dsk_istr
  | Istr2 of db2 and (string * string) and int
  | Istr2New of db2 and string ]
;

type person =
  [ Person of dsk_person
  | Person2 of db2 and int
  | Person2Gen of db2 and gen_person iper string ]
;
type ascend =
  [ Ascend of dsk_ascend
  | Ascend2 of db2 and int
  | Ascend2Gen of db2 and gen_ascend ifam ]
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

type string_person_index =
  [ Spi of gen_string_person_index dsk_istr
  | Spi2 of bool ]
;

type base =
  [ Base of Dbdisk.dsk_base
  | Base2 of db2 ]
;

value eq_istr i1 i2 =
  match (i1, i2) with
  [ (Istr i1, Istr i2) -> Adef.int_of_istr i1 = Adef.int_of_istr i2
  | (Istr2 _ (f11, f12) i1, Istr2 _ (f21, f22) i2) ->
      i1 = i2 && f11 = f21 && f12 = f22
  | (Istr2New _ s1, Istr2New _ s2) -> s1 = s2
  | _ -> failwith "eq_istr" ]
;

value get_field_acc db2 i (f1, f2) = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, "access") with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir [f1; f2; "access"])
        in
        Hashtbl.add db2.cache_chan (f1, f2, "access") ic;
        ic
      } ]
  in
(*
let _ = do { Printf.eprintf "acc %d %s %s \n" i f1 f2; flush stderr; } in
*)
  seek_in ic (4 * i);
  input_binary_int ic
};

value get_field_data db2 pos (f1, f2) data = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, data) with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir [f1; f2; data])
        in
        Hashtbl.add db2.cache_chan (f1, f2, data) ic;
        ic
      } ]
  in
(*
let _ = do { Printf.eprintf "dat %d %s %s \n" pos f1 f2; flush stderr; } in
*)
  seek_in ic pos;
  Iovalue.input ic
};

value get_field_2_data db2 pos (f1, f2) data = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, data) with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir [f1; f2; data])
        in
        Hashtbl.add db2.cache_chan (f1, f2, data) ic;
        ic
      } ]
  in
(*
let _ = do { Printf.eprintf "dat2 %d %s %s \n" pos f1 f2; flush stderr; } in
*)
  seek_in ic pos;
  let r = Iovalue.input ic in
  let s = Iovalue.input ic in
  (r, s)
};

value get_field db2 i path =
  let pos = get_field_acc db2 i path in
  get_field_data db2 pos path "data"
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

value get_access =
  fun
  [ Person p -> p.Def.access
  | Person2 db2 i -> get_field db2 i ("person", "access")
  | Person2Gen db2 p -> p.Def.access ]
;
value get_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.aliases
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "aliases") in
      if pos = -1 then []
      else
        let list = get_field_data db2 pos ("person", "aliases") "data2.ext" in
        List.map (fun pos -> Istr2 db2 ("person", "aliases") pos) list
  | Person2Gen db2 p -> List.map (fun s -> Istr2New db2 s) p.Def.aliases ]
;
value get_baptism =
  fun
  [ Person p -> p.Def.baptism
  | Person2 db2 i -> get_field db2 i ("person", "baptism")
  | Person2Gen db2 p -> p.Def.baptism ]
;
value get_baptism_place =
  fun
  [ Person p -> Istr p.Def.baptism_place
  | Person2 db2 i -> make_istr2 db2 ("person", "baptism_place") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.baptism_place ]
;
value get_baptism_src =
  fun
  [ Person p -> Istr p.Def.baptism_src
  | Person2 db2 i -> make_istr2 db2 ("person", "baptism_src") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.baptism_src ]
;
value get_birth =
  fun
  [ Person p -> p.Def.birth
  | Person2 db2 i -> get_field db2 i ("person", "birth")
  | Person2Gen db2 p -> p.Def.birth ]
;
value get_birth_place =
  fun
  [ Person p -> Istr p.Def.birth_place
  | Person2 db2 i -> make_istr2 db2 ("person", "birth_place") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.birth_place ]
;
value get_birth_src =
  fun
  [ Person p -> Istr p.Def.birth_src
  | Person2 db2 i -> make_istr2 db2 ("person", "birth_src") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.birth_src ]
;
value get_burial =
  fun
  [ Person p -> p.Def.burial
  | Person2 db2 i -> get_field db2 i ("person", "burial")
  | Person2Gen db2 p -> p.Def.burial ]
;
value get_burial_place =
  fun
  [ Person p -> Istr p.Def.burial_place
  | Person2 db2 i -> make_istr2 db2 ("person", "burial_place") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.burial_place ]
;
value get_burial_src =
  fun
  [ Person p -> Istr p.Def.burial_src
  | Person2 db2 i -> make_istr2 db2 ("person", "burial_src") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.burial_src ]
;
value get_death =
  fun
  [ Person p -> p.Def.death
  | Person2 db2 i -> get_field db2 i ("person", "death")
  | Person2Gen db2 p -> p.Def.death ]
;
value get_death_place =
  fun
  [ Person p -> Istr p.Def.death_place
  | Person2 db2 i -> make_istr2 db2 ("person", "death_place") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.death_place ]
;
value get_death_src =
  fun
  [ Person p -> Istr p.Def.death_src
  | Person2 db2 i -> make_istr2 db2 ("person", "death_src") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.death_src ]
;
value get_first_name =
  fun
  [ Person p -> Istr p.Def.first_name
  | Person2 db2 i -> make_istr2 db2 ("person", "first_name") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.first_name ]
;
value get_first_names_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.first_names_aliases
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "first_names_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "first_names_aliases") "data2.ext"
        in
        List.map (fun pos -> Istr2 db2 ("person", "first_names_aliases") pos)
          list
  | Person2Gen db2 p ->
      List.map (fun s -> Istr2New db2 s) p.Def.first_names_aliases ]
;
value get_image =
  fun
  [ Person p -> Istr p.Def.image
  | Person2 db2 i -> make_istr2 db2 ("person", "image") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.image ]
;
value get_key_index =
  fun
  [ Person p -> p.Def.key_index
  | Person2 _ i -> Adef.iper_of_int i
  | Person2Gen db2 p -> p.Def.key_index ]
;
value get_notes =
  fun
  [ Person p -> Istr p.Def.notes
  | Person2 db2 i -> make_istr2 db2 ("person", "notes") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.notes ]
;
value get_occ =
  fun
  [ Person p -> p.Def.occ
  | Person2 db2 i -> get_field db2 i ("person", "occ")
  | Person2Gen db2 p -> p.Def.occ ]
;
value get_occupation =
  fun
  [ Person p -> Istr p.Def.occupation
  | Person2 db2 i -> make_istr2 db2 ("person", "occupation") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.occupation ]
;
value get_psources =
  fun
  [ Person p -> Istr p.Def.psources
  | Person2 db2 i -> make_istr2 db2 ("person", "psources") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.psources ]
;
value get_public_name =
  fun
  [ Person p -> Istr p.Def.public_name
  | Person2 db2 i -> make_istr2 db2 ("person", "public_name") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.public_name ]
;
value get_qualifiers =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.qualifiers
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "qualifiers") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "qualifiers") "data2.ext"
        in
        List.map (fun pos -> Istr2 db2 ("person", "qualifiers") pos) list
  | Person2Gen db2 p -> List.map (fun s -> Istr2New db2 s) p.Def.qualifiers ]
;
value get_related =
  fun
  [ Person p -> p.Def.related
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "related") in
      loop [] pos where rec loop list pos =
        if pos = -1 then List.rev list
        else
          let (ip, pos) =
            get_field_2_data db2 pos ("person", "related") "data"
          in
          loop [ip :: list] pos
  | Person2Gen db2 p -> p.Def.related ]
;
value get_rparents =
  fun
  [ Person p ->
      List.map (map_relation_ps (fun x -> x) (fun i -> Istr i)) p.Def.rparents
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "rparents") in
      if pos = -1 then []
      else
        let rl = get_field_data db2 pos ("person", "rparents") "data" in
        List.map (* field "r_sources" is actually unused *)
          (map_relation_ps (fun x -> x) (fun _ -> Istr2 db2 ("", "") (-1)))
          rl
  | Person2Gen db2 p ->
      List.map (map_relation_ps (fun x -> x) (fun s -> Istr2New db2 s))
        p.Def.rparents ]
;
value get_sex =
  fun
  [ Person p -> p.Def.sex
  | Person2 db2 i -> get_field db2 i ("person", "sex")
  | Person2Gen db2 p -> p.Def.sex ]
;
value get_surname =
  fun
  [ Person p -> Istr p.Def.surname
  | Person2 db2 i -> make_istr2 db2 ("person", "surname") i
  | Person2Gen db2 p -> Istr2New db2 p.Def.surname ]
;
value get_surnames_aliases =
  fun
  [ Person p -> List.map (fun i -> Istr i) p.Def.surnames_aliases
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "surnames_aliases") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "surnames_aliases") "data2.ext"
        in
        List.map
          (fun pos -> Istr2 db2 ("person", "surnames_aliases") pos) list
  | Person2Gen db2 p ->
      List.map (fun s -> Istr2New db2 s) p.Def.surnames_aliases ]
;
value get_titles =
  fun
  [ Person p ->
      List.map (fun t -> map_title_strings (fun i -> Istr i) t)
        p.Def.titles
  | Person2 db2 i ->
      let pos = get_field_acc db2 i ("person", "titles") in
      if pos = -1 then []
      else
        let list =
          get_field_data db2 pos ("person", "titles") "data2.ext"
        in
        List.map
          (map_title_strings (fun pos -> Istr2 db2 ("person", "titles") pos))
          list
  | Person2Gen db2 p ->
      List.map (fun t -> map_title_strings (fun s -> Istr2New db2 s) t)
        p.Def.titles ]
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
  | Person2 _ _ -> failwith "not impl person_with_key"
  | Person2Gen _ _ -> failwith "not impl person_with_key (gen)" ]
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

value person_with_rparents p r =
  match p with
  [ Person p ->
      let r = List.map (map_relation_ps (fun p -> p) un_istr) r in
      Person {(p) with rparents = r}
  | Person2 _ _ -> failwith "not impl person_with_rparents"
  | Person2Gen _ _ -> failwith "not impl person_with_rparents (gen)" ]
;
value person_with_sex p s =
  match p with
  [ Person p -> Person {(p) with sex = s}
  | Person2 _ _ -> failwith "not impl person_with_sex"
  | Person2Gen db2 p -> Person2Gen db2 {(p) with sex = s} ]
;

value person_of_gen_person base p =
  match base with
  [ Base _ -> Person (map_person_ps (fun p -> p) un_istr p)
  | Base2 db2 -> Person2Gen db2 (map_person_ps (fun p -> p) un_istr2 p) ]
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
       key_index = get_key_index p}
  | Person2Gen db2 p ->
      map_person_ps (fun p -> p) (fun s -> Istr2New db2 s) p ]
;

value no_consang = Adef.fix (-1);

value get_consang =
  fun
  [ Ascend a -> a.Def.consang
  | Ascend2 db2 i ->
      match db2.consang_array with
      [ Some tab -> tab.(i)
      | None ->
          try get_field db2 i ("person", "consang") with
          [ Sys_error _ -> no_consang ] ]
  | Ascend2Gen _ a -> a.Def.consang ]
;

value get_parents =
  fun
  [ Ascend a -> a.Def.parents
  | Ascend2 db2 i ->
      match db2.parents_array with
      [ Some tab -> tab.(i)
      | None ->
          let pos = get_field_acc db2 i ("person", "parents") in
          if pos = -1 then None
          else Some (get_field_data db2 pos ("person", "parents") "data") ]
  | Ascend2Gen _ a -> a.Def.parents ]
;

value ascend_of_gen_ascend base a =
  match base with
  [ Base _ -> Ascend a
  | Base2 db2 -> Ascend2Gen db2 a ]
;

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
  | Union2 db2 i ->
      let pos = get_field_acc db2 i ("person", "family") in
      loop [] pos where rec loop list pos =
        if pos = -1 then Array.of_list list
        else
          let (ifam, pos) =
            get_field_2_data db2 pos ("person", "family") "data"
          in
          loop [ifam :: list] pos
  | Union2Gen db2 u -> u.Def.family ]
;

value union_of_gen_union base u =
  match base with
  [ Base _ -> Union u
  | Base2 db2 -> Union2Gen db2 u ]
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
value get_fam_index =
  fun
  [ Family f -> f.Def.fam_index
  | Family2 _ i -> Adef.ifam_of_int i
  | Family2Gen db2 f -> f.Def.fam_index ]
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

value family_of_gen_family base f =
  match base with
  [ Base _ -> Family (map_family_ps (fun p -> p) un_istr f)
  | Base2 db2 -> Family2Gen db2 (map_family_ps (fun p -> p) un_istr2 f) ]
;

value gen_family_of_family =
  fun
  [ Family f -> map_family_ps (fun p -> p) (fun s -> Istr s) f
  | Family2 _ _ as f ->
      {marriage = get_marriage f; marriage_place = get_marriage_place f;
       marriage_src = get_marriage_src f; witnesses = get_witnesses f;
       relation = get_relation f; divorce = get_divorce f;
       comment = get_comment f; origin_file = get_origin_file f;
       fsources = get_fsources f; fam_index = get_fam_index f}
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

value couple_of_gen_couple base c =
  match base with
  [ Base _ -> Couple c
  | Base2 db2 -> Couple2Gen db2 c ]
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
  | Descend2 db2 i -> get_field db2 i ("family", "children")
  | Descend2Gen db2 d -> d.Def.children ]
;

value descend_of_gen_descend base d =
  match base with
  [ Base _ -> Descend d
  | Base2 db2 -> Descend2Gen db2 d ]
;

value gen_descend_of_descend =
  fun
  [ Descend d -> d
  | Descend2 _ _ as d -> {children = get_children d}
  | Descend2Gen db2 d -> d ]
;

value poi base i =
  match base with
  [ Base base -> Person (base.data.persons.get (Adef.int_of_iper i))
  | Base2 db2 ->
      try Person2Gen db2 (Hashtbl.find db2.patches.h_person i) with
      [ Not_found -> Person2 db2 (Adef.int_of_iper i) ] ]
;
value aoi base i =
  match base with
  [ Base base -> Ascend (base.data.ascends.get (Adef.int_of_iper i))
  | Base2 db2 ->
      try Ascend2Gen db2 (Hashtbl.find db2.patches.h_ascend i) with
      [ Not_found -> Ascend2 db2 (Adef.int_of_iper i) ] ]
;
value uoi base i =
  match base with
  [ Base base -> Union (base.data.unions.get (Adef.int_of_iper i))
  | Base2 db2 ->
      try Union2Gen db2 (Hashtbl.find db2.patches.h_union i) with
      [ Not_found -> Union2 db2 (Adef.int_of_iper i) ] ]
;

value foi base i =
  match base with
  [ Base base -> Family (base.data.families.get (Adef.int_of_ifam i))
  | Base2 db2 ->
      try Family2Gen db2 (Hashtbl.find db2.patches.h_family i) with
      [ Not_found -> Family2 db2 (Adef.int_of_ifam i) ] ]
;
value coi base i =
  match base with
  [ Base base -> Couple (base.data.couples.get (Adef.int_of_ifam i))
  | Base2 db2 ->
      try Couple2Gen db2 (Hashtbl.find db2.patches.h_couple i) with
      [ Not_found -> Couple2 db2 (Adef.int_of_ifam i) ] ]
;
value doi base i =
  match base with
  [ Base base -> Descend (base.data.descends.get (Adef.int_of_ifam i))
  | Base2 db2 ->
      try Descend2Gen db2 (Hashtbl.find db2.patches.h_descend i) with
      [ Not_found -> Descend2 db2 (Adef.int_of_ifam i) ] ]
;

value sou base i =
  match (base, i) with
  [ (Base base, Istr i) -> base.data.strings.get (Adef.int_of_istr i)
  | (Base2 _, Istr2 db2 f pos) ->
      if pos = -1 || pos = Db2.empty_string_pos then ""
      else get_field_data db2 pos f "data"
  | (Base2 _, Istr2New db2 s) -> s
  | _ -> assert False ]
;

value nb_of_persons base =
  match base with
  [ Base base -> base.data.persons.len
  | Base2 db2 ->
      if db2.patches.max_per > 0 then db2.patches.max_per
      else
        let fname =
          List.fold_left Filename.concat db2.bdir ["person"; "sex"; "access"]
        in
        let st = Unix.lstat fname in
        st.Unix.st_size / 4 ]
;

value nb_of_families base =
  match base with
  [ Base base -> base.data.families.len
  | Base2 db2 ->
      if db2.patches.max_fam > 0 then db2.patches.max_fam
      else
        let fname =
          List.fold_left Filename.concat db2.bdir
            ["family"; "marriage"; "access"]
        in
        let st = Unix.lstat fname in
        st.Unix.st_size / 4 ]
;

value patch_person base ip p =
  match (base, p) with
  [ (Base base, Person p) -> base.func.patch_person ip p
  | (Base2 _, Person2Gen db2 p) -> do {
      Hashtbl.replace db2.patches.h_person ip p;
      db2.patches.max_per :=
        max (Adef.int_of_iper ip + 1) db2.patches.max_per;
    }
  | _ -> assert False ]
;
value patch_ascend base ip a =
  match (base, a) with
  [ (Base base, Ascend a) -> base.func.patch_ascend ip a
  | (Base2 _, Ascend2Gen db2 a) -> do {
      Hashtbl.replace db2.patches.h_ascend ip a;
      db2.patches.max_per :=
        max (Adef.int_of_iper ip + 1) db2.patches.max_per;
    }
  | _ -> assert False ]
;
value patch_union base ip u =
  match (base, u) with
  [ (Base base, Union u) -> base.func.patch_union ip u
  | (Base2 _, Union2Gen db2 u) -> do {
      Hashtbl.replace db2.patches.h_union ip u;
      db2.patches.max_per :=
        max (Adef.int_of_iper ip + 1) db2.patches.max_per;
    }
  | _ -> failwith "not impl patch_union" ]
;

value patch_family base ifam f =
  match (base, f) with
  [ (Base base, Family f) -> base.func.patch_family ifam f
  | (Base2 _, Family2Gen db2 f) -> do {
      Hashtbl.replace db2.patches.h_family ifam f;
      db2.patches.max_fam :=
        max (Adef.int_of_ifam ifam + 1) db2.patches.max_fam;
    }
  | _ -> failwith "not impl patch_family" ]
;
value patch_descend base ifam d =
  match (base, d) with
  [ (Base base, Descend d) -> base.func.patch_descend ifam d
  | (Base2 _, Descend2Gen db2 d) -> do {
      Hashtbl.replace db2.patches.h_descend ifam d;
      db2.patches.max_fam :=
        max (Adef.int_of_ifam ifam + 1) db2.patches.max_fam;
    }
  | _ -> failwith "not impl patch_descend" ]
;
value patch_couple base ifam c =
  match (base, c) with
  [ (Base base, Couple c) -> base.func.patch_couple ifam c
  | (Base2 _, Couple2Gen db2 c) -> do {
      Hashtbl.replace db2.patches.h_couple ifam c;
      db2.patches.max_fam :=
        max (Adef.int_of_ifam ifam + 1) db2.patches.max_fam;
    }
  | _ -> failwith "not impl patch_couple" ]
;

value patch_name base s ip =
  match base with
  [ Base base -> base.func.patch_name s ip
  | Base2 db2 ->
      let s = Name.crush_lower s in
      let ht = db2.patches.h_name in
      try
        let ipl = Hashtbl.find ht s in
        if List.mem ip ipl then () else Hashtbl.replace ht s [ip :: ipl]
      with
      [ Not_found -> Hashtbl.add ht s [ip] ] ]

;

value patch_key base ip fn sn occ =
  match base with
  [ Base _ -> ()
  | Base2 db2 ->
      let fn = Name.lower (nominative fn) in
      let sn = Name.lower (nominative sn) in
      Hashtbl.replace db2.patches.h_key (fn, sn, occ) ip ]
;

value insert_string base s =
  match base with
  [ Base base -> Istr (base.func.insert_string s)
  | Base2 db2 -> Istr2New db2 s ]
;

value commit_patches base =
  match base with
  [ Base base -> base.func.commit_patches ()
  | Base2 db2 -> do {
      let fname = Filename.concat db2.bdir "patches" in
      let oc = open_out_bin (fname ^ "1") in
      output_value oc db2.patches;
      close_out oc;
      remove_file (fname ^ "~");
      try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
      Sys.rename (fname ^ "1") fname
    } ]
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
  | Base2 _ ->
      let _ = do { eprintf "not impl patched_ascends\n"; flush stderr; } in
      [] ]
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

value person_of_key base =
  match base with
  [ Base base -> base.func.person_of_key
  | Base2 db2 -> person2_of_key db2 ]
;
value persons_of_name base =
  match base with
  [ Base base -> base.func.persons_of_name
  | Base2 db2 -> persons2_of_name db2 ]
;
value persons_of_first_name base =
  match base with
  [ Base base -> Spi base.func.persons_of_first_name
  | Base2 _ -> Spi2 True ]
;
value persons_of_surname base =
  match base with
  [ Base base -> Spi base.func.persons_of_surname
  | Base2 _ -> Spi2 False ]
;

value spi_cursor spi s =
  match spi with
  [ Spi spi -> Istr (spi.cursor s)
  | Spi2 _ -> failwith "not impl spi_cursor" ]
;

value spi_find spi s =
  match (spi, s) with
  [ (Spi spi, Istr s) -> spi.find s
  | (Spi2 _, Istr2 db2 (f1, f2) pos) -> do {
      let dir = List.fold_left Filename.concat db2.bdir [f1; f2] in
      hashtbl_find_all dir "person_of_string.ht" pos
    }
  | (Spi2 is_first_name, Istr2New db2 s) ->
      let proj =
        if is_first_name then fun p -> p.first_name else fun p -> p.surname
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
  | Base2 db2 ->
      Mutil.input_particles (Filename.concat db2.bdir "../particles.txt") ]
;

value base_strings_of_first_name_or_surname base field proj s =
  match base with
  [ Base base -> List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  | Base2 db2 ->
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
        db2.patches.h_key istrl ]
;

value base_strings_of_first_name base s =
  base_strings_of_first_name_or_surname base "first_name"
    (fun p -> p.first_name) s
;

value base_strings_of_surname base s =
  base_strings_of_first_name_or_surname base "surname"
    (fun p -> p.surname) s
;

value load_array2 bdir nb f1 f2 get =
  if nb = 0 then [| |]
  else do {
(*
let _ = do { eprintf "start load %s arr\n" f2; flush stderr; } in
*)
    let ic_acc =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "access"])
    in
    let ic_dat =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "data"])
    in
    let tab = Array.create nb (get ic_dat (input_binary_int ic_acc)) in
    for i = 1 to nb - 1 do {
      tab.(i) := get ic_dat (input_binary_int ic_acc);
    };
    close_in ic_dat;
    close_in ic_acc;
(*
let _ = do { eprintf "load %s ok\n" f2; flush stderr; } in
*)
    tab
  }
;

value parents_array2 db2 nb =
  load_array2 db2.bdir nb "person" "parents"
    (fun ic_dat pos ->
       if pos = -1 then None
       else do {
         seek_in ic_dat pos;
         Some (Iovalue.input ic_dat : ifam)
       })
;

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

value load_ascends_array base =
  match base with
  [ Base base -> base.data.ascends.load_array ()
  | Base2 db2 -> do {
     let nb = nb_of_persons base in
      match db2.parents_array with
      [ Some _ -> ()
      | None -> db2.parents_array := Some (parents_array2 db2 nb) ];
      match db2.consang_array with
      [ Some _ -> ()
      | None -> db2.consang_array := Some (consang_array2 db2 nb) ];
    } ]
;

value load_unions_array base =
  match base with
  [ Base base -> base.data.unions.load_array ()
  | Base2 _ -> () ]
;

value load_couples_array base =
  match base with
  [ Base base -> base.data.couples.load_array ()
  | Base2 db2 -> do {
      let nb = nb_of_families base in
      match db2.father_array with
      [ Some _ -> ()
      | None ->
          let tab =
            load_array2 db2.bdir nb "family" "father"
              (fun ic_dat pos -> do {
                 seek_in ic_dat pos;
                 Iovalue.input ic_dat
               })
          in
          db2.father_array := Some tab ];
      match db2.mother_array with
      [ Some _ -> ()
      | None ->
          let tab =
            load_array2 db2.bdir nb "family" "mother"
              (fun ic_dat pos -> do {
                 seek_in ic_dat pos;
                 Iovalue.input ic_dat
               })
          in
          db2.mother_array := Some tab ]
    } ]
;

value load_descends_array base =
  match base with
  [ Base base -> base.data.descends.load_array ()
  | Base2 _ -> () ]
;

value load_strings_array base =
  match base with
  [ Base base -> base.data.strings.load_array ()
  | Base2 _ -> () ]
;

value persons_array base =
  match base with
  [ Base base ->
      let get i = Person (base.data.persons.get i) in
      let set i =
        fun
        [ Person p -> base.data.persons.set i p
        | Person2 _ _ -> assert False
        | Person2Gen _ _ -> assert False ]
      in
      (get, set)
  | Base2 _ -> failwith "not impl persons_array" ]
;

value ascends_array base =
  match base with
  [ Base base ->
      let fget i = (base.data.ascends.get i).parents in
      let cget i = (base.data.ascends.get i).consang in
      let cset i v =
        base.data.ascends.set i
          {(base.data.ascends.get i) with consang = v}
      in
      (fget, cget, cset, None)
  | Base2 db2 ->
      let nb = nb_of_persons base in
      let cg_tab =
        match db2.consang_array with
        [ Some tab -> tab
        | None -> consang_array2 db2 nb ]
      in
      let fget i = get_parents (Ascend2 db2 i) in
      let cget i = cg_tab.(i) in
      let cset i v = cg_tab.(i) := v in
      (fget, cget, cset, Some cg_tab) ]
;

value output_consang_tab base tab =
  match base with
  [ Base _ -> do { eprintf "error Gwdb.output_consang_tab\n"; flush stdout }
  | Base2 db2 -> do {
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
    } ]
;

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
value base_notes_read base db2 =
  match base with
  [ Base base -> base.data.bnotes.nread db2 RnAll
  | Base2 {bdir = bn} -> read_notes (Filename.dirname bn) db2 RnAll ]
;
value base_notes_read_first_line base db2 =
  match base with
  [ Base base -> base.data.bnotes.nread db2 Rn1Ln
  | Base2 {bdir = bn} -> read_notes (Filename.dirname bn) db2 Rn1Ln ]
;
value base_notes_are_empty base db2 =
  match base with
  [ Base base -> base.data.bnotes.nread db2 RnDeg = ""
  | Base2 {bdir = bn} -> read_notes (Filename.dirname bn) db2 RnDeg = "" ]
;

value base_notes_origin_file base =
  match base with
  [ Base base -> base.data.bnotes.norigin_file
  | Base2 db2 ->
      let fname = Filename.concat db2.bdir "notes_of.txt" in
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic -> do {
          let r = input_line ic in
          close_in ic;
          r
        }
      | None -> "" ] ]
;

value base_notes_dir base =
  match base with
  [ Base _ -> "notes_d"
  | Base2 _ -> Filename.concat "base_d" "notes_d" ]
;

value base_wiznotes_dir base =
  match base with
  [ Base _ -> "wiznotes"
  | Base2 _ -> Filename.concat "base_d" "wiznotes_d" ]
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

value dsk_person_of_person =
  fun
  [ Person p -> p
  | Person2 _ _ -> failwith "not impl dsk_person_of_person"
  | Person2Gen _ _ -> failwith "not impl dsk_person_of_person (gen)" ]
;

value base_of_base2 bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let bdir = Filename.concat bname "base_d" in
  let patches =
    let patch_fname = Filename.concat bdir "patches" in
    match try Some (open_in_bin patch_fname) with [ Sys_error _ -> None ] with
    [ Some ic -> do {
        let ht = input_value ic in
        close_in ic;
        ht
      }
    | None ->
        let empty_ht () = Hashtbl.create 1 in
        {max_per = 0; max_fam = 0;
         h_person = empty_ht (); h_ascend = empty_ht ();
         h_union = empty_ht (); h_family = empty_ht ();
         h_couple = empty_ht (); h_descend = empty_ht ();
         h_key = empty_ht (); h_name = empty_ht ()} ]
  in
  Base2
    {bdir = bdir; cache_chan = Hashtbl.create 1; patches = patches;
     parents_array = None; consang_array = None;
     father_array = None; mother_array = None}
;

value open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  if Sys.file_exists (Filename.concat bname "base_d") then do {
    Printf.eprintf "*** database new implementation\n";
    flush stderr; 
    base_of_base2 bname
  }
  else base_of_dsk_base (Database.opendb bname)
;

value close_base base =
  match base with
  [ Base base -> base.func.cleanup ()
  | Base2 db2 ->
      Hashtbl.iter (fun (f1, f2, f) ic -> close_in ic) db2.cache_chan ]
;
