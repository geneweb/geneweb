(* $Id: gwdb.ml,v 5.199 2006-12-28 23:07:41 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Dbdisk;
open Db2disk;
open Def;
open Futil;
open Mutil;
open Printf;

type gen_string_person_index 'istr = Dbdisk.string_person_index 'istr ==
  { find : 'istr -> list iper;
    cursor : string -> 'istr;
    next : 'istr -> 'istr }
;

value no_consang = Adef.fix (-1);

value no_person empty_string ip =
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
value no_ascend = {parents = None; consang = no_consang};
value no_union = {family = [| |]};

(* Strings - common definitions *)

type istr =
  [ Istr of dsk_istr
  | Istr2 of db2 and (string * string) and int
  | Istr2New of db2 and string ]
;

type istr_fun 'a =
  { is_empty_string : 'a -> bool;
    is_quest_string : 'a -> bool;
    un_istr : 'a -> Adef.istr;
    un_istr2 : 'a -> string }
;

type relation = Def.gen_relation iper istr;
type title = Def.gen_title istr;

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

(* Strings - implementation database 1 *)

value istr1_fun =
  {is_empty_string istr = Adef.int_of_istr istr = 0;
   is_quest_string istr = Adef.int_of_istr istr = 1;
   un_istr i = i;
   un_istr2 i = failwith "un_istr2 1"}
;

(* Strings - implementation database 2 *)

value istr2_fun =
  {is_empty_string (db2, path, pos) = pos = Db2.empty_string_pos;
   is_quest_string (db2, path, pos) = failwith "not impl is_quest_string";
   un_istr _ = failwith "un_istr";
   un_istr2 _ = failwith "un_istr2 2"}
;

value istr2new_fun =
  {is_empty_string (db2, s) = s = "";
   is_quest_string (db2, s) = s = "?";
   un_istr (db2, s) = failwith "un_istr";
   un_istr2 (db2, s) = s}
;

(* Strings - user functions *)

value wrap_istr f g h =
  fun
  [ Istr istr -> f istr1_fun istr
  | Istr2 db2 path pos -> g istr2_fun (db2, path, pos)
  | Istr2New db2 s -> h istr2new_fun (db2, s) ]
;

value is_empty_string i =
  let f pf = pf.is_empty_string in
  wrap_istr f f f i
;
value is_quest_string i =
  let f pf = pf.is_quest_string in
  wrap_istr f f f i
;
value un_istr i =
  let f pf = pf.un_istr in
  wrap_istr f f f i
;
value un_istr2 i =
  let f pf = pf.un_istr2 in
  wrap_istr f f f i
;

(* String person index - common definitions *)

type string_person_index =
  [ Spi of gen_string_person_index dsk_istr
  | Spi2 of db2 and string_person_index2 ]
;

type spi 'a =
  { spi_first : 'a -> string -> istr;
    spi_next : 'a -> istr -> bool -> (istr * int);
    spi_find : 'a -> istr -> list iper }
;

(* String person index - implementation database 1 *)

value spi1_fun =
  {spi_first spi s = Istr (spi.cursor s);
   spi_next spi istr need_whole_list =
     match istr with
     [ Istr s -> (Istr (spi.next s), 1)
     | _ -> failwith "not impl spi_next" ];
   spi_find spi s =
     match s with
     [ Istr s -> spi.find s
     | _ -> failwith "not impl spi_find" ]}
;

(* String person index - implementation database 2 *)

value spi2_fun =
  {spi_first (db2, spi) s =
     let (f1, f2, pos) = spi2_first db2 spi s in
     Istr2 db2 (f1, f2) pos
   ;
   spi_next (db2, spi) istr need_whole_list =
     match istr with
     [ Istr2 _ (f1, f2) _ ->
         let (pos, dlen) = spi2_next db2 spi need_whole_list (f1, f2) in
         (Istr2 db2 (f1, f2) pos, dlen)
     | _ -> failwith "not impl spi_next" ];
   spi_find (db2, spi) s =
     match s with
     [ Istr2 db2 (f1, f2) pos -> spi2_find db2 (f1, f2) pos
     | Istr2New db2 s -> spi2gen_find db2 spi s
     | _ -> failwith "not impl spi_find" ]}
;

(* String person index - user functions *)

value wrap_spi f g =
  fun
  [ Spi spi -> f spi1_fun spi
  | Spi2 db2 spi2 -> g spi2_fun (db2, spi2) ]
;

value spi_find =
  let f pf = pf.spi_find in
  wrap_spi f f
;
value spi_first =
  let f pf = pf.spi_first in
  wrap_spi f f
;
value spi_next =
  let f pf = pf.spi_next in
  wrap_spi f f
;

(* Persons - common definitions *)

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

type person_fun 'p 'a 'u =
  { get_access : 'p -> access;
    get_aliases : 'p -> list istr;
    get_baptism : 'p -> codate;
    get_baptism_place : 'p -> istr;
    get_baptism_src : 'p -> istr;
    get_birth : 'p -> codate;
    get_birth_place : 'p -> istr;
    get_birth_src : 'p -> istr;
    get_burial : 'p -> Def.burial;
    get_burial_place : 'p -> istr;
    get_burial_src : 'p -> istr;
    get_death : 'p -> Def.death;
    get_death_place : 'p -> istr;
    get_death_src : 'p -> istr;
    get_first_name : 'p -> istr;
    get_first_names_aliases : 'p -> list istr;
    get_image : 'p -> istr;
    get_key_index : 'p -> iper;
    get_notes : 'p -> istr;
    get_occ : 'p -> int;
    get_occupation : 'p -> istr;
    get_psources : 'p -> istr;
    get_public_name : 'p -> istr;
    get_qualifiers : 'p -> list istr;
    get_related : 'p -> list iper;
    get_rparents : 'p -> list relation;
    get_sex : 'p -> Def.sex;
    get_surname : 'p -> istr;
    get_surnames_aliases : 'p -> list istr;
    get_titles : 'p -> list title;
    gen_person_of_person : 'p -> Def.gen_person iper istr;
    dsk_person_of_person : 'p -> Dbdisk.dsk_person;
    get_consang : 'a -> Adef.fix;
    get_parents : 'a -> option ifam;
    get_family : 'u -> array ifam }
;

(* Persons - implementation database 1 *)

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
   gen_person_of_person p = map_person_ps (fun p -> p) (fun s -> Istr s) p;
   dsk_person_of_person p = p;
   get_consang a = a.Def.consang;
   get_parents a = a.Def.parents;
   get_family u = u.Def.family}
;

(* Persons - implementation database 2 *)

value make_istr2 db2 path i = Istr2 db2 path (get_field_acc db2 i path);

value get_list_field db2 i f1f2 =
  let pos = get_field_acc db2 i f1f2 in
  if pos = -1 then [] else get_field_data db2 pos f1f2 "data2.ext"
;

value sou2 i =
  match i with
  [ Istr2 db2 f pos -> string_of_istr2 db2 f pos
  | Istr2New db2 s -> s
  | _ -> assert False ]
;

value person2_fun =
  self where rec self =
    {get_access (db2, i) = get_field db2 i ("person", "access");
     get_aliases (db2, i) =
       let list = get_list_field db2 i ("person", "aliases") in
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
       let list = get_list_field db2 i ("person", "first_names_aliases") in
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
       let list = get_list_field db2 i ("person", "qualifiers") in
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
       let list = get_list_field db2 i ("person", "surnames_aliases") in
       List.map (fun pos -> Istr2 db2 ("person", "surnames_aliases") pos)
         list;
     get_titles (db2, i) =
       let list = get_list_field db2 i ("person", "titles") in
       List.map
         (map_title_strings (fun pos -> Istr2 db2 ("person", "titles") pos))
         list;
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
     dsk_person_of_person p = failwith "not impl dsk_person_of_person";
     get_consang (db2, i) =
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
           else Some (get_field_data db2 pos ("person", "parents") "data") ];
     get_family (db2, i) =
       match db2.family_array with
       [ Some tab -> tab.(i)
       | None -> get_field db2 i ("person", "family") ]}
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
   gen_person_of_person (db2, p) =
     map_person_ps (fun p -> p) (fun s -> Istr2New db2 s) p;
   dsk_person_of_person (db2, p) =
     failwith "not impl dsk_person_of_person (gen)";
   get_consang (db2, a) = a.Def.consang;
   get_parents (db2, a) = a.Def.parents;
   get_family (db2, u) = u.Def.family}
;

(* Persons - user functions *)

value wrap_per f g h =
  fun
  [ Person p -> f person1_fun p
  | Person2 db2 i -> g person2_fun (db2, i)
  | Person2Gen db2 p -> h person2gen_fun (db2, p) ]
;

value wrap_asc f g h =
  fun
  [ Ascend a -> f person1_fun a
  | Ascend2 db2 i -> g person2_fun (db2, i)
  | Ascend2Gen db2 a -> h person2gen_fun (db2, a) ]
;

value wrap_uni f g h =
  fun
  [ Union u -> f person1_fun u
  | Union2 db2 i -> g person2_fun (db2, i)
  | Union2Gen db2 u -> h person2gen_fun (db2, u) ]
;

value get_access p =
  let f pf = pf.get_access in
  wrap_per f f f p
;
value get_aliases p =
  let f pf = pf.get_aliases in
  wrap_per f f f p
;
value get_baptism p =
  let f pf = pf.get_baptism in
  wrap_per f f f p
;
value get_baptism_place p =
  let f pf = pf.get_baptism_place in
  wrap_per f f f p
;
value get_baptism_src p =
  let f pf = pf.get_baptism_src in
  wrap_per f f f p
;
value get_birth p =
  let f pf = pf.get_birth in
  wrap_per f f f p
;
value get_birth_place p =
  let f pf = pf.get_birth_place in
  wrap_per f f f p
;
value get_birth_src p =
  let f pf = pf.get_birth_src in
  wrap_per f f f p
;
value get_burial p =
  let f pf = pf.get_burial in
  wrap_per f f f p
;
value get_burial_place p =
  let f pf = pf.get_burial_place in
  wrap_per f f f p
;
value get_burial_src p =
  let f pf = pf.get_burial_src in
  wrap_per f f f p
;
value get_death p =
  let f pf = pf.get_death in
  wrap_per f f f p
;
value get_death_place p =
  let f pf = pf.get_death_place in
  wrap_per f f f p
;
value get_death_src p =
  let f pf = pf.get_death_src in
  wrap_per f f f p
;
value get_first_name p =
  let f pf = pf.get_first_name in
  wrap_per f f f p
;
value get_first_names_aliases p =
  let f pf = pf.get_first_names_aliases in
  wrap_per f f f p
;
value get_image p =
  let f pf = pf.get_image in
  wrap_per f f f p
;
value get_key_index p =
  let f pf = pf.get_key_index in
  wrap_per f f f p
;
value get_notes p =
  let f pf = pf.get_notes in
  wrap_per f f f p
;
value get_occ p =
  let f pf = pf.get_occ in
  wrap_per f f f p
;
value get_occupation p =
  let f pf = pf.get_occupation in
  wrap_per f f f p
;
value get_psources p =
  let f pf = pf.get_psources in
  wrap_per f f f p
;
value get_public_name p =
  let f pf = pf.get_public_name in
  wrap_per f f f p
;
value get_qualifiers p =
  let f pf = pf.get_qualifiers in
  wrap_per f f f p
;
value get_related p =
  let f pf = pf.get_related in
  wrap_per f f f p
;
value get_rparents p =
  let f pf = pf.get_rparents in
  wrap_per f f f p
;
value get_sex p =
  let f pf = pf.get_sex in
  wrap_per f f f p
;
value get_surname p =
  let f pf = pf.get_surname in
  wrap_per f f f p
;
value get_surnames_aliases p =
  let f pf = pf.get_surnames_aliases in
  wrap_per f f f p
;
value get_titles p =
  let f pf = pf.get_titles in
  wrap_per f f f p
;

value gen_person_of_person p =
  let f pf = pf.gen_person_of_person in
  wrap_per f f f p
;
value dsk_person_of_person p =
  let f pf = pf.dsk_person_of_person in
  wrap_per f f f p
;

value get_consang a =
  let f pf = pf.get_consang in
  wrap_asc f f f a
;
value get_parents a =
  let f pf = pf.get_parents in
  wrap_asc f f f a
;

value get_family u =
  let f pf = pf.get_family in
  wrap_uni f f f u
;

(* Families - common definitions *)

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

type family_fun 'f 'c 'd =
  { get_comment : 'f -> istr;
    get_divorce : 'f -> Def.divorce;
    get_fsources : 'f -> istr;
    get_marriage : 'f -> codate;
    get_marriage_place : 'f -> istr;
    get_marriage_src : 'f -> istr;
    get_origin_file : 'f -> istr;
    get_relation : 'f -> Def.relation_kind;
    get_witnesses : 'f -> array iper;
    gen_family_of_family : 'f -> Def.gen_family iper istr;
    is_deleted_family : 'f -> bool;
    get_father : 'c -> iper;
    get_mother : 'c -> iper;
    get_parent_array : 'c -> array iper;
    gen_couple_of_couple : 'c -> Def.gen_couple iper;
    get_children : 'd -> array iper;
    gen_descend_of_descend : 'd -> Def.gen_descend iper }
;

(* Families - implementation database 1 *)

value family1_fun =
  {get_comment f = Istr f.Def.comment;
   get_divorce f = f.Def.divorce;
   get_fsources f = Istr f.Def.fsources;
   get_marriage f = f.Def.marriage;
   get_marriage_place f = Istr f.Def.marriage_place;
   get_marriage_src f = Istr f.Def.marriage_src;
   get_origin_file f = Istr f.Def.origin_file;
   get_relation f = f.Def.relation;
   get_witnesses f = f.Def.witnesses;
   gen_family_of_family f = map_family_ps (fun p -> p) (fun s -> Istr s) f;
   is_deleted_family f = f.Def.fam_index = Adef.ifam_of_int (-1);
   get_father c = Adef.father c;
   get_mother c = Adef.mother c;
   get_parent_array c = Adef.parent_array c;
   gen_couple_of_couple c = c;
   get_children d = d.Def.children;
   gen_descend_of_descend d = d}
;

(* Families - implementation database 2 *)

value family2_fun =
  self where rec self =
    {get_comment (db2, i) = make_istr2 db2 ("family", "comment") i;
     get_divorce (db2, i) = get_field db2 i ("family", "divorce");
     get_fsources (db2, i) = make_istr2 db2 ("family", "fsources") i;
     get_marriage (db2, i) = get_field db2 i ("family", "marriage");
     get_marriage_place (db2, i) =
       make_istr2 db2 ("family", "marriage_place") i;
     get_marriage_src (db2, i) = make_istr2 db2 ("family", "marriage_src") i;
     get_origin_file (db2, i) = make_istr2 db2 ("family", "origin_file") i;
     get_relation (db2, i) = get_field db2 i ("family", "relation");
     get_witnesses (db2, i) = get_field db2 i ("family", "witnesses");
     gen_family_of_family ((db2, i) as f) =
       {marriage = self.get_marriage f;
        marriage_place = self.get_marriage_place f;
        marriage_src = self.get_marriage_src f;
        witnesses = self.get_witnesses f; relation = self.get_relation f;
        divorce = self.get_divorce f; comment = self.get_comment f;
        origin_file = self.get_origin_file f; fsources = self.get_fsources f;
        fam_index = Adef.ifam_of_int i};
     is_deleted_family (db2, f) = False (* not yet implemented *);
     get_father (db2, i) =
       match db2.father_array with
       [ Some tab -> tab.(i)
       | None -> get_field db2 i ("family", "father") ];
     get_mother (db2, i) =
       match db2.mother_array with
       [ Some tab -> tab.(i)
       | None -> get_field db2 i ("family", "mother") ];
     get_parent_array (db2, i) =
       let p1 = get_field db2 i ("family", "father") in
       let p2 = get_field db2 i ("family", "mother") in
       [| p1; p2 |];
     gen_couple_of_couple c =
       Adef.couple (self.get_father c) (self.get_mother c);
     get_children (db2, i) =
       match db2.children_array with
       [ Some tab -> tab.(i)
       | None -> get_field db2 i ("family", "children") ];
     gen_descend_of_descend d = {children = self.get_children d}}
;

value family2gen_fun =
  {get_comment (db2, f) = Istr2New db2 f.Def.comment;
   get_divorce (db2, f) = f.Def.divorce;
   get_fsources (db2, f) = Istr2New db2 f.Def.fsources;
   get_marriage (db2, f) = f.Def.marriage;
   get_marriage_place (db2, f) = Istr2New db2 f.Def.marriage_place;
   get_marriage_src (db2, f) = Istr2New db2 f.Def.marriage_src;
   get_origin_file (db2, f) = Istr2New db2 f.Def.origin_file;
   get_relation (db2, f) = f.Def.relation;
   get_witnesses (db2, f) = f.Def.witnesses;
   gen_family_of_family (db2, f) =
      map_family_ps (fun p -> p) (fun s -> Istr2New db2 s) f;
   is_deleted_family (db2, f) = f.Def.fam_index = Adef.ifam_of_int (-1);
   get_father (db2, c) = Adef.father c;
   get_mother (db2, c) = Adef.mother c;
   get_parent_array (db2, c) = Adef.parent_array c;
   gen_couple_of_couple (db2, c) = c;
   get_children (db2, d) = d.Def.children;
   gen_descend_of_descend (db2, d) = d}
;

(* Families - user functions *)

value wrap_fam f g h =
  fun
  [ Family p -> f family1_fun p
  | Family2 db2 i -> g family2_fun (db2, i)
  | Family2Gen db2 p -> h family2gen_fun (db2, p) ]
;

value wrap_cpl f g h =
  fun
  [ Couple p -> f family1_fun p
  | Couple2 db2 i -> g family2_fun (db2, i)
  | Couple2Gen db2 p -> h family2gen_fun (db2, p) ]
;

value wrap_des f g h =
  fun
  [ Descend p -> f family1_fun p
  | Descend2 db2 i -> g family2_fun (db2, i)
  | Descend2Gen db2 p -> h family2gen_fun (db2, p) ]
;

value get_comment fam =
  let f pf = pf.get_comment in
  wrap_fam f f f fam
;
value get_divorce fam =
  let f pf = pf.get_divorce in
  wrap_fam f f f fam
;
value get_fsources fam =
  let f pf = pf.get_fsources in
  wrap_fam f f f fam
;
value get_marriage fam =
  let f pf = pf.get_marriage in
  wrap_fam f f f fam
;
value get_marriage_place fam =
  let f pf = pf.get_marriage_place in
  wrap_fam f f f fam
;
value get_marriage_src fam =
  let f pf = pf.get_marriage_src in
  wrap_fam f f f fam
;
value get_origin_file fam =
  let f pf = pf.get_origin_file in
  wrap_fam f f f fam
;
value get_relation fam =
  let f pf = pf.get_relation in
  wrap_fam f f f fam
;
value get_witnesses fam =
  let f pf = pf.get_witnesses in
  wrap_fam f f f fam
;
value gen_family_of_family fam =
  let f pf = pf.gen_family_of_family in
  wrap_fam f f f fam
;
value is_deleted_family fam =
  let f pf = pf.is_deleted_family in
  wrap_fam f f f fam
;

value get_father cpl =
  let f pf = pf.get_father in
  wrap_cpl f f f cpl
;
value get_mother cpl =
  let f pf = pf.get_mother in
  wrap_cpl f f f cpl
;
value get_parent_array cpl =
  let f pf = pf.get_parent_array in
  wrap_cpl f f f cpl
;
value gen_couple_of_couple cpl =
  let f pf = pf.gen_couple_of_couple in
  wrap_cpl f f f cpl
;

value get_children des =
  let f pf = pf.get_children in
  wrap_des f f f des
;
value gen_descend_of_descend des =
  let f pf = pf.gen_descend_of_descend in
  wrap_des f f f des
;

(* Databases - common definitions *)

type base =
  { close_base : unit -> unit;
    empty_person : iper -> person;
    empty_ascend : iper -> ascend;
    empty_union : iper -> union;
    person_of_gen_person :
      (gen_person iper istr * gen_ascend ifam * gen_union ifam) ->
         (person * ascend * union);
    family_of_gen_family :
      (gen_family iper istr * gen_couple iper * gen_descend iper) ->
         (family * couple * descend);
    poi : iper -> person;
    aoi : iper -> ascend;
    uoi : iper -> union;
    foi : ifam -> family;
    coi : ifam -> couple;
    doi : ifam -> descend;
    sou : istr -> string;
    nb_of_persons : unit -> int;
    nb_of_families : unit -> int;
    patch_person : iper -> Def.gen_person iper istr -> unit;
    patch_ascend : iper -> Def.gen_ascend ifam -> unit;
    patch_union : iper -> Def.gen_union ifam -> unit;
    patch_family : ifam -> Def.gen_family iper istr -> unit;
    patch_descend : ifam -> Def.gen_descend iper -> unit;
    patch_couple : ifam -> Def.gen_couple iper -> unit;
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
    nobtit :
      Lazy.t (list string) -> Lazy.t (list string) ->  person -> list title;
    p_first_name : person -> string;
    p_surname : person -> string;
    date_of_last_change : unit -> float;
    apply_as_dsk_base : (Dbdisk.dsk_base -> unit) -> unit }
;

module C_base :
  sig
    value delete_family : base -> ifam -> unit;
    value nobtit :
      base -> Lazy.t (list string) -> Lazy.t (list string) -> person ->
        list title;
    value p_first_name : base -> person -> string;
    value p_surname : base -> person -> string;
  end =
  struct
    value delete_family self ifam = do {
      let cpl = Adef.couple (Adef.iper_of_int (-1)) (Adef.iper_of_int (-1)) in
      let fam =
        let empty = self.insert_string "" in
        {marriage = Adef.codate_None; marriage_place = empty;
         marriage_src = empty; relation = Married; divorce = NotDivorced;
         witnesses = [| |]; comment = empty; origin_file = empty;
         fsources = empty; fam_index = Adef.ifam_of_int (-1)}
      in
      let des = {children = [| |]} in
      self.patch_family ifam fam;
      self.patch_couple ifam cpl;
      self.patch_descend ifam des
    };
    value nobtit self allowed_titles denied_titles p =
      let list = get_titles p in
      match Lazy.force allowed_titles with
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
          match Lazy.force denied_titles with
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

(* Database - implementation 1 *)

value base1 base =
  let base_strings_of_first_name_or_surname s =
    List.map (fun s -> Istr s) (base.func.strings_of_fsname s)
  in
  self where rec self =
    {close_base = base.func.cleanup;
     empty_person ip = Person (no_person (Adef.istr_of_int 0) ip);
     empty_ascend ip = Ascend no_ascend;
     empty_union ip = Union no_union;
     person_of_gen_person (p, a, u) =
       (Person (map_person_ps (fun p -> p) un_istr p), Ascend a, Union u);
     family_of_gen_family (f, c, d) =
       (Family (map_family_ps (fun p -> p) un_istr f), Couple c, Descend d);
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
       let p = map_person_ps (fun p -> p) un_istr p in
       base.func.Dbdisk.patch_person ip p;
     patch_ascend ip a = base.func.Dbdisk.patch_ascend ip a;
     patch_union ip u = base.func.Dbdisk.patch_union ip u;
     patch_family ifam f =
       let f = map_family_ps (fun p -> p) un_istr f in
       base.func.Dbdisk.patch_family ifam f;
     patch_descend ifam d = base.func.Dbdisk.patch_descend ifam d;
     patch_couple ifam c = base.func.Dbdisk.patch_couple ifam c;
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
     nobtit conf p = C_base.nobtit self conf p;
     p_first_name p = C_base.p_first_name self p;
     p_surname p = C_base.p_surname self p;
     date_of_last_change () =
       let s =
         let bdir = base.data.bdir in
         try Unix.stat (Filename.concat bdir "patches") with
         [ Unix.Unix_error _ _ _ -> Unix.stat (Filename.concat bdir "base") ]
       in
       s.Unix.st_mtime;
     apply_as_dsk_base f = f base}
;

(* Database - implementation 2 *)

value ok_I_know = ref False;
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
  self where rec self =
    {close_base () =
       Hashtbl.iter (fun (f1, f2, f) ic -> close_in ic) db2.cache_chan;
     empty_person ip = Person2Gen db2 (no_person "" ip);
     empty_ascend ip = Ascend2Gen db2 no_ascend;
     empty_union ip = Union2Gen db2 no_union;
     person_of_gen_person (p, a, u) =
       (Person2Gen db2 (map_person_ps (fun p -> p) un_istr2 p),
        Ascend2Gen db2 a, Union2Gen db2 u);
     family_of_gen_family (f, c, d) =
       (Family2Gen db2 (map_family_ps (fun p -> p) un_istr2 f),
        Couple2Gen db2 c, Descend2Gen db2 d);
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
     patch_person ip p = do {
       let p = map_person_ps (fun p -> p) un_istr2 p in
       Hashtbl.replace db2.patches.h_person ip p;
       db2.patches.nb_per := max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
     };
     patch_ascend ip a = do {
       Hashtbl.replace db2.patches.h_ascend ip a;
       db2.patches.nb_per := max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
     };
     patch_union ip u = do {
       Hashtbl.replace db2.patches.h_union ip u;
       db2.patches.nb_per := max (Adef.int_of_iper ip + 1) db2.patches.nb_per;
     };
     patch_family ifam f = do {
       let f = map_family_ps (fun p -> p) un_istr2 f in
       Hashtbl.replace db2.patches.h_family ifam f;
       db2.patches.nb_fam :=
         max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
     };
     patch_descend ifam d = do {
       Hashtbl.replace db2.patches.h_descend ifam d;
       db2.patches.nb_fam :=
         max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
     };
     patch_couple ifam c = do {
       Hashtbl.replace db2.patches.h_couple ifam c;
       db2.patches.nb_fam :=
         max (Adef.int_of_ifam ifam + 1) db2.patches.nb_fam
     };
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
     commit_patches () = commit_patches2 db2;
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
         List.fold_left Filename.concat db2.bdir2 ["person"; "consang"]
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
     persons_of_first_name () =
       Spi2 db2 (persons_of_first_name_or_surname2 db2 True);
     persons_of_surname () =
       Spi2 db2 (persons_of_first_name_or_surname2 db2 False);
     base_visible_get f = failwith "not impl visible_get";
     base_visible_write () = failwith "not impl visible_write";
     base_particles () =
       Mutil.input_particles (Filename.concat db2.bdir2 "particles.txt");
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
     load_couples_array () = load_couples_array2 db2;
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
       read_notes (Filename.dirname db2.bdir2) fnotes RnAll;
     base_notes_read_first_line fnotes =
       read_notes (Filename.dirname db2.bdir2) fnotes Rn1Ln;
     base_notes_are_empty fnotes =
       read_notes (Filename.dirname db2.bdir2) fnotes RnDeg = "";
     base_notes_origin_file () =
       let fname = Filename.concat db2.bdir2 "notes_of.txt" in
       match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
       [ Some ic ->
           let r = input_line ic in
           do { close_in ic; r }
       | None -> "" ];
     base_notes_dir () = Filename.concat "base_d" "notes_d";
     base_wiznotes_dir () = Filename.concat "base_d" "wiznotes_d";
     nobtit conf p = C_base.nobtit self conf p;
     p_first_name p = C_base.p_first_name self p;
     p_surname p = C_base.p_surname self p;
     date_of_last_change () =
       let s =
         let bdir = db2.bdir2 in
         try Unix.stat (Filename.concat bdir "patches") with
         [ Unix.Unix_error _ _ _ -> Unix.stat bdir ]
       in
       s.Unix.st_mtime;
     apply_as_dsk_base f = failwith "not impl apply_as_dsk_base"}
;

(* Database - user functions *)

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
value empty_ascend b = b.empty_ascend;
value empty_union b = b.empty_union;
value person_of_gen_person b = b.person_of_gen_person;
value family_of_gen_family b = b.family_of_gen_family;
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
value nobtit b = b.nobtit;
value p_first_name b = b.p_first_name;
value p_surname b = b.p_surname;
value date_of_last_change b = b.date_of_last_change ();
value apply_as_dsk_base f b = b.apply_as_dsk_base f;
value base_of_dsk_base = base1;

value husbands base p =
  let u = uoi base p.key_index in
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
  match get_parents (aoi base p.key_index) with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (get_father cpl) in
      List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> [] ]
;

value gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.sex = Female then husbands base p else [])
    (father_titles_places base p nobtit_fun)
;

value gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))
;

value person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit
;
