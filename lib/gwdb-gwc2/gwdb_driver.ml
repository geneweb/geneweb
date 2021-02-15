(* Copyright (c) 1998-2007 INRIA *)

open Db2disk
open Def
open Futil
open Mutil
open Printf

type 'istr gen_string_person_index =
  { find : 'istr -> int list;
    cursor : string -> 'istr;
    next : 'istr -> 'istr }

let milazy_force f a (get, set) p =
  match get p with
    Some v -> v
  | None -> let v = f a in set p (Some v); v

let ht_find ht i = try Some (Hashtbl.find ht i) with Not_found -> None

let no_person empty_string ip =
  {first_name = empty_string; surname = empty_string; occ = 0;
   image = empty_string; first_names_aliases = []; surnames_aliases = [];
   public_name = empty_string; qualifiers = []; titles = []; rparents = [];
   related = []; aliases = []; occupation = empty_string; sex = Neuter;
   access = Private; birth = Adef.cdate_None; birth_place = empty_string;
   birth_note = empty_string; birth_src = empty_string;
   baptism = Adef.cdate_None; baptism_place = empty_string;
   baptism_note = empty_string; baptism_src = empty_string;
   death = DontKnowIfDead; death_place = empty_string;
   death_note = empty_string; death_src = empty_string;
   burial = UnknownBurial; burial_place = empty_string;
   burial_note = empty_string; burial_src = empty_string; pevents = [];
   notes = empty_string; psources = empty_string; key_index = ip}
let no_ascend = {parents = None; consang = Adef.no_consang}
let no_union = {family = [| |]}

type iper = int
let string_of_iper = string_of_int
let iper_of_string = int_of_string

type ifam = int
let string_of_ifam = string_of_int
let ifam_of_string = int_of_string

(* Strings - common definitions *)

type istr =
  | Istr2 of (string * string) * int
  | Istr2New of string

type 'a istr_fun =
  { is_empty_string : 'a -> bool;
    is_quest_string : 'a -> bool;
    un_istr2 : 'a -> string }

type relation = (int, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (int, istr) Def.gen_pers_event
type fam_event = (int, istr) Def.gen_fam_event

let eq_istr i1 i2 =
  match i1, i2 with
  | Istr2 ((f11, f12), i1), Istr2 ((f21, f22), i2) -> i1 = i2 && f11 = f21 && f12 = f22
  | Istr2New s1, Istr2New s2 -> s1 = s2
  | _ -> assert false

(* Strings - implementation database 2 *)

let istr2_fun =
  {is_empty_string =
    (fun (path, pos) -> string_of_istr2 db2 path pos = "");
   is_quest_string =
     (fun (db2, path, pos) -> string_of_istr2 db2 path pos = "?");
   un_istr2 = fun (db2, path, pos) -> string_of_istr2 db2 path pos}

let istr2new_fun =
  {is_empty_string = (fun (db2, s) -> s = "");
   is_quest_string = (fun (db2, s) -> s = "?");
   un_istr2 = fun (db2, s) -> s}

(* Strings - user functions *)

let wrap_istr f g h =
  function
  | Istr2 (path, pos) -> g istr2_fun (path, pos)
  | Istr2New (s) -> h istr2new_fun (s)

let is_empty_string i = let f pf = pf.is_empty_string in wrap_istr f f f i
let is_quest_string i = let f pf = pf.is_quest_string in wrap_istr f f f i
let un_istr2 i = let f pf = pf.un_istr2 in wrap_istr f f f i

(* String person index - common definitions *)

type string_person_index =
  | Spi2 of db2 * string_person_index2

type 'a spi =
  { spi_first : 'a -> string -> istr;
    spi_next : 'a -> istr -> bool -> istr * int;
    spi_find : 'a -> istr -> int list }

(* String person index - implementation database 2 *)

let spi2_fun =
  {spi_first =
    (fun (db2, spi) s ->
       let f1 = "person" in
       let f2 = if spi.is_first_name then "first_name" else "surname" in
       match spi2_first db2 spi (f1, f2) s with
         Sp pos -> Istr2 (db2, (f1, f2), pos)
       | SpNew s2 -> Istr2New (db2, s2));
   spi_next =
     (fun (db2, spi) istr need_whole_list ->
        let f1 = "person" in
        let f2 = if spi.is_first_name then "first_name" else "surname" in
        let (sp, dlen) = spi2_next db2 spi (f1, f2) need_whole_list in
        let r =
          match sp with
            Sp pos -> Istr2 (db2, (f1, f2), pos)
          | SpNew s2 -> Istr2New (db2, s2)
        in
        r, dlen);
   spi_find =
     fun (db2, spi) s ->
       match s with
       | Istr2 (db2, (f1, f2), pos) -> spi2_find db2 spi (f1, f2) pos
       | Istr2New (db2, s) -> spi2gen_find db2 spi s
  }

(* String person index - user functions *)

let wrap_spi f g =
  function
  | Spi2 (db2, spi2) -> g spi2_fun (db2, spi2)

let spi_find = let f pf = pf.spi_find in wrap_spi f f
let spi_first = let f pf = pf.spi_first in wrap_spi f f
let spi_next = let f pf = pf.spi_next in wrap_spi f f

(* Persons - common definitions *)

type person =
  | Person2 of db2 * int * person2_dat
and person2_dat =
  { mutable per2 : (int, int, string) gen_person option option;
    mutable asc2 : int gen_ascend option option;
    mutable uni2 : int gen_union option option }

type ('p, 'a, 'u) person_fun =
  { get_access : 'p -> access;
    get_aliases : 'p -> istr list;
    get_baptism : 'p -> cdate;
    get_baptism_place : 'p -> istr;
    get_baptism_note : 'p -> istr;
    get_baptism_src : 'p -> istr;
    get_birth : 'p -> cdate;
    get_birth_place : 'p -> istr;
    get_birth_note : 'p -> istr;
    get_birth_src : 'p -> istr;
    get_burial : 'p -> Def.burial;
    get_burial_place : 'p -> istr;
    get_burial_note : 'p -> istr;
    get_burial_src : 'p -> istr;
    get_death : 'p -> Def.death;
    get_death_place : 'p -> istr;
    get_death_note : 'p -> istr;
    get_death_src : 'p -> istr;
    get_first_name : 'p -> istr;
    get_first_names_aliases : 'p -> istr list;
    get_image : 'p -> istr;
    get_key_index : 'p -> int;
    get_notes : 'p -> istr;
    get_occ : 'p -> int;
    get_occupation : 'p -> istr;
    get_psources : 'p -> istr;
    get_public_name : 'p -> istr;
    get_qualifiers : 'p -> istr list;
    get_related : 'p -> int list;
    get_rparents : 'p -> relation list;
    get_sex : 'p -> Def.sex;
    get_surname : 'p -> istr;
    get_surnames_aliases : 'p -> istr list;
    get_titles : 'p -> title list;
    get_pevents : 'p -> pers_event list;
    gen_person_of_person : 'p -> (int, int, istr) Def.gen_person;
    get_consang : 'a -> Adef.fix;
    get_parents : 'a -> int option;
    get_family : 'u -> int array }

(* Persons - implementation database 2 *)

let make_istr2 db2 path i = Istr2 (db2, path, get_field_acc db2 i path)

let get_list_field db2 i f1f2 =
  let pos = get_field_acc db2 i f1f2 in
  if pos = -1 then [] else get_field_data db2 pos f1f2 "data2.ext"

let sou2 i =
  match i with
  | Istr2 (db2, f, pos) -> string_of_istr2 db2 f pos
  | Istr2New (db2, s) -> s

let person2_fun =
  let rec self =
    {get_access = (fun (db2, i) -> get_field db2 i ("person", "access"));
     get_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "aliases") in
          List.map (fun pos -> Istr2 (db2, ("person", "aliases"), pos)) list);
     get_baptism = (fun (db2, i) -> get_field db2 i ("person", "baptism"));
     get_baptism_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_place") i);
     get_baptism_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_note") i);
     get_baptism_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "baptism_src") i);
     get_birth = (fun (db2, i) -> get_field db2 i ("person", "birth"));
     get_birth_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_place") i);
     get_birth_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_note") i);
     get_birth_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "birth_src") i);
     get_burial = (fun (db2, i) -> get_field db2 i ("person", "burial"));
     get_burial_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_place") i);
     get_burial_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_note") i);
     get_burial_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "burial_src") i);
     get_death = (fun (db2, i) -> get_field db2 i ("person", "death"));
     get_death_place =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_place") i);
     get_death_note =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_note") i);
     get_death_src =
       (fun (db2, i) -> make_istr2 db2 ("person", "death_src") i);
     get_first_name =
       (fun (db2, i) -> make_istr2 db2 ("person", "first_name") i);
     get_first_names_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "first_names_aliases") in
          List.map
            (fun pos -> Istr2 (db2, ("person", "first_names_aliases"), pos))
            list);
     get_image = (fun (db2, i) -> make_istr2 db2 ("person", "image") i);
     get_key_index = (fun (db2, i) -> i);
     get_notes = (fun (db2, i) -> make_istr2 db2 ("person", "notes") i);
     get_occ = (fun (db2, i) -> get_field db2 i ("person", "occ"));
     get_occupation =
       (fun (db2, i) -> make_istr2 db2 ("person", "occupation") i);
     get_psources = (fun (db2, i) -> make_istr2 db2 ("person", "psources") i);
     get_public_name =
       (fun (db2, i) -> make_istr2 db2 ("person", "public_name") i);
     get_qualifiers =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "qualifiers") in
          List.map (fun pos -> Istr2 (db2, ("person", "qualifiers"), pos))
            list);
     get_related =
       (fun (db2, i) ->
          let pos = get_field_acc db2 i ("person", "related") in
          let rec loop list pos =
            if pos = -1 then List.rev list
            else
              let (ip, pos) =
                get_field_2_data db2 pos ("person", "related") "data"
              in
              loop (ip :: list) pos
          in
          loop [] pos);
     get_rparents =
       (fun (db2, i) ->
          let pos = get_field_acc db2 i ("person", "rparents") in
          if pos = -1 then []
          else
            let rl = get_field_data db2 pos ("person", "rparents") "data" in
            List.map
              (map_relation_ps (fun x -> x)
                 (fun _ -> Istr2 (db2, ("", ""), -1)))
              rl);
     get_sex = (fun (db2, i) -> get_field db2 i ("person", "sex"));
     get_surname = (fun (db2, i) -> make_istr2 db2 ("person", "surname") i);
     get_surnames_aliases =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "surnames_aliases") in
          List.map
            (fun pos -> Istr2 (db2, ("person", "surnames_aliases"), pos))
            list);
     get_titles =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "titles") in
          List.map
            (map_title_strings
               (fun pos -> Istr2 (db2, ("person", "titles"), pos)))
            list);
     get_pevents =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("person", "pevents") in
          List.map
            (map_pers_event (fun x -> x)
               (fun pos -> Istr2 (db2, ("person", "pevents"), pos)))
            list);
     gen_person_of_person =
       (fun pp ->
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
           birth_note = self.get_birth_note pp;
           birth_src = self.get_birth_src pp; baptism = self.get_baptism pp;
           baptism_place = self.get_baptism_place pp;
           baptism_note = self.get_baptism_note pp;
           baptism_src = self.get_baptism_src pp; death = self.get_death pp;
           death_place = self.get_death_place pp;
           death_note = self.get_death_note pp;
           death_src = self.get_death_src pp; burial = self.get_burial pp;
           burial_place = self.get_burial_place pp;
           burial_note = self.get_burial_note pp;
           burial_src = self.get_burial_src pp; pevents = self.get_pevents pp;
           notes = self.get_notes pp; psources = self.get_psources pp;
           key_index = self.get_key_index pp});
     get_consang =
       (fun (db2, i) ->
          match db2.consang_array with
            Some tab -> tab.(i)
          | None ->
              let f = "person", "consang" in
              if field_exists db2 f then get_field db2 i f
              else Adef.no_consang);
     get_parents =
       (fun (db2, i) ->
          match db2.parents_array with
            Some tab -> tab.(i)
          | None ->
              let pos = get_field_acc db2 i ("person", "parents") in
              if pos = -1 then None
              else
                Some (get_field_data db2 pos ("person", "parents") "data"));
     get_family =
       fun (db2, i) ->
         match db2.family_array with
           Some tab -> tab.(i)
         | None -> get_field db2 i ("person", "family")}
  in
  self

let person2gen_fun =
  {get_access = (fun (db2, i, p) -> p.Def.access);
   get_aliases =
     (fun (db2, i, p) -> List.map (fun s -> Istr2New (db2, s)) p.Def.aliases);
   get_baptism = (fun (db2, i, p) -> p.Def.baptism);
   get_baptism_place =
     (fun (db2, i, p) -> Istr2New (db2, p.Def.baptism_place));
   get_baptism_note = (fun (db2, i, p) -> Istr2New (db2, p.Def.baptism_note));
   get_baptism_src = (fun (db2, i, p) -> Istr2New (db2, p.Def.baptism_src));
   get_birth = (fun (db2, i, p) -> p.Def.birth);
   get_birth_place = (fun (db2, i, p) -> Istr2New (db2, p.Def.birth_place));
   get_birth_note = (fun (db2, i, p) -> Istr2New (db2, p.Def.birth_note));
   get_birth_src = (fun (db2, i, p) -> Istr2New (db2, p.Def.birth_src));
   get_burial = (fun (db2, i, p) -> p.Def.burial);
   get_burial_place = (fun (db2, i, p) -> Istr2New (db2, p.Def.burial_place));
   get_burial_note = (fun (db2, i, p) -> Istr2New (db2, p.Def.burial_note));
   get_burial_src = (fun (db2, i, p) -> Istr2New (db2, p.Def.burial_src));
   get_death = (fun (db2, i, p) -> p.Def.death);
   get_death_place = (fun (db2, i, p) -> Istr2New (db2, p.Def.death_place));
   get_death_note = (fun (db2, i, p) -> Istr2New (db2, p.Def.death_note));
   get_death_src = (fun (db2, i, p) -> Istr2New (db2, p.Def.death_src));
   get_first_name = (fun (db2, i, p) -> Istr2New (db2, p.Def.first_name));
   get_first_names_aliases =
     (fun (db2, i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.first_names_aliases);
   get_image = (fun (db2, i, p) -> Istr2New (db2, p.Def.image));
   get_key_index = (fun (db2, i, p) -> p.Def.key_index);
   get_notes = (fun (db2, i, p) -> Istr2New (db2, p.Def.notes));
   get_occ = (fun (db2, i, p) -> p.Def.occ);
   get_occupation = (fun (db2, i, p) -> Istr2New (db2, p.Def.occupation));
   get_psources = (fun (db2, i, p) -> Istr2New (db2, p.Def.psources));
   get_public_name = (fun (db2, i, p) -> Istr2New (db2, p.Def.public_name));
   get_qualifiers =
     (fun (db2, i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.qualifiers);
   get_related = (fun (db2, i, p) -> p.Def.related);
   get_rparents =
     (fun (db2, i, p) ->
        List.map (map_relation_ps (fun x -> x) (fun s -> Istr2New (db2, s)))
          p.Def.rparents);
   get_sex = (fun (db2, i, p) -> p.Def.sex);
   get_surname = (fun (db2, i, p) -> Istr2New (db2, p.Def.surname));
   get_surnames_aliases =
     (fun (db2, i, p) ->
        List.map (fun s -> Istr2New (db2, s)) p.Def.surnames_aliases);
   get_titles =
     (fun (db2, i, p) ->
        List.map (fun t -> map_title_strings (fun s -> Istr2New (db2, s)) t)
          p.Def.titles);
   get_pevents =
     (fun (db2, i, p) ->
        List.map
          (fun t ->
             map_pers_event (fun x -> x) (fun s -> Istr2New (db2, s)) t)
          p.Def.pevents);
   gen_person_of_person =
     (fun (db2, i, p) ->
        map_person_ps (fun p -> p) (fun s -> Istr2New (db2, s)) p);
   get_consang = (fun (db2, i, a) -> a.Def.consang);
   get_parents = (fun (db2, i, a) -> a.Def.parents);
   get_family = fun (db2, i, u) -> u.Def.family}

(* Persons - user functions *)

let get_set_per2 = (fun p -> p.per2), (fun p v -> p.per2 <- v)
let get_set_asc2 = (fun p -> p.asc2), (fun p v -> p.asc2 <- v)
let get_set_uni2 = (fun p -> p.uni2), (fun p v -> p.uni2 <- v)

let wrap_per f g h =
  function
  | Person2 (db2, i, p) ->
      let per =
        milazy_force (ht_find db2.patches.h_person) (i)
          get_set_per2 p
      in
      match per with
        Some p -> h person2gen_fun (db2, i, p)
      | None -> g person2_fun (db2, i)

let wrap_asc f g h =
  function
  | Person2 (db2, i, p) ->
      let asc =
        milazy_force (ht_find db2.patches.h_ascend) (i)
          get_set_asc2 p
      in
      match asc with
        Some a -> h person2gen_fun (db2, i, a)
      | None -> g person2_fun (db2, i)

let wrap_uni f g h =
  function
  | Person2 (db2, i, p) ->
      let uni =
        milazy_force (ht_find db2.patches.h_union) (i)
          get_set_uni2 p
      in
      match uni with
        Some u -> h person2gen_fun (db2, i, u)
      | None -> g person2_fun (db2, i)

let get_access p = let f pf = pf.get_access in wrap_per f f f p
let get_aliases p = let f pf = pf.get_aliases in wrap_per f f f p
let get_baptism p = let f pf = pf.get_baptism in wrap_per f f f p
let get_baptism_place p = let f pf = pf.get_baptism_place in wrap_per f f f p
let get_baptism_note p = let f pf = pf.get_baptism_note in wrap_per f f f p
let get_baptism_src p = let f pf = pf.get_baptism_src in wrap_per f f f p
let get_birth p = let f pf = pf.get_birth in wrap_per f f f p
let get_birth_place p = let f pf = pf.get_birth_place in wrap_per f f f p
let get_birth_note p = let f pf = pf.get_birth_note in wrap_per f f f p
let get_birth_src p = let f pf = pf.get_birth_src in wrap_per f f f p
let get_burial p = let f pf = pf.get_burial in wrap_per f f f p
let get_burial_place p = let f pf = pf.get_burial_place in wrap_per f f f p
let get_burial_note p = let f pf = pf.get_burial_note in wrap_per f f f p
let get_burial_src p = let f pf = pf.get_burial_src in wrap_per f f f p
let get_death p = let f pf = pf.get_death in wrap_per f f f p
let get_death_place p = let f pf = pf.get_death_place in wrap_per f f f p
let get_death_note p = let f pf = pf.get_death_note in wrap_per f f f p
let get_death_src p = let f pf = pf.get_death_src in wrap_per f f f p
let get_pevents p = let f pf = pf.get_pevents in wrap_per f f f p
let get_first_name p = let f pf = pf.get_first_name in wrap_per f f f p
let get_first_names_aliases p =
  let f pf = pf.get_first_names_aliases in wrap_per f f f p
let get_image p = let f pf = pf.get_image in wrap_per f f f p
let get_key_index p = let f pf = pf.get_key_index in wrap_per f f f p
let get_notes p = let f pf = pf.get_notes in wrap_per f f f p
let get_occ p = let f pf = pf.get_occ in wrap_per f f f p
let get_occupation p = let f pf = pf.get_occupation in wrap_per f f f p
let get_psources p = let f pf = pf.get_psources in wrap_per f f f p
let get_public_name p = let f pf = pf.get_public_name in wrap_per f f f p
let get_qualifiers p = let f pf = pf.get_qualifiers in wrap_per f f f p
let get_related p = let f pf = pf.get_related in wrap_per f f f p
let get_rparents p = let f pf = pf.get_rparents in wrap_per f f f p
let get_sex p = let f pf = pf.get_sex in wrap_per f f f p
let get_surname p = let f pf = pf.get_surname in wrap_per f f f p
let get_surnames_aliases p =
  let f pf = pf.get_surnames_aliases in wrap_per f f f p
let get_titles p = let f pf = pf.get_titles in wrap_per f f f p

let gen_person_of_person p =
  let f pf = pf.gen_person_of_person in wrap_per f f f p

let get_consang a =
  let f pf = pf.get_consang in
  match a with
    Person2 (db2, i, _) ->
      begin match db2.consang_array with
        Some tab -> tab.(i)
      | None -> wrap_asc f f f a
      end
let get_parents a =
  let f pf = pf.get_parents in
  match a with
    Person2 (db2, i, _) ->
      begin match db2.parents_array with
        Some tab -> tab.(i)
      | None -> wrap_asc f f f a
      end

let get_family u = let f pf = pf.get_family in wrap_uni f f f u

(* Families - common definitions *)

type family =
  | Family2 of db2 * int * family2_dat
and family2_dat =
  { mutable fam2 : (int, int, string) gen_family option option;
    mutable cpl2 : int gen_couple option option;
    mutable des2 : int gen_descend option option }

type ('f, 'c, 'd) family_fun =
  { get_comment : 'f -> istr;
    get_divorce : 'f -> Def.divorce;
    get_fsources : 'f -> istr;
    get_fevents : 'f -> fam_event list;
    get_marriage : 'f -> cdate;
    get_marriage_place : 'f -> istr;
    get_marriage_note : 'f -> istr;
    get_marriage_src : 'f -> istr;
    get_origin_file : 'f -> istr;
    get_relation : 'f -> Def.relation_kind;
    get_witnesses : 'f -> int array;
    gen_family_of_family : 'f -> (int, int, istr) Def.gen_family;
    is_deleted_family : 'f -> bool;
    get_father : 'c -> int;
    get_mother : 'c -> int;
    get_parent_array : 'c -> int array;
    gen_couple_of_couple : 'c -> int Def.gen_couple;
    get_children : 'd -> int array;
    gen_descend_of_descend : 'd -> int Def.gen_descend }

(* Families - implementation database 2 *)

let family2_fun =
  let rec self =
    {get_comment = (fun (db2, i) -> make_istr2 db2 ("family", "comment") i);
     get_divorce = (fun (db2, i) -> get_field db2 i ("family", "divorce"));
     get_fsources = (fun (db2, i) -> make_istr2 db2 ("family", "fsources") i);
     get_fevents =
       (fun (db2, i) ->
          let list = get_list_field db2 i ("family", "fevents") in
          List.map
            (map_fam_event (fun x -> x)
               (fun pos -> Istr2 (db2, ("family", "fevents"), pos)))
            list);
     get_marriage = (fun (db2, i) -> get_field db2 i ("family", "marriage"));
     get_marriage_place =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_place") i);
     get_marriage_note =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_note") i);
     get_marriage_src =
       (fun (db2, i) -> make_istr2 db2 ("family", "marriage_src") i);
     get_origin_file =
       (fun (db2, i) -> make_istr2 db2 ("family", "origin_file") i);
     get_relation = (fun (db2, i) -> get_field db2 i ("family", "relation"));
     get_witnesses =
       (fun (db2, i) -> get_field db2 i ("family", "witnesses"));
     gen_family_of_family =
       (fun (db2, i as f) ->
          {marriage = self.get_marriage f;
           marriage_place = self.get_marriage_place f;
           marriage_note = self.get_marriage_note f;
           marriage_src = self.get_marriage_src f;
           witnesses = self.get_witnesses f; relation = self.get_relation f;
           divorce = self.get_divorce f; fevents = self.get_fevents f;
           comment = self.get_comment f; origin_file = self.get_origin_file f;
           fsources = self.get_fsources f; fam_index = i});
     is_deleted_family =
       (fun (db2, i) ->
          let fath =
            match db2.father_array with
              Some tab -> tab.(i)
            | None -> get_field db2 i ("family", "father")
          in
          fath < 0);
     get_father =
       (fun (db2, i) ->
          match db2.father_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "father"));
     get_mother =
       (fun (db2, i) ->
          match db2.mother_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "mother"));
     get_parent_array =
       (fun (db2, i) ->
          let p1 = get_field db2 i ("family", "father") in
          let p2 = get_field db2 i ("family", "mother") in [| p1; p2 |]);
     gen_couple_of_couple =
       (fun c -> Adef.couple (self.get_father c) (self.get_mother c));
     get_children =
       (fun (db2, i) ->
          match db2.children_array with
            Some tab -> tab.(i)
          | None -> get_field db2 i ("family", "children"));
     gen_descend_of_descend = fun d -> {children = self.get_children d}}
  in
  self

let family2gen_fun =
  {get_comment = (fun (db2, f) -> Istr2New (db2, f.Def.comment));
   get_divorce = (fun (db2, f) -> f.Def.divorce);
   get_fsources = (fun (db2, f) -> Istr2New (db2, f.Def.fsources));
   get_fevents =
     (fun (db2, f) ->
        List.map
          (fun t -> map_fam_event (fun x -> x) (fun s -> Istr2New (db2, s)) t)
          f.Def.fevents);
   get_marriage = (fun (db2, f) -> f.Def.marriage);
   get_marriage_place =
     (fun (db2, f) -> Istr2New (db2, f.Def.marriage_place));
   get_marriage_note = (fun (db2, f) -> Istr2New (db2, f.Def.marriage_note));
   get_marriage_src = (fun (db2, f) -> Istr2New (db2, f.Def.marriage_src));
   get_origin_file = (fun (db2, f) -> Istr2New (db2, f.Def.origin_file));
   get_relation = (fun (db2, f) -> f.Def.relation);
   get_witnesses = (fun (db2, f) -> f.Def.witnesses);
   gen_family_of_family =
     (fun (db2, f) ->
        map_family_ps (fun p -> p) (fun f -> f) (fun s -> Istr2New (db2, s)) f);
   is_deleted_family =
     (fun (db2, f) -> f.Def.fam_index = (-1));
   get_father = (fun (db2, c) -> Adef.father c);
   get_mother = (fun (db2, c) -> Adef.mother c);
   get_parent_array = (fun (db2, c) -> Adef.parent_array c);
   gen_couple_of_couple = (fun (db2, c) -> c);
   get_children = (fun (db2, d) -> d.Def.children);
   gen_descend_of_descend = fun (db2, d) -> d}

(* Families - user functions *)

let get_set_fam2 = (fun p -> p.fam2), (fun p v -> p.fam2 <- v)
let get_set_cpl2 = (fun p -> p.cpl2), (fun p v -> p.cpl2 <- v)
let get_set_des2 = (fun p -> p.des2), (fun p v -> p.des2 <- v)

let wrap_fam f g h =
  function
  | Family2 (db2, i, d) ->
      let fam =
        milazy_force (ht_find db2.patches.h_family) (i)
          get_set_fam2 d
      in
      match fam with
        Some fam -> h family2gen_fun (db2, fam)
      | None -> g family2_fun (db2, i)

let wrap_cpl f g h =
  function
  | Family2 (db2, i, d) ->
      let cpl =
        milazy_force (ht_find db2.patches.h_couple) (i)
          get_set_cpl2 d
      in
      match cpl with
        Some cpl -> h family2gen_fun (db2, cpl)
      | None -> g family2_fun (db2, i)

let wrap_des f g h =
  function
  | Family2 (db2, i, d) ->
      let des =
        milazy_force (ht_find db2.patches.h_descend) (i)
          get_set_des2 d
      in
      match des with
        Some des -> h family2gen_fun (db2, des)
      | None -> g family2_fun (db2, i)

let get_comment fam = let f pf = pf.get_comment in wrap_fam f f f fam
let get_divorce fam = let f pf = pf.get_divorce in wrap_fam f f f fam
let get_fsources fam = let f pf = pf.get_fsources in wrap_fam f f f fam
let get_fevents fam = let f pf = pf.get_fevents in wrap_fam f f f fam
let get_marriage fam = let f pf = pf.get_marriage in wrap_fam f f f fam
let get_marriage_place fam =
  let f pf = pf.get_marriage_place in wrap_fam f f f fam
let get_marriage_note fam =
  let f pf = pf.get_marriage_note in wrap_fam f f f fam
let get_marriage_src fam =
  let f pf = pf.get_marriage_src in wrap_fam f f f fam
let get_origin_file fam = let f pf = pf.get_origin_file in wrap_fam f f f fam
let get_relation fam = let f pf = pf.get_relation in wrap_fam f f f fam
let get_witnesses fam = let f pf = pf.get_witnesses in wrap_fam f f f fam
let gen_family_of_family fam =
  let f pf = pf.gen_family_of_family in wrap_fam f f f fam
let is_deleted_family fam =
  let f pf = pf.is_deleted_family in wrap_fam f f f fam

let get_father cpl =
  let f pf = pf.get_father in
  match cpl with
    Family2 (db2, i, _) ->
      begin match db2.father_array with
        Some tab -> tab.(i)
      | None -> wrap_cpl f f f cpl
      end
let get_mother cpl =
  let f pf = pf.get_mother in
  match cpl with
    Family2 (db2, i, _) ->
      begin match db2.mother_array with
        Some tab -> tab.(i)
      | None -> wrap_cpl f f f cpl
      end
let get_parent_array cpl =
  let f pf = pf.get_parent_array in wrap_cpl f f f cpl
let gen_couple_of_couple cpl =
  let f pf = pf.gen_couple_of_couple in wrap_cpl f f f cpl

let get_children des = let f pf = pf.get_children in wrap_des f f f des
let gen_descend_of_descend des =
  let f pf = pf.gen_descend_of_descend in wrap_des f f f des

(* Databases - common definitions *)

type base =
  { close_base : unit -> unit;
    empty_person : int -> person;
    person_of_gen_person :
      (int, int, istr) gen_person * int gen_ascend * int gen_union -> person;
    family_of_gen_family :
      (int, int, istr) gen_family * int gen_couple * int gen_descend -> family;
    poi : int -> person;
    foi : int -> family;
    sou : istr -> string;
    nb_of_persons : unit -> int;
    nb_of_families : unit -> int;
    patch_person : int -> (int, int, istr) Def.gen_person -> unit;
    patch_ascend : int -> int Def.gen_ascend -> unit;
    patch_union : int -> int Def.gen_union -> unit;
    patch_family : int -> (int, int, istr) Def.gen_family -> unit;
    patch_descend : int -> int Def.gen_descend -> unit;
    patch_couple : int -> int Def.gen_couple -> unit;
    patch_name : string -> int -> unit;
    patch_key : int -> string -> string -> int -> unit;
    delete_key : string -> string -> int -> unit;
    insert_string : string -> istr;
    commit_patches : unit -> unit;
    commit_notes : string -> string -> unit;
    is_patched_person : int -> bool;
    patched_ascends : unit -> int list;
    delete_family : int -> unit;
    person_of_key : string -> string -> int -> int option;
    persons_of_name : string -> int list;
    persons_of_first_name : unit -> string_person_index;
    persons_of_surname : unit -> string_person_index;
    base_visible_get : (person -> bool) -> int -> bool;
    base_visible_write : unit -> unit;
    base_particles : unit -> string list;
    base_strings_of_first_name : string -> istr list;
    base_strings_of_surname : string -> istr list;
    load_ascends_array : unit -> unit;
    load_unions_array : unit -> unit;
    load_couples_array : unit -> unit;
    load_descends_array : unit -> unit;
    load_strings_array : unit -> unit;
    persons_array :
      unit ->
        (int -> (int, int, istr) gen_person) *
          (int -> (int, int, istr) gen_person -> unit);
    ascends_array :
      unit ->
        (int -> int option) * (int -> Adef.fix) * (int -> Adef.fix -> unit) *
          Adef.fix array option;
    base_notes_read : string -> string;
    base_notes_read_first_line : string -> string;
    base_notes_are_empty : string -> bool;
    base_notes_origin_file : unit -> string;
    base_notes_dir : unit -> string;
    base_wiznotes_dir : unit -> string;
    nobtit : string list Lazy.t -> string list Lazy.t -> person -> title list;
    p_first_name : person -> string;
    p_surname : person -> string;
    date_of_last_change : unit -> float;
    apply_base2 : (Db2disk.db2 -> unit) -> unit }

module C_base :
  sig
    val delete_family : base -> int -> unit
    val nobtit :
      base -> string list Lazy.t -> string list Lazy.t -> person -> title list
    val p_first_name : base -> person -> string
    val p_surname : base -> person -> string
  end =
  struct
    let delete_family self ifam =
      let cpl = Adef.couple ((-1)) ((-1)) in
      let fam =
        let empty = self.insert_string "" in
        {marriage = Adef.cdate_None; marriage_place = empty;
         marriage_note = empty; marriage_src = empty; relation = Married;
         divorce = NotDivorced; fevents = []; witnesses = [| |];
         comment = empty; origin_file = empty; fsources = empty;
         fam_index = (-1)}
      in
      let des = {children = [| |]} in
      self.patch_family ifam fam;
      self.patch_couple ifam cpl;
      self.patch_descend ifam des
    let nobtit self allowed_titles denied_titles p =
      let list = get_titles p in
      match Lazy.force allowed_titles with
        [] -> list
      | allowed_titles ->
          let list =
            List.fold_right
              (fun t l ->
                 let id = Name.lower (self.sou t.t_ident) in
                 let pl = Name.lower (self.sou t.t_place) in
                 if pl = "" then
                   if List.mem id allowed_titles then t :: l else l
                 else if
                   List.mem (id ^ "/" ^ pl) allowed_titles ||
                   List.mem (id ^ "/*") allowed_titles
                 then
                   t :: l
                 else l)
              list []
          in
          match Lazy.force denied_titles with
            [] -> list
          | denied_titles ->
              List.filter
                (fun t ->
                   let id = Name.lower (self.sou t.t_ident) in
                   let pl = Name.lower (self.sou t.t_place) in
                   if List.mem (id ^ "/" ^ pl) denied_titles ||
                      List.mem ("*/" ^ pl) denied_titles
                   then
                     false
                   else true)
                list
    let p_first_name self p = nominative (self.sou (get_first_name p))
    let p_surname self p = nominative (self.sou (get_surname p))
  end

(* Database - implementation 2 *)

let base2 db2 =
  let base_strings_of_first_name_or_surname field proj s =
    let posl = strings2_of_fsname db2 field s in
    let istrl =
      List.map (fun pos -> Istr2 (db2, ("person", field), pos)) posl
    in
    let s = Name.crush_lower s in
    let sl =
      Hashtbl.fold
        (fun _ p sl ->
           if Name.crush_lower (proj p) = s then proj p :: sl else sl)
        db2.patches.h_person []
    in
    let sl = List.sort_uniq compare sl in
    List.fold_left (fun istrl s -> Istr2New (db2, s) :: istrl) istrl sl
  in
  let rec self =
    {close_base =
      (fun () ->
         Hashtbl.iter (fun (f1, f2, f) ic -> close_in ic) db2.cache_chan);
     empty_person =
       (fun ip ->
          Person2
            (db2, ip,
             {per2 = Some (Some (no_person "" ip));
              asc2 = Some (Some no_ascend); uni2 = Some (Some no_union)}));
     person_of_gen_person =
       (fun (p, a, u) ->
          Person2
            (db2, p.key_index,
             {per2 = Some (Some (map_person_ps (fun p -> p) un_istr2 p));
              asc2 = Some (Some a); uni2 = Some (Some u)}));
     family_of_gen_family =
       (fun (f, c, d) ->
          Family2
            (db2, f.fam_index,
             {fam2 = Some (Some (map_family_ps (fun p -> p) (fun f -> f) un_istr2 f));
              cpl2 = Some (Some c); des2 = Some (Some d)}));
     poi =
       (fun i ->
          Person2
            (db2, i,
             {per2 = None; asc2 = None; uni2 = None}));
     foi =
       (fun i ->
          Family2
            (db2, i,
             {fam2 = None; cpl2 = None; des2 = None}));
     sou =
       (fun i ->
          match i with
            Istr2 (db2, f, pos) -> string_of_istr2 db2 f pos
          | Istr2New (db2, s) -> s
       );
     nb_of_persons = (fun () -> db2.patches.nb_per);
     nb_of_families = (fun () -> db2.patches.nb_fam);
     patch_person =
       (fun ip p ->
          let p = map_person_ps (fun p -> p) un_istr2 p in
          Hashtbl.replace db2.patches.h_person ip p;
          db2.patches.nb_per <-
            max (ip + 1) db2.patches.nb_per);
     patch_ascend =
       (fun ip a ->
          Hashtbl.replace db2.patches.h_ascend ip a;
          db2.patches.nb_per <-
            max (ip + 1) db2.patches.nb_per);
     patch_union =
       (fun ip u ->
          Hashtbl.replace db2.patches.h_union ip u;
          db2.patches.nb_per <-
            max (ip + 1) db2.patches.nb_per);
     patch_family =
       (fun ifam f ->
          let f = map_family_ps (fun p -> p) (fun f -> f) un_istr2 f in
          Hashtbl.replace db2.patches.h_family ifam f;
          db2.patches.nb_fam <-
            max (ifam + 1) db2.patches.nb_fam);
     patch_descend =
       (fun ifam d ->
          Hashtbl.replace db2.patches.h_descend ifam d;
          db2.patches.nb_fam <-
            max (ifam + 1) db2.patches.nb_fam);
     patch_couple =
       (fun ifam c ->
          Hashtbl.replace db2.patches.h_couple ifam c;
          db2.patches.nb_fam <-
            max (ifam + 1) db2.patches.nb_fam);
     patch_name =
       (fun s ip ->
          let s = Name.crush_lower s in
          let ht = db2.patches.h_name in
          try
            let ipl = Hashtbl.find ht s in
            if List.mem ip ipl then () else Hashtbl.replace ht s (ip :: ipl)
          with Not_found -> Hashtbl.add ht s [ip]);
     patch_key =
       (fun ip fn sn occ ->
          let fn = Name.lower (nominative fn) in
          let sn = Name.lower (nominative sn) in
          Hashtbl.replace db2.patches.h_key (fn, sn, occ) (Some ip));
     delete_key =
       (fun fn sn occ ->
          let fn = Name.lower (nominative fn) in
          let sn = Name.lower (nominative sn) in
          match disk_person2_of_key db2 fn sn occ with
            Some _ -> Hashtbl.replace db2.patches.h_key (fn, sn, occ) None
          | None -> Hashtbl.remove db2.patches.h_key (fn, sn, occ));
     insert_string = (fun s -> Istr2New (db2, s));
     commit_patches = (fun () -> commit_patches2 db2);
     commit_notes = (fun fnotes s -> commit_notes2 db2 fnotes s);
     is_patched_person = (fun ip -> Hashtbl.mem db2.patches.h_person ip);
     patched_ascends =
       (fun () ->
          let r = ref [] in
          Hashtbl.iter (fun ip _ -> r := ip :: !r) db2.patches.h_ascend; !r);
     delete_family = (fun ifam -> C_base.delete_family self ifam);
     person_of_key = (fun fn sn oc -> person2_of_key db2 fn sn oc);
     persons_of_name = (fun s -> persons2_of_name db2 s);
     persons_of_first_name =
       (fun () -> Spi2 (db2, persons_of_first_name_or_surname2 db2 true));
     persons_of_surname =
       (fun () -> Spi2 (db2, persons_of_first_name_or_surname2 db2 false));
     base_visible_get = (fun f -> failwith "not impl visible_get");
     base_visible_write = (fun () -> failwith "not impl visible_write");
     base_particles =
       (fun () ->
          Mutil.input_particles (Filename.concat db2.bdir2 "particles.txt"));
     base_strings_of_first_name =
       (fun s ->
          base_strings_of_first_name_or_surname "first_name"
            (fun p -> p.first_name) s);
     base_strings_of_surname =
       (fun s ->
          base_strings_of_first_name_or_surname "surname" (fun p -> p.surname)
            s);
     load_ascends_array =
       (fun () ->
          eprintf "*** loading ascends array\n";
          flush stderr;
          let nb = db2.patches.nb_per in
          let nb_ini = db2.patches.nb_per_ini in
          begin match db2.parents_array with
            Some _ -> ()
          | None -> db2.parents_array <- Some (parents_array2 db2 nb_ini nb)
          end;
          match db2.consang_array with
            Some _ -> ()
          | None -> db2.consang_array <- Some (consang_array2 db2 nb));
     load_unions_array =
       (fun () ->
          match db2.family_array with
            Some _ -> ()
          | None ->
              eprintf "*** loading unions array\n";
              flush stderr;
              db2.family_array <- Some (family_array2 db2));
     load_couples_array = (fun () -> load_couples_array2 db2);
     load_descends_array =
       (fun () ->
          match db2.children_array with
            Some _ -> ()
          | None ->
              eprintf "*** loading descends array\n";
              flush stderr;
              db2.children_array <- Some (children_array2 db2));
     load_strings_array = (fun () -> ());
     persons_array = (fun () -> failwith "not impl persons_array");
     ascends_array =
       (fun () ->
          let nb = db2.patches.nb_per in
          let nb_ini = db2.patches.nb_per_ini in
          let ptab =
            match db2.parents_array with
              Some tab -> tab
            | None -> parents_array2 db2 nb_ini nb
          in
          let cg_tab =
            match db2.consang_array with
              Some tab -> tab
            | None -> consang_array2 db2 nb
          in
          let fget i = ptab.(i) in
          let cget i = cg_tab.(i) in
          let cset i v = cg_tab.(i) <- v in fget, cget, cset, Some cg_tab);
     base_notes_read = (fun fnotes -> read_notes db2 fnotes RnAll);
     base_notes_read_first_line = (fun fnotes -> read_notes db2 fnotes Rn1Ln);
     base_notes_are_empty = (fun fnotes -> read_notes db2 fnotes RnDeg = "");
     base_notes_origin_file =
       (fun () ->
          let fname = Filename.concat db2.bdir2 "notes_of.txt" in
          match try Some (Secure.open_in fname) with Sys_error _ -> None with
            Some ic -> let r = input_line ic in close_in ic; r
          | None -> "");
     base_notes_dir = (fun () -> Filename.concat "base_d" "notes_d");
     base_wiznotes_dir = (fun () -> Filename.concat "base_d" "wiznotes_d");
     nobtit = (fun conf p -> C_base.nobtit self conf p);
     p_first_name = (fun p -> C_base.p_first_name self p);
     p_surname = (fun p -> C_base.p_surname self p);
     date_of_last_change =
       (fun () ->
          let s =
            let bdir = db2.bdir2 in
            try Unix.stat (Filename.concat bdir "patches") with
              Unix.Unix_error (_, _, _) -> Unix.stat bdir
          in
          s.Unix.st_mtime);
     apply_base2 = fun f -> f db2}
  in
  self

(* Database - user functions *)

let open_base bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  base2 (base_of_base2 bname)

let apply_base2 b = b.apply_base2
let ascends_array b = b.ascends_array ()
let base_notes_are_empty b = b.base_notes_are_empty
let base_notes_dir b = b.base_notes_dir ()
let base_notes_origin_file b = b.base_notes_origin_file ()
let base_notes_read b = b.base_notes_read
let base_notes_read_first_line b = b.base_notes_read_first_line
let base_particles b = b.base_particles ()
let base_strings_of_first_name b = b.base_strings_of_first_name
let base_strings_of_surname b = b.base_strings_of_surname
let base_visible_get b = b.base_visible_get
let base_visible_write b = b.base_visible_write ()
let base_wiznotes_dir b = b.base_wiznotes_dir ()
let close_base b = b.close_base ()
let commit_notes b = b.commit_notes
let commit_patches b = b.commit_patches ()
let date_of_last_change b = b.date_of_last_change ()
let delete_family b = b.delete_family
let delete_key b = b.delete_key
let empty_person b = b.empty_person
let family_of_gen_family b = b.family_of_gen_family
let foi b = b.foi
let insert_string b = b.insert_string
let is_patched_person b = b.is_patched_person
let load_ascends_array b = b.load_ascends_array ()
let load_couples_array b = b.load_couples_array ()
let load_descends_array b = b.load_descends_array ()
let load_strings_array b = b.load_strings_array ()
let load_unions_array b = b.load_unions_array ()
let nb_of_families b = b.nb_of_families ()
let nb_of_persons b = b.nb_of_persons ()
let nobtit b = b.nobtit
let p_first_name b = b.p_first_name
let p_surname b = b.p_surname
let patch_ascend b = b.patch_ascend
let patch_couple b = b.patch_couple
let patch_descend b = b.patch_descend
let patch_family b = b.patch_family
let patch_key b = b.patch_key
let patch_name b = b.patch_name
let patch_person b = b.patch_person
let patch_union b = b.patch_union
let patched_ascends b = b.patched_ascends ()
let person_of_gen_person b = b.person_of_gen_person
let person_of_key b = b.person_of_key
let persons_array b = b.persons_array ()
let persons_of_first_name b = b.persons_of_first_name ()
let persons_of_name b = b.persons_of_name
let persons_of_surname b = b.persons_of_surname ()
let poi b = b.poi
let sou b = b.sou

let husbands base gp =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
       let fam = foi base ifam in
       let husband = poi base (get_father fam) in
       let husband_surname = get_surname husband in
       let husband_surnames_aliases = get_surnames_aliases husband in
       husband_surname, husband_surnames_aliases)
    (get_family p)

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
    Some ifam ->
      let fam = foi base ifam in
      let fath = poi base (get_father fam) in
      nobtit fath
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names sou is_empty_string is_quest_string
    (p.first_name) (p.surname)
    (p.public_name) (p.qualifiers) (p.aliases)
    (p.first_names_aliases) (p.surnames_aliases)
    nobtit
    (if p.sex = Female then husbands base p else [||])
    (father_titles_places base p nobtit_fun)

let gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit

(* TODO *)

module Collection = struct
  type 'a t
  let length _ = assert false
  let map _ = assert false
  let iter _ = assert false
  let iteri _ = assert false
  let fold ?from:_ ?until:_ _ = assert false
  let fold_until _ = assert false
  let iterator _ = assert false
end

module Marker = struct
  type ('k, 'v) t
  let get _ = assert false
  let set _ = assert false
end

let dummy_collection _ = assert false
let dummy_ifam = Obj.magic ()
let dummy_iper = Obj.magic ()
let dummy_marker _ = assert false
let empty_family _ = assert false
let empty_person _ = assert false
let empty_string = ()
let istr_of_string _ = assert false
let load_ascends_array _ = assert false
let load_couples_array _ = assert false
let load_descends_array _ = assert false
let load_families_array _ = assert false
let load_persons_array _ = assert false
let load_strings_array _ = assert false
let load_unions_array _ = assert false
let string_of_istr _ = assert false
let quest_string = ()
let sync ?scratch:_ = assert false
let write_nldb _ = assert false
let get_ifam _ = assert false
let get_iper _ = assert false
let make _ = assert false
let nb_of_families _ = assert false
let nb_of_persons _ = assert false
let nb_of_real_persons _ = assert false
let new_ifam _ = assert false
let new_iper _ = assert false
let no_ascend = { Def.parents = None ; consang = Adef.no_consang }
let no_couple = Adef.couple dummy_iper dummy_iper
let no_descend = { Def.children = [||] }
let no_family ifam =
  { Def.marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [||]
  ; relation = Def.NoMention
  ; divorce = Def.NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = ifam
  }
let no_person ip =
  { Def.first_name = empty_string
  ; surname = empty_string
  ; occ = 0
  ; image = empty_string
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; public_name = empty_string
  ; qualifiers = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; aliases = []
  ; occupation = empty_string
  ; sex = Def.Neuter
  ; access = Def.Private
  ; birth = Adef.cdate_None
  ; birth_place = empty_string
  ; birth_note = empty_string
  ; birth_src = empty_string
  ; baptism = Adef.cdate_None
  ; baptism_place = empty_string
  ; baptism_note = empty_string
  ; baptism_src = empty_string
  ; death = Def.DontKnowIfDead
  ; death_place = empty_string
  ; death_note = empty_string
  ; death_src = empty_string
  ; burial = Def.UnknownBurial
  ; burial_place = empty_string
  ; burial_note = empty_string
  ; burial_src = empty_string
  ; pevents = []
  ; notes = empty_string
  ; psources = empty_string
  ; key_index = ip }
let no_union = { Def.family = [||] }
let read_nldb _ = assert false
