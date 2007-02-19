(* camlp4r ./pa_lock.cmo *)
(* $Id: gwc2.ml,v 5.36 2007-02-19 02:20:58 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def;
open Futil;
open Gwcomp;
open Printf;

type family =
  { fam : gen_family iper string;
    cpl : gen_couple iper;
    des : gen_descend iper }
;

type file_field 'a =
  { oc_dat : out_channel;
    oc_acc : out_channel;
    start_pos : Iovalue.header_pos;
    sz32 : mutable int;
    sz64 : mutable int;
    item_cnt : mutable int;
    valu : 'a -> Obj.t }
;

type key =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

type gen =
  { g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;
    g_tmp_dir : string;
    g_particles : list string;

    g_strings : Hashtbl.t string Adef.istr;
    g_index_of_key : Hashtbl.t key iper;
    g_person_fields : list (file_field (gen_person iper string));
    g_family_fields : list (file_field family);
    g_person_parents : (Iochan.t * out_channel);
    g_person_unions : (Iochan.t * out_channel);
    g_person_rparents : (Iochan.t * out_channel);
    g_person_related : (Iochan.t * Iochan.t);
    g_person_notes : (Iochan.t * out_channel)}
;

value int_size = 4;

value default_source = ref "";
value do_check = ref True;

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value iter_option f =
  fun
  [ Some x -> f x
  | None -> () ]
;

value check_magic =
  let b = String.create (String.length magic_gwo) in
  fun fname ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_gwo then
        if String.sub magic_gwo 0 4 = String.sub b 0 4 then
          failwith
            ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
        else
          failwith
            ("\"" ^ fname ^
               "\" is not a GeneWeb object file, or it is a very old version")
      else ()
    }
;

value phony_min_size = 8;

type hashtbl_t 'a 'b =
  { size: mutable int;
    data: mutable array (bucketlist 'a 'b) }
and bucketlist 'a 'b =
  [ Empty
  | Cons of 'a and 'b and bucketlist 'a 'b ]
;

value output_hashtbl dir file ht = do {
  let oc_ht = open_out_bin (Filename.concat dir file) in
  let oc_hta = open_out_bin (Filename.concat dir (file ^ "a")) in
  let ht : hashtbl_t 'a 'b = Obj.magic (ht : Hashtbl.t 'a 'b) in
  output_binary_int oc_hta (Array.length ht.data);

  (* we could alternatively use Iovalue.output_array_access, the
     advantage would be that we could use output_value into the first
     file .ht and making the access into the second file .hta, this is
     probably faster, but the drawback is that we must know exactly
     where the array starts *)
  let pos_start = Iovalue.create_output_value_header oc_ht in
  Iovalue.output_block_header oc_ht 0 2;
  Iovalue.output oc_ht ht.size;
  Iovalue.output_block_header oc_ht 0 (Array.length ht.data);
  for i = 0 to Array.length ht.data - 1 do {
    output_binary_int oc_hta (pos_out oc_ht);
    Iovalue.output oc_ht ht.data.(i);
  };
  ignore (Iovalue.patch_output_value_header oc_ht pos_start : int);

  close_out oc_hta;
  close_out oc_ht;
};

value open_out_field tmp_dir (name, valu) = do {
  let d = Filename.concat tmp_dir name in
  try Mutil.mkdir_p d with _ -> ();

  let oc_dat = open_out_bin (Filename.concat d "data") in
  let oc_acc = open_out_bin (Filename.concat d "access") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 phony_min_size;
  assert (pos_out oc_dat = Db2.first_item_pos);
  {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = start_pos;
   sz32 = Iovalue.size_32.val; sz64 = Iovalue.size_64.val;
   item_cnt = 0; valu = valu}
};

value output_field so ff = do {
  output_binary_int ff.oc_acc (pos_out ff.oc_dat);
  Iovalue.size_32.val := ff.sz32;
  Iovalue.size_64.val := ff.sz64;
  Iovalue.output ff.oc_dat (ff.valu so);
  ff.sz32 := Iovalue.size_32.val;
  ff.sz64 := Iovalue.size_64.val;
  ff.item_cnt := ff.item_cnt + 1;
};

value close_out_field ff = do {
  close_out ff.oc_acc;
  Iovalue.size_32.val := ff.sz32 - phony_min_size + ff.item_cnt;
  Iovalue.size_64.val := ff.sz64 - phony_min_size + ff.item_cnt;
  ignore (Iovalue.patch_output_value_header ff.oc_dat ff.start_pos : int);
  Iovalue.output_block_header ff.oc_dat 0 ff.item_cnt;
  close_out ff.oc_dat;
};

value person_fields_arr =
 [("first_name", fun so -> Obj.repr so.first_name);
  ("surname", fun so -> Obj.repr so.surname);
  ("occ", fun so -> Obj.repr so.occ);
  ("image", fun so -> Obj.repr so.image);
  ("public_name", fun so -> Obj.repr so.public_name);
  ("qualifiers", fun so -> Obj.repr so.qualifiers);
  ("aliases", fun so -> Obj.repr so.aliases);
  ("first_names_aliases", fun so -> Obj.repr so.first_names_aliases);
  ("surnames_aliases", fun so -> Obj.repr so.surnames_aliases);
  ("titles", fun so -> Obj.repr so.titles);
  ("occupation", fun so -> Obj.repr so.occupation);
  ("sex", fun so -> Obj.repr so.sex);
  ("access", fun so -> Obj.repr so.access);
  ("birth", fun so -> Obj.repr so.birth);
  ("birth_place", fun so -> Obj.repr so.birth_place);
  ("birth_src", fun so -> Obj.repr so.birth_src);
  ("baptism", fun so -> Obj.repr so.baptism);
  ("baptism_place", fun so -> Obj.repr so.baptism_place);
  ("baptism_src", fun so -> Obj.repr so.baptism_src);
  ("death", fun so -> Obj.repr so.death);
  ("death_place", fun so -> Obj.repr so.death_place);
  ("death_src", fun so -> Obj.repr so.death_src);
  ("burial", fun so -> Obj.repr so.burial);
  ("burial_place", fun so -> Obj.repr so.burial_place);
  ("burial_src", fun so -> Obj.repr so.burial_src);
  ("psources", fun so -> Obj.repr so.psources)]
;

value family_fields_arr =
  [("marriage", fun so -> Obj.repr so.fam.marriage);
   ("marriage_place", fun so -> Obj.repr so.fam.marriage_place);
   ("marriage_src", fun so -> Obj.repr so.fam.marriage_src);
   ("witnesses", fun so -> Obj.repr so.fam.witnesses);
   ("relation", fun so -> Obj.repr so.fam.relation);
   ("divorce", fun so -> Obj.repr so.fam.divorce);
   ("comment", fun so -> Obj.repr so.fam.comment);
   ("origin_file", fun so -> Obj.repr so.fam.origin_file);
   ("fsources", fun so -> Obj.repr so.fam.fsources);
   ("father", fun so -> Obj.repr (Adef.father so.cpl));
   ("mother", fun so -> Obj.repr (Adef.mother so.cpl));
   ("children", fun so -> Obj.repr so.des.children)]
;

value make_string_of_crush_index tmp_dir =
  List.iter
    (fun field -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; "person"; field]
       in
       let ic_dat = open_in_bin (Filename.concat field_d "data") in
       if Mutil.verbose.val then do {
         eprintf "string_of_crush %s..." field;
         flush stderr;
       }
       else ();
       let ht = Hashtbl.create 1 in
       seek_in ic_dat Db2.empty_string_pos;
       loop Db2.empty_string_pos where rec loop pos =
         match
           try Some (Iovalue.input ic_dat) with [ End_of_file -> None ]
         with
         [ Some s -> do {
             let k = Name.crush_lower s in
             let posl = Hashtbl.find_all ht k in
             if List.mem pos posl then () else Hashtbl.add ht k pos;
             loop (pos_in ic_dat)
           }
         | None -> () ];
       close_in ic_dat;
       output_hashtbl field_d "string_of_crush.ht" ht;
       if Mutil.verbose.val then do {
         eprintf "\n";
         flush stderr;
       }
       else ();
    })
   ["first_name"; "surname"]
;

value make_person_of_string_index tmp_dir =
  List.iter
    (fun field -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; "person"; field]
       in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       if Mutil.verbose.val then do {
         eprintf "person_of_string %s..." field;
         flush stderr;
       }
       else ();
       let ht = Hashtbl.create 1 in
       loop 0 where rec loop i =
         match
           try Some (input_binary_int ic_acc) with [ End_of_file -> None ]
         with
         [ Some pos -> do { Hashtbl.add ht pos i; loop (i + 1) }
         | None -> () ];
       close_in ic_acc;
       output_hashtbl field_d "person_of_string.ht" ht;
       if Mutil.verbose.val then do {
         eprintf "\n";
         flush stderr;
       }
       else ();
     })
    ["first_name"; "surname"]
;

value read_field (ic_acc, ic_dat) i = do {
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  seek_in ic_dat pos;
  Iovalue.input ic_dat
};

value read_string_field : _ -> _ -> string = read_field;
value read_int_field : _ -> _ -> int = read_field;
value read_array_int_field : _ -> _ -> array int = read_field;

value read_string_list_field (ic_acc, ic_dat, ic_str) i = do {
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  if pos = -1 then []
  else do {
    seek_in ic_dat pos;
    let posl : list int = Iovalue.input ic_dat in
    List.map
      (fun pos -> do {
         seek_in ic_str pos;
         (Iovalue.input ic_str : string)
       })
      posl
  }
};

value read_int_array_field (ic_acc, ic_dat) i = do {
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  loop [] pos where rec loop list pos =
    if pos = -1 then Array.of_list list
    else do {
      seek_in ic_dat pos;
      let i = Iovalue.input ic_dat in
      loop [i :: list] (Iovalue.input ic_dat)
    }
};

value read_title_list_field (ic_acc, ic_dat, ic_str) i = do {
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  if pos = -1 then []
  else do {
    seek_in ic_dat pos;
    let tl : list (gen_title int) = Iovalue.input ic_dat in
    List.map
      (map_title_strings
        (fun pos -> do {
           seek_in ic_str pos;
           (Iovalue.input ic_str : string)
         }))
      tl
  }
};

value str_pos oc_str ht item_cnt s =
  try Hashtbl.find ht s with
  [ Not_found -> do {
      incr item_cnt;
      let pos = pos_out oc_str in
      Iovalue.output oc_str s;
      Hashtbl.add ht s pos;
      pos
    } ]
;

value output_value_array_string oc_str f = do {
  let ht = Hashtbl.create 1 in
  let header_pos = Iovalue.create_output_value_header oc_str in
  Iovalue.output_block_header oc_str 0 phony_min_size;
  assert (pos_out oc_str = Db2.first_item_pos);
  let nb_items = ref 0 in
  let istr_empty = str_pos oc_str ht nb_items "" in
  let istr_quest = str_pos oc_str ht nb_items "?" in
  assert (istr_empty = Db2.empty_string_pos);
  assert (istr_quest = Db2.quest_string_pos);
  f (str_pos oc_str ht nb_items);
  (* padding to at least 8 items to allow correct read by input_value *)
  for i = nb_items.val + 1 to phony_min_size do {
    incr nb_items;
    Iovalue.output oc_str "";
  };
  Iovalue.size_32.val := Iovalue.size_32.val - phony_min_size + nb_items.val;
  Iovalue.size_64.val := Iovalue.size_32.val - phony_min_size + nb_items.val;
  ignore (Iovalue.patch_output_value_header oc_str header_pos : int);
  Iovalue.output_block_header oc_str 0 nb_items.val;
};

value compress_type_string field_d ic oc oc_str =
  output_value_array_string oc_str
    (fun output_string ->
       try
         while True do {
           let s : string = Iovalue.input ic in
           let pos = output_string s in
           output_binary_int oc pos
         }
       with
       [ End_of_file -> () ])
;

value compress_type_list_string field_d ic oc oc_str = do {
  let ht = Hashtbl.create 1 in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  try
    let items_cnt = ref 0 in
    while True do {
      let sl : list string = Iovalue.input ic in
      match sl with
      [ [_ :: _] -> do {
          output_binary_int oc (pos_out oc_ext);
          let sl = List.map (str_pos oc_str ht items_cnt) sl in
          Iovalue.output oc_ext (sl : list int)
        }
      | [] ->
          output_binary_int oc (-1) ]
    }
  with
  [ End_of_file -> () ];
  close_out oc_ext;
};

value compress_type_list_title field_d ic oc oc_str = do {
  let ht = Hashtbl.create 1 in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  try
    let items_cnt = ref 0 in
    while True do {
      let tl : list (gen_title string) = Iovalue.input ic in
      match tl with
      [ [_ :: _] -> do {
          output_binary_int oc (pos_out oc_ext);
          let tl =
            List.map (map_title_strings (str_pos oc_str ht items_cnt)) tl
          in
          Iovalue.output oc_ext (tl : list (gen_title int))
        }
      | [] ->
          output_binary_int oc (-1) ]
    }
  with
  [ End_of_file -> () ];
  close_out oc_ext;
};

value compress_fields tmp_dir =
  List.iter
    (fun (f1, f2, compress_type) -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
       in
       let ic = open_in_bin (Filename.concat field_d "data") in
       if Mutil.verbose.val then do {
         eprintf "compressing %s..." f2;
         flush stderr;
       }
       else ();
       let oc_acc2 = open_out_bin (Filename.concat field_d "access2") in
       let oc_dat2 = open_out_bin (Filename.concat field_d "data2") in
       seek_in ic Db2.first_item_pos;

       compress_type field_d ic oc_acc2 oc_dat2;

       close_out oc_dat2;
       close_out oc_acc2;
       close_in ic;
       List.iter
         (fun n -> do {
            let f = Filename.concat field_d n in
            Mutil.remove_file f;
            Sys.rename (Filename.concat field_d (n ^ "2")) f;
          })
         ["data"; "access"];
       if Mutil.verbose.val then do {
         Printf.eprintf "\n";
         flush stderr
       }
       else ();
     })
    [("person", "baptism_place", compress_type_string);
     ("person", "baptism_src", compress_type_string);
     ("person", "birth_place", compress_type_string);
     ("person", "birth_src", compress_type_string);
     ("person", "burial_place", compress_type_string);
     ("person", "burial_src", compress_type_string);
     ("family", "comment", compress_type_string);
     ("person", "death_place", compress_type_string);
     ("person", "death_src", compress_type_string);
     ("person", "first_name", compress_type_string);
     ("family", "fsources", compress_type_string);
     ("person", "image", compress_type_string);
     ("family", "marriage_place", compress_type_string);
     ("family", "marriage_src", compress_type_string);
     ("person", "occupation", compress_type_string);
     ("family", "origin_file", compress_type_string);
     ("person", "psources", compress_type_string);
     ("person", "public_name", compress_type_string);
     ("person", "surname", compress_type_string);

     ("person", "aliases", compress_type_list_string);
     ("person", "first_names_aliases", compress_type_list_string);
     ("person", "qualifiers", compress_type_list_string);
     ("person", "surnames_aliases", compress_type_list_string);
     ("person", "titles", compress_type_list_title)]
;

value reorder_type_list_int field_d ic_acc ic_dat ff = do {
  let item_cnt = ref 0 in
  try
    while True do {
      let x = read_int_array_field (ic_acc, ic_dat) item_cnt.val in
      output_field x ff;
      incr item_cnt;
    }
  with
  [ End_of_file -> () ];
};

value reorder_fields tmp_dir =
  List.iter
    (fun (f1, f2, reorder_type) -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
       in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       let ic_dat = open_in_bin (Filename.concat field_d "data") in
       if Mutil.verbose.val then do {
         eprintf "reordering %s..." f2;
         flush stderr;
       }
       else ();
       let ff = do {
         let oc_dat = open_out_bin (Filename.concat field_d "data2") in
         let oc_acc = open_out_bin (Filename.concat field_d "access2") in
         let start_pos = Iovalue.create_output_value_header oc_dat in
         Iovalue.output_block_header oc_dat 0 phony_min_size;
         assert (pos_out oc_dat = Db2.first_item_pos);
         {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = start_pos;
          sz32 = Iovalue.size_32.val; sz64 = Iovalue.size_64.val;
          item_cnt = 0; valu = Obj.repr}
       }
       in
       reorder_type field_d ic_acc ic_dat ff;

       close_out_field ff;
       close_in ic_dat;
       close_in ic_acc;
       List.iter
         (fun n -> do {
            let f = Filename.concat field_d n in
            Mutil.remove_file f;
            Sys.rename (Filename.concat field_d (n ^ "2")) f;
          })
         ["data"; "access"];
       if Mutil.verbose.val then do {
         Printf.eprintf "\n";
         flush stderr
       }
       else ();
     })
    [("person", "family", reorder_type_list_int)]
;

value make_name_index tmp_dir nbper = do {
  if Mutil.verbose.val then do {
    eprintf "name index...\n";
    flush stderr;
  }
  else ();        
  let base_d = Filename.concat tmp_dir "base_d" in
  let ic2_list =
    List.map
      (fun (d, f) ->
         let d = List.fold_left Filename.concat base_d [d; f] in
         let fn_acc = Filename.concat d "access" in
         let fn_dat = Filename.concat d "data" in
         let ic_acc = open_in_bin fn_acc in
         let ic_dat = open_in_bin fn_dat in
         (f, (ic_acc, ic_dat)))
      [("person", "first_name"); ("person", "surname");
       ("person", "public_name"); ("person", "sex"); ("person", "family");
       ("family", "father"); ("person", "parents")]
  in
  let ic3_list =
    List.map
      (fun f ->
         let d = List.fold_left Filename.concat base_d ["person"; f] in
         let fn_acc = Filename.concat d "access" in
         let ic_acc = open_in_bin fn_acc in
         let fn_dat = Filename.concat d "data2.ext" in
         let ic_dat = open_in_bin fn_dat in
         let fn_str = Filename.concat d "data" in
         let ic_str = open_in_bin fn_str in
         (f, (ic_acc, ic_dat, ic_str)))
      ["qualifiers"; "aliases"; "first_names_aliases"; "surnames_aliases";
       "titles"]
  in
  let get_first_name = read_string_field (List.assoc "first_name" ic2_list) in
  let get_surname = read_string_field (List.assoc "surname" ic2_list) in
  let get_public_name =
    read_string_field (List.assoc "public_name" ic2_list)
  in
  let get_qualifiers =
    read_string_list_field (List.assoc "qualifiers" ic3_list)
  in
  let get_aliases = read_string_list_field (List.assoc "aliases" ic3_list) in
  let get_first_names_aliases =
    read_string_list_field (List.assoc "first_names_aliases" ic3_list)
  in
  let get_surnames_aliases =
    read_string_list_field (List.assoc "surnames_aliases" ic3_list)
  in
  let get_titles = read_title_list_field (List.assoc "titles" ic3_list) in
  let get_family = read_array_int_field (List.assoc "family" ic2_list) in
  let get_father = read_int_field (List.assoc "father" ic2_list) in
  let get_husbands =
    let (ic_acc, ic_dat) = List.assoc "sex" ic2_list in
    fun i -> do {
      seek_in ic_acc (4 * i);
      let pos = input_binary_int ic_acc in
      seek_in ic_dat pos;
      let sex : sex = Iovalue.input ic_dat in
      if sex = Female then
        List.map
          (fun ifam ->
             let husb = get_father ifam in
             let husb_surname = get_surname husb in
             let husb_surn_ali = get_surnames_aliases husb in
             (husb_surname, husb_surn_ali))
          (Array.to_list (get_family i))
      else []
    }
  in
  let get_parents =
    let (ic_acc, ic_dat) = List.assoc "parents" ic2_list in
    fun i -> do {
      seek_in ic_acc (4 * i);
      let pos = input_binary_int ic_acc in
      if pos = -1 then None
      else do {
        seek_in ic_dat pos;
        Some (Iovalue.input ic_dat)
      }
    }
  in
  let get_father_titles_places i = do {
    match get_parents i with
    [ Some ifam ->
        let ifath = get_father ifam in
        List.map (fun t -> t.t_place) (get_titles ifath)
    | None -> [] ]
  }
  in
  let ht = Hashtbl.create 1 in
  ProgrBar.start ();
  for i = 0 to nbper - 1 do {
    ProgrBar.run i nbper;
    let first_name = get_first_name i in
    let surname = get_surname i in
    let names =
      let names =
        Futil.gen_person_misc_names first_name surname
          (get_public_name i) (get_qualifiers i) (get_aliases i)
          (get_first_names_aliases i) (get_surnames_aliases i)
          (get_titles i) (get_husbands i) (get_father_titles_places i)
      in
      [Name.lower (first_name ^ " " ^ surname) :: names]
    in
    List.iter (fun s -> Hashtbl.add ht (Name.crush_lower s) i) names;
  };
  ProgrBar.finish ();
  List.iter
    (fun (_, (ic_acc, ic_dat)) -> do { close_in ic_acc; close_in ic_dat })
    ic2_list;
  List.iter
    (fun (_, (ic_acc, ic_dat, ic_str)) -> do {
       close_in ic_acc;
       close_in ic_dat;
       close_in ic_str
     })
    ic3_list;
  let dir =
    List.fold_left Filename.concat tmp_dir ["base_d"; "person_of_name"]
  in
  Mutil.mkdir_p dir;
  output_hashtbl dir "person_of_name.ht" ht;
};

value start_with s p =
  String.length p < String.length s &&
  String.sub s 0 (String.length p) = p
;

value make_index gen f2 = do {
  let f1 = "person" in
  let bdir = Filename.concat gen.g_tmp_dir "base_d" in
  let fdir = List.fold_left Filename.concat bdir [f1; f2] in
  let index_dat_fname = Filename.concat fdir "index.dat" in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let data_fname = Filename.concat fdir "data" in
  let ic = open_in_bin data_fname in
  seek_in ic Db2.first_item_pos;
  let (list, len) =
    loop [] 0 Db2.first_item_pos where rec loop list len pos =
      match
        try Some (Iovalue.input ic : string) with
        [ End_of_file -> None ]
      with
      [ Some s ->
          let s =
            try
              let part = List.find (start_with s) gen.g_particles in
              let plen = String.length part in
              String.sub s plen (String.length s - plen) ^ " (" ^
              part ^ ")"
            with
            [ Not_found -> s ]
          in
          let list = [(s, pos) :: list] in
          loop list (len + 1) (pos_in ic)
      | None -> (list, len) ]
  in
  let list = List.sort compare list in
  let a = Array.make len ("", 0) in
  let iofc =
    loop [] 0 list where rec loop rev_iofc i =
      fun
      [ [] -> List.rev rev_iofc
      | [((s, _) as s_pos) :: list] -> do {
          a.(i) := s_pos;
          let rev_iofc =
            match rev_iofc with
            [ [(prev_s, _) :: _] ->
                if prev_s = "" then [(s, i) :: rev_iofc]
                else
                  let prev_nbc = Name.nbc prev_s.[0] in
                  let nbc = Name.nbc s.[0] in
                  if prev_nbc = nbc && nbc > 0 &&
                     nbc <= String.length prev_s &&
                     nbc <= String.length s &&
                     String.sub prev_s 0 nbc = String.sub s 0 nbc
                  then
                    rev_iofc
                  else
                    [(s, i) :: rev_iofc]
            | [] -> [s_pos] ]
          in
          loop rev_iofc (i + 1) list
        } ]
  in
  let oc = open_out_bin index_dat_fname in
  output_value oc (a : array (string * int));
  close_out oc;
  let oc = open_out_bin (Filename.concat fdir "index.acc") in
  let _ : int =
    Iovalue.output_array_access oc (Array.get a) (Array.length a) 0
  in
  close_out oc;
  let oc = open_out_bin index_ini_fname in
  output_value oc (iofc : list (string * int));
  close_out oc;
};

value unique_key_string gen s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find gen.g_strings s with
  [ Not_found -> do {
      let istr = Adef.istr_of_int gen.g_scnt in
      Hashtbl.add gen.g_strings s istr;
      gen.g_scnt := gen.g_scnt + 1;
      istr
    } ]
;

value empty_person =
  {first_name = ""; surname = ""; occ = 0; image = "";
   first_names_aliases = []; surnames_aliases = []; public_name = "";
   qualifiers = []; aliases = []; titles = []; rparents = []; related = [];
   occupation = ""; sex = Neuter; access = IfTitles;
   birth = Adef.codate_None; birth_place = ""; birth_src = "";
   baptism = Adef.codate_None; baptism_place = ""; baptism_src = "";
   death = DontKnowIfDead; death_place = ""; death_src = "";
   burial = UnknownBurial; burial_place = ""; burial_src = ""; notes = "";
   psources = ""; key_index = Adef.iper_of_int 0}
;

value key_hashtbl_add ht (fn, sn, oc) v =
  let k = if oc = 0 then Key0 fn sn else Key fn sn oc in
  Hashtbl.add ht k v
;
value key_hashtbl_find ht (fn, sn, oc) =
  let k = if oc = 0 then Key0 fn sn else Key fn sn oc in
  Hashtbl.find ht k
;

value insert_person1 gen so = do {
  if so.first_name <> "?" && so.surname <> "?" then do {
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let k = (fn, sn, so.occ) in
    try do {
      let _ = key_hashtbl_find gen.g_index_of_key k in
      eprintf "already defined %s.%d %s\n" so.first_name so.occ so.surname;
      flush stderr;
    }
    with
    [ Not_found -> do {
        key_hashtbl_add gen.g_index_of_key k (Adef.iper_of_int gen.g_pcnt);
        List.iter (output_field so) gen.g_person_fields;
        Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
        Iochan.output_binary_int (fst gen.g_person_parents) (-1);
        Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
        Iochan.output_binary_int (fst gen.g_person_unions) (-1);
        Iochan.seek (fst gen.g_person_rparents) (int_size * gen.g_pcnt);
        Iochan.output_binary_int (fst gen.g_person_rparents) (-1);
        Iochan.seek (fst gen.g_person_related) (int_size * gen.g_pcnt);
        Iochan.output_binary_int (fst gen.g_person_related) (-1);
        Iochan.seek (fst gen.g_person_notes) (int_size * gen.g_pcnt);
        Iochan.output_binary_int (fst gen.g_person_notes) (-1);
        gen.g_pcnt := gen.g_pcnt + 1;
      } ]
  }
  else ();
};

value insert_undefined2 gen key fn sn sex = do {
  if key.pk_first_name <> "?" && key.pk_surname <> "?" then
    key_hashtbl_add gen.g_index_of_key (fn, sn, key.pk_occ)
      (Adef.iper_of_int gen.g_pcnt)
  else ();
  if do_check.val then do {
    eprintf "Adding undefined %s.%d %s\n" (Name.lower key.pk_first_name)
      key.pk_occ (Name.lower key.pk_surname);
    flush stderr;
  }
  else ();
  let so =
    {(empty_person) with
     first_name = key.pk_first_name; surname = key.pk_surname;
     occ = key.pk_occ; sex = sex}
  in
  List.iter (output_field so) gen.g_person_fields;
  Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_parents) (-1);
  Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_unions) (-1);
  Iochan.seek (fst gen.g_person_rparents) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_rparents) (-1);
  Iochan.seek (fst gen.g_person_related) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_related) (-1);
  Iochan.seek (fst gen.g_person_notes) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_notes) (-1);
  gen.g_pcnt := gen.g_pcnt + 1;
  Adef.iper_of_int (gen.g_pcnt - 1)
};

value get_person2 gen so sex =
  if so.first_name <> "?" && so.surname <> "?" then do {
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    try key_hashtbl_find gen.g_index_of_key (fn, sn, so.occ) with
    [ Not_found ->
        failwith
          (sprintf "*** bug not found %s.%d %s" so.first_name so.occ
             so.surname) ]
  }
  else do {
    let so = if so.sex = Neuter then {(so) with sex = sex} else so in
    List.iter (output_field so) gen.g_person_fields;
    Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_parents) (-1);
    Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_unions) (-1);
    Iochan.seek (fst gen.g_person_rparents) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_rparents) (-1);
    Iochan.seek (fst gen.g_person_related) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_related) (-1);
    Iochan.seek (fst gen.g_person_notes) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_notes) (-1);
    gen.g_pcnt := gen.g_pcnt + 1;
    Adef.iper_of_int (gen.g_pcnt - 1)
  }
;

value get_undefined2 gen key sex =
  let fn = unique_key_string gen key.pk_first_name in
  let sn = unique_key_string gen key.pk_surname in
  try key_hashtbl_find gen.g_index_of_key (fn, sn, key.pk_occ) with
  [ Not_found -> insert_undefined2 gen key fn sn sex ]
;

value insert_somebody1 gen sex =
  fun
  [ Undefined key -> ()
  | Defined so ->
      let so = {(so) with sex = sex} in
      insert_person1 gen so ]
;

value get_somebody2 gen sex =
  fun
  [ Undefined key -> get_undefined2 gen key sex
  | Defined so -> get_person2 gen so sex ]
;

value insert_family1 gen co fath_sex moth_sex witl fo deo = do {
  let _ifath = insert_somebody1 gen fath_sex (Adef.father co) in
  let _imoth = insert_somebody1 gen moth_sex (Adef.mother co) in
  Array.iter (fun key -> insert_person1 gen key) deo.children;
  List.iter (fun (so, sex) -> insert_somebody1 gen sex so) witl;
};

value insert_family2 gen co fath_sex moth_sex witl fo deo = do {
  let ifath = get_somebody2 gen fath_sex (Adef.father co) in
  let imoth = get_somebody2 gen moth_sex (Adef.mother co) in
  let children =
    Array.map (fun key -> get_person2 gen key Neuter) deo.children
  in
  let witn = List.map (fun (so, sex) -> get_somebody2 gen sex so) witl in
  let fam =
    {fam ={(fo) with witnesses = Array.of_list witn};
     cpl = Adef.couple ifath imoth;
     des = {children = children}}
  in
  List.iter (output_field fam) gen.g_family_fields;

  let pos_acc_fath = pos_out (snd gen.g_person_unions) in

  Iochan.seek (fst gen.g_person_unions) (int_size * Adef.int_of_iper ifath);
  let pos_data_fath = Iochan.input_binary_int (fst gen.g_person_unions) in
  Iochan.seek (fst gen.g_person_unions) (int_size * Adef.int_of_iper ifath);
  Iochan.output_binary_int (fst gen.g_person_unions) pos_acc_fath;
  Iovalue.output (snd gen.g_person_unions) gen.g_fcnt;
  Iovalue.output (snd gen.g_person_unions) pos_data_fath;

  let pos_acc_moth = pos_out (snd gen.g_person_unions) in

  Iochan.seek (fst gen.g_person_unions) (int_size * Adef.int_of_iper imoth);
  let pos_data_moth = Iochan.input_binary_int (fst gen.g_person_unions) in
  Iochan.seek (fst gen.g_person_unions) (int_size * Adef.int_of_iper imoth);
  Iochan.output_binary_int (fst gen.g_person_unions) pos_acc_moth;
  Iovalue.output (snd gen.g_person_unions) gen.g_fcnt;
  Iovalue.output (snd gen.g_person_unions) pos_data_moth;

  let pos_acc_parents = pos_out (snd gen.g_person_parents) in
  Array.iter
    (fun iper -> do {
       Iochan.seek (fst gen.g_person_parents)
         (int_size * Adef.int_of_iper iper);
       Iochan.output_binary_int (fst gen.g_person_parents) pos_acc_parents
     })
    children;
  Iovalue.output (snd gen.g_person_parents) gen.g_fcnt;

  gen.g_fcnt := gen.g_fcnt + 1
};

value insert_notes2 gen key str = do {
  let ip = get_undefined2 gen key Neuter in
  let pos = pos_out (snd gen.g_person_notes) in
  Iovalue.output (snd gen.g_person_notes) str;
  Iochan.seek (fst gen.g_person_notes) (int_size * Adef.int_of_iper ip);
  Iochan.output_binary_int (fst gen.g_person_notes) pos;
};

value insert_relation1 gen r = do {
  iter_option (insert_somebody1 gen Male) r.r_fath;
  iter_option (insert_somebody1 gen Female) r.r_moth;
};

value insert_rparents1 gen sb sex rl = do {
  insert_somebody1 gen sex sb;
  List.iter (insert_relation1 gen) rl
};

value insert_relation_parent2 gen ip sex k = do {
  let irp = get_somebody2 gen sex k in
  let (ioc_acc, ioc_dat) = gen.g_person_related in
  Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
  let pos1 = Iochan.input_binary_int ioc_acc in
  loop pos1 where rec loop pos =
    if pos = -1 then do {
      let pos2 = Iochan.seek_end ioc_dat in
      Iochan.output_value_no_header ioc_dat (Adef.int_of_iper ip);
      Iochan.output_value_no_header ioc_dat pos1;
      Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
      Iochan.output_binary_int ioc_acc pos2;
    }
    else do {
      Iochan.seek ioc_dat pos;
      let i = Iochan.input_value_no_header ioc_dat in
      if i = Adef.int_of_iper ip then ()
      else loop (Iochan.input_value_no_header ioc_dat)
    };
  irp
};

value insert_relation2 gen ip r =
  let r_fath = map_option (insert_relation_parent2 gen ip Male) r.r_fath in
  let r_moth = map_option (insert_relation_parent2 gen ip Female) r.r_moth in
  {r_type = r.r_type; r_fath = r_fath; r_moth = r_moth;
   r_sources = 0}
;

value insert_rparents2 gen sb sex rl = do {
  let ip = get_somebody2 gen sex sb in
  let rl = List.map (insert_relation2 gen ip) rl in
  let pos = pos_out (snd gen.g_person_rparents) in
  Iovalue.output (snd gen.g_person_rparents) rl;
  Iochan.seek (fst gen.g_person_rparents) (int_size * Adef.int_of_iper ip);
  Iochan.output_binary_int (fst gen.g_person_rparents) pos;
};

value insert_bnotes1 gen srcfile notesname str = do {
  let nfname =
    if notesname = "" then "notes"
    else
      let f =
        match NotesLinks.check_file_name notesname with
        [ Some (dl, f) -> List.fold_right Filename.concat dl f
        | None -> "bad" ]
      in
      Filename.concat "notes_d" f
  in
  let fname =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; nfname ^ ".txt"]
  in
  Mutil.mkdir_p (Filename.dirname fname);
  let oc = Secure.open_out fname in
  output_string oc str;
  close_out oc;
  if notesname = "" then do {
    let fname =
      List.fold_left Filename.concat gen.g_tmp_dir
        ["base_d"; "notes_of.txt"]
    in
    let oc = Secure.open_out fname in
    fprintf oc "%s\n" srcfile;
    close_out oc;
  }
  else ();
};

value write_file_contents fname text = do {
  let oc = open_out fname in
  output_string oc text;
  close_out oc;
};

value insert_wiznotes1 gen wizid str = do {
  let wizdir =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; "wiznotes_d"]
  in
  Mutil.mkdir_p wizdir;
  let fname = Filename.concat wizdir wizid ^ ".txt" in
  write_file_contents fname str;
};

value insert_gwo_1 gen srcfile =
  fun
  [ Family cpl fs ms witl fam des -> insert_family1 gen cpl fs ms witl fam des
  | Notes key str -> ()
  | Relations sb sex rl -> insert_rparents1 gen sb sex rl
  | Bnotes nfname str -> insert_bnotes1 gen srcfile nfname str
  | Wnotes wizid str -> insert_wiznotes1 gen wizid str ]
;

value insert_gwo_2 gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family2 gen cpl fs ms witl fam des
  | Notes key str -> insert_notes2 gen key str
  | Relations sb sex rl -> insert_rparents2 gen sb sex rl
  | Bnotes nfname str -> ()
  | Wnotes wizid str -> () ]
;

value insert_comp_families1 gen run (x, separate, shift) =
  do {
    run ();
    let ic = open_in_bin x in
    check_magic x ic;
    let srcfile : string = input_value ic in
    try
      while True do {
        let fam : syntax_o = input_value ic in
        insert_gwo_1 gen srcfile fam
      }
    with
    [ End_of_file -> close_in ic ]
  }
;

value insert_comp_families2 gen run (x, separate, shift) =
  do {
    run ();
    let ic = open_in_bin x in
    check_magic x ic;
    let _ : string = input_value ic in
    try
      while True do {
        let fam : syntax_o = input_value ic in
        insert_gwo_2 gen fam
      }
    with
    [ End_of_file -> close_in ic ]
  }
;

value just_comp = ref False;
value out_file = ref (Filename.concat Filename.current_dir_name "a");
value force = ref False;
value do_consang = ref False;
value pr_stats = ref False;

value part_file = ref "";

value input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur ";
     "AF "; "D'"; "DAL "; "DE "; "DI "; "DU "; "OF "; "VAN ";
     "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file
;

value output_particles_file tmp_dir particles = do {
  let fname =
    List.fold_left Filename.concat tmp_dir ["base_d"; "particles.txt"]
  in
  let oc = open_out fname in
  List.iter (fun s -> fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
  close_out oc;
};

value link gwo_list bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let tmp_dir = Filename.concat "gw_tmp" bdir in
  do {
    Mutil.remove_dir tmp_dir;
    try Mutil.mkdir_p tmp_dir with _ -> ();
    let person_d =
      List.fold_left Filename.concat tmp_dir ["base_d"; "person"]
    in
    try Mutil.mkdir_p person_d with _ -> ();
    let person_fields =
      List.map (open_out_field person_d) person_fields_arr
    in
    let family_fields = do {
      let family_d =
        List.fold_left Filename.concat tmp_dir ["base_d"; "family"]
      in
      try Mutil.mkdir_p family_d with _ -> ();
      List.map (open_out_field family_d) family_fields_arr
    }
    in
    let person_parents = do {
      let d = Filename.concat person_d "parents" in
      try Mutil.mkdir_p d with _ -> ();
      (Iochan.openfile (Filename.concat d "access") True,
       open_out_bin (Filename.concat d "data"))
    }
    in
    let person_unions = do {
      let d = Filename.concat person_d "family" in
      try Mutil.mkdir_p d with _ -> ();
      (Iochan.openfile (Filename.concat d "access") True,
       open_out_bin (Filename.concat d "data"))
    }
    in
    let person_rparents = do {
      let d = Filename.concat person_d "rparents" in
      try Mutil.mkdir_p d with _ -> ();
      (Iochan.openfile (Filename.concat d "access") True,
       open_out_bin (Filename.concat d "data"))
    }
    in
    let person_related = do {
      let d = Filename.concat person_d "related" in
      try Mutil.mkdir_p d with _ -> ();
      (Iochan.openfile (Filename.concat d "access") True,
       Iochan.openfile (Filename.concat d "data") True)
    }
    in
    let person_notes = do {
      let d = Filename.concat person_d "notes" in
      try Mutil.mkdir_p d with _ -> ();
      (Iochan.openfile (Filename.concat d "access") True,
       open_out_bin (Filename.concat d "data"))
    }
    in
    let gen =
      {g_pcnt = 0; g_fcnt = 0; g_scnt = 0; g_tmp_dir = tmp_dir;
       g_particles = input_particles part_file.val;
       g_strings = Hashtbl.create 1;
       g_index_of_key = Hashtbl.create 1;
       g_person_fields = person_fields;
       g_family_fields = family_fields;
       g_person_parents = person_parents;
       g_person_unions = person_unions;
       g_person_rparents = person_rparents;
       g_person_related = person_related;
       g_person_notes = person_notes}
    in
    let ngwo = List.length gwo_list in
    if ngwo >= 10 && Mutil.verbose.val then do {
      eprintf "pass 1: creating persons...\n";
      flush stderr
    }
    else ();
    let run =
      if ngwo < 10 || not Mutil.verbose.val then fun () -> ()
      else if ngwo < 60 then
        fun () -> do { Printf.eprintf "."; flush stderr; }
      else do {
        let bar_cnt = ref 0 in
        let run () = do { ProgrBar.run bar_cnt.val ngwo; incr bar_cnt } in
        ProgrBar.empty.val := 'o';
        ProgrBar.full.val := '*';
        ProgrBar.start ();
        run
      }
    in
    List.iter (insert_comp_families1 gen run) gwo_list;

    if ngwo < 10 || not Mutil.verbose.val then ()
    else if ngwo < 60 then do { Printf.eprintf "\n"; flush stderr }
    else ProgrBar.finish ();

    Gc.compact ();

    if ngwo >= 10 || not Mutil.verbose.val then do {
      eprintf "pass 2: creating families...\n";
      flush stderr
    }
    else ();
    let run =
      if ngwo < 10 || not Mutil.verbose.val then fun () -> ()
      else if ngwo < 60 then
        fun () -> do { Printf.eprintf "."; flush stderr; }
      else do {
        let bar_cnt = ref 0 in
        let run () = do { ProgrBar.run bar_cnt.val ngwo; incr bar_cnt } in
        ProgrBar.empty.val := 'o';
        ProgrBar.full.val := '*';
        ProgrBar.start ();
        run
      }
    in
    List.iter (insert_comp_families2 gen run) gwo_list;
    if ngwo < 10 || not Mutil.verbose.val then ()
    else if ngwo < 60 then do { Printf.eprintf "\n"; flush stderr }
    else ProgrBar.finish ();

    List.iter close_out_field person_fields;
    List.iter close_out_field family_fields;
    Iochan.close (fst person_notes);
    close_out (snd person_notes);
    Iochan.close (fst person_related);
    Iochan.close (snd person_related);
    Iochan.close (fst person_rparents);
    close_out (snd person_rparents);
    Iochan.close (fst person_unions);
    close_out (snd person_unions);
    Iochan.close (fst person_parents);
    close_out (snd person_parents);
    Gc.compact ();

    let person_of_key_d = 
      List.fold_left Filename.concat tmp_dir ["base_d"; "person_of_key"]
    in
    try Mutil.mkdir_p person_of_key_d with _ -> ();

    output_hashtbl person_of_key_d "iper_of_key.ht"
      (gen.g_index_of_key : Hashtbl.t key iper);
    Hashtbl.clear gen.g_index_of_key;

    output_hashtbl person_of_key_d "istr_of_string.ht"
      (gen.g_strings : Hashtbl.t string Adef.istr);
    Hashtbl.clear gen.g_strings;
    Gc.compact ();

    compress_fields tmp_dir;
    reorder_fields tmp_dir;
    make_string_of_crush_index tmp_dir;
    make_person_of_string_index tmp_dir;
    make_name_index tmp_dir gen.g_pcnt;
    make_index gen "first_name";
    make_index gen "surname";

    output_particles_file tmp_dir gen.g_particles;

    if Mutil.verbose.val then do {
      Printf.eprintf "pcnt %d\n" gen.g_pcnt;
      Printf.eprintf "fcnt %d\n" gen.g_fcnt;
      Printf.eprintf "scnt %d\n" gen.g_scnt;
      flush stderr;
    }
    else ();

    Mutil.mkdir_p bdir;
    let dir = Filename.concat bdir "base_d" in
    let old_dir = Filename.concat bdir "base_d~" in
    Mutil.remove_dir old_dir;
    try Sys.rename dir old_dir with [ Sys_error _ -> () ];
    Sys.rename (Filename.concat tmp_dir "base_d") dir;
    Mutil.remove_dir old_dir;
    try Unix.rmdir tmp_dir with [ Unix.Unix_error _ _ _ -> () ];
    try Unix.rmdir "gw_tmp" with [ Unix.Unix_error _ _ _ -> () ];
  }
;

value output_command_line bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let oc = open_out (Filename.concat bdir "command.txt") in
  do {
    fprintf oc "%s" Sys.argv.(0);
    for i = 1 to Array.length Sys.argv - 1 do {
      fprintf oc " %s" Sys.argv.(i)
    };
    fprintf oc "\n";
    close_out oc;
  }
;

value separate = ref False;
value shift = ref 0;
value files = ref [];

value speclist =
  [("-c", Arg.Set just_comp, "Only compiling");
   ("-o", Arg.String (fun s -> out_file.val := s),
    "<file> Output database (default: a.gwb)");
   ("-f", Arg.Set force, " Remove database if already existing");
   ("-stats", Arg.Set pr_stats, "Print statistics");
   ("-nc", Arg.Clear do_check, "No consistency check");
   ("-cg", Arg.Set do_consang, "Compute consanguinity");
   ("-sep", Arg.Set separate, " Separate all persons in next file");
   ("-sh", Arg.Int (fun x -> shift.val := x),
    "<int> Shift all persons numbers in next files");
   ("-ds", Arg.String (fun s -> default_source.val := s), "\
     <str> Set the source field for persons and families without source data");
   ("-part", Arg.String (fun s -> part_file.val := s), "\
     <file> Particles file (default = predefined particles)");
   ("-mem", Arg.Unit (fun () -> ()), " (obsolete option)");
   ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
   ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.");
   ("-q", Arg.Clear Mutil.verbose, " no verbose")]
;

value anonfun x =
  let sep = separate.val in
  do {
    if Filename.check_suffix x ".gw" then ()
    else if Filename.check_suffix x ".gwo" then ()
    else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
    separate.val := False;
    files.val := [(x, sep, shift.val) :: files.val]
  }
;

value errmsg =
  "\
Usage: gwc [options] [files]
where [files] are a list of files:
  source files end with .gw
  object files end with .gwo
and [options] are:"
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    Secure.set_base_dir (Filename.dirname out_file.val);
    let gwo = ref [] in
    List.iter
      (fun (x, separate, shift) ->
         if Filename.check_suffix x ".gw" then do {
           try Gwcomp.comp_families x with e ->
             do {
               printf "File \"%s\", line %d:\n" x line_cnt.val; raise e
             };
           gwo.val := [(x ^ "o", separate, shift) :: gwo.val];
         }
         else if Filename.check_suffix x ".gwo" then
           gwo.val := [(x, separate, shift) :: gwo.val]
         else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
      (List.rev files.val);
    if not just_comp.val then do {
      let bdir =
        if Filename.check_suffix out_file.val ".gwb" then out_file.val
        else out_file.val ^ ".gwb"
      in
      if not force.val && Sys.file_exists bdir then do {
        printf "\
The database \"%s\" already exists. Use option -f to overwrite it.
" out_file.val;
        flush stdout;
        exit 2
      }
      else ();
      lock (Mutil.lock_file out_file.val) with
      [ Accept -> do {
          link (List.rev gwo.val) out_file.val;
          output_command_line out_file.val;
        }
      | Refuse ->
          do {
            printf "Base is locked: cannot write it\n";
            flush stdout;
            exit 2
          } ];
    }
    else ();
  }
;

value print_exc =
  fun
  [ Failure txt ->
      do { printf "Failed: %s\n" txt; flush stdout; exit 2 }
  | exc -> Printexc.catch raise exc ]
;

try main () with exc -> print_exc exc;
