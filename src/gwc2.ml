(* camlp4r ./pa_lock.cmo *)
(* $Id: gwc2.ml,v 5.5 2006-10-18 21:59:28 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

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
  (out_channel * out_channel * ref int * ref int * 'a -> Obj.t)
;

type key =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

type gen =
  { g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;

    g_strings : Hashtbl.t string Adef.istr;
    g_index_of_key : Hashtbl.t key iper;
    g_person_fields : list (file_field (gen_person iper string));
    g_family_fields : list (file_field family);
    g_person_parents : (Iochan.t * out_channel);
    g_person_unions : (Iochan.t * out_channel) }
;

value int_size = 4;

value default_source = ref "";
value do_check = ref True;

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

value intext_magic_number = [| 0x84; 0x95; 0xA6; 0xBE |];
value phony_min_size = 8;

type hashtbl_t 'a 'b =
  { size: mutable int;
    data: mutable array (bucketlist 'a 'b) }
and bucketlist 'a 'b =
  [ Empty
  | Cons of 'a and 'b and bucketlist 'a 'b ]
;

value output_hashtbl oc_hta oc_ht ht = do {
  let ht : hashtbl_t 'a 'b = Obj.magic (ht : Hashtbl.t 'a 'b) in
  output_binary_int oc_hta (Array.length ht.data);
  for i = 0 to 3 do { output_byte oc_ht intext_magic_number.(i); };
  let pos = pos_out oc_ht in
  output_binary_int oc_ht 0;
  output_binary_int oc_ht 0;
  output_binary_int oc_ht 0;
  output_binary_int oc_ht 0;
  Iovalue.size_32.val := 0;
  Iovalue.size_64.val := 0;

  let pos_start = pos_out oc_ht in
  Iovalue.output_block_header oc_ht 0 2;
  Iovalue.output oc_ht ht.size;
  Iovalue.output_block_header oc_ht 0 (Array.length ht.data);
  for i = 0 to Array.length ht.data - 1 do {
    output_binary_int oc_hta (pos_out oc_ht);
    Iovalue.output oc_ht ht.data.(i);
  };
  let pos_end = pos_out oc_ht in

  seek_out oc_ht pos;
  output_binary_int oc_ht (pos_end - pos_start);
  output_binary_int oc_ht 0;
  output_binary_int oc_ht Iovalue.size_32.val;
  output_binary_int oc_ht Iovalue.size_64.val;
};

value open_out_field tmp_dir (name, valu) = do {
  let d = Filename.concat tmp_dir name in
  try Mutil.mkdir_p d with _ -> ();
  let oc_dat = open_out_bin (Filename.concat d "data") in
  let oc_acc = open_out_bin (Filename.concat d "access") in
  for i = 0 to 3 do { output_byte oc_dat intext_magic_number.(i); };
  output_binary_int oc_dat 0;
  output_binary_int oc_dat 0;
  output_binary_int oc_dat 0;
  output_binary_int oc_dat 0;
  Iovalue.size_32.val := 0;
  Iovalue.size_64.val := 0;
  Iovalue.output_block_header oc_dat 0 phony_min_size;
  (oc_dat, oc_acc, ref Iovalue.size_32.val, ref Iovalue.size_64.val, valu)
};

value output_field so (oc_dat, oc_acc, sz32, sz64, valu) = do {
  output_binary_int oc_acc (pos_out oc_dat);
  Iovalue.size_32.val := sz32.val;
  Iovalue.size_64.val := sz64.val;
  Iovalue.output oc_dat (valu so);
  sz32.val := Iovalue.size_32.val;
  sz64.val := Iovalue.size_64.val;
};

value close_out_field nb_items (oc_dat, oc_acc, sz32, sz64, evalu) = do {
  close_out oc_acc;
  let pos_start = 20 in
  let pos_end = pos_out oc_dat in
  seek_out oc_dat 4;
  output_binary_int oc_dat (pos_end - pos_start);
  output_binary_int oc_dat 0;
  output_binary_int oc_dat (sz32.val - phony_min_size + nb_items);
  output_binary_int oc_dat (sz64.val - phony_min_size + nb_items);
  Iovalue.output_block_header oc_dat 0 nb_items;
  close_out oc_dat;
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
  ("rparents", fun so -> Obj.repr so.rparents);
  ("related", fun so -> Obj.repr so.related);
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
  ("notes", fun so -> Obj.repr so.notes);
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

value str_pos oc_str ht s =
  try Hashtbl.find ht s with
  [ Not_found -> do {
      let pos = pos_out oc_str in
      Iovalue.output oc_str s;
      Hashtbl.add ht s pos;
      pos
    } ]
;

value optim_type_string field_d ic oc oc_str ht = do {
  let istr_empty = str_pos oc_str ht "" in
  let istr_quest = str_pos oc_str ht "?" in
  (* istr_empty and istr_quest must be in consistency with
     Gwdb.is_empty_string and Gwdb.is_quest_string *)
  assert (istr_empty = 0);
  assert (istr_quest = 1);
  try
    while True do {
      let s : string = Iovalue.input ic in
      let pos = str_pos oc_str ht s in
      output_binary_int oc pos
    }
  with
  [ End_of_file -> () ]
};

value optim_type_list_string field_d ic oc oc_str ht = do {
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  try
    while True do {
      let sl : list string = Iovalue.input ic in
      match sl with
      [ [_ :: _] -> do {
          output_binary_int oc (pos_out oc_ext);
          let sl = List.map (str_pos oc_str ht) sl in
          Iovalue.output oc_ext (sl : list int)
        }
      | [] ->
          output_binary_int oc (-1) ]
    }
  with
  [ End_of_file -> () ];
  close_out oc_ext;
};

value optim_type_list_title field_d ic oc oc_str ht = do {
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  try
    while True do {
      let tl : list (gen_title string) = Iovalue.input ic in
      match tl with
      [ [_ :: _] -> do {
          output_binary_int oc (pos_out oc_ext);
          let tl = List.map (map_title_strings (str_pos oc_str ht)) tl in
          Iovalue.output oc_ext (tl : list (gen_title int))
        }
      | [] ->
          output_binary_int oc (-1) ]
    }
  with
  [ End_of_file -> () ];
  close_out oc_ext;
};

value optim_type_list_relation field_d ic oc oc_str ht = do {
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  try
    while True do {
      let tl : list (gen_relation 'a string) = Iovalue.input ic in
      match tl with
      [ [_ :: _] -> do {
          output_binary_int oc (pos_out oc_ext);
          let tl =
            List.map (map_relation_ps (fun x -> x) (str_pos oc_str ht)) tl
          in
          Iovalue.output oc_ext (tl : list (gen_relation 'a int))
        }
      | [] ->
          output_binary_int oc (-1) ]
    }
  with
  [ End_of_file -> () ];
  close_out oc_ext;
};

value optim_person_fields tmp_dir =
  List.iter
    (fun (field, optim_type) -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; "person"; field]
       in
       let ic = open_in_bin (Filename.concat field_d "data") in
       eprintf "%s..." field;
       flush stderr;
       let oc_acc2 = open_out_bin (Filename.concat field_d "access2") in
       let oc_dat2 = open_out_bin (Filename.concat field_d "data2") in
       let ht : Hashtbl.t string int = Hashtbl.create 1 in
       seek_in ic 25;

       optim_type field_d ic oc_acc2 oc_dat2 ht;

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
       Printf.eprintf "\n"; flush stderr
     })
    [("baptism_place", optim_type_string); ("birth_place", optim_type_string);
     ("first_name", optim_type_string); ("image", optim_type_string);
     ("occupation", optim_type_string); ("public_name", optim_type_string);
     ("surname", optim_type_string);

     ("aliases", optim_type_list_string);
     ("first_names_aliases", optim_type_list_string);
     ("qualifiers", optim_type_list_string);
     ("surnames_aliases", optim_type_list_string);
     ("titles", optim_type_list_title);
     ("rparents", optim_type_list_relation)]
;

value string_of_crush_index tmp_dir =
  List.iter
    (fun field -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; "person"; field]
       in
       let ic_dat = open_in_bin (Filename.concat field_d "data") in
       eprintf "string_of_crush %s..." field;
       flush stderr;
       let ht = Hashtbl.create 1 in
       loop 0 where rec loop pos =
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
       let oc_ht =
         open_out_bin (Filename.concat field_d "string_of_crush.ht")
       in
       output_value oc_ht ht;
       close_out oc_ht;
       eprintf "\n";
       flush stderr;
    })
   ["first_name"; "surname"]
;

value person_of_string_index tmp_dir =
  List.iter
    (fun field -> do {
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; "person"; field]
       in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       eprintf "person_of_string %s..." field;
       flush stderr;
       let ht = Hashtbl.create 1 in
       loop 0 where rec loop i =
         match
           try Some (input_binary_int ic_acc) with [ End_of_file -> None ]
         with
         [ Some pos -> do { Hashtbl.add ht pos i; loop (i + 1) }
         | None -> () ];
       close_in ic_acc;
       let oc_ht =
         open_out_bin (Filename.concat field_d "person_of_string.ht")
       in
       output_value oc_ht ht;
       close_out oc_ht;
       eprintf "\n";
       flush stderr;
     })
    ["first_name"; "surname"]
;

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
    key_hashtbl_add gen.g_index_of_key (fn, sn, so.occ)
      (Adef.iper_of_int gen.g_pcnt);
    List.iter (output_field so) gen.g_person_fields;
    Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_parents) (-1);
    Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_unions) (-1);
    gen.g_pcnt := gen.g_pcnt + 1;
  }
  else ();
};

value insert_undefined2 gen key fn sn = do {
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
     occ = key.pk_occ}
  in
  List.iter (output_field so) gen.g_person_fields;
  Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_parents) (-1);
  Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
  Iochan.output_binary_int (fst gen.g_person_unions) (-1);
  gen.g_pcnt := gen.g_pcnt + 1;
  Adef.iper_of_int (gen.g_pcnt - 1)
};

value get_person2 gen so =
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
    List.iter (output_field so) gen.g_person_fields;
    Iochan.seek (fst gen.g_person_parents) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_parents) (-1);
    Iochan.seek (fst gen.g_person_unions) (int_size * gen.g_pcnt);
    Iochan.output_binary_int (fst gen.g_person_unions) (-1);
    gen.g_pcnt := gen.g_pcnt + 1;
    Adef.iper_of_int (gen.g_pcnt - 1)
  }
;

value get_undefined2 gen key =
  let fn = unique_key_string gen key.pk_first_name in
  let sn = unique_key_string gen key.pk_surname in
  try key_hashtbl_find gen.g_index_of_key (fn, sn, key.pk_occ) with
  [ Not_found -> insert_undefined2 gen key fn sn ]
;

value insert_somebody1 gen sex =
  fun
  [ Undefined key -> ()
  | Defined so ->
      let so = {(so) with sex = sex} in
      insert_person1 gen so ]
;

value get_somebody2 gen =
  fun
  [ Undefined key -> get_undefined2 gen key
  | Defined so -> get_person2 gen so ]
;

value insert_family1 gen co fath_sex moth_sex witl fo deo = do {
  let _ifath = insert_somebody1 gen fath_sex (Adef.father co) in
  let _imoth = insert_somebody1 gen moth_sex (Adef.mother co) in
  Array.iter (fun key -> insert_person1 gen key) deo.children;
};

value insert_family2 gen co fath_sex moth_sex witl fo deo = do {
  let ifath = get_somebody2 gen (Adef.father co) in
  let imoth = get_somebody2 gen (Adef.mother co) in
  let children = Array.map (fun key -> get_person2 gen key) deo.children in
  let fam =
    {fam ={(fo) with witnesses = [| |]};
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

value insert_gwo_1 fname gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family1 gen cpl fs ms witl fam des
  | Notes key str -> ()
  | Relations sb sex rl -> ()
  | Bnotes nfname str -> ()
  | Wnotes wizid str -> () ]
;

value insert_gwo_2 fname gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family2 gen cpl fs ms witl fam des
  | Notes key str -> ()
  | Relations sb sex rl -> ()
  | Bnotes nfname str -> ()
  | Wnotes wizid str -> () ]
;

value insert_comp_families1 gen run (x, separate, shift) =
  do {
    run ();
    let ic = open_in_bin x in
    check_magic x ic;
    let src : string = input_value ic in
    try
      while True do {
        let fam : syntax_o = input_value ic in
        insert_gwo_1 src gen fam
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
    let src : string = input_value ic in
    try
      while True do {
        let fam : syntax_o = input_value ic in
        insert_gwo_2 src gen fam
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

value link gwo_list bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let tmp_dir = Filename.concat "gw_tmp" bdir in
  do {
    try Mutil.mkdir_p tmp_dir with _ -> ();
    let person_d =
      List.fold_left Filename.concat tmp_dir ["base_d"; "person"]
    in
    try Mutil.mkdir_p person_d with _ -> ();
    let person_fields =
      List.map (open_out_field person_d) person_fields_arr
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
    let family_fields = do {
      let family_d =
        List.fold_left Filename.concat tmp_dir ["base_d"; "family"]
      in
      try Mutil.mkdir_p family_d with _ -> ();
      List.map (open_out_field family_d) family_fields_arr
    }
    in
    let gen =
      {g_pcnt = 0; g_fcnt = 0; g_scnt = 0;
       g_strings = Hashtbl.create 1;
       g_index_of_key = Hashtbl.create 1;
       g_person_fields = person_fields;
       g_family_fields = family_fields;
       g_person_parents = person_parents;
       g_person_unions = person_unions}
    in
    let ngwo = List.length gwo_list in
    let run =
      if ngwo < 10 then fun () -> ()
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

    if ngwo < 10 then ()
    else if ngwo < 60 then do { Printf.eprintf "\n"; flush stderr }
    else ProgrBar.finish ();

Gc.compact ();

    let run =
      if ngwo < 10 then fun () -> ()
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
    if ngwo < 10 then ()
    else if ngwo < 60 then do { Printf.eprintf "\n"; flush stderr }
    else ProgrBar.finish ();

    List.iter (close_out_field gen.g_pcnt) person_fields;
    List.iter (close_out_field gen.g_fcnt) family_fields;
    Iochan.close (fst person_unions);
    close_out (snd person_unions);
    Gc.compact ();

    let person_of_key_d = 
      List.fold_left Filename.concat tmp_dir ["base_d"; "person_of_key"]
    in
    try Mutil.mkdir_p person_of_key_d with _ -> ();

    let oc_hta =
      open_out_bin (Filename.concat person_of_key_d "iper_of_key.hta")
    in
    let oc_ht =
      open_out_bin (Filename.concat person_of_key_d "iper_of_key.ht")
    in
    output_hashtbl oc_hta oc_ht (gen.g_index_of_key : Hashtbl.t key iper);
    close_out oc_hta;
    close_out oc_ht;
    Hashtbl.clear gen.g_index_of_key;

    let oc_hta =
      open_out_bin (Filename.concat person_of_key_d "istr_of_string.hta")
    in
    let oc_ht =
      open_out_bin (Filename.concat person_of_key_d "istr_of_string.ht")
    in
    output_hashtbl oc_hta oc_ht (gen.g_strings : Hashtbl.t string Adef.istr);
    close_out oc_hta;
    close_out oc_ht;
    Hashtbl.clear gen.g_strings;
    Gc.compact ();

    optim_person_fields tmp_dir;
    string_of_crush_index tmp_dir;
    person_of_string_index tmp_dir;

    Printf.eprintf "pcnt %d\n" gen.g_pcnt;
    Printf.eprintf "fcnt %d\n" gen.g_fcnt;
    Printf.eprintf "scnt %d\n" gen.g_scnt;
    flush stderr;

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

value write_file_contents fname text =
  let oc = open_out fname in
  do {
    output_string oc text;
    close_out oc;
  }
;

value output_wizard_notes bname wiznotes =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let wizdir = Filename.concat bdir "wiznotes" in
  do {
    Mutil.remove_dir wizdir;
    if wiznotes = [] then ()
    else
      do {
        try Unix.mkdir wizdir 0o755 with _ -> ();
        List.iter
          (fun (wizid, text) ->
             let fname = Filename.concat wizdir wizid ^ ".txt" in
             write_file_contents fname text)
          wiznotes;
      }
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

value input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur ";
     "AF "; "D'"; "DAL "; "DE "; "DI "; "DU "; "OF "; "VAN ";
     "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file
;

value output_particles_file bname particles =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let oc = open_out (Filename.concat bdir "particles.txt") in
  do {
    List.iter (fun s -> fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
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
   ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.")]
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
          let particles = input_particles part_file.val in
          output_particles_file out_file.val particles;
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
