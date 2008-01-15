(* camlp5r *)
(* $Id: db2link.ml,v 5.4 2008-01-15 17:12:01 ddr Exp $ *)
(* Copyright (c) 2006-2008 INRIA *)

open Def;
open Futil;
open Gwcomp;
open Printf;

value default_source = ref "";
value do_check = ref True;
value do_consang = ref False;
value pr_stats = ref False;

type file_info =
  { f_curr_src_file : mutable string;
    f_curr_gwo_file : mutable string;
    f_separate : mutable bool;
    f_has_separates : mutable bool;
    f_sep_file_inx : mutable int }
;

value max_warnings = 10;
value max_errors = 10;

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

type gen =
  { g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;
    g_file_info : file_info;
    g_error : mutable bool;
    g_error_cnt : mutable int;
    g_warning_cnt : mutable int;
    g_first_av_occ : Hashtbl.t (Adef.istr * Adef.istr) int;

    g_tmp_dir : string;
    g_particles : list string;

    g_strings : Hashtbl.t string Adef.istr;
    g_index_of_key : Hashtbl.t Db2.key2 iper;
    g_occ_of_key : mutable array (Hashtbl.t Db2.key2 int);
    g_person_fields : list (file_field (gen_person iper string));
    g_family_fields : list (file_field family);
    g_person_parents : (Iochan.t * out_channel);
    g_person_unions : (Iochan.t * out_channel);
    g_person_rparents : (Iochan.t * out_channel);
    g_person_related : (Iochan.t * Iochan.t);
    g_person_notes : (Iochan.t * out_channel)}
;

value open_out_field tmp_dir (name, valu) = do {
  let d = Filename.concat tmp_dir name in
  try Mutil.mkdir_p d with _ -> ();

  let oc_dat = open_out_bin (Filename.concat d "data") in
  let oc_acc = open_out_bin (Filename.concat d "access") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 Db2out.phony_min_size;
  assert (pos_out oc_dat = Db2.first_item_pos);
  {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = start_pos;
   sz32 = Iovalue.size_32.val; sz64 = Iovalue.size_64.val;
   item_cnt = 0; valu = valu}
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

value input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur ";
     "AF "; "D'"; "DAL "; "DE "; "DI "; "DU "; "OF "; "VAN ";
     "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file
;

value particules_file = ref "";

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

value key_hashtbl_find ht k = Hashtbl.find ht (Db2.key2_of_key k);
value key_hashtbl_add ht k v = Hashtbl.add ht (Db2.key2_of_key k) v;

value occ_of_key_ht gen = do {
  let i = gen.g_file_info.f_sep_file_inx in
  let len = Array.length gen.g_occ_of_key in
  if i >= len then
    let new_len = max (i + 1) (2 * len + 1) in
    gen.g_occ_of_key :=
      Array.append gen.g_occ_of_key
        (Array.init (new_len - len) (fun _ -> Hashtbl.create 1))
  else ();
  gen.g_occ_of_key.(i)
};

value find_first_available_occ gen so fn sn =
  let occ =
    try Hashtbl.find gen.g_first_av_occ (fn, sn) with
    [ Not_found -> 0 ]
  in
  loop occ where rec loop occ =
    let k1 = (fn, sn, occ) in
    match
      try Some (key_hashtbl_find gen.g_index_of_key k1) with
      [ Not_found -> None ]
    with
    [ Some _ -> loop (occ + 1)
    | None -> do {
        gen.g_warning_cnt := gen.g_warning_cnt - 1;
        if gen.g_warning_cnt > 0 then do {
          eprintf "Warning: %s: %s.%d %s renumbered %d\n"
            gen.g_file_info.f_curr_gwo_file so.first_name so.occ
            so.surname occ;
          flush stderr;
        }
        else ();
        key_hashtbl_add (occ_of_key_ht gen) (fn, sn, so.occ) occ;
        Hashtbl.replace gen.g_first_av_occ (fn, sn) occ;
        occ;
      } ]
;

value output_item v ff = do {
  Iovalue.size_32.val := ff.sz32;
  Iovalue.size_64.val := ff.sz64;
  Iovalue.output ff.oc_dat v;
  ff.sz32 := Iovalue.size_32.val;
  ff.sz64 := Iovalue.size_64.val;
  ff.item_cnt := ff.item_cnt + 1;
};

value output_field so ff = do {
  output_binary_int ff.oc_acc (pos_out ff.oc_dat);
  output_item (ff.valu so) ff;
};

value int_size = 4;

value insert_person1 gen so = do {
  if so.first_name <> "?" && so.surname <> "?" then do {
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let k = (fn, sn, so.occ) in
    try do {
      if gen.g_file_info.f_separate then
        ignore (key_hashtbl_find (occ_of_key_ht gen) k : int)
      else
        ignore (key_hashtbl_find gen.g_index_of_key k : iper);
      gen.g_error_cnt := gen.g_error_cnt - 1;
      if gen.g_error_cnt > 0 then do {
        eprintf "File \"%s\"\n" gen.g_file_info.f_curr_gwo_file;
        eprintf "Error: already defined %s.%d %s\n" so.first_name so.occ
          so.surname
      }
      else ();
      flush stderr;
      gen.g_error := True;
    }
    with
    [ Not_found -> do {
        let (k, so) =
          if gen.g_file_info.f_separate then
            let occ = find_first_available_occ gen so fn sn in
            ((fn, sn, occ), {(so) with occ = occ})
          else (k, so)
        in
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

value insert_somebody1 gen sex =
  fun
  [ Undefined key -> ()
  | Defined so ->
      let so = {(so) with sex = sex} in
      insert_person1 gen so ]
;

value insert_family1 gen co fath_sex moth_sex witl fo deo = do {
  let _ifath = insert_somebody1 gen fath_sex (Adef.father co) in
  let _imoth = insert_somebody1 gen moth_sex (Adef.mother co) in
  Array.iter (fun key -> insert_person1 gen key) deo.children;
  List.iter (fun (so, sex) -> insert_somebody1 gen sex so) witl;
};

value iter_option f =
  fun
  [ Some x -> f x
  | None -> () ]
;

value insert_relation1 gen r = do {
  iter_option (insert_somebody1 gen Male) r.r_fath;
  iter_option (insert_somebody1 gen Female) r.r_moth;
};

value insert_rparents1 gen sb sex rl = do {
  insert_somebody1 gen sex sb;
  List.iter (insert_relation1 gen) rl
};

value insert_bnotes1 gen notesname str = do {
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
  let oc = open_out fname in
  output_string oc str;
  close_out oc;
  if notesname = "" then do {
    let fname =
      List.fold_left Filename.concat gen.g_tmp_dir
        ["base_d"; "notes_of.txt"]
    in
    let oc = open_out fname in
    fprintf oc "%s\n" gen.g_file_info.f_curr_src_file;
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

value insert_gwo_1 gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family1 gen cpl fs ms witl fam des
  | Notes key str -> ()
  | Relations sb sex rl -> insert_rparents1 gen sb sex rl
  | Bnotes nfname str -> insert_bnotes1 gen nfname str
  | Wnotes wizid str -> insert_wiznotes1 gen wizid str ]
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

value insert_undefined2 gen key fn sn sex = do {
  if key.pk_first_name <> "?" && key.pk_surname <> "?" then
    key_hashtbl_add gen.g_index_of_key (fn, sn, key.pk_occ)
      (Adef.iper_of_int gen.g_pcnt)
  else ();
  if gen.g_file_info.f_has_separates then do {
    gen.g_error_cnt := gen.g_error_cnt - 1;
    if gen.g_error_cnt > 0 then do {
      gen.g_error_cnt := -1;
      eprintf
        "Error: option -sep does not work when there are undefined persons\n";
      flush stderr;
    }
    else ();
    gen.g_error := True;
  }
  else if do_check.val then do {
    gen.g_warning_cnt := gen.g_warning_cnt - 1;
    if gen.g_warning_cnt > 0 then
      eprintf "Warning: adding undefined %s.%d %s\n"
        (Name.lower key.pk_first_name) key.pk_occ (Name.lower key.pk_surname)
    else ();
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

value get_undefined2 gen key sex =
  let fn = unique_key_string gen key.pk_first_name in
  let sn = unique_key_string gen key.pk_surname in
  let occ =
    if gen.g_file_info.f_separate then
      try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, key.pk_occ) with
      [ Not_found -> key.pk_occ ]
    else key.pk_occ
  in
  try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
  [ Not_found -> insert_undefined2 gen key fn sn sex ]
;

value get_person2 gen so sex =
  if so.first_name <> "?" && so.surname <> "?" then do {
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let occ =
      if gen.g_file_info.f_separate then
        try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, so.occ) with
        [ Not_found -> so.occ ]
      else so.occ
    in
    try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
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

value get_somebody2 gen sex =
  fun
  [ Undefined key -> get_undefined2 gen key sex
  | Defined so -> get_person2 gen so sex ]
;

value insert_related gen irp ip = do {
  let (ioc_acc, ioc_dat) = gen.g_person_related in
  Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
  let pos1 = Iochan.input_binary_int ioc_acc in
  let pos2 = Iochan.seek_end ioc_dat in
  Iochan.output_value_no_header ioc_dat (Adef.int_of_iper ip);
  Iochan.output_value_no_header ioc_dat pos1;
  Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
  Iochan.output_binary_int ioc_acc pos2;
};

value insert_family2 gen co fath_sex moth_sex witl fo deo = do {
  let ifath = get_somebody2 gen fath_sex (Adef.father co) in
  let imoth = get_somebody2 gen moth_sex (Adef.mother co) in
  let children =
    Array.map (fun key -> get_person2 gen key Neuter) deo.children
  in
  let witn =
    List.map
      (fun (so, sex) -> do {
         let ip = get_somebody2 gen sex so in
         insert_related gen ip ifath;
         ip
       })
      witl
  in
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

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value insert_relation_parent2 gen ip sex k = do {
  let irp = get_somebody2 gen sex k in
  insert_related gen irp ip;
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

value insert_gwo_2 gen =
  fun
  [ Family cpl fs ms witl fam des -> insert_family2 gen cpl fs ms witl fam des
  | Notes key str -> insert_notes2 gen key str
  | Relations sb sex rl -> insert_rparents2 gen sb sex rl
  | Bnotes nfname str -> ()
  | Wnotes wizid str -> () ]
;

value close_out_field pad ff = do {
  close_out ff.oc_acc;
  for i = ff.item_cnt + 1 to Db2out.phony_min_size do {
    output_item (ff.valu pad) ff;
  };
  Iovalue.size_32.val := ff.sz32 - Db2out.phony_min_size + ff.item_cnt;
  Iovalue.size_64.val := ff.sz64 - Db2out.phony_min_size + ff.item_cnt;
  ignore (Iovalue.patch_output_value_header ff.oc_dat ff.start_pos : int);
  Iovalue.output_block_header ff.oc_dat 0 ff.item_cnt;
  close_out ff.oc_dat;
};

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

value pad_per = no_person "" (Adef.iper_of_int 0);

value no_family empty_string ifam =
  {fam =
     {marriage = Adef.codate_None; marriage_place = empty_string;
      marriage_src = empty_string; witnesses = [| |]; relation = Married;
      divorce = NotDivorced; comment = empty_string;
      origin_file = empty_string; fsources = empty_string; fam_index = ifam};
   cpl = Adef.couple (Adef.iper_of_int 0) (Adef.iper_of_int 0);
   des = {children = [| |]}}
;

value pad_fam = no_family "" (Adef.ifam_of_int 0);

value compress_type_string field_d ic oc oc_str =
  Db2out.output_value_array oc_str "" True
    (fun output_item -> do {
       let istr_empty = output_item "" in
       let istr_quest = output_item "?" in
       assert (istr_empty = Db2.empty_string_pos);
       assert (istr_quest = Db2.quest_string_pos);
       try
         while True do {
           let s : string = Iovalue.input ic in
           let pos = output_item s in
           output_binary_int oc pos
         }
       with
       [ End_of_file -> () ]
     })
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
          let sl =
            List.map
              (Db2out.output_item_return_pos oc_str ht items_cnt True) sl
          in
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
            List.map
              (map_title_strings
                 (Db2out.output_item_return_pos oc_str ht items_cnt True))
              tl
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
         Iovalue.output_block_header oc_dat 0 Db2out.phony_min_size;
         assert (pos_out oc_dat = Db2.first_item_pos);
         {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = start_pos;
          sz32 = Iovalue.size_32.val; sz64 = Iovalue.size_64.val;
          item_cnt = 0; valu = Obj.repr}
       }
       in
       reorder_type field_d ic_acc ic_dat ff;

       close_out_field [| |] ff;
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

value output_command_line bdir = do {
  let oc = open_out (Filename.concat bdir "command.txt") in
  fprintf oc "%s" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do {
    fprintf oc " %s" Sys.argv.(i)
  };
  fprintf oc "\n";
  close_out oc;
};

value output_particles_file tmp_dir particles = do {
  let fname =
    List.fold_left Filename.concat tmp_dir ["base_d"; "particles.txt"]
  in
  let oc = open_out fname in
  List.iter (fun s -> fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
  close_out oc;
};

value set_error base x = do {
  printf "\nError: ";
  Check.print_base_error stdout base x;
};

value set_warning base =
  fun
  [ UndefinedSex _ -> ()
  | x -> do {
      printf "\nWarning: ";
      Check.print_base_warning stdout base x;
    } ]
;

value fold_option fsome vnone =
  fun
  [ Some v -> fsome v
  | None -> vnone ]
;

value changed_p (ip, p, o_sex, o_rpar) =
  let p = Gwdb.dsk_person_of_person p in
  let _p =
    {(p) with
     sex = fold_option (fun s -> s) p.sex o_sex;
     rparents =
       fold_option
         (List.map
            (Futil.map_relation_ps (fun p -> p)
               (fun s -> Adef.istr_of_int 0)))
         p.rparents o_rpar}
  in
  let i = Adef.int_of_iper ip in
  do { Printf.eprintf "person %d not changed\n" i; flush stderr }
;

value link next_family_fun bdir = do {
  let tmp_dir = Filename.concat "gw_tmp" bdir in
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
  let fi =
    {f_curr_src_file = ""; f_curr_gwo_file = ""; f_separate = False;
     f_has_separates = False; f_sep_file_inx = 0}
  in
  let gen =
    {g_pcnt = 0; g_fcnt = 0; g_scnt = 0; g_file_info = fi;
     g_error = False; g_error_cnt = max_errors + 1;
     g_warning_cnt = max_warnings + 1;
     g_first_av_occ = Hashtbl.create 1;
     g_tmp_dir = tmp_dir;
     g_particles = input_particles particules_file.val;
     g_strings = Hashtbl.create 1;
     g_index_of_key = Hashtbl.create 1;
     g_occ_of_key = [| |];
     g_person_fields = person_fields;
     g_family_fields = family_fields;
     g_person_parents = person_parents;
     g_person_unions = person_unions;
     g_person_rparents = person_rparents;
     g_person_related = person_related;
     g_person_notes = person_notes}
  in
  if Mutil.verbose.val then do {
    eprintf "pass 1: creating persons...\n";
    flush stderr
  }
  else ();
  let next_family = next_family_fun fi in
  loop () where rec loop () =
    match next_family () with
    [ Some fam -> do { insert_gwo_1 gen fam; loop () }
    | None -> () ];

  Gc.compact ();

  if Mutil.verbose.val then do {
    eprintf "pass 2: creating families...\n";
    flush stderr
  }
  else ();
  let next_family = next_family_fun fi in
  loop () where rec loop () =
    match next_family () with
    [ Some fam -> do { insert_gwo_2 gen fam; loop () }
    | None -> () ];

  if gen.g_warning_cnt < 0 then do {
    eprintf "Warning: %d more warnings...\n" (-gen.g_warning_cnt);
    flush stderr;
  }
  else ();
  if gen.g_error_cnt < 0 then do {
    eprintf "Error: %d more errors...\n" (-gen.g_error_cnt);
    flush stderr;
  }
  else ();

  List.iter (close_out_field pad_per) person_fields;
  List.iter (close_out_field pad_fam) family_fields;
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

  Db2out.output_hashtbl person_of_key_d "iper_of_key.ht"
    (gen.g_index_of_key : Hashtbl.t Db2.key2 iper);
  Hashtbl.clear gen.g_index_of_key;

  Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
    (gen.g_strings : Hashtbl.t string Adef.istr);
  Hashtbl.clear gen.g_strings;
  Gc.compact ();

  compress_fields tmp_dir;
  reorder_fields tmp_dir;

  Db2out.make_indexes (Filename.concat tmp_dir "base_d") gen.g_pcnt
    gen.g_particles;

  output_particles_file tmp_dir gen.g_particles;

  if Mutil.verbose.val then do {
    Printf.eprintf "pcnt %d\n" gen.g_pcnt;
    Printf.eprintf "fcnt %d\n" gen.g_fcnt;
    Printf.eprintf "scnt %d\n" gen.g_scnt;
    flush stderr;
  }
  else ();

  if not gen.g_error then do {
    Mutil.mkdir_p bdir;
    let dir = Filename.concat bdir "base_d" in
    let old_dir = Filename.concat bdir "base_d~" in
    Mutil.remove_dir old_dir;
    try Sys.rename dir old_dir with [ Sys_error _ -> () ];
    Sys.rename (Filename.concat tmp_dir "base_d") dir;
    Mutil.remove_dir old_dir;
    try Unix.rmdir tmp_dir with [ Unix.Unix_error _ _ _ -> () ];
    try Unix.rmdir "gw_tmp" with [ Unix.Unix_error _ _ _ -> () ];
    output_command_line bdir;
    if do_check.val || do_consang.val then do {
      let base = Gwdb.open_base bdir in
      if do_check.val then
        Check.check_base base (set_error base) (set_warning base)
          (fun _ -> True) changed_p pr_stats.val
      else ();
      if do_consang.val then
        let _ : option _ = ConsangAll.compute base True False in ()
      else ();
    }
    else ();
    True
  }
  else False
};
