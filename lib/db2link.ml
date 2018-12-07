(* $Id: db2link.ml,v 5.18 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 2006-2008 INRIA *)

open Def
open Gwcomp

let default_source = ref ""
let do_check = ref true
let do_consang = ref false
let pr_stats = ref false

type file_info =
  { mutable f_curr_src_file : string;
    mutable f_curr_gwo_file : string;
    mutable f_separate : bool;
    mutable f_has_separates : bool;
    mutable f_sep_file_inx : int }

let max_warnings = 10
let max_errors = 10

type family =
  { fam : (iper, string) gen_family;
    cpl : iper gen_couple;
    des : iper gen_descend }

type 'a file_field =
  { oc_dat : out_channel;
    oc_acc : out_channel;
    dname : string;
    start_pos : Iovalue.header_pos option;
    mutable sz32 : int;
    mutable sz64 : int;
    mutable item_cnt : int;
    valu : 'a -> Obj.t }

type gen =
  { mutable g_pcnt : int;
    mutable g_fcnt : int;
    mutable g_scnt : int;
    g_file_info : file_info;
    mutable g_error : bool;
    mutable g_error_cnt : int;
    mutable g_warning_cnt : int;
    g_first_av_occ : (Adef.istr * Adef.istr, int) Hashtbl.t;
    g_tmp_dir : string;
    g_particles : string list;
    g_strings : (string, Adef.istr) Hashtbl.t;
    g_index_of_key : (Db2.key2, iper) Hashtbl.t;
    mutable g_occ_of_key : (Db2.key2, int) Hashtbl.t array;
    g_person_fields : (iper, string) gen_person file_field list;
    g_family_fields : family file_field list;
    g_person_parents : Iochan.t * out_channel;
    g_person_unions : Iochan.t * out_channel;
    g_person_rparents : Iochan.t * out_channel;
    g_person_related : Iochan.t * Iochan.t;
    g_person_notes : Iochan.t * out_channel;
    g_person_pevents : Iochan.t * out_channel }

let person_fields_arr =
  ["first_name", (fun so -> Obj.repr so.first_name);
   "surname", (fun so -> Obj.repr so.surname);
   "occ", (fun so -> Obj.repr so.occ); "image", (fun so -> Obj.repr so.image);
   "public_name", (fun so -> Obj.repr so.public_name);
   "qualifiers", (fun so -> Obj.repr so.qualifiers);
   "aliases", (fun so -> Obj.repr so.aliases);
   "first_names_aliases", (fun so -> Obj.repr so.first_names_aliases);
   "surnames_aliases", (fun so -> Obj.repr so.surnames_aliases);
   "titles", (fun so -> Obj.repr so.titles);
   "occupation", (fun so -> Obj.repr so.occupation);
   "sex", (fun so -> Obj.repr so.sex);
   "access", (fun so -> Obj.repr so.access);
   "birth", (fun so -> Obj.repr so.birth);
   "birth_place", (fun so -> Obj.repr so.birth_place);
   "birth_note", (fun so -> Obj.repr so.birth_note);
   "birth_src", (fun so -> Obj.repr so.birth_src);
   "baptism", (fun so -> Obj.repr so.baptism);
   "baptism_place", (fun so -> Obj.repr so.baptism_place);
   "baptism_note", (fun so -> Obj.repr so.baptism_note);
   "baptism_src", (fun so -> Obj.repr so.baptism_src);
   "death", (fun so -> Obj.repr so.death);
   "death_place", (fun so -> Obj.repr so.death_place);
   "death_note", (fun so -> Obj.repr so.death_note);
   "death_src", (fun so -> Obj.repr so.death_src);
   "burial", (fun so -> Obj.repr so.burial);
   "burial_place", (fun so -> Obj.repr so.burial_place);
   "burial_note", (fun so -> Obj.repr so.burial_note);
   "burial_src", (fun so -> Obj.repr so.burial_src);
   "pevents", (fun so -> Obj.repr so.pevents);
   "psources", (fun so -> Obj.repr so.psources)]

let family_fields_arr =
  ["marriage", (fun so -> Obj.repr so.fam.marriage);
   "marriage_place", (fun so -> Obj.repr so.fam.marriage_place);
   "marriage_note", (fun so -> Obj.repr so.fam.marriage_note);
   "marriage_src", (fun so -> Obj.repr so.fam.marriage_src);
   "fevents", (fun so -> Obj.repr so.fam.fevents);
   "witnesses", (fun so -> Obj.repr so.fam.witnesses);
   "relation", (fun so -> Obj.repr so.fam.relation);
   "divorce", (fun so -> Obj.repr so.fam.divorce);
   "comment", (fun so -> Obj.repr so.fam.comment);
   "origin_file", (fun so -> Obj.repr so.fam.origin_file);
   "fsources", (fun so -> Obj.repr so.fam.fsources);
   "father", (fun so -> Obj.repr (Adef.father so.cpl));
   "mother", (fun so -> Obj.repr (Adef.mother so.cpl));
   "children", (fun so -> Obj.repr so.des.children)]

let input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "d’"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur "; "AF "; "D'"; "D’"; "DAL "; "DE ";
     "DI "; "DU "; "OF "; "VAN "; "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file

let particules_file = ref ""

let unique_key_string gen s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find gen.g_strings s with
    Not_found ->
      let istr = Adef.istr_of_int gen.g_scnt in
      Hashtbl.add gen.g_strings s istr; gen.g_scnt <- gen.g_scnt + 1; istr

let key_hashtbl_find ht k = Hashtbl.find ht (Db2.key2_of_key k)
let key_hashtbl_add ht k v = Hashtbl.add ht (Db2.key2_of_key k) v

let occ_of_key_ht gen =
  let i = gen.g_file_info.f_sep_file_inx in
  let len = Array.length gen.g_occ_of_key in
  if i >= len then
    begin let new_len = max (i + 1) (2 * len + 1) in
      gen.g_occ_of_key <-
        Array.append gen.g_occ_of_key
          (Array.init (new_len - len) (fun _ -> Hashtbl.create 1))
    end;
  gen.g_occ_of_key.(i)

let find_first_available_occ gen so fn sn =
  let occ =
    try Hashtbl.find gen.g_first_av_occ (fn, sn) with Not_found -> 0
  in
  let rec loop occ =
    let k1 = fn, sn, occ in
    match
      try Some (key_hashtbl_find gen.g_index_of_key k1) with Not_found -> None
    with
      Some _ -> loop (occ + 1)
    | None ->
        gen.g_warning_cnt <- gen.g_warning_cnt - 1;
        if gen.g_warning_cnt > 0 then
          begin
            Printf.eprintf "Warning: %s: %s.%d %s renumbered %d\n"
              gen.g_file_info.f_curr_gwo_file so.first_name so.occ so.surname
              occ;
            flush stderr
          end;
        key_hashtbl_add (occ_of_key_ht gen) (fn, sn, so.occ) occ;
        Hashtbl.replace gen.g_first_av_occ (fn, sn) occ;
        occ
  in
  loop occ

let output_item v ff =
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  Iovalue.output ff.oc_dat v;
  ff.sz32 <- !(Iovalue.size_32);
  ff.sz64 <- !(Iovalue.size_64);
  ff.item_cnt <- ff.item_cnt + 1

let output_field so ff =
  output_binary_int ff.oc_acc (pos_out ff.oc_dat); output_item (ff.valu so) ff

let int_size = 4

let update_pevents_with_person so =
  let evt_birth =
    if Adef.od_of_cdate so.birth <> None
    then
      let evt =
        {epers_name = Epers_Birth; epers_date = so.birth;
         epers_place = so.birth_place; epers_reason = "";
         epers_note = so.birth_note; epers_src = so.birth_src;
         epers_witnesses = [| |]}
      in
      Some evt
    else if so.birth_place = "" && so.birth_src = "" then None
    else
      let evt =
        {epers_name = Epers_Birth; epers_date = so.birth;
         epers_place = so.birth_place; epers_reason = "";
         epers_note = so.birth_note; epers_src = so.birth_src;
         epers_witnesses = [| |]}
      in
      Some evt
  in
  let evt_bapt =
    if Adef.od_of_cdate so.baptism <> None
    then
      let evt =
        {epers_name = Epers_Baptism; epers_date = so.baptism;
         epers_place = so.baptism_place; epers_reason = "";
         epers_note = so.baptism_note; epers_src = so.baptism_src;
         epers_witnesses = [| |]}
      in
      Some evt
    else if so.baptism_place = "" && so.baptism_src = "" then None
    else
      let evt =
        {epers_name = Epers_Baptism; epers_date = so.baptism;
         epers_place = so.baptism_place; epers_reason = "";
         epers_note = so.baptism_note; epers_src = so.baptism_src;
         epers_witnesses = [| |]}
      in
      Some evt
  in
  let evt_death =
    match so.death with
      NotDead | DontKnowIfDead ->
        if so.death_place = "" && so.death_src = "" then None
        else
          let evt =
            {epers_name = Epers_Death; epers_date = Adef.cdate_None;
             epers_place = so.death_place; epers_reason = "";
             epers_note = so.death_note; epers_src = so.death_src;
             epers_witnesses = [| |]}
          in
          Some evt
    | Death (_, cd) ->
        let date = Adef.cdate_of_od (Some (Adef.date_of_cdate cd)) in
        let evt =
          {epers_name = Epers_Death; epers_date = date;
           epers_place = so.death_place; epers_reason = "";
           epers_note = so.death_note; epers_src = so.death_src;
           epers_witnesses = [| |]}
        in
        Some evt
    | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
        let evt =
          {epers_name = Epers_Death; epers_date = Adef.cdate_None;
           epers_place = so.death_place; epers_reason = "";
           epers_note = so.death_note; epers_src = so.death_src;
           epers_witnesses = [| |]}
        in
        Some evt
  in
  let evt_burial =
    match so.burial with
      UnknownBurial ->
        if so.burial_place = "" && so.burial_src = "" then None
        else
          let evt =
            {epers_name = Epers_Burial; epers_date = Adef.cdate_None;
             epers_place = so.burial_place; epers_reason = "";
             epers_note = so.burial_note; epers_src = so.burial_src;
             epers_witnesses = [| |]}
          in
          Some evt
    | Buried cd ->
        let evt =
          {epers_name = Epers_Burial; epers_date = cd;
           epers_place = so.burial_place; epers_reason = "";
           epers_note = so.burial_note; epers_src = so.burial_src;
           epers_witnesses = [| |]}
        in
        Some evt
    | Cremated cd ->
        let evt =
          {epers_name = Epers_Cremation; epers_date = cd;
           epers_place = so.burial_place; epers_reason = "";
           epers_note = so.burial_note; epers_src = so.burial_src;
           epers_witnesses = [| |]}
        in
        Some evt
  in
  let pevents = [evt_birth; evt_bapt; evt_death; evt_burial] in
  let pevents =
    List.fold_right
      (fun evt pevents ->
         match evt with
           Some evt -> evt :: pevents
         | None -> pevents)
      pevents []
  in
  {so with pevents = pevents}

let insert_person1 gen so =
  if so.first_name <> "?" && so.surname <> "?" then
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let k = fn, sn, so.occ in
    try
      if gen.g_file_info.f_separate then
        ignore (key_hashtbl_find (occ_of_key_ht gen) k : int)
      else ignore (key_hashtbl_find gen.g_index_of_key k : iper);
      gen.g_error_cnt <- gen.g_error_cnt - 1;
      if gen.g_error_cnt > 0 then
        begin
          Printf.eprintf "File \"%s\"\n" gen.g_file_info.f_curr_gwo_file;
          Printf.eprintf "Error: already defined %s.%d %s\n" so.first_name so.occ
            so.surname
        end;
      flush stderr;
      gen.g_error <- true
    with Not_found ->
      let (k, so) =
        if gen.g_file_info.f_separate then
          let occ = find_first_available_occ gen so fn sn in
          (fn, sn, occ), {so with occ = occ}
        else k, so
      in
      let (k, so) =
        let psources =
          if so.psources = "" then !default_source else so.psources
        in
        k, {so with psources = psources}
      in
      let so = update_pevents_with_person so in
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
      (*
      Iochan.seek (fst gen.g_person_pevents) (int_size * gen.g_pcnt);
      if so.pevents = [] then
        Iochan.output_binary_int (fst gen.g_person_pevents) (-1)
      else do {
        Iovalue.output (snd gen.g_person_pevents) so.pevents;
        let pos = pos_out (snd gen.g_person_pevents) in
        Iochan.output_binary_int (fst gen.g_person_pevents) pos;
      };
      *)
      gen.g_pcnt <- gen.g_pcnt + 1

let insert_somebody1 gen sex =
  function
    Undefined _ -> ()
  | Defined so -> let so = {so with sex = sex} in insert_person1 gen so

let insert_family1 gen co fath_sex moth_sex witl fevents _fo deo =
  let _ifath = insert_somebody1 gen fath_sex (Adef.father co) in
  let _imoth = insert_somebody1 gen moth_sex (Adef.mother co) in
  Array.iter (fun key -> insert_person1 gen key) deo.children;
  List.iter (fun (so, sex) -> insert_somebody1 gen sex so) witl;
  List.iter
    (fun (_, _, _, _, _, _, wl) ->
       List.iter (fun (so, sex, _) -> insert_somebody1 gen sex so) wl)
    fevents

let iter_option f =
  function
    Some x -> f x
  | None -> ()

let insert_relation1 gen r =
  iter_option (insert_somebody1 gen Male) r.r_fath;
  iter_option (insert_somebody1 gen Female) r.r_moth

let insert_rparents1 gen sb sex rl =
  insert_somebody1 gen sex sb; List.iter (insert_relation1 gen) rl

let insert_pevents1 gen sb sex pevents =
  insert_somebody1 gen sex sb;
  List.iter
    (fun (_, _, _, _, _, _, wl) ->
       List.iter (fun (so, sex, _) -> insert_somebody1 gen sex so) wl)
    pevents

let insert_bnotes1 gen notesname str =
  let nfname = if notesname = "" then "notes" else notesname in
  let nfname =
      let f =
        match NotesLinks.check_file_name nfname with
          Some (dl, f) -> List.fold_right Filename.concat dl f
        | None -> "bad"
      in
      Filename.concat "notes" f
  in
  let fname =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; nfname ^ ".txt"]
  in
  Mutil.mkdir_p (Filename.dirname fname);
  let oc = open_out fname in
  output_string oc str;
  close_out oc;
  if notesname = "" then
    let fname =
      List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; "notes_of.txt"]
    in
    let oc = open_out fname in
    Printf.fprintf oc "%s\n" gen.g_file_info.f_curr_src_file; close_out oc

let write_file_contents fname text =
  let oc = open_out fname in output_string oc text; close_out oc

let insert_wiznotes1 gen wizid str =
  let wizdir =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; "wiznotes"]
  in
  Mutil.mkdir_p wizdir;
  let fname = Filename.concat wizdir wizid ^ ".txt" in
  write_file_contents fname str

let insert_gwo_1 gen =
  function
    Family (cpl, fs, ms, witl, fevents, fam, des) ->
      insert_family1 gen cpl fs ms witl fevents fam des
  | Notes (_, _) -> ()
  | Relations (sb, sex, rl) -> insert_rparents1 gen sb sex rl
  | Pevent (sb, sex, pevents) -> insert_pevents1 gen sb sex pevents
  | Bnotes (nfname, str) -> insert_bnotes1 gen nfname str
  | Wnotes (wizid, str) -> insert_wiznotes1 gen wizid str

let empty_person =
  {first_name = ""; surname = ""; occ = 0; image = "";
   first_names_aliases = []; surnames_aliases = []; public_name = "";
   qualifiers = []; aliases = []; titles = []; rparents = []; related = [];
   occupation = ""; sex = Neuter; access = IfTitles; birth = Adef.cdate_None;
   birth_place = ""; birth_note = ""; birth_src = "";
   baptism = Adef.cdate_None; baptism_place = ""; baptism_note = "";
   baptism_src = ""; death = DontKnowIfDead; death_place = "";
   death_note = ""; death_src = ""; burial = UnknownBurial; burial_place = "";
   burial_note = ""; burial_src = ""; pevents = []; notes = ""; psources = "";
   key_index = Adef.iper_of_int 0}

let insert_undefined2 gen key fn sn sex =
  if key.pk_first_name <> "?" && key.pk_surname <> "?" then
    key_hashtbl_add gen.g_index_of_key (fn, sn, key.pk_occ)
      (Adef.iper_of_int gen.g_pcnt);
  if gen.g_file_info.f_has_separates then
    begin
      gen.g_error_cnt <- gen.g_error_cnt - 1;
      if gen.g_error_cnt > 0 then
        begin
          gen.g_error_cnt <- -1;
          Printf.eprintf
            "Error: option -sep does not work when there are undefined persons\n";
          flush stderr
        end;
      gen.g_error <- true
    end
  else if !do_check then
    begin
      gen.g_warning_cnt <- gen.g_warning_cnt - 1;
      if gen.g_warning_cnt > 0 then
        Printf.eprintf "Warning: adding undefined %s.%d %s\n"
          (Name.lower key.pk_first_name) key.pk_occ
          (Name.lower key.pk_surname);
      flush stderr
    end;
  let so =
    {empty_person with first_name = key.pk_first_name;
     surname = key.pk_surname; occ = key.pk_occ; sex = sex}
  in
  let so = update_pevents_with_person so in
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
  Iochan.seek (fst gen.g_person_pevents) (int_size * gen.g_pcnt);
  if so.pevents = [] then
    Iochan.output_binary_int (fst gen.g_person_pevents) (-1)
  else
    begin
      Iovalue.output (snd gen.g_person_pevents) so.pevents;
      let pos = pos_out (snd gen.g_person_pevents) in
      Iochan.output_binary_int (fst gen.g_person_pevents) pos
    end;
  gen.g_pcnt <- gen.g_pcnt + 1;
  Adef.iper_of_int (gen.g_pcnt - 1)

let get_undefined2 gen key sex =
  let fn = unique_key_string gen key.pk_first_name in
  let sn = unique_key_string gen key.pk_surname in
  let occ =
    if gen.g_file_info.f_separate then
      try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, key.pk_occ) with
        Not_found -> key.pk_occ
    else key.pk_occ
  in
  try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
    Not_found -> insert_undefined2 gen key fn sn sex

let get_person2 gen so sex =
  if so.first_name <> "?" && so.surname <> "?" then
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let occ =
      if gen.g_file_info.f_separate then
        try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, so.occ) with
          Not_found -> so.occ
      else so.occ
    in
    try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
      Not_found ->
        failwith
          (Printf.sprintf "*** bug not found %s.%d %s" so.first_name so.occ
             so.surname)
  else
    let so = if so.sex = Neuter then {so with sex = sex} else so in
    let so = update_pevents_with_person so in
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
    Iochan.seek (fst gen.g_person_pevents) (int_size * gen.g_pcnt);
    if so.pevents = [] then
      Iochan.output_binary_int (fst gen.g_person_pevents) (-1)
    else
      begin
        Iovalue.output (snd gen.g_person_pevents) so.pevents;
        let pos = pos_out (snd gen.g_person_pevents) in
        Iochan.output_binary_int (fst gen.g_person_pevents) pos
      end;
    gen.g_pcnt <- gen.g_pcnt + 1;
    Adef.iper_of_int (gen.g_pcnt - 1)

let get_somebody2 gen sex =
  function
    Undefined key -> get_undefined2 gen key sex
  | Defined so -> get_person2 gen so sex

let insert_related gen irp ip =
  let (ioc_acc, ioc_dat) = gen.g_person_related in
  Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
  let pos1 = Iochan.input_binary_int ioc_acc in
  let pos2 = Iochan.seek_end ioc_dat in
  Iochan.output_value_no_header ioc_dat (Adef.int_of_iper ip);
  Iochan.output_value_no_header ioc_dat pos1;
  Iochan.seek ioc_acc (int_size * Adef.int_of_iper irp);
  Iochan.output_binary_int ioc_acc pos2

let update_family_with_fevents fam =
  let found_marriage = ref false in
  let found_divorce = ref false in
  let nsck_std_fields =
    match fam.relation with
      NoSexesCheckNotMarried | NoSexesCheckMarried -> true
    | _ -> false
  in
  (* On veut cette fois ci que ce soit le dernier évènement *)
  (* qui soit mis dans les évènements principaux.           *)
  let rec loop fevents fam =
    match fevents with
      [] -> fam
    | evt :: l ->
        match evt.efam_name with
          Efam_Engage ->
            if !found_marriage then loop l fam
            else
              let witnesses = Array.map fst evt.efam_witnesses in
              let fam =
                {fam with relation =
                  if nsck_std_fields then NoSexesCheckNotMarried else Engaged;
                 marriage = evt.efam_date; marriage_place = evt.efam_place;
                 marriage_note = evt.efam_note; marriage_src = evt.efam_src;
                 witnesses = witnesses}
              in
              let () = found_marriage := true in loop l fam
        | Efam_Marriage ->
            let witnesses = Array.map fst evt.efam_witnesses in
            let fam =
              {fam with relation =
                if nsck_std_fields then NoSexesCheckMarried else Married;
               marriage = evt.efam_date; marriage_place = evt.efam_place;
               marriage_note = evt.efam_note; marriage_src = evt.efam_src;
               witnesses = witnesses}
            in
            let () = found_marriage := true in fam
        | Efam_MarriageContract ->
            if !found_marriage then loop l fam
            else
              let witnesses = Array.map fst evt.efam_witnesses in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on met une précision "vers".      *)
              let date =
                match Adef.od_of_cdate evt.efam_date with
                  Some (Dgreg (dmy, cal)) ->
                    let dmy = {dmy with prec = About} in
                    Adef.cdate_of_od (Some (Dgreg (dmy, cal)))
                | _ -> evt.efam_date
              in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on ne met pas de lieu.            *)
              let place = "" in
              let fam =
                {fam with relation =
                  if nsck_std_fields then NoSexesCheckMarried else Married;
                 marriage = date; marriage_place = place;
                 marriage_note = evt.efam_note; marriage_src = evt.efam_src;
                 witnesses = witnesses}
              in
              let () = found_marriage := true in loop l fam
        | Efam_NoMention | Efam_MarriageBann | Efam_MarriageLicense |
          Efam_Annulation | Efam_PACS ->
            if !found_marriage then loop l fam
            else
              let witnesses = Array.map fst evt.efam_witnesses in
              let fam =
                {fam with relation =
                  if nsck_std_fields then NoSexesCheckNotMarried
                  else NoMention;
                 marriage = evt.efam_date; marriage_place = evt.efam_place;
                 marriage_note = evt.efam_note; marriage_src = evt.efam_src;
                 witnesses = witnesses}
              in
              let () = found_marriage := true in loop l fam
        | Efam_NoMarriage ->
            if !found_marriage then loop l fam
            else
              let witnesses = Array.map fst evt.efam_witnesses in
              let fam =
                {fam with relation =
                  if nsck_std_fields then NoSexesCheckNotMarried
                  else NotMarried;
                 marriage = evt.efam_date; marriage_place = evt.efam_place;
                 marriage_note = evt.efam_note; marriage_src = evt.efam_src;
                 witnesses = witnesses}
              in
              let () = found_marriage := true in loop l fam
        | Efam_Divorce ->
            if !found_divorce then loop l fam
            else
              let fam = {fam with divorce = Divorced evt.efam_date} in
              let () = found_divorce := true in loop l fam
        | Efam_Separated ->
            if !found_divorce then loop l fam
            else
              let fam = {fam with divorce = Separated} in
              let () = found_divorce := true in loop l fam
        | _ -> loop l fam
  in
  loop (List.rev fam.fevents) fam

let update_fevents_with_family fam =
  let evt_marr =
    let name =
      match fam.relation with
        Married -> Efam_Marriage
      | NotMarried -> Efam_NoMarriage
      | Engaged -> Efam_Engage
      | NoSexesCheckNotMarried -> Efam_NoMarriage
      | NoMention -> Efam_NoMention
      | NoSexesCheckMarried -> Efam_Marriage
    in
    let witnesses = Array.map (fun ip -> ip, Witness) fam.witnesses in
    let evt =
      {efam_name = name; efam_date = fam.marriage;
       efam_place = fam.marriage_place; efam_reason = "";
       efam_note = fam.marriage_note; efam_src = fam.marriage_src;
       efam_witnesses = witnesses}
    in
    Some evt
  in
  let evt_div =
    match fam.divorce with
      NotDivorced -> None
    | Divorced cd ->
        let evt =
          {efam_name = Efam_Divorce; efam_date = cd; efam_place = "";
           efam_reason = ""; efam_note = ""; efam_src = "";
           efam_witnesses = [| |]}
        in
        Some evt
    | Separated ->
        let evt =
          {efam_name = Efam_Separated; efam_date = Adef.cdate_None;
           efam_place = ""; efam_reason = ""; efam_note = ""; efam_src = "";
           efam_witnesses = [| |]}
        in
        Some evt
  in
  let fevents = [evt_marr; evt_div] in
  let fevents =
    List.fold_right
      (fun evt fevents ->
         match evt with
           Some evt -> evt :: fevents
         | None -> fevents)
      fevents []
  in
  {fam with fevents = fevents}

let insert_family2 gen co fath_sex moth_sex witl fevents fo deo =
  let ifath = get_somebody2 gen fath_sex (Adef.father co) in
  let imoth = get_somebody2 gen moth_sex (Adef.mother co) in
  let children =
    Array.map (fun key -> get_person2 gen key Neuter) deo.children
  in
  let witn =
    List.map
      (fun (so, sex) ->
         let ip = get_somebody2 gen sex so in insert_related gen ip ifath; ip)
      witl
  in
  (* On tri les évènements pour être sûr. *)
  let fevents =
    CheckItem.sort_events
      ((fun (name, _, _, _, _, _, _) -> CheckItem.Fsort name),
       (fun (_, date, _, _, _, _, _) -> date))
      fevents
  in
  let fevents =
    List.map
      (fun (name, date, place, reason, note, src, wl) ->
         let wl =
           List.map
             (fun (so, sex, wk) ->
                let ip = get_somebody2 gen sex so in
                insert_related gen ip ifath; ip, wk)
             wl
         in
         {efam_name = name; efam_date = date; efam_place = place;
          efam_reason = reason; efam_note = note; efam_src = src;
          efam_witnesses = Array.of_list wl})
      fevents
  in
  let fo = {fo with witnesses = Array.of_list witn; fevents = fevents} in
  (* On mets à jour les fevents et events normaux *)
  let fo =
    if fevents <> [] then update_family_with_fevents fo
    else update_fevents_with_family fo
  in
  let fam =
    {fam = fo; cpl = Adef.couple ifath imoth; des = {children = children}}
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
    (fun iper ->
       Iochan.seek (fst gen.g_person_parents)
         (int_size * Adef.int_of_iper iper);
       Iochan.output_binary_int (fst gen.g_person_parents) pos_acc_parents)
    children;
  Iovalue.output (snd gen.g_person_parents) gen.g_fcnt;
  gen.g_fcnt <- gen.g_fcnt + 1

let insert_notes2 gen key str =
  let ip = get_undefined2 gen key Neuter in
  let pos = pos_out (snd gen.g_person_notes) in
  Iovalue.output (snd gen.g_person_notes) str;
  Iochan.seek (fst gen.g_person_notes) (int_size * Adef.int_of_iper ip);
  Iochan.output_binary_int (fst gen.g_person_notes) pos

let map_option f =
  function
    Some x -> Some (f x)
  | None -> None

let insert_relation_parent2 gen ip sex k =
  let irp = get_somebody2 gen sex k in insert_related gen irp ip; irp

let insert_relation2 gen ip r =
  let r_fath = map_option (insert_relation_parent2 gen ip Male) r.r_fath in
  let r_moth = map_option (insert_relation_parent2 gen ip Female) r.r_moth in
  {r_type = r.r_type; r_fath = r_fath; r_moth = r_moth; r_sources = 0}

let insert_rparents2 gen sb sex rl =
  let ip = get_somebody2 gen sex sb in
  let rl = List.map (insert_relation2 gen ip) rl in
  let pos = pos_out (snd gen.g_person_rparents) in
  Iovalue.output (snd gen.g_person_rparents) rl;
  Iochan.seek (fst gen.g_person_rparents) (int_size * Adef.int_of_iper ip);
  Iochan.output_binary_int (fst gen.g_person_rparents) pos

let insert_pevents2 gen sb sex pevents =
  let ip = get_somebody2 gen sex sb in
  let pos = pos_out (snd gen.g_person_pevents) in
  (* On tri les évènements pour être sûr. *)
  let pevents =
    CheckItem.sort_events
      ((fun (name, _, _, _, _, _, _) -> CheckItem.Psort name),
       (fun (_, date, _, _, _, _, _) -> date))
      pevents
  in
  let pevents =
    List.map
      (fun (name, date, place, reason, note, src, wl) ->
         let wl =
           List.map
             (fun (so, sex, wk) ->
                let iw = get_somebody2 gen sex so in
                insert_related gen ip iw; iw, wk)
             wl
         in
         {epers_name = name; epers_date = date; epers_place = place;
          epers_reason = reason; epers_note = note; epers_src = src;
          epers_witnesses = Array.of_list wl})
      pevents
  in
  Iovalue.output (snd gen.g_person_pevents) pevents;
  Iochan.seek (fst gen.g_person_pevents) (int_size * Adef.int_of_iper ip);
  Iochan.output_binary_int (fst gen.g_person_pevents) pos

let insert_gwo_2 gen =
  function
    Family (cpl, fs, ms, witl, fevents, fam, des) ->
      insert_family2 gen cpl fs ms witl fevents fam des
  | Notes (key, str) -> insert_notes2 gen key str
  | Relations (sb, sex, rl) -> insert_rparents2 gen sb sex rl
  | Pevent (sb, sex, pevents) -> insert_pevents2 gen sb sex pevents
  | Bnotes (_, _) -> ()
  | Wnotes (_, _) -> ()

let open_out_field_unknown_size d valu =
  let oc_dat = open_out_bin (Filename.concat d "data1") in
  let oc_acc = open_out_bin (Filename.concat d "access1") in
  {oc_dat = oc_dat; oc_acc = oc_acc; dname = d; start_pos = None; sz32 = 0;
   sz64 = 0; item_cnt = 0; valu = valu}

let close_out_field_known_size ff =
  close_out ff.oc_dat;
  close_out ff.oc_acc;
  (* making input_value header of "data" file, and copying "data1" file *)
  let oc_dat = open_out_bin (Filename.concat ff.dname "data") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  Iovalue.output_block_header oc_dat 0 ff.item_cnt;
  let acc_shift = pos_out oc_dat in
  let fname1 = Filename.concat ff.dname "data1" in
  let ic_dat = open_in_bin fname1 in
  begin try while true do output_byte oc_dat (input_byte ic_dat) done with
    End_of_file -> ()
  end;
  let _ = (Iovalue.patch_output_value_header oc_dat start_pos : int) in
  close_out oc_dat;
  close_in ic_dat;
  Sys.remove fname1;
  (* making "access" file from "access1" file with shift *)
  let oc_acc = open_out_bin (Filename.concat ff.dname "access") in
  let fname = Filename.concat ff.dname "access1" in
  let ic_acc = open_in_bin fname in
  begin try
    while true do
      output_binary_int oc_acc (input_binary_int ic_acc + acc_shift)
    done
  with End_of_file -> ()
  end;
  close_out oc_acc;
  close_in ic_acc;
  Sys.remove fname;
  (* test *)
  Db2out.check_input_value "Db2link.close_out_field_known_size"
    (Filename.concat ff.dname "data") ff.item_cnt

(* used only by "reorder_fields": should be simplified *)
let open_out_field d len valu =
  let oc_dat = open_out_bin (Filename.concat d "data2") in
  let oc_acc = open_out_bin (Filename.concat d "access2") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 (max len Db2out.phony_min_size);
  assert (pos_out oc_dat = Db2.first_item_pos len);
  {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = Some start_pos; dname = d;
   sz32 = !(Iovalue.size_32); sz64 = !(Iovalue.size_64); item_cnt = 0;
   valu = valu}

(* used only by "reorder_fields": should be simplified *)
let close_out_field pad ff len =
  close_out ff.oc_acc;
  ff.item_cnt <- len;
  for i = ff.item_cnt + 1 to Db2out.phony_min_size do
    output_item (ff.valu pad) ff
  done;
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  let start_pos =
    match ff.start_pos with
      Some s -> s
    | None -> assert false
  in
  ignore (Iovalue.patch_output_value_header ff.oc_dat start_pos : int);
  Iovalue.output_block_header ff.oc_dat 0 ff.item_cnt;
  assert (pos_out ff.oc_dat = Db2.first_item_pos ff.item_cnt);
  close_out ff.oc_dat;
  (* test *)
  Db2out.check_input_value "Db2link.close_out_field"
    (Filename.concat ff.dname "data2") ff.item_cnt

let compress_type_string len field_d e ic =
  Db2out.output_value_array_compress field_d e len ""
    (fun oc_acc output_item ->
       seek_in ic (Db2.first_item_pos len);
       try
         while true do
           let s : string = Iovalue.input ic in
           assert (Obj.tag (Obj.repr s) = Obj.string_tag);
           let pos = output_item s in output_binary_int oc_acc pos
         done
       with End_of_file -> ())

let compress_type_list_string len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let sl : string list = Iovalue.input ic in
      if Obj.is_block (Obj.repr sl) then
        begin
          assert (Obj.tag (Obj.repr sl) = 0);
          assert (Obj.size (Obj.repr sl) = 2)
        end
      else assert (Obj.magic sl = 0);
      match sl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let sl =
            List.map
              (Db2out.output_item_compress_return_pos oc_dat ht items_cnt) sl
          in
          Iovalue.output oc_ext (sl : int list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_type_list_title len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let tl : string gen_title list = Iovalue.input ic in
      match tl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let tl =
            List.map
              (Futil.map_title_strings
                 (Db2out.output_item_compress_return_pos oc_dat ht items_cnt))
              tl
          in
          Iovalue.output oc_ext (tl : int gen_title list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_type_list_pevents len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let pl : (int, string) gen_pers_event list = Iovalue.input ic in
      match pl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let pl =
            List.map
              (Futil.map_pers_event (fun id -> id)
                 (Db2out.output_item_compress_return_pos oc_dat ht items_cnt))
              pl
          in
          Iovalue.output oc_ext (pl : (int, int) gen_pers_event list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_type_list_fevents len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let fl : (int, string) gen_fam_event list = Iovalue.input ic in
      match fl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let fl =
            List.map
              (Futil.map_fam_event (fun id -> id)
                 (Db2out.output_item_compress_return_pos oc_dat ht items_cnt))
              fl
          in
          Iovalue.output oc_ext (fl : (int, int) gen_fam_event list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_fields nper nfam tmp_dir =
  List.iter
    (fun (f1, f2, compress_type, len) ->
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
       in
       let ic = open_in_bin (Filename.concat field_d "data") in
       if !(Mutil.verbose) then
         begin Printf.eprintf "compressing %s..." f2; flush stderr end;
       compress_type len field_d "2" ic;
       close_in ic;
       List.iter
         (fun n ->
            let f = Filename.concat field_d n in
            Mutil.remove_file f;
            Sys.rename (Filename.concat field_d (n ^ "2")) f)
         ["data"; "access"];
       if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end)
    ["person", "baptism_place", compress_type_string, nper;
     "person", "baptism_note", compress_type_string, nper;
     "person", "baptism_src", compress_type_string, nper;
     "person", "birth_place", compress_type_string, nper;
     "person", "birth_note", compress_type_string, nper;
     "person", "birth_src", compress_type_string, nper;
     "person", "burial_place", compress_type_string, nper;
     "person", "burial_note", compress_type_string, nper;
     "person", "burial_src", compress_type_string, nper;
     "family", "comment", compress_type_string, nfam;
     "person", "death_place", compress_type_string, nper;
     "person", "death_note", compress_type_string, nper;
     "person", "death_src", compress_type_string, nper;
     "person", "first_name", compress_type_string, nper;
     "family", "fsources", compress_type_string, nfam;
     "person", "image", compress_type_string, nper;
     "family", "marriage_place", compress_type_string, nfam;
     "family", "marriage_note", compress_type_string, nfam;
     "family", "marriage_src", compress_type_string, nfam;
     "person", "occupation", compress_type_string, nper;
     "family", "origin_file", compress_type_string, nfam;
     "person", "psources", compress_type_string, nper;
     "person", "public_name", compress_type_string, nper;
     "person", "surname", compress_type_string, nper;
     "person", "aliases", compress_type_list_string, nper;
     "person", "first_names_aliases", compress_type_list_string, nper;
     "person", "qualifiers", compress_type_list_string, nper;
     "person", "surnames_aliases", compress_type_list_string, nper;
     "person", "titles", compress_type_list_title, nper;
     "person", "pevents", compress_type_list_pevents, nper;
     "family", "fevents", compress_type_list_fevents, nfam]

let read_int_array_field (ic_acc, ic_dat) n =
  seek_in ic_acc (4 * n);
  let pos = input_binary_int ic_acc in
  let rec loop list pos =
    if pos = -1 then Array.of_list list
    else
      begin
        seek_in ic_dat pos;
        let i = Iovalue.input ic_dat in
        loop (i :: list) (Iovalue.input ic_dat)
      end
  in
  loop [] pos

let reorder_type_list_int ic_acc ic_dat ff =
  let item_cnt = ref 0 in
  try
    while true do
      let x = read_int_array_field (ic_acc, ic_dat) !item_cnt in
      output_field x ff; incr item_cnt
    done
  with End_of_file -> ()

let reorder_fields tmp_dir nper =
  let f1 = "person" in
  let f2 = "family" in
  let reorder_type = reorder_type_list_int in
  let len = nper in
  let field_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
  in
  let ic_acc = open_in_bin (Filename.concat field_d "access") in
  let ic_dat = open_in_bin (Filename.concat field_d "data") in
  if !(Mutil.verbose) then (Printf.eprintf "reordering %s..." f2; flush stderr);
  let ff = open_out_field field_d len Obj.repr in
  reorder_type ic_acc ic_dat ff;
  close_out_field [| |] ff len;
  close_in ic_dat;
  close_in ic_acc;
  List.iter
    (fun n ->
       let f = Filename.concat field_d n in
       Mutil.remove_file f;
       Sys.rename (Filename.concat field_d (n ^ "2")) f)
    ["data"; "access"];
  if !(Mutil.verbose) then (Printf.eprintf "\n"; flush stderr)

let output_command_line bdir =
  let oc = open_out (Filename.concat bdir "command.txt") in
  Printf.fprintf oc "%s" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.fprintf oc " %s" Sys.argv.(i)
  done;
  Printf.fprintf oc "\n";
  close_out oc

let output_particles_file tmp_dir particles =
  let fname =
    List.fold_left Filename.concat tmp_dir ["base_d"; "particles.txt"]
  in
  let oc = open_out fname in
  List.iter (fun s -> Printf.fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
  close_out oc

let set_error base x =
  Printf.printf "\nError: "; Check.print_base_error stdout base x

let set_warning base =
  function
    UndefinedSex _ -> ()
  | x -> Printf.printf "\nWarning: "; Check.print_base_warning stdout base x

let fold_option fsome vnone =
  function
    Some v -> fsome v
  | None -> vnone

let changed_p (ip, p, o_sex, o_rpar) =
  let p = Gwdb.dsk_person_of_person p in
  let _p =
    {p with sex = fold_option (fun s -> s) p.sex o_sex;
     rparents =
       fold_option
         (List.map
            (Futil.map_relation_ps (fun p -> p)
               (fun _ -> Adef.istr_of_int 0)))
         p.rparents o_rpar}
  in
  let i = Adef.int_of_iper ip in
  Printf.eprintf "person %d not changed\n" i; flush stderr

let mkdir_and_open_out_field_unknown_size tmp_dir (name, valu) =
  let d = Filename.concat tmp_dir name in
  (try Mutil.mkdir_p d with _ -> ()); open_out_field_unknown_size d valu

(* ******************************************************************** *)
(*  [Fonc] link : (file_info -> unit -> Gwcomp.gw_syntax option) ->     *)
(*                  string -> bool                                      *)
(** [Description] : TODO
    [Args] :
      - next_family_fun :
      - bdir : nom de la base.
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let link next_family_fun bdir =
  let tmp_dir = Filename.concat "gw_tmp" bdir in
  Mutil.remove_dir tmp_dir;
  (try Mutil.mkdir_p tmp_dir with _ -> ());
  let person_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "person"]
  in
  let family_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "family"]
  in
  (try Mutil.mkdir_p person_d with _ -> ());
  (try Mutil.mkdir_p family_d with _ -> ());
  let person_fields =
    List.map (mkdir_and_open_out_field_unknown_size person_d)
      person_fields_arr
  in
  let family_fields =
    List.map (mkdir_and_open_out_field_unknown_size family_d)
      family_fields_arr
  in
  let person_parents =
    let d = Filename.concat person_d "parents" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    open_out_bin (Filename.concat d "data")
  in
  let person_unions =
    let d = Filename.concat person_d "family" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    open_out_bin (Filename.concat d "data")
  in
  let person_rparents =
    let d = Filename.concat person_d "rparents" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    open_out_bin (Filename.concat d "data")
  in
  let person_related =
    let d = Filename.concat person_d "related" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    Iochan.openfile (Filename.concat d "data") true
  in
  let person_notes =
    let d = Filename.concat person_d "notes" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    open_out_bin (Filename.concat d "data")
  in
  let person_pevents =
    let d = Filename.concat person_d "pevents" in
    (try Mutil.mkdir_p d with _ -> ());
    Iochan.openfile (Filename.concat d "access") true,
    open_out_bin (Filename.concat d "data")
  in
  let fi =
    {f_curr_src_file = ""; f_curr_gwo_file = ""; f_separate = false;
     f_has_separates = false; f_sep_file_inx = 0}
  in
  let gen =
    {g_pcnt = 0; g_fcnt = 0; g_scnt = 0; g_file_info = fi; g_error = false;
     g_error_cnt = max_errors + 1; g_warning_cnt = max_warnings + 1;
     g_first_av_occ = Hashtbl.create 1; g_tmp_dir = tmp_dir;
     g_particles = input_particles !particules_file;
     g_strings = Hashtbl.create 1; g_index_of_key = Hashtbl.create 1;
     g_occ_of_key = [| |]; g_person_fields = person_fields;
     g_family_fields = family_fields; g_person_parents = person_parents;
     g_person_unions = person_unions; g_person_rparents = person_rparents;
     g_person_related = person_related; g_person_notes = person_notes;
     g_person_pevents = person_pevents}
  in
  if !(Mutil.verbose) then
    begin Printf.eprintf "pass 1: creating persons...\n"; flush stderr end;
  let next_family = next_family_fun fi in
  begin let rec loop () =
    match next_family () with
      Some fam -> insert_gwo_1 gen fam; loop ()
    | None -> ()
  in
    loop ()
  end;
  Gc.compact ();
  if !(Mutil.verbose) then
    begin Printf.eprintf "pass 2: creating families...\n"; flush stderr end;
  let next_family = next_family_fun fi in
  begin let rec loop () =
    match next_family () with
      Some fam -> insert_gwo_2 gen fam; loop ()
    | None -> ()
  in
    loop ()
  end;
  if gen.g_warning_cnt < 0 then
    begin
      Printf.eprintf "Warning: %d more warnings...\n" (-gen.g_warning_cnt);
      flush stderr
    end;
  if gen.g_error_cnt < 0 then
    begin
      Printf.eprintf "Error: %d more errors...\n" (-gen.g_error_cnt);
      flush stderr
    end;
  List.iter close_out_field_known_size person_fields;
  List.iter close_out_field_known_size family_fields;
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
  Iochan.close (fst person_pevents);
  close_out (snd person_pevents);
  Gc.compact ();
  let person_of_key_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "person_of_key"]
  in
  (try Mutil.mkdir_p person_of_key_d with _ -> ());
  Db2out.output_hashtbl person_of_key_d "iper_of_key.ht"
    (gen.g_index_of_key : (Db2.key2, iper) Hashtbl.t);
  Hashtbl.clear gen.g_index_of_key;
  Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
    (gen.g_strings : (string, Adef.istr) Hashtbl.t);
  Hashtbl.clear gen.g_strings;
  Gc.compact ();
  compress_fields gen.g_pcnt gen.g_fcnt tmp_dir;
  reorder_fields tmp_dir gen.g_pcnt;
  Db2out.make_indexes (Filename.concat tmp_dir "base_d") gen.g_pcnt
    gen.g_particles;
  output_particles_file tmp_dir gen.g_particles;
  if !(Mutil.verbose) then
    begin
      Printf.eprintf "pcnt %d\n" gen.g_pcnt;
      Printf.eprintf "fcnt %d\n" gen.g_fcnt;
      Printf.eprintf "scnt %d\n" gen.g_scnt;
      flush stderr
    end;
  if not gen.g_error then
    begin
      Mutil.mkdir_p bdir;
      let dir = Filename.concat bdir "base_d" in
      let old_dir = Filename.concat bdir "base_d~" in
      Mutil.remove_dir old_dir;
      (try Sys.rename dir old_dir with Sys_error _ -> ());
      Sys.rename (Filename.concat tmp_dir "base_d") dir;
      Mutil.remove_dir old_dir;
      (try Unix.rmdir tmp_dir with Unix.Unix_error (_, _, _) -> ());
      (try Unix.rmdir "gw_tmp" with Unix.Unix_error (_, _, _) -> ());
      output_command_line bdir;
      if !do_check || !do_consang then
        begin let base = Gwdb.open_base bdir in
          if !do_check then
            Check.check_base base (set_error base) (set_warning base)
              (fun _ -> true) changed_p !pr_stats;
          if !do_consang then
            let _ = (ConsangAll.compute base (-1) true : _ option) in ()
        end;
      true
    end
  else false
