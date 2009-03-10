(* camlp5r *)
(* $Id: db2out.ml,v 5.12 2009-03-10 22:05:46 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size = 8;

value output_item_return_pos oc_dat ht item_cnt compress s =
  try if compress then Hashtbl.find ht s else raise Not_found with
  [ Not_found -> do {
      incr item_cnt;
      let pos = pos_out oc_dat in
      Iovalue.output oc_dat s;
      if compress then Hashtbl.add ht s pos else ();
      pos
    } ]
;

value output_value_array oc_dat pad compress f = do {
  let ht : Hashtbl.t 'a _ = Hashtbl.create 1 in
  let header_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 phony_min_size;
  assert (pos_out oc_dat = Db2.first_item_pos);
  let nb_items = ref 0 in
  f (output_item_return_pos oc_dat ht nb_items compress);
  (* padding to at least 8 items to allow correct read by input_value *)
  for i = nb_items.val + 1 to phony_min_size do {
    incr nb_items;
    Iovalue.output oc_dat (pad : 'a);
  };
  Iovalue.size_32.val := Iovalue.size_32.val - phony_min_size + nb_items.val;
  Iovalue.size_64.val := Iovalue.size_64.val - phony_min_size + nb_items.val;
  ignore (Iovalue.patch_output_value_header oc_dat header_pos : int);
  Iovalue.output_block_header oc_dat 0 nb_items.val;
};

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

value add_name ht s pos =
  let k = Name.crush_lower s in
  let posl = Hashtbl.find_all ht k in
  if List.mem pos posl then () else Hashtbl.add ht k pos
;

value make_string_of_crush_index bpdir =
  List.iter
    (fun (field, is_surname) -> do {
       let field_d = Filename.concat bpdir field in
       let ic_dat = open_in_bin (Filename.concat field_d "data") in
       if Mutil.verbose.val then do {
         Printf.eprintf "string_of_crush %s..." field;
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
             if s <> "?" then do {
               add_name ht s pos;
               if is_surname then
                 List.iter (fun s -> add_name ht s pos)
                   (Mutil.surnames_pieces s)
               else ();
             }
             else ();
             loop (pos_in ic_dat)
           }
         | None -> () ];
       close_in ic_dat;
       output_hashtbl field_d "string_of_crush.ht" ht;
       if Mutil.verbose.val then do {
         Printf.eprintf "\n";
         flush stderr;
       }
       else ();
    })
   [("first_name", False); ("surname", True)]
;

value make_person_of_string_index bpdir =
  List.iter
    (fun field -> do {
       let field_d = Filename.concat bpdir field in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       if Mutil.verbose.val then do {
         Printf.eprintf "person_of_string %s..." field;
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
         Printf.eprintf "\n";
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
value read_array_int_field : _ -> _ -> array int = read_field;
value read_int_field : _ -> _ -> int = read_field;

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

value read_title_list_field (ic_acc, ic_dat, ic_str) i = do {
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  if pos = -1 then []
  else do {
    seek_in ic_dat pos;
    let tl : list (Def.gen_title int) = Iovalue.input ic_dat in
    List.map
      (Futil.map_title_strings
        (fun pos -> do {
           seek_in ic_str pos;
           (Iovalue.input ic_str : string)
         }))
      tl
  }
};

value make_name_index base_d nbper = do {
  if Mutil.verbose.val then do {
    Printf.eprintf "name index...\n";
    flush stderr;
  }
  else ();        
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
      let sex : Def.sex = Iovalue.input ic_dat in
      if sex = Def.Female then
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
        List.map (fun t -> t.Def.t_place) (get_titles ifath)
    | None -> [] ]
  }
  in
  let ht = Hashtbl.create 1 in
  if nbper > 0 && Mutil.verbose.val then ProgrBar.start () else ();
  for i = 0 to nbper - 1 do {
    if Mutil.verbose.val then ProgrBar.run i nbper else ();
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
  if nbper > 0 && Mutil.verbose.val then ProgrBar.finish () else ();
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
  let dir = Filename.concat base_d "person_of_name" in
  Mutil.mkdir_p dir;
  output_hashtbl dir "person_of_name.ht" ht;
};

value start_with s p =
  String.length p < String.length s &&
  String.sub s 0 (String.length p) = p
;

value make_index bdir particles f2 = do {
  let f1 = "person" in
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
              let part = List.find (start_with s) particles in
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
                if s = "" && i <> 0 then (* pad *) rev_iofc
                else if prev_s = "" then [(s, i) :: rev_iofc]
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
            | [] -> [(s, i)] ]
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

value make_indexes bbdir nb_per particles = do {
  let bpdir = Filename.concat bbdir "person" in
  make_string_of_crush_index bpdir;
  make_person_of_string_index bpdir;
  make_name_index bbdir nb_per;
  make_index bbdir particles "first_name";
  make_index bbdir particles "surname";
};
