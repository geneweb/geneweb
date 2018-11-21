(* $Id: db2out.ml,v 5.28 2012-01-27 17:14:03 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

let phony_min_size = 8

let check_input_value _func _fname _len = ()
  (* Printf.eprintf "*** check input_value (%s) %s\n" func fname; flush stderr;
   * let ic = open_in_bin fname in
   * let tab = input_value ic in
   * if not (Obj.is_block (Obj.repr tab)) then failwith "not a block" else ();
   * Printf.eprintf "tab len %d cnt %d\n" (Array.length tab) len;
   * flush stderr;
   * if Array.length tab <> len then failwith "error" else ();
   * close_in ic;
   * Printf.eprintf "check ok\n"; flush stderr *)

let output_item_no_compress_return_pos oc_dat item_cnt s =
  incr item_cnt ; let pos = pos_out oc_dat in Iovalue.output oc_dat s; pos

let output_value_array_no_compress bdir e len pad f =
  let oc_acc = open_out_bin (Filename.concat bdir ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat bdir ("data" ^ e)) in
  let header_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 (max len phony_min_size);
  assert (pos_out oc_dat = Db2.first_item_pos len);
  let nb_items = ref 0 in
  f oc_acc (output_item_no_compress_return_pos oc_dat nb_items);
  (* padding to at least 8 items to allow correct read by input_value *)
  for i = !nb_items + 1 to 8 do
    incr nb_items;
    Iovalue.output oc_dat (pad : 'a)
  done;
  assert (Db2.first_item_pos !nb_items = Db2.first_item_pos len);
  let _ = (Iovalue.patch_output_value_header oc_dat header_pos : int) in
  close_out oc_dat;
  close_out oc_acc;
  (* test *)
  check_input_value "Db2out.output_value_array_no_compress"
    (Filename.concat bdir ("data" ^ e)) (max len phony_min_size)

let output_item_compress_return_pos oc_dat ht item_cnt s =
  try Hashtbl.find ht s with
    Not_found ->
      incr item_cnt;
      let pos = pos_out oc_dat in
      Iovalue.output oc_dat s; Hashtbl.add ht s pos; pos

let output_value_array_compress bdir e _ pad f =
  let oc_acc = open_out_bin (Filename.concat bdir ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat bdir ("data" ^ e)) in
  let ht : ('a, _) Hashtbl.t = Hashtbl.create 1 in
  let header_pos = Iovalue.create_output_value_header oc_dat in
  let len = phony_min_size in
  Iovalue.output_block_header oc_dat 0 len;
  assert (pos_out oc_dat = Db2.first_item_pos len);
  let nb_items = ref 0 in
  f oc_acc (output_item_compress_return_pos oc_dat ht nb_items);
  (* padding to at least 8 items to allow correct read by input_value *)
  for i = !nb_items + 1 to 8 do
    incr nb_items;
    Iovalue.output oc_dat (pad : 'a)
  done;
  if Db2.first_item_pos !nb_items = Db2.first_item_pos len then
    begin
      Iovalue.size_32 := !(Iovalue.size_32) - len + !nb_items;
      Iovalue.size_64 := !(Iovalue.size_64) - len + !nb_items;
      let _ = (Iovalue.patch_output_value_header oc_dat header_pos : int) in
      Iovalue.output_block_header oc_dat 0 !nb_items;
      assert (pos_out oc_dat = Db2.first_item_pos !nb_items);
      close_out oc_dat;
      close_out oc_acc;
      (* test *)
      let fname = Filename.concat bdir ("data" ^ e) in
      check_input_value "Db2out.output_value_array_compress" fname !nb_items
    end
  else if Db2.first_item_pos !nb_items > Db2.first_item_pos len then
    begin
      (* may happen one day and to be debugged then *)
      Printf.eprintf "nb_items %d\n" !nb_items;
      Printf.eprintf "first_item_pos nb_items %d\n"
        (Db2.first_item_pos !nb_items);
      flush stderr;
      Printf.eprintf "rebuilding it...";
      flush stderr;
      close_out oc_dat;
      close_out oc_acc;
      let fname = Filename.concat bdir ("data" ^ e) in
      let ic = open_in_bin fname in
      let oc = open_out_bin (fname ^ "2") in
      let header_pos = Iovalue.create_output_value_header oc in
      Iovalue.output_block_header oc 0 !nb_items;
      seek_in ic (Db2.first_item_pos len);
      begin try while true do output_byte oc (input_byte ic) done with
        End_of_file -> ()
      end;
      let _ = (Iovalue.patch_output_value_header oc header_pos : int) in
      close_out oc;
      close_in ic;
      Mutil.remove_file fname;
      Sys.rename (fname ^ "2") fname;
      Printf.eprintf " ok";
      flush stderr;
      (* test *)
      check_input_value "Db2out.output_value_array_compress 1" fname !nb_items
    end
  else assert false

[@@@ocaml.warning "-37"]
type ('a, 'b) hashtbl_t =
  { mutable size : int;
    mutable data : ('a, 'b) bucketlist array;
    mutable seed : int;
    initial_size : int }
and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist
[@@@ocaml.warning "+37"]

let output_hashtbl dir file ht =
  let oc_ht = open_out_bin (Filename.concat dir file) in
  let oc_hta = open_out_bin (Filename.concat dir (file ^ "a")) in
  let ht : ('a, 'b) hashtbl_t = Obj.magic (ht : ('a, 'b) Hashtbl.t) in
  (* check compatibility with version of Hashtbl of OCaml *)
  assert (Obj.is_block (Obj.repr ht));
  assert (Obj.tag (Obj.repr ht) = 0);
  assert (Obj.size (Obj.repr ht) >= 2 && Obj.size (Obj.repr ht) <= 4);
  assert (Obj.is_int (Obj.repr ht.size));
  assert (Obj.is_block (Obj.repr ht.data));
  if Obj.size (Obj.repr ht) >= 3 then assert (Obj.is_int (Obj.repr ht.seed));
  if Obj.size (Obj.repr ht) >= 4 then
    assert (Obj.is_int (Obj.repr ht.initial_size));
  output_binary_int oc_hta (Array.length ht.data);
  let pos_start = Iovalue.create_output_value_header oc_ht in
  Iovalue.output_block_header oc_ht 0 (Obj.size (Obj.repr ht));
  Iovalue.output oc_ht ht.size;
  Iovalue.output_block_header oc_ht 0 (Array.length ht.data);
  for i = 0 to Array.length ht.data - 1 do
    assert
      (Obj.is_int (Obj.repr ht.data.(i)) && Obj.magic ht.data.(i) = 0 ||
       Obj.is_block (Obj.repr ht.data.(i)) &&
       Obj.tag (Obj.repr ht.data.(i)) = 0 &&
       Obj.size (Obj.repr ht.data.(i)) = 3);
    output_binary_int oc_hta (pos_out oc_ht);
    Iovalue.output oc_ht ht.data.(i)
  done;
  if Obj.size (Obj.repr ht) >= 3 then Iovalue.output oc_ht ht.seed;
  if Obj.size (Obj.repr ht) >= 4 then Iovalue.output oc_ht ht.initial_size;
  let _ = (Iovalue.patch_output_value_header oc_ht pos_start : int) in
  close_out oc_hta; close_out oc_ht

let add_name ht s pos =
  let k = Name.crush_lower s in
  let posl = Hashtbl.find_all ht k in
  if List.mem pos posl then () else Hashtbl.add ht k pos

let make_string_of_crush_index bpdir =
  List.iter
    (fun (field, is_surname) ->
       let field_d = Filename.concat bpdir field in
       let pos_1st =
         let ic_acc = open_in_bin (Filename.concat field_d "access") in
         let pos = try input_binary_int ic_acc with End_of_file -> -1 in
         close_in ic_acc; pos
       in
       if !(Mutil.verbose) then
         begin Printf.eprintf "string_of_crush %s..." field; flush stderr end;
       let ht = Hashtbl.create 1 in
       if pos_1st >= 0 then
         begin let ic_dat = open_in_bin (Filename.concat field_d "data") in
           seek_in ic_dat pos_1st;
           begin let rec loop pos =
             match
               try Some (Iovalue.input ic_dat) with End_of_file -> None
             with
               Some s ->
                 assert (Obj.tag (Obj.repr s) = Obj.string_tag);
                 if s <> "?" then
                   begin
                     add_name ht s pos;
                     if is_surname then
                       List.iter (fun s -> add_name ht s pos)
                         (Mutil.surnames_pieces s)
                   end;
                 loop (pos_in ic_dat)
             | None -> ()
           in
             loop pos_1st
           end;
           close_in ic_dat
         end;
       output_hashtbl field_d "string_of_crush.ht" ht;
       if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end)
    ["first_name", false; "surname", true]

let make_person_of_string_index bpdir =
  List.iter
    (fun field ->
       let field_d = Filename.concat bpdir field in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       if !(Mutil.verbose) then
         begin
           Printf.eprintf "person_of_string %s..." field;
           flush stderr
         end;
       let ht = Hashtbl.create 1 in
       begin let rec loop i =
         match
           try Some (input_binary_int ic_acc) with End_of_file -> None
         with
           Some pos -> Hashtbl.add ht pos i; loop (i + 1)
         | None -> ()
       in
         loop 0
       end;
       close_in ic_acc;
       output_hashtbl field_d "person_of_string.ht" ht;
       if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end)
    ["first_name"; "surname"]

let read_field (ic_acc, ic_dat) i =
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  seek_in ic_dat pos; Iovalue.input ic_dat

let read_string_field : _ -> _ -> string = read_field
let read_array_int_field : _ -> _ -> int array = read_field
let read_int_field : _ -> _ -> int = read_field

let read_string_list_field (ic_acc, ic_dat, ic_str) i =
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  if pos = -1 then []
  else
    begin
      seek_in ic_dat pos;
      let posl : int list = Iovalue.input ic_dat in
      List.map
        (fun pos -> seek_in ic_str pos; (Iovalue.input ic_str : string)) posl
    end

let read_title_list_field (ic_acc, ic_dat, ic_str) i =
  seek_in ic_acc (4 * i);
  let pos = input_binary_int ic_acc in
  if pos = -1 then []
  else
    begin
      seek_in ic_dat pos;
      let tl : int Def.gen_title list = Iovalue.input ic_dat in
      List.map
        (Futil.map_title_strings
           (fun pos -> seek_in ic_str pos; (Iovalue.input ic_str : string)))
        tl
    end

let make_name_index base_d nbper =
  if !(Mutil.verbose) then
    begin Printf.eprintf "name index...\n"; flush stderr end;
  let ic2_list =
    List.map
      (fun (d, f) ->
         let d = List.fold_left Filename.concat base_d [d; f] in
         let fn_acc = Filename.concat d "access" in
         let fn_dat = Filename.concat d "data" in
         let ic_acc = open_in_bin fn_acc in
         let ic_dat = open_in_bin fn_dat in f, (ic_acc, ic_dat))
      ["person", "first_name"; "person", "surname"; "person", "public_name";
       "person", "sex"; "person", "family"; "family", "father";
       "person", "parents"]
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
         let ic_str = open_in_bin fn_str in f, (ic_acc, ic_dat, ic_str))
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
    fun i ->
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
             husb_surname, husb_surn_ali)
          (Array.to_list (get_family i))
      else []
  in
  let get_parents =
    let (ic_acc, ic_dat) = List.assoc "parents" ic2_list in
    fun i ->
      seek_in ic_acc (4 * i);
      let pos = input_binary_int ic_acc in
      if pos = -1 then None
      else begin seek_in ic_dat pos; Some (Iovalue.input ic_dat) end
  in
  let get_father_titles_places i =
    match get_parents i with
      Some ifam ->
        let ifath = get_father ifam in
        List.map (fun t -> t.Def.t_place) (get_titles ifath)
    | None -> []
  in
  let ht = Hashtbl.create 1 in
  if nbper > 0 && !(Mutil.verbose) then ProgrBar.start ();
  for i = 0 to nbper - 1 do
    if !(Mutil.verbose) then ProgrBar.run i nbper;
    let first_name = get_first_name i in
    let surname = get_surname i in
    let names =
      let names =
        Futil.gen_person_misc_names first_name surname (get_public_name i)
          (get_qualifiers i) (get_aliases i) (get_first_names_aliases i)
          (get_surnames_aliases i) (get_titles i) (get_husbands i)
          (get_father_titles_places i)
      in
      Name.lower (first_name ^ " " ^ surname) :: names
    in
    List.iter (fun s -> Hashtbl.add ht (Name.crush_lower s) i) names
  done;
  if nbper > 0 && !(Mutil.verbose) then ProgrBar.finish ();
  List.iter (fun (_, (ic_acc, ic_dat)) -> close_in ic_acc; close_in ic_dat)
    ic2_list;
  List.iter
    (fun (_, (ic_acc, ic_dat, ic_str)) ->
       close_in ic_acc; close_in ic_dat; close_in ic_str)
    ic3_list;
  let dir = Filename.concat base_d "person_of_name" in
  Mutil.mkdir_p dir; output_hashtbl dir "person_of_name.ht" ht

let start_with s p =
  String.length p < String.length s && String.sub s 0 (String.length p) = p

let make_index bdir particles f2 =
  let f1 = "person" in
  let fdir = List.fold_left Filename.concat bdir [f1; f2] in
  let index_dat_fname = Filename.concat fdir "index.dat" in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let pos_1st =
    let ic_acc = open_in_bin (Filename.concat fdir "access") in
    let pos = try input_binary_int ic_acc with End_of_file -> -1 in
    close_in ic_acc; pos
  in
  let (list, len) =
    if pos_1st >= 0 then
      let data_fname = Filename.concat fdir "data" in
      let ic = open_in_bin data_fname in
      seek_in ic pos_1st;
      let rec loop list len pos =
        match
          try Some (Iovalue.input ic : string) with End_of_file -> None
        with
          Some s ->
            assert (Obj.tag (Obj.repr s) = Obj.string_tag);
            let s =
              try
                let part = List.find (start_with s) particles in
                let plen = String.length part in
                String.sub s plen (String.length s - plen) ^ " (" ^ part ^ ")"
              with Not_found -> s
            in
            let list = (s, pos) :: list in loop list (len + 1) (pos_in ic)
        | None -> let _ = close_in ic in list, len
      in
      loop [] 0 pos_1st
    else [], 0
  in
  let list = List.sort compare list in
  let a = Array.make len ("", 0) in
  let iofc =
    let rec loop rev_iofc i =
      function
        [] -> List.rev rev_iofc
      | (s, _ as s_pos) :: list ->
          a.(i) <- s_pos;
          let rev_iofc =
            match rev_iofc with
              (prev_s, _) :: _ ->
                if s = "" && i <> 0 then rev_iofc
                else if prev_s = "" then (s, i) :: rev_iofc
                else
                  let prev_nbc = Name.nbc prev_s.[0] in
                  let nbc = Name.nbc s.[0] in
                  if prev_nbc = nbc && nbc > 0 &&
                     nbc <= String.length prev_s && nbc <= String.length s &&
                     String.sub prev_s 0 nbc = String.sub s 0 nbc
                  then
                    rev_iofc
                  else (s, i) :: rev_iofc
            | [] -> [s, i]
          in
          loop rev_iofc (i + 1) list
    in
    loop [] 0 list
  in
  let oc = open_out_bin index_dat_fname in
  output_value oc (a : (string * int) array);
  close_out oc;
  let oc = open_out_bin (Filename.concat fdir "index.acc") in
  let _ =
    (Iovalue.output_array_access oc (Array.get a) (Array.length a) 0 : int)
  in
  close_out oc;
  let oc = open_out_bin index_ini_fname in
  output_value oc (iofc : (string * int) list); close_out oc

let make_indexes bbdir nb_per particles =
  let bpdir = Filename.concat bbdir "person" in
  make_string_of_crush_index bpdir;
  make_person_of_string_index bpdir;
  make_name_index bbdir nb_per;
  make_index bbdir particles "first_name";
  make_index bbdir particles "surname"
