(* camlp4r *)
(* $Id: db2out.ml,v 5.2 2007-02-20 02:54:53 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

value phony_min_size = 8;

value output_item_and_its_pos oc_dat ht item_cnt s =
  try Hashtbl.find ht s with
  [ Not_found -> do {
      incr item_cnt;
      let pos = pos_out oc_dat in
      Iovalue.output oc_dat s;
      Hashtbl.add ht s pos;
      pos
    } ]
;

value output_value_array oc_dat pad f = do {
  let ht : Hashtbl.t 'a _ = Hashtbl.create 1 in
  let header_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 phony_min_size;
  assert (pos_out oc_dat = Db2.first_item_pos);
  let nb_items = ref 0 in
  f (output_item_and_its_pos oc_dat ht nb_items);
  (* padding to at least 8 items to allow correct read by input_value *)
  for i = nb_items.val + 1 to phony_min_size do {
    incr nb_items;
    Iovalue.output oc_dat (pad : 'a);
  };
  Iovalue.size_32.val := Iovalue.size_32.val - phony_min_size + nb_items.val;
  Iovalue.size_64.val := Iovalue.size_32.val - phony_min_size + nb_items.val;
  ignore (Iovalue.patch_output_value_header oc_dat header_pos : int);
  Iovalue.output_block_header oc_dat 0 nb_items.val;
};
