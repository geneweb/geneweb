(* $Id: iolight.ml,v 1.1 2000-07-27 02:26:21 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;

value magic_gwb = "GnWb001y";

value check_magic =
  let b = String.create (String.length magic_gwb) in
  fun ic ->
    do really_input ic b 0 (String.length b); return
    if b <> magic_gwb then
      if String.sub magic_gwb 0 4 = String.sub b 0 4 then
        failwith "this is a GeneWeb base, but not compatible"
      else
        failwith "this is not a GeneWeb base, or it is a very old version"
    else ()
;

value make_cache ic shift array_pos len name =
  let tab = ref None in
  let r =
    {array = fun []; get = fun []; len = len;
     clear_array = fun _ -> tab.val := None}
  in
  let array () =
    match tab.val with
    [ Some x -> x
    | None ->
do Printf.eprintf "*** read %s\n" name; flush stderr; return
        do seek_in ic array_pos; return
        let t = input_value ic in
        do tab.val := Some t; return t ]
  in
  let gen_get i = (r.array ()).(i) in
  do r.array := array; r.get := gen_get; return r
;

value input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let ic =
    let ic = open_in_bin (Filename.concat bname "base") in
    do check_magic ic; return ic
  in
  let persons_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let unions_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let descends_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let norigin_file = input_value ic in
  let shift = 0 in
  let persons = make_cache ic shift persons_array_pos persons_len "persons" in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends = make_cache ic shift ascends_array_pos persons_len "ascends" in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions = make_cache ic shift unions_array_pos persons_len "unions" in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_cache ic shift families_array_pos families_len "families"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples = make_cache ic shift couples_array_pos families_len "couples" in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_cache ic shift descends_array_pos families_len "descends"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings = make_cache ic shift strings_array_pos strings_len "strings" in
  let cleanup () = close_in ic in
  let read_notes mlen =
    match
      try Some (open_in (Filename.concat bname "notes")) with
      [ Sys_error _ -> None ]
    with
    [ Some ic ->
        let len = ref 0 in
        do try
             while mlen = 0 || len.val < mlen do
               len.val := Buff.store len.val (input_char ic);
             done
           with
           [ End_of_file -> () ];
           close_in ic;
        return Buff.get len.val
    | None -> "" ]
  in
  let commit_notes s =
    let fname = Filename.concat bname "notes" in
    do try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
       try Sys.rename fname (fname ^ "~") with _ -> ();
    return
    if s = "" then ()
    else
      let oc = open_out fname in
      do output_string oc s;
         close_out oc;
      return ()
  in
  let bnotes = {nread = read_notes; norigin_file = norigin_file} in
  let base_data =
    {persons = persons;
     ascends = ascends;
     unions = unions;
     families = families;
     couples = couples;
     descends = descends;
     strings = strings;
     bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = fun [];
     strings_of_fsname = fun [];
     index_of_string = fun [];
     persons_of_surname = {find = fun []; cursor = fun []; next = fun []};
     persons_of_first_name = {find = fun []; cursor = fun []; next = fun []};
     patch_person = fun [];
     patch_ascend = fun [];
     patch_union = fun [];
     patch_family = fun [];
     patch_couple = fun [];
     patch_descend = fun [];
     patch_string = fun [];
     patch_name = fun [];
     patched_ascends = fun [];
     commit_patches = fun [];
     commit_notes = commit_notes;
     cleanup = cleanup}
  in
  {data = base_data; func = base_func}
;
