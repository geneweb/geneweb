(* $Id: iolight.ml,v 5.13 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk;
open Def;

type person = dsk_person;
type ascend = dsk_ascend;
type union = dsk_union;
type family = dsk_family;
type couple = dsk_couple;
type descend = dsk_descend;

value magic_gwb = "GnWb0020";

value check_magic =
  let b = String.create (String.length magic_gwb) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_gwb then
        if String.sub magic_gwb 0 4 = String.sub b 0 4 then
          failwith "this is a GeneWeb base, but not compatible"
        else
          failwith "this is not a GeneWeb base, or it is a very old version"
      else ()
    }
;

type patches =
  { p_person : ref (list (int * person));
    p_ascend : ref (list (int * ascend));
    p_union : ref (list (int * union));
    p_family : ref (list (int * family));
    p_couple : ref (list (int * couple));
    p_descend : ref (list (int * descend));
    p_string : ref (list (int * string));
    p_name : ref (list (int * list iper)) }
;

value rec patch_len len =
  fun
  [ [] -> len
  | [(i, _) :: l] -> patch_len (max len (i + 1)) l ]
;

value apply_patches tab plist plen =
  if plist = [] then tab
  else do {
    let new_tab =
      if plen > Array.length tab then do {
        let new_tab = Array.create plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab);
        new_tab
      }
      else tab
    in
    List.iter (fun (i, v) -> new_tab.(i) := v) plist;
    new_tab
  }
;

value value_header_size = 20;
value array_header_size len = if len < 8 then 1 else 5;

(* to turn around lack of header in some output valued arrays version 4.10 *)
value input_4_10_array ic pos len =
  do {
    Printf.eprintf "*** recovering 4.10 array...\n";
    flush stderr;
    seek_in ic (pos + value_header_size + array_header_size len);
    Array.init len (fun _ -> Iovalue.input ic)
  }
;

value make_record_access ic shift array_pos patches len name =
  let tab = ref None in
  let rec array () =
    match tab.val with
    [ Some x -> x
    | None -> do {
        Printf.eprintf "*** read %s\n" name;
        flush stderr;
        seek_in ic array_pos;
        let v =
          try input_value ic with
          [ Failure _ -> input_4_10_array ic array_pos len ]
        in
        let t = apply_patches v patches.val r.len in
        tab.val := Some t;
        t
      } ]
  and r =
    {load_array () = let _ = array () in (); get i = (array ()).(i);
     set i v = (array ()).(i) := v; len = patch_len len patches.val;
     output_array oc = Mutil.output_value_no_sharing oc (array () : array _);
     clear_array () = tab.val := None}
  in
  r
;

value input_patches bname =
  let patches =
    match
      try Some (open_in_bin (Filename.concat bname "patches")) with _ -> None
    with
    [ Some ic -> let p = input_value ic in do { close_in ic; p }
    | None ->
        {p_person = ref []; p_ascend = ref []; p_union = ref [];
         p_family = ref []; p_couple = ref []; p_descend = ref [];
         p_string = ref []; p_name = ref []} ]
  in
  patches
;

value input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let ic =
    let ic = open_in_bin (Filename.concat bname "base") in
    do { check_magic ic; ic }
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
  let persons =
    make_record_access ic shift persons_array_pos patches.p_person persons_len
      "persons"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_record_access ic shift ascends_array_pos patches.p_ascend persons_len
      "ascends"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_record_access ic shift unions_array_pos patches.p_union persons_len
      "unions"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_record_access ic shift families_array_pos patches.p_family
      families_len "families"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_record_access ic shift couples_array_pos patches.p_couple families_len
      "couples"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_record_access ic shift descends_array_pos patches.p_descend
      families_len "descends"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_record_access ic shift strings_array_pos patches.p_string strings_len
      "strings"
  in
  let cleanup () = close_in ic in
  let read_notes fnotes rn_mode =
    let fname =
      if fnotes = "" then "notes"
      else Filename.concat "notes_d" (fnotes ^ ".txt")
    in
    match
      try Some (Secure.open_in (Filename.concat bname fname)) with
      [ Sys_error _ -> None ]
    with
    [ Some ic -> do {
        let str =
          match rn_mode with
          [ RnDeg -> if in_channel_length ic = 0 then "" else " "
          | Rn1Ln -> try input_line ic with [ End_of_file -> "" ]
          | RnAll ->
              loop 0 where rec loop len =
                match
                  try Some (input_char ic) with [ End_of_file -> None ]
                with
                [ Some c -> loop (Buff.store len c)
                | _ -> Buff.get len ] ]
        in
        close_in ic;
        str
      }
    | None -> "" ]
  in
  let commit_notes fnotes s =
    let fname =
      if fnotes = "" then "notes"
      else do {
        try Unix.mkdir (Filename.concat bname "notes_d") 0o755 with _ -> ();
        Filename.concat "notes_d" (fnotes ^ ".txt")
      }
    in
    let fname = Filename.concat bname fname in
    do {
      try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
      try Sys.rename fname (fname ^ "~") with _ -> ();
      if s = "" then ()
      else do {
        let oc = open_out fname in output_string oc s; close_out oc;
      }
    }
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles _ = []}
  in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible = { v_write = fun []; v_get = fun [] };
     families = families; couples = couples; descends = descends;
     strings = strings; particles = []; bnotes = bnotes; bdir = bname}
  in
  let base_func =
    {person_of_key = fun []; persons_of_name = fun [];
     strings_of_fsname = fun [];
     persons_of_surname = {find = fun []; cursor = fun []; next = fun []};
     persons_of_first_name = {find = fun []; cursor = fun []; next = fun []};
     patch_person = fun []; patch_ascend = fun [];
     patch_union = fun []; patch_family = fun []; patch_couple = fun [];
     patch_descend = fun []; patch_name = fun []; insert_string = fun [];
     commit_patches = fun []; commit_notes = commit_notes;
     patched_ascends = fun []; is_patched_person _ = False;
     cleanup = cleanup}
  in
  {data = base_data; func = base_func}
;
