(* $Id: iolight.ml,v 5.13 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

let magic_gwb = "GnWb0020"

let check_magic ic =
  let b = really_input_string ic (String.length magic_gwb) in
  if b <> magic_gwb then
    if String.sub magic_gwb 0 4 = String.sub b 0 4 then
      failwith "this is a GeneWeb base, but not compatible"
    else failwith "this is not a GeneWeb base, or it is a very old version"

type patches =
  { p_person : (int * person) list ref;
    p_ascend : (int * ascend) list ref;
    p_union : (int * union) list ref;
    p_family : (int * family) list ref;
    p_couple : (int * couple) list ref;
    p_descend : (int * descend) list ref;
    p_string : (int * string) list ref;
    p_name : (int * iper list) list ref }

let rec patch_len len =
  function
    [] -> len
  | (i, _) :: l -> patch_len (max len (i + 1)) l

let apply_patches tab plist plen =
  if plist = [] then tab
  else
    let new_tab =
      if plen > Array.length tab then
        let new_tab = Array.make plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab); new_tab
      else tab
    in
    List.iter (fun (i, v) -> new_tab.(i) <- v) plist; new_tab

let value_header_size = 20
let array_header_size len = if len < 8 then 1 else 5

(* to turn around lack of header in some output valued arrays version 4.10 *)
let input_4_10_array ic pos len =
  Printf.eprintf "*** recovering 4.10 array...\n";
  flush stderr;
  seek_in ic (pos + value_header_size + array_header_size len);
  Array.init len (fun _ -> Iovalue.input ic)

let make_record_access ic shift array_pos patches len name =
  let tab = ref None in
  let rec array () =
    match !tab with
      Some x -> x
    | None ->
        Printf.eprintf "*** read %s\n" name;
        flush stderr;
        seek_in ic array_pos;
        let v =
          try input_value ic with
            Failure _ -> input_4_10_array ic array_pos len
        in
        let t = apply_patches v !patches r.len in tab := Some t; t
  and r =
    {load_array = (fun () -> let _ = array () in ());
     get = (fun i -> (array ()).(i)); set = (fun i v -> (array ()).(i) <- v);
     len = patch_len len !patches;
     output_array =
       (fun oc -> Mutil.output_value_no_sharing oc (array () : _ array));
     clear_array = fun () -> tab := None}
  in
  r

let input_patches bname =
  let patches =
    match
      try Some (open_in_bin (Filename.concat bname "patches")) with _ -> None
    with
      Some ic -> let p = input_value ic in close_in ic; p
    | None ->
        {p_person = ref []; p_ascend = ref []; p_union = ref [];
         p_family = ref []; p_couple = ref []; p_descend = ref [];
         p_string = ref []; p_name = ref []}
  in
  patches

let input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let ic =
    let ic = open_in_bin (Filename.concat bname "base") in check_magic ic; ic
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
    make_record_access ic shift couples_array_pos patches.p_couple
      families_len "couples"
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
        Sys_error _ -> None
    with
      Some ic ->
        let str =
          match rn_mode with
            RnDeg -> if in_channel_length ic = 0 then "" else " "
          | Rn1Ln -> (try input_line ic with End_of_file -> "")
          | RnAll ->
              let rec loop len =
                match try Some (input_char ic) with End_of_file -> None with
                  Some c -> loop (Buff.store len c)
                | _ -> Buff.get len
              in
              loop 0
        in
        close_in ic; str
    | None -> ""
  in
  let commit_notes fnotes s =
    let fname =
      if fnotes = "" then "notes"
      else
        begin
          begin try Unix.mkdir (Filename.concat bname "notes_d") 0o755 with
            _ -> ()
          end;
          Filename.concat "notes_d" (fnotes ^ ".txt")
        end
    in
    let fname = Filename.concat bname fname in
    (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
    (try Sys.rename fname (fname ^ "~") with _ -> ());
    if s = "" then ()
    else let oc = open_out fname in output_string oc s; close_out oc
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles = fun _ -> []}
  in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible =
       {v_write =
         (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 218, 27)));
        v_get =
          (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 218, 43)))};
     families = families; couples = couples; descends = descends;
     strings = strings; particles = []; bnotes = bnotes; bdir = bname}
  in
  let base_func =
    {person_of_key =
      (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 223, 21)));
     persons_of_name =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 223, 47)));
     strings_of_fsname =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 224, 25)));
     persons_of_surname =
       {find = (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 225, 34)));
        cursor =
          (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 225, 51)));
        next = (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 225, 66)))};
     persons_of_first_name =
       {find = (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 226, 37)));
        cursor =
          (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 226, 54)));
        next = (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 226, 69)))};
     patch_person =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 227, 20)));
     patch_ascend =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 227, 43)));
     patch_union =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 228, 19)));
     patch_family =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 228, 42)));
     patch_couple =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 228, 65)));
     patch_descend =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 229, 21)));
     patch_name =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 229, 42)));
     insert_string =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 229, 66)));
     commit_patches =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 230, 22)));
     commit_notes = commit_notes;
     patched_ascends =
       (fun _ -> raise (Match_failure ("gwtp/iolight.ml", 231, 23)));
     is_patched_person = (fun _ -> false); cleanup = cleanup}
  in
  {data = base_data; func = base_func}
