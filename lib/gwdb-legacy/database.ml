(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

let move_with_backup src dst =
  Mutil.rm (dst ^ "~");
  Mutil.mv dst (dst ^ "~") ;
  Mutil.mv src dst

(*
 Files in base (directory .gwb)

    base - the base itself
       magic number (magic_gwb)                 : string of length 8
       number of persons                        : binary_int
       number of families                       : binary_int
       number of strings                        : binary_int
       persons array offset in file             : binary_int
       ascends array offset in file             : binary_int
       unions array offset in file              : binary_int
       families array offset in file            : binary_int
       couples array offset in file             : binary_int
       descends array offset in file            : binary_int
       strings array offset in file             : binary_int
       notes origin file                        : value
       persons array                            : value
       ascends array                            : value
       unions array                             : value
       families array                           : value
       couples array                            : value
       descends array                           : value
       strings array                            : value

    base.acc - direct accesses to arrays inside base
       persons offsets   : array of binary_ints
       ascends offsets   : array of binary_ints
       unions offsets    : array of binary_ints
       families offsets  : array of binary_ints
       couples offsets   : array of binary_ints
       descends offsets  : array of binary_ints
       strings offsets   : array of binary_ints

    names.inx - index for names, strings of first names and surnames
       offset to 2nd index : binary_int
       1st index (names) : value
         array, length = "table_size", associating:
          - a hash value of a "crushed" (module "Name") name (modulo length)
          - to the array of indexes of the corresponding persons
       2nd index (first names and surnames strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") first name or
            surname (modulo length)
          - to the array of the corresponding string indexes

    names.acc - direct accesses to arrays inside names.inx

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       offset of surnames index           : binary_int
       offset of first names index        : binary_int
       strings hash table index           : 2 arrays of binary_ints
         strings offset array (length = prime after 10 * strings array length)
           - associating a hash value of the string modulo length
           - to its index in the string array
         strings list array (length = string array length)
           - associating a string index
           - to the index of the next index holding the same hash value
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by snames.inx/sname.dat which use
       -- much less memory
       surnames index                     : value
         binary tree
          - associating the string index of a surname
          - to the corresponding list of persons holding this surname
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by fnames.inx/fname.dat which use
       -- much less memory
       first_names index                  : value
         binary tree
          - associating the string index of a first name
          - to the corresponding list of persons holding this first name

    snames.inx - index for surnames
       binary tree
        - associating the string index of a surname
        - to a pointer (int) to snames.dat

    snames.dat - data associated with snames.inx
      table of list of persons holding a surname

    fnames.inx - index for first names
       binary tree
        - associating the string index of a first name
        - to a pointer (int) to fnames.dat

    fnames.dat - data associated with fnames.inx
      table of list of persons holding a first name

the corresponding list of persons holding this surname

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".
*)

exception Found of int

let hashtbl_right_assoc s ht =
  try
    Hashtbl.iter (fun i1 s1 -> if s = s1 then raise (Found i1)) ht;
    raise Not_found
  with Found x -> x

let index_of_string strings ic start_pos hash_len string_patches string_pending s =
  try hashtbl_right_assoc s string_pending with
  | Not_found ->
    try hashtbl_right_assoc s string_patches with
    | Not_found ->
      match ic, hash_len with
        Some ic, Some hash_len ->
        let ia = Hashtbl.hash s mod hash_len in
        seek_in ic (start_pos + ia * Dutil.int_size);
        let i1 = input_binary_int ic in
        let rec loop i =
          if i = -1 then raise Not_found
          else if strings.get i = s then i
          else
            begin
              seek_in ic (start_pos + (hash_len + i) * Dutil.int_size);
              loop (input_binary_int ic)
            end
        in
        loop i1
      | _ ->
        Printf.eprintf "Sorry. I really need string.inx\n";
        flush stderr;
        failwith "database access"

let persons_of_first_name_or_surname base_data strings params =
  let (_, _, proj, person_patches, names_inx, names_dat, bname) = params in
  let module IstrTree =
    Btree.Make
      (struct
        type t = int
        let compare = Dutil.compare_istr_fun base_data
      end)
  in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    let btr = ref None in
    fun () ->
      match !btr with
        Some bt -> bt
      | None ->
        let fname_inx = Filename.concat bname names_inx in
        let ic_inx = Secure.open_in_bin fname_inx in
          (*
          let ab1 = Gc.allocated_bytes () in
          *)
        let bt : int IstrTree.t = input_value ic_inx in
          (*
          let ab2 = Gc.allocated_bytes () in
          Printf.eprintf "*** new database created by version >= 4.10\n";
          Printf.eprintf "*** using index '%s' allocating here only %.0f bytes\n"
            names_inx (ab2 -. ab1);
          flush stderr;
          *)
        close_in ic_inx;
        btr := Some bt;
        bt
  in
  let find istr =
    let ipera =
      try
        let pos = IstrTree.find istr (bt ()) in
        let ic_dat = Secure.open_in_bin fname_dat in
        seek_in ic_dat pos;
        let len = input_binary_int ic_dat in
        let rec read_loop ipera len =
          if len = 0 then ipera
          else
            let iper = input_binary_int ic_dat in
            read_loop (iper :: ipera) (len - 1)
        in
        let ipera = read_loop [] len in close_in ic_dat; ipera
      with Not_found -> []
    in
    let ipera = ref ipera in
    Hashtbl.iter
      (fun i p ->
         let istr1 = proj p in
         if istr1 = istr && not (List.mem i !ipera)
         then ipera := i :: !ipera)
      person_patches;
    !ipera
  in
  let bt_patched =
    let btr = ref None in
    fun () ->
      match !btr with
        Some bt -> bt
      | None ->
        let bt = ref (bt ()) in
        Hashtbl.iter
          (fun _i p ->
             let istr1 = proj p in
             if not @@ IstrTree.mem istr1 !bt
             then bt := IstrTree.add istr1 0 !bt)
          person_patches;
        btr := Some !bt;
        !bt
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
         Dutil.compare_names base_data str (strings.get key))
      (bt_patched ())
  in
  let next key = IstrTree.next key (bt_patched ()) in
  {find = find; cursor = cursor; next = next}

(* Search index for a given name in file names.inx *)

let persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod Dutil.table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * i);
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : int array)
        else
          let a =
            match !t with
              Some a -> a
            | None ->
                seek_in ic_inx Dutil.int_size;
                let a : Dutil.name_index_data = input_value ic_inx in
                t := Some a;
                a
          in
          a.(i)
      in
      close_in ic_inx; ai
    in
    try let l = Hashtbl.find patches i in l @ Array.to_list ai
    with Not_found -> Array.to_list ai

let strings_of_fsname bname strings (_, person_patches) =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod Dutil.table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * (Dutil.table_size + i));
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : int array)
        else
          let a =
            match !t with
              Some a -> a
            | None ->
                let pos = input_binary_int ic_inx in
                seek_in ic_inx pos;
                let a : Dutil.strings_of_fsname = input_value ic_inx in
                t := Some a;
                a
          in
          a.(i)
      in
      close_in ic_inx; ai
    in
    let l = ref (Array.to_list r) in
    Hashtbl.iter
      (fun _ p ->
         if not (List.mem p.first_name !l) then
           begin let s1 = strings.get p.first_name in
             let s1 = Mutil.nominative s1 in
             if s = Name.crush_lower s1 then l := p.first_name :: !l
           end;
         if not (List.mem p.surname !l) then
           let s1 = strings.get p.surname in
           let s1 = Mutil.nominative s1 in
           if s = Name.crush_lower s1 then l := p.surname :: !l)
      person_patches;
    !l
(**)

(* Restrict file *)

type visible_state = VsNone | VsTrue | VsFalse

let verbose = Mutil.verbose

let make_visible_record_access bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let read_or_create_visible () =
    let visible =
      try
        let ic = Secure.open_in fname in
        if Sys.unix && !verbose then
          begin
            Printf.eprintf "*** read restrict file\n";
            flush stderr
          end ;
        let visible = input_value ic in
        close_in ic ;
        visible
      with Sys_error _ -> Array.make persons.len VsNone
    in
    visible_ref := Some visible ;
    visible
  in
  let v_write () =
    match !visible_ref with
      Some visible ->
        begin try
          let oc = Secure.open_out fname in
          if Sys.unix && !verbose then
            begin
              Printf.eprintf "*** write restrict file\n";
              flush stderr
            end;
          output_value oc visible;
          close_out oc
          with Sys_error _ -> ()
        end
    | None -> ()
  in
  let v_get fct i =
    let visible =
      match !visible_ref with
        Some visible -> visible
      | None -> read_or_create_visible ()
    in
    if i < Array.length visible then
      match visible.(i) with
        VsNone ->
          let status = fct (persons.get i) in
          visible.(i) <- if status then VsTrue else VsFalse;
          visible_ref := Some visible;
          status
      | VsTrue -> true
      | VsFalse -> false
    else fct (persons.get i)
  in
  {v_write = v_write; v_get = v_get}

(*
   Synchro:
     - synchro_person contient la liste des ip des personnes patchées.
     - synchro_family contient la liste des ifam des familles patchées.
     - synchro_patch contient :
         * le timestamp de la modification
         * la liste des personnes modifiées
         * la liste des familles modifiées
*)
let synchro_person = ref []
let synchro_family = ref []
type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }

(* Input *)

type patches_ht =
  { h_person : int ref * (int, person) Hashtbl.t;
    h_ascend : int ref * (int, ascend) Hashtbl.t;
    h_union : int ref * (int, union) Hashtbl.t;
    h_family : int ref * (int, family) Hashtbl.t;
    h_couple : int ref * (int, couple) Hashtbl.t;
    h_descend : int ref * (int, descend) Hashtbl.t;
    h_string : int ref * (int, string) Hashtbl.t;
    h_name : (int, int list) Hashtbl.t
  }

(* Old structure of file "patches", kept for backward compatibility.
   After conversion, a new change will be saved with a magic number
   (magic_patch) and a record "patch_ht" above. *)

module Old =
  struct
    type patches =
      { p_person : (int * person) list ref;
        p_ascend : (int * ascend) list ref;
        p_union : (int * union) list ref;
        p_family : (int * family) list ref;
        p_couple : (int * couple) list ref;
        p_descend : (int * descend) list ref;
        p_string : (int * string) list ref;
        p_name : (int * int list) list ref }
  end

let apply_patches tab patches plen =
  if plen = 0 then tab
  else begin
    let new_tab =
      if plen > Array.length tab then
        let new_tab = Array.make plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab) ;
        new_tab
      else tab
    in
    Hashtbl.iter (Array.set new_tab) patches ;
    new_tab
  end

let make_record_access ic ic_acc shift array_pos (plenr, patches) (_, pending) len name input_array input_item =
  let tab = ref None in
  let cleared = ref false in
  let gen_get nopending i =
    match if nopending then None else Hashtbl.find_opt pending i with
    | Some v -> v
    | None -> match Hashtbl.find_opt patches i with
      | Some v -> v
      | None ->
        match !tab with
        | Some x -> x.(i)
        | None ->
          if i < 0 || i >= len
          then failwith ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
          else match ic_acc with
            | Some ic_acc ->
              seek_in ic_acc (shift + Iovalue.sizeof_long * i);
              let pos = input_binary_int ic_acc in
              seek_in ic pos ;
              input_item ic
            | None ->
              Printf.eprintf "Sorry; I really need base.acc\n";
              flush stderr;
              failwith "cannot access database"
  in
  let array () =
    match !tab with
    | Some x -> x
    | None ->
      seek_in ic array_pos;
      let t = input_array ic in
      tab := Some t ;
      t
  in
  let rec r =
    { load_array = (fun () -> ignore @@ array ())
    ; get = gen_get false
    ; get_nopending = gen_get true
    ; set = (fun i v -> (array ()).(i) <- v)
    ; len = max len !plenr
    ; output_array = begin fun oc ->
        let v = array () in
        let a = apply_patches v patches r.len in
        Dutil.output_value_no_sharing oc (a : _ array)
      end
    ; clear_array = (fun () -> cleared := true ; tab := None)
    }
  in
  r

let magic_patch = "GnPa0001"
let check_patch_magic ic =
  really_input_string ic (String.length magic_patch) = magic_patch

let empty_patch_ht () =
  { h_person = ref 0, Hashtbl.create 1
  ; h_ascend = ref 0, Hashtbl.create 1
  ; h_union = ref 0, Hashtbl.create 1
  ; h_family = ref 0, Hashtbl.create 1
  ; h_couple = ref 0, Hashtbl.create 1
  ; h_descend = ref 0, Hashtbl.create 1
  ; h_string = ref 0, Hashtbl.create 1
  ; h_name = Hashtbl.create 1
  }

let input_patches bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "patches") in
      let r =
        if check_patch_magic ic then (input_value ic : patches_ht)
        else
          begin
            (* old implementation of patches *)
            seek_in ic 0;
            let patches : Old.patches = input_value ic in
            let ht = empty_patch_ht () in
            let add (ir, ht) (k, v) =
              if k >= !ir then ir := k + 1; Hashtbl.add ht k v
            in
            List.iter (add ht.h_person) !(patches.Old.p_person);
            List.iter (add ht.h_ascend) !(patches.Old.p_ascend);
            List.iter (add ht.h_union) !(patches.Old.p_union);
            List.iter (add ht.h_family) !(patches.Old.p_family);
            List.iter (add ht.h_couple) !(patches.Old.p_couple);
            List.iter (add ht.h_descend) !(patches.Old.p_descend);
            List.iter (add ht.h_string) !(patches.Old.p_string);
            List.iter (add (ref 0, ht.h_name)) !(patches.Old.p_name);
            ht
          end
      in
      close_in ic; r
  with _ -> empty_patch_ht ()

let input_synchro bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "synchro_patches") in
    let r : synchro_patch = input_value ic in
    close_in ic ;
    r
  with _ -> {synch_list = []}

let person_of_key persons strings persons_of_name first_name surname occ =
    let first_name = Mutil.nominative first_name in
    let surname = Mutil.nominative surname in
    let ipl = persons_of_name (first_name ^ " " ^ surname) in
    let first_name = Name.lower first_name in
    let surname = Name.lower surname in
    let rec find =
      function
        ip :: ipl ->
          let p = persons.get ip in
          if occ = p.occ &&
             first_name =
               Name.lower (strings.get p.first_name) &&
             surname = Name.lower (strings.get p.surname)
          then
            Some ip
          else find ipl
      | _ -> None
    in
    find ipl

let opendb bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let pending : patches_ht = empty_patch_ht () in
  fst pending.h_person := !(fst patches.h_person) ;
  fst pending.h_ascend := !(fst patches.h_ascend) ;
  fst pending.h_union := !(fst patches.h_union) ;
  fst pending.h_family := !(fst patches.h_family) ;
  fst pending.h_couple := !(fst patches.h_couple) ;
  fst pending.h_descend := !(fst patches.h_descend) ;
  fst pending.h_string := !(fst patches.h_string) ;
  let synchro = input_synchro bname in
  let particles =
    Mutil.input_particles (Filename.concat bname "particles.txt")
  in
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    Dutil.check_magic ic;
    ic
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
  let ic_acc =
    try Some (Secure.open_in_bin (Filename.concat bname "base.acc")) with
      Sys_error _ ->
        Printf.eprintf "File base.acc not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2 =
    try Some (Secure.open_in_bin (Filename.concat bname "strings.inx")) with
      Sys_error _ ->
        Printf.eprintf "File strings.inx not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2_string_start_pos = 3 * Dutil.int_size in
  let ic2_string_hash_len =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_surname_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_first_name_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let shift = 0 in
  let persons =
    make_record_access ic ic_acc shift persons_array_pos patches.h_person pending.h_person
      persons_len "persons" (input_value : _ -> person array)
      (Iovalue.input : _ -> person)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_record_access ic ic_acc shift ascends_array_pos patches.h_ascend pending.h_ascend
      persons_len "ascends" (input_value : _ -> ascend array)
      (Iovalue.input : _ -> ascend)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_record_access ic ic_acc shift unions_array_pos patches.h_union pending.h_union
      persons_len "unions" (input_value : _ -> union array)
      (Iovalue.input : _ -> union)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_record_access ic ic_acc shift families_array_pos patches.h_family pending.h_family
      families_len "families" (input_value : _ -> family array)
      (Iovalue.input : _ -> family)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_record_access ic ic_acc shift couples_array_pos patches.h_couple pending.h_couple
      families_len "couples" (input_value : _ -> couple array)
      (Iovalue.input : _ -> couple)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_record_access ic ic_acc shift descends_array_pos patches.h_descend pending.h_descend
      families_len "descends" (input_value : _ -> descend array)
      (Iovalue.input : _ -> descend)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_record_access ic ic_acc shift strings_array_pos patches.h_string pending.h_string
      strings_len "strings" (input_value : _ -> string array)
      (Iovalue.input : _ -> string)
  in
  let cleanup () =
    close_in ic;
    begin match ic_acc with
        Some ic_acc -> close_in ic_acc
      | None -> ()
    end;
    match ic2 with
      Some ic2 -> close_in ic2
    | None -> ()
  in
  let commit_synchro () =
    let tmp_fname = Filename.concat bname "1synchro_patches" in
    let fname = Filename.concat bname "synchro_patches" in
    let oc9 =
      try Secure.open_out_bin tmp_fname with
        Sys_error _ ->
          raise (Adef.Request_failure "the database is not writable")
    in
    let synchro =
      let timestamp = string_of_float (Unix.time ()) in
      let timestamp = String.sub timestamp 0 (String.index timestamp '.') in
      let v = timestamp, !synchro_person, !synchro_family in
      {synch_list = v :: synchro.synch_list}
    in
    Dutil.output_value_no_sharing oc9 (synchro : synchro_patch);
    close_out oc9;
    move_with_backup tmp_fname fname
  in
  let nbp_fname = Filename.concat bname "nb_persons" in
  let is_empty_name p =
    (0 = p.surname || 1 = p.surname)
    && (0 = p.first_name || 1 = p.first_name)
  in
  let npb_init () =
    let cnt = ref 0 in
    for i = 0 to persons.len - 1 do
      if not @@ is_empty_name @@ persons.get i then incr cnt
    done ;
    let oc = Secure.open_out_bin nbp_fname in
    output_value oc !cnt ;
    close_out oc ;
    !cnt
  in
  let nbp_read () =
    if Sys.file_exists nbp_fname
    then begin
      let ic = Secure.open_in_bin nbp_fname in
      let x : int = input_value ic in
      close_in ic ;
      x
    end else npb_init ()
  in
  let commit_patches () =
    let nbp =
      Hashtbl.fold begin fun ip p acc ->
        try
          match try Some (persons.get_nopending ip) with _ -> None with
          | Some old ->
            if is_empty_name old && not (is_empty_name p)
            then acc + 1
            else if not (is_empty_name old) && is_empty_name p
            then acc - 1
            else acc
          | None -> if not (is_empty_name p) then acc + 1 else acc
        with _ ->
          if not (is_empty_name p) then acc + 1
          else acc
      end (snd pending.h_person) (nbp_read ())
    in
    let tmp_nbp_fname = nbp_fname ^ "_tmp" in
    let oc = Secure.open_out_bin tmp_nbp_fname in
    output_value oc nbp ;
    close_out oc ;
    let aux (n, ht) (n', ht') =
      n := !n' ;
      Hashtbl.iter (Hashtbl.replace ht) ht' ;
      Hashtbl.clear ht' ;
    in
    aux patches.h_person pending.h_person ;
    aux patches.h_ascend pending.h_ascend ;
    aux patches.h_union pending.h_union ;
    aux patches.h_family pending.h_family ;
    aux patches.h_couple pending.h_couple ;
    aux patches.h_descend pending.h_descend ;
    aux patches.h_string pending.h_string ;
    let tmp_fname = Filename.concat bname "1patches" in
    let fname = Filename.concat bname "patches" in
    let oc9 = Secure.open_out_bin tmp_fname in
    output_string oc9 magic_patch;
    Dutil.output_value_no_sharing oc9 (patches : patches_ht);
    close_out oc9;
    move_with_backup tmp_nbp_fname nbp_fname ;
    move_with_backup tmp_fname fname ;
    commit_synchro ()
  in
  let patch_person i p =
    persons.len <- max persons.len (i + 1);
    fst pending.h_person := persons.len;
    Hashtbl.replace (snd pending.h_person) i p;
    synchro_person := i :: !synchro_person
  in
  let patch_ascend i a =
    ascends.len <- max ascends.len (i + 1);
    fst pending.h_ascend := ascends.len;
    Hashtbl.replace (snd pending.h_ascend) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_union i a =
    unions.len <- max unions.len (i + 1);
    fst pending.h_union := ascends.len;
    Hashtbl.replace (snd pending.h_union) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_family i f =
    families.len <- max families.len (i + 1);
    fst pending.h_family := families.len;
    Hashtbl.replace (snd pending.h_family) i f;
    synchro_family := i :: !synchro_family
  in
  let patch_couple i c =
    couples.len <- max couples.len (i + 1);
    fst pending.h_couple := couples.len;
    Hashtbl.replace (snd pending.h_couple) i c;
    synchro_family := i :: !synchro_family
  in
  let patch_descend i c =
    descends.len <- max descends.len (i + 1);
    fst pending.h_descend := descends.len;
    Hashtbl.replace (snd pending.h_descend) i c;
    synchro_family := i :: !synchro_family
  in
  let index_of_string =
    index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
      (snd patches.h_string) (snd pending.h_string)
  in
  let insert_string s =
    try index_of_string s with
      Not_found ->
        let i = strings.len in
        strings.len <- max strings.len (i + 1);
        fst pending.h_string := strings.len;
        Hashtbl.replace (snd pending.h_string) i s;
        i
  in
  let patch_name s ip =
    (* FIXME: pending patches? *)
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    try
      let ipl = Hashtbl.find patches.h_name i in
      if List.mem ip ipl then ()
      else Hashtbl.replace patches.h_name i (ip :: ipl)
    with Not_found -> Hashtbl.add patches.h_name i [ip]
  in
  let read_notes fnotes rn_mode =
    let fname =
      if fnotes = "" then "notes"
      else Filename.concat "notes_d" (fnotes ^ ".txt")
    in
    try
      let ic = Secure.open_in (Filename.concat bname fname) in
      let str =
        match rn_mode with
          RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> (try input_line ic with End_of_file -> "")
        | RnAll ->
          let rec loop len =
            match input_char ic with
            | exception End_of_file -> Buff.get len
            | c -> loop (Buff.store len c)
          in
          loop 0
      in
      close_in ic ;
      str
    with Sys_error _ -> ""
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
    else
      let oc = Secure.open_out fname in output_string oc s; close_out oc; ()
  in
  let ext_files () =
    let top = Filename.concat bname "notes_d" in
    let rec loop list subdir =
      let dir = Filename.concat top subdir in
      try
        let files = Sys.readdir dir in
        Array.fold_left
          (fun files file ->
             let f = Filename.concat subdir file in
             if Filename.check_suffix f ".txt" then
               Filename.chop_suffix f ".txt" :: files
             else loop files f)
          list files
      with Sys_error _ -> list
    in
    loop [] Filename.current_dir_name
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles = ext_files}
  in
  let base_data =
    { persons
    ; ascends
    ; unions
    ; visible = make_visible_record_access bname persons
    ; families
    ; couples
    ; descends
    ; strings
    ; particles
    ; bnotes
    ; bdir = bname
    }
  in
  let persons_of_name = persons_of_name bname patches.h_name in
  let base_func =
    { person_of_key = person_of_key persons strings persons_of_name
    ; persons_of_name = persons_of_name
    ; strings_of_fsname = strings_of_fsname bname strings patches.h_person
    ; persons_of_surname =
        persons_of_first_name_or_surname base_data strings
          (ic2, ic2_surname_start_pos, (fun p -> p.surname),
           snd patches.h_person, "snames.inx", "snames.dat", bname)
    ; persons_of_first_name =
        persons_of_first_name_or_surname base_data strings
          (ic2, ic2_first_name_start_pos, (fun p -> p.first_name),
           snd patches.h_person, "fnames.inx", "fnames.dat", bname)
    ; patch_person = patch_person; patch_ascend = patch_ascend
    ; patch_union = patch_union; patch_family = patch_family
    ; patch_couple = patch_couple; patch_descend = patch_descend
    ; patch_name = patch_name; insert_string = insert_string
    ; commit_patches = commit_patches
    ; commit_notes = commit_notes
    ; cleanup = cleanup
    ; nb_of_real_persons = nbp_read
    }
  in
  { data = base_data
  ; func = base_func }
