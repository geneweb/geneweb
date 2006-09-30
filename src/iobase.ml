(* $Id: iobase.ml,v 5.18 2006-09-30 12:15:00 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gutil;
open Gwdb;

value magic_gwb = "GnWb0020";
value magic_gwb_iso_8859_1 = "GnWb001y";

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
       2nd index offset : binary_int
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

(* Search index of a given string in file strings.inx *)

type name_index_data = array (array iper);
type strings_of_fsname = array (array istr);

value table_size = 0x3fff;

value string_piece s =
  let s = String.escaped s in
  if String.length s > 20 then
    String.sub s 0 10 ^ " ... " ^ String.sub s (String.length s - 10) 10
  else s
;

exception Found of int;

value hashtbl_right_assoc s ht =
  try
    do {
      Hashtbl.iter
        (fun i1 s1 -> if s = s1 then raise (Found i1) else ()) ht;
      raise Not_found;
    }
  with
  [ Found x -> x ]
;

value index_of_string strings ic start_pos hash_len string_patches s =
  try Adef.istr_of_int (hashtbl_right_assoc s string_patches) with
  [ Not_found ->
      match (ic, hash_len) with
      [ (Some ic, Some hash_len) ->
          let ia = Hashtbl.hash s mod hash_len in
          do {
            seek_in ic (start_pos + ia * int_size);
            let i1 = input_binary_int ic in
            let rec loop i =
              if i == -1 then raise Not_found
              else if strings.get i = s then Adef.istr_of_int i
              else do {
                seek_in ic (start_pos + (hash_len + i) * int_size);
                loop (input_binary_int ic)
              }
            in
            loop i1
          }
      | _ -> do {
          Printf.eprintf "Sorry. I really need string.inx\n";
          flush stderr;
          failwith "database access"
        } ] ]
;

(* Search index of a given surname or given first name in file strings.inx *)

value name_key s =
  let i = Gutil.initial s in
  let s =
    if i == 0 then s
    else String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
  in
  Name.lower s
;

value initial s =
  loop 0 where rec loop i =
    if i == String.length s then 0
    else
      match s.[i] with
      [ 'A'..'Z' | 'À'..'Ý' -> i
      | _ -> loop (succ i) ]
;

value unaccent =
  fun
  [ 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' | 'æ' -> 'a'
  | 'ç' -> 'c'
  | 'è' | 'é' | 'ê' | 'ë' -> 'e'
  | 'ì' | 'í' | 'î' | 'ï' -> 'i'
  | 'ð' -> 'd'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ý' | 'ÿ' -> 'y'
  | 'þ' -> 'p'
  | c -> c ]
;

value compare_names_1 s1 s2 =
  let compare_aux e1 e2 =
    loop where rec loop i1 i2 =
      if i1 == e1 && i2 == e2 then 0
      else if i1 == e1 then -1
      else if i2 == e2 then 1
      else
        let c1 = unaccent (Char.lowercase s1.[i1]) in
        let c2 = unaccent (Char.lowercase s2.[i2]) in
        match (c1, c2) with
        [ ('a'..'z', 'a'..'z') ->
            if c1 < c2 then -1
            else if c1 > c2 then 1
            else loop (i1 + 1) (i2 + 1)
        | ('a'..'z', _) -> 1
        | (_, 'a'..'z') -> -1
        | _ -> loop (i1 + 1) (i2 + 1) ]
  in
  if s1 = s2 then 0
  else
    let i1 = initial s1 in
    let i2 = initial s2 in
    match compare_aux (String.length s1) (String.length s2) i1 i2 with
    [ 0 -> compare_aux i1 i2 0 0
    | x -> x ]
;

value start_with ini s =
  loop 0 0 where rec loop i j =
    if i = String.length ini then True
    else if j = String.length s then False
    else if String.unsafe_get ini i = String.unsafe_get s j then
      loop (i + 1) (j + 1)
    else False
;

value get_particle s =
  loop where rec loop =
    fun
    [ [part :: parts] -> if start_with part s then part else loop parts
    | [] -> "" ]
;

value compare_part particles s1 s2 =
  let p1 = get_particle s1 particles in
  let p2 = get_particle s2 particles in
  loop (String.length p1) (String.length p2) where rec loop i1 i2 =
    if i1 = String.length s1 && i2 = String.length s2 then compare p1 p2
    else if i1 = String.length s1 then -1
    else if i2 = String.length s2 then 1
    else
      let c1 = String.unsafe_get s1 i1 in
      let c2 = String.unsafe_get s2 i2 in
      if c1 < c2 then -1
      else if c1 > c2 then 1
      else loop (i1 + 1) (i2 + 1)
;

value compare_names base_data s1 s2 =
  if utf_8_db.val then compare_part base_data.particles s1 s2
  else compare_names_1 s1 s2
;

value compare_istr_fun base_data is1 is2 =
  if is1 == is2 then 0
  else
    compare_names base_data (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))
;

value rec list_remove_elemq x =
  fun
  [ [y :: l] -> if x == y then l else [y :: list_remove_elemq x l]
  | [] -> [] ]
;

(* compatibility with databases created with versions <= 4.09 *)
(* should be removed after some time (when all databases will have
   been rebuilt with version >= 4.10 *)
value old_persons_of_first_name_or_surname base_data strings params =
  let (ic2, start_pos, proj, person_patches, _, _, _) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base_data; end)
  in
  let bt =
    let btr = ref None in
    let completed = ref False in
    let update_bt gistro bt =
      do {
        let bt = ref bt in
        Hashtbl.iter
          (fun i p ->
             let istr = proj p in
             if gistro <> None && gistro <> Some istr then ()
             else
               let ipera =
                 try IstrTree.find istr bt.val with [ Not_found -> [] ]
               in
               if List.memq (Adef.iper_of_int i) ipera then ()
               else
                 bt.val :=
                   IstrTree.add istr [Adef.iper_of_int i :: ipera] bt.val)
          person_patches;
        if gistro = None then completed.val := True else ();
        bt.val
      }
    in
    fun gistro ->
      match btr.val with
      [ Some bt ->
          if completed.val then bt
          else
            let bt = update_bt gistro bt in
            do { btr.val := Some bt; bt }
      | None ->
          match (ic2, start_pos) with
          [ (Some ic2, Some start_pos) -> do {
              seek_in ic2 start_pos;
(*
let ab1 = Gc.allocated_bytes () in
*)
              let bt : IstrTree.t (list iper) = input_value ic2 in
(*
let ab2 = Gc.allocated_bytes () in
*)
              let bt = update_bt gistro bt in
              btr.val := Some bt;
(*
Printf.eprintf "*** old database created by version <= 4.09\n"; flush stderr;
Printf.eprintf "*** using index allocating here %.0f bytes\n"
  (ab2 -. ab1);
flush stderr;
*)
              bt
            }
          | _ -> do {
              Printf.eprintf "Sorry, I really need strings.inx.\n";
              flush stderr;
              failwith "database access"
            } ] ]
  in
  let check_patches istr ipl =
    let ipl = ref ipl in
    do {
      Hashtbl.iter
        (fun i p ->
           if List.memq (Adef.iper_of_int i) ipl.val then
             if compare_istr_fun base_data istr (get_first_name p) == 0 ||
                compare_istr_fun base_data istr (get_surname p) == 0 then
               ()
             else ipl.val := list_remove_elemq (Adef.iper_of_int i) ipl.val
           else ())
        person_patches;
      ipl.val
    }
  in
  let find istr =
    try check_patches istr (IstrTree.find istr (bt (Some istr))) with
    [ Not_found -> [] ]
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
         compare_names base_data str (strings.get (Adef.int_of_istr key)))
      (bt None)
  in
  let next key = IstrTree.next key (bt None) in
  {find = find; cursor = cursor; next = next}
;

value new_persons_of_first_name_or_surname base_data strings params =
  let (_, _, proj, person_patches, names_inx, names_dat, bname) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base_data; end)
  in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    let btr = ref None in
    fun () ->
      match btr.val with
      [ Some bt -> bt
      | None ->
          do {
            let fname_inx = Filename.concat bname names_inx in
            let ic_inx = Secure.open_in_bin fname_inx in
(*
let ab1 = Gc.allocated_bytes () in
*)
            let bt : IstrTree.t int = input_value ic_inx in
(*
let ab2 = Gc.allocated_bytes () in
Printf.eprintf "*** new database created by version >= 4.10\n";
Printf.eprintf "*** using index '%s' allocating here only %.0f bytes\n"
  names_inx (ab2 -. ab1);
flush stderr;
*)
            close_in ic_inx;
            btr.val := Some bt;
            bt
          } ]
  in
  let find istr =
    let ipera =
      try
        let pos = IstrTree.find istr (bt ()) in
        let ic_dat = Secure.open_in_bin fname_dat in
        do {
          seek_in ic_dat pos;
          let len = input_binary_int ic_dat in
          let rec read_loop ipera len =
            if len = 0 then ipera
            else
              let iper = Adef.iper_of_int (input_binary_int ic_dat) in
              read_loop [iper :: ipera] (len - 1)
          in
          let ipera = read_loop [] len in
          close_in ic_dat;
          ipera
        }
      with
      [ Not_found -> [] ]
    in
    let ipera = ref ipera in
    do {
      Hashtbl.iter
        (fun i p ->
           let istr1 = proj p in
           if istr1 <> istr then ()
           else if List.memq (Adef.iper_of_int i) ipera.val then ()
           else ipera.val := [Adef.iper_of_int i :: ipera.val])
        person_patches;
      ipera.val
    }
  in
  let bt_patched =
    let btr = ref None in
    fun () ->
      match btr.val with
      [ Some bt -> bt
      | None ->
          let bt = ref (bt ()) in
          do {
            Hashtbl.iter
              (fun i p ->
                 let istr1 = proj p in
                 try
                   let _ = IstrTree.find istr1 bt.val in
                   ()
                 with
                 [ Not_found -> bt.val := IstrTree.add istr1 0 bt.val ])
              person_patches;
            btr.val := Some bt.val;
            bt.val
          } ]
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
         compare_names base_data str (strings.get (Adef.int_of_istr key)))
      (bt_patched ())
  in
  let next key = IstrTree.next key (bt_patched ()) in
  {find = find; cursor = cursor; next = next}
;

value persons_of_first_name_or_surname base_data strings params =
  let (_, _, _, _, names_inx, _, bname) = params in
  if Sys.file_exists (Filename.concat bname names_inx) then
    new_persons_of_first_name_or_surname base_data strings params
  else
    old_persons_of_first_name_or_surname base_data strings params
;

(* Search index for a given name in file names.inx *)

value persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =        
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          do {
            seek_in ic_inx_acc (Iovalue.sizeof_long * i);
            let pos = input_binary_int ic_inx_acc in
            close_in ic_inx_acc;
            seek_in ic_inx pos;
            (Iovalue.input ic_inx : array iper)
          }
        else (* compatibility *)
          let a =
            match t.val with
            [ Some a -> a
            | None ->
                do {
                  seek_in ic_inx int_size;
                  let a : name_index_data = input_value ic_inx in
                  t.val := Some a;
                  a
                } ]
          in
          a.(i)
      in
      do { close_in ic_inx; ai }          
    in
    try
      let l = Hashtbl.find patches i in
      l @ Array.to_list ai
    with
    [ Not_found -> Array.to_list ai ]
;

value strings_of_fsname bname strings (_, person_patches) =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          do {
            seek_in ic_inx_acc (Iovalue.sizeof_long * (table_size + i));
            let pos = input_binary_int ic_inx_acc in
            close_in ic_inx_acc;
            seek_in ic_inx pos;
            (Iovalue.input ic_inx : array istr)
          }
        else (* compatibility *)
          let a =
            match t.val with
            [ Some a -> a
            | None ->
                let pos = input_binary_int ic_inx in
                do {
                  seek_in ic_inx pos;
                  let a : strings_of_fsname = input_value ic_inx in
                  t.val := Some a;
                  a
                } ]
          in
          a.(i)
        in
        do { close_in ic_inx; ai }
    in
    let l = ref (Array.to_list r) in
    do {
      Hashtbl.iter
        (fun _ p ->
           do {
             if not (List.memq (get_first_name p) l.val) then
               let s1 = strings.get (Adef.int_of_istr (get_first_name p)) in
               let s1 = nominative s1 in
               if s = Name.crush_lower s1 then
                 l.val := [get_first_name p :: l.val]
               else ()
             else ();
             if not (List.memq (get_surname p) l.val) then
               let s1 = strings.get (Adef.int_of_istr (get_surname p)) in
               let s1 = nominative s1 in
               if s = Name.crush_lower s1 then
                 l.val := [get_surname p :: l.val]
               else ()
             else ();
           })
        person_patches;
      l.val
    }
;
(**)

value lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"
;

(* Restrict file *)

type visible_state = [ VsNone | VsTrue | VsFalse ];

value make_visible_record_access bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let read_or_create_visible () =
    let visible =
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic ->
          do {
            IFDEF UNIX THEN
              if verbose.val then do {
                Printf.eprintf "*** read restrict file\n";
                flush stderr;
              }
              else ()
            ELSE () END;
            let visible = input_value ic;
            close_in ic;
            visible
          }
      | None -> Array.create persons.len VsNone ]
    in
    do { visible_ref.val := Some visible; visible }
  in
  let v_write () =
    match visible_ref.val with
    [ Some visible ->
        try do {
          let oc = Secure.open_out fname;
          IFDEF UNIX THEN
            if verbose.val then do {
              Printf.eprintf "*** write restrict file\n";
              flush stderr;
            }
            else ()
          ELSE () END;
          output_value oc visible;
          close_out oc
        }
        with [ Sys_error _ -> () ]
    | None -> () ]
  in
  let v_get fct i =
    let visible =
      match visible_ref.val with
      [ Some visible -> visible
      | None -> read_or_create_visible () ]
    in
    if i < Array.length visible then
      match visible.(i) with
      [ VsNone ->
          let status = fct (persons.get i) in
          do {
            visible.(i) := if status then VsTrue else VsFalse;
            visible_ref.val := Some visible;
            status
          }
      | VsTrue -> True
      | VsFalse -> False ]
    else fct (persons.get i)
  in
  { v_write = v_write; v_get = v_get }
;

(* Input *)

value apply_patches tab f patches plen =
  if plen = 0 then tab
  else do {
    let new_tab =
      if plen > Array.length tab then do {
        let new_tab = Array.create plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab);
        new_tab
      }
      else tab
    in
    Hashtbl.iter (fun i v -> new_tab.(i) := f v) patches;
    new_tab
  }
;

(*
value rec patch_len len =
  fun
  [ [] -> len
  | [(i, _) :: l] -> patch_len (max len (i + 1)) l ]
;
*)

(* patches data; should be saved instead of Old.patches, but not done yet
   for backward compatibility; in case of major change, it can be done
   and module Old deleted; think of output_value_no_sharing which may
   not be good for this type (anyway I think not useful for the file
   patches) *)

type patches_ht =
  { h_person : (ref int * Hashtbl.t int person);
    h_ascend : (ref int * Hashtbl.t int ascend);
    h_union : (ref int * Hashtbl.t int union);
    h_family : (ref int * Hashtbl.t int family);
    h_couple : (ref int * Hashtbl.t int couple);
    h_descend : (ref int * Hashtbl.t int descend);
    h_string : (ref int * Hashtbl.t int string);
    h_name : (ref int * Hashtbl.t int (list iper)) }
;

module Old =
  struct
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
  end
;

value check_magic =
  let b = String.create (String.length magic_gwb) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      Gutil.utf_8_db.val := True;
      if b <> magic_gwb then
        if b = magic_gwb_iso_8859_1 then Gutil.utf_8_db.val := False
        else if String.sub magic_gwb 0 4 = String.sub b 0 4 then
          failwith "this is a GeneWeb base, but not compatible"
        else
          failwith "this is not a GeneWeb base, or it is a very old version"
      else ()
    }
;

value phony_person =
  {first_name = 0; surname = 0;
   occ = 0; image = 0; first_names_aliases = [];
   surnames_aliases = []; public_name = 0; qualifiers = [];
   aliases = []; titles = []; rparents = []; related = [];
   occupation = 0; sex = Neuter; access = IfTitles;
   birth = Adef.codate_None; birth_place = 0;
   birth_src = 0; baptism = Adef.codate_None;
   baptism_place = 0; baptism_src = 0;
   death = DontKnowIfDead; death_place = 0;
   death_src = 0; burial = UnknownBurial;
   burial_place = 0; burial_src = 0;
   notes = 0; psources = 0;
   key_index = Adef.iper_of_int 0}
;

value phony_family =
  {marriage = Adef.codate_None;
   marriage_place = 0; marriage_src = 0;
   witnesses = [| |]; relation = Married;
   divorce = NotDivorced; comment = 0;
   origin_file = 0; fsources = 0;
   fam_index = Adef.ifam_of_int 0}
;

value ext phony v =
  let rlen = Array.length (Obj.magic v) in
  let alen = Array.length (Obj.magic phony) in
  if rlen = alen then v
  else if rlen < alen then do {
    let x = Array.copy (Obj.magic phony) in
    Array.blit (Obj.magic v) 0 x 0 rlen;
    Obj.magic x
  }
  else
    failwith "this is a GeneWeb base, but not compatible; please upgrade"
;

value array_ext phony fa =
  let a = Obj.magic fa in
  if Array.length a = 0 then fa
  else
    let rlen = Array.length a.(0) in
    let alen = Array.length (Obj.magic phony) in
    if rlen = alen then fa
    else if rlen < alen then do {
      IFDEF UNIX THEN
        if verbose.val then do {
          Printf.eprintf
            "*** extending records from size %d to size %d\n" 
            rlen alen;
          flush stderr;
        }
        else ()
      ELSE () END;
      for i = 0 to Array.length a - 1 do {
        let x = Array.copy (Obj.magic phony) in
        Array.blit a.(i) 0 x 0 rlen;
        a.(i) := x;
      };
      fa
    }
    else
      failwith "this is a GeneWeb base, but not compatible; please upgrade"
;

value make_record_access ic ic_acc shift array_pos (plenr, patches) len name
  input_array input_item
=
  let v_ext v =
    if name = "persons" then ext phony_person v
    else if name = "families" then ext phony_family v
    else v
  in
  let v_arr_ext v =
    if name = "persons" then array_ext phony_person v
    else if name = "families" then array_ext phony_family v
    else v
  in
  let tab = ref None in
  let cleared = ref False in
  let gen_get i =
    match tab.val with
    [ Some x -> x.(i)
    | None ->
        try
          let v = Hashtbl.find patches i in
          v_ext v
        with
        [ Not_found ->
            if i < 0 || i >= len then
              failwith
                ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
            else
              match ic_acc with
              [ Some ic_acc -> do {
                  seek_in ic_acc (shift + Iovalue.sizeof_long * i);
                  let pos = input_binary_int ic_acc in
                  seek_in ic pos;
                  let v = input_item ic in
                  v_ext v
                }
              | None -> do {
                  Printf.eprintf "Sorry; I really need base.acc\n";
                  flush stderr;
                  failwith "cannot access database" } ] ] ]
  in
  let rec array () =
    match tab.val with
    [ Some x -> x
    | None -> do {
        IFDEF UNIX THEN
          if verbose.val then do {
            Printf.eprintf "*** read %s%s\n" name
              (if cleared.val then " (again)" else "");
            flush stderr;
          }
          else ()
        ELSE () END;
        seek_in ic array_pos;
        let v = input_array ic in
        let v = v_arr_ext v in
        let t = apply_patches v v_ext patches r.len in
        tab.val := Some t;
        t
      } ]
  and r =
    {load_array () = let _ = array () in (); get = gen_get;
     len = max len plenr.val; array_obj = array;
     clear_array () = do { cleared.val := True; tab.val := None }}
  in
  r
;

value input_patches bname =
  let patches =
    match
      try
        Some (Secure.open_in_bin (Filename.concat bname "patches"))
      with _ -> None
    with
    [ Some ic -> let p = input_value ic in do { close_in ic; p }
    | None ->
        {Old.p_person = ref []; Old.p_ascend = ref []; Old.p_union = ref [];
         Old.p_family = ref []; Old.p_couple = ref []; Old.p_descend = ref [];
         Old.p_string = ref []; Old.p_name = ref []} ]
  in
  let ht =
    {h_person = (ref 0, Hashtbl.create 101);
     h_ascend = (ref 0, Hashtbl.create 101);
     h_union = (ref 0, Hashtbl.create 101);
     h_family = (ref 0, Hashtbl.create 101);
     h_couple = (ref 0, Hashtbl.create 101);
     h_descend = (ref 0, Hashtbl.create 101);
     h_string = (ref 0, Hashtbl.create 101);
     h_name = (ref 0, Hashtbl.create 101)}
  in
  let add (ir, ht) (k, v) =
    do {
      if k >= ir.val then ir.val := k + 1 else ();
      Hashtbl.add ht k v;
    }
  in
  do {
    List.iter (add ht.h_person) patches.Old.p_person.val;
    List.iter (add ht.h_ascend) patches.Old.p_ascend.val;
    List.iter (add ht.h_union) patches.Old.p_union.val;
    List.iter (add ht.h_family) patches.Old.p_family.val;
    List.iter (add ht.h_couple) patches.Old.p_couple.val;
    List.iter (add ht.h_descend) patches.Old.p_descend.val;
    List.iter (add ht.h_string) patches.Old.p_string.val;
    List.iter (add ht.h_name) patches.Old.p_name.val;
    ht
   }
;

value patches_of_patches_ht patches =
  let p =
    {Old.p_person = ref [];
     Old.p_ascend = ref [];
     Old.p_union = ref [];
     Old.p_family = ref [];
     Old.p_couple = ref [];
     Old.p_descend = ref [];
     Old.p_string = ref [];
     Old.p_name = ref []}
  in
  let add r k v = r.val := [(k, v) :: r.val] in
  do {
    Hashtbl.iter (add p.Old.p_person) (snd patches.h_person);
    Hashtbl.iter (add p.Old.p_ascend) (snd patches.h_ascend);
    Hashtbl.iter (add p.Old.p_union) (snd patches.h_union);
    Hashtbl.iter (add p.Old.p_family) (snd patches.h_family);
    Hashtbl.iter (add p.Old.p_couple) (snd patches.h_couple);
    Hashtbl.iter (add p.Old.p_descend) (snd patches.h_descend);
    Hashtbl.iter (add p.Old.p_string) (snd patches.h_string);
    Hashtbl.iter (add p.Old.p_name) (snd patches.h_name);
    p
  }
;

value input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let particles =
    Gutil.input_particles (Filename.concat bname "particles.txt")
  in
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
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
  let ic_acc =
    try Some (Secure.open_in_bin (Filename.concat bname "base.acc")) with
    [ Sys_error _ -> do {
        Printf.eprintf "File base.acc not found; trying to continue...\n";
        flush stderr;
        None } ]
  in
  let ic2 =
    try Some (Secure.open_in_bin (Filename.concat bname "strings.inx")) with
    [ Sys_error _ -> do {
        Printf.eprintf "File strings.inx not found; trying to continue...\n";
        flush stderr;
        None } ]
  in
  let ic2_string_start_pos = 3 * int_size in
  let ic2_string_hash_len =
    match ic2 with
    [ Some ic2 -> Some (input_binary_int ic2)
    | None -> None ]
  in
  let ic2_surname_start_pos =
    match ic2 with
    [ Some ic2 -> Some (input_binary_int ic2)
    | None -> None ]
  in
  let ic2_first_name_start_pos =
    match ic2 with
    [ Some ic2 -> Some (input_binary_int ic2)
    | None -> None ]
  in
  let shift = 0 in
  let persons =
    make_record_access ic ic_acc shift persons_array_pos patches.h_person
      persons_len "persons" (input_value : _ -> array person)
      (Iovalue.input : _ -> person)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_record_access ic ic_acc shift ascends_array_pos patches.h_ascend
      persons_len "ascends" (input_value : _ -> array ascend)
      (Iovalue.input : _ -> ascend)

  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_record_access ic ic_acc shift unions_array_pos patches.h_union
      persons_len "unions" (input_value : _ -> array union)
      (Iovalue.input : _ -> union)

  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_record_access ic ic_acc shift families_array_pos patches.h_family
      families_len "families" (input_value : _ -> array family)
      (Iovalue.input : _ -> family)

  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_record_access ic ic_acc shift couples_array_pos patches.h_couple
      families_len "couples" (input_value : _ -> array couple)
      (Iovalue.input : _ -> couple)

  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_record_access ic ic_acc shift descends_array_pos patches.h_descend
      families_len "descends" (input_value : _ -> array descend)
      (Iovalue.input : _ -> descend)

  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_record_access ic ic_acc shift strings_array_pos patches.h_string
      strings_len "strings" (input_value : _ -> array string)
      (Iovalue.input : _ -> string)

  in
  let cleanup_ref =
    ref
      (fun () -> do {
         close_in ic;
         match ic_acc with
         [ Some ic_acc -> close_in ic_acc
         | None -> () ];
         match ic2 with
         [ Some ic2 -> close_in ic2
         | None -> () ];
       })
  in
  let cleanup () = cleanup_ref.val () in
  let commit_patches () =
    let tmp_fname = Filename.concat bname "1patches" in
    let fname = Filename.concat bname "patches" in
    do {
      let oc9 =
        try Secure.open_out_bin tmp_fname with
        [ Sys_error _ ->
            raise (Adef.Request_failure "the database is not writable") ]
      in
      let patches = patches_of_patches_ht patches in
      remove_file (fname ^ "~");
      output_value_no_sharing oc9 (patches : Old.patches);
      close_out oc9;
      try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
      try Sys.rename tmp_fname fname with [ Sys_error _ -> () ];
    }
  in
  let patched_ascends () =
    let r = ref [] in
    do {
      Hashtbl.iter (fun i _ -> r.val := [Adef.iper_of_int i :: r.val])
        (snd patches.h_ascend);
      r.val
    }
  in
  let is_patched_person ip =
    Hashtbl.mem (snd patches.h_person) (Adef.int_of_iper ip)
  in
  let patch_person i p =
    let i = Adef.int_of_iper i in
    do {
      persons.len := max persons.len (i + 1);
      (fst patches.h_person).val := persons.len;
      Hashtbl.replace (snd patches.h_person) i p;
    }
  in
  let patch_ascend i a =
    let i = Adef.int_of_iper i in
    do {
      ascends.len := max ascends.len (i + 1);
      (fst patches.h_ascend).val := ascends.len;
      Hashtbl.replace (snd patches.h_ascend) i a;
    }
  in
  let patch_union i a =
    let i = Adef.int_of_iper i in
    do {
      unions.len := max unions.len (i + 1);
      (fst patches.h_union).val := ascends.len;
      Hashtbl.replace (snd patches.h_union) i a;
    }
  in
  let patch_family i f =
    let i = Adef.int_of_ifam i in
    do {
      families.len := max families.len (i + 1);
      (fst patches.h_family).val := families.len;
      Hashtbl.replace (snd patches.h_family) i f;
    }
  in
  let patch_couple i c =
    let i = Adef.int_of_ifam i in
    do {
      couples.len := max couples.len (i + 1);
      (fst patches.h_couple).val := couples.len;
      Hashtbl.replace (snd patches.h_couple) i c;
    }
  in
  let patch_descend i c =
    let i = Adef.int_of_ifam i in
    do {
      descends.len := max descends.len (i + 1);
      (fst patches.h_descend).val := descends.len;
      Hashtbl.replace (snd patches.h_descend) i c;
    }
  in
  let patch_string i s =
    let i = Adef.int_of_istr i in
    do {
      strings.len := max strings.len (i + 1);
      (fst patches.h_string).val := strings.len;
      Hashtbl.replace (snd patches.h_string) i s;
    }
  in
  let patch_name s ip =
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    try
      let ipl = Hashtbl.find (snd patches.h_name) i in
      if List.memq ip ipl then ()
      else Hashtbl.replace (snd patches.h_name) i [ip :: ipl]
    with
    [ Not_found -> Hashtbl.add (snd patches.h_name) i [ip] ]
  in
  let read_notes fnotes rn_mode =
    let fname =
      if fnotes = "" then "notes"
      else Filename.concat "notes_d" (fnotes ^ ".txt")
    in
    match
      try Some (Secure.open_in (Filename.concat bname fname)) with
      [ Sys_error _ -> None ]
    with
    [ Some ic ->
        let len = ref 0 in
        do {
          try
            while True do {
              let c = input_char ic in
              len.val := Buff.store len.val c;
              match rn_mode with
              [ Rn1Ch -> raise Exit
              | Rn1Ln -> if c = '\n' then do { decr len; raise Exit } else ()
              | RnAll -> () ];
            }
          with
          [ Exit | End_of_file -> () ];
          close_in ic;
          Buff.get len.val
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
        let oc = Secure.open_out fname in output_string oc s; close_out oc; ()
      }
    }
  in
  let ext_files () =
    let top = Filename.concat bname "notes_d" in
    loop [] (Filename.current_dir_name) where rec loop list subdir =
      let dir = Filename.concat top subdir in
      match try Some (Sys.readdir dir) with [ Sys_error _ -> None ] with
      [ Some files ->
          List.fold_left
            (fun files file ->
               let f = Filename.concat subdir file in
               if Filename.check_suffix f ".txt" then
                 [Filename.chop_suffix f ".txt" :: files]
               else loop files f)
            list (Array.to_list files)
      | None -> list ]
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles = ext_files}
  in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible = make_visible_record_access bname persons;
     families = families; couples = couples; descends = descends;
     strings = strings; particles = particles; bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = persons_of_name bname (snd patches.h_name);
     strings_of_fsname = strings_of_fsname bname strings patches.h_person;
     index_of_string =
       index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
         (snd patches.h_string);
     persons_of_surname =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_surname_start_pos, get_surname, snd patches.h_person,
          "snames.inx", "snames.dat", bname);
     persons_of_first_name =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_first_name_start_pos, get_first_name,
          snd patches.h_person, "fnames.inx", "fnames.dat", bname);
     patch_person = patch_person; patch_ascend = patch_ascend;
     patch_union = patch_union; patch_family = patch_family;
     patch_couple = patch_couple; patch_descend = patch_descend;
     patch_string = patch_string; patch_name = patch_name;
     commit_patches = commit_patches;
     patched_ascends = patched_ascends;
     is_patched_person = is_patched_person;
     commit_notes = commit_notes; cleanup = cleanup}
  in
  {data = base_data; func = base_func}
;
