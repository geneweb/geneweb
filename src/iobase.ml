(* $Id: iobase.ml,v 2.3 1999-04-05 23:42:28 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;

value magic_gwb = "GnWb001s";

(*
 Files in base (directory .gwb)

    base - the base itself
       magic number (magic_gwb)                 : string of length 8
       number of persons                        : binary_int
       number of ascends (= number of persons)  : binary_int
       number of families                       : binary_int
       number of couples (= number of families) : binary_int
       number of strings                        : binary_int
       persons array offset in file             : binary_int
       ascends array offset in file             : binary_int
       families array offset in file            : binary_int
       couples array offset in file             : binary_int
       strings array offset in file             : binary_int
       persons array                            : value
       ascends array                            : value
       families array                           : value
       couples array                            : value
       strings array                            : value

    base.acc - direct accesses to arrays inside gwb
       persons offsets   : array of binary_ints
       ascends offsets   : array of binary_ints
       families offsets  : array of binary_ints
       couples offsets   : array of binary_ints
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

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       offset of surnames index           : binary_int
       offset of first names index        : binary_int
       strings hash table index           : 2 arrays of binary_ints
         strings offset array (length = prime after 10 * strings array length)
           - associating a hash value of the string modulo length
           - to his index in the string array
         strings list array (length = string array length)
           - associating a string index
           - to the index of the next index holding the same hash value
       surnames index                     : value
         binary tree
          - associating the string index of a surname
          - to the corresponding list of persons holding this surname
       first_names index                  : value
         binary tree
          - associating the string index of a first name
          - to the corresponding list of persons holding this first name

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".
*)

value remove_file f = try Sys.remove f with [ Sys_error _ -> () ];

value output_value_header_size = 20;
value output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]
;

value array_header_size arr =
  if Array.length arr < 8 then 1 else 5
;

value output_array_access oc arr pos =
  let rec loop pos i =
    if i == Array.length arr then pos
    else
      do output_binary_int oc pos; return
      loop (pos + Iovalue.size arr.(i)) (i + 1)
  in
  loop (pos + output_value_header_size + array_header_size arr) 0
;

value rec list_remove_assoc x =
  fun
  [ [(x1, y1) :: l] ->
      if x = x1 then l else [(x1, y1) :: list_remove_assoc x l]
  | [] -> [] ]
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

(* Search index of a given string in file strings.inx *)

value int_size = 4;

value string_piece s =
  let s = String.escaped s in
  if String.length s > 20 then
    String.sub s 0 10 ^ " ... " ^ String.sub s (String.length s - 10) 10
  else s
;

value rec list_right_assoc s =
  fun
  [ [(i1, s1) :: l] -> if s = s1 then i1 else list_right_assoc s l
  | [] -> raise Not_found ]
;

value index_of_string strings ic start_pos hash_len string_patches s =
  try Adef.istr_of_int (list_right_assoc s string_patches.val) with
  [ Not_found ->
      let ia = Hashtbl.hash s mod hash_len in
      do seek_in ic (start_pos + ia * int_size); return
      let i1 = input_binary_int ic in
      loop i1 where rec loop i =
        if i == -1 then raise Not_found
        else
          if strings.get i = s then Adef.istr_of_int i
          else
            do seek_in ic (start_pos + (hash_len + i) * int_size);
            return loop (input_binary_int ic) ]
;

(* Search index of a given surname or given first name in file strings.inx *)

value name_key s =
  let i = initiale s in
  let s =
    if i == 0 then s
    else String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
  in
  Name.lower s
;

(**)
value initial s =
  loop 0 where rec loop i =
    if i == String.length s then 0
    else
      match s.[i] with
      [ 'A'..'Z' -> i
      | _ -> loop (succ i) ]
;

value compare_names s1 s2 =
  let compare_aux e1 e2 =
    loop where rec loop i1 i2 =
      if i1 == e1 && i2 == e2 then 0
      else if i1 == e1 then -1
      else if i2 == e2 then 1
      else
        let c1 = Char.lowercase s1.[i1] in
        let c2 = Char.lowercase s2.[i2] in
        if Char.code c1 > 127 then loop (i1 + 1) i2
        else if Char.code c2 > 127 then loop i1 (i2 + 1)
        else
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
(*
value compare_names s1 s2 = compare (name_key s1) (name_key s2);
*)

value compare_istr_fun base_data is1 is2 =
  if is1 == is2 then 0
  else
    compare_names (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))
;

value rec list_remove_elemq x =
  fun
  [ [y :: l] -> if x == y then l else [y :: list_remove_elemq x l]
  | [] -> [] ]
;

value persons_of_first_name_or_surname base_data strings params =
  let (ic2, start_pos, proj, person_patches, tree_name) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base_data; end)
  in
  let bt =
    let btr = ref None in
    fun () ->
      match btr.val with
      [ Some bt -> bt
      | None ->
          do seek_in ic2 start_pos; return
          let bt : IstrTree.t (list iper) = input_value ic2 in
          let bt =
            List.fold_left
              (fun bt (i, p) ->
                 let istr = proj p in
                 let ipera =
                   try IstrTree.find istr bt with
                   [ Not_found -> [] ]
                 in
                 if List.memq p.cle_index ipera then bt
                 else IstrTree.add istr [p.cle_index :: ipera] bt)
              bt person_patches.val
          in
          do btr.val := Some bt; return bt ]
  in
  let check_patches istr ipl =
    List.fold_left
      (fun ipl (i, p) ->
         if List.memq (Adef.iper_of_int i) ipl then
           if compare_istr_fun base_data istr p.first_name == 0
           || compare_istr_fun base_data istr p.surname == 0
           then ipl
           else list_remove_elemq (Adef.iper_of_int i) ipl
         else ipl)
      ipl person_patches.val
  in
  let find istr =
    try check_patches istr (IstrTree.find istr (bt ())) with
    [ Not_found -> [] ]
  in
  let cursor str =
    IstrTree.key_after
      (fun key -> compare_names str (strings.get (Adef.int_of_istr key)))
      (bt ())
  in
  let next key = IstrTree.next key (bt ()) in  
  {find = find; cursor = cursor; next = next}
;

(* Search index for a given name in file names.inx *)

type name_index_data = array (array iper);

value persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let a =
      match t.val with
      [ Some a -> a
      | None ->
          let ic_inx = open_in_bin (Filename.concat bname "names.inx") in
          do seek_in ic_inx int_size; return
          let a = (input_value ic_inx : name_index_data) in
          do close_in ic_inx; t.val := Some a; return a ]
    in
    let i = Hashtbl.hash s in
    match patches.val with
    [ [] -> Array.to_list a.(i mod (Array.length a))
    | pl ->
        let l = try List.assoc i patches.val with [ Not_found -> [] ] in
        l @ Array.to_list a.(i mod (Array.length a)) ]
;

type strings_of_fsname = array (array istr);

value strings_of_fsname bname strings person_patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let a =
      match t.val with
      [ Some a -> a
      | None ->
          let ic_inx = open_in_bin (Filename.concat bname "names.inx") in
          let pos = input_binary_int ic_inx in
          do seek_in ic_inx pos; return
          let a = (input_value ic_inx : strings_of_fsname) in
          do close_in ic_inx; t.val := Some a; return a ]
    in
    let i = Hashtbl.hash s in
    let r = a.(i mod (Array.length a)) in
    match person_patches.val with
    [ [] -> Array.to_list r
    | _ ->
        let l =
          List.fold_left
            (fun l (_, p) ->
               let l =
                 if not (List.memq p.first_name l) then
                   let s1 = strings.get (Adef.int_of_istr p.first_name) in
                   if s = Name.crush_lower s1 then [p.first_name :: l] else l
                 else l
               in
               let l =
                 if not (List.memq p.surname l) then
                   let s1 = strings.get (Adef.int_of_istr p.surname) in
                   if s = Name.crush_lower s1 then [p.surname :: l] else l
                 else l
               in l)
            (Array.to_list r) person_patches.val
        in
        l ]
;

value lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"
;

(* Input *)

value apply_patches tab plist plen =
  if plist = [] then tab
  else
    let new_tab =
      if plen > Array.length tab then
        let new_tab = Array.create plen (Obj.magic 0) in
        do Array.blit tab 0 new_tab 0 (Array.length tab); return
        new_tab
      else tab
    in
    do List.iter (fun (i, v) -> new_tab.(i) := v) plist; return new_tab
;

value rec patch_len len =
  fun
  [ [] -> len
  | [(i, _) :: l] -> patch_len (max len (i + 1)) l ]
;

type patches =
  { p_person : ref (list (int * person));
    p_ascend : ref (list (int * ascend));
    p_family : ref (list (int * family));
    p_couple : ref (list (int * couple));
    p_string : ref (list (int * string));
    p_name : ref (list (int * list iper)) }
;

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

value make_cache ic ic_acc shift array_pos patches len name =
  let tab = ref None in
  let plen = patch_len len patches.val in
  let array () =
    match tab.val with
    [ Some x -> x
    | None ->
do ifdef UNIX then do Printf.eprintf "*** read %s\n" name; flush stderr; return () else (); return
        do seek_in ic array_pos; return
        let t = apply_patches (input_value ic) patches.val plen in
        do tab.val := Some t; return t ]
  in
  let r = {array = array; get = fun []; len = plen} in
  let gen_get i =
    if tab.val <> None then (r.array ()).(i)
    else
      try List.assoc i patches.val with
      [ Not_found ->
          if i < 0 || i >= len then
            failwith ("access " ^ name ^ " out of bounds")
          else
            do seek_in ic_acc (shift + Iovalue.sizeof_long * i); return
            let pos = input_binary_int ic_acc in
            do seek_in ic pos; return
            Iovalue.input ic ]
  in
  do r.get := gen_get; return r
;

value make_cached ic ic_acc shift array_pos patches len cache_htab name =
  let tab = ref None in
  let plen = patch_len len patches.val in
  let array () =
    match tab.val with
    [ Some x -> x
    | None ->
do ifdef UNIX then do Printf.eprintf "*** read %s\n" name; flush stderr; return () else (); return
        do seek_in ic array_pos; return
        let t = apply_patches (input_value ic) patches.val plen in
        do tab.val := Some t; return t ]
  in
  let r = {array = array; get = fun []; len = plen} in
  let gen_get i =
    if tab.val <> None then (r.array ()).(i)
    else
      try Hashtbl.find cache_htab i with
      [ Not_found ->
          let r =
            try List.assoc i patches.val with
            [ Not_found ->
                if i < 0 || i >= len then
                  failwith ("access " ^ name ^ " out of bounds")
                else
                  do seek_in ic_acc (shift + Iovalue.sizeof_long * i); return
                  let pos = input_binary_int ic_acc in
                  do seek_in ic pos; return
                  Iovalue.input ic ]
          in
          do Hashtbl.add cache_htab i r; return r ]
  in
  do r.get := gen_get; return r
;

value input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let patches =
    match
      try Some (open_in_bin (Filename.concat bname "patches")) with _ -> None
    with
    [ Some ic ->
        let p = input_value ic in
        do close_in ic; return p
    | None ->
        {p_person = ref []; p_ascend = ref []; p_family = ref [];
         p_couple = ref []; p_string = ref []; p_name = ref []} ]
  in
  let ic =
    let ic = open_in_bin (Filename.concat bname "base") in
    do check_magic ic; return ic
  in
  let ic_acc = open_in_bin (Filename.concat bname "base.acc") in
  let ic2 = open_in_bin (Filename.concat bname "strings.inx") in
  let persons_len = input_binary_int ic in
  let ascends_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let couples_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let ic2_string_start_pos = 3 * int_size in
  let ic2_string_hash_len = input_binary_int ic2 in
  let ic2_surname_start_pos = input_binary_int ic2 in
  let ic2_first_name_start_pos = input_binary_int ic2 in
  let shift = 0 in
  let persons =
    make_cache ic ic_acc shift persons_array_pos patches.p_person persons_len
      "persons"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_cache ic ic_acc shift ascends_array_pos patches.p_ascend ascends_len
      "ascends"
  in
  let shift = shift + ascends_len * Iovalue.sizeof_long in
  let families =
    make_cache ic ic_acc shift families_array_pos patches.p_family
      families_len "families"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_cache ic ic_acc shift couples_array_pos patches.p_couple couples_len
      "couples"
  in
  let shift = shift + couples_len * Iovalue.sizeof_long in
  let strings_cache = Hashtbl.create 101 in
  let strings =
    make_cached ic ic_acc shift strings_array_pos patches.p_string strings_len
      strings_cache "strings"
  in
  let cleanup () =
    do close_in ic; close_in ic_acc; close_in ic2; return ()
  in
  let commit_patches () =
    let fname = Filename.concat bname "patches" in
    do try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
       try Sys.rename fname (fname ^ "~") with _ -> ();
    return
    let oc9 = open_out_bin fname in
    do output_value_no_sharing oc9 patches;
       close_out oc9;
    return ()
  in
  let patch_person i p =
    let i = Adef.int_of_iper i in
    do persons.len := max persons.len (i + 1);
       patches.p_person.val :=
         [(i, p) :: list_remove_assoc i patches.p_person.val];
    return ()
  in
  let patch_ascend i a =
    let i = Adef.int_of_iper i in
    do ascends.len := max ascends.len (i + 1);
       patches.p_ascend.val :=
         [(i, a) :: list_remove_assoc i patches.p_ascend.val];
    return ()
  in
  let patch_family i f =
    let i = Adef.int_of_ifam i in
    do families.len := max families.len (i + 1);
       patches.p_family.val :=
         [(i, f) :: list_remove_assoc i patches.p_family.val];
    return ()
  in
  let patch_couple i c =
    let i = Adef.int_of_ifam i in
    do couples.len := max couples.len (i + 1);
       patches.p_couple.val :=
         [(i, c) :: list_remove_assoc i patches.p_couple.val];
    return ()
  in
  let patch_string i s =
    let i = Adef.int_of_istr i in
    do strings.len := max strings.len (i + 1);
       patches.p_string.val :=
         [(i, s) :: list_remove_assoc i patches.p_string.val];
       Hashtbl.add strings_cache i s;
    return ()
  in
  let patch_name s ip =
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let (ipl, name_patches_rest) =
      find patches.p_name.val where rec find =
        fun
        [ [] -> ([], [])
        | [(i1, ipl1) :: l] ->
            if i = i1 then (ipl1, l)
            else let (ipl, l) = find l in (ipl, [(i1, ipl1) :: l]) ]
    in
    if List.memq ip ipl then ()
    else patches.p_name.val := [(i, [ip :: ipl]) :: name_patches_rest]
  in
  let base_data =
    {persons = persons;
     ascends = ascends;
     families = families;
     couples = couples;
     strings = strings;
     has_family_patches =
       patches.p_family.val <> [] || patches.p_couple.val <> []}
  in
  let base_func =
    {persons_of_name = persons_of_name bname patches.p_name;
     strings_of_fsname = strings_of_fsname bname strings patches.p_person;
     index_of_string =
       index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
         patches.p_string;
     persons_of_surname =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_surname_start_pos, fun p -> p.surname, patches.p_person,
          "surname");
     persons_of_first_name =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_first_name_start_pos, fun p -> p.first_name,
          patches.p_person, "first_name");
     patch_person = patch_person;
     patch_ascend = patch_ascend;
     patch_family = patch_family;
     patch_couple = patch_couple;
     patch_string = patch_string;
     patch_name = patch_name;
     commit_patches = commit_patches; cleanup = cleanup}
  in
  {data = base_data; func = base_func}
;

(* Output *)

value is_prime a =
  loop 2 where rec loop b =
    if a / b < b then True
    else if a mod b == 0 then False
    else loop (b + 1)
;

value rec prime_after n =
  if is_prime n then n else prime_after (n + 1)
;

value output_strings_hash oc2 base =
  let strings_array = base.data.strings.array () in
  let taba =
    Array.create (prime_after (max 2 (10 * Array.length strings_array))) (-1)
  in
  let tabl = Array.create (Array.length strings_array) (-1) in
  do for i = 0 to Array.length strings_array - 1 do
       let ia = Hashtbl.hash (strings_array.(i)) mod (Array.length taba) in
       do tabl.(i) := taba.(ia);
          taba.(ia) := i;
       return ();
     done;
  return
  do output_binary_int oc2 (Array.length taba);
     output_binary_int oc2 0;
     output_binary_int oc2 0;
     for i = 0 to Array.length taba - 1 do
       output_binary_int oc2 taba.(i);
     done;
     for i = 0 to Array.length tabl - 1 do
       output_binary_int oc2 tabl.(i);
     done;
  return ()
;

value output_surname_index oc2 base =
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base.data; end)
  in
  let bt = ref IstrTree.empty in
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       let a =
         try IstrTree.find p.surname bt.val with
         [ Not_found -> [] ]
       in
       bt.val := IstrTree.add p.surname [ p.cle_index :: a] bt.val;
     done;
     output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
  return ()
;

value output_first_name_index oc2 base =
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base.data; end)
  in
  let bt = ref IstrTree.empty in
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       let a =
         try IstrTree.find p.first_name bt.val with
         [ Not_found -> [] ]
       in
       bt.val := IstrTree.add p.first_name [ p.cle_index :: a] bt.val;
     done;
     output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
  return ()
;

value table_size = 0x3fff;
value make_name_index base =
  let t = Array.create table_size [| |] in
  let a = base.data.persons.array () in
  let add_name key valu =
    let i = Hashtbl.hash (Name.crush (Name.abbrev key)) mod (Array.length t) in
    if array_memq valu t.(i) then ()
    else t.(i) := Array.append [| valu |] t.(i)
  in
  let rec add_names ip =
    fun
    [ [] -> ()
    | [n :: nl] -> do add_name n ip; return add_names ip nl ]
  in
  do for i = 0 to Array.length a - 1 do
       let p = base.data.persons.get i in
       let first_name = sou base p.first_name in
       let surname = sou base p.surname in
       if first_name <> "?" && surname <> "?" then
         let names =
           [Name.lower (first_name ^ " " ^ surname) ::
            person_misc_names base p]
         in
         add_names p.cle_index names
       else ();       
     done;
  return t
;

value create_name_index oc_inx base =
  let ni = make_name_index base in
  output_value_no_sharing oc_inx (ni : name_index_data)
;

value add_name t key valu =
  let i = Hashtbl.hash (Name.crush_lower key) mod (Array.length t) in
  if array_memq valu t.(i) then ()
  else t.(i) := Array.append [| valu |] t.(i)
;

value make_strings_of_fsname base =
  let t = Array.create table_size [||] in
  let a = base.data.persons.array () in
  do for i = 0 to Array.length a - 1 do
       let p = base.data.persons.get i in
       let first_name = sou base p.first_name in
       let surname = sou base p.surname in
       do if first_name <> "?" then add_name t first_name p.first_name
          else ();
          if surname <> "?" then add_name t surname p.surname
          else ();
       return ();
     done;
  return t
;

value create_strings_of_fsname oc_inx base =
  let t = make_strings_of_fsname base in
  output_value_no_sharing oc_inx (t : strings_of_fsname)
;

value count_error computed found =
  do Printf.eprintf "Count error. Computed %d. Found %d.\n" computed found;
     flush stderr;
  return exit 2
;

value output bname base =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  do try Unix.mkdir bname 0o755 with _ -> (); return
  let tmp_fname = Filename.concat bname "1base" in
  let tmp_fname_acc = Filename.concat bname "1base.acc" in
  let tmp_fname_inx = Filename.concat bname "1names.inx" in
  let tmp_fname_gw2 = Filename.concat bname "1strings.inx" in
  let _ = base.data.persons.array () in
  let _ = base.data.ascends.array () in
  let _ = base.data.families.array () in
  let _ = base.data.couples.array () in
  let _ = base.data.strings.array () in
  do base.func.cleanup (); return
  let oc = open_out_bin tmp_fname in
  let oc_acc = open_out_bin tmp_fname_acc in
  let oc_inx = open_out_bin tmp_fname_inx in
  let oc2 = open_out_bin tmp_fname_gw2 in
  let output_array arr =
    let bpos = pos_out oc in
    do output_value_no_sharing oc arr; return
    let epos = output_array_access oc_acc arr bpos in
    if epos <> pos_out oc then count_error epos (pos_out oc) else ()
  in
  try
    do output_string oc magic_gwb;
       output_binary_int oc base.data.persons.len;
       output_binary_int oc base.data.ascends.len;
       output_binary_int oc base.data.families.len;
       output_binary_int oc base.data.couples.len;
       output_binary_int oc base.data.strings.len;
    return
    let array_start_indexes = pos_out oc in
    do output_binary_int oc 0;
       output_binary_int oc 0;
       output_binary_int oc 0;
       output_binary_int oc 0;
       output_binary_int oc 0;
    return
    let persons_array_pos = pos_out oc in
    do output_array (base.data.persons.array ()); return
    let ascends_array_pos = pos_out oc in
    do output_array (base.data.ascends.array ()); return
    let families_array_pos = pos_out oc in
    do output_array (base.data.families.array ()); return
    let couples_array_pos = pos_out oc in
    do output_array (base.data.couples.array ()); return
    let strings_array_pos = pos_out oc in
    do output_array (base.data.strings.array ());
       seek_out oc array_start_indexes;
       output_binary_int oc persons_array_pos;
       output_binary_int oc ascends_array_pos;
       output_binary_int oc families_array_pos;
       output_binary_int oc couples_array_pos;
       output_binary_int oc strings_array_pos;
       close_out oc;
       close_out oc_acc;
do Printf.eprintf "*** create name index\n"; flush stderr; return
       output_binary_int oc_inx 0;
       create_name_index oc_inx base;
       let surname_or_first_name_pos = pos_out oc_inx in
do Printf.eprintf "*** create strings of fsname\n"; flush stderr; return
       do create_strings_of_fsname oc_inx base;
          seek_out oc_inx 0;
          output_binary_int oc_inx surname_or_first_name_pos;
          close_out oc_inx;
       return ();
do Printf.eprintf "*** create string index\n"; flush stderr; return
       output_strings_hash oc2 base;
       let surname_pos = pos_out oc2 in
do Printf.eprintf "*** create surname index\n"; flush stderr; return
       do output_surname_index oc2 base; return
       let first_name_pos = pos_out oc2 in
do Printf.eprintf "*** create first name index\n"; flush stderr; return
       do output_first_name_index oc2 base;
          seek_out oc2 int_size;
          output_binary_int oc2 surname_pos;
          output_binary_int oc2 first_name_pos;
       return ();
do Printf.eprintf "*** ok\n"; flush stderr; return
       close_out oc2;
       remove_file (Filename.concat bname "base");
       Sys.rename tmp_fname (Filename.concat bname "base");
       remove_file (Filename.concat bname "base.acc");
       Sys.rename tmp_fname_acc (Filename.concat bname "base.acc");
       remove_file (Filename.concat bname "names.inx");
       Sys.rename tmp_fname_inx (Filename.concat bname "names.inx");
       remove_file (Filename.concat bname "strings.inx");
       Sys.rename tmp_fname_gw2 (Filename.concat bname "strings.inx");
       remove_file (Filename.concat bname "patches");
       remove_file (Filename.concat bname "tstab");
    return ()
  with e ->
    do try close_out oc with _ -> ();
       try close_out oc_acc with _ -> ();
       try close_out oc_inx with _ -> ();
       try close_out oc2 with _ -> ();
       remove_file tmp_fname;
       remove_file tmp_fname_acc;
       remove_file tmp_fname_inx;
       remove_file tmp_fname_gw2;
    return raise e
;
