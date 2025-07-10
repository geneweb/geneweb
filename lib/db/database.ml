(* Copyright (c) 1998-2007 INRIA *)

open Dbdisk
open Def
module Logs = Geneweb_logs.Logs

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

let move_with_backup src dst =
  Mutil.rm (dst ^ "~");
  Mutil.mv dst (dst ^ "~");
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
       offset to sindex : binary_int
       offset to findex : binary_int
       1st index (mixes between names) : value
         array, length = "table_size", associating:
          - a hash value of a "crushed" (module "Name") name (modulo length)
          - to the array of indexes of the corresponding persons
       2nd index (surnames sub-strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") surname sub-string
           (modulo length)
          - to the array of the corresponding surnames (string indexes) that contains
            giving surname substring
       3rd index (first name sub-strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") first name sub-string
          (modulo length)
          - to the array of the corresponding string indexes that contains
            giving fiest name substring

    names.acc - direct accesses to arrays inside names.inx

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       strings hash table index           : 2 arrays of binary_ints
         strings offset array (length = prime after 10 * strings array length)
           - associating a hash value of the string modulo length
           - to its index in the string array
         strings list array (length = string array length)
           - associating a string index
           - to the index of the next index (previus value) holding the same hash value

    snames.inx - index for surnames
       array ordered by surname
        - associating the string index of a surname
        - to a pointer (int) to snames.dat

    snames.dat - data associated with snames.inx
      table of list of persons holding a surname

    fnames.inx - index for first names
       array ordered by first name
        - associating the string index of a first name
        - to a pointer (int) to fnames.dat

    fnames.dat - data associated with fnames.inx
      table of list of persons holding a first name

    notes - text file containing data base notes.

    notes_d - directory containing .txt for each extended page

    particles.txt - text file with autorised name's particles

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".

    nb_persons - number of real persons (with those added by patches)

    synchro_patches - timestamped history of base's modification.

    restrict - defines visibility of each person in the base
*)

(* deprecated version using the old btree implementation.
   It will be kept for a while, but it will eventually be remove.
   Rebuilding the indexes with new implementation is as simple as
   running `gwfixbase -index /path/to/base.gwb`
*)
let old_persons_of_first_name_or_surname base_data params =
  let proj, person_patches, names_inx, names_dat, bname = params in
  let module IstrTree = Avl.Make (struct
    type t = int

    let compare = Dutil.compare_snames_i base_data
  end) in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    let btr = ref None in
    fun () ->
      match !btr with
      | Some bt -> bt
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
        let ipera = read_loop [] len in
        close_in ic_dat;
        ipera
      with Not_found -> []
    in
    let ipera = ref ipera in
    Hashtbl.iter
      (fun i p ->
        let istr1 = proj p in
        if istr1 = istr && not (List.mem i !ipera) then ipera := i :: !ipera)
      person_patches;
    !ipera
  in
  let bt_patched =
    let btr = ref None in
    fun () ->
      match !btr with
      | Some bt -> bt
      | None ->
          let bt = ref (bt ()) in
          Hashtbl.iter
            (fun _i p ->
              let istr1 = proj p in
              if not @@ IstrTree.mem istr1 !bt then
                bt := IstrTree.add istr1 0 !bt)
            person_patches;
          btr := Some !bt;
          !bt
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
        Dutil.compare_snames base_data str (base_data.strings.get key))
      (bt_patched ())
  in
  let next key = IstrTree.next key (bt_patched ()) in
  { find; cursor; next }

let binary_search arr cmp =
  if arr = [||] then raise Not_found;
  let rec aux low high =
    if high <= low then if cmp arr.(low) = 0 then low else raise Not_found
    else
      let mid = (low + high) / 2 in
      let c = cmp arr.(mid) in
      if c < 0 then aux low (mid - 1)
      else if c > 0 then aux (mid + 1) high
      else mid
  in
  aux 0 (Array.length arr - 1)

let binary_search_key_after arr cmp =
  if arr = [||] then raise Not_found;
  let rec aux acc low high =
    if high <= low then
      if cmp arr.(low) <= 0 then low
      else match acc with Some x -> x | None -> raise Not_found
    else
      let mid = (low + high) / 2 in
      let c = cmp arr.(mid) in
      if c < 0 then aux (Some mid) low (mid - 1)
      else if c > 0 then aux acc (mid + 1) high
      else mid
  in
  aux None 0 (Array.length arr - 1)

let binary_search_next arr cmp =
  if arr = [||] then raise Not_found;
  let rec aux acc low high =
    if high <= low then
      if cmp arr.(low) < 0 then low
      else match acc with Some x -> x | None -> raise Not_found
    else
      let mid = (low + high) / 2 in
      let c = cmp arr.(mid) in
      if c < 0 then aux (Some mid) low (mid - 1) else aux acc (mid + 1) high
  in
  aux None 0 (Array.length arr - 1)

let new_persons_of_first_name_or_surname cmp_str cmp_istr base_data params =
  let proj, person_patches, names_inx, names_dat, bname = params in
  let fname_dat = Filename.concat bname names_dat in
  (* content of "snames.inx" *)
  let bt =
    lazy
      (let fname_inx = Filename.concat bname names_inx in
       let ic_inx = Secure.open_in_bin fname_inx in
       let bt : (int * int) array = input_value ic_inx in
       close_in ic_inx;
       bt)
  in
  (* ordered by string name's ids attached to the patched persons *)
  let patched =
    (* This is not useful to keep the list of ipers here because [patched]
       is not used by [find] but only by [cursor] and [next] *)
    lazy
      (let ht = Dutil.IntHT.create 0 in
       Hashtbl.iter
         (fun _ p ->
           let k = proj p in
           if not @@ Dutil.IntHT.mem ht k then Dutil.IntHT.add ht k [])
         person_patches;
       let a = Array.make (Dutil.IntHT.length ht) (0, []) in
       ignore
       @@ Dutil.IntHT.fold
            (fun k v i ->
              Array.set a i (k, v);
              succ i)
            ht 0;
       Array.sort (fun (k, _) (k', _) -> cmp_istr base_data k k') a;
       a)
  in
  let find istr =
    let ipera =
      try
        let bt = Lazy.force bt in
        let s = base_data.strings.get istr in
        let cmp (k, _) =
          if k = istr then 0 else cmp_str base_data s (base_data.strings.get k)
        in
        let pos = snd @@ bt.(binary_search bt cmp) in
        let ic_dat = Secure.open_in_bin fname_dat in
        seek_in ic_dat pos;
        let len = input_binary_int ic_dat in
        let rec read_loop ipera len =
          if len = 0 then ipera
          else
            let iper = input_binary_int ic_dat in
            read_loop (iper :: ipera) (len - 1)
        in
        let ipera = read_loop [] len in
        close_in ic_dat;
        ipera
      with Not_found -> []
    in
    let patched = Hashtbl.fold (fun i _ acc -> i :: acc) person_patches [] in
    let ipera = List.filter (fun i -> not @@ List.mem i patched) ipera in
    Hashtbl.fold
      (fun i p acc ->
        let istr1 = proj p in
        if istr1 = istr then if List.mem i acc then acc else i :: acc else acc)
      person_patches ipera
  in
  let cursor str =
    let bt = Lazy.force bt in
    let patched = Lazy.force patched in
    let cmp (k, _) = cmp_str base_data str (base_data.strings.get k) in
    let istr1 =
      try fst bt.(binary_search_key_after bt cmp) with Not_found -> -1
    in
    let istr2 =
      try fst patched.(binary_search_key_after patched cmp)
      with Not_found -> -1
    in
    if istr2 = -1 then if istr1 = -1 then raise Not_found else istr1
    else if istr1 = -1 then istr2
    else if istr1 = istr2 then istr1
    else
      let c =
        cmp_str base_data
          (base_data.strings.get istr1)
          (base_data.strings.get istr2)
      in
      if c < 0 then istr1 else istr2
  in
  let next istr =
    let bt = Lazy.force bt in
    let patched = Lazy.force patched in
    let s = base_data.strings.get istr in
    let cmp (k, _) =
      if k = istr then 0 else cmp_str base_data s (base_data.strings.get k)
    in
    let istr1 = try fst bt.(binary_search_next bt cmp) with Not_found -> -1 in
    let istr2 =
      try fst patched.(binary_search_next patched cmp) with Not_found -> -1
    in
    if istr2 = -1 then if istr1 = -1 then raise Not_found else istr1
    else if istr1 = -1 then istr2
    else if istr1 = istr2 then istr1
    else
      let c =
        cmp_str base_data
          (base_data.strings.get istr1)
          (base_data.strings.get istr2)
      in
      if c < 0 then istr1 else istr2
  in
  { find; cursor; next }

let persons_of_first_name = function
  | GnWb0024 ->
      new_persons_of_first_name_or_surname
        (fun _ -> Dutil.compare_fnames)
        Dutil.compare_fnames_i
  | GnWb0023 | GnWb0022 | GnWb0021 ->
      new_persons_of_first_name_or_surname Dutil.compare_snames
        Dutil.compare_snames_i
  | GnWb0020 -> old_persons_of_first_name_or_surname

let persons_of_surname = function
  | GnWb0024 | GnWb0023 | GnWb0022 | GnWb0021 ->
      new_persons_of_first_name_or_surname Dutil.compare_snames
        Dutil.compare_snames_i
  | GnWb0020 -> old_persons_of_first_name_or_surname

(* Search index for a given name in file names.inx *)

let persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let i = Dutil.name_index s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then (
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * i);
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : int array))
        else
          let a =
            match !t with
            | Some a -> a
            | None ->
                seek_in ic_inx Dutil.int_size;
                let a : Dutil.name_index_data = input_value ic_inx in
                t := Some a;
                a
          in
          a.(i)
      in
      close_in ic_inx;
      ai
    in
    match Hashtbl.find patches i with
    | patches ->
        List.fold_left
          (fun acc ip -> if Array.mem ip ai then acc else ip :: acc)
          (Array.to_list ai) patches
    | exception Not_found -> Array.to_list ai

let old_strings_of_fsname bname strings (_, person_patches) =
  let t = ref None in
  fun s ->
    let i = Dutil.name_index s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then (
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * (Dutil.table_size + i));
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : int array))
        else
          let a =
            match !t with
            | Some a -> a
            | None ->
                let pos = input_binary_int ic_inx in
                seek_in ic_inx pos;
                let a : Dutil.strings_of_fsname = input_value ic_inx in
                t := Some a;
                a
          in
          a.(i)
      in
      close_in ic_inx;
      ai
    in
    Hashtbl.fold
      (fun _ p acc ->
        let aux split acc istr =
          let str = strings.get istr in
          if
            (not (List.mem istr acc))
            &&
            match split str with
            | [ s ] -> i = Dutil.name_index s
            | list -> List.exists (fun s -> i = Dutil.name_index s) (str :: list)
          then istr :: acc
          else acc
        in
        let acc = aux Name.split_fname acc p.first_name in
        let acc = aux Name.split_sname acc p.surname in
        acc)
      person_patches (Array.to_list r)
(**)

(** offset: 1 pour sname 2 pour fname *)
let new_strings_of_fsname_aux offset_acc offset_inx split get bname strings
    (_, person_patches) =
  let t = ref None in
  fun s ->
    let i = Dutil.name_index s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then (
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc
            (Iovalue.sizeof_long * ((offset_acc * Dutil.table_size) + i));
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : int array))
        else
          let a =
            match !t with
            | Some a -> a
            | None ->
                seek_in ic_inx offset_inx;
                let pos = input_binary_int ic_inx in
                seek_in ic_inx pos;
                let a : Dutil.strings_of_fsname = input_value ic_inx in
                t := Some a;
                a
          in
          a.(i)
      in
      close_in ic_inx;
      ai
    in
    Hashtbl.fold
      (fun _ p acc ->
        let istr = get p in
        let str = strings.get istr in
        if
          (not (List.mem istr acc))
          &&
          match split str with
          | [ s ] -> i = Dutil.name_index s
          | list -> List.exists (fun s -> i = Dutil.name_index s) (str :: list)
        then istr :: acc
        else acc)
      person_patches (Array.to_list r)

let new_strings_of_sname =
  new_strings_of_fsname_aux 1 0 Name.split_sname (fun p -> p.surname)

let new_strings_of_fname =
  new_strings_of_fsname_aux 2 1 Name.split_fname (fun p -> p.first_name)

let strings_of_sname = function
  | GnWb0024 | GnWb0023 -> new_strings_of_sname
  | _ -> old_strings_of_fsname

let strings_of_fname = function
  | GnWb0024 | GnWb0023 -> new_strings_of_fname
  | _ -> old_strings_of_fsname

(* Restrict file *)

type visible_state = VsNone | VsTrue | VsFalse

let verbose = Mutil.verbose

let make_visible_record_access perm bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let read_or_create_visible () =
    let visible =
      try
        let ic = Secure.open_in fname in
        if Sys.unix && !verbose then (
          Printf.eprintf "*** read restrict file\n";
          flush stderr);
        let visible = input_value ic in
        close_in ic;
        visible
      with Sys_error _ -> Array.make persons.len VsNone
    in
    visible_ref := Some visible;
    visible
  in
  let v_write () =
    match !visible_ref with
    | Some visible ->
        if perm = RDONLY then raise (HttpExn (Forbidden, __LOC__))
        else
          let oc = Secure.open_out fname in
          if Sys.unix && !verbose then (
            Printf.eprintf "*** write restrict file\n";
            flush stderr);
          output_value oc visible;
          close_out oc
    | None -> ()
  in
  let v_get fct i =
    let visible =
      match !visible_ref with
      | Some visible -> visible
      | None -> read_or_create_visible ()
    in
    if i < Array.length visible then
      match visible.(i) with
      | VsNone ->
          let status = fct (persons.get i) in
          visible.(i) <- (if status then VsTrue else VsFalse);
          visible_ref := Some visible;
          status
      | VsTrue -> true
      | VsFalse -> false
    else fct (persons.get i)
  in
  { v_write; v_get }

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

type synchro_patch = {
  mutable synch_list : (string * int list * int list) list;
}

(* Input *)

type patches_ht = {
  h_person : int ref * (int, person) Hashtbl.t;
  h_ascend : int ref * (int, ascend) Hashtbl.t;
  h_union : int ref * (int, union) Hashtbl.t;
  h_family : int ref * (int, family) Hashtbl.t;
  h_couple : int ref * (int, couple) Hashtbl.t;
  h_descend : int ref * (int, descend) Hashtbl.t;
  h_string : int ref * (int, string) Hashtbl.t;
  h_name : (int, int list) Hashtbl.t;
}

(* Old structure of file "patches", kept for backward compatibility.
   After conversion, a new change will be saved with a magic number
   (magic_patch) and a record "patch_ht" above. *)

module Old = struct
  type patches = {
    p_person : (int * person) list ref;
    p_ascend : (int * ascend) list ref;
    p_union : (int * union) list ref;
    p_family : (int * family) list ref;
    p_couple : (int * couple) list ref;
    p_descend : (int * descend) list ref;
    p_string : (int * string) list ref;
    p_name : (int * int list) list ref;
  }
end

module type X = sig
  val equal : string -> string -> bool
  (** [equal s1 s2] checks if [s1] and [s2] are equal. *)

  val hash : string -> int
  (** [hash s] computes a hash of the string [s]. *)

  val string_of_id : int -> string
  (** [string_of_id i] returns the string of identifier [i]. *)
end

module InvertedIndex (X : X) : sig
  type t
  type idx = (int, string) Hashtbl.t

  exception Conflict of string * int * int
  (** Exception raised when a conflict between indexes is detected. *)

  val load : base_version -> inx:string -> idx list -> t
  (** [load version ~inx l] loads the inverted index of strings from file [inx],
      assuming that the binary format is [version]. The list of indexes [l] are
      loaded. *)

  val find : t -> string -> int
  (** [find t s] searches for string [s] in the inverted index [t].

      @raise Not_found if string [s] is not found in [t]. *)

  val insert : t -> string -> int -> unit
  (** [insert t s i] adds string [s] with its index [i] in [t].

      @raise Conflict
        if string [s] is already present in [t] with a different index. *)
end = struct
  module HS = Hashtbl.Make (struct
    type t = string

    let equal = X.equal
    let hash = X.hash
  end)

  type inx = { fl : string; start : int; size : int }
  type t = { inx : inx option; tbl : int HS.t }
  type idx = (int, string) Hashtbl.t

  let int_size = 4

  exception Conflict of string * int * int

  let insert t s i =
    match HS.find t.tbl s with
    | j when i <> j -> raise (Conflict (s, j, i))
    | _ | (exception Not_found) -> HS.add t.tbl s i

  let load_inx version ~inx =
    Secure.with_open_in_bin inx @@ fun ic ->
    let start =
      match version with
      | GnWb0024 | GnWb0023 | GnWb0022 -> int_size
      | GnWb0021 | GnWb0020 -> 3 * int_size
    in
    let size = input_binary_int ic in
    ignore (input_binary_int ic : int);
    ignore (input_binary_int ic : int);
    { fl = inx; start; size }

  let load version ~inx indexes =
    let inx =
      if Sys.file_exists inx then Some (load_inx version ~inx)
      else (
        Logs.warn (fun k -> k "cannot load the inverted index file %S" inx);
        None)
    in
    let tbl = HS.create 17 in
    let t = { inx; tbl } in
    List.iter (Hashtbl.iter (fun i s -> insert t s i)) indexes;
    t

  let find t s =
    match HS.find t.tbl s with
    | i -> i
    | exception Not_found -> (
        match t.inx with
        | None -> raise Not_found
        | Some inx ->
            Secure.with_open_in_bin inx.fl @@ fun ic ->
            let h = X.hash s mod inx.size in
            seek_in ic (inx.start + (h * int_size));
            let rec loop i =
              if i = -1 then raise Not_found
              else if X.equal (X.string_of_id i) s then i
              else (
                seek_in ic (inx.start + ((inx.size + i) * int_size));
                loop @@ input_binary_int ic)
            in
            loop @@ input_binary_int ic)
end

let ( // ) = Filename.concat

let apply_patches tab patches plen =
  if plen = 0 then tab
  else
    let new_tab =
      if plen > Array.length tab then (
        let new_tab = Array.make plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab);
        new_tab)
      else tab
    in
    Hashtbl.iter (Array.set new_tab) patches;
    new_tab

let make_record_exists patches pending len i =
  Hashtbl.mem pending i || Hashtbl.mem patches i || (i < len && i >= 0)

type 'a data_array =
  | ReadOnly of 'a array Gw_ancient.ancient
  | ReadWrite of 'a array

type 'a immut_record = {
  im_array : unit -> 'a data_array;
  im_get : int -> 'a;
  im_clear_array : unit -> unit;
}

let make_immut_record_access ~read_only ic ic_acc shift array_pos len name
    input_array input_item =
  let (tab : _ data_array option ref) = ref None in
  let cleared = ref false in
  let im_get i =
    match !tab with
    | Some (ReadWrite x) -> x.(i)
    | Some (ReadOnly x) -> (Gw_ancient.follow x).(i)
    | None -> (
        if i < 0 || i >= len then
          failwith ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
        else
          match ic_acc with
          | Some ic_acc ->
              seek_in ic_acc (shift + (Iovalue.sizeof_long * i));
              let pos = input_binary_int ic_acc in
              seek_in ic pos;
              input_item ic
          | None ->
              Printf.eprintf "Sorry; I really need base.acc\n";
              flush stderr;
              failwith "cannot access database")
  in
  let im_array () =
    match !tab with
    | Some x -> x
    | None ->
        seek_in ic array_pos;
        let t =
          if read_only then (
            let t = ReadOnly (Gw_ancient.mark (input_array ic)) in
            Gc.compact ();
            t)
          else ReadWrite (input_array ic)
        in
        tab := Some t;
        t
  in
  let r =
    {
      im_array;
      im_get;
      im_clear_array =
        (fun () ->
          cleared := true;
          match !tab with
          | None -> ()
          | Some _a ->
              (* We could call [Gw_ancient.delete] here to
                 free the memory allocated with Ancient. Unfortunately,
                 [Ancient.delete] is buggy and cannot be used without causing
                 the process terminate abruptly. *)
              tab := None);
    }
  in
  r

let make_record_access immut_record (plenr, patches) (_, pending) len =
  let get_nopending i =
    match Hashtbl.find patches i with
    | v -> v
    | exception Not_found -> immut_record.im_get i
  in
  let get i =
    match Hashtbl.find pending i with
    | v -> v
    | exception Not_found -> get_nopending i
  in
  let len = max len !plenr in
  let rec r =
    {
      load_array = (fun () -> ignore @@ immut_record.im_array ());
      get;
      get_nopending;
      len;
      output_array =
        (fun oc ->
          match immut_record.im_array () with
          | ReadOnly _ -> failwith "cannot modify read-only data"
          | ReadWrite v ->
              let a = apply_patches v patches r.len in
              Dutil.output_value_no_sharing oc (a : _ array));
      clear_array = immut_record.im_clear_array;
    }
  in
  r

let magic_patch = "GnPa0001"

let check_patch_magic ic =
  really_input_string ic (String.length magic_patch) = magic_patch

let empty_patch_ht () =
  {
    h_person = (ref 0, Hashtbl.create 1);
    h_ascend = (ref 0, Hashtbl.create 1);
    h_union = (ref 0, Hashtbl.create 1);
    h_family = (ref 0, Hashtbl.create 1);
    h_couple = (ref 0, Hashtbl.create 1);
    h_descend = (ref 0, Hashtbl.create 1);
    h_string = (ref 0, Hashtbl.create 1);
    h_name = Hashtbl.create 1;
  }

let input_patches bname =
  let fname = Filename.concat bname "patches" in
  if Sys.file_exists fname then
    try
      let ic = Secure.open_in_bin fname in
      let r =
        if check_patch_magic ic then (input_value ic : patches_ht)
        else (
          (* old implementation of patches *)
          seek_in ic 0;
          let patches : Old.patches = input_value ic in
          let ht = empty_patch_ht () in
          let add (ir, ht) (k, v) =
            if k >= !ir then ir := k + 1;
            Hashtbl.add ht k v
          in
          List.iter (add ht.h_person) !(patches.Old.p_person);
          List.iter (add ht.h_ascend) !(patches.Old.p_ascend);
          List.iter (add ht.h_union) !(patches.Old.p_union);
          List.iter (add ht.h_family) !(patches.Old.p_family);
          List.iter (add ht.h_couple) !(patches.Old.p_couple);
          List.iter (add ht.h_descend) !(patches.Old.p_descend);
          List.iter (add ht.h_string) !(patches.Old.p_string);
          List.iter (add (ref 0, ht.h_name)) !(patches.Old.p_name);
          ht)
      in
      close_in ic;
      Ok r
    with _ -> Error (Printf.sprintf "%s: corrupted file" fname)
  else Ok (empty_patch_ht ())

let input_synchro bname =
  try
    let ic = Secure.open_in_bin (Filename.concat bname "synchro_patches") in
    let r : synchro_patch = input_value ic in
    close_in ic;
    r
  with _ -> { synch_list = [] }

let person_of_key persons strings persons_of_name first_name surname occ =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  let ipl = persons_of_name (first_name ^ " " ^ surname) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec find = function
    | ip :: ipl ->
        let p = persons.get ip in
        if
          occ = p.occ
          && first_name = Name.lower (strings.get p.first_name)
          && surname = Name.lower (strings.get p.surname)
        then Some ip
        else find ipl
    | _ -> None
  in
  find ipl

type ro_data_records =
  dsk_person immut_record
  * dsk_ascend immut_record
  * dsk_union immut_record
  * dsk_family immut_record
  * dsk_couple immut_record
  * dsk_descend immut_record
  * string immut_record

let cached_records = ref []

let try_with_open openfun s f =
  let ic =
    try Some (openfun s)
    with Sys_error e ->
      Format.eprintf "@[While loading '%s', got: %s@ trying to continue...@]@."
        s e;
      None
  in
  let finally () = match ic with Some ic -> close_in_noerr ic | None -> () in
  Fun.protect ~finally (fun () -> f ic)

let try_with_open_bin s f = try_with_open Secure.open_in_bin s f

let with_database ?(read_only = false) bname k =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let tm_fname = bname // "commit_timestamp" in
  let patches = input_patches bname in
  let pending : patches_ht = empty_patch_ht () in
  let patches, perm =
    match patches with
    | Ok patches ->
        fst pending.h_person := !(fst patches.h_person);
        fst pending.h_ascend := !(fst patches.h_ascend);
        fst pending.h_union := !(fst patches.h_union);
        fst pending.h_family := !(fst patches.h_family);
        fst pending.h_couple := !(fst patches.h_couple);
        fst pending.h_descend := !(fst patches.h_descend);
        fst pending.h_string := !(fst patches.h_string);
        (patches, if Sys.file_exists tm_fname then RDONLY else RDRW)
    | Error msg ->
        prerr_endline msg;
        (empty_patch_ht (), RDONLY)
  in
  let synchro = input_synchro bname in
  let particles = Mutil.input_particles (bname // "particles.txt") in
  Secure.with_open_in_bin (bname // "base") @@ fun ic ->
  let version =
    if Mutil.check_magic Dutil.magic_GnWb0024 ic then GnWb0024
    else if Mutil.check_magic Dutil.magic_GnWb0023 ic then GnWb0023
    else if Mutil.check_magic Dutil.magic_GnWb0022 ic then GnWb0022
    else if Mutil.check_magic Dutil.magic_GnWb0021 ic then GnWb0021
    else if Mutil.check_magic Dutil.magic_GnWb0020 ic then GnWb0020
    else if really_input_string ic 4 = "GnWb" then
      failwith "this is a GeneWeb base, but not compatible"
    else failwith "this is not a GeneWeb base, or it is a very old version"
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
  try_with_open_bin (bname // "base.acc") @@ fun ic_acc ->
  let shift = 0 in
  let iper_exists =
    make_record_exists (snd patches.h_person) (snd pending.h_person) persons_len
  in
  let ifam_exists =
    make_record_exists (snd patches.h_family) (snd pending.h_family)
      families_len
  in
  let ( im_persons,
        im_ascends,
        im_unions,
        im_families,
        im_couples,
        im_descends,
        im_strings ) : ro_data_records =
    let bid =
      let s = Unix.stat bname in
      (s.st_dev, s.st_ino)
    in
    match List.find_opt (fun (n, _) -> bid = n) !cached_records with
    | Some (_, records) -> records
    | None ->
        let persons =
          make_immut_record_access ~read_only ic ic_acc shift persons_array_pos
            persons_len "persons"
            (input_value : _ -> person array)
            (Iovalue.input : _ -> person)
        in
        let shift = shift + (persons_len * Iovalue.sizeof_long) in
        let ascends =
          make_immut_record_access ~read_only ic ic_acc shift ascends_array_pos
            persons_len "ascends"
            (input_value : _ -> ascend array)
            (Iovalue.input : _ -> ascend)
        in
        let shift = shift + (persons_len * Iovalue.sizeof_long) in
        let unions =
          make_immut_record_access ~read_only ic ic_acc shift unions_array_pos
            persons_len "unions"
            (input_value : _ -> union array)
            (Iovalue.input : _ -> union)
        in
        let shift = shift + (persons_len * Iovalue.sizeof_long) in
        let families =
          make_immut_record_access ~read_only ic ic_acc shift families_array_pos
            families_len "families"
            (input_value : _ -> family array)
            (Iovalue.input : _ -> family)
        in
        let shift = shift + (families_len * Iovalue.sizeof_long) in
        let couples =
          make_immut_record_access ~read_only ic ic_acc shift couples_array_pos
            families_len "couples"
            (input_value : _ -> couple array)
            (Iovalue.input : _ -> couple)
        in
        let shift = shift + (families_len * Iovalue.sizeof_long) in
        let descends =
          make_immut_record_access ~read_only ic ic_acc shift descends_array_pos
            families_len "descends"
            (input_value : _ -> descend array)
            (Iovalue.input : _ -> descend)
        in
        let shift = shift + (families_len * Iovalue.sizeof_long) in
        let strings =
          make_immut_record_access ~read_only ic ic_acc shift strings_array_pos
            strings_len "strings"
            (input_value : _ -> string array)
            (Iovalue.input : _ -> string)
        in
        let dr =
          (persons, ascends, unions, families, couples, descends, strings)
        in
        if read_only then cached_records := (bid, dr) :: !cached_records;
        dr
  in
  let persons =
    make_record_access im_persons patches.h_person pending.h_person persons_len
  in
  let ascends =
    make_record_access im_ascends patches.h_ascend pending.h_ascend persons_len
  in
  let unions =
    make_record_access im_unions patches.h_union pending.h_union persons_len
  in
  let families =
    make_record_access im_families patches.h_family pending.h_family
      families_len
  in
  let couples =
    make_record_access im_couples patches.h_couple pending.h_couple families_len
  in
  let descends =
    make_record_access im_descends patches.h_descend pending.h_descend
      families_len
  in
  let strings =
    make_record_access im_strings patches.h_string pending.h_string strings_len
  in
  let commit_synchro () =
    let tmp_fname = bname // "1synchro_patches" in
    let fname = bname // "synchro_patches" in
    let oc9 =
      try Secure.open_out_bin tmp_fname
      with Sys_error _ -> raise (Failure "the database is not writable")
    in
    let synchro =
      let timestamp = string_of_float (Unix.time ()) in
      let timestamp = String.sub timestamp 0 (String.index timestamp '.') in
      let v = (timestamp, !synchro_person, !synchro_family) in
      { synch_list = v :: synchro.synch_list }
    in
    Dutil.output_value_no_sharing oc9 (synchro : synchro_patch);
    close_out oc9;
    move_with_backup tmp_fname fname
  in
  let nbp_fname = bname // "nb_persons" in
  let is_empty_name p =
    (0 = p.surname || 1 = p.surname) && (0 = p.first_name || 1 = p.first_name)
  in
  let npb_init () =
    let cnt = ref 0 in
    for i = 0 to persons.len - 1 do
      if not @@ is_empty_name @@ persons.get i then incr cnt
    done;
    Secure.with_open_out_bin nbp_fname (fun oc -> output_value oc !cnt);
    !cnt
  in
  let nbp_read () =
    if Sys.file_exists nbp_fname then
      Secure.with_open_in_bin nbp_fname input_value
    else npb_init ()
  in
  let commit_patches =
    if perm = RDONLY then fun () -> raise (HttpExn (Forbidden, __LOC__))
    else fun () ->
      let tm = Unix.time () |> Unix.gmtime |> Mutil.sprintf_date in
      (* read real person number (considering pending patches) *)
      let nbp =
        Hashtbl.fold
          (fun ip p acc ->
            try
              match try Some (persons.get_nopending ip) with _ -> None with
              | Some old ->
                  if is_empty_name old && not (is_empty_name p) then acc + 1
                  else if (not (is_empty_name old)) && is_empty_name p then
                    acc - 1
                  else acc
              | None -> if not (is_empty_name p) then acc + 1 else acc
            with _ -> if not (is_empty_name p) then acc + 1 else acc)
          (snd pending.h_person) (nbp_read ())
      in
      let tmp_nbp_fname = nbp_fname ^ "_tmp" in
      let oc = Secure.open_out_bin tmp_nbp_fname in
      output_value oc nbp;
      close_out oc;
      let aux (n, ht) (n', ht') =
        n := !n';
        Hashtbl.iter (Hashtbl.replace ht) ht';
        Hashtbl.clear ht'
      in
      (* commit every pending patch *)
      aux patches.h_person pending.h_person;
      aux patches.h_ascend pending.h_ascend;
      aux patches.h_union pending.h_union;
      aux patches.h_family pending.h_family;
      aux patches.h_couple pending.h_couple;
      aux patches.h_descend pending.h_descend;
      aux patches.h_string pending.h_string;
      (* update "patches" file *)
      let tmp_fname = Filename.concat bname "1patches" in
      let fname = Filename.concat bname "patches" in
      let tm_oc = Secure.open_out_bin tm_fname in
      output_string tm_oc (tm : Adef.safe_string :> string);
      close_out tm_oc;
      let oc_tmp = Secure.open_out_bin tmp_fname in
      output_string oc_tmp magic_patch;
      Dutil.output_value_no_sharing oc_tmp (patches : patches_ht);
      close_out oc_tmp;
      move_with_backup tmp_nbp_fname nbp_fname;
      move_with_backup tmp_fname fname;
      commit_synchro ();
      Sys.remove tm_fname
  in
  let patch_person i p =
    assert (i <> -1);
    persons.len <- max persons.len (i + 1);
    fst pending.h_person := persons.len;
    Hashtbl.replace (snd pending.h_person) i p;
    synchro_person := i :: !synchro_person
  in
  let patch_ascend i a =
    assert (i <> -1);
    ascends.len <- max ascends.len (i + 1);
    fst pending.h_ascend := ascends.len;
    Hashtbl.replace (snd pending.h_ascend) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_union i a =
    assert (i <> -1);
    unions.len <- max unions.len (i + 1);
    fst pending.h_union := ascends.len;
    Hashtbl.replace (snd pending.h_union) i a;
    synchro_person := i :: !synchro_person
  in
  let patch_family i f =
    assert (i <> -1);
    families.len <- max families.len (i + 1);
    fst pending.h_family := families.len;
    Hashtbl.replace (snd pending.h_family) i f;
    synchro_family := i :: !synchro_family
  in
  let patch_couple i c =
    assert (i <> -1);
    couples.len <- max couples.len (i + 1);
    fst pending.h_couple := couples.len;
    Hashtbl.replace (snd pending.h_couple) i c;
    synchro_family := i :: !synchro_family
  in
  let patch_descend i c =
    assert (i <> -1);
    descends.len <- max descends.len (i + 1);
    fst pending.h_descend := descends.len;
    Hashtbl.replace (snd pending.h_descend) i c;
    synchro_family := i :: !synchro_family
  in
  let module I = InvertedIndex (struct
    let hash = Hashtbl.hash
    let equal = String.equal
    let string_of_id = strings.get
  end) in
  let inv_idx =
    I.load version ~inx:(bname // "strings.inx")
      [ snd patches.h_string; snd pending.h_string ]
  in
  let insert_string s =
    try I.find inv_idx s
    with Not_found ->
      let i = strings.len in
      strings.len <- strings.len + 1;
      fst pending.h_string := strings.len;
      Hashtbl.add (snd pending.h_string) i s;
      I.insert inv_idx s i;
      i
  in
  let patch_name s ip =
    (* FIXME: pending patches? *)
    let i = Dutil.name_index s in
    try
      let ipl = Hashtbl.find patches.h_name i in
      if List.mem ip ipl then ()
      else Hashtbl.replace patches.h_name i (ip :: ipl)
    with Not_found -> Hashtbl.add patches.h_name i [ ip ]
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
        | RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> ( try input_line ic with End_of_file -> "")
        | RnAll ->
            let rec loop len =
              match input_char ic with
              | exception End_of_file -> Buff.get len
              | c -> loop (Buff.store len c)
            in
            loop 0
      in
      close_in ic;
      str
    with Sys_error _ -> ""
  in
  let commit_notes =
    if perm = RDONLY then fun _ _ -> raise (HttpExn (Forbidden, __LOC__))
    else fun fnotes s ->
      let fname =
        if fnotes = "" then "notes"
        else (
          (try Unix.mkdir (Filename.concat bname "notes_d") 0o755 with _ -> ());
          Filename.concat "notes_d" (fnotes ^ ".txt"))
      in
      let fname = Filename.concat bname fname in
      (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
      (try Sys.rename fname (fname ^ "~") with _ -> ());
      if s <> "" then (
        let oc = Secure.open_out fname in
        output_string oc s;
        close_out oc)
  in
  let commit_wiznotes =
    if perm = RDONLY then fun _ _ -> raise (HttpExn (Forbidden, __LOC__))
    else fun fnotes s ->
      if fnotes <> "" then (
        let wiznotes_dir = Filename.concat bname "wiznotes" in
        let fname =
          (try
             if Sys.file_exists wiznotes_dir then ()
             else Unix.mkdir (Filename.concat bname "wiznotes") 0o755
           with _ -> ());
          Filename.concat wiznotes_dir (fnotes ^ ".txt")
        in
        (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
        (try Sys.rename fname (fname ^ "~") with _ -> ());
        if s <> "" then (
          let oc = Secure.open_out fname in
          output_string oc s;
          close_out oc))
  in
  let ext_files () =
    Filesystem.walk_folder ~recursive:true
      (fun fl files ->
        match fl with
        | File f when Filename.check_suffix f ".txt" ->
            Filename.chop_suffix f ".txt" :: files
        | File _ | Dir _ | Exn _ ->
            (* TODO: we may print a warning for errors. *)
            files)
      (Filename.concat bname "nodes_d")
      []
  in
  let bnotes = { nread = read_notes; norigin_file; efiles = ext_files } in
  let base_data =
    {
      persons;
      ascends;
      unions;
      visible = make_visible_record_access perm bname persons;
      families;
      couples;
      descends;
      strings;
      particles_txt = particles;
      particles = lazy (Mutil.compile_particles particles);
      bnotes;
      bdir = bname;
      perm;
    }
  in
  let persons_of_name = persons_of_name bname patches.h_name in
  let base_func =
    {
      person_of_key = person_of_key persons strings persons_of_name;
      persons_of_name;
      strings_of_sname = strings_of_sname version bname strings patches.h_person;
      strings_of_fname = strings_of_fname version bname strings patches.h_person;
      persons_of_surname =
        persons_of_surname version base_data
          ( (fun p -> p.surname),
            snd patches.h_person,
            "snames.inx",
            "snames.dat",
            bname );
      persons_of_first_name =
        persons_of_first_name version base_data
          ( (fun p -> p.first_name),
            snd patches.h_person,
            "fnames.inx",
            "fnames.dat",
            bname );
      patch_person;
      patch_ascend;
      patch_union;
      patch_family;
      patch_couple;
      patch_descend;
      patch_name;
      insert_string;
      commit_patches;
      commit_notes;
      commit_wiznotes;
      nb_of_real_persons = nbp_read;
      iper_exists;
      ifam_exists;
    }
  in
  k { data = base_data; func = base_func; version }

let record_access_of tab =
  {
    Dbdisk.load_array = (fun () -> ());
    get = (fun i -> tab.(i));
    get_nopending = (fun i -> tab.(i));
    output_array = (fun oc -> Dutil.output_value_no_sharing oc (tab : _ array));
    len = Array.length tab;
    clear_array = (fun () -> ());
  }

let make bname particles ((persons, families, strings, bnotes) as _arrays) k =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  Filesystem.create_dir ~parent:true (bdir // "notes_d");
  Filesystem.create_dir (bdir // "wiznotes");
  Filesystem.create_file (bdir // "notes");
  let persons, ascends, unions = persons in
  let families, couples, descends = families in
  let data : Dbdisk.base_data =
    {
      persons = record_access_of persons;
      ascends = record_access_of ascends;
      unions = record_access_of unions;
      families = record_access_of families;
      visible =
        { v_write = (fun _ -> assert false); v_get = (fun _ -> assert false) };
      couples = record_access_of couples;
      descends = record_access_of descends;
      strings = record_access_of strings;
      particles_txt = particles;
      particles = lazy (Mutil.compile_particles particles);
      bnotes;
      bdir;
      perm = RDRW;
    }
  in
  (* since this function is called exclusively to create a database, it doesn't
     needs for functionalities over arays *)
  let func : Dbdisk.base_func =
    {
      person_of_key = (fun _ -> assert false);
      persons_of_name = (fun _ -> assert false);
      strings_of_sname = (fun _ -> assert false);
      strings_of_fname = (fun _ -> assert false);
      persons_of_surname =
        {
          find = (fun _ -> assert false);
          cursor = (fun _ -> assert false);
          next = (fun _ -> assert false);
        };
      persons_of_first_name =
        {
          find = (fun _ -> assert false);
          cursor = (fun _ -> assert false);
          next = (fun _ -> assert false);
        };
      patch_person = (fun _ -> assert false);
      patch_ascend = (fun _ -> assert false);
      patch_union = (fun _ -> assert false);
      patch_family = (fun _ -> assert false);
      patch_couple = (fun _ -> assert false);
      patch_descend = (fun _ -> assert false);
      patch_name = (fun _ -> assert false);
      insert_string = (fun _ -> assert false);
      commit_patches = (fun _ -> assert false);
      commit_notes = (fun _ -> assert false);
      commit_wiznotes = (fun _ -> assert false);
      nb_of_real_persons = (fun _ -> assert false);
      iper_exists = (fun _ -> assert false);
      ifam_exists = (fun _ -> assert false);
    }
  in
  k { data; func; version = GnWb0024 }
