open Dbdisk

let magic = "GnBi0001"
let landmark_max = 64
let default_budget = 1 lsl 20

type section = {
  code : int;
  n : int;
  block : int;
  width : int;
  istr_off : int;
  landmarks_len : int;
}

let section_size = 28

let code d =
  match d with
  | Dict.First_name -> 0
  | Dict.Surname -> 1
  | Dict.First_name_alias -> 2
  | Dict.Surname_alias -> 3
  | Dict.Public_name -> 4
  | Dict.Qualifier -> 5
  | Dict.Alias -> 6
  | Dict.Occupation -> 7
  | Dict.Place -> 8
  | Dict.Source -> 9
  | Dict.Title -> 10
  | Dict.Domain -> 11

let keeps_empty d =
  match d with
  | Dict.First_name | Dict.Surname -> true
  | Dict.(
      ( First_name_alias | Surname_alias | Public_name | Qualifier | Alias
      | Occupation | Place | Source | Title | Domain )) ->
      false

let landmark_len k = min landmark_max (String.length k)

let output_int64 oc n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  output_bytes oc b

let output_section oc s =
  output_binary_int oc s.code;
  output_binary_int oc s.n;
  output_binary_int oc s.block;
  output_binary_int oc s.width;
  output_int64 oc s.istr_off;
  output_binary_int oc s.landmarks_len

let block_size keys budget =
  let n = Array.length keys in
  let size block =
    let rec loop r acc =
      if r >= n then acc else loop (r + block) (acc + 1 + landmark_len keys.(r))
    in
    loop 0 0
  in
  let rec loop block =
    if block >= n || size block <= budget then block else loop (2 * block)
  in
  loop 1

let section_person d p =
  let l = Dict.person_strings d p in
  List.sort_uniq Int.compare
    (if keeps_empty d then l else List.filter (fun i -> i <> 0) l)

let section_family d (f : dsk_family) =
  if f.Def.fam_index < 0 || not (Dict.has_family_fields d) then []
  else
    List.sort_uniq Int.compare
      (List.filter (fun i -> i <> 0) (Dict.family_strings d f))

let records (data : base_data) d =
  let refs = Dutil.IntHT.create 1024 in
  let add r istr =
    Dutil.IntHT.replace refs istr
      (r :: Option.value ~default:[] (Dutil.IntHT.find_opt refs istr))
  in
  for i = 0 to data.persons.len - 1 do
    List.iter (add (2 * i)) (section_person d (data.persons.get_nopending i))
  done;
  if Dict.has_family_fields d then
    for i = 0 to data.families.len - 1 do
      List.iter
        (add ((2 * i) + 1))
        (section_family d (data.families.get_nopending i))
    done;
  refs

let output_dict ~budget data particles oc oc_dat d =
  let refs = records data d in
  let entries = Array.make (Dutil.IntHT.length refs) ("", 0) in
  ignore
    (Dutil.IntHT.fold
       (fun istr _ i ->
         entries.(i) <-
           (Dict.key particles d (data.strings.get_nopending istr), istr);
         i + 1)
       refs 0
      : int);
  Array.sort
    (fun (k1, i1) (k2, i2) ->
      match String.compare k1 k2 with 0 -> Int.compare i1 i2 | c -> c)
    entries;
  let n = Array.length entries in
  let positions =
    Array.init n (fun r ->
        let pos = pos_out oc_dat in
        let l = List.rev (Dutil.IntHT.find refs (snd entries.(r))) in
        output_binary_int oc_dat (List.length l);
        List.iter (output_binary_int oc_dat) l;
        pos)
  in
  let width = if n = 0 || positions.(n - 1) < 1 lsl 31 then 4 else 8 in
  let keys = Array.map fst entries in
  let block = block_size keys budget in
  let istr_off = pos_out oc in
  Array.iter (fun (_, istr) -> output_binary_int oc istr) entries;
  Array.iter
    (fun pos ->
      if width = 4 then output_binary_int oc pos else output_int64 oc pos)
    positions;
  let landmarks_off = pos_out oc in
  let rec loop r =
    if r < n then (
      let len = landmark_len keys.(r) in
      output_byte oc len;
      output_substring oc keys.(r) 0 len;
      loop (r + block))
  in
  loop 0;
  {
    code = code d;
    n;
    block;
    width;
    istr_off;
    landmarks_len = pos_out oc - landmarks_off;
  }

let output ~budget data ~inx ~dat =
  let particles = Lazy.force data.particles in
  Secure.with_open_out_bin dat @@ fun oc_dat ->
  Secure.with_open_out_bin inx @@ fun oc ->
  output_string oc magic;
  output_binary_int oc (List.length Dict.all);
  let directory = pos_out oc in
  output_string oc (String.make (section_size * List.length Dict.all) '\000');
  let sections =
    List.fold_left
      (fun l d -> output_dict ~budget data particles oc oc_dat d :: l)
      [] Dict.all
  in
  seek_out oc directory;
  List.iter (output_section oc) (List.rev sections)

let input_int64 ic =
  Int64.to_int (String.get_int64_be (really_input_string ic 8) 0)

let input_section ic =
  let code = input_binary_int ic in
  let n = input_binary_int ic in
  let block = input_binary_int ic in
  let width = input_binary_int ic in
  let istr_off = input_int64 ic in
  let landmarks_len = input_binary_int ic in
  { code; n; block; width; istr_off; landmarks_len }

let input_sections fname ic =
  seek_in ic 0;
  let found =
    try really_input_string ic (String.length magic) with End_of_file -> ""
  in
  if found <> magic then
    raise (Dutil.Incompatible_format (fname, found, [ magic ]));
  let nb = input_binary_int ic in
  List.init nb (fun _ -> input_section ic)

let input_landmarks ic s =
  seek_in ic (s.istr_off + ((4 + s.width) * s.n));
  let b = really_input_string ic s.landmarks_len in
  let a = Array.make ((s.n + s.block - 1) / s.block) "" in
  let rec loop i pos =
    if i < Array.length a then (
      let len = Char.code b.[pos] in
      a.(i) <- String.sub b (pos + 1) len;
      loop (i + 1) (pos + 1 + len))
  in
  loop 0 0;
  a

let istr_at ic s r =
  seek_in ic (s.istr_off + (4 * r));
  input_binary_int ic

let pos_at ic s r =
  seek_in ic (s.istr_off + (4 * s.n) + (s.width * r));
  if s.width = 4 then input_binary_int ic else input_int64 ic

let input_refs ic pos =
  seek_in ic pos;
  let n = input_binary_int ic in
  List.init n (fun _ -> input_binary_int ic)

(* Smallest [i] of [lo, hi) such that [p i], [hi] if none; [p] is monotone. *)
let first p lo hi =
  let rec loop lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if p mid then loop lo mid else loop (mid + 1) hi
  in
  loop lo hi

(* Rank of the first key not less than [k]. The landmark of a block bounds
   its first key from below; a truncated landmark that is a prefix of [k]
   tells nothing, so the keys of its block are read. *)
let lower_bound landmarks s key k =
  let surely_ge j = String.compare landmarks.(j) k >= 0 in
  let maybe_ge j =
    surely_ge j
    || String.length landmarks.(j) = landmark_max
       && String.starts_with ~prefix:landmarks.(j) k
  in
  let nl = Array.length landmarks in
  let j1 = first maybe_ge 0 nl in
  let j2 = first surely_ge j1 nl in
  let lo = if j1 = 0 then 0 else ((j1 - 1) * s.block) + 1 in
  let hi = if j2 = nl then s.n else j2 * s.block in
  first (fun r -> String.compare (key r) k >= 0) lo hi

let after_prefix p = p ^ "\xFF"

let label g =
  let n = String.length g in
  if n > 0 && g.[n - 1] = '\000' then String.sub g 0 (n - 1) else g

type stamp = int * int64 * float

let stamp_of st = Unix.LargeFile.(st.st_ino, st.st_size, st.st_mtime)

let stamp fname =
  try Some (stamp_of (Unix.LargeFile.stat fname))
  with Unix.Unix_error _ -> None

type source = {
  file_person : int -> dsk_person;
  persons_len : int;
  file_family : int -> dsk_family;
  families_len : int;
  file_string : int -> string;
  strings_len : int;
  person_patches : (int, dsk_person) Hashtbl.t;
  family_patches : (int, dsk_family) Hashtbl.t;
  string_patches : (int, string) Hashtbl.t;
  patches_stamp : stamp option;
}

type file_section = {
  s : section;
  istr : int -> int;
  key : int -> string;
  lower_bound : string -> int;
  rank : int -> int option;
  count : int -> int;
  refs : int -> int list;
}

(* [hidden]: ranks of the file entries without records left or whose string
   was rewritten by replace_string; [extra]: entries out of the file order,
   sorted by their current key. *)
type correction = {
  delta : (int, int list * int list) Hashtbl.t;
  hidden : int array;
  extra : (string * int) array;
}

let delta src d =
  let tbl = Hashtbl.create 64 in
  let update i f =
    Hashtbl.replace tbl i
      (f (Option.value ~default:([], []) (Hashtbl.find_opt tbl i)))
  in
  let diff r old_l new_l =
    List.iter
      (fun i ->
        if not (List.mem i new_l) then
          update i (fun (rm, add) -> (r :: rm, add)))
      old_l;
    List.iter
      (fun i ->
        if not (List.mem i old_l) then
          update i (fun (rm, add) -> (rm, r :: add)))
      new_l
  in
  Hashtbl.iter
    (fun ip p ->
      let old_l =
        if ip < src.persons_len then section_person d (src.file_person ip)
        else []
      in
      diff (2 * ip) old_l (section_person d p))
    src.person_patches;
  if Dict.has_family_fields d then
    Hashtbl.iter
      (fun ifam f ->
        let old_l =
          if ifam < src.families_len then
            section_family d (src.file_family ifam)
          else []
        in
        diff ((2 * ifam) + 1) old_l (section_family d f))
      src.family_patches;
  tbl

let records_count f delta rank i =
  let removed, added =
    Option.value ~default:([], []) (Hashtbl.find_opt delta i)
  in
  Option.fold ~none:0 ~some:f.count rank
  - List.length removed + List.length added

let correction src key f d =
  let delta = delta src d in
  let moved = Hashtbl.create 16 in
  Hashtbl.iter
    (fun i s ->
      if i < src.strings_len && s <> src.file_string i then
        Hashtbl.replace moved i ())
    src.string_patches;
  let touched = Hashtbl.copy moved in
  Hashtbl.iter (fun i _ -> Hashtbl.replace touched i ()) delta;
  let hidden, extra =
    Hashtbl.fold
      (fun i () (hidden, extra) ->
        let rank = f.rank i in
        let count = records_count f delta rank i in
        let moved = Hashtbl.mem moved i in
        let hidden =
          match rank with
          | Some r when count = 0 || moved -> r :: hidden
          | Some _ | None -> hidden
        in
        let extra =
          if count > 0 && (moved || Option.is_none rank) then
            (key i, i) :: extra
          else extra
        in
        (hidden, extra))
      touched ([], [])
  in
  let hidden = Array.of_list hidden and extra = Array.of_list extra in
  Array.sort Int.compare hidden;
  Array.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) extra;
  { delta; hidden; extra }

let first_index a p = first (fun i -> p a.(i)) 0 (Array.length a)
let hidden_from c r = first_index c.hidden (fun r' -> r' >= r)

let is_hidden c r =
  let i = hidden_from c r in
  i < Array.length c.hidden && c.hidden.(i) = r

let extra_from c k =
  first_index c.extra (fun (k', _) -> String.compare k' k >= 0)

let count f c k1 k2 =
  let lo = f.lower_bound k1 and hi = f.lower_bound k2 in
  hi - lo
  - (hidden_from c hi - hidden_from c lo)
  + (extra_from c k2 - extra_from c k1)

let next_entry f c k =
  let rec visible r =
    if r < f.s.n && is_hidden c r then visible (r + 1) else r
  in
  let r = visible (f.lower_bound k) in
  let e = extra_from c k in
  let file = if r < f.s.n then Some (f.key r, f.istr r) else None in
  let extra = if e < Array.length c.extra then Some c.extra.(e) else None in
  match (file, extra) with
  | Some (kf, _), Some (ke, _) when String.compare ke kf < 0 -> extra
  | Some _, (Some _ | None) -> file
  | None, (Some _ | None) -> extra

let entries f c k1 k2 =
  let hi = f.lower_bound k2 in
  let e1 = extra_from c k1 in
  let extras =
    List.map
      (fun (k, i) -> (f.lower_bound k, i))
      (Array.to_list (Array.sub c.extra e1 (extra_from c k2 - e1)))
  in
  let rec loop r extras acc =
    match extras with
    | (before, i) :: l when before <= r || r >= hi -> loop r l (i :: acc)
    | _ :: _ | [] ->
        if r >= hi then List.rev acc
        else
          loop (r + 1) extras (if is_hidden c r then acc else f.istr r :: acc)
  in
  loop (f.lower_bound k1) extras []

let without removed l =
  match removed with
  | [] -> l
  | _ :: _ ->
      let t = Hashtbl.create (List.length removed) in
      List.iter (fun r -> Hashtbl.replace t r ()) removed;
      List.filter (fun r -> not (Hashtbl.mem t r)) l

let refs f c i =
  let removed, added =
    Option.value ~default:([], []) (Hashtbl.find_opt c.delta i)
  in
  let file = Option.fold ~none:[] ~some:f.refs (f.rank i) in
  List.rev_append
    (List.rev (without removed file))
    (List.sort Int.compare added)

let groups next_entry count p =
  let lp = String.length p in
  let stop = after_prefix p in
  let rec loop cursor acc =
    match next_entry cursor with
    | Some (k, _) when String.compare k stop < 0 ->
        if String.length k = lp then loop (p ^ "\000") ((p, 1) :: acc)
        else
          let g =
            String.sub k 0
              (lp + Uchar.utf_decode_length (String.get_utf_8_uchar k lp))
          in
          let next = after_prefix g in
          loop next ((label g, count g next) :: acc)
    | Some _ | None -> List.rev acc
  in
  loop p []

type memo = {
  inx : stamp;
  sections : (int * section) list;
  landmarks : (int, string array) Hashtbl.t;
  mutable patches : stamp option;
  mutable corrections : (Dict.t, correction) Hashtbl.t;
}

let memos = Hashtbl.create 1

let memo_of fname ic =
  let inx = stamp_of (Unix.LargeFile.fstat (Unix.descr_of_in_channel ic)) in
  match Hashtbl.find_opt memos fname with
  | Some m when m.inx = inx -> m
  | Some _ | None ->
      let m =
        {
          inx;
          sections = List.map (fun s -> (s.code, s)) (input_sections fname ic);
          landmarks = Hashtbl.create 12;
          patches = None;
          corrections = Hashtbl.create 12;
        }
      in
      Hashtbl.replace memos fname m;
      m

let with_reader bdir ~particles src k =
  let fname = Filename.concat bdir "base.inx" in
  let inx = lazy (Secure.open_in_bin fname) in
  let dat = lazy (Secure.open_in_bin (Filename.concat bdir "base.dat")) in
  let close c = if Lazy.is_val c then close_in_noerr (Lazy.force c) in
  Fun.protect ~finally:(fun () ->
      close inx;
      close dat)
  @@ fun () ->
  let memo = lazy (memo_of fname (Lazy.force inx)) in
  let current_string i =
    match Hashtbl.find_opt src.string_patches i with
    | Some s -> s
    | None -> src.file_string i
  in
  let file_section d =
    let m = Lazy.force memo in
    let s =
      match List.assoc_opt (code d) m.sections with
      | Some s -> s
      | None -> failwith (fname ^ ": no dictionary " ^ Dict.to_string d)
    in
    let inx = Lazy.force inx in
    let landmarks =
      match Hashtbl.find_opt m.landmarks s.code with
      | Some l -> l
      | None ->
          let l = input_landmarks inx s in
          Hashtbl.replace m.landmarks s.code l;
          l
    in
    let key_of str = Dict.key (Lazy.force particles) d str in
    let istr r = istr_at inx s r in
    let key r = key_of (src.file_string (istr r)) in
    let lower_bound = lower_bound landmarks s key in
    let rank i =
      if i < 0 || i >= src.strings_len then None
      else
        let k = key_of (src.file_string i) in
        let rec find r =
          if r < s.n && String.equal (key r) k then
            if istr r = i then Some r else find (r + 1)
          else None
        in
        find (lower_bound k)
    in
    let count r =
      let dat = Lazy.force dat in
      seek_in dat (pos_at inx s r);
      input_binary_int dat
    in
    let refs r = input_refs (Lazy.force dat) (pos_at inx s r) in
    ({ s; istr; key; lower_bound; rank; count; refs }, key_of)
  in
  let corrections =
    ref
      (lazy
        (let m = Lazy.force memo in
         match src.patches_stamp with
         | None -> Hashtbl.create 12
         | Some _ as st ->
             if st <> m.patches then (
               m.patches <- st;
               m.corrections <- Hashtbl.create 12);
             m.corrections))
  in
  let view d =
    let f, key_of = file_section d in
    let corrections = Lazy.force !corrections in
    let c =
      match Hashtbl.find_opt corrections d with
      | Some c -> c
      | None ->
          let c = correction src (fun i -> key_of (current_string i)) f d in
          Hashtbl.replace corrections d c;
          c
    in
    (f, c)
  in
  let dict_size d p =
    let f, c = view d in
    count f c p (after_prefix p)
  in
  let dict_groups d p =
    let f, c = view d in
    groups (next_entry f c) (count f c) p
  in
  let dict_entries d p =
    let f, c = view d in
    entries f c p (after_prefix p)
  in
  let dict_refs d i =
    let f, c = view d in
    refs f c i
  in
  let dict_count d i =
    let f, c = view d in
    records_count f c.delta (f.rank i) i
  in
  let entry_from d k =
    let f, c = view d in
    Option.map snd (next_entry f c k)
  in
  let key_of d str = Dict.key (Lazy.force particles) d str in
  let dict_first d str = entry_from d (key_of d str) in
  let dict_after d i = entry_from d (key_of d (current_string i) ^ "\000") in
  k
    {
      dict_size;
      dict_groups;
      dict_entries;
      dict_refs;
      dict_count;
      dict_first;
      dict_after;
    } (fun () -> corrections := lazy (Hashtbl.create 12))
