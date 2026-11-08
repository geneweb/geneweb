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

let records data d =
  let refs = Dutil.IntHT.create 1024 in
  let add r istr =
    if istr <> 0 || keeps_empty d then
      Dutil.IntHT.replace refs istr
        (r :: Option.value ~default:[] (Dutil.IntHT.find_opt refs istr))
  in
  let add_all r l = List.iter (add r) (List.sort_uniq Int.compare l) in
  for i = 0 to data.persons.len - 1 do
    add_all (2 * i) (Dict.person_strings d (data.persons.get_nopending i))
  done;
  if Dict.has_family_fields d then
    for i = 0 to data.families.len - 1 do
      let f = data.families.get_nopending i in
      if f.Def.fam_index >= 0 then
        add_all ((2 * i) + 1) (Dict.family_strings d f)
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

let groups lower_bound key p =
  let lp = String.length p in
  let stop = lower_bound (after_prefix p) in
  let rec loop r acc =
    if r >= stop then List.rev acc
    else
      let k = key r in
      if String.length k = lp then loop (r + 1) ((p, 1) :: acc)
      else
        let g =
          String.sub k 0
            (lp + Uchar.utf_decode_length (String.get_utf_8_uchar k lp))
        in
        let next = lower_bound (after_prefix g) in
        loop next ((label g, next - r) :: acc)
  in
  loop (lower_bound p) []

let with_reader bdir ~particles ~strings_len ~file_string k =
  let fname = Filename.concat bdir "base.inx" in
  let inx = lazy (Secure.open_in_bin fname) in
  let dat = lazy (Secure.open_in_bin (Filename.concat bdir "base.dat")) in
  let close c = if Lazy.is_val c then close_in_noerr (Lazy.force c) in
  Fun.protect ~finally:(fun () ->
      close inx;
      close dat)
  @@ fun () ->
  let sections =
    lazy
      (let ic = Lazy.force inx in
       List.map
         (fun s -> (s.code, (s, lazy (input_landmarks ic s))))
         (input_sections fname ic))
  in
  let section d =
    match List.assoc_opt (code d) (Lazy.force sections) with
    | Some (s, landmarks) -> (s, Lazy.force landmarks)
    | None -> failwith (fname ^ ": no dictionary " ^ Dict.to_string d)
  in
  let key d istr = Dict.key (Lazy.force particles) d (file_string istr) in
  let search d =
    let s, landmarks = section d in
    let ic = Lazy.force inx in
    let istr r = istr_at ic s r in
    (s, istr, lower_bound landmarks s (fun r -> key d (istr r)))
  in
  let range lower_bound p = (lower_bound p, lower_bound (after_prefix p)) in
  let dict_size d p =
    let _, _, lower_bound = search d in
    let lo, hi = range lower_bound p in
    hi - lo
  in
  let dict_groups d p =
    let _, istr, lower_bound = search d in
    groups lower_bound (fun r -> key d (istr r)) p
  in
  let dict_entries d p =
    let _, istr, lower_bound = search d in
    let lo, hi = range lower_bound p in
    List.init (hi - lo) (fun i -> istr (lo + i))
  in
  let dict_refs d i =
    if i < 0 || i >= strings_len then []
    else
      let s, istr, lower_bound = search d in
      let k = key d i in
      let rec find r =
        if r < s.n && String.equal (key d (istr r)) k then
          if istr r = i then
            input_refs (Lazy.force dat) (pos_at (Lazy.force inx) s r)
          else find (r + 1)
        else []
      in
      find (lower_bound k)
  in
  k { dict_size; dict_groups; dict_entries; dict_refs }
