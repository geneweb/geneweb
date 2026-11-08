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
