#define FN_strings "0"
#define FN_idx_strings "00"
#define FN_p_access "1"
#define FN_p_aliases "2"
#define FN_p_consang "3"
#define FN_p_first_name_aliases "4"
#define FN_p_firstname "5"
#define FN_p_image "6"
#define FN_p_lastname "7"
#define FN_p_notes "8"
#define FN_p_occ "9"
#define FN_p_occupation "A"
#define FN_p_parents "B"
#define FN_p_pevents "C"
#define FN_p_psources "D"
#define FN_p_public_name "E"
#define FN_p_qualifiers "F"
#define FN_p_related "G"
#define FN_p_rparents "H"
#define FN_p_sex "I"
#define FN_p_surnames_aliases "J"
#define FN_p_titles "K"
#define FN_p_unions "L"
#define FN_f_comment "M"
#define FN_f_fevents "N"
#define FN_f_fsources "O"
#define FN_f_origin_file "P"
#define FN_f_couple "Q"
#define FN_f_children "R"

#define FN_p_deleted "S"
#define FN_f_deleted "T"

#define FN_idx_npoc "U"
#define FN_idx_iof "W"
#define FN_idx_ios "X"

#define FN_spi_f "Y"
#define FN_spi_s "Z"

#define FN_particles "V"

#define FN_notes "01"
#define FN_notes_d "notes_d"

(* FIXME: FIX 991d7c70f *)
(* TODO: Use position in FN_strings.dat as iter instead of Fn_strings.inx *)

(* TODO:
   Use (G)ADT for idx so we can have the same functions for indexes:
   - loaded in memory
   - using fd and 'raw' reads
   - using simple in_channel
*)
(* TODO: try this for idx reading *)
(* TODO: try the same approach for value reading (need to parse size in header, read string, unmarshal) *)

type ifam = int
type iper = int
type istr = int

let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string
let string_of_ifam = string_of_int
let string_of_iper = string_of_int
let string_of_istr = string_of_int
let empty_string = 0
let is_empty_string = (=) empty_string
let quest_string = 1
let is_quest_string = (=) quest_string
let eq_istr = Int.equal
let dummy_ifam = -1
let dummy_iper = -1

type fam_event = (iper, istr) Def.gen_fam_event
type pers_event = (iper, istr) Def.gen_pers_event
type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title

type person =
  { access : Def.access Lazy.t
  ; aliases : istr list Lazy.t
  ; first_names_aliases : istr list Lazy.t
  ; firstname : istr Lazy.t
  ; image : istr Lazy.t
  ; iper : iper
  ; lastname : istr Lazy.t
  ; note : istr Lazy.t
  ; occ : int Lazy.t
  ; occupation : istr Lazy.t
  ; parents : ifam option Lazy.t
  ; consang : Adef.fix Lazy.t
  ; pevents : pers_event list Lazy.t
  ; psources : istr Lazy.t
  ; public_name : istr Lazy.t
  ; qualifiers : istr list Lazy.t
  ; related : iper list Lazy.t
  ; rparents : relation list Lazy.t
  ; sex : Def.sex Lazy.t
  ; surnames_aliases : istr list Lazy.t
  ; titles : title list Lazy.t
  ; unions : ifam array Lazy.t

  ; birth : pers_event option Lazy.t
  ; baptism : pers_event option Lazy.t
  ; death : pers_event option Lazy.t
  ; burial : pers_event option Lazy.t
  }

type family =
  { comment : istr Lazy.t
  ; fevents : fam_event list Lazy.t
  ; fsources : istr Lazy.t
  ; ifam : ifam Lazy.t
  ; origin_file : istr Lazy.t
  ; couple : iper array Lazy.t
  ; children : iper array Lazy.t

  ; relation : fam_event option Lazy.t
  ; separation : fam_event option Lazy.t
  }

type base_version = GnWbLazy0000

module IntMap = Map.Make (Int)

let map_max m =
  match IntMap.max_binding_opt m with
  | Some (i, _) -> i
  | None -> 0

type idx =
  | Loaded of int ref * bytes
  | File_descr of Unix.file_descr * bytes
  | In_channel of in_channel

let _seek_in idx i = match idx with
  | Loaded (p, _) -> p := i
  | File_descr (fd, _) -> ignore (Unix.lseek fd i Unix.SEEK_SET)
  | In_channel ic -> seek_in ic i

(* TODO: _read_binary_uint *)
(* TODO: seek_in after reading Loaded or File_descr? *)
let _read_binary_int =
  let aux b off =
    let r =
      let aux c res = (res lsl 8) + Char.code c in
      0
      |> aux (Bytes.unsafe_get b off)
      |> aux (Bytes.unsafe_get b @@ off + 1)
      |> aux (Bytes.unsafe_get b @@ off + 2)
      |> aux (Bytes.unsafe_get b @@ off + 3)
    in (r lsl 32) asr 32
  in
  function
  | Loaded (p, b) ->
    let off = !p in
    let r = aux b off in
    p := off + 4 ;
    r
  | File_descr (fd, b) ->
    assert (4 = Unix.read fd b 0 4) ;
    aux b 0
  | In_channel ic ->
    input_binary_int ic

let () =
  let fn = "/tmp/bench" in
  let n = 25000 in
  (*  sur mon PC : File_descr plus rapide que In_channel pour n >= 25000 *)
  let oc = open_out_bin fn in
  for i = 0 to n do output_binary_int oc i done ;
  close_out oc ;
  Random.self_init () ;
  let r = Array.init 50 (fun _ -> Random.int n) in
  let bench name ic =
    Gc.compact () ;
    Mutil.bench name begin fun () ->
      Array.iter begin fun i ->
        _seek_in ic (i * 4) ;
        assert (_read_binary_int ic = i) ;
      end r
    end
  in
  bench __LOC__ begin
    let ic = open_in_bin fn in
    let r = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    Loaded (ref 0, Bytes.unsafe_of_string r)
  end ;
  bench __LOC__ begin
    let fd = Unix.openfile fn [Unix.O_RDONLY] 0o400 in
    let b = Bytes.create (max 4 Marshal.header_size) in
    File_descr (fd, b)
  end ;
  bench __LOC__ begin
    In_channel (open_in_bin fn)
  end

let _unmarshal : 'a . idx -> 'a = function
  | Loaded (p, b) ->
    Marshal.from_bytes b !p
  | File_descr (fd, b) ->
    (* Does not seem to work *)
    assert (Marshal.header_size = Unix.read fd b 0 Marshal.header_size) ;
    let len = Marshal.data_size b 0 in
    let b' = Bytes.create (len + Marshal.header_size) in
    Bytes.blit b 0 b' 0 Marshal.header_size ;
    assert (len = Unix.read fd b' Marshal.header_size len) ;
    Marshal.from_bytes b' 0
  | In_channel ic ->
    Marshal.from_channel ic

let _unmarshal ?pp idx =
  if pp <> None then print_string (__LOC__ ^ ": ") ;
  let r = _unmarshal idx in
  (match pp with Some pp -> print_endline (pp r) | None -> ()) ;
  r

let () =
  let fn = "/tmp/assert" in
  let t = ("a", "b", [12], [|1;2;3;4;5;6;7|]) in
  let oc = open_out_bin fn in
  Marshal.to_channel oc t [ Marshal.No_sharing ] ;
  close_out oc ;
  let test name ic =
    print_string name ;
    print_string "... " ;
    assert (t = _unmarshal ic) ;
    print_endline "OK!"
  in
  test __LOC__ begin
    let ic = open_in_bin fn in
    let r = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    Loaded (ref 0, Bytes.unsafe_of_string r)
  end ;
  test __LOC__ begin
    let fd = Unix.openfile fn [Unix.O_RDONLY] 0o400 in
    let b = Bytes.create (max 4 Marshal.header_size) in
    File_descr (fd, b)
  end ;
  test __LOC__ begin
    In_channel (open_in_bin fn)
  end

let _in_channel_length = function
  | Loaded (_, b) -> Bytes.length b
  | File_descr (fd, _) ->
    let pos = Unix.lseek fd 0 Unix.SEEK_CUR in
    let len = Unix.lseek fd 0 Unix.SEEK_END in
    assert (pos = Unix.lseek fd pos Unix.SEEK_SET) ;
    len
  | In_channel ic -> in_channel_length ic

type base =
  { bdir : string
  ; channels : (string, idx) Hashtbl.t
  ; cache_sou : (int, string) Hashtbl.t
  ; particles : string list Lazy.t
  ; mutable patch_string : (string * int) list
  ; mutable patch_person : (iper, iper, istr) Def.gen_person IntMap.t
  ; mutable patch_ascend : ifam Def.gen_ascend IntMap.t
  ; mutable patch_union : ifam Def.gen_union IntMap.t
  ; mutable patch_family : (iper, ifam, istr) Def.gen_family IntMap.t
  ; mutable patch_descend : iper Def.gen_descend IntMap.t
  ; mutable patch_couple : iper Def.gen_couple IntMap.t
  }

let _open_loaded fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0o400 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let bytes = Bytes.create len in
  assert (len = Unix.read fd bytes 0 len) ;
  Unix.close fd ;
  Loaded (ref 0, bytes)

let _open_file_descr fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0o400 in
  let b = Bytes.create (max 4 Marshal.header_size) in
  File_descr (fd, b)

let _open_in_channel fn =
  In_channel (open_in_bin fn)

let base_open_dat base fn =
  try Hashtbl.find base.channels fn
  with Not_found ->
    let fn' = Filename.concat base.bdir fn in
    let idx = _open_in_channel fn' in
    Hashtbl.add base.channels fn idx ;
    idx

let base_open_inx base fn =
  try Hashtbl.find base.channels fn
  with Not_found ->
    let fn' = Filename.concat base.bdir fn in
    let idx = _open_in_channel fn' in
    Hashtbl.add base.channels fn idx ;
    idx

(* TODO: create an array header so we can read all as array *)
let output_array_dat_inx bdir fn get arr =
  Mutil.bench_times fn @@ fun () ->
  let oc_dat = open_out_bin @@ Filename.concat bdir @@ fn ^ ".dat" in
  let oc_inx = open_out_bin @@ Filename.concat bdir @@ fn ^ ".inx" in
  Array.iteri begin fun i x ->
    assert (pos_out oc_inx = i * 4) ;
    output_binary_int oc_inx (pos_out oc_dat) ;
    Marshal.to_channel oc_dat (get x) [ Marshal.No_sharing ] ;
  end arr ;
  close_out oc_dat ;
  close_out oc_inx

(* TODO: create an array header so we can read all as array *)
let output_array_dat bdir fn get arr =
  Mutil.bench_times fn @@ fun () ->
  let oc_dat = open_out_bin @@ Filename.concat bdir @@ fn ^ ".dat" in
  Array.iteri begin fun i x ->
    assert (pos_out oc_dat = i * 4) ;
    output_binary_int oc_dat (get x) ;
  end arr ;
  close_out oc_dat

let insert_dat bdir fn p v =
  Mutil.bench_times fn @@ fun () ->
  let fn = Filename.concat bdir @@ fn ^ ".dat" in
  let ic = open_in_bin fn in
  let oc = open_out_bin @@ fn ^ ".tmp" in
  output_string oc (really_input_string ic (p * 4)) ;
  output_binary_int oc v ;
  output_string oc (really_input_string ic (in_channel_length ic - p * 4)) ;
  Sys.rename (fn ^ ".tmp") fn

let remove_dat bdir fn p =
  Mutil.bench_times fn @@ fun () ->
  let fn = Filename.concat bdir @@ fn ^ ".dat" in
  let ic = open_in_bin fn in
  let oc = open_out_bin @@ fn ^ ".tmp" in
  output_string oc (really_input_string ic (p * 4)) ;
  ignore (input_binary_int ic) ;
  output_string oc (really_input_string ic (in_channel_length ic - p * 4 - 1)) ;
  Sys.rename (fn ^ ".tmp") fn

let read_dat_inx_aux : 'a . ?pp:('a -> string) -> idx -> idx -> int -> 'a =
  fun ?pp ic_dat ic_inx i ->
  _seek_in ic_inx (i * 4) ;
  _seek_in ic_dat (_read_binary_int ic_inx) ;
  _unmarshal ?pp ic_dat

let read_dat_inx ?pp base name : int -> 'a =
  read_dat_inx_aux ?pp
    (base_open_dat base @@ name ^ ".dat")
    (base_open_inx base @@ name ^ ".inx")

let read_dat base name i =
  let ic_dat = base_open_dat base @@ name ^ ".dat" in
  _seek_in ic_dat (i * 4) ;
  let r = _read_binary_int ic_dat in
  r

let dump_dat_inx bdir fn out pp =
  let ic_inx = open_in_bin @@ Filename.concat bdir @@ fn ^ ".inx" in
  let ic_dat = open_in_bin @@ Filename.concat bdir @@ fn ^ ".dat" in
  let out = open_out out in
  let len = in_channel_length ic_inx / 4 in
  for i = 0 to len - 1 do
    seek_in ic_inx (i * 4) ;
    seek_in ic_dat (input_binary_int ic_inx) ;
    let v = Marshal.from_channel ic_dat in
    Printf.fprintf out "%d -> %s\n%!" i (pp v)
  done ;
  close_in ic_inx ;
  close_in ic_dat ;
  close_out out

let update_dat bdir fn i v =
  Mutil.bench_times fn @@ fun () ->
  let oc = open_out_bin @@ Filename.concat bdir @@ fn ^ ".dat" in
  seek_out oc i ;
  output_binary_int oc v ;
  close_out oc

let update_dat_inx base fn i v =
  let ic_dat = base_open_dat base @@ fn ^ ".dat" in
  let ic_inx = base_open_inx base @@ fn ^ ".inx" in
  let oi =
    _seek_in ic_inx (i * 4) ;
    _read_binary_int ic_inx ;
  in
  let ov =
    _seek_in ic_dat oi ;
    _unmarshal ic_dat
  in
  let len = String.length (Marshal.to_string ov [ Marshal.No_sharing ]) in
  let str = Marshal.to_string v [ Marshal.No_sharing ] in
  if String.length str <= len then begin
    let oc_dat = open_out_bin @@ Filename.concat base.bdir @@ fn ^ ".dat" in
    seek_out oc_dat oi ;
    output_string oc_dat str ;
    close_out oc_dat
  end else begin
    let oc_dat = open_out_bin @@ Filename.concat base.bdir @@ fn ^ ".dat" in
    let oc_inx = open_out_bin @@ Filename.concat base.bdir @@ fn ^ ".inx" in
    seek_out oc_dat (out_channel_length oc_dat) ;
    let ni = pos_out oc_dat in
    output_string oc_dat str ;
    seek_out oc_inx (i * 4) ;
    output_binary_int oc_inx ni ;
    close_out oc_dat ;
    close_out oc_inx ;
  end

let open_base bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let channels = Hashtbl.create 0 in
  let cache_sou = Hashtbl.create 0 in
  let particles =
    lazy (let ic = open_in_bin @@ Filename.concat bdir FN_particles in
          let r : string list = Marshal.from_channel ic in
          close_in ic ;
          r
         )
  in
  { bdir ; channels ; cache_sou ; particles
  ; patch_string = []
  ; patch_person = IntMap.empty
  ; patch_ascend = IntMap.empty
  ; patch_union = IntMap.empty
  ; patch_family = IntMap.empty
  ; patch_descend = IntMap.empty
  ; patch_couple =  IntMap.empty
  }

let nb_of_persons b =
  _in_channel_length (base_open_dat b @@ FN_p_firstname ^ ".dat") / 4

let nb_of_families b =
  _in_channel_length (base_open_dat b @@ FN_f_comment ^ ".dat") / 4

let new_iper b =
  map_max b.patch_person
  |> max (map_max b.patch_ascend)
  |> max (map_max b.patch_union)
  |> (+) 1
  |> max (nb_of_persons b)

let new_ifam b =
  map_max b.patch_family
  |> max (map_max b.patch_couple)
  |> max (map_max b.patch_descend)
  |> (+) 1
  |> max (nb_of_families b)

(* TODO: filter out empty people *)
let make_idx_npoc p strings : ((string * string * int) * int) array =
  let a =
    Array.mapi
      (fun i p -> ( ( Name.crush_lower strings.(p.Def.first_name)
                    , Name.crush_lower strings.(p.Def.surname)
                    , p.occ)
                  , i ) )
      p
  in
  Array.fast_sort compare a ;
  a

let ht_add ht k v =
  match Hashtbl.find_opt ht k with
  | Some acc ->
    if not @@ List.mem v acc then Hashtbl.replace ht k (v :: acc)
  | None -> Hashtbl.add ht k [v]

let make_idx_iofs_aux g crush p : (string * istr list) array =
  let ht = Hashtbl.create 0 in
  Array.iter (fun p -> ht_add ht (crush (g p)) (g p)) p ;
  let a = Array.make (Hashtbl.length ht) ("", []) in
  assert (Array.length a = Hashtbl.fold begin fun k v i -> Array.set a i (k, v) ; i + 1 end ht 0) ;
  Array.fast_sort compare a ;
  a

let make_idx_strings strings =
  let idx = Array.mapi (fun i s -> s, i) strings in
  Array.fast_sort compare idx ;
  idx

let make_idx_iof = make_idx_iofs_aux (fun p -> p.Def.first_name)
let make_idx_ios = make_idx_iofs_aux (fun p -> p.Def.surname)

let make_spi_aux g part p strings : (int * int list) array =
  let cmp (k, _) (k', _) = Mutil.compare_after_particle part strings.(k) strings.(k') in
  let ht = Hashtbl.create 0 in
  Array.iteri (fun i p -> ht_add ht (g p) i) p ;
  let a = Array.make (Hashtbl.length ht) (-1, []) in
  assert (Array.length a = Hashtbl.fold begin fun k v i -> Array.set a i (k, v) ; i + 1 end ht 0) ;
  Array.fast_sort cmp a ;
  a

let make_spi_f = make_spi_aux (fun p -> p.Def.first_name)
let make_spi_s = make_spi_aux (fun p -> p.Def.surname)

(* FIXME: write to tmp, then move *)
let make bname particles ((p, a, u), (f, c, d), strings, bnotes) =
  let strings = Array.map Mutil.normalize_utf_8 strings in
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  Mutil.mkdir_p bdir ;
  (* particles *)
  (let oc = open_out_bin @@ Filename.concat bdir FN_particles in
   Marshal.to_channel oc particles [ Marshal.No_sharing ] ;
   close_out oc) ;
  (* notes *)
  (let output_note s fn =
     let oc = Secure.open_out fn in
     output_string oc s ;
     close_out oc
   in
   let ndir = Filename.concat bdir FN_notes_d in
   List.iter begin function
     | ("", s) -> output_note s FN_notes
     | (fn, s) ->
       let fn = Filename.concat ndir (fn ^ ".txt") in
       Mutil.mkdir_p (Filename.dirname fn) ;
       output_note s fn
   end bnotes
  );
  (* strings *)
  output_array_dat_inx bdir FN_strings (fun x -> x) strings ;
  dump_dat_inx bdir FN_strings "/tmp/FN_strings.creation" (fun x -> x) ;
  (* persons *)
  output_array_dat bdir FN_p_access (fun x -> Obj.magic x.Def.access) p ;
  output_array_dat bdir FN_p_consang (fun x -> Obj.magic x.Def.consang) a ;
  output_array_dat bdir FN_p_firstname (fun x -> x.Def.first_name) p ;
  output_array_dat bdir FN_p_image (fun x -> x.Def.image) p ;
  output_array_dat bdir FN_p_lastname (fun x -> x.Def.surname) p ;
  output_array_dat bdir FN_p_notes (fun x -> x.Def.notes) p ;
  output_array_dat bdir FN_p_occ (fun x -> x.Def.occ) p ;
  output_array_dat bdir FN_p_occupation (fun x -> x.Def.occupation) p ;
  output_array_dat bdir FN_p_parents (fun x -> match x.Def.parents with Some x -> x | _ -> -1) a ;
  output_array_dat bdir FN_p_public_name (fun x -> x.Def.public_name) p ;
  output_array_dat bdir FN_p_psources (fun x -> x.Def.psources) p ;
  output_array_dat bdir FN_p_sex (fun x -> Obj.magic x.Def.sex) p ;
  output_array_dat_inx bdir FN_p_aliases (fun x -> x.Def.aliases) p ;
  output_array_dat_inx bdir FN_p_first_name_aliases (fun x -> x.Def.first_names_aliases) p ;
  output_array_dat_inx bdir FN_p_pevents (fun x -> x.Def.pevents) p ;
  output_array_dat_inx bdir FN_p_qualifiers (fun x -> x.Def.qualifiers) p ;
  output_array_dat_inx bdir FN_p_related (fun x -> x.Def.related) p ;
  output_array_dat_inx bdir FN_p_rparents (fun x -> x.Def.rparents) p ;
  output_array_dat_inx bdir FN_p_surnames_aliases (fun x -> x.Def.surnames_aliases) p ;
  output_array_dat_inx bdir FN_p_titles (fun x -> x.Def.titles) p ;
  output_array_dat_inx bdir FN_p_unions (fun x -> x.Def.family) u ;
  (* families *)
  output_array_dat bdir FN_f_comment (fun x -> x.Def.comment) f ;
  output_array_dat bdir FN_f_fsources (fun x -> x.Def.fsources) f ;
  output_array_dat bdir FN_f_origin_file (fun x -> x.Def.origin_file) f ;
  output_array_dat_inx bdir FN_f_fevents (fun x -> x.Def.fevents) f ;
  output_array_dat_inx bdir FN_f_couple (fun x -> [| Adef.father x ; Adef.mother x |]) c ;
  output_array_dat_inx bdir FN_f_children (fun x -> x.Def.children) d ;
  (* indexes *)
  let crush =
    let ht = Hashtbl.create 0 in
    fun i ->
      try Hashtbl.find ht i
      with _ ->
        let s = Name.crush_lower strings.(i) in
        Hashtbl.add ht i s ;
        s
  in
  Mutil.bench_times __LOC__ begin fun () ->
    output_array_dat_inx bdir FN_idx_strings (fun x -> x) (make_idx_strings strings) ;
    output_array_dat_inx bdir FN_idx_npoc (fun x -> x) (make_idx_npoc p strings) ;
    output_array_dat_inx bdir FN_idx_iof (fun x -> x) (make_idx_iof crush p) ;
    output_array_dat_inx bdir FN_idx_ios (fun x -> x) (make_idx_ios crush p) ;
    output_array_dat_inx bdir FN_spi_f (fun x -> x) (make_spi_f particles p strings) ;
    output_array_dat_inx bdir FN_spi_s (fun x -> x) (make_spi_s particles p strings) ;
  end ;
  open_base bname

let sync ?scratch:_ _ = ()

let find_events g b x e =
  List.find_opt (fun x' -> List.mem (g x') x) e

let find_event g b x e =
  List.find_opt (fun x' -> g x' = x) e

let istr_of_option = function None -> -1 | Some x -> x

let istr_to_option = function -1 -> None | x -> Some x

let sou b i =
  try Hashtbl.find b.cache_sou i
  with _ ->
    let x = read_dat_inx b FN_strings i in
    Hashtbl.add b.cache_sou i x ;
    x

let poi b i =
  let pevents = lazy (read_dat_inx b FN_p_pevents i) in
  let find_event = find_event (fun e -> e.Def.epers_name) in
  let find_events = find_events (fun e -> e.Def.epers_name) in
  { access = lazy (Obj.magic @@ read_dat b FN_p_access i)
  ; aliases = lazy (read_dat_inx b FN_p_aliases i)
  ; first_names_aliases = lazy (read_dat_inx b FN_p_first_name_aliases i)
  ; firstname = lazy (read_dat b FN_p_firstname i)
  ; image = lazy (read_dat b FN_p_image i)
  ; iper = i
  ; lastname = lazy (read_dat b FN_p_lastname i)
  ; note = lazy (read_dat b FN_p_notes i)
  ; occ = lazy (read_dat b FN_p_occ i)
  ; occupation = lazy (read_dat b FN_p_occupation i)
  ; parents = lazy (istr_to_option @@ read_dat b FN_p_parents i)
  ; consang = lazy (Obj.magic @@ read_dat b FN_p_consang i)
  ; pevents
  ; psources = lazy (read_dat b FN_p_psources i)
  ; public_name = lazy (read_dat b FN_p_public_name i)
  ; qualifiers = lazy (read_dat_inx b FN_p_qualifiers i)
  ; related = lazy (read_dat_inx b FN_p_related i)
  ; rparents = lazy (read_dat_inx b FN_p_rparents i)
  ; sex = lazy (Obj.magic read_dat b FN_p_sex i)
  ; surnames_aliases = lazy (read_dat_inx b FN_p_surnames_aliases i)
  ; titles = lazy (read_dat_inx b FN_p_titles i)
  ; unions = lazy (read_dat_inx b FN_p_unions i)

  ; birth = lazy (find_event b Def.Epers_Birth @@ Lazy.force pevents)
  ; baptism = lazy (find_event b Def.Epers_Baptism @@ Lazy.force pevents)
  ; death = lazy (find_event b Def.Epers_Death @@ Lazy.force pevents)
  ; burial = lazy (find_events b [ Def.Epers_Burial ; Def.Epers_Cremation ] @@ Lazy.force pevents)
  }

let foi b i =
  let fevents = lazy (read_dat_inx b FN_f_fevents i) in
  let find_events = find_events (fun e -> e.Def.efam_name) in
  { comment = lazy (read_dat b FN_f_comment i)
  ; fevents
  ; fsources = lazy (read_dat b FN_f_fsources i)
  ; ifam = lazy ((* if Obj.magic @@ read_dat b FN_f_deleted i then dummy_ifam else *) i) (* FIXME *)
  ; origin_file = lazy (read_dat b FN_f_origin_file i)
  ; couple = lazy (read_dat_inx b FN_f_couple i)
  ; children = lazy (read_dat_inx b FN_f_children i)

  (* FIXME: choose the right one *)
  ; relation = lazy (find_events b
                       [ Def.Efam_Marriage ; Def.Efam_NoMarriage ; Def.Efam_Engage
                       ; Def.Efam_NoMention ; Def.Efam_MarriageBann ; Def.Efam_MarriageContract
                       ; Def.Efam_MarriageLicense ; Def.Efam_PACS ; Def.Efam_Residence ]
                     @@ Lazy.force fevents)
  ; separation = lazy (find_events b [ Def.Efam_Divorce ; Def.Efam_Separated ] @@ Lazy.force fevents)
  }

let map fn = function Some x -> fn x | None -> None
let default d = function Some x -> x | None -> d
let map_default d fn = function Some x -> fn x | None -> d

let get_access p = Lazy.force p.access
let get_aliases p = Lazy.force p.aliases
let get_baptism p = map_default Adef.cdate_None (fun e -> e.Def.epers_date) (Lazy.force p.baptism)
let get_baptism_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.baptism)
let get_baptism_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.baptism)
let get_baptism_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.baptism)
let get_birth p = map_default Adef.cdate_None (fun e -> e.Def.epers_date) (Lazy.force p.birth)
let get_birth_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.birth)
let get_birth_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.birth)
let get_birth_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.birth)
let get_first_name p = Lazy.force p.firstname
let get_surname p = Lazy.force p.lastname
let get_iper p = p.iper
let get_burial p =
  match Lazy.force p.burial with
  | Some { Def.epers_name = Epers_Burial ; epers_date ; _ } -> Def.Buried epers_date
  | Some { Def.epers_name = Epers_Cremation ; epers_date ; _ } -> Def.Cremated epers_date
  | _ -> Def.UnknownBurial
let get_burial_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.burial)
let get_burial_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.burial)
let get_burial_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.burial)
let get_death p = (* FIXME *)
  match Lazy.force p.death with
  | Some { Def.epers_date ; _ } ->
    if epers_date <> Adef.cdate_None then Def.Death (Def.Unspecified, epers_date)
    else Def.OfCourseDead
  | _ -> Def.NotDead
let get_death_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.death)
let get_death_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.death)
let get_death_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.death)
let get_consang p = Lazy.force p.consang
let get_first_names_aliases p = Lazy.force p.first_names_aliases
let get_family p = Lazy.force p.unions
let get_image p = Lazy.force p.image
let get_notes p = Lazy.force p.note
let get_occ p = Lazy.force p.occ
let get_occupation p = Lazy.force p.occupation
let get_origin_file p = Lazy.force p.origin_file
let get_parents p = Lazy.force p.parents
let get_pevents p = Lazy.force p.pevents
let get_psources p = Lazy.force p.psources
let get_public_name p = Lazy.force p.public_name
let get_qualifiers p = Lazy.force p.qualifiers
let get_related p = Lazy.force p.related
let get_rparents p = Lazy.force p.rparents
let get_sex p = Lazy.force p.sex
let get_surnames_aliases p = Lazy.force p.surnames_aliases
let get_titles p = Lazy.force p.titles

let get_children f = Lazy.force f.children
let get_comment f = Lazy.force f.comment
let get_divorce f =
  match Lazy.force f.separation with
  | None -> Def.NotDivorced
  | Some { Def.efam_name = Def.Efam_Divorce ; efam_date } -> Def.Divorced efam_date
  | Some { Def.efam_name = Def.Efam_Separated } -> Def.Separated
  | _ -> assert false
let get_father f = Array.unsafe_get (Lazy.force f.couple) 0
let get_fevents f = Lazy.force f.fevents
let get_fsources f = Lazy.force f.fsources
let get_ifam f = Lazy.force f.ifam
let get_marriage f = map_default Adef.cdate_None (fun e -> e.Def.efam_date) (Lazy.force f.relation)
let get_marriage_note f = map_default empty_string (fun e -> e.Def.efam_note) (Lazy.force f.relation)
let get_marriage_place f = map_default empty_string (fun e -> e.Def.efam_place) (Lazy.force f.relation)
let get_marriage_src f = map_default empty_string (fun e -> e.Def.efam_src) (Lazy.force f.relation)
let get_mother f = Array.unsafe_get (Lazy.force f.couple) 1
let get_parent_array f = Lazy.force f.couple
let get_witnesses f =
  match Lazy.force f.relation with
  | Some e -> Array.map fst e.Def.efam_witnesses
  | None -> [||]
let get_relation f =
  match Lazy.force f.relation with
  | Some { Def.efam_name = Def.Efam_Marriage } -> Def.Married
  | Some { Def.efam_name = Def.Efam_NoMarriage } -> Def.NotMarried
  | Some { Def.efam_name = Def.Efam_Engage } -> Def.Engaged
  | Some { Def.efam_name = Def.Efam_NoMention } -> Def.NoMention
  | Some { Def.efam_name = Def.Efam_MarriageBann } -> Def.MarriageBann
  | Some { Def.efam_name = Def.Efam_MarriageContract } -> Def.MarriageContract
  | Some { Def.efam_name = Def.Efam_MarriageLicense } -> Def.MarriageLicense
  | Some { Def.efam_name = Def.Efam_PACS } -> Def.Pacs
  | Some { Def.efam_name = Def.Efam_Residence } -> Def.Residence
  | _ -> assert false

let bname b = Filename.(remove_extension @@ basename b.bdir)


let load_ascends_array _ = ()
let load_couples_array _ = ()
let load_descends_array _ = ()
let load_families_array _ = ()
let load_persons_array _ = ()
let load_strings_array _ = ()
let load_unions_array _ = ()
let clear_ascends_array _ = ()
let clear_couples_array _ = ()
let clear_descends_array _ = ()
let clear_families_array _ = ()
let clear_persons_array _ = ()
let clear_strings_array _ = ()
let clear_unions_array _ = ()
let base_particles b = Lazy.force b.particles

let base_visible_get _ = assert false
let base_visible_write _ = assert false
let base_wiznotes_dir _ = assert false
let close_base _ = assert false
let commit_notes _ = assert false
let date_of_last_change _ = assert false
let delete_ascend _ = assert false
let delete_couple _ = assert false
let delete_descend _ = assert false
let delete_family _ = assert false
let delete_person _ = assert false
let delete_union _ = assert false
let empty_family _ = assert false
let empty_person _ = assert false
let families ?select:_ _ = assert false
let family_of_gen_family _ = assert false
let gen_ascend_of_person _ = assert false
let gen_couple_of_family _ = assert false
let gen_descend_of_family _ = assert false
let gen_family_of_family _ = assert false
let gen_person_of_person _ = assert false
let gen_union_of_person _ = assert false
let insert_ascend _ = assert false
let insert_couple _ = assert false
let insert_descend _ = assert false
let insert_family _ = assert false
let insert_person _ = assert false
let insert_union _ = assert false

let nb_of_real_persons _ = assert false
let no_ascend = { Def.parents = None ; consang = Adef.no_consang }
let no_couple = Adef.couple dummy_iper dummy_iper
let no_descend = { Def.children = [||] }
let no_family ifam =
  { Def.marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [||]
  ; relation = Def.NoMention
  ; divorce = Def.NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = ifam
  }
let no_person ip =
  { Def.first_name = empty_string
  ; surname = empty_string
  ; occ = 0
  ; image = empty_string
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; public_name = empty_string
  ; qualifiers = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; aliases = []
  ; occupation = empty_string
  ; sex = Def.Neuter
  ; access = Def.Private
  ; birth = Adef.cdate_None
  ; birth_place = empty_string
  ; birth_note = empty_string
  ; birth_src = empty_string
  ; baptism = Adef.cdate_None
  ; baptism_place = empty_string
  ; baptism_note = empty_string
  ; baptism_src = empty_string
  ; death = Def.DontKnowIfDead
  ; death_place = empty_string
  ; death_note = empty_string
  ; death_src = empty_string
  ; burial = Def.UnknownBurial
  ; burial_place = empty_string
  ; burial_note = empty_string
  ; burial_src = empty_string
  ; pevents = []
  ; notes = empty_string
  ; psources = empty_string
  ; key_index = ip }
let no_union = { Def.family = [||] }

#define macropatch(arg) let arg b i x = b.arg <- IntMap.update i (function _ -> Some x) b.arg
macropatch(patch_person)
macropatch(patch_ascend)
macropatch(patch_union)
macropatch(patch_couple)
macropatch(patch_family)
macropatch(patch_descend)

let person_of_gen_person _ = assert false
let persons _ = assert false
let persons_of_name _ = assert false

module IDX = struct

  (* Return the index of an element using cmp *)
  let binary_search cmp get len =
    let rec aux low high =
      if high <= low then
        if cmp (get low) = 0 then low
        else raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (get mid) in
        if c < 0 then
          aux low (mid - 1)
        else if c > 0 then
          aux (mid + 1) high
        else
          mid
    in aux 0 len

  (* Return the index of the first element matching, or the which would come after if unbound *)
  let binary_search_or_next cmp get len =
    let rec aux acc low high =
      if high <= low then
        if cmp (get low) <= 0 then low
        else match acc with Some x -> x | None -> raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (get mid) in
        if c < 0 then
          aux (Some mid) low (mid - 1)
        else if c > 0 then
          aux acc (mid + 1) high
        else
          mid
    in aux None 0 len

  (* Return the index of the element comming after, using cmp *)
  let binary_search_next cmp get len =
    let rec aux acc low high =
      if high <= low then
        if cmp (get low) < 0 then low
        else match acc with Some x -> x | None -> raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (get mid) in
        if c < 0 then
          aux (Some mid) low (mid - 1)
        else
          aux acc (mid + 1) high
    in aux None 0 len

end

let search_aux ?pp search fn compare b convert =
  let ic_dat = base_open_dat b @@ fn ^ ".dat" in
  let ic_inx = base_open_inx b @@ fn ^ ".inx" in
  let get = read_dat_inx_aux ?pp ic_dat ic_inx in
  search compare get (_in_channel_length ic_inx / 4)
  |> get
  |> convert

let binary_search ?pp fn compare b = search_aux IDX.binary_search fn compare b

let insert_aux write ic oc p v =
  output_string oc (really_input_string ic p) ;
  write oc v ;
  output_string oc (really_input_string ic (in_channel_length ic - p))

let search_npoc b (k : string * string * int) =
  binary_search FN_idx_npoc (fun (k', _) -> compare k k') b (snd : ((string * string * int) * int) -> int)

let person_of_key b p n o =
  try Some (search_npoc b (Name.crush_lower p, Name.crush_lower n, o))
  with e -> None

let search_iof b k = binary_search FN_idx_iof (fun (k', _) -> String.compare k k') b (snd : (string * istr list) -> istr list)
let search_ios b k = binary_search FN_idx_ios (fun (k', _) -> String.compare k k') b (snd : (string * istr list) -> istr list)

let base_strings_of_first_name b s =
  try search_iof b (Name.crush_lower s)
  with e -> []

let base_strings_of_surname b s =
  try search_ios b (Name.crush_lower s)
  with e -> []

type string_person_index =
  { first : string -> istr
  ; next : istr -> istr
  ; find : istr -> iper list
  }
let spi_find s = s.find
let spi_first s = s.first
let spi_next s = s.next

let spi fn b =
  let cmp k (k', _) = Mutil.compare_after_particle (Lazy.force b.particles) k (sou b k') in
  let pp (k, v) = string_of_int k ^ " -> " ^ String.concat " " (List.map string_of_int v) in
  { first = (fun k -> search_aux ~pp IDX.binary_search_or_next fn (cmp k) b (fst : (int * int list) -> istr))
  ; next = (fun k -> let k = sou b k in search_aux ~pp IDX.binary_search_next fn (cmp k) b (fst : (int * int list) -> istr) : istr -> istr)
  ; find = (fun k -> let k = sou b k in search_aux ~pp IDX.binary_search fn (cmp k) b (snd : (int * int list) -> int list) : istr -> iper list)
  }

let persons_of_first_name = spi FN_spi_f
let persons_of_surname = spi FN_spi_s

let insert write bdir fn p v =
  Mutil.bench_times fn @@ fun () ->
  let fn = Filename.concat bdir fn in
  let ic = open_in_bin fn in
  let oc = open_out_bin @@ fn ^ ".tmp" in
  insert_aux write ic oc p v ;
  Sys.rename (fn ^ ".tmp") fn

let append_inx bdir fn i =
  Mutil.bench_times fn @@ fun () ->
  let fn = Filename.concat bdir fn in
  let ic = open_in_bin fn in
  let oc = open_out_bin @@ fn ^ ".tmp" in
  let p = in_channel_length ic in
  insert_aux output_binary_int ic oc p i ;
  Sys.rename (fn ^ ".tmp") fn

let insert_dat_inx_aux p_inx p_dat bdir fn x =
  Mutil.bench_times fn @@ fun () ->
  let fn = Filename.concat bdir fn in
  let ic_inx = open_in_bin @@ fn ^ ".inx" in
  let oc_inx = open_out_bin @@ fn ^ ".inx.tmp" in
  let ic_dat = open_in_bin @@ fn ^ ".dat" in
  let oc_dat = open_out_bin @@ fn ^ ".dat.tmp" in
  let p_inx = p_inx ic_inx in
  let p_dat = p_dat ic_dat in
  insert_aux output_binary_int ic_inx oc_inx p_inx p_dat ;
  insert_aux (fun oc x -> Marshal.to_channel oc x [ Marshal.No_sharing ]) ic_dat oc_dat p_dat x ;
  close_out oc_inx ;
  close_out oc_dat ;
  close_in ic_inx ;
  close_in ic_dat ;
  Sys.rename (fn ^ ".inx.tmp") (fn ^ ".inx") ;
  Sys.rename (fn ^ ".dat.tmp") (fn ^ ".dat") ;
  (p_inx, p_dat)

let append_dat_inx = insert_dat_inx_aux in_channel_length in_channel_length

let do_insert_string b istr s =
  let (p_inx, _p_dat) = append_dat_inx b.bdir FN_strings s in
  assert (istr = p_inx / 4) ;
  let pos = search_aux IDX.binary_search_or_next FN_idx_strings (fun (s', _) -> String.compare s s') b snd in
  let pos = if pos = 0 then 0 else pos - 1 in (* useless? *)
  insert_dat_inx_aux (fun _ -> pos * 4) in_channel_length b.bdir FN_idx_strings (s, istr)

let commit_patches b =
  let aux_dat i fn ov nv =
    if ov <> nv then update_dat b.bdir fn i (Obj.magic nv)
  in
  let aux_dat_inx i fn ov nv =
    if ov <> nv then update_dat_inx b fn i nv
  in
  List.iter (fun (s, i) -> ignore @@ do_insert_string b i s) b.patch_string ;
  flush_all () ;
  dump_dat_inx b.bdir FN_strings "/tmp/FN_strings.updated" (fun x -> x) ;
  IntMap.iter begin fun i p ->
    let o = poi b i in
    aux_dat i FN_p_access (get_access o) p.Def.access ;
    aux_dat i FN_p_firstname (get_first_name o) p.Def.first_name ;
    aux_dat i FN_p_image (get_image o) p.Def.image ;
    aux_dat i FN_p_lastname (get_surname o) p.Def.surname ;
    aux_dat i FN_p_notes (get_notes o) p.Def.notes ;
    aux_dat i FN_p_occ (get_occ o) p.Def.occ ;
    aux_dat i FN_p_occupation (get_occupation o) p.Def.occupation ;
    aux_dat i FN_p_public_name (get_public_name o) p.Def.public_name ;
    aux_dat i FN_p_psources (get_psources o) p.Def.psources ;
    aux_dat i FN_p_sex (get_sex o) p.Def.sex ;
    aux_dat_inx i FN_p_aliases (get_aliases o) p.Def.aliases ;
    aux_dat_inx i FN_p_first_name_aliases (get_first_names_aliases o) p.Def.first_names_aliases ;
    aux_dat_inx i FN_p_pevents (get_pevents o) p.Def.pevents ;
    aux_dat_inx i FN_p_qualifiers (get_qualifiers o) p.Def.qualifiers ;
    aux_dat_inx i FN_p_related (get_related o) p.Def.related ;
    aux_dat_inx i FN_p_rparents (get_rparents o) p.Def.rparents ;
    aux_dat_inx i FN_p_surnames_aliases (get_surnames_aliases o) p.Def.surnames_aliases ;
    aux_dat_inx i FN_p_titles (get_titles o) p.Def.titles ;
  end b.patch_person ;
  IntMap.iter begin fun i a ->
    let o = poi b i in
    aux_dat i FN_p_consang (get_consang o) a.Def.consang ;
    aux_dat i FN_p_parents (get_parents o) a.Def.parents ;
  end b.patch_ascend ;
  IntMap.iter begin fun i u ->
    let o = poi b i in
    aux_dat_inx i FN_p_unions (get_family o) u.Def.family ;
  end b.patch_union ;
  IntMap.iter begin fun i f ->
    let o = foi b i in
    aux_dat i FN_f_comment (get_comment o) f.Def.comment ;
    aux_dat i FN_f_fsources (get_fsources o) f.Def.fsources ;
    aux_dat i FN_f_origin_file (get_origin_file o) f.Def.origin_file ;
    aux_dat_inx i FN_f_fevents (get_fevents o) f.Def.fevents ;
  end b.patch_family ;
  IntMap.iter begin fun i c ->
    let o = foi b i in
    aux_dat_inx i FN_f_couple (get_parent_array o) [| Adef.father c ; Adef.mother c |] ;
  end b.patch_couple ;
  IntMap.iter begin fun i d ->
    let o = foi b i in
    aux_dat_inx i FN_f_children (get_children o) d.Def.children ;
  end b.patch_descend

let insert_string b s =
  try List.assoc s b.patch_string
  with Not_found ->
  try binary_search FN_idx_strings (fun (s', _) -> String.compare s s') b snd
  with _ ->
    let istr =
      match b.patch_string with
      | (_, i) :: _ -> i + 1
      | [] -> _in_channel_length (base_open_inx b (FN_strings ^ ".inx")) / 4
    in
    b.patch_string <- (s, istr) :: b.patch_string ;
    istr

(* Copied from gwc1 *)

module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a option
    }

  let map (fn : 'a -> 'b) c =
    { length = c.length
    ; get = (fun i ->  match c.get i with Some x -> Some (fn x) | None -> None)
    }

  let length { length ; _ } = length

  let iter fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn x | None -> () done

  let iteri fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn i x | None -> () done

  let fold ?from ?until fn acc { get ; length } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then
        match get !cursor with
        | None -> incr cursor ; next ()
        |  v -> incr cursor ; v
      else None
    in
    next

end

module Marker = struct

  type ('k, 'v) t =
    { get : 'k -> 'v
    ; set : 'k -> 'v -> unit
    }

  let make (k : 'a -> 'k) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    { get = (fun x -> Array.get a (k x) )
    ; set = (fun x v -> Array.set a (k x) v) }

  let get ({ get ; _ } : _ t) k = get k
  let set ({ set ; _ } : _ t) k = set k

end

let persons base =
  { Collection.length = nb_of_persons base
  ; get = (fun i -> Some (poi base i))
  }

let ipers base =
  { Collection.length = nb_of_persons base
  ; get = (fun i -> Some i)
  }

let iper_marker c i = Marker.make (fun i -> i) c i

let ifams ?(select = fun _ -> true) base =
  { Collection.length = nb_of_families base
  ; get = begin fun i ->
      if select i
      then
        if get_ifam (foi base i) = dummy_ifam
        then None else Some i
      else None
    end
  }

let families ?(select = fun _ -> true) base =
  { Collection.length = nb_of_families base
  ; get = begin fun i ->
      let f = foi base i in
      if get_ifam f <> dummy_ifam && select f
      then Some f
      else None
    end
  }

let dummy_collection _ =
  { Collection.length = -1
  ; get = fun _ -> None
  }

let ifam_marker c i = Marker.make (fun i -> i) c i

let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  { Marker.get = begin fun _ -> v end
  ; set = begin fun _ _ -> () end
  }

let bfname base fname =
  Filename.concat base.bdir fname

module NLDB = struct

  let magic = "GWNL0010"

  let read base =
    let fname = bfname base "notes_links" in
    match try Some (open_in_bin fname) with Sys_error _ -> None with
    | Some ic ->
      let r =
        if Mutil.check_magic magic ic
        then (input_value ic : (iper, iper) Def.NLDB.t)
        else failwith "unsupported nldb format"
      in
      close_in ic ; r
    | None -> []

  let write base db =
    let fname_tmp = bfname base "1notes_links" in
    let fname_def = bfname base "notes_links" in
    let fname_back = bfname base "notes_links~" in
    let oc = open_out_bin fname_tmp in
    output_string oc magic ;
    output_value oc (db : (iper, ifam) Def.NLDB.t) ;
    close_out oc ;
    Mutil.rm fname_back;
    Mutil.mv fname_def fname_back ;
    Sys.rename fname_tmp fname_def

end

let read_nldb = NLDB.read
let write_nldb = NLDB.write

let base_notes_origin_file _ = "" (* FIXME *)
let base_notes_dir b = Filename.concat b.bdir FN_notes_d
let base_wiznotes_dir b = Filename.concat b.bdir "wiznotes"

let base_notes_read_aux base fnotes mode =
  let fname =
    if fnotes = "" then FN_notes
    else Filename.concat FN_notes_d (fnotes ^ ".txt")
  in
  try
    let ic = Secure.open_in @@ Filename.concat base.bdir fname in
    let str =
      match mode with
      | Def.RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Def.Rn1Ln -> (try input_line ic with End_of_file -> "")
      | Def.RnAll -> Mutil.input_file_ic ic
    in
    close_in ic ;
    str
  with Sys_error _ -> ""

let base_notes_read base fnotes = base_notes_read_aux base fnotes Def.RnAll
let base_notes_read_first_line base fnotes = base_notes_read_aux base fnotes Def.Rn1Ln
let base_notes_are_empty base fnotes = base_notes_read_aux base fnotes Def.RnDeg = ""
